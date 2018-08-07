function [b] = bandit_vba_read_in_data( varargin )
% process behavioral data from variable-schedule 3-armed bandit task
% more details at: http://bit.ly/HvBdby
% How to use this function : [b,tmp_reg] = bandit_fmri_sub_proc_ad('id','12345')

% toggle whether figures are displayed
% fig_flag = false;

% This experiment's output is not as straight-forward. You'll
% need to run an instance of 'eprimeread' for each row below (at
% minimum).
%
% Procedure     stimulus prefix         response prefix
% ---------------------------------------------------------------
% halfproc:     showstimhalf.*          FeedbackHalf.*
% normalproc:   showstimnormal.*        FeedbackNormal.*
% doubleproc:   showstimdouble.*        FeedbackDouble.*
%
% comphalf:     computerplayblank.*     FeedbackRepeatedHalf.*
% compnorm:     computerplayblank.*     FeedbackRepeatedNorm.*
% compdouble:   computerplayblank.*     FeedbackRepeatedDouble.*
%
% mystnorm:     mystery.*               FeedbackMystery.*




procs.names = {'halfproc','normalproc','doubleproc', ...
    'comphalf','compnorm','compdouble', ...
    'mystnorm'};

n = 0;
n = n+1; procs.prefixes{n} = {'showstimhalf','FeedbackHalf'};
n = n+1; procs.prefixes{n} = {'showstimnormal','FeedbackNormal'};
n = n+1; procs.prefixes{n} = {'showstimdouble','FeedbackDouble'};
n = n+1; procs.prefixes{n} = {'computerplayblank','FeedbackRepeatedHalf'};
n = n+1; procs.prefixes{n} = {'computerplayblank','FeedbackRepeatedNorm'};
n = n+1; procs.prefixes{n} = {'computerplayblank','FeedbackRepeatedDouble'};
n = n+1; procs.prefixes{n} = {'mystery','FeedbackMystery'};

procs.suffixes.stim   = {'OnsetTime','OffsetTime','RT','RTTime','RESP','CRESP','ACC'};
procs.suffixes.feedbk = {'OnsetDelay','OnsetTime','OffsetTime'};


%  --  parse 'varargin' arguments  --  %
id = varargin{ find(strcmp('id',varargin))+1 };
data_dir = varargin{ find(strcmp('data_dir',varargin))+1 };

%  --  get the data  --  %
% (yuck)
for n_proc = 1:numel(procs.names)
    get_vars = {};
    
    for n_stim_suffix = 1:numel(procs.suffixes.stim)
        tmp = sprintf('%s.%s',procs.prefixes{n_proc}{1},procs.suffixes.stim{n_stim_suffix});
        get_vars(end+1) = {tmp};
    end
    
    for n_fb_suffix = 1:numel(procs.suffixes.feedbk)
        tmp = sprintf('%s.%s',procs.prefixes{n_proc}{2},procs.suffixes.feedbk{n_fb_suffix});
        get_vars(end+1) = {tmp};
    end
    
    % this variable keeps track of trial numbers
    get_vars{end+1} = 'stimlist';
    get_vars{end+1} = 'jitter1'; get_vars{end+1} = 'jitter2';
    
    a.(procs.names{n_proc}) = getfmriData(id,get_vars,procs.names{n_proc},data_dir);
    a.(procs.names{n_proc}).showstim_jitter1 = a.(procs.names{n_proc}).jitter1;
    a.(procs.names{n_proc}).showstim_jitter2 = a.(procs.names{n_proc}).jitter2;
    a.(procs.names{n_proc}) = rmfield(a.(procs.names{n_proc}),{'jitter1','jitter2'});
    
end

% -- organize structure and clean up vars -- %
fprintf('reading in data...\n');
ntrial = maximumTrialNum(a,'stimlist');
b = preallocateStruct(fieldnames(rmfield(a.halfproc,{'fname','stimlist'})),ntrial);

b.fname = a.halfproc.fname;
b.protocol_type = cell(ntrial,1); % preallocate space for keeping track of trial type

fprintf('matching data to appropriate fields...\n');
for n = fieldnames(a)'
    for m = fieldnames(rmfield(a.(n{:}),{'fname','stimlist'}))'
        % grab trial number index for given "protocol"
        q_trial_index = a.(n{:}).stimlist;
        
        % now we need to match current fieldnames in the
        % structure (b) with variable names of read-in data
        new_fieldname = matchDataToField(fieldnames(rmfield(b,{'fname','protocol_type'})),m);
        
        % not all protocols' variables overlap, leading to empty
        % arrays/cell arrays -- we need to convert them to arrays
        % of NaNs if they're found. (if a variable is missing for
        % a given protocol, that will result in an empty cell
        % array -- just check for a cell array.)
        tmp = a.(n{:}).(m{:}); % get the array
        if(iscell(tmp)), tmp = nan(size(tmp)); end
        
        % store into new structure
        b.(new_fieldname)(q_trial_index) = tmp;
        
        % store the type of trial
        b.protocol_type(q_trial_index) = n(:);
    end
end


% -- add stimulus chosen and its position to structure -- %
design_struct = bandit_fmri_load_design; % load design file

%N.B. This was the coding in the original data analysis, although you would
%think "top" refers to finger glove button 2, this is not the case. It has 
%been confirmed that "top" stim is the left-most image displayed to the 
%subject, so the order is "top" ,"left", "right" with corresponding buttton
%glove presses 7,2,3 
q_top   = ( b.stim_RESP == 7 )';
q_left  = ( b.stim_RESP == 2 )';
q_right = ( b.stim_RESP == 3 )';


%Chosen stim refers to A B or C not 2 3 or 7 
b.chosen_stim = cell(numel(design_struct.Procedure),1);

b.chosen_stim(q_top) = design_struct.topstim(q_top);
b.chosen_stim(q_left) = design_struct.leftstim(q_left);
b.chosen_stim(q_right) = design_struct.rightstim(q_right);



% recode chars as stum IDs
q = ~cellfun(@isempty,b.chosen_stim);
tmp(q') = cellfun(@(c) cast(c,'double')-64, b.chosen_stim(q));
tmp(~q) = 999; % missed answers coded as 999
b.chosen_stim = tmp;

% -- code for stimulus choice switches -- %
b.stim_switch = zeros(numel(b.chosen_stim),1);
for n = 2:numel(b.stim_switch)
    last_stim    = b.chosen_stim(n-1);
    current_stim = b.chosen_stim(n);
    b.stim_switch(n) = ne(last_stim,current_stim);
end

% -- code choice switches on the next trial
b.next_switch = [b.stim_switch(2:end); 0];

% remove the 'break' trial types
q_to_fix  = ( structfun(@numel,b) == numel(b.protocol_type) );
q_to_keep = ~cellfun(@isempty,b.protocol_type);
b_fnames = fieldnames(b);
for n = 1:length(b_fnames)
    if(q_to_fix(n))
        b.(b_fnames{n}) = b.(b_fnames{n})(q_to_keep);
    end
end

% code missed responses
b.missed_responses = ( b.stim_RT == 0 );


b.sub_proc = bandit_fMRI_sub_proc(id, b, design_struct);

% Create the reward stake vector
b.stakeVec = ones(length(b.stim_RT),1); 
for i = 1:length(b.protocol_type)
    if (findstr(b.protocol_type{i},'norm'))
        b.stakeVec(i) = 25;
    elseif findstr(b.protocol_type{i},'half')
        b.stakeVec(i) = 10;
    else 
        b.stakeVec(i) = 50;
    end
end

% Create subject reward Vecotr (how much they won for specific trial)
b.rewardVec = b.stakeVec.*b.stim_ACC;

%% here you would censor trials without fMRI data
% 1. Check the duration of scanner runs (TR*volumes)
% 2. Remove trials w/o scanner volumes based on onset times (since trials
% have unequal duration due to jittering)
% b_fnames = fieldnames(b);
% 
% TR = 750;
% scan_vol = 925;
% scan_trial_length = TR * scan_vol;
% if (id == 219089)
%     bad_scan_vol = 466;
%     secnd_rn_dur = TR * bad_scan_vol;
%     censor_time_start = b.stim_OnsetTime(101) + secnd_rn_dur;
%     censor_time_end = scan_trial_length - secnd_rn_dur + censor_time_start;
%     censor_idx = find( censor_time_start <= b.feedback_OffsetTime & censor_time_end >= b.stim_OnsetTime );
%     
%     for n = 1:length(b_fnames)
%         if  (length(b.(b_fnames{n})) > 250)
%             b.(b_fnames{n})(censor_idx)=[];
%         end
%     end 
% end
%%

%% Flag abnormal subjects
if (id == 202021) %This subject only has 2 blocks of Bandit
    b.chosen_stim(201:end)=[];
    b.stim_switch(201:end)=[];
    b.next_switch(201:end)=[];
end

%% Find find where the mystery and computer trials are, use to censor later
myst_cens = 'myst';
comp_cens = 'comp';
prefix=cellfun(@(x) x(1:4), b.protocol_type, 'UniformOutput', false);
myst_index = cell2mat(cellfun(@(x) isequal(x,myst_cens), prefix, 'UniformOutput', false));
comp_index = cell2mat(cellfun(@(x) isequal(x,comp_cens), prefix, 'UniformOutput', false));
%b.stim_RT(comp_index)=0; %remove computer trials from choice

%Save the myst and comp indexs for regs and info
b.myst_index = myst_index;
b.comp_index = comp_index;

%For long format data file
b.stakeVec_filtered=b.stakeVec;
b.stakeVec_filtered(~comp_index==myst_index) = [];

%%

% %run model
% s = rlmodel3fmri(b);
% b.params.alphawin = s.prob.alphawin;
% b.params.alphaloss = s.prob.alphaloss;
% b.params.c = s.prob.c;
% b.echosen = s.prob.echosen;
% b.delta = s.prob.delta;
% b.deltaplus = s.prob.delta.*(s.prob.delta>0);
% b.deltaminus = -s.prob.delta.*(s.prob.delta<0);
% b.emsd = s.prob.emsd; %Expected value from max squared difference
% b.essd = s.prob.essd; %Expected value from summed squared difference (trialwise)
% % code onset of the next trial
% 
% %b.stim_NextOnsetTime=[b.stim_OnsetTime(2:300); b.stim_RTTime(300)];
% %%
% b.stim_NextOnsetTime=[b.stim_OnsetTime(2:end); b.stim_RTTime(end)]; %This is just a more generic line than above


%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function xout = getfmriData(id,vars,procedure,data_dir)
% reads eprime data for a given bandit subject based on ID

% Find the eprime file
%data_dir  = [pathroot 'analysis/bandit/fmri/data/raw/'];
%data_dir  = 'C:/kod/fMRI/data/raw/'; %for testing purposes
file_name = dir([data_dir '/' sprintf('%d/*vrbl_scanner*.txt',id)]);
%fpath     = @(~) [pathroot sprintf('analysis/bandit/fmri/data/raw/%d/%s',id,file_name)];
fpath     = @(~) [ data_dir filesep sprintf('%d/%s',id,file_name.name)];
if     exist(fpath(),'file') 

% read in the data (make sure range to search raw text is correct)
xout = eprimeread(fpath(),procedure,vars,0,-13,18);
else
xout = eprimeread(file_name,procedure,vars,0,-13,18);
end
return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function nmax = maximumTrialNum(a_secondary_structure,target_fieldname)
% by secondary structure, I mean something like the following:
% (there may be an actual term for this. I am unaware of it.)
% the "secondary" fieldname (target_fieldname) must be consistent
%
% root          // root
%    .foo_ax    // primary
%        .bar   // secondary
%    .foo_by
%        .bar

nmax = 0;
for n = fieldnames(a_secondary_structure)'
    nmax = max([ nmax; a_secondary_structure.(n{:}).(target_fieldname) ]);
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function sout = preallocateStruct(fieldnames_to_trim,n_trials)
% this function takes the 'a' structure and uses it to make and
% preallocate memory for the final data structure
%
% instead of simply trimming off all of the fieldname before the
% underscore, you must parse which are stimulus and feedback
% related variables

for n = fieldnames_to_trim'
    % set index
    q = strfind(n{:},'_')+1;
    if(isempty(q))
        q = 1;
        prefix = [];
    elseif(regexp(n{:},'Feedback'))
        prefix = 'feedback';
    else
        prefix = 'stim';
    end
    
    new_fieldname = [prefix '_' n{:}(q:end)];
    sout.(new_fieldname) = nan(n_trials,1);
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function matched_struc_fieldname = matchDataToField(struct_fieldnames,in_var)

if(iscell(in_var)), in_var = cell2mat(in_var); end % convert to string

% preallocate and preset variables
fieldname_parts   = regexp(struct_fieldnames,'_','split');
bool_is_match     = false(size(struct_fieldnames));
front_matches     = bool_is_match;
back_matches      = bool_is_match;

for q_index = 1:numel(struct_fieldnames)
    
    front_chunk = fieldname_parts{q_index}{1};
    back_chunk  = fieldname_parts{q_index}{2};
    
    % function handle because I'm lazy (case insensitive)
    matchVarName = @(s) ~isempty(strfind(lower(in_var),lower( s )));
    
    % check if front chunk matches anything in the variable name
    front_matches(q_index) = matchVarName(front_chunk);
    if(~front_matches(q_index))
        odd_man_out = ( matchVarName('mystery') | matchVarName('computer') );
        if(odd_man_out && strcmp(front_chunk,'stim') && ~matchVarName('feedback'))
            front_matches(q_index) = true;
        end
    end
    
    % check if the back chunk matches
    back_matches(q_index) = ~isempty(regexp(in_var,['_' back_chunk '$'],'once'));
    
    % see which fieldname is a match with the variable
    bool_is_match(q_index) = ( front_matches(q_index) & back_matches(q_index) );
    
end

% make sure there is only one match, otherwise we have a problem
if(sum(bool_is_match) > 1)
    % if there are multiple matches...
    keyboard
else
    matched_struc_fieldname = struct_fieldnames{bool_is_match};
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function foo = createSimpleRegressor(event_begin,event_end,epoch_window,conditional_trial)

if(~exist('conditional_trial','var') || isempty(conditional_trial))
    conditional_trial = ones(length(event_begin),1);
end

% create epoch windows for each trial
epoch = arrayfun(@(a,b) a:b,event_begin,event_end,'UniformOutput',false);

foo = zeros(size(epoch_window));

for n = 1:numel(epoch)
    if(conditional_trial(n))
        foo = logical(foo + histc(epoch{n},epoch_window));
    end
end

return

% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function foo = alexSimpleRegressor(event_beg,event_end,RT,onsets,cond,mod1,mod2)

bar=[];
idx=cond(find(RT));
for ct=1:length(onsets)
    if idx(ct) %>0?
        int=round(event_beg(ct)./100)+mod1:(round(event_end(ct)./100))+mod2;
        bar = [bar int];
    end
end
foo=zeros(1,20814); %hard code bad!
if bar(1)==0
    foo(bar(2:end))=1; %remove leading 0 if applicable
else
    foo(bar)=1;
end
foo=logical(foo);

return



function foo = alexParametricRegressor(event_beg,event_end,RT,onsets,cond,mod1,mod2)
% similar to the other regressor function, but does what Alex thought the other one
% initially did (multiply, not filter)
%bar=[];
tmp = zeros(1,20814);
idx=cond(find(RT));
for ct=1:length(onsets)
    if idx(ct) 
        int=round(event_beg(ct)./100)+mod1:(round(event_end(ct)./100))+mod2;
        %bar = [bar int];
        if int(1)==0 %Remove leading 0 if applicable
            tmp(int(2:end))= 1*cond(ct);
        else
            tmp(int) = 1*cond(ct);
        end
    end
end
% foo=zeros(1,20814); %hard code bad!
% if bar(1)==0
%     foo(bar(2:end))=1; %remove leading 0 if applicable
% else
%     foo(bar)=1;
% end
foo = tmp;
%foo=logical(foo);
return




function foo = createParametricRegressor(event_begin,event_end,epoch_window,parametric_mult)
% similar to the other regressor function, but does what Alex thought the other one
% initially did (multiply, not filter)

% this was not a problem earlier, but for some reason it is now: find indices that would
% result in a negative value and set them both to 0
qbz = ( event_begin == 0 ); qez = ( event_end == 0 );
event_begin( qbz | qez ) = 0; event_end( qbz | qez ) = 0;

% check if optional parametric variable was used
if(~exist('parametric_mult','var') || isempty(parametric_mult))
    parametric_mult = ones(length(event_begin),1);
end

% create epoch windows for each trial
epoch = arrayfun(@(a,b) a:b,event_begin,event_end,'UniformOutput',false);

% for each "epoch" (array of event_begin -> event_end), count events
per_event_histcs = cellfun(@(h) logical(histc(h,epoch_window)),epoch,'UniformOutput',false);

tmp = zeros(size(per_event_histcs{1}));
for n = 1:numel(per_event_histcs)
    tmp = tmp + parametric_mult(n)*per_event_histcs{n};
end

foo = tmp;

return

function foo = eprime2ScanTime(epoch,blen)
%create onset times (in scanner time)
bl1onsets=epoch(1:100)-epoch(1);
bl2onsets=epoch(101:200)-epoch(101)+blen;
bl3onsets=epoch(201:end)-epoch(201)+(blen*2);
foo=[bl1onsets' bl2onsets' bl3onsets'];
return

