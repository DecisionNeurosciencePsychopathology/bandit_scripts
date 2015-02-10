%
%
% Jan Kalkus & Alex Dombrovski
% 3 May 2013

function [b,tmp_reg] = bandit_fmri_sub_proc_ad( varargin )
% process behavioral data from variable-schedule 3-armed bandit task
% more details at: http://bit.ly/HvBdby

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
    
    a.(procs.names{n_proc}) = getfmriData(id,get_vars,procs.names{n_proc});
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

q_top   = ( b.stim_RESP == 7 )';
q_left  = ( b.stim_RESP == 2 )';
q_right = ( b.stim_RESP == 3 )';

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

%%

%run model
s = rlmodel3fmri(b);
b.params.alphawin = s.prob.alphawin;
b.params.alphaloss = s.prob.alphaloss;
b.params.c = s.prob.c;
b.echosen = s.prob.echosen;
b.delta = s.prob.delta;
b.deltaplus = s.prob.delta.*(s.prob.delta>0);
b.deltaminus = -s.prob.delta.*(s.prob.delta<0);
b.emsd = s.prob.emsd; %Expected value from max squared difference
b.essd = s.prob.essd; %Expected value from summed squared difference (trialwise)
% code onset of the next trial

%b.stim_NextOnsetTime=[b.stim_OnsetTime(2:300); b.stim_RTTime(300)];
%%
b.stim_NextOnsetTime=[b.stim_OnsetTime(2:end); b.stim_RTTime(end)]; %This is just a more generic line than above
%%

% -- make regressors --
scan_tr = .75;
% block_length is a length of a single scanner run (provided that they are
% equal, the case for bandit) in scanner TRs, NOT in seconds
block_length = 925;
tr = .1;
hemoir = spm_hrf(tr, [6,16,1,1,6,0,32]); % could stand to be tuned...
% uphemoir = smooth(resample(hemoir,.5,10),20);
%uphemoir = gsresample(hemoir,.5,10); BAD!!!
% load betas_contr_postRT150

fprintf('computing regressors...\n');
frequency_scale_hz = 10;
feedback_end = 400;
%x = [1:100;101:200;201:300];
%x = [1:100];
%%
%Create array based on number of trials completed
total_task_len = length(b.stim_OnsetTime);
num_blocks = ceil(total_task_len/100);
for n = 1:num_blocks
    if (n ==1)
        x(n,:)=1:100;
    elseif (n>1)
        x(n,:)=x(n-1,:)+100;
    else
        error('You have no trials');
    end
end

%%--------------------------------------------------------------------------------------------------------
% Alex make regressor 

blck_len = scan_tr*block_length*1000;
%create onset times (in scanner time)
bl1onsets=b.stim_OnsetTime(1:100)-b.stim_OnsetTime(1);
bl2onsets=b.stim_OnsetTime(101:200)-b.stim_OnsetTime(101)+blck_len;
bl3onsets=b.stim_OnsetTime(201:end)-b.stim_OnsetTime(201)+(blck_len*2);
blk.time.onsets=[bl1onsets' bl2onsets' bl3onsets'];

%create feedback onset times (in scanner time)
bl1feedonsets=b.feedback_OnsetTime(1:100)-b.feedback_OnsetTime(1);
bl2feedonsets=b.feedback_OnsetTime(101:200)-b.feedback_OnsetTime(101)+blck_len;
bl3feedonsets=b.feedback_OnsetTime(201:end)-b.feedback_OnsetTime(201)+(blck_len*2);
blk.time.feedback=[bl1feedonsets' bl2feedonsets' bl3feedonsets'];

%create rts (in scanner time)
blk.time.rts=blk.time.onsets+b.stim_RT';

%take RTs from trials where subject actually responded
blk.time.goodrts=blk.time.rts(b.stim_RT'~=0);
blk.time.goodonsets=blk.time.onsets(b.stim_RT'~=0);
blk.time.goodfeed=blk.time.feedback(b.stim_RT'~=0);
blk.time.goodfeed(find(isnan(blk.time.goodfeed)))=[]; %remove computer trials

% make reg for deliberation time: onset to RT
decide10hz=[];
for ct=1:length(blk.time.goodonsets)
    % decide10hz is the index of non-zero values of the regressor
    % 1 bin = 100ms subtracted to account for the time needed to
    % perform the button press
    int=round(blk.time.goodonsets(ct)./100):round(blk.time.goodrts(ct)./100-1);
    blk.time.rt_length(ct)=length(round(blk.time.goodonsets(ct)./100):round(blk.time.goodrts(ct)./100-1));
    decide10hz=[decide10hz int];
end
decide10hz=decide10hz(2:end); %remove leading 0
blk.time.alexChoice=zeros(1,20814);
blk.time.alexChoice(decide10hz)=1;
blk.time.alexChoice=logical(blk.time.alexChoice);

%Feedback
feed10hz=[];
for ct=1:length(blk.time.goodfeed)
    int=round(blk.time.goodfeed(ct)./100):(round(blk.time.goodfeed(ct)./100)+4); %feedback end
    feed10hz=[feed10hz int];
end
blk.time.alexFeed=zeros(1,20814);
blk.time.alexFeed(feed10hz(2:end))=1; %remove leading 0
blk.time.alexFeed=logical(blk.time.alexFeed);

%Feedback for previous trials
prevfeed10hz=[];
for ct=2:length(blk.time.goodfeed)
        int=round(blk.time.goodfeed(ct-1)./100):(round(blk.time.goodfeed(ct-1)./100)+3); %feedback end
        prevfeed10hz=[prevfeed10hz int];
end
blk.time.prevFeed=zeros(1,20814);
blk.time.prevFeed(prevfeed10hz(2:end))=1; %remove leading 0
blk.time.prevFeed=logical(blk.time.prevFeed);


%Actions
for n_stim = [2 3 7]
    action10hz=[];
    %blk.time.(['action_' num2str(n_stim)])=[];
    q_stim_id = b.stim_RESP == n_stim;
    idx=q_stim_id(find(b.stim_RT));
    for ct=1:length(blk.time.goodonsets)
        if idx(ct)==1
            int=round(blk.time.goodrts(ct)./100)-2:(round(blk.time.goodrts(ct)./100));
            action10hz = [action10hz int];
            %blk.time.(['action_' num2str(n_stim)])=[blk.time.(['action_' num2str(n_stim)]) int];
        end
    end
    blk.time.(['alexAction_' num2str(n_stim)])=zeros(1,20814);
    blk.time.(['alexAction_' num2str(n_stim)])(action10hz)=1; 
    blk.time.(['alexAction_' num2str(n_stim)])=logical( blk.time.(['alexAction_' num2str(n_stim)]));
end

%PE and EV
blk.time.alexDeltaplus = ...
    alexParametricRegressor(blk.time.goodfeed,blk.time.goodfeed,b.stim_RT,blk.time.goodfeed...
    ,b.deltaplus,0,4);

blk.time.alexDeltaminus = ...
    alexParametricRegressor(blk.time.goodfeed,blk.time.goodfeed,b.stim_RT,blk.time.goodfeed...
    ,b.deltaminus,0,4);

blk.time.alexEchosen = ...
    alexParametricRegressor(blk.time.goodonsets,blk.time.goodonsets,b.stim_RT,blk.time.goodonsets...
    ,b.echosen,0,5);

%Alex model any valid action chosen
tmp_RESP=b.stim_RESP~=-999; %create condition variable
blk.time.alexAnyaction= ...
    alexSimpleRegressor(blk.time.goodrts,blk.time.goodrts,b.stim_RT,blk.time.goodrts...
    ,tmp_RESP,-2,0);

%Mean centered PE and EV in hopes to yield more powerful maps
b.echosen_MC = b.echosen - mean(b.echosen);
b.deltaminus_MC = b.deltaminus - mean(b.deltaminus);
b.deltaplus_MC = b.deltaplus - mean(b.deltaplus);

blk.time.alexDeltaplus_MC = ...
    alexParametricRegressor(blk.time.goodfeed,blk.time.goodfeed,b.stim_RT,blk.time.goodfeed...
    ,b.deltaplus_MC,0,4);

blk.time.alexDeltaminus_MC = ...
    alexParametricRegressor(blk.time.goodfeed,blk.time.goodfeed,b.stim_RT,blk.time.goodfeed...
    ,b.deltaminus_MC,0,4);

blk.time.alexEchosen_MC = ...
    alexParametricRegressor(blk.time.goodonsets,blk.time.goodonsets,b.stim_RT,blk.time.goodonsets...
    ,b.echosen_MC,0,5);

blk.time.Value_diff_best = ...
    alexParametricRegressor(blk.time.goodonsets,blk.time.goodonsets,b.stim_RT,blk.time.goodonsets...
    ,b.emsd,0,5);

blk.time.Value_diff_all = ...
    alexParametricRegressor(blk.time.goodonsets,blk.time.goodonsets,b.stim_RT,blk.time.goodonsets...
    ,b.essd,0,5);




%---------------------------------------------------------------------------------------------------------
    
%%
%for block_n = 1:3
for block_n = 1:num_blocks %more generic
    % this scale is in msec, but it is separated into bins of X
    % Hz (defined by 'frequency_scale' above. the resulting
    % output will be in the scale of X Hz.
    bin_size = 1/frequency_scale_hz*1000; % convert Hz to msec
    epoch_window = b.stim_OnsetTime(x(block_n,1)):bin_size:b.stim_OnsetTime(x(block_n,1))+scan_tr*block_length*1000;
    intv = length(epoch_window);
    
    % for RTs
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = b.stim_RTTime(x(block_n,:));
    tmp_reg.(['regressors' num2str(block_n)]).RT = ...
        createSimpleRegressor(event_beg,event_end,epoch_window);
    
    % choice -- after stimulus presentation
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = event_beg + 500;
    tmp_reg.(['regressors' num2str(block_n)]).choice = ...
        createSimpleRegressor(event_beg,event_end,epoch_window);
    
    % for feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).feedback = ...
        createSimpleRegressor(event_beg,event_end,epoch_window);
    
    
    % convolve switch regressor with RT
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = b.stim_OffsetTime(x(block_n,:));
    tmp_reg.(['regressors' num2str(block_n)]).switch_RT = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.stim_switch(x(block_n,:)));
    
    % convolve next switch regressor with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).switch_feedback = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.next_switch(x(block_n,:)));
    
    % convolve error regressor with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).error_feedback = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,~b.stim_ACC(x(block_n,:)));

    %% Rescorla-Wagner
    
    % convolve PE+ regressor with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).deltaplus = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.deltaplus(x(block_n,:)));

    % convolve PE- regressor with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).deltaminus = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.deltaminus(x(block_n,:)));
   
    % make EVchosen regressor, non-RT-convolved, aligned with stimulus
    % presentation
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = event_beg + 500;
    tmp_reg.(['regressors' num2str(block_n)]).echosen = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.echosen(x(block_n,:)));
    %% TD
    
    % 1: get boxcar for choice --> feedback modulated by echosen
    event_beg = b.stim_RTTime(x(block_n,:)); event_end = b.feedback_OnsetTime(x(block_n,:));
    main_body_boxcar = createSimpleRegressor(event_beg,event_end,epoch_window, b.echosen(x(block_n,:)));
    % 2: make boxcar for positive predition errors (duration is 400 msec.)
%     event_beg = b.fb_onset_time(x{block_n}); event_end = event_beg + 400;
%     q_pos_errs = ( b.fauxPE > 0 );
%     pos_pred_err = createSimpleRegressor(event_beg,event_end,epoch_window,q_pos_errs(x{block_n}));
%     % 3: make boxcar for negative predition errors 
%     q_neg_errs = ( b.fauxPE < 0 );
%     neg_pred_err = -1*createSimpleRegressor(event_beg,event_end,epoch_window,q_neg_errs(x{block_n}));
    % 4: add main boxcar and prediction error boxcars together for TD boxcar
    tmp_reg.(['regressors' num2str(block_n)]).TD_reg = ...
        ( main_body_boxcar + tmp_reg.(['regressors' num2str(block_n)]).deltaplus + tmp_reg.(['regressors' num2str(block_n)]).deltaminus);
    % -- 0000000000000000000 --
   %%
    % make an error>correct regressor convolved with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).error2correct1 = ...
        1 + createSimpleRegressor(event_beg,event_end,epoch_window,(~b.stim_ACC(x(block_n,:))));
        
     % make choice and switch regressors, non-RT-convolved
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = event_beg + 500;
    tmp_reg.(['regressors' num2str(block_n)]).switch_choice = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.stim_switch(x(block_n,:)));
    
    % convolve correct regressor with feedback
    event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
    tmp_reg.(['regressors' num2str(block_n)]).correct_feedback = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.stim_ACC(x(block_n,:)));
    
    % regressor for missed responses
    event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = b.stim_NextOnsetTime(x(block_n,:));
    tmp_reg.(['regressors' num2str(block_n)]).to_censor = ...
        createSimpleRegressor(event_beg,event_end,epoch_window,b.missed_responses(x(block_n,:)));
    
    
    % create regressors for "actions"
    event_beg = b.stim_RTTime(x(block_n,:))-200; event_end = b.stim_RTTime(x(block_n,:));
    
    for n_stim = [2 3 7]
        
        q_stim_id = ( b.stim_RESP(x(block_n,:)) == n_stim );
        tmp_reg.(['regressors' num2str(block_n)]).(['action_' num2str(n_stim)])(:,1) = ...
            createSimpleRegressor(event_beg,event_end,epoch_window,q_stim_id);
        
    end
    
    % create regressor for any "action", condidtion is only where any
    % response is made (2,3,7)
    event_beg = b.stim_RTTime(x(block_n,:))-200; event_end = b.stim_RTTime(x(block_n,:));
    tmp_reg.(['regressors' num2str(block_n)]).anyAction = ...
            createSimpleRegressor(event_beg,event_end,epoch_window,b.stim_RESP(x(block_n,:))~=-999);
    
%--------------------------------------------------------------------------------------------------------
    % Create regs for alex model
    alfns=fieldnames(blk.time)';
    st=find(ismember(alfns,'alexChoice'));
    for j=st:length(alfns)
        if block_n ==1
            tmp_reg.(['regressors' num2str(block_n)]).(alfns{j}) = ...
                builtin('_paren', blk.time.(alfns{j}), (block_n:block_n*intv)); 
                %blk.time.alfns(j)(block_n:block_n*intv);
        elseif block_n ==2
           tmp_reg.(['regressors' num2str(block_n)]).(alfns{j}) = ...
               builtin('_paren', blk.time.(alfns{j}), (intv+1:block_n*intv));
                %blk.time.(alfns{j})(intv+1:block_n*intv);
        else
            tmp_reg.(['regressors' num2str(block_n)]).(alfns{j}) = ...
                builtin('_paren', blk.time.(alfns{j}), ((block_n-1)*intv)+1:block_n*intv);
                %blk.time.alfns(j)(((block_n-1)*intv)+1:end);
        end
    end

% % create regressors for alexDecision
%     if block_n ==1
%     tmp_reg.(['regressors' num2str(block_n)]).alexChoice = ...
%         blk.time.decide10hz(block_n:block_n*intv);
%     elseif block_n ==2
%         tmp_reg.(['regressors' num2str(block_n)]).alexChoice = ...
%         blk.time.decide10hz(intv+1:block_n*intv);
%     else 
%         tmp_reg.(['regressors' num2str(block_n)]).alexChoice = ...
%         blk.time.decide10hz(((block_n-1)*intv)+1:end);
%     end
%     
%     % create regressors for alexFeedback
%     if block_n ==1
%     tmp_reg.(['regressors' num2str(block_n)]).alexFeed = ...
%         blk.time.feed10hz(block_n:block_n*intv);
%     elseif block_n ==2
%         tmp_reg.(['regressors' num2str(block_n)]).alexFeed = ...
%         blk.time.feed10hz(intv+1:block_n*intv);
%     else 
%         tmp_reg.(['regressors' num2str(block_n)]).alexFeed = ...
%         blk.time.feed10hz(((block_n-1)*intv)+1:end);
%     end
%     
%     
%     % create regressors for alexActions
%     
%     for n_stim = [2 3 7]
%         if block_n ==1
%             tmp_reg.(['regressors' num2str(block_n)]).(['alexAction_' num2str(n_stim)]) = ...
%                 blk.time.(['action_' num2str(n_stim)])(block_n:block_n*intv);
%         elseif block_n ==2
%             tmp_reg.(['regressors' num2str(block_n)]).(['alexAction_' num2str(n_stim)]) = ...
%                 blk.time.(['action_' num2str(n_stim)])(intv+1:block_n*intv);
%         else
%             tmp_reg.(['regressors' num2str(block_n)]).(['alexAction_' num2str(n_stim)]) = ...
%                 blk.time.(['action_' num2str(n_stim)])(((block_n-1)*intv)+1:end);
%         end
%     end

%--------------------------------------------------------------------------------------------------------
    
%     % Faux expected value regressor -- with stim. onset
%     % NB: when looking at the time of choice, we use the EV estimate from
%     % the previous trial
%     b.fauxEV = alexcausfilt(b.stim_ACC);
%     b.fauxEVt = [0 b.fauxEV(1:end-1)];
%     event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = b.stim_RTTime(x(block_n,:));
%     tmp_reg.(['regressors' num2str(block_n)]).fauxEV_RT(:,1) = ...
%         createSimpleRegressor(event_beg,event_end,epoch_window,b.fauxEVt(x(block_n,:)));
%     
%     event_beg = b.stim_OnsetTime(x(block_n,:)); event_end = b.stim_OnsetTime(x(block_n,:))+200;
%     tmp_reg.(['regressors' num2str(block_n)]).fauxEV_stim_onset(:,1) = ...
%         createSimpleRegressor(event_beg,event_end,epoch_window,b.fauxEVt(x(block_n,:)));
%     
%     % Faux expected value regressos -- with feedback
%     % NB: when looking at the feedback time window, we use the EV estimate that integrates
%     % the current trial
%     event_beg = b.feedback_OnsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
%     tmp_reg.(['regressors' num2str(block_n)]).faux_EV_feedback(:,1) = ...
%         createSimpleRegressor(event_beg,event_end,epoch_window,b.fauxEV(x(block_n,:))); 
%     
%     
%     % Faux prediction error -- with feedback
%     b.fauxPE = b.stim_ACC - [0 alexcausfilt(b.stim_ACC(1:end-1))]';
%     event_beg = b.feedback_OffsetTime(x(block_n,:)); event_end = event_beg+feedback_end;
%     tmp_reg.(['regressors' num2str(block_n)]).fauxPE(:,1) = ...
%         createSimpleRegressor(event_beg,event_end,epoch_window,b.fauxPE(x(block_n,:)));
%     
     % -- TD model regressor --
    
    % HRF-convolve all the event regressors
    hrfregs = fieldnames(rmfield(tmp_reg.regressors1,'to_censor'));
    for n = 1:numel(hrfregs)
        % b.hrfreg1.RT
        tmp_reg.(['hrfreg' num2str(block_n)]).(hrfregs{n}) = ...
            conv(1*tmp_reg.(['regressors' num2str(block_n)]).(hrfregs{n}),hemoir);
        
        % cut off the tail after convolution and downsample
        tmp = gsresample(tmp_reg.(['hrfreg' num2str(block_n)]).(hrfregs{n}),10,1./scan_tr);
        tmp_reg.(['hrfreg' num2str(block_n)]).(hrfregs{n}) = tmp(1:block_length);
    end
    
    % shift the tocensor regressor by the HRF lag = 5 seconds
    tmp_reg.(['hrfreg' num2str(block_n)]).to_censor = ...
        gsresample( ...
            [zeros(50,1)' tmp_reg.(['regressors' num2str(block_n)]).to_censor(1:end-51)], ...
        10,1./scan_tr);
    
%     %censor first 7 TR's for 2nd and 3rd block
%     % in 100ms bins 7*750ms = 5250ms ~ 5.2/5.3 seconds
%     if (block_n==2 || block_n==3)
%         tmp_reg.(['regressors' num2str(block_n)]).to_censor(52:104)=0;
%     end
end


% concatenate everything
fnm = fieldnames(tmp_reg.regressors1)';
% for ct=1:length(fnm)
%     b.hrf_regs.(fnm{ct}) = [tmp_reg.hrfreg1.(fnm{ct}) tmp_reg.hrfreg2.(fnm{ct}) tmp_reg.hrfreg3.(fnm{ct})];
% end

%Added switch case for subjects with irregular trials
ct=1:length(fnm);
switch num_blocks
    case 1
        for ct=1:length(fnm)
            b.hrf_regs.(fnm{ct}) = [tmp_reg.hrfreg1.(fnm{ct})];
        end
    case 2
        for ct=1:length(fnm)
            b.hrf_regs.(fnm{ct}) = [tmp_reg.hrfreg1.(fnm{ct}) tmp_reg.hrfreg2.(fnm{ct})];
        end
    case 3
        for ct=1:length(fnm)
            b.hrf_regs.(fnm{ct}) = [tmp_reg.hrfreg1.(fnm{ct}) tmp_reg.hrfreg2.(fnm{ct}) tmp_reg.hrfreg3.(fnm{ct})];
        end
    otherwise
        disp('Error occured somewhere')
end



% b.hrfreg1=tmp_reg.hrfreg1;
% b.hrfreg2=tmp_reg.hrfreg2;
% b.hrfreg3=tmp_reg.hrfreg3;

% flip and round to_censor for AFNI 1=analyze, 0=censor
b.hrf_regs.to_censor = 1-(ceil(b.hrf_regs.to_censor));
b.hrf_regs.to_censor = b.hrf_regs.to_censor(1:length(b.hrf_regs.RT));
% b.hrfreg1.to_censor = 1-(ceil(b.hrfreg1.to_censor));
% b.hrfreg2.to_censor = 1-(ceil(b.hrfreg2.to_censor));
% b.hrfreg3.to_censor = 1-(ceil(b.hrfreg3.to_censor));


% save individual file
%save(sprintf([pathroot 'analysis/bandit/fmri/data/%d.mat'],id),'b');
%save(sprintf('C:/regs/bandit%d.mat',id),'b');
save(sprintf('c:/kod/fMRI/bandit_regs/bandit%d.mat',id),'b');

%cd('C:\regs')
cd('c:/kod/fMRI/bandit_regs')
gdlmwrite(sprintf('bandit%d.regs',id),[b.hrf_regs.to_censor' ... %0 trials with responses
    b.hrf_regs.RT' b.hrf_regs.feedback' ... % 1 RT    2 feedback
    b.hrf_regs.switch_RT'... %3 switch convolved with RT window
    b.hrf_regs.error_feedback' ...%4 error convolved with feedback window
    b.hrf_regs.correct_feedback' ...%5 correct convolved with feedback window
    b.hrf_regs.action_2' ...%6 right index 
    b.hrf_regs.action_3' ...%7 right middle
    b.hrf_regs.action_7' ...%8 left index 
    b.hrf_regs.error2correct1' ...%9 error=2, correct=1
    b.hrf_regs.choice' ...%10 "choice" - 500ms following stimulus presentation
    b.hrf_regs.switch_choice' ...%11 switch convolved with "choice" - 500ms following stimulus presentation
    b.hrf_regs.switch_feedback' ...%12 switch convolved with feedback
    b.hrf_regs.echosen' ...%13 estimated EV at feedback
    b.hrf_regs.deltaplus' ...%14 PE+ at feedback       
    b.hrf_regs.deltaminus' ...%15 PE- at feedback       
    b.hrf_regs.echosen' ...%16 EV of subsequently chosen stim at choice
    b.hrf_regs.alexChoice' ...%17 Alex's older method of determining RT's
    b.hrf_regs.alexFeed' ...%18 Alex's older method of determining RT's, Feedback
    b.hrf_regs.alexAction_2' ...%19 Alex's method actions right index
    b.hrf_regs.alexAction_3' ...%20 Alex's method actions right middle
    b.hrf_regs.alexAction_7' ...%21 Alex's method actions left index
    b.hrf_regs.alexDeltaplus' ...%22 Alex model PE+ at feedback
    b.hrf_regs.alexDeltaminus' ...%23 Alex model PE- at feedback
    b.hrf_regs.alexEchosen' ...%24 Alex model EV of subsequently chosen stim at choice
    b.hrf_regs.anyAction' ...%25 Any valid stimulus chosen by subject
    b.hrf_regs.alexAnyaction' ...%26 Any valid stimulus chosen by subject
    b.hrf_regs.prevFeed' ...%27 trial(2) = trial(1) Feedback Onset + 300ms, ect
    b.hrf_regs.alexDeltaplus_MC' ...%28 mean centered PE+
    b.hrf_regs.alexDeltaminus_MC' ...%29 mean centered PE-
    b.hrf_regs.alexEchosen_MC' ...%30 Mean centered EV
    b.hrf_regs.Value_diff_best' ...%31 best value squared difference 
    b.hrf_regs.Value_diff_all' ...%32 trial-wise sum squared difference
    
    ],'\t');

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
function xout = getfmriData(id,vars,procedure)
% reads eprime data for a given bandit subject based on ID

% Find the eprime file
%data_dir  = [pathroot 'analysis/bandit/fmri/data/raw/'];
data_dir  = 'C:/kod/fMRI/bandit_raw/'; %for testing purposes
file_name = ls([data_dir sprintf('%d/*vrbl_scanner*.txt',id)]);
%fpath     = @(~) [pathroot sprintf('analysis/bandit/fmri/data/raw/%d/%s',id,file_name)];
fpath     = @(~) [ data_dir sprintf('%d/%s',id,file_name)];

% read in the data (make sure range to search raw text is correct)
xout = eprimeread(fpath(),procedure,vars,0,-13,18);

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
