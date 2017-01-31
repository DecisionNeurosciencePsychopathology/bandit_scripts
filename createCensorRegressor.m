function b=createCensorRegressor(b,num_blocks)
%This is a file of 1s and 0s, indicating which     
%time points are to be included (1) and which are  
%to be excluded (0).

%NOTE: this maybe confusing, by censor I actually mean which trials to
%remove. because after the createSimpleRegressor fx is ran, I subtract
%everthing by 1 to correctly flip, thus including all the trials we
%originally wanted to keep. Then after reampling to TR's there are some
%values which are not a 1 or 0. So we need to round using either ceil
%(currently), round, or floor. Not sure which is best or if it has that
%large of an impact. I'm not really sure why we subtract everything by 1
%after rounding, but reagrdless we need to flip it again so we take the
%opposite logical.

frequency_scale_hz = 10;
scan_tr = .75;
tr = .1;
hemoir = spm_hrf(tr, [6,16,1,1,6,0,32]); % could stand to be tuned...
block_length = 925; %But its really 925 ask Alex!
b.stim_NextOnsetTime=[b.stim_OnsetTime(2:end); b.stim_RTTime(end)];

%Do a quick check for irregular times and note them
if(any((b.stim_NextOnsetTime-b.stim_OnsetTime) < 0))
    warning('Irregular onset times, just using offset time ')
    b.stim_NextOnsetTime = b.stim_OffsetTime + min(b.stim_OffsetTime-b.stim_OnsetTime); %Add buffer stick
    b.stim_NextOnsetTime(end) = b.stim_RTTime(end); % just to keep same convention
end


%b.missed_responses = ( b.stim_RT == 0 ); %Only included no responses
b.missed_responses = ( b.chosen_stim == 999 ); %Includes wrong button presses and rt == 0
b.trials_to_censor = b.missed_responses;

b.loss_trials = (b.stim_ACC | b.trials_to_censor); %Subject did not win but made a choice.

%b.trials_to_censor(b.rewardVec==0) = 1; %Adding in the trials in which there was no reward as well.

for block_n = 1:num_blocks
        %Set up trial ranges
    trial_index_1 = b.trial_index(block_n);
    trial_index_2 = trial_index_1 + b.trials_per_block-1;
    
    bin_size = 1/frequency_scale_hz*1000; % convert Hz to msec
    epoch_window = b.stim_OnsetTime(trial_index_1:trial_index_2):bin_size:b.stim_OnsetTime(trial_index_1:trial_index_2)+scan_tr*block_length*1000;    
    event_beg = b.stim_OnsetTime(trial_index_1:trial_index_2); event_end = b.stim_NextOnsetTime(trial_index_1:trial_index_2);
    
    
    tmp_reg.(['regressors' num2str(block_n)]).to_censor = ...
        createSimpleRegressor(event_beg, event_end, epoch_window, b.trials_to_censor(trial_index_1:trial_index_2));
    tmp_reg.(['regressors' num2str(block_n)]).to_censor = ones(size(tmp_reg.(['regressors' num2str(block_n)]).to_censor)) - tmp_reg.(['regressors' num2str(block_n)]).to_censor; %flip it the correct way
    
    %It is ~b.stim_ACC becase we want to CENSOR the losses i.e. they have a
    %1 or TRUE value
    tmp_reg.(['regressors' num2str(block_n)]).win_censor = ...
        createSimpleRegressor(event_beg, event_end, epoch_window, ~b.stim_ACC(trial_index_1:trial_index_2));
    tmp_reg.(['regressors' num2str(block_n)]).win_censor = ones(size(tmp_reg.(['regressors' num2str(block_n)]).win_censor)) - tmp_reg.(['regressors' num2str(block_n)]).win_censor;
    
    tmp_reg.(['regressors' num2str(block_n)]).loss_censor = ...
        createSimpleRegressor(event_beg, event_end, epoch_window, b.loss_trials(trial_index_1:trial_index_2));
    tmp_reg.(['regressors' num2str(block_n)]).loss_censor = ones(size(tmp_reg.(['regressors' num2str(block_n)]).loss_censor)) - tmp_reg.(['regressors' num2str(block_n)]).loss_censor;
    
    
    
    
    hrfregs = fieldnames(tmp_reg.regressors1)';
    for n = 1:numel(hrfregs)
    % NB: the first 5s (or TRs?) are censored because they capture HRF to events
    % preceding the first trial
    tmp_reg.(['hrfreg' num2str(block_n)]).(hrfregs{n}) = ...
        gsresample( ...
            [zeros(50,1)' tmp_reg.(['regressors' num2str(block_n)]).(hrfregs{n})(1:end-51)], ...
        10,1./scan_tr);
    end
    

        
end





fnm = fieldnames(tmp_reg.regressors1)';
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

%Only subtract everything by 1 if you built the censor to remove trials
%with a TRUE value for that time point, i.e. I want to remove trial x so
%its vvalue is a 1, threrfore I must subjtract everything by 1 later since
%1=keep 0 = remove.
b.hrf_regs.to_censor = 1-(ceil(b.hrf_regs.to_censor));
b.hrf_regs.to_censor = ~b.hrf_regs.to_censor;

b.hrf_regs.win_censor = 1-(ceil(b.hrf_regs.win_censor));
b.hrf_regs.win_censor = ~b.hrf_regs.win_censor;

b.hrf_regs.loss_censor = 1-(ceil(b.hrf_regs.loss_censor));
b.hrf_regs.loss_censor = ~b.hrf_regs.loss_censor;
%b.hrf_regs.to_censor = b.hrf_regs.to_censor(1:length(b.hrf_regs.RT));
foo=0;



function foo = createSimpleRegressor(event_begin,event_end,epoch_window,conditional_trials)
% this was not a problem earlier, but for some reason it is now: find indices that would
% result in a negative value and set them both to 0
qbz = ( event_begin == 0 ); qez = ( event_end == 0 );
event_begin( qbz | qez ) = 0; event_end( qbz | qez ) = 0;

% check if optional censoring variable was used
if(~exist('conditional_trials','var') || isempty(conditional_trials))
    conditional_trials = true(length(event_begin),1);
elseif(~islogical(conditional_trials))
    % needs to be logical format to index cells
    conditional_trials = logical(conditional_trials);
end

% this only happened recently, but it's weird
if(any((event_end(conditional_trials)-event_begin(conditional_trials)) < 0))
    error('MATLAB:bandit_fmri:time_travel','feedback is apparently received before RT');
end

% create epoch windows for each trial
epoch = arrayfun(@(a,b) a:b,event_begin,event_end,'UniformOutput',false);

% for each "epoch" (array of event_begin -> event_end), count events
% per_event_histcs = cellfun(@(h) histc(h,epoch_window),epoch(conditional_trials),'UniformOutput',false);
% foo = logical(sum(cell2mat(per_event_histcs),1));

foo = zeros(size(epoch_window));

for n = 1:numel(epoch)
    if(conditional_trials(n))
        foo = logical(foo + histc(epoch{n},epoch_window));
    end
end


% createAndCatRegs(event_begin,event_end,epoch_window);

return