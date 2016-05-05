function b = banditmakeregressor_vba(b,out)

fprintf('\nCreating subject specific regressor files\n\n');


data_dump_str = strcat('E:\data\bandit\regs\', num2str(b.id));
b.regs = [];
n_t = length(b.stim_ACC); %Length of trials
total_blocks = 3;
b.trials_per_block = n_t/total_blocks;
stick = 1000; %ms (duration for feedback events)

firstfix_Onset = b.stim_OnsetTime(1); %very first time point
b.trial_index = 1:b.trials_per_block:total_blocks*b.trials_per_block;

%Store these for later use
% b.trial_index = trial_index;
% b.trials_per_block = b.trials_per_block;

%% Find find where the mystery and computer trials are, use to censor later
myst_cens = 'myst';
comp_cens = 'comp';
prefix=cellfun(@(x) x(1:4), b.protocol_type, 'UniformOutput', false);
myst_index = cell2mat(cellfun(@(x) isequal(x,myst_cens), prefix, 'UniformOutput', false));
comp_index = cell2mat(cellfun(@(x) isequal(x,comp_cens), prefix, 'UniformOutput', false));
%Make choice and feedback censors
b.choice_censor = ~((b.stim_RT==0) + comp_index); %remove computer trials
b.feedback_censor = ~((b.stim_RT==0) + myst_index); %remove mystery trials

%Create volume-wise censor regressor
b=createCensorRegressor(b,total_blocks);


%% Pre-allocate memory for regressor time structures
decision.event_beg=zeros(b.trials_per_block,total_blocks);
decision.event_end=zeros(b.trials_per_block,total_blocks);
feedback.event_beg=zeros(b.trials_per_block,total_blocks);
feedback.event_end=zeros(b.trials_per_block,total_blocks);

%% Main loop
for block= 1:total_blocks
    
    %Set up trial ranges
    trial_index_1 = b.trial_index(block);
    trial_index_2 = trial_index_1 + b.trials_per_block-1;
    
    %for decision onset to response (motor response)
    decision.event_beg(:,block) = b.stim_OnsetTime(trial_index_1:trial_index_2)-firstfix_Onset;
    %decision.event_end(:,block) = b.stim_OffsetTime(trial_index_1:trial_index_2)-firstfix_Onset;
    decision.event_end(:,block) = b.stim_OnsetTime(trial_index_1:trial_index_2)-firstfix_Onset + b.stim_RT(trial_index_1:trial_index_2); %Duration should be length of RT
    
    %for feedback onset to offset
    feedback.event_beg(:,block) = b.feedback_OnsetTime(trial_index_1:trial_index_2)-firstfix_Onset;
    feedback.event_end(:,block) = b.feedback_OnsetTime(trial_index_1:trial_index_2)-firstfix_Onset+stick; %Just make the durations all a 1 second stick
    %feedback.event_end(:,block) = b.feedback_OffsetTime(trial_index_1:trial_index_2)-firstfix_Onset;
    
    trial.event_beg(:,block) = b.stim_OnsetTime(trial_index_1:trial_index_2)-firstfix_Onset;
    trial.event_end(:,block) = b.feedback_OffsetTime-firstfix_Onset; %From stim onset to feedback offset
    
    if block<total_blocks
        %Update the first fix onset to be the first trial in the next block
        firstfix_Onset = b.stim_OnsetTime(trial_index_2+1);
    end
end

%Reshape into single vector
decision.event_beg=reshape(decision.event_beg,[n_t,1]);
decision.event_end=reshape(decision.event_end,[n_t,1]);
feedback.event_beg=reshape(feedback.event_beg,[n_t,1]);
feedback.event_end=reshape(feedback.event_end,[n_t,1]);
trial.event_beg=reshape(feedback.event_beg,[n_t,1]);
trial.event_end=reshape(feedback.event_end,[n_t,1]);

%% Decision aligned Regressors
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'decision_Times',b.choice_censor,0,b);
[b.stim_times.comp_fsl,b.stim_times.comp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'comp_trials',comp_index,0,b);
[b.stim_times.myst_fsl,b.stim_times.myst_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'myst_trials',myst_index,0,b);

% Value Regressors
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'valueDecisionAligned',out.suffStat.value',0,b);
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'valueDecisionAligned_diff',out.suffStat.value_diff',0,b);
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'valueDecisionAligned_chosen',out.suffStat.value_chosen',0,b);
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'valueDecisionAligned_chosen_diff',out.suffStat.value_chosen_diff',0,b);
[b.stim_times.resp_fsl,b.stim_times.resp_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'valueDecisionAligned_chosen_diff_standardized',out.suffStat.value_chosen_diff_standardized',0,b);

% Stake vector
[b.stim_times.rew_stake,b.stim_times.rew_stake]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'stakeDecisionAligned',out.suffStat.stake',0,b);

% Motor Regressors
% right = 2; left = 7;
b.right = (b.stim_RESP==2);
b.left = (b.stim_RESP==7);
[b.stim_times.left_fsl,b.stim_times.left_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'left',b.left,0,b);
[b.stim_times.right_fsl,b.stim_times.right_spmg]=write3Ddeconv_startTimes(data_dump_str,decision.event_beg,decision.event_end,'right',b.right,0,b);


%% Feedback aligned Regressors
[b.stim_times.feedback_fsl,b.stim_times.feedback_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'feedback_Times',b.feedback_censor,0,b);

% PE regressors
[b.stim_times.unsignedpes_fsl,b.stim_times.unsignedpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'unsignedPEs',out.suffStat.PEunsigned',0,b);
[b.stim_times.signedpes_fsl,b.stim_times.signedpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'signedPEs',out.suffStat.PEsigned',0,b);
[b.stim_times.chosenpes_fsl,b.stim_times.chosenpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'chosenPosPEs',out.suffStat.PEchosen_pos',0,b);
[b.stim_times.chosenpes_fsl,b.stim_times.chosenpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'chosenNegPEs',out.suffStat.PEchosen_neg',0,b);
[b.stim_times.chosenpes_fsl,b.stim_times.chosenpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'chosenPEs',out.suffStat.PEchosen',0,b);
[b.stim_times.chosenpes_fsl,b.stim_times.chosenpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'chosenPEs_standardized',out.suffStat.PEchosen_standardized',0,b);
[b.stim_times.pospes_fsl,b.stim_times.pospes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'posPEs',out.suffStat.PEplus',0,b);
[b.stim_times.negpes_fsl,b.stim_times.negpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'negPEs',out.suffStat.PEminus',0,b);
plusMinusPE=(out.suffStat.PEplus+out.suffStat.PEminus*-1)'; %Combine the two into one regressor
[b.stim_times.pos_negpes_fsl,b.stim_times.pos_negpes_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'pos_negPEs',plusMinusPE,0,b);

% Value Regressors -- aligned with feedback
[b.stim_times.val_fsl,b.stim_times.val_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'valueFeedbackAligned',out.suffStat.value',0,b);
[b.stim_times.valdiff_fsl,b.stim_times.valdiff_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'valueFeedbackAligned_diff',out.suffStat.value_diff',0,b);
[b.stim_times.valchosen_fsl,b.stim_times.valchosen_spmg]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'valueFeedbackAligned_chosen',out.suffStat.value_chosen',0,b);

% Stake vector
[b.stim_times.rew_stake,b.stim_times.rew_stake]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'stakeFeedbackAligned',out.suffStat.stake',0,b);

% Reward-Stake aligned with Feedback
[b.stim_times.rew_stake,b.stim_times.rew_stake]=write3Ddeconv_startTimes(data_dump_str,feedback.event_beg,feedback.event_end,'rewardMagnitudeFeedbackAligned',out.suffStat.reward_stake',0,b);

%% Trial length regressors
[b.stim_times.rew_stake,b.stim_times.rew_stake]=write3Ddeconv_startTimes(data_dump_str,trial.event_beg,trial.event_end,'rewardMagnitudeTrialAligned',out.suffStat.reward_stake',0,b);

%% Censor file
gdlmwrite([data_dump_str 'banditCensorOnly.regs'],b.hrf_regs.to_censor');



function [x,y]=write3Ddeconv_startTimes(file_loc,event_beg,event_end,fname,modulator,noFSL,b)
% Function will write FSL styled regressors in dat files for fMRI analysis
% Inputs:
% file_loc: file location (str)
% event_beg: the time in miliseconds of the event beginning
% event_end: the time in milliseconds of the event ending
% fname: the file names
% censor: the censor vector or parametric value vector depending on the regressor
% noFSL either to write a FSL file or a different single line version (see 3dDeconvolve help for more info)
% trial_index: the position of when a new block starts (trialwise)

if nargin <6
    %censor = 1;
    noFSL=0;
end
format long
x(:,1) = event_beg';
x(:,2) = event_end'-event_beg';
x=x./1000; %Convert to seconds
x(:,3) = ones(length(x),1).*modulator; %originally was modulator'
%write the -stim_times_FSL

if ~noFSL
    %Save to regs folder
    %dlmwrite([file_loc fname '.dat'],x,'delimiter','\t','precision','%.6f')
    c = asterisk(x,b); %Add in asterisks and clean up data
    dlmcell([file_loc fname '.dat'],c,'delimiter','\t')
    %dlmcell([data_dump_str filename],c,'\t');
    y=0;
else
    %write the -stim_times file
    fname = [fname '_noFSL'];
    y = x(logical(x(:,3)),1)';
    %Quick fix hack for just first ten trials troubleshoot SPMG2
    %y = y(1:10);
    dlmwrite([file_loc fname '.dat'],y,'delimiter','\t','precision','%.6f')
end
return

function c = asterisk(x,b)
%adding asterisk to existing .dat files also removes any nans present

c=[];
ast = {'*', '*', '*'};
for i = 1:length(b.trial_index)
    %Set up trial ranges
    trial_index_1 = b.trial_index(i);
    trial_index_2 = trial_index_1 + b.trials_per_block-1;
    block_data = num2cell(x(trial_index_1: trial_index_2,:));
    if i<length(b.trial_index)
        c = [c; block_data; ast];
    else
        c = [c; block_data;];
    end
end

%clean up any nans
%fh = @(y) all(isnan(y(:)));
c = c(~any(cellfun(@isnan,c),2),:);
%c(cellfun(fh, c)) = [];
%Check on c!

return
