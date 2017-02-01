function [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results)

%% fits BANDIT rl model to 3 armed bandit subject data using VBA toolbox
% example call:
% [posterior,out]=bandit_vba(id, multinomial,multisession, saveresults, graphics)
% id:           6-digit subject id from subject database
% multinomial:  if 1 fits p_chosen from the softmax; continuous RT (multinomial=0) works less well
% multisession: treats runs/conditions as separate, helps fit (do not allow X0 to vary though)
% fixed_params_across_runs -- self-explanatoryr
% n_steps:      number of time bins
%%
close all


if nargin<2
    graphics=0;
    plot_subject=0;
elseif nargin<3
    plot_subject =0;
    valence = 1;
end


%I think it would be easier just to not make this an argument
use_reward_vec = 0;


%% Where to look for data
%Quick username check, and path setting

[~, me] = system('whoami');
me = strtrim(me);
if save_results
    file_path = 'vba_output';
else
    error('something went wrong with the vba output dir!')
end

%Evolution options
options.inF.utility = 0;
options.inF.valence = 0;
options.inF.decay = 0;


%Turn graphics on or off
if ~graphics
    options.DisplayWin = 0;
    options.GnFigs = 0;
end
%% set up dim defaults
if valence
    n_theta = 3; %Number of evolution params (AlphaWin AlphaLoss LossDecay WinDecay)
    options.inF.valence = 1;
else
    n_theta = 2;
end

if utility
    n_theta = n_theta +1; %Add in steepness parameter
    options.inF.utility = 1;
end


if ~decay
    n_theta = n_theta-1;
    options.inF.decay = 0;
else
    options.inF.decay = 1;
end

n_phi = 1; %Number of observation params (Beta)
f_name = @f_bandit_Qlearn; %Evolution function
g_name = @g_bandit_softmax; %Observation function
n_t = 300; %Total number of trials
% n_runs = 3; %3 blocks total
n_hidden_states = 4; %Track value for each arm of the bandit + PE

%% Load in the subject's data
%u is 2 x ntrials where first row is actions and second row is reward
b = bandit_vba_read_in_data( 'id',id,'data_dir','subjects'); %REPLACE subjects with local dir
b.id = id;
censor = b.chosen_stim==999; %Censor some trials first
subjects_actions = b.chosen_stim;
subjects_actions(censor)=nan;
u(1,:) = subjects_actions; %Chosen action [1 2 3]
if use_reward_vec
    u(2,:) = b.rewardVec; %Reward has actual value [10 25 50]
else
    u(2,:) = b.stim_ACC; %Reward or not [1 0]
end
u = [zeros(size(u,1),1) u(:,1:end-1)]; %Shift the u!


y = zeros(3, n_t);
for i = 1:n_t
    try
        y(subjects_actions(i), i) = 1;
    catch
        y(:,i) = nan;
    end
end

%% set up models within evolution/observation Fx
options.inF.b = b;
options.inG.b = b;

%% skip first trial
options.skipf = zeros(1,n_t);
options.skipf(1) = 1;

options.binomial = 1;

%% split into conditions/runs
% if multisession %improves fits moderately
%     options.multisession.split = repmat(n_t/n_runs,1,n_runs); % three runs of 100 datapoints each
%     %% fix parameters
%     if fixed_params_across_runs
%         options.multisession.fixed.theta = 'all';
%         options.multisession.fixed.phi = 'all';
%         %
%         % allow unique initial values for each run?x
%         options.multisession.fixed.X0 = 'all';
%     end
%
% end

%% defined number of hidden states and parameters
dim = struct('n',n_hidden_states,'n_theta',n_theta,'n_phi',n_phi, 'n_t', n_t);

%% priors
priors.muPhi = zeros(dim.n_phi,1);
priors.muTheta = zeros(dim.n_theta,1);

if utility
    priors.muTheta(end) = -2;
end

priors.muX0 = zeros(n_hidden_states,1);
priors.SigmaTheta = 1e1*eye(dim.n_theta);
%priors.SigmaPhi = diag([1,1,1]);
priors.SigmaPhi = 1e1*eye(dim.n_phi);
priors.SigmaX0 = 0*eye(dim.n);
priors.a_alpha = Inf;
priors.b_alpha = 0;
priors.a_sigma = 1;     % Jeffrey's prior
priors.b_sigma = 1;     % Jeffrey's prior

options.priors = priors;
%options.inG.priors = priors; %copy priors into inG for parameter transformation (e.g., Gaussian -> uniform)

%% Last bit of option declarations
options.TolFun = 1e-6;
options.GnTolFun = 1e-6;
options.verbose=1;

%Censor any bad trials
options.isYout = repmat(censor,1,3)';
options.inF.Yout = options.isYout;

%% Run the vba model
[posterior,out] = VBA_NLStateSpaceModel(y,u,f_name,g_name,dim,options);

%Plot the subjects choices if needed
if plot_subject
    plot_subject_vs_contingency(b,out)
end

% if saveresults
%     cd(results_dir);
%     %% save output figure
%     % h = figure(1);
%     % savefig(h,sprintf('results/%d_%s_multinomial%d_multisession%d_fixedParams%d',id,model,multinomial,multisession,fixed_params_across_runs))
%     save(sprintf('SHIFTED_CORRECT%d_%s_multinomial%d_multisession%d_fixedParams%d_uaversion%d_sceptic_vba_fit', id, model, multinomial,multisession,fixed_params_across_runs, u_aversion), 'posterior', 'out');
% end

%% get prediction errors
% alphaWin = 1./(1+exp(-posterior.muTheta(1)));
% alphaLoss = 1./(1+exp(-posterior.muTheta(2)));
% win_index = u(2,:)==1; %Determine which alpha to use
% diff_of_muX = diff(choices,1,2);
% 
% %winning_trials = find(u(2,2:end)==1 & ~ismember(1:n_t-1,bad_trials));

% %diff_of_muX = [[0 0 0]' diff_of_muX];
% for i = 1:length(diff_of_muX)
%     %Filter bad trials
%     if ismember(i,bad_trials-1) %Becasue the u is shifted we didn't want to subject 1 from bad trials yet, now that we are realigned we do. I think.
%         out.suffStat.PE(i)=0;
%     elseif ismember(i,winning_trials)
%         muX_per_trial(i) = diff_of_muX(subjects_actions(i),i);
%         out.suffStat.PE(i)=diff_of_muX(subjects_actions(i),i)/alphaWin;
%     else
%         muX_per_trial(i) = diff_of_muX(subjects_actions(i),i);
%         out.suffStat.PE(i)=diff_of_muX(subjects_actions(i),i)/alphaLoss;
%     end
% end
%
%
%
% %diff_of_muX = diff(-diff_of_muX);
% out.suffStat.PE(win_index) = diff_of_muX(win_index)./alphaWin; %Handle wins
% out.suffStat.PE(~win_index) = diff_of_muX(~win_index)./alphaLoss; %Handle losses

bad_trials = find(isnan(u(1,:)));
winning_trials = find((b.stim_ACC==1)' & ~ismember(1:n_t,bad_trials-1));
losing_trials = find((b.stim_ACC==0)' & ~ismember(1:n_t,bad_trials-1));

%Seperate the hidden states
choices = out.suffStat.muX(1:3,:);
delta = out.suffStat.muX(4,:);

%Shift PE Regressor if needed...
out.suffStat.delta = shiftMe(delta);

%SHOULD THESE CHANGE?
%Values are shifted to the left add on as many zeros as needed to regain
%proper length
muX_diff = [diff(choices,1,2) zeros(size(choices,1),1)];
PEunsigned = max(abs(muX_diff));
%PEunsigned = [PEunsigned 0]; %Tack on zero to the end?
PEplus = zeros(1,length(b.stim_ACC));
PEminus = zeros(1,length(b.stim_ACC));
out.suffStat.PEchosen_pos = zeros(1,length(b.stim_ACC));
out.suffStat.PEchosen_neg = zeros(1,length(b.stim_ACC));
PEplus(winning_trials) = PEunsigned(winning_trials); %Good!
PEminus(losing_trials) = PEunsigned(losing_trials); %Out of bounds because losing trials contains index 300
out.suffStat.PEunsigned=PEunsigned;
out.suffStat.PEplus=PEplus;
out.suffStat.PEminus=PEminus;
out.suffStat.PEsigned=PEplus-PEminus;
%Grab the option they chose and remove any error codes
chosen_index = y;
chosen_index = carryValueForward(chosen_index,y); %If there are any Nan's replace them with the most recent decision made

%PEchosen is now the pe hidden state
%out.suffStat.PEchosen = muX_diff(logical(chosen_index))'; %PE of chosen choice
out.suffStat.PEchosen = out.suffStat.delta; %PE of chosen choice
out.suffStat.PEchosen_pos(out.suffStat.PEchosen>0) = out.suffStat.PEchosen(out.suffStat.PEchosen>0);
out.suffStat.PEchosen_neg(out.suffStat.PEchosen<0) = out.suffStat.PEchosen(out.suffStat.PEchosen<0);

%Create Value regressors
out.suffStat.value = max(choices); %Max value of each hidden state per trial
out.suffStat.value_diff = out.suffStat.value - mean(choices); 
out.suffStat.value_chosen = choices(logical(chosen_index))';
out.suffStat.value_not_chosen=choices(~logical(chosen_index))';
out.suffStat.value_not_chosen = reshape(out.suffStat.value_not_chosen,2,300);
not_chosen_sum=sum(out.suffStat.value_not_chosen); %Keep this var on ice for now
out.suffStat.value_chosen_diff = out.suffStat.value_chosen - mean(out.suffStat.value_not_chosen); %Do not shift this one!
out.suffStat.value_chosen=out.suffStat.value_chosen./sum(choices); 

%Shift all the value regressors by 1
out.suffStat.value=shiftMe(out.suffStat.value);
out.suffStat.value_diff=shiftMe(out.suffStat.value_diff);
out.suffStat.value_chosen=shiftMe(out.suffStat.value_chosen);
out.suffStat.value_not_chosen=shiftMe(out.suffStat.value_not_chosen);
out.suffStat.value_chosen=shiftMe(out.suffStat.value_chosen);
%out.suffStat.value_chosen(1) = 0; %This is a NAN since 0/0

%Standardize the PEChosen and ValueChosenDiff regs

%zscore is only in stat toolbox?
try
out.suffStat.value_chosen_diff_standardized = ...
    zscore(out.suffStat.value_chosen_diff);
out.suffStat.PEchosen_standardized = ...
    zscore(out.suffStat.PEchosen);
catch
    fprintf('Stat toolbox not found!\n\n')
end

%Create reward stake and just stake align it with trialonset
out.suffStat.reward_stake = b.rewardVec';
out.suffStat.reward_stake(b.rewardVec==10)=1;
out.suffStat.reward_stake(b.rewardVec==25)=2;
out.suffStat.reward_stake(b.rewardVec==50)=3;

%mean corrected rew mag
out.suffStat.reward_stake_mc = out.suffStat.reward_stake - mean(out.suffStat.reward_stake);

%This is what they could have won per trial <- what we want for createing the probabilities
out.suffStat.stake = b.stakeVec';
out.suffStat.stake(b.stakeVec==10)=1;
out.suffStat.stake(b.stakeVec==25)=2;
out.suffStat.stake(b.stakeVec==50)=3;

%Mean corrected stake regressor
out.suffStat.stake_mc = out.suffStat.stake - mean(out.suffStat.stake);


%Percentages of reward magnitude and staying
out.suffStat.mag10_trials = find(b.stakeVec==10);
out.suffStat.mag25_trials = find(b.stakeVec==25);
out.suffStat.mag50_trials = find(b.stakeVec==50);

%Determine number of trials with specific magnitude that were win/loss
out.suffStat.win_10_trials = intersect(winning_trials,out.suffStat.mag10_trials);
out.suffStat.win_25_trials = intersect(winning_trials,out.suffStat.mag25_trials);
out.suffStat.win_50_trials = intersect(winning_trials,out.suffStat.mag50_trials);
out.suffStat.loss_10_trials = intersect(losing_trials,out.suffStat.mag10_trials);
out.suffStat.loss_25_trials = intersect(losing_trials,out.suffStat.mag25_trials);
out.suffStat.loss_50_trials = intersect(losing_trials,out.suffStat.mag50_trials);


%Find stay trials
error_code = 999; %If they missed a trial, ie don't want two zeros in a row to count...
out.suffStat.stay_trials  = find(logical([0; b.chosen_stim(2:end)==b.chosen_stim(1:end-1)]) & b.chosen_stim~=error_code);

%These aren;t really nedded but its good to have a breakdown...
out.suffStat.stay_10_trials = intersect(out.suffStat.stay_trials,out.suffStat.mag10_trials);
out.suffStat.stay_25_trials = intersect(out.suffStat.stay_trials,out.suffStat.mag25_trials);
out.suffStat.stay_50_trials = intersect(out.suffStat.stay_trials,out.suffStat.mag50_trials);


%Stay prob example (number of win 10 rew mag trials which subj stayed / number of win 10 rew mag trials) 
out.suffStat.win_stay_10_prob = length(intersect(out.suffStat.win_10_trials,out.suffStat.stay_trials))./length(out.suffStat.win_10_trials);
out.suffStat.win_stay_25_prob = length(intersect(out.suffStat.win_25_trials,out.suffStat.stay_trials))./length(out.suffStat.win_25_trials);
out.suffStat.win_stay_50_prob = length(intersect(out.suffStat.win_50_trials,out.suffStat.stay_trials))./length(out.suffStat.win_50_trials);
out.suffStat.loss_stay_10_prob = length(intersect(out.suffStat.loss_10_trials,out.suffStat.stay_trials))./length(out.suffStat.loss_10_trials);
out.suffStat.loss_stay_25_prob = length(intersect(out.suffStat.loss_25_trials,out.suffStat.stay_trials))./length(out.suffStat.loss_25_trials);
out.suffStat.loss_stay_50_prob = length(intersect(out.suffStat.loss_50_trials,out.suffStat.stay_trials))./length(out.suffStat.loss_50_trials);



%Old - incorrect
% out.suffStat.stay_10_prob = sum(out.suffStat.stay_10)./length(out.suffStat.rew10_trials);
% out.suffStat.stay_25_prob = sum(out.suffStat.stay_25)./length(out.suffStat.rew25_trials);
% out.suffStat.stay_50_prob = sum(out.suffStat.stay_50)./length(out.suffStat.rew50_trials);
% out.suffStat.diff_10_50_prob = out.suffStat.stay_10_prob - out.suffStat.stay_50_prob;

if save_results
    file_name = sprintf('id_%d_bandit_vba_output_%d_rewVec',id,use_reward_vec);
    file_str = [file_path filesep file_name];
    save(file_str,'posterior', 'out', 'b')
end

function x=shiftMe(x)
%Shift the reg data by 1 to the left
    x = [x(:,2:end) zeros(size(x,1),1)];


function x = carryValueForward(x,y)
%Remove all NANs and push chosen index forward 

%Pesky indexing, if nan at 1 set it to [1 0 0]'
if sum(isnan(y(:,1)))>0
    x(:,1) = [1 0 0]';
    y(:,1) = [1 0 0]';
end

x(isnan(y)) = x(find(isnan(y))-size(y,1));
if numel(find(isnan(x)))>0
    x = carryValueForawrd(x,y);
end

function plot_subject_vs_contingency(b,out)
%remove nans from design file
design = bandit_fmri_load_design;
design.Arew(isnan(design.Arew))=[];
design.Brew(isnan(design.Brew))=[];
design.Crew(isnan(design.Crew))=[];

figure(7)
clf
subplot(3,1,1)
plot(smooth(double(b.chosen_stim==1)),'LineWidth',2);
hold on
plot(smooth(design.Arew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Arew vs A chosen');
subplot(3,1,2)
plot(smooth(double(b.chosen_stim==2)),'LineWidth',2);
hold on
plot(smooth(design.Brew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Brew vs B chosen');
subplot(3,1,3)
plot(smooth(double(b.chosen_stim==3)),'LineWidth',2);
hold on
plot(smooth(design.Crew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Crew vs C chosen');
hleg1 = legend('subj choice','reinforcement', 'predicted value');
set(hleg1,'Location','Best')

%% final sanity check: y vs. hidden states (values)

figure(8); clf; subplot(2,1,1); plot(out.y','LineWidth',4); axis([0 300 -.25 1.25]);
subplot(2,1,2); plot(out.suffStat.gx','LineWidth',4);