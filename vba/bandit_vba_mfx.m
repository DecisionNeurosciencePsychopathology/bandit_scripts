function [posterior_sub,out_sub,posterior_group,out_group,b] = bandit_vba_mfx(rootdir,dirs,g_graphics,s_graphics,plot_subject,save_results,parameterization)

% fits BANDIT rl model to 3 armed bandit subject data using VBA toolbox
% example call: %%%NEED TO FIX THIS
% [posterior_sub,out_sub,posterior_group,out_group,b] = bandit_vba_mfx(dirs,graphics,plot_subject,save_results,parameterization);
% dirs:          struct of dirs with field for 6-digit subject id from subject database
% %to do: add additional calls if needed
%
close all

valence = parameterization.valence;
fix_decay = parameterization.fix_decay; %The logic is fixed
utility = parameterization.utility;
disappointment = parameterization.disappointment;
regret = parameterization.regret;
fix_all_params = parameterization.fix_all_params ;
use_reward_vec = parameterization.use_reward_vec;
wsls = parameterization.wsls;
wsls_soft = parameterization.wsls_soft;
null = parameterization.null;
sticky = parameterization.sticky;
model = parameterization.model;

%If we only want to use the first 150 trials
use_first_150 = 0;


% Where to look for data
%Quick username check, and path setting
if save_results==1
    file_path = 'vba_output';
else
    fprintf('You are not saving the data!\n\n')
end

%Evolution options- set as 0 and then overwrite below if indicated 
options.inF.utility = 0;
options.inF.valence = 0;
options.inF.decay = 0;

%Turn graphics on or off
if ~g_graphics
    options.DisplayWin = 0;
    options.GnFigs = 0;
end
% set up dim defaults
if valence && ~disappointment
    n_theta = 3; %Number of evolution params (AlphaWin AlphaLoss Beta)
    options.inF.valence = 1;
    options.inF.disappointment= 0;

elseif valence && disappointment
    n_theta = 4; %Number of evolution params (AlphaWin AlphaLoss Beta Omega)
    options.inF.valence = 1;
    options.inF.disappointment= 1;

else
    n_theta = 2;
    options.inF.valence = 0;
    options.inF.disappointment= 0;
end

if utility
    n_theta = n_theta +1; %Add in steepness parameter
    options.inF.utility = 1;
end



if fix_decay
    n_theta = n_theta-1;
    options.inF.fix_decay = 1;
else
    options.inF.fix_decay = 0;
end

n_phi = 1; %Number of observation params (Beta)
f_name = @f_bandit_Qlearn; %Evolution function
g_name = @g_bandit_softmax; %Observation function
n_t = 300; %Total number of trials
% n_runs = 3; %3 blocks total
n_hidden_states = 4; %Track value for each arm of the bandit + PE

if sticky
    n_phi = n_phi + 1;
    options.inG.autocorrelation = 1;
end

if wsls
    n_phi = 1;
    n_theta = 0;
    g_name = @g_bandit_wsls;
    n_hidden_states = 0;
    f_name = '';
end

if wsls_soft
    n_phi = 1;
    n_theta = 0;
    g_name = @g_bandit_wsls_soft;
    n_hidden_states = 0;
    f_name = '';
end

if null
n_phi = 1;
    n_theta = 0;
    g_name = @g_bandit_null;
    n_hidden_states = 0;
end




% Fixed parameters		
 if fix_all_params				
     n_theta=0;		
     n_phi=0;		
     options.inF.fixed_params=1;		
     options.inG.fixed_params=1;		
 else		
     options.inF.fixed_params=0;		
     options.inG.fixed_params=0;		
 end
 
 % defined number of hidden states and parameters
 dim = struct('n',n_hidden_states,'n_theta',n_theta,'n_phi',n_phi, 'n_t', n_t);
 
 % priors- set these for the group, do not need subject-level priors since
 % determined by group distributions in MFX
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
 priors.a_sigma = 1;     % Jeffreys prior
 priors.b_sigma = 1;     % Jeffreys prior
             
 
 %preallocate cell arrays for all subjects
 all_u=cell(size(dirs,1),1);
 all_y=cell(size(dirs,1),1);
 all_options=cell(size(dirs,1),1);
 b=cell(size(dirs,1),1);
 
 % Loop through subjects
 for i = 1:length(dirs)
     try
         id=str2double(dirs(i).name);
         
         % Load in the subject's data
         %u is 2 x ntrials where first row is actions and second row is reward
         b{i} = bandit_vba_read_in_data( 'id',id,'data_dir',rootdir); %REPLACE subjects with local dir
         b{i}.id = id;
         censor = b{i}.chosen_stim==999; %Censor some trials first
         subjects_actions = b{i}.chosen_stim;
         subjects_actions(censor)=nan;
         all_u{i}(1,:) = subjects_actions; %Chosen action [1 2 3]
         if use_reward_vec
             all_u{i}(2,:) = b{i}.rewardVec; %Reward has actual value [10 25 50]
             all_u{i}(3,:) = b{i}.stakeVec; %Stake
         else
             all_u{i}(2,:) = b{i}.stim_ACC; %Reward or not [1 0]
             all_u{i}(3,:) = NaN;
         end
         all_u{i} = [zeros(size(all_u{i},1),1) all_u{i}(:,1:end-1)]; %Shift the u!
         
         %Only use the first 150 trials
         if use_first_150==1
             n_t = n_t/2; %Should take care of the y
             all_u{i} = all_u{i}(:,1:n_t);
             censor = censor(1:n_t);
         end
         
         all_y{i} = zeros(3, n_t);
         for ii = 1:n_t
             try
                 all_y{i}(subjects_actions(ii), ii) = 1;
             catch
                 all_y{i}(:,ii) = nan;
             end
         end
         
         % set up models within evolution/observation Fx
         all_options{i}.inF.b = b{i};
         all_options{i}.inG.b = b{i};
         
         % skip first trial
         all_options{i}.skipf = zeros(1,n_t);
         all_options{i}.skipf(1) = 1;
         
         all_options{i}.binomial = 1;
         
         %Do Priors too.
         all_options{i}.priors=priors;
         % split into conditions/runs
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
         all_options{i}.id = id;
         % Last bit of option declarations
         all_options{i}.TolFun = 1e-6;
         all_options{i}.GnTolFun = 1e-6;
         all_options{i}.verbose=1;
         
         %Censor any bad trials
         all_options{i}.isYout = repmat(censor,1,3)';
         all_options{i}.inF.Yout = all_options{i}.isYout;
         
         if fix_all_params
             all_options{i}.inF.fixed_params=1;
             all_options{i}.inG.fixed_params=1;
         else
             all_options{i}.inF.fixed_params=0;
             all_options{i}.inG.fixed_params=0;
         end
         
         %Evolution options- set as 0 and then overwrite below if indicated
         all_options{i}.inF.utility = 0;
         all_options{i}.inF.valence = 0;
         all_options{i}.inF.decay = 0;
         
         
         %Turn graphics on or off
         if ~s_graphics
             all_options{i}.DisplayWin = 0;
             all_options{i}.GnFigs = 0;
         end
         
         
         if valence && ~disappointment
             n_theta = 3; %Number of evolution params (AlphaWin AlphaLoss Beta)
             all_options{i}.inF.valence = 1;
             all_options{i}.inF.disappointment= 0;
             
         elseif valence && disappointment
             n_theta = 4; %Number of evolution params (AlphaWin AlphaLoss Beta Omega)
             all_options{i}.inF.valence = 1;
             all_options{i}.inF.disappointment= 0;
             
         else
             n_theta = 2;
             all_options{i}.inF.valence = 0;
             all_options{i}.inF.disappointment= 0;
         end
         
         
         % set up dim defaults- may not need to do this again
         all_options{i}.inG.autocorrelation = 0;
         
         if utility
             n_theta = n_theta +1; %Add in steepness parameter
             all_options{i}.inF.utility = 1;
         end
         
         if fix_decay
             n_theta = n_theta-1;
             all_options{i}.inF.fix_decay = 1;
         else
             all_options{i}.inF.fix_decay = 0;
         end
         if sticky
             n_phi = n_phi + 1;
             all_options{i}.inG.autocorrelation = 1;            
         end
         
     catch
     end
 end
 all_y_use=all_y(~cellfun('isempty',all_y));
 all_u_use=all_u(~cellfun('isempty',all_u));
 all_options_use=all_options(~cellfun('isempty',all_options));
 %save as temp file so during test we dont have to re-do the processing all
 %the time 
 save('temp')
 
 % Run the vba model
 [posterior_sub,out_sub,posterior_group,out_group] = VBA_MFX_PAR(all_y_use,all_u_use,f_name,g_name,dim,all_options_use,priors,options); %VBA_MFX(y,u,f_fname,g_fname,dim,options,priors_group, options_group)
 

 for i = 1:length(out_sub)
     try
         if ~isempty(out_sub{i}.suffStat.muX)
             %add ID #s to subject structures
             out_sub{i}.id=all_options_use{i}.inF.b.id;
             posterior_sub{i}.id=all_options_use{i}.inF.b.id;
             
             bad_trials = find(isnan(all_u_use{i}(1,:)));
             winning_trials = find((b{i}.stim_ACC==1)' & ~ismember(1:n_t,bad_trials-1));
             losing_trials = find((b{i}.stim_ACC==0)' & ~ismember(1:n_t,bad_trials-1));
             
             %Separate the hidden states
             choices = out_sub{i}.suffStat.muX(1:3,:);
             delta = out_sub{i}.suffStat.muX(4,:);
             
             %Shift PE Regressor if needed...
             out_sub{i}.suffStat.delta = shiftMe(delta);
             
             %SHOULD THESE CHANGE?
             %Values are shifted to the left add on as many zeros as needed to regain
             %proper length
             muX_diff = [diff(choices,1,2) zeros(size(choices,1),1)];
             PEunsigned = max(abs(muX_diff));
             %PEunsigned = [PEunsigned 0]; %Tack on zero to the end?
             PEplus = zeros(1,length(b{i}.stim_ACC));
             PEminus = zeros(1,length(b{i}.stim_ACC));
             out_sub{i}.suffStat.PEchosen_pos = zeros(1,length(b{i}.stim_ACC));
             out_sub{i}.suffStat.PEchosen_neg = zeros(1,length(b{i}.stim_ACC));
             PEplus(winning_trials) = PEunsigned(winning_trials); %Good!
             PEminus(losing_trials) = PEunsigned(losing_trials); %Out of bounds because losing trials contains index 300
             out_sub{i}.suffStat.PEunsigned=PEunsigned;
             out_sub{i}.suffStat.PEplus=PEplus;
             out_sub{i}.suffStat.PEminus=PEminus;
             out_sub{i}.suffStat.PEsigned=PEplus-PEminus;
             %Grab the option they chose and remove any error codes
             chosen_index = all_y_use{i};
             chosen_index = carryValueForward(chosen_index,all_y_use{i}); %If there are any Nan's replace them with the most recent decision made
             
             %PEchosen is now the pe hidden state
             %out_sub{i}.suffStat.PEchosen = muX_diff(logical(chosen_index))'; %PE of chosen choice
             out_sub{i}.suffStat.PEchosen = out_sub{i}.suffStat.delta; %PE of chosen choice
             out_sub{i}.suffStat.PEchosen_pos(out_sub{i}.suffStat.PEchosen>0) = out_sub{i}.suffStat.PEchosen(out_sub{i}.suffStat.PEchosen>0);
             out_sub{i}.suffStat.PEchosen_neg(out_sub{i}.suffStat.PEchosen<0) = out_sub{i}.suffStat.PEchosen(out_sub{i}.suffStat.PEchosen<0);
             
             %Create Value regressors
             out_sub{i}.suffStat.value = max(choices); %Max value of each hidden state per trial
             out_sub{i}.suffStat.value_diff = out_sub{i}.suffStat.value - mean(choices);
             out_sub{i}.suffStat.value_chosen = choices(logical(chosen_index))';
             out_sub{i}.suffStat.value_not_chosen=choices(~logical(chosen_index))';
             out_sub{i}.suffStat.value_not_chosen = reshape(out_sub{i}.suffStat.value_not_chosen,2,300);
             not_chosen_sum=sum(out_sub{i}.suffStat.value_not_chosen); %Keep this var on ice for now
             out_sub{i}.suffStat.value_chosen_diff = out_sub{i}.suffStat.value_chosen - mean(out_sub{i}.suffStat.value_not_chosen); %Do not shift this one!
             
             %Create different flavor of value difference regressor in which v(t+1) is
             %indexed by chosen_index, (i.e. Vtplus1(chosen_index)) and subtracted by
             %the mean(Vtplus1(~chosen_index)))
             out_sub{i}.suffStat.vtplus1 = choices(:,2:end);
             out_sub{i}.suffStat.vtplus1_not_chosen=out_sub{i}.suffStat.vtplus1(~logical(chosen_index(:,1:end-1)))';
             out_sub{i}.suffStat.vtplus1_not_chosen = reshape(out_sub{i}.suffStat.vtplus1_not_chosen,2,length(out_sub{i}.suffStat.vtplus1));
             out_sub{i}.suffStat.vtplus1_chosen_diff=[(out_sub{i}.suffStat.vtplus1(logical(chosen_index(:,1:end-1)))' - mean(out_sub{i}.suffStat.vtplus1_not_chosen)) 0];
             
             %Create a regrssor like vtplus1 but using the max subtracted by the median - V.B.
             out_sub{i}.suffStat.vtplus1_max=[(max(out_sub{i}.suffStat.vtplus1) - median(out_sub{i}.suffStat.vtplus1)) 0];
             
             %Value chosen was normalized at one point?
             %out_sub{i}.suffStat.value_chosen=out_sub{i}.suffStat.value_chosen./sum(choices);
             
             %Shift all the value regressors by 1
             out_sub{i}.suffStat.value=shiftMe(out_sub{i}.suffStat.value);
             out_sub{i}.suffStat.value_diff=shiftMe(out_sub{i}.suffStat.value_diff);
             out_sub{i}.suffStat.value_chosen=shiftMe(out_sub{i}.suffStat.value_chosen);
             out_sub{i}.suffStat.value_not_chosen=shiftMe(out_sub{i}.suffStat.value_not_chosen);
             %out_sub{i}.suffStat.value_chosen=shiftMe(out_sub{i}.suffStat.value_chosen);
             %out_sub{i}.suffStat.value_chosen(1) = 0; %This is a NAN since 0/0
             
             %Standardize the PEChosen and ValueChosenDiff regs
             
             %zscore is only in stat toolbox?
             try
                 out_sub{i}.suffStat.value_chosen_diff_standardized = ...
                     zscore(out_sub{i}.suffStat.value_chosen_diff);
                 out_sub{i}.suffStat.PEchosen_standardized = ...
                     zscore(out_sub{i}.suffStat.PEchosen);
             catch
                 fprintf('Stat toolbox not found!\n\n')
             end
             
             %Create reward stake and just stake align it with trialonset
             out_sub{i}.suffStat.reward_stake = b{i}.rewardVec';
             out_sub{i}.suffStat.reward_stake(b{i}.rewardVec==10)=1;
             out_sub{i}.suffStat.reward_stake(b{i}.rewardVec==25)=2;
             out_sub{i}.suffStat.reward_stake(b{i}.rewardVec==50)=3;
             
             %mean corrected rew mag
             out_sub{i}.suffStat.reward_stake_mc = out_sub{i}.suffStat.reward_stake - mean(out_sub{i}.suffStat.reward_stake);
             
             %This is what they could have won per trial <- what we want for createing the probabilities
             out_sub{i}.suffStat.stake = b{i}.stakeVec';
             out_sub{i}.suffStat.stake(b{i}.stakeVec==10)=1;
             out_sub{i}.suffStat.stake(b{i}.stakeVec==25)=2;
             out_sub{i}.suffStat.stake(b{i}.stakeVec==50)=3;
             
             %Mean corrected stake regressor
             out_sub{i}.suffStat.stake_mc = out_sub{i}.suffStat.stake - mean(out_sub{i}.suffStat.stake);
             
             
             %Percentages of reward magnitude and staying
             out_sub{i}.suffStat.mag10_trials = find(b{i}.stakeVec==10);
             out_sub{i}.suffStat.mag25_trials = find(b{i}.stakeVec==25);
             out_sub{i}.suffStat.mag50_trials = find(b{i}.stakeVec==50);
             
             %Determine number of trials with specific magnitude that were win/loss
             out_sub{i}.suffStat.win_10_trials = intersect(winning_trials,out_sub{i}.suffStat.mag10_trials);
             out_sub{i}.suffStat.win_25_trials = intersect(winning_trials,out_sub{i}.suffStat.mag25_trials);
             out_sub{i}.suffStat.win_50_trials = intersect(winning_trials,out_sub{i}.suffStat.mag50_trials);
             out_sub{i}.suffStat.loss_10_trials = intersect(losing_trials,out_sub{i}.suffStat.mag10_trials);
             out_sub{i}.suffStat.loss_25_trials = intersect(losing_trials,out_sub{i}.suffStat.mag25_trials);
             out_sub{i}.suffStat.loss_50_trials = intersect(losing_trials,out_sub{i}.suffStat.mag50_trials);
             
             %Find stay trials
             error_code = 999; %If they missed a trial, ie don't want two zeros in a row to i...
             out_sub{i}.suffStat.stay_trials  = find(logical([0; b{i}.chosen_stim(2:end)==b{i}.chosen_stim(1:end-1)]) & b{i}.chosen_stim~=error_code);
             
             %These aren;t really nedded but its good to have a breakdown...
             out_sub{i}.suffStat.stay_10_trials = intersect(out_sub{i}.suffStat.stay_trials,out_sub{i}.suffStat.mag10_trials);
             out_sub{i}.suffStat.stay_25_trials = intersect(out_sub{i}.suffStat.stay_trials,out_sub{i}.suffStat.mag25_trials);
             out_sub{i}.suffStat.stay_50_trials = intersect(out_sub{i}.suffStat.stay_trials,out_sub{i}.suffStat.mag50_trials);
             
             %Stay prob example (number of win 10 rew mag trials which subj stayed / number of win 10 rew mag trials)
             out_sub{i}.suffStat.win_stay_10_prob = length(intersect(out_sub{i}.suffStat.win_10_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.win_10_trials);
             out_sub{i}.suffStat.win_stay_25_prob = length(intersect(out_sub{i}.suffStat.win_25_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.win_25_trials);
             out_sub{i}.suffStat.win_stay_50_prob = length(intersect(out_sub{i}.suffStat.win_50_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.win_50_trials);
             out_sub{i}.suffStat.loss_stay_10_prob = length(intersect(out_sub{i}.suffStat.loss_10_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.loss_10_trials);
             out_sub{i}.suffStat.loss_stay_25_prob = length(intersect(out_sub{i}.suffStat.loss_25_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.loss_25_trials);
             out_sub{i}.suffStat.loss_stay_50_prob = length(intersect(out_sub{i}.suffStat.loss_50_trials,out_sub{i}.suffStat.stay_trials))./length(out_sub{i}.suffStat.loss_50_trials);
         end
     catch
     end
 end

%Plot the subjects choices if needed %NEEDS FIXING
if plot_subject==1
    for i = 1:length(dirs)
        plot_subject_vs_contingency(b{i},out_sub{i})
    end
end

% %By pass saving here since we are only interested in the params right now.
% if save_results
%     if use_first_150
%         %Save value chosen as well
%         chosen_index = y;
%         chosen_index = carryValueForward(chosen_index,y); %If there are any Nan's replace them with the most recent decision made
%         choices = out.suffStat.muX(1:3,:);
%         out.suffStat.value_chosen = choices(logical(chosen_index))';
%         file_name = sprintf('id_%d_bandit_vba_output_%d_rewVec_first_150_only',id,use_reward_vec);
%         file_path = 'vba_output/first_150';
%         file_str = [file_path filesep file_name];
%         save(file_str,'posterior', 'out', 'b')
%         return
%     end
% end


% get prediction errors
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

if save_results
    %create name for saving output based on model inputs and date
    model_string='';
    if valence; model_string=strcat(model_string,'valence_'); end
    if utility; model_string=strcat(model_string,'utility_'); end
    if disappointment; model_string=strcat(model_string,'disappointment_'); end
    if regret; model_string=strcat(model_string,'regret_'); end
    if fix_decay; model_string=strcat(model_string,'fixedDecay_'); end
    if use_reward_vec; model_string=strcat(model_string,'rewardVec_'); end
    model_string=strcat(model_string,date,'_');
    if fix_all_params
        file_name = 'all_bandit_vba_output_fixed_params_';
        file_path = 'vba_output/fixed_params';
        file_str = [file_path filesep file_name model_string];
    else
        file_name = 'all_bandit_vba_output_';
        file_str = [file_path filesep file_name model_string];
    end
    save(file_str,'posterior_group','posterior_sub', 'out_group','out_sub', 'b')
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
    x = carryValueForward(x,y);
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

% final sanity check: y vs. hidden states (values)

figure(8); clf; subplot(2,1,1); plot(out.y','LineWidth',4); axis([0 300 -.25 1.25]);
subplot(2,1,2); plot(out.suffStat.gx','LineWidth',4);
