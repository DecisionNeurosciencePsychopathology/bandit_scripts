function [df1, df2]=create_trialwise_bandit_dataset(ids)
%This function is designed to export the bandit data contained in the .mat
%files to csv's for analysis in R

if nargin<1; ids=[]; end

%Load in ball
load('C:\kod\fMRI\subjects\bandit_data.mat')

%Load in healthy control list
%id_list=load('hc_list.mat')

%Load in the demographics 
load('C:\kod\Neuropsych_preproc\matlab\tmp\demogs_data.mat');
demog_data = data;
clear data


%Load in all subjs currently
if isempty(ids)
    id_list=load('all_subjs_ids_6_15_17.mat'); %Fallback
    
    %Grab fieldname of id struct
    foo=fieldnames(id_list);
    id_list = id_list.(foo{:});
    
else
    id_list=ids;
end


%If we want to add in the vba results from fixing the parameters
merge_fixed_dataset = 1;

%If we want to merge the parameter estimates for fitting the first 150
%trials
merge_first_150 = 1;

%Create data frames as tables to be saved as csvs
df1 = table();
df1_tmp = table();

df2 = table();
df2_tmp = table();

%Loop over subjects
for i = 1:length(ball.id)
    
    %For now only use the HC's
    if ismember(ball.id(i),id_list)
        %% %--------- DF1 choices and VBA output ---------------%
        %load in data
        load(['E:\data\bandit\bandit_behav_data\' num2str(ball.id(i)) '.mat'])
               
        %Set subject's data
        subj_data = b;        
        
        %Let the first column in the table be id
        df1_tmp.ID = repmat(ball.id(i),length(subj_data.sub_proc.stim_choice_numeric),1);
        
        %Set trial number
        df1_tmp.Trial = [1:length(subj_data.sub_proc.stim_choice_numeric)]';
        
        %Set multinomial choice
        df1_tmp.multinomial_choice = cellstr(subj_data.sub_proc.stim_choice);                
        
        %Split choices into binary choice vectors (A=1, B=2, C=3)
%         df1_tmp.A_Choice = subj_data.choice_numeric==1;
%         df1_tmp.B_Choice = subj_data.choice_numeric==2;
%         df1_tmp.C_Choice = subj_data.choice_numeric==3;
        df1_tmp.choice_numeric = subj_data.sub_proc.stim_choice_numeric;
        
        %If subj switched from last trial (Stay=1 Switched=0)
        %df1_tmp.switch_vector=create_switch_vector(subj_data);
        
        %Add if trial was rewarded or not
        df1_tmp.correct_incorrect = subj_data.stim_ACC;
        
        %Add amount for stake
        df1_tmp.stake = subj_data.stakeVec;
        
        %Grab the reward subject actually recieved
        df1_tmp.reward = subj_data.rewardVec;
        
        %Grab computer and mystery indices
        df1_tmp.comp_trials = subj_data.comp_index;
        df1_tmp.mystery_trials = subj_data.myst_index;
        
        %Load  in the output of vba -- be careful here when loading in the
        %correct vba output. -- yes this contains the 'b' struc however if
        %the correct filter was not applied you have to run ARC all over
        %again for the proper filter to reflect in these files...
        vba_file = glob(['vba_output/*' num2str(ball.id(i)) '*.mat']);
        load(vba_file{:})
        
        %Grab value data
        df1_tmp.value_chosen = out.suffStat.value_chosen';
        df1_tmp.value_max = out.suffStat.value'; %This is the max value of each hidden state per trial
        [~,max_idx]=max(out.suffStat.muX(1:3,:));
        df1_tmp.best_value_option = max_idx';
        
        %Entropy
        H = calc_entropy(out);
        df1_tmp.H = H';
        
        %If we want to add the fixed parameter data -- maybe think of
        %making this a funciton if you have to merge more than 2 vba output
        %datasets i.e. if tables can be dynamically named supply a cell
        %list of varable names with out and posterior, or rename them after
        if merge_fixed_dataset
            vba_file = glob(['vba_output/fixed_params/*' num2str(ball.id(i)) '*.mat']);
            load(vba_file{:})
            
            %Pull log model evidence
            out_vba_L_fixed_params = [out_vba_L_fixed_params; [ball.id(i) out.F]];
            
            df1_tmp.value_A_stim_fixed_params = posterior.muX(1,:)'; %value of A choice
            df1_tmp.value_B_stim_fixed_params = posterior.muX(2,:)'; %value of B choice
            df1_tmp.value_C_stim_fixed_params = posterior.muX(3,:)'; %value of C choice
            
            df1_tmp.value_chosen_fixed_params = out.suffStat.value_chosen';
            df1_tmp.value_max_fixed_params = out.suffStat.value'; %This is the max value of each hidden state per trial
            [~,max_idx]=max(out.suffStat.muX(1:3,:));
            df1_tmp.best_value_option_fixed_params = max_idx';
            
            %Entropy
            H_fixed_params = calc_entropy(out);
            df1_tmp.H_fixed_params = H_fixed_params';
        end
        
        if merge_first_150
            vba_file = glob(['vba_output/first_150/*' num2str(ball.id(i)) '*.mat']);
            load(vba_file{:})
            
            %Grab value chosen
            df1_tmp.value_chosen_first_150 = [out.suffStat.value_chosen'; nan(150, 1)];
            
            df1_tmp.value_A_stim_first_150 = [posterior.muX(1,:)'; nan(150, 1)]; %value of A choice
            df1_tmp.value_B_stim_first_150 = [posterior.muX(2,:)'; nan(150, 1)]; %value of B choice
            df1_tmp.value_C_stim_first_150 = [posterior.muX(3,:)'; nan(150, 1)]; %value of C choice
        end
        
        
        %Update dataframe
        df1 = [df1; df1_tmp];
        
        
        %% %--------- DF2 switches, errors, and demographics -------------%
        
        %Setup id
        df2_tmp.ID = ball.id(i);
        
        %Grab switch errors
        df2_tmp.spont_switch_err = sum(subj_data.sub_proc.errors.spont_switch_err);
        df2_tmp.prob_switch_err = sum(subj_data.sub_proc.errors.prob_switch_err);
        df2_tmp.erratic_spont = sum(subj_data.sub_proc.errors.erratic_spont);
        df2_tmp.explore_switch = sum(subj_data.sub_proc.errors.explore_switch);
        df2_tmp.error_NOS = sum(subj_data.sub_proc.errors.error_NOS);
        
        %Grab the demographics and measures
        demog_idx = demog_data.ID==ball.id(i);
        df2_tmp.group1245 = demog_data.GROUP1245(demog_idx);
        df2_tmp.group12467 = demog_data.GROUP12467(demog_idx);
        df2_tmp.sex = demog_data.GENDERTEXT(demog_idx);
        df2_tmp.age = demog_data.LEARNAGE(demog_idx);
        df2_tmp.race = demog_data.RACETEXT(demog_idx);
        df2_tmp.education = demog_data.EDUCATION(demog_idx);
        df2_tmp.marital_status = demog_data.MARITALTEXT(demog_idx);
        df2_tmp.max_lethality = demog_data.MAXLETHALITY(demog_idx);
        
        %Update dataframe
        df2 = [df2; df2_tmp];
   
    end
    
end

%Fill any missing subjects from what Josh sent with nans
assesments=readtable('C:\kod\fMRI\from_josh\bandit_10-26-17.xlsx');
df2=join(df2,assesments,'Keys','ID');

%% Parameters - load in the parameter matrix i.e. output from
%compile_vba_parameters and join to df2
load('C:\kod\fMRI\vba\fMRI_param_estimates.mat')
df2=join(df2,fMRI_parameter_estimates,'Keys','ID');

%Transform them -- this could be a funciton as well
df2.alpha_win_transformed = 1./(1+exp(-df2.alpha_win)); % learning rate is bounded between 0 and 1.
df2.alpha_loss_transformed = 1./(1+exp(-df2.alpha_loss)); % learning rate is bounded between 0 and 1.
df2.decay_transformed = 1./(1+exp(-df2.decay)); % decay is bounded between 0 and 1.
df2.beta_transformed = exp(df2.beta);

%Put in the fixed params?
if merge_fixed_dataset
    load('C:\kod\fMRI\vba\fMRI_median_values.mat')
    df2.alpha_win_median = repmat(fMRI_median_values(1),height(df2),1);
    df2.alpha_loss_median = repmat(fMRI_median_values(2),height(df2),1);
    df2.decay_median = repmat(fMRI_median_values(3),height(df2),1);
    df2.beta_median = repmat(fMRI_median_values(4),height(df2),1);
    
    df2.alpha_win_median_transformed = 1./(1+exp(-df2.alpha_win_median)); % learning rate is bounded between 0 and 1.
    df2.alpha_loss_median_transformed = 1./(1+exp(-df2.alpha_loss_median)); % learning rate is bounded between 0 and 1.
    df2.decay_median_transformed = 1./(1+exp(-df2.decay_median)); % decay is bounded between 0 and 1.
    df2.beta_median_transformed = exp(df2.beta_median);
end

%Put in first 150 parameter estimates -- need to clean this up
if merge_first_150
    load('C:\kod\fMRI\vba\fMRI_first_150_param_estimates.mat')
    df2=join(df2,first_150_fMRI_param_table,'Keys','ID');
    
    %Transform them -- this could be a funciton as well
    df2.alpha_win_first_150_transformed = 1./(1+exp(-df2.alpha_win_first_150)); % learning rate is bounded between 0 and 1.
    df2.alpha_loss_first_150_transformed = 1./(1+exp(-df2.alpha_loss_first_150)); % learning rate is bounded between 0 and 1.
    df2.decay_first_150_transformed = 1./(1+exp(-df2.decay_first_150)); % decay is bounded between 0 and 1.
    df2.beta_first_150_transformed = exp(df2.beta_first_150);
end

%% Write the tables to csvs

%Write the tables to csvs
if exist('R','dir')
    mkdir('R')
end
writetable(df1,'R/bandit_df1.csv')
writetable(df2,'R/bandit_df2.csv')

%--------------------------------------------------------------------------
function compare_choice=create_switch_vector(data)
%Create a vector of stay switch choices where missed responses are Nans

%Create the t+1 vector to compare subj choices to
choice_tplus1 = [nan; data.choice_numeric(1:end-1)];
compare_choice = choice_tplus1==data.choice_numeric;

%Take care of missed responses
missed_responses=find(data.choice_numeric==0);

%If they missed the first response remove it from the list as it would be
%Nan anyway
if ismember(1,missed_responses)
    missed_responses(missed_responses==1)=[];
end

mr_val=data.choice_numeric(missed_responses-1)==data.choice_numeric(missed_responses+1);
compare_choice(missed_responses+1)=mr_val;

%Convert to double since logicals can't be nans
compare_choice = double(compare_choice);
compare_choice(missed_responses)=nan;

%First element is always nan
compare_choice(1) = nan;

