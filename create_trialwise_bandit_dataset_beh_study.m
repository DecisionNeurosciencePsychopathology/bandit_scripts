function [df1, df2]=create_trialwise_bandit_dataset_beh_study(ids)
%This function is designed to export the bandit data contained in the .mat
%files to csv's for analysis in R

% Alex's quick tweaks before the conference -- need to edit to incorporate all data

if nargin<1; ids=[]; end

%Load in ball
% load('C:\kod\fMRI\subjects\bandit_data.mat')

%Load in healthy control list
%id_list=load('hc_list.mat')

%Load in the demographics 
% load('C:\kod\Neuropsych_preproc\matlab\tmp\demogs_data.mat');
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

%Create data frames as tables to be saved as csvs
df1 = table();
df1_tmp = table();

df2 = table();
df2_tmp = table();
id_list= ball.id;

%Loop over subjects
for i = 1:length(ball.id)
    
    %For now only use the HC's
    if ismember(ball.id(i),id_list)
        %% %--------- DF1 choices and VBA output ---------------%
        %load in data
        load(['/Volumes/bek/behav_data_backup/NP_preproc_backup/Neuropsych_preproc/matlab/analysis/bandit/data/' num2str(ball.id(i)) '.mat'])
               
        %Set subject's data
        subj_data = b;        
        
        %Let the first column in the table be id
        df1_tmp.ID = repmat(ball.id(i),length(subj_data.stim_choice_numeric),1);
        
        %Set trial number
        df1_tmp.Trial = [1:length(subj_data.stim_choice_numeric)]';
        
        %Set multinomial choice
        df1_tmp.multinomial_choice = cellstr(subj_data.stim_choice);                
        
        %Split choices into binary choice vectors (A=1, B=2, C=3)
%         df1_tmp.A_Choice = subj_data.choice_numeric==1;
%         df1_tmp.B_Choice = subj_data.choice_numeric==2;
%         df1_tmp.C_Choice = subj_data.choice_numeric==3;
        df1_tmp.choice_numeric = subj_data.stim_choice_numeric;
        
        %If subj switched from last trial (Stay=1 Switched=0)
        %df1_tmp.switch_vector=create_switch_vector(subj_data);
        
        %Add if trial was rewarded or not
        df1_tmp.correct_incorrect = subj_data.showstim_ACC;

        %Load  in the output of vba -- be careful here when loading in the
        %correct vba output. -- yes this contains the 'b' struc however if
        %the correct filter was not applied you have to run ARC all over
        %again for the proper filter to reflect in these files...

        %Update dataframe
        df1 = [df1; df1_tmp];
        
        
%         %% %--------- DF2 switches, errors, and demographics -------------%
%         
%         %Setup id
%         df2_tmp.ID = ball.id(i);
%         
%         %Grab switch errors
%         df2_tmp.spont_switch_err = sum(subj_data.sub_proc.errors.spont_switch_err);
%         df2_tmp.prob_switch_err = sum(subj_data.sub_proc.errors.prob_switch_err);
%         df2_tmp.erratic_spont = sum(subj_data.sub_proc.errors.erratic_spont);
%         df2_tmp.explore_switch = sum(subj_data.sub_proc.errors.explore_switch);
%         df2_tmp.error_NOS = sum(subj_data.sub_proc.errors.error_NOS);
%         
%         %Grab the demographics and measures
%         demog_idx = demog_data.ID==ball.id(i);
%         df2_tmp.group1245 = demog_data.GROUP1245(demog_idx);
%         df2_tmp.group12467 = demog_data.GROUP12467(demog_idx);
%         df2_tmp.sex = demog_data.GENDERTEXT(demog_idx);
%         df2_tmp.age = demog_data.LEARNAGE(demog_idx);
%         df2_tmp.race = demog_data.RACETEXT(demog_idx);
%         df2_tmp.education = demog_data.EDUCATION(demog_idx);
%         df2_tmp.marital_status = demog_data.MARITALTEXT(demog_idx);
%         df2_tmp.max_lethality = demog_data.MAXLETHALITY(demog_idx);
%         
%         %Update dataframe
%         df2 = [df2; df2_tmp];
   
    end
    
end

%Fill any missing subjects from what Josh sent with nans
assesments=readtable('C:\kod\fMRI\from_josh\bandit_10-26-17.xlsx');
df2=join(df2,assesments,'Keys','ID');

%Write the tables to csvs
if exist('R','dir')
    mkdir('R')
end
writetable(df1,'~/code/bandit_scripts/R/bandit_beh_df1.csv')
writetable(id_list,'~/code/bandit_scripts/R/ids.csv')
csvwrite('~/code/bandit_scripts/R/ids.csv',id_list)
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

