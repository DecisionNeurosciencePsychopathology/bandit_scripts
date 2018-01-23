function [df1, df2]=create_trialwise_bandit_dataset(ids, dataset_to_compile)
%This function is designed to export the bandit data contained in the .mat
%files to csv's for analysis in R
%EX: [df1, df2]=create_trialwise_bandit_dataset(ids(:,1));

if nargin<1; error('Must supply id list'); end
if nargin<2; dataset_to_compile='fMRI'; end

if strcmpi(dataset_to_compile,'fMRI')
    %Let usr know which dataset you are compiling fyi
    fprintf('Compiling fMRI bandit datasets now...\n\n')
    
    %Load in ball
    load('C:\kod\fMRI\subjects\bandit_data.mat')
    
    %Set up file paths
    file_strings.subj_behavior = 'E:/data/bandit/bandit_scan_data/';
    file_strings.vba_output = 'C:/kod/fMRI/vba_output/*';
    file_strings.vba_output_fixed_params = 'C:/kod/fMRI/vba_output/fixed_params/*';
    file_strings.vba_output_first_150 = 'C:/kod/fMRI/vba_output/first_150/*';
    file_strings.vba_output_mfx = 'E:/data/bandit/vba_mfx/';
    file_strings.save_path = 'C:/kod/fMRI/R/';
    
elseif strcmpi(dataset_to_compile,'behav')
    %Let usr know which dataset you are compiling fyi
    fprintf('Compiling behavioral bandit datasets now...\n\n')
    
    %Load in ball
    load('C:\kod\Neuropsych_preproc\matlab\analysis\bandit\data\bandit_data.mat')
    
    %Set up file paths
    file_strings.subj_behavior = 'C:/kod/Neuropsych_preproc/matlab/analysis/bandit/data/';
    file_strings.vba_output = 'E:/data/bandit/bandit_behav_vba_output/*';
    file_strings.vba_output_fixed_params = 'E:/data/bandit/bandit_behav_vba_output/fixed_params/*';
    file_strings.vba_output_first_150 = 'E:/data/bandit/bandit_behav_vba_output/first_150/*';
    file_strings.vba_output_mfx = 'E:/data/bandit/vba_mfx/';
    file_strings.save_path = 'C:/kod/Neuropsych_preproc/matlab/analysis/bandit/R/';
end

%Load in the demographics
load('C:\kod\Neuropsych_preproc\matlab\tmp\demogs_data.mat');
demog_data = data;
clear data

%If we want to add in the vba results from fixing the parameters
merge_fixed_dataset = 1;

%If we want to merge the parameter estimates for fitting the first 150
%trials
merge_first_150 = 1;

%If you want to use the vba_mfx_results
merge_vba_mfx = 1;
if merge_vba_mfx
    if strcmpi(dataset_to_compile,'fMRI')
        mfx_data = load([file_strings.vba_output_mfx 'fmri_vba_mfx_vars_n_128.mat'],'vba_mfx_df'); %vba_mfx_df
        mfx_data = mfx_data.('vba_mfx_df');
    elseif strcmpi(dataset_to_compile,'behav')
        mfx_data = load('E:\data\bandit\vba_mfx\behav_vba_mfx_vars_n286.mat','vba_mfx_df');
        mfx_data = mfx_data.('vba_mfx_df');
    end
end

%Create data frames as tables to be saved as csvs
df1 = table();
df1_tmp = table();

df2 = table();
df2_tmp = table();

%Save log model evidence
out_vba_L = [];
out_vba_L_fixed_params = [];
out_vba_L_first_150 = [];
out_vba_L_mfx = [];

%Loop over subjects
for i = 1:length(ball.id)
    
    %For now only use the HC's
    if ismember(ball.id(i),ids)
        %% %--------- DF1 choices and VBA output ---------------%
        %TODO just use the 'ball' variable instead of loading individual
        %files
        %load in data
        load([file_strings.subj_behavior num2str(ball.id(i)) '.mat']) %Path need to be a variable
        
        %Set subject's data
        subj_data = b;
        
        %To try and preserve the order of the data frames, and to move sub
        %struct up a level -- this is bad you need to jsut have eveything
        %in here be in the ball structure for both scanning and behavioral
        try
            for fn = fieldnames(subj_data.sub_proc)'
                subj_data.(fn{1}) = subj_data.sub_proc.(fn{1});
            end
        catch
            %PASS
        end
        
        
        %Let the first column in the table be id
        df1_tmp.ID = repmat(ball.id(i),length(subj_data.stim_choice_numeric),1);
        
        %Set trial number
        %df1_tmp.Trial = [1:length(subj_data.sub_proc.stim_choice_numeric)]';
        df1_tmp.Trial = [1:length(subj_data.stim_choice_numeric)]';
        
        %Set multinomial choice
        %df1_tmp.multinomial_choice = cellstr(subj_data.sub_proc.stim_choice);
        df1_tmp.multinomial_choice = cellstr(subj_data.stim_choice);
        
        %Split choices into binary choice vectors (A=1, B=2, C=3)
        %df1_tmp.choice_numeric = subj_data.sub_proc.stim_choice_numeric;
        df1_tmp.choice_numeric = subj_data.stim_choice_numeric;
        
        
        %If we are using the fMRI datast
        if strcmpi(dataset_to_compile,'fMRI')
            %Add Rts
            df1_tmp.RT = subj_data.stim_RT;
            
            %Add if trial was rewarded or not
            df1_tmp.correct_incorrect = subj_data.stim_ACC;
            
            %Add amount for stake
            df1_tmp.stake = subj_data.stakeVec;
            
            %Grab the reward subject actually recieved
            df1_tmp.reward = subj_data.rewardVec;
            
            %Grab computer and mystery indices
            df1_tmp.comp_trials = subj_data.comp_index;
            df1_tmp.mystery_trials = subj_data.myst_index;
        elseif strcmpi(dataset_to_compile,'behav') %behavioral dataset
            
            %Add Rts
            df1_tmp.RT = subj_data.showstim_RT;
            
            %Add if trial was rewarded or not
            df1_tmp.correct_incorrect = subj_data.showstim_ACC;
            
        end
                
        %Load  in the output of vba -- be careful here when loading in the
        %correct vba output. -- yes this contains the 'b' struc however if
        %the correct filter was not applied you have to run ARC all over
        %again for the proper filter to reflect in these files...
        vba_file = glob([file_strings.vba_output num2str(ball.id(i)) '*.mat']); %This path also need to be a variable
        load(vba_file{:}) %Out and posterior
        
        %Function to pull all vba, value data and entropy data
        [df1_tmp, out_F] = compile_vba_data(ball.id(i), df1_tmp, out, posterior, ''); 
        %df1_tmp.best_value_option = max_idx';
                
        %Grab log model evidence
        out_vba_L = [out_vba_L; [ball.id(i) out_F]];
        
% % %         %Grab value data
% % %         df1_tmp.value_A_stim = posterior.muX(1,:)'; %value of A choice
% % %         df1_tmp.value_B_stim = posterior.muX(2,:)'; %value of B choice
% % %         df1_tmp.value_C_stim = posterior.muX(3,:)'; %value of C choice
% % %         
% % %         df1_tmp.value_chosen = out.suffStat.value_chosen';
% % %         df1_tmp.value_max = out.suffStat.value'; %This is the max value of each hidden state per trial
% % %         [~,max_idx]=max(out.suffStat.muX(1:3,:));

% % %         
% % %         %Entropy
% % %         H = calc_entropy(out);
% % %         df1_tmp.H = H';
        
        %If we want to add the fixed parameter data -- maybe think of
        %making this a funciton if you have to merge more than 2 vba output
        %datasets i.e. if tables can be dynamically named supply a cell
        %list of varable names with out and posterior, or rename them after
        if merge_fixed_dataset
            vba_file = glob([file_strings.vba_output_fixed_params num2str(ball.id(i)) '*.mat']);
            load(vba_file{:})
            
            %Function to pull all vba, value data and entropy data
            [df1_tmp, out_fixed_F] = compile_vba_data(ball.id(i), df1_tmp, out, posterior, '_fixed_params');
                        
            %Pull log model evidence
            out_vba_L_fixed_params = [out_vba_L_fixed_params; [ball.id(i) out_fixed_F]];
            
% % %             df1_tmp.value_A_stim_fixed_params = posterior.muX(1,:)'; %value of A choice
% % %             df1_tmp.value_B_stim_fixed_params = posterior.muX(2,:)'; %value of B choice
% % %             df1_tmp.value_C_stim_fixed_params = posterior.muX(3,:)'; %value of C choice
% % %             
% % %             df1_tmp.value_chosen_fixed_params = out.suffStat.value_chosen';
% % %             df1_tmp.value_max_fixed_params = out.suffStat.value'; %This is the max value of each hidden state per trial
% % %             [~,max_idx]=max(out.suffStat.muX(1:3,:));
% % %             df1_tmp.best_value_option_fixed_params = max_idx';
% % %             
% % %             %Entropy
% % %             H_fixed_params = calc_entropy(out);
% % %             df1_tmp.H_fixed_params = H_fixed_params';
        end
        
        if merge_first_150
            %This is a special case in which the indices don't workout
            %unless we fill them with nans, I think it would be better to
            %just handle this case specifically instead of changing the
            %function to handle this senario
            vba_file = glob([file_strings.vba_output_first_150 num2str(ball.id(i)) '*.mat']);
            load(vba_file{:})
            
            %Function to pull all vba, value data and entropy data
            %[df1_tmp, out_first_150_F] = compile_vba_data(ball.id(i), df1_tmp, out, posterior, '_first_150');
            
            %Pull log model evidence
            out_vba_L_first_150 = [out_vba_L_first_150; [ball.id(i) out.F]];
            
            %Grab value chosen
            df1_tmp.value_chosen_first_150 = shiftMe([out.suffStat.value_chosen'; nan(150, 1)]')';
            df1_tmp.value_chosen_first_150(end) = nan;
            
            %Stim values are not shifted though
            df1_tmp.value_A_stim_first_150 = [posterior.muX(1,:)'; nan(150, 1)]; %value of A choice
            df1_tmp.value_B_stim_first_150 = [posterior.muX(2,:)'; nan(150, 1)]; %value of B choice
            df1_tmp.value_C_stim_first_150 = [posterior.muX(3,:)'; nan(150, 1)]; %value of C choice
            
            %PE
            df1_tmp.PE_chosen_first_150 = shiftMe([posterior.muX(4,:)'; nan(150, 1)]')';
            df1_tmp.PE_chosen_first_150(end) = nan;
        end
        
        %TODO:
        %Make this a function which takes posterior and out as arguments
        %and maybe a string of output variables for naming
        if merge_vba_mfx
            vba_idx = ismember(mfx_data.ID,ball.id(i));
            posterior = mfx_data.p_sub{vba_idx};
            out = mfx_data.o_sub{vba_idx};
            
            %Function to pull all vba, value data and entropy data
            [df1_tmp, out_mfx_F] = compile_vba_data(ball.id(i), df1_tmp, out, posterior, '_vba_mfx');
            
            %Pull log model evidence
            out_vba_L_mfx = [out_vba_L_mfx; [ball.id(i) out_mfx_F]];
            
% % %             %Calc value chosen
% % %             choices = out.suffStat.muX(1:3,:);
% % %             chosen_index = out.y;
% % %             chosen_index = carryValueForward(chosen_index,out.y);
% % %             out.suffStat.value_chosen = choices(logical(chosen_index))';
% % %             out.suffStat.value_chosen=shiftMe(out.suffStat.value_chosen);
% % %             df1_tmp.value_chosen_vba_mfx = out.suffStat.value_chosen';
% % %             
% % %             %Value max
% % %             out.suffStat.value = max(choices); %Max value of each hidden state per trial
% % %             out.suffStat.value=shiftMe(out.suffStat.value);
% % %             df1_tmp.value_max_vba_mfx = out.suffStat.value'; %This is the max value of each hidden state per trial
% % %             
% % %             
% % %             out_vba_mfx_L = [out_vba_mfx_L; [ball.id(i) out.F]];
% % %             
% % %             df1_tmp.value_A_stim_vba_mfx = posterior.muX(1,:)'; %value of A choice
% % %             df1_tmp.value_B_stim_vba_mfx = posterior.muX(2,:)'; %value of B choice
% % %             df1_tmp.value_C_stim_vba_mfx = posterior.muX(3,:)'; %value of C choice
% % %             
% % %             [~,max_idx]=max(out.suffStat.muX(1:3,:));
% % %             df1_tmp.best_value_option_vba_mfx = max_idx';
% % %             
% % %             %Entropy
% % %             H_vba_mfx = calc_entropy(out);
% % %             df1_tmp.H_vba_mfx = H_vba_mfx';
        end
        
        
        %Update dataframe
        df1 = [df1; df1_tmp];
        
        %% %--------- DF2 switches, errors, and demographics, parameter estimates -------------%
        
        %Setup id
        df2_tmp.ID = ball.id(i);
        
        %Grab switch errors
        df2_tmp.spont_switch_err = sum(subj_data.errors.spont_switch_err);
        df2_tmp.prob_switch_err = sum(subj_data.errors.prob_switch_err);
        df2_tmp.erratic_spont = sum(subj_data.errors.erratic_spont);
        df2_tmp.explore_switch = sum(subj_data.errors.explore_switch);
        df2_tmp.error_NOS = sum(subj_data.errors.error_NOS);
        
        %Grab the demographics and measures
        demog_idx = demog_data.ID==ball.id(i);
        df2_tmp.group1245 = demog_data.GROUP1245(demog_idx);
        df2_tmp.group12467 = demog_data.GROUP12467(demog_idx);
        df2_tmp.sex = demog_data.GENDERTEXT(demog_idx);
        %Split age dependent upon administration
        if strcmpi(dataset_to_compile,'fMRI')
            df2_tmp.age = demog_data.LEARNAGE(demog_idx);
        else
            df2_tmp.age = demog_data.BASELINEAGE(demog_idx);
        end
        df2_tmp.race = demog_data.RACETEXT(demog_idx);
        df2_tmp.education = demog_data.EDUCATION(demog_idx);
        df2_tmp.marital_status = demog_data.MARITALTEXT(demog_idx);
        df2_tmp.max_lethality = demog_data.MAXLETHALITY(demog_idx);
        
        %Update dataframe
        df2 = [df2; df2_tmp];
        
    end
    
end

%% Add log model evidence to df2
df2.L = out_vba_L(:,2);
df2.L_fixed_params = out_vba_L_fixed_params(:,2);
df2.L_first_150 = out_vba_L_first_150(:,2);
df2.L_vba_mfx = out_vba_L_mfx(:,2);


%% Fill any missing subjects from what Josh sent with nans
if strcmpi(dataset_to_compile,'fMRI')
    assesments=readtable('C:\kod\fMRI\from_josh\bandit_10-26-17.xlsx');
else
    assesments=readtable('C:\kod\Neuropsych_preproc\matlab\analysis\bandit\11-02-17 JON FINAL.xlsx'); %Does this work for the imaging sample? -- no it doesn't
end
%JON UNCOMMENT THIS ONCE YOU GET EVERYONE!
%df2=join(df2,assesments,'Keys','ID');

%% Parameters - load in the parameter matrix i.e. output from
vba_vanilla_param_table=compile_vba_parameters(file_strings.vba_output);

%Load the lookup table -- for now this is universal
load('C:\kod\fMRI\vba\bandit_parameter_lookup_table.mat')
vba_vanilla_param_table = merge_parameters(vba_vanilla_param_table,lookup_table);


%compile_vba_parameters and join to df2
%load('C:\kod\fMRI\vba\fMRI_param_estimates.mat')
% % % df2=join(df2,fMRI_parameter_estimates,'Keys','ID');
df2=join(df2,vba_vanilla_param_table,'Keys','ID');

% % % %Transform them -- this could be a funciton as well
% % % df2.alpha_win_transformed = 1./(1+exp(-df2.alpha_win)); % learning rate is bounded between 0 and 1.
% % % df2.alpha_loss_transformed = 1./(1+exp(-df2.alpha_loss)); % learning rate is bounded between 0 and 1.
% % % df2.decay_transformed = 1./(1+exp(-df2.decay)); % decay is bounded between 0 and 1.
% % % df2.beta_transformed = exp(df2.beta);

%Put in the fixed params?
if merge_fixed_dataset
    % Special case unless I think of something
% % %     vba_fixed_param_table=compile_vba_parameters(file_strings.vba_output_fixed_params);
% % %     lookup_table{:,2} = {'alpha_win_median'; 'alpha_loss_median'; 'decay_median'; 'beta_median'};
% % %     vba_fixed_param_table = merge_parameters(vba_fixed_param_table,lookup_table);

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
    
    vba_first_150_param_table=compile_vba_parameters(file_strings.vba_output_first_150);
    lookup_table(:,2) = {'alpha_win_first_150'; 'alpha_loss_first_150'; 'decay_first_150'; 'beta_first_150'};
    vba_first_150_param_table = merge_parameters(vba_first_150_param_table,lookup_table);
    
    
    load('C:\kod\fMRI\vba\fMRI_first_150_param_estimates.mat')
    %df2=join(df2,first_150_fMRI_param_table,'Keys','ID');
    df2=join(df2,vba_first_150_param_table,'Keys','ID');
    
    %Transform them -- this could be a funciton as well
%     df2.alpha_win_first_150_transformed = 1./(1+exp(-df2.alpha_win_first_150)); % learning rate is bounded between 0 and 1.
%     df2.alpha_loss_first_150_transformed = 1./(1+exp(-df2.alpha_loss_first_150)); % learning rate is bounded between 0 and 1.
%     df2.decay_first_150_transformed = 1./(1+exp(-df2.decay_first_150)); % decay is bounded between 0 and 1.
%     df2.beta_first_150_transformed = exp(df2.beta_first_150);
end

%Put in the parameters from the vba_mfx f(x)
if merge_vba_mfx
    %Might also be a special case that just makes more sense to compile in
    %this fashion
    
    %Remove the unneeded vars
    mfx_data.p_sub = [];
    mfx_data.o_sub = [];
    
    %Merge
    df2=join(df2,mfx_data,'Keys','ID');
    
    %Transform them -- this could be a funciton as well
    df2.alpha_win_vba_mfx_transformed = 1./(1+exp(-df2.alpha_win_mfx_data)); % learning rate is bounded between 0 and 1.
    df2.alpha_loss_vba_mfx_transformed = 1./(1+exp(-df2.alpha_loss_mfx_data)); % learning rate is bounded between 0 and 1.
    df2.decay_vba_mfx_transformed = 1./(1+exp(-df2.decay_mfx_data)); % decay is bounded between 0 and 1.
    df2.beta_vba_mfx_transformed = exp(df2.beta_mfx_data);
end



%% Write the tables to csvs
if exist(file_strings.save_path,'dir')
    mkdir(file_strings.save_path)
end
writetable(df1,[file_strings.save_path 'bandit_df1.csv'])
writetable(df2,[file_strings.save_path 'bandit_df2.csv'])

%TODO
%Print out some stats i.e. number of subjects in sample, save path (?), 
fprintf('\nCompilation complete!\n');


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

function H = calc_entropy(out)
value = out.suffStat.muX(1:3,:); %Hard code bad
for j = 1:length(value)
    val_per_trial = value(:,j);
    val_p_exp = exp(val_per_trial);
    val_p_exp_sum = sum(val_p_exp);
    val_p_softmax = val_p_exp./val_p_exp_sum;
    H(j) = calc_shannon_H(val_p_softmax);
end

function table_out = merge_parameters(parameter_table,lookup_table)
%Mock up of function to merge output of compile_vba_parameters into a
%table that contains the proper name for the parameters as well as the
%transformed and untransformed values.

%Initalize table
table_out = table();
table_out.ID = parameter_table.ID; %For merging later

%Kick out if lookup is not a cell
if ~iscell(lookup_table)
    error('Lookup table is not a cell exitting')
end


%Loop over parameter table names to rename and transform them respectively
for i = 1:length(parameter_table.Properties.VariableNames)
    lookup_t_idx=cellfun(@(x) ismember(x,parameter_table.Properties.VariableNames(i)), {lookup_table{:,1}})';
    if any(lookup_t_idx)
        table_out.(lookup_table{lookup_t_idx,2}) = parameter_table.(parameter_table.Properties.VariableNames{i});
        fh = lookup_table{lookup_t_idx,3}; %Grab function handle
        table_out.([lookup_table{lookup_t_idx,2} '_transformed']) = fh(table_out.(lookup_table{lookup_t_idx,2}));
         %parameter_table.Properties.VariableNames{paramter_table_name(i)} = lookup_table_name{i}   
    end
    
% % %     if strfind(paramter_table_name(i),'theta')
% % %         table_out.([lookup_table_or_cell_of_strings{i} '_transformed']) = 1./(1+exp(-paramter_table_name(i)));
% % %         %%Potentially if you didn't already rename them and save otherwise
% % %         %%use something like..
% % %         %parameter_table.Properties.VariableNames{paramter_table_name(i)} = lookup_table_name{i}
% % %     elseif strfind(paramter_table_name(i),'phi')
% % %         table_out.(lookup_table_or_cell_of_strings{i}) = exp(paramter_table_name(i));
% % %     else
% % %         continue
% % %     end
    
    
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

function [df, L] = compile_vba_data(id, df, out, posterior, suffix)
%vba_file = glob([path_to_data num2str(id) '*.mat']);
%load(vba_file{:}) %Out and posterior

%Value of each stimuli
df.(['value_A_stim' suffix]) = posterior.muX(1,:)'; %value of A choice
df.(['value_B_stim' suffix]) = posterior.muX(2,:)'; %value of B choice
df.(['value_C_stim' suffix]) = posterior.muX(3,:)'; %value of C choice

%Value chosen
% df.(['value_chosen_fixed_params' suffix]) = out.suffStat.value_chosen';
% df.(['value_max_fixed_params' suffix]) = out.suffStat.value'; %This is the max value of each hidden state per trial
% [~,max_idx]=max(out.suffStat.muX(1:3,:));
% df.(['best_value_option_fixed_params' suffix]) = max_idx';

%Calc value chosen
choices = out.suffStat.muX(1:3,:);
chosen_index = out.y;
chosen_index = carryValueForward(chosen_index,out.y);
out.suffStat.value_chosen = choices(logical(chosen_index))';
out.suffStat.value_chosen=shiftMe(out.suffStat.value_chosen);
df.(['value_chosen' suffix]) = out.suffStat.value_chosen';

%Value max
out.suffStat.value = max(choices); %Max value of each hidden state per trial
out.suffStat.value=shiftMe(out.suffStat.value);
df.(['value_max' suffix]) = out.suffStat.value'; %This is the max value of each hidden state per trial

%Best value option
[~,max_idx]=max(choices);
df.(['best_value_option' suffix]) = max_idx';

%Entropy
H = calc_entropy(out);
df.(['H' suffix]) = H';

%Prediction error
df.(['PE_chosen' suffix]) = shiftMe(out.suffStat.muX(4,:))'; %delta

%Log model evidence
L = out.F;


