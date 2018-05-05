%Clear workspace
% clear;
% clc;

%Handle dir paths
dirs=dir('subjects');

addpath('vba\')
addpath('behav_scripts\')

%Initialize the tracking data.
task_data=initialize_task_tracking_data('bandit');

%if directories do not exist create them
if ~exist('regs','dir')
    mkdir('regs')
end
if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%The vanilla version is currently valence=1 decay=1 utility=0

%Set up input arguements
graphics = 0;
plot_subject=0;
save_results=1;
parameterization.valence=1;
parameterization.fix_decay=0; %The logic surrounds decay is kind of confusing
parameterization.utility=0;
parameterization.fix_all_params=0;
parameterization.disappointment = 0;
parameterization.regret = 0;
parameterization.use_reward_vec=0;

for i = 3:length(dirs)
    
    %Until I think of a more elegent fix
    %     if ismember(str2double(dirs(i).name),filter);
    %         continue
    %     end
    %
    %Quick patch for getting only the dirs not the .mat files
    %if dirs(i).bytes <=0 && exist(['regs/' dirs(i).name],'file')==0 %I think the 'regs' part is just to process those who haven't been processed yet
    if dirs(i).bytes <=0 
        try
            id=str2double(dirs(i).name);
            b.id = id;
            
            %Update task_tracking data
            task_data.behave_completed=1;
            
            %Save all the ids processed
            idNumbers(i) = id;
            %[posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, fix_decay,utility,save_results,fix_all_params);
            [posterior,out,b] = bandit_vba(id,graphics,plot_subject,save_results,parameterization);
            
            %Update task_tracking data
            task_data.behave_processed=1;
            
            %Write the regressors to file
            b = banditmakeregressor_vba(b,out);
            
            %We currently do not connect to Thorndike for bandit since
            %volumes are fixed, 

            %move the regressor files to thorndike
            newfolder='/Volumes/bek/learn/regs/bandit'; %folder to be place in within thorndike
            
            %get file paths
            scriptName = mfilename('fullpath');
            [currentpath, filename, fileextension]= fileparts(scriptName);
            moveregs(currentpath,num2str(id),newfolder);
            
            %write the task data to file
            record_subj_to_file(id,task_data)
            
        catch exception
            
            %write the task data to file
            record_subj_to_file(id,task_data)
            
            %Record errors in logger
            errorlog('bandit',b.id,exception)
        end
    end
end





%Close up anything that's stil open
fclose all;

%Update the filter data if needed
%save filter.mat filter

% % % bad_subjs = L==0;
% % % lambdas(bad_subjs)=[];
% % % % diff_of_stay_prob(bad_subjs)=[];
% % % win_ratio_10(bad_subjs) = [];
% % % win_ratio_25(bad_subjs) = [];
% % % win_ratio_50(bad_subjs) = [];
% % % loss_ratio_10(bad_subjs) = [];
% % % loss_ratio_25(bad_subjs) = [];
% % % loss_ratio_50(bad_subjs) = [];
idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers


%%OLD CODE

%             %Getting the correctlation of stay probailites and lambdas
%             lambdas(i-2) = posterior.muTheta(end,1);
%             %diff_of_stay_prob(i-2) = out.suffStat.diff_10_50_prob;
%             win_ratio_10(i-2) = out.suffStat.win_stay_10_prob;
%             win_ratio_25(i-2) = out.suffStat.win_stay_25_prob;
%             win_ratio_50(i-2) = out.suffStat.win_stay_50_prob;
%             loss_ratio_10(i-2) = out.suffStat.loss_stay_10_prob;
%             loss_ratio_25(i-2) = out.suffStat.loss_stay_25_prob;
%             loss_ratio_50(i-2) = out.suffStat.loss_stay_50_prob;
