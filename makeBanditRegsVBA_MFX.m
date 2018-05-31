%Clear workspace
% clear;
% clc;

%Handle dir paths
dirs=dir('subjects');

addpath('vba\')
addpath('behav_scripts\')

%Initialize the tracking data.
%task_data=initialize_task_tracking_data('bandit');

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

%note: the bandit_vba_mfx file uses the structure of dirs rather than the
%id used in single-subject bandit_vba
[posterior,out,b] = bandit_vba_mfx(dirs,graphics,plot_subject,save_results,parameterization);

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
            %[posterior,out,b] = bandit_vba_mfx(dirs,graphics,plot_subject,save_results,parameterization);
            
            %Update task_tracking data
            task_data.behave_processed=1;
            
            %Write the regressors to file
            b = banditmakeregressor_vba(b,out);
            
            %We currently do not connect to Thorndike for bandit since
            %volumes are fixed, 

            %move the regressor files to thorndike
            if exist('/Volumes/bek','dir')==7
                newfolder='/Volumes/bek/learn/regs/bandit'; %folder to be place in within thorndike
            elseif exist('T:/learn/','dir')==7 %VB filepath
                newfolder='T:/learn/regs/bandit'; 
            else
                print('unfamiliar directory structure')
            end
            
            %get file paths
%             scriptName = mfilename('fullpath');
%             [currentpath, filename, fileextension]= fileparts(scriptName);
%             moveregs(currentpath,num2str(id),newfolder); %%%%%NO FILE
                
   
            %write the task data to file
            %record_subj_to_file(id,task_data) %%%%NO FILE

            
        catch exception
            
            %write the task data to file
            %record_subj_to_file(id,task_data) %%%%NO FILE
            
            %Record errors in logger
            errorlog('bandit',b.id,exception)
        end
    end
end


%Close up anything that's stil open
fclose all;

idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers