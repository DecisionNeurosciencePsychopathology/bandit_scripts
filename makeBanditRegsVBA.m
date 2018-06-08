%%%%%%
% runs single-subject VBA analyses 
%
% to run, you will need paths to: 
% -VBA_toolbox_master, including subfolders
% -folders afni_opt and utils from skinner (in data/matlab/programs/)
% 
% also needed is a subfolder named 'subjects' that contains subjects'
%    behavioral output files
%
% enable task tracking by setting task_tracking value to 1; assumes that
% some functions (e.g. moveregs) only work with this since all are
% dependent on preprocessing automation pipeline functioning
%
%%%%%%

%toggle task tracking on/off (will need task tracking scripts if this is
%set to 1)
task_tracking=0; 

%Handle dir paths
dirs_all=dir('subjects');
subj_dirs=dirs_all([dirs_all.bytes]==0);
dirs=subj_dirs(3:end);

addpath('vba\')
addpath('behav_scripts\')

if task_tracking==1
    %Initialize the tracking data.
    task_data=initialize_task_tracking_data('bandit');
end

%if directories do not exist create them
if ~exist('regs','dir')
    mkdir('regs')
end
if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%The vanilla version is currently valence=1 decay=1 utility=0

%Set up input arguments
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

idNumbers=zeros(size(dirs,1));

for i = 1:length(dirs)
%     try
        id=str2double(dirs(i).name);
        b.id = id;
        
        if task_tracking==1
            %Update task_tracking data
            task_data.behave_completed=1;
            
            %Save all the ids processed
            idNumbers(i) = id;
        end
        
        [posterior,out,b] = bandit_vba(id,graphics,plot_subject,save_results,parameterization);
        
        if task_tracking==1
            %Update task_tracking data
            task_data.behave_processed=1;
        end
        
        %Write the regressors to file
        b = banditmakeregressor_vba(b,out);
        
        if  task_tracking==1
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
            scriptName = mfilename('fullpath');
            [currentpath, filename, fileextension]= fileparts(scriptName);
            moveregs(currentpath,num2str(id),newfolder);
            
            %write the task data to file
            record_subj_to_file(id,task_data)
        end
        
%     catch exception
%         if task_tracking==1
%             write the task data to file
%             record_subj_to_file(id,task_data)
%             
%             %Record errors in logger
%             errorlog('bandit',b.id,exception)
%         else
%             fprintf(['error with subject #',num2str(id),'!'])
%         end
%     end
end

%Close up anything that's stil open
fclose all;

idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers
