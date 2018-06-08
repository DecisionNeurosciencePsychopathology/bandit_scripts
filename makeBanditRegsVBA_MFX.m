%%%%%%
% runs VBA analyses in MFX (mixed effects/empirical Bayes) form
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

%note: the bandit_vba_mfx file uses the structure of dirs rather than the
%id used in single-subject bandit_vba
[posterior_sub,out_sub,posterior_group,out_group,b] = bandit_vba_mfx(dirs,graphics,plot_subject,save_results,parameterization);

idNumbers=zeros(size(dirs,1));

%for the non-MFX (single-subject) VBA, this loop is used for model fitting,
%but since this is done for all subjects at once above, cut out most things
%and just use to 1) add subject ID to b, 2) ensure subj ID was run in
%MFX VBA above, and 3) ensure IDs in VBA output and in b match
for i = 1:length(dirs)
%     try
        id=str2double(dirs(i).name);
        b{i}.id = id;
        fprintf(num2str(id));
        
        if task_tracking==1
            %Update task_tracking data
            task_data.behave_completed=1;
           % Save all the ids processed
            idNumbers(i) = id;
            if out_sub{i}.id==id
                %Update task_tracking data
                task_data.behave_processed=1;
            else
                print(['error: subject IDs do not match for ID #',num2str(id)])
            end
        elseif out_sub{i}.id~=id
            print(['error: subject IDs do not match for ID #',num2str(id)])
        end
        
        %Write the regressors to file
        b{i} = banditmakeregressor_vba(b{i},out_sub{i},1);
        
        
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
%             errorlog('bandit',b{i}.id,exception)
%         else
%             fprintf(['error with subject #',num2str(id),'! \n'])
%         end
%     end
end

%make overall output
b_red=b(~cellfun('isempty',b));
check_ids=zeros(size(b_red,1),1);
for subj=1:length(b_red)
check_ids(subj,1)=b_red{subj}.id;
check_ids(subj,2)=out_sub{subj}.id;
end
if size(check_ids(check_ids(:,1)~=check_ids(:,2),:),1)>0
    print('warning: subject IDs in b and out_subj do not match!')
end


%Close up anything that's stil open
fclose all;

idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers