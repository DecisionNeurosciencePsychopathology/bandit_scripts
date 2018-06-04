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
% enable task tracking by setting task_tracking value to 1
%
%%%%%%

%toggle task tracking on/off (will need task tracking scripts if this is
%set to 1
task_tracking=0; 

%Handle dir paths
dirs=dir('subjects');

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

int_num=1; %current dirs has many empty cells, one thing to do in the future 
%strip these out ahead of time rather than use this workaround to only use
%the cells with actual data

%for the non-MFX (single-subject) VBA, this loop is used for model fitting,
%but since this is done for all subjects at once above, disable most things
%and just use to 1) add subject ID to b, 2) ensure subj ID was run in
%MFX VBA above, and 3) ensure IDs in VBA output and in b match
for i = 3:length(dirs)
    
    if dirs(i).bytes <=0 
%         try 
%             can-reenable try-catch statement but disabled for now to aid 
%             in debugging
            
            id=str2double(dirs(i).name);
            b{i-2}.id = id;
            fprintf(num2str(id));
            
            if task_tracking==1
                Update task_tracking data
                task_data.behave_completed=1;
                Save all the ids processed
                idNumbers(i) = id;
                if out_sub{int_num}.id==id
                    Update task_tracking data
                    task_data.behave_processed=1;
                else
                    print(['error: subject IDs do not match for ID #',num2str(id)])
                end
            elseif out_sub{int_num}.id~=id
                print(['error: subject IDs do not match for ID #',num2str(id)])
            end
            
            %Write the regressors to file
            b{i-2} = banditmakeregressor_vba(b{i-2},out_sub{int_num});
            
            if  exist(moveregs)>0 %#ok<EXIST>
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
                
                if task_tracking==1
                    %write the task data to file
                    record_subj_to_file(id,task_data)
                end
            end

            
%         catch exception
%             
%             %write the task data to file
%             %record_subj_to_file(id,task_data) 
%             
%             %Record errors in logger
%             errorlog('bandit',b.id,exception)
%         end
        int_num=int_num+1;
    end
end

%make overall output
b_red=b(~cellfun('isempty',b));
check_ids=b_red{:}.id(b_red{:}.id~=out_subj{:}.id,:);


%Close up anything that's stil open
fclose all;

idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers