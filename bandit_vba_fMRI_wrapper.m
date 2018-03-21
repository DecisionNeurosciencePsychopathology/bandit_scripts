%Handle dir paths
dirs=dir('subjects');

addpath('vba\')
addpath('behav_scripts\')

if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%Loading in the subjects that still need processed
load('fMRI_ids_to_run_vba_on.mat')

%The vanilla version is currently valence=1 decay=1 utility=0

%Set up input arguements
graphics = 0;
plot_subject=0;
valence=1;
fix_decay=0; %The logic is fixed
utility=0;
save_results=1;
fix_all_params = 0;
for i = 3:length(dirs)
    if dirs(i).bytes <=0 
        try
            id=str2double(dirs(i).name);
            b.id = id;
            
            %Save all the ids processed
            idNumbers(i) = id;
            %[posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
            [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, fix_decay,utility,save_results,fix_all_params);
            
        catch exception
            
            %Record errors in logger
            errorlog('bandit',b.id,exception)
        end
    end
end


% % for i = 1:length(fMRI_ids_to_run_vba_on)
% %         try
% %             id=fMRI_ids_to_run_vba_on(i);
% %             b.id = id;
% %             
% %             %Save all the ids processed
% %             idNumbers(i) = id;
% %             %[posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
% %             [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, fix_decay,utility,save_results,fix_all_params);
% %             
% %         catch exception
% %             
% %             %Record errors in logger
% %             errorlog('bandit',b.id,exception)
% %         end
% % end




%Close up anything that's stil open
fclose all;
