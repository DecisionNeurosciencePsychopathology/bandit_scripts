%Handle dir paths

%
dirs=dir('~/Box Sync/skinner/data/eprime/bandit');


addpath('vba/')
addpath('behav_scripts/')

if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%Loading in the subjects that still need processed
% load('fMRI_ids_to_run_vba_on.mat')

%The vanilla version is currently valence=1 decay=1 utility=0

% write model loop

% models = {'null', 'wsls', '1lr_decay', '2lr_decay', '2lr_no_decay'};
models = {'null'};

for m = 1:length(models)
    
    model = models(m);
    
    parameterization.model = model;
    %Set up input arguements
    graphics = 0;
    plot_subject=0;
    save_results=0;
    
    if strcmp(model,'null')
        parameterization.valence=0;
        parameterization.fix_decay=0;
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 0;
        parameterization.regret = 0;
        parameterization.use_reward_vec=0;
        parameterization.wsls = 0;
        parameterization.null = 1;
    elseif strcmp(model,'wsls')
        parameterization.valence=0;
        parameterization.fix_decay=0;
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 0;
        parameterization.regret = 0;
        parameterization.use_reward_vec=0;
        parameterization.wsls = 1;
        parameterization.null = 0;
        
    elseif strcmp(model,'1lr_decay')
        parameterization.valence=0;
        parameterization.fix_decay=0;
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 0;
        parameterization.regret = 0;
        parameterization.use_reward_vec=0;
        parameterization.wsls = 0;
        parameterization.null = 0;
        
    elseif strcmp(model,'2lr_decay')
        parameterization.valence=1;
        parameterization.fix_decay=0;
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 0;
        parameterization.regret = 0;
        parameterization.use_reward_vec=0;
        parameterization.wsls = 0;
        parameterization.null = 0;
        
        
    elseif strcmp(model,'2lr_no_decay')
        parameterization.valence=1;
        parameterization.fix_decay=1;
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 0;
        parameterization.regret = 0;
        parameterization.use_reward_vec=0;
        parameterization.wsls = 0;
        parameterization.null = 0;
        
    else
        parameterization.valence=1;
        parameterization.fix_decay=0; %The logic surrounds decay is kind of confusing
        parameterization.utility=0;
        parameterization.fix_all_params=0;
        parameterization.disappointment = 1;
        parameterization.regret = 0;
        parameterization.use_reward_vec=1;
        parameterization.wsls = 0;
        parameterization.null = 0;
    end
    
    
    for i = 4:length(dirs)
        if  dirs(i).bytes <=0
            id=str2double(dirs(i).name);
            b.id = id;
            
            %Save all the ids processed
            %                 idNumbers(i) = id;
            %[posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
            [posterior,out,b] = bandit_vba(id,graphics,plot_subject,save_results,parameterization);
            
            %HOT fix for VB new model
            file_path = '/Volumes/bek/datafolderfromjonEdrive/bandit/new_model_vb';
            file_name = sprintf('id_%d_bandit_vba_output_%d_rewVec_new_model',id,use_reward_vec);
            file_str = [file_path filesep file_name];
            save(file_str,'posterior', 'out', 'b', 'parameterization')
            
            
            
            
            
            %Record errors in logger
            %             errorlog('bandit',b.id,exception)
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
