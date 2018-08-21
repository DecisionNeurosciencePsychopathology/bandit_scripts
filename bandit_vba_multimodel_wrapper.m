%Handle dir paths
clear
rootdir='~/Box Sync/skinner/data/eprime/bandit';
dirs=dir(rootdir);

addpath('vba/')
addpath('behav_scripts/')

if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%Run MFX instead of ssub?
run_MFX=1;
run_withPseudoSub=1;
injectNoise = 0.0;
%Loading in the subjects that still need processed
% load('fMRI_ids_to_run_vba_on.mat')

%The vanilla version is currently valence=1 decay=1 utility=0

% write model loop

%models = {'null', 'wsls_soft','1lr_decay', '2lr_decay', '2lr_no_decay','2lr_decay_sticky'};
%models = {'1lr_decay', '2lr_decay', '2lr_no_decay','2lr_decay_sticky'};
%models = {'1lr_decay', '2lr_decay', '2lr_no_decay', '2lr_decay_sticky'};
models = {'2lr_decay'};




for m = 1:length(models)
    
    model = models(m);
    modelx=models(m);
    parameterization.model = model;
    %Set up input arguements
    graphics = 0;
    s_graphics=0;
    g_graphics=0;
    plot_subject=0;
    save_results=0;
    
    parameterization.valence=0;
    parameterization.fix_decay=0;
    parameterization.utility=0;
    parameterization.fix_all_params=0;
    parameterization.disappointment = 0;
    parameterization.regret = 0;
    parameterization.use_reward_vec=0;
    parameterization.wsls = 0;
    parameterization.null = 0;
    parameterization.wsls_soft = 0;
    parameterization.sticky = 0;
    parameterization.runPseudo=run_withPseudoSub;
    
    if strcmp(model,'null')
        parameterization.null = 1;
        
    elseif strcmp(model,'wsls')
        parameterization.wsls = 1;
        
        
    elseif strcmp(model,'wsls_soft')
        parameterization.wsls_soft = 1;
        
    elseif strcmp(model,'1lr_decay')
        %Nothing need to be changed for this model
        
    elseif strcmp(model,'2lr_decay')
        parameterization.valence=1;
        
        
    elseif strcmp(model,'2lr_no_decay')
        parameterization.valence=1;
        parameterization.fix_decay=1;
        
    elseif strcmp(model,'2lr_decay_sticky')
        parameterization.valence=1;
        parameterization.fix_decay=1;
        parameterization.sticky = 1;

        
        
        
        %else
        %    parameterization.valence=1;
        %    parameterization.fix_decay=0; %The logic surrounds decay is kind of confusing
        %    parameterization.utility=0;
        %    parameterization.fix_all_params=0;
        %    parameterization.disappointment = 1;
        %    parameterization.regret = 0;
        %    parameterization.use_reward_vec=1;
        %    parameterization.wsls = 0;
        %    parameterization.null = 0;
        %    parameterization.wsls_soft = 0;
    end
    if run_withPseudoSub
        datadir=fullfile(rootdir,'vba_pseudosub','data',char(model),num2str(injectNoise));
        dirs=dir(datadir);
        run_MFX=1;
        parameterization.runPseudo=1;
        model = [char(model) '_wpseudosub'];
    else
        datadir=rootdir;
        dirs=dirs([dirs.isdir]);
    end
    
    if run_MFX
        dirs=dirs(3:length(dirs));
        dirs=dirs(~strcmp('.DS_Store',{dirs.name}));
        [posterior_sub,out_sub,posterior_group,out_group,b] = bandit_vba_mfx(datadir,dirs,g_graphics,s_graphics,plot_subject,save_results,parameterization);
        
        %Output
        file_path = fullfile(datadir,"vba_MFX_output");
        mkdir(file_path)
        file_name = sprintf('bandit_vba_MFX_output_%s',char(modelx));
        file_str = fullfile(file_path,file_name);
        save(char(file_str),'posterior_sub', 'out_sub', 'posterior_group','out_group','b','parameterization');
        
    else
        
        for i = 4:length(dirs)
            try
                if  dirs(i).bytes <=0
                    id=str2double(dirs(i).name);
                    b.id = id;
                    %Save all the ids processed
                    %                 idNumbers(i) = id;
                    %[posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
                    [posterior,out,b] = bandit_vba(datadir,id,graphics,plot_subject,save_results,parameterization);
                    
                    %HOT fix for VB new model
                    file_path = fullfile(datadir,"vba_output",char(modelx));
                    mkdir(file_path)
                    file_str = fullfile(file_path,file_name);
                    save(char(file_str),'posterior', 'out', 'b', 'parameterization')
             
                        
                end
                
              catch  
                %Record errors in logger
                %             errorlog('bandit',b.id,exception)
            end
        end
    end
end


%Now do group BMC;
if ~run_withPseudoSub
    [posterior,out,p] = bandit_grp_BMC('~/Box Sync/skinner/data/eprime/bandit',run_MFX,1,1);
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
