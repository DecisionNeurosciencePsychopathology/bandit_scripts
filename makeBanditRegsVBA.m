%Clear workspace
% clear;
% clc;
jj=1;
hh=1;

%Handle dir paths
dirs=dir('subjects');

addpath('vba\')
addpath('behav_scripts\')

%if directories do not exist create them
if ~exist('regs','dir')
    mkdir('regs')
end
if ~exist('vba_output','dir')
    mkdir('vba_output');
end

%If you want to check the names
%dirs(3:end).name

%filter these cases as they have either bad data or something else went
%wrong...
%filter = [202021; 210381];
%load('filter.mat')

%The vanilla version is currently valence=1 decay=1 utility=0

%Set up input arguements
graphics = 0;
plot_subject=0;
valence=1;
decay=1; %The logic surrounds decay is kind of confusing
utility=0;
save_results=1;

for i = 3:length(dirs)
    % make this a function in which you can overwrite everything, or check
    % typically you only make the regs for subject not yet processed.
    
    %Until I think of a more elegent fix
    %     if ismember(str2double(dirs(i).name),filter);
    %         continue
    %     end
    %
    %Quick patch for getting only the dirs not the .mat files
    if dirs(i).bytes <=0 && exist(['regs/' dirs(i).name],'file')==0
        try
            id=str2double(dirs(i).name);
            %id=46069;
            [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
            L(i-2) = out.F;
            idNumbers(i) = id; %Save all the ids processed
            
            %Getting the correctlation of stay probailites and lambdas
            lambdas(i-2) = posterior.muTheta(end,1);
            %diff_of_stay_prob(i-2) = out.suffStat.diff_10_50_prob;
            win_ratio_10(i-2) = out.suffStat.win_stay_10_prob;
            win_ratio_25(i-2) = out.suffStat.win_stay_25_prob;
            win_ratio_50(i-2) = out.suffStat.win_stay_50_prob;
            loss_ratio_10(i-2) = out.suffStat.loss_stay_10_prob;
            loss_ratio_25(i-2) = out.suffStat.loss_stay_25_prob;
            loss_ratio_50(i-2) = out.suffStat.loss_stay_50_prob;
            %     catch
            %         warning('Bad run detected look at filter!')
            %         filter(i) = id;
            %         continue
            %     end
            
            %     try
            
            %Write the regressors to file
            b = banditmakeregressor_vba(b,out);
            
            
            %move the regressor files to thorndike
            newfolder='/Volumes/bek/learn/regs/bandit'; %folder to be place in within thorndike
            
            %get file paths
            scriptName = mfilename('fullpath');
            [currentpath, filename, fileextension]= fileparts(scriptName);
            moveregs(currentpath,num2str(id),newfolder);
            
            %write the ids that successfully ran into a cell
            ID(jj,1)=b.id;
            
            
            task={'bandit'};
            Task{jj,1}=task;
            
            trialdone=fopen('idlog_bandit.txt', 'a+');
            trialdone=fscanf(trialdone,'%d');
            
            
            trialdone1=0;
            for aa=1:length(trialdone)
                if trialdone(aa,1) == b.id
                    trialdone1=1;
                end
            end
            
            if trialdone1 == 1
                td={'yes'};
            else
                td={'no'};
            end
            fMRI_Preprocess_Complete{jj,1}=td;
            
            jj=jj+1;
            
            %turn completed cell into table
            bt=table(ID,Task,fMRI_Preprocess_Complete);
            save('completed','bt')
            
            
        catch exception
            
            errorlog('bandit',b.id,exception)
            
            %put IDs that didn't run into table
            ID2(hh,1)=b.id;
            
            task={'bandit'};
            Task2{hh,1}=task;
            
            hh=hh+1;
            
            bt2=table (ID2,Task2);
            save('unable_to_run','bt2')
            
        end
        
        
        if exist('bt2')==0
            ID2=0;
            Task2={'bandit'};
            bt2=table(ID2,Task2);
            save('unable_to_run','bt2')
        end
        
    end
end





%Close up anything that's stil open
fclose all;

%Update the filter data if needed
%save filter.mat filter

bad_subjs = L==0;
lambdas(bad_subjs)=[];
% diff_of_stay_prob(bad_subjs)=[];
win_ratio_10(bad_subjs) = [];
win_ratio_25(bad_subjs) = [];
win_ratio_50(bad_subjs) = [];
loss_ratio_10(bad_subjs) = [];
loss_ratio_25(bad_subjs) = [];
loss_ratio_50(bad_subjs) = [];
idNumbers(idNumbers==0)=[];

%make a text file of all the current ids
save idNumbers idNumbers