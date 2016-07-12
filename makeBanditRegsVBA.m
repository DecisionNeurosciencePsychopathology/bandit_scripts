%Clear workspace
clear;
clc;

dirs=dir('data/raw');
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
parfor i = 3:length(dirs)
    % make this a function in which you can overwrite everything, or check
    % typically you only make the regs for subject not yet processed.
    
    %Until I think of a more elegent fix
%     if ismember(str2double(dirs(i).name),filter);
%         continue
%     end
%     
    
    try
        id=str2double(dirs(i).name);
        %id=46069;
        [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,utility,save_results);
        L(i-2) = out.F;   
        idNumbers(i) = id; %Save all the ids processed
        
        %Getting the correctlation of stay probailites and lambdas
        lambdas(i-2) = posterior.muTheta(end,1);
        diff_of_stay_prob(i-2) = out.suffStat.diff_10_50_prob;
        ratio_10(i-2) = out.suffStat.stay_10_prob;
        ratio_50(i-2) = out.suffStat.stay_50_prob;
        
        
    catch
        warning('Bad run detected look at filter!')
        filter(i) = id;
        continue
    end
    
    %Write the regressors to file
    b = banditmakeregressor_vba(b,out);
    
    
end

%Close up anything that's stil open
fclose all;

%Update the filter data if needed
%save filter.mat filter

bad_subjs = L==0; 
lambdas(bad_subjs)=[];
diff_of_stay_prob(bad_subjs)=[];
ratio_10(bad_subjs)=[];
ratio_50(bad_subjs)=[];

%make a text file of all the current ids
save idNumbers idNumbers