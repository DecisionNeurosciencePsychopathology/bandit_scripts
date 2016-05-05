dirs=dir('data/raw');
%If you want to check the names
%dirs(3:end).name

%filter these cases as they have either bad data or something else went
%wrong...
%filter = [202021; 210381];
%load('filter.mat')

%Set up input arguements
graphics = 0;
plot_subject=0;
valence=1;
decay=1;
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
        [posterior,out,b] = bandit_vba(id,graphics,plot_subject,valence, decay,save_results);
        idNumbers(i) = id; %Save all the ids processed
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

%make a text file of all the current ids
save idNumbers idNumbers