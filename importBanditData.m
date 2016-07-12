%This is hard coded but is should copy any folders from the pican drive to
%where I create the regs, I don't plan on moving them anytime in the future
%so this should hold up for a while.
% copyfile('K:\studies\suicide\docs\3-Armed Bandit ePrime\*','C:\kod\fMRI\bandit_raw\');
% addpath('C:\kod\fMRI\bandit_raw\');

%Script should now pull from google drive and only pull the most recent
%dirs not already in the local drive. N.B. You should change the name

%Also fix 210290 see email from Jun 10th his data is split into 2 files can
%we just combine?
remote_dirs = dir('E:\Users\wilsonj3\Google Drive\skinner\data - IP\eprime\bandit\*');
local_dirs = dir('c:\kod\fMRI\data\raw\*');

remote_dirs = {remote_dirs.name}';
local_dirs = {local_dirs.name}';

[idx,pos]=cellfun(@(x)ismember(remote_dirs,x),{local_dirs},'UniformOutput',0);

idx = idx{:};
new_ids={remote_dirs{~idx}};


if ~isempty(new_ids)
    for i = 1:length(new_ids)
        [status,message,messageId] = copyfile(['E:\Users\wilsonj3\Google Drive\skinner\data - IP\eprime\bandit\' new_ids{i}],['c:\kod\fMRI\data\raw\' new_ids{i}]);
    end
end



addpath('c:\kod\fMRI\data\raw\');