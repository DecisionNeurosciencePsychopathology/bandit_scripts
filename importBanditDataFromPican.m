%This is hard coded but is should copy any folders from the pican drive to
%where I create the regs, I don't plan on moving them anytime in the future
%so this should hold up for a while.
copyfile('K:\studies\suicide\docs\3-Armed Bandit ePrime\*','C:\kod\fMRI\bandit_raw\');
addpath('C:\kod\fMRI\bandit_raw\');

%3/15/2016
%So I'm not sure if the data has moved off Pican yet and is not on google
%drive so check where the data is now being stored, second you need to make
%sure the raw folders only have the most recent .txt. and .edat files in
%their respective subject folders. You can either copy everything and
%delete later or just copy the newest files from the get go. Not sure what
%would be easier.