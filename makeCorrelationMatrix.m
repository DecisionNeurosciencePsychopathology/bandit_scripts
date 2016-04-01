%% Finally figured out correlation matrix 
%Probably could make this pretty nice by automating everything, make a gui
%even and pick which variables you want to see from a struct? That's
%probably overkill but it'd be nice to have.

%Load in the subject ids from the regression folder
dirs=dir('bandit_regs');

%Initalize the struct
correlationStruct =[];

for idx = 3:2:length(dirs)
  %load in data
    load([pwd,'\bandit_regs\',dirs(idx,1).name])
    
     %grab name
    name=dirs(idx,1).name(7:end-4);


%data = [b.deltaplus' b.deltaminus' b.essd' b.emsd'];
%Put variables here
%This is after hrf convolution
% data = [b.hrf_regs.alexChoice',b.hrf_regs.alexFeed',b.hrf_regs.alexDeltaplus_MC', b.hrf_regs.alexDeltaminus_MC',...
%     b.hrf_regs.alexEchosen_MC',b.hrf_regs.Value_diff_best',b.hrf_regs.Value_diff_all',b.hrf_regs.alexAction_2',...
%     b.hrf_regs.alexAction_3',b.hrf_regs.alexAction_7'];

data = [b.hrf_regs.alexChoice',b.hrf_regs.alexFeed',b.hrf_regs.alexRewardVec_MC', b.hrf_regs.alexCorrectFeed_MC',...
    b.hrf_regs.alexInCorrectFeed_MC',b.hrf_regs.Value_diff_best',b.hrf_regs.Value_diff_all',b.hrf_regs.alexAction_2',...
    b.hrf_regs.alexAction_3',b.hrf_regs.alexAction_7'];

[r,p]=corrcoef(data);

% Row and column names of variables needing correlated
% names = {'Choice' 'Feedback' 'Deltaplus' 'Deltaminus' 'Echosen' 'ValDiffBest' 'ValdiffAll'...
%     'Action2' 'Action3' 'Action7'};

names = {'Choice' 'Feedback' 'Stake' 'Correct' 'Incorrect' 'ValDiffBest' 'ValdiffAll'...
    'Action2' 'Action3' 'Action7'};
CovMat{1,1} =''; %Initialize

for i = 2:length(names)+1
    CovMat{i,1} = names{i-1};
    CovMat{1,i} = names{i-1};
    for j = 2:length(names)+1
        CovMat{i,j} = r(i-1,j-1);
    end 
end

CovMat{1,1} =name; % set subj ID in top left corner

%Save everything to struct
correlationStruct = setfield(correlationStruct,['id' name],CovMat);
end