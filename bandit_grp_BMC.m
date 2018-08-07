
function [posterior,out] = bandit_grp_BMC(DataDir,isMFX,savefile,MFXFolderName,noMFXFolderName,MFXFileName)


%Where is the root path for the processed data, should contain a model
%folder & within it are subject vba files
%DataDir='~/Box Sync/skinner/data/eprime/bandit';
%
%exampel:
% bandit_grp_BMC('~/Box Sync/skinner/data/eprime/bandit',0,1) for noMFX

if nargin > 6
    error('At most 6 inputs');
end

% Fill in unset optional values.
switch nargin
    case 2
        savefile = 1;
        MFXFolderName = 'vba_MFX_output';
        noMFXFolderName = 'vba_output';
        MFXFileName = "bandit_vba_MFX_output_";
    case 3
        MFXFolderName = 'vba_MFX_output';
        noMFXFolderName = 'vba_output';
        MFXFileName = "bandit_vba_MFX_output_";
    case 4
        noMFXFolderName = 'vba_output';
        MFXFileName = "bandit_vba_MFX_output_";
    case 5
        MFXFileName = "bandit_vba_MFX_output_";
end

if isMFX 
   TypeN=MFXFolderName;
   Models=dir(fullfile(DataDir,TypeN));
   Models=Models(~[Models.isdir]);
   TypeK=fullfile(TypeN,'model_evidence');
else
   TypeN = noMFXFolderName;
   Models=dir(fullfile(DataDir,TypeN));
   Models=Models(3:length(Models));
   Models=Models([Models.isdir]);
   TypeK=TypeN;
end

Models=Models(~strcmp('.DS_Store',{Models.name}));
AllSubOutput = struct();
AllL=struct();
for x = 1:length(Models) 
    Model = Models(x);
    if isMFX
        load(fullfile(Model.folder,Model.name));
        Subs=out_sub;
        Model.name=char(extractBetween(Model.name,MFXFileName,".mat"));
        Models(x).name=Model.name;
    else
        Subs=dir(fullfile(Model.folder,Model.name));
        Subs=Subs(~[Subs.isdir]);
    end
    Models(x).name=strrep(Model.name,'_',' ');
    AllL.(['L_' char(Model.name)])=zeros(size(Subs))';
    % write an L for BMC directly
    for j = 1:length(Subs)
        if isMFX
            out=out_sub{j,1};
            posterior=posterior_sub{j,1};
            Id=out.options.inF.b.id;
        else
            Sub=Subs(j);
            load(fullfile(Sub.folder,Sub.name));
            Id=str2double(regexp(Sub.name,'\d*','Match'));
            Id=Id(1);
        end
        AllSubOutput.(['model_' char(Model.name)]).(sprintf('id_%d',Id)).out=out;
        AllSubOutput.(['model_' char(Model.name)]).(sprintf('id_%d',Id)).posterior=posterior;
        AllL.(['L_' char(Model.name)])(j)=out.F;
    end
    
end

%Made them Ls:
%We at here assumed that all model has same subject number (the dim of
%BanditL is dependent on number of models and the last model's subject
%number, if the subj number is NOT the same; it will break subsequent code;
BanditL = zeros(length(Models),length(Subs));
for y = 1:length(fieldnames(AllL))
    yn=fieldnames(AllL);
    ynx=yn(y);
    BanditL(y,:) = AllL.(char(ynx));
    
end

options.modelNames = {Models.name};

[posterior,out] = VBA_groupBMC(BanditL,options);



if savefile 
file_path = fullfile(DataDir,TypeK);
mkdir(file_path)
file_name = sprintf('bandit_vba_model_evidence_%d_models',length(Models));
file_str = fullfile(file_path,file_name);
save(char(file_str),'posterior', 'out')
end 
