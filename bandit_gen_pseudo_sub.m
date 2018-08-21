%Simulate Subject with Learning Rules;
clear
datadir='~/Box Sync/skinner/data/eprime/bandit';
addpath('vba/')
addpath('behav_scripts/')
runWithFixPara=0;
modelname='2lr_decay';
parameters.N = 1;  %Number of stimulation to run (if used in subject para mode, it will do N times n subjects
injectNoise = 0.4;

%Set up functions
func_evo=@f_bandit_Qlearn;
func_obv=@g_bandit_softmax;
%Set parameters here: %Later when functionalized, put them outside please.
parameters.inF.fixed_params = 0; %DO NOT CHANGE THIS
parameters.inF.fix_decay = 0;
parameters.inF.valence = 1;
parameters.inF.disappointment = 0;
parameters.inF.utility = 0;
parameters.inF.use_reward_vec = 0;

parameters.inG.fixed_params = 0; %Again, if has fixed params, DO NOT USE;
parameters.inG.autocorrelation=0;



if runWithFixPara
    parameters.theta(1) = 0.176093991469957; %alpha_win
    parameters.theta(2) = -0.625165021117312; %alpha_loss
    parameters.theta(3) = 0.627873168355044; %lambda
    %parameters.beta =  -1.353624512501712; %beta
    parameters.beta =  10; %new beta
    parameters.id= 'fake_fixpara';
    posterior_sub(1)={parameters};
    
else
    filename=['bandit_vba_MFX_output_' modelname '.mat'];
    load(fullfile(datadir,'vba_MFX_output',filename));
end
parameters.numtrials = 300;
parameters.numchoices = 3;

%Set up the reinforcement scheudle, second line of u; %Now again, for a
%general function, get this part out of here;
design=bandit_fmri_load_design;
contengencys(:,1)=design.Arew(~isnan(design.Arew))';
contengencys(:,2)=design.Brew(~isnan(design.Brew))';
contengencys(:,3)=design.Crew(~isnan(design.Crew))';
contengencys=contengencys';

for xj = 1:length(posterior_sub)
    posd=posterior_sub{xj};
    if ~runWithFixPara
        parameters.theta = posd.muTheta'; %hidden states
        parameters.beta =  posd.muPhi'; %new beta
        id=posd.id;
    end
    for xk = 1:parameters.N
        [gxs, values, decisions,noises] = gen_pseudosubj(func_evo,func_obv,parameters,contengencys,injectNoise);
        bx.chosen_stim=decisions(1,:)';
        bx.stim_ACC=decisions(2,:)';
        bx.id=id;
        bx.noise=injectNoise;
        foldername = fullfile(datadir,'vba_pseudosub','data',modelname,num2str(injectNoise));
        mkdir(foldername);
        file_str = fullfile(foldername,sprintf('vba_%s_id_%d_runt_%d_noise_%s_output.mat',char(modelname),id,xk,num2str(injectNoise)));
        save(file_str, 'bx','gxs','values','decisions','noises')
    end
    
end
%can loop over like 300 subjects or something...lol


function [gxs, values, decisions,noises] = gen_pseudosubj(func_evo,func_obv,parameters,contengencys,injectNoise)
%we initialize;
%Set up probablity distribution;
gxs=zeros(parameters.numchoices,parameters.numtrials);
for z = 1:parameters.numchoices
    gx(z)= 1/parameters.numchoices;
end
gxs(:,1)=gx;
%Set up prediction error vector;
vx=gx;
values=zeros(parameters.numchoices+1,parameters.numtrials);
vx(4)=0;
values(:,1)=vx;
noises=zeros(parameters.numchoices+1,parameters.numtrials);
%Set up decision/feedback vector;
decisions=zeros(2,parameters.numtrials);

%Set up theta parameters;
if length(parameters.theta)==parameters.numtrials
    thetas=parameters.theta;
else
    thetas=repmat(parameters.theta',[1,parameters.numtrials]);
end
%Set up beta paramenters;
if length(parameters.beta)==parameters.numtrials
    betas=parameters.beta;
else
    betas=repmat(parameters.beta',[1,parameters.numtrials]);
end

if isempty(injectNoise)
    injectNoise = 0;
end
%single trial:
for y = 1:parameters.numtrials
    gx=gxs(:,y);
    beta=betas(:,y);
    theta=thetas(:,y);
    value=values(:,y);
    decision=decisions(:,y);
    sd = injectNoise;
    noise = sd.*randn(3,1);
    noise(4) = 0;
    value=value+noise;
    noises(:,y)=noise;
    %Update gx with choice;
    gxi=func_obv(value,beta,decision,parameters.inG);
    %Now we make a decision and see if we get enforeced;
    gxs(:,y)=gxi;
    decision(1) = find(mnrnd(1,gxi'));
    reinforced=find(contengencys(:,y));
    if ismember(decision(1),reinforced)
        decision(2) = 1;
    else
        decision(2) = 0;
    end
    decisions(:,y)=decision;
    
    %Now we do a evolution!
    fx = func_evo(value,theta,decision,parameters.inF);
    if ~isempty(find(fx > 1,1))
        fx(fx > 1)=1;
    end
    if ~isempty(find(fx < -1,1))
        fx(fx < -1)=-1;
    end
    values(:,y+1)=fx;
end

end







