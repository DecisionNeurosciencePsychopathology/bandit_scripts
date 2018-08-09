%Simulate Subject with Learning Rules;
clear
%Set up functions
func_evo=@f_bandit_Qlearn;
func_obv=@g_bandit_softmax;
%Set parameters here: %Later when functionalized, put them outside please.
parameters.inF.fixed_params = 0; %DO NOT CHANGE THIS
parameters.inF.fix_decay = 0;
parameters.inF.valence = 0;
parameters.inF.disappointment = 0;
parameters.inF.utility = 0;
parameters.inF.use_reward_vec = 0;

parameters.inG.fixed_params = 0; %Again, if has fixed params, DO NOT USE;
parameters.inG.autocorrelation=0;

parameters.theta(1) = 0.176093991469957; %alpha_win
parameters.theta(2) = -0.625165021117312; %alpha_loss
parameters.theta(3) = 0.627873168355044; %lambda
%parameters.beta =  -1.353624512501712; %beta
parameters.beta =  -2; %new beta
parameters.numtrials = 300;
parameters.numchoices = 3;

%Set up the reinforcement scheudle, second line of u; %Now again, for a
%general function, get this part out of here;
design=bandit_fmri_load_design;
contengencys(:,1)=design.Arew(~isnan(design.Arew))';
contengencys(:,2)=design.Brew(~isnan(design.Brew))';
contengencys(:,3)=design.Crew(~isnan(design.Crew))';
contengencys=contengencys';


[gxs, values, decisions] = gen_pseudosubj(func_evo,func_obv,parameters,contengencys);

%can loop over like 300 subjects or something...lol


function [gxs, values, decisions] = gen_pseudosubj(func_evo,func_obv,parameters,contengencys)
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
%Set up decision/feedback vector; 
decisions=zeros(2,parameters.numtrials);
%%%%%Maybe later 
%Set up theta parameters;
thetas=repmat(parameters.theta',[1,parameters.numtrials]);
%Set up beta paramenters;
betas=repmat(parameters.beta',[1,parameters.numtrials]);



%single trial:
for y = 1:parameters.numtrials
gx=gxs(:,y);
beta=betas(:,y);
theta=thetas(:,y);
value=values(:,y);
decision=decisions(:,y);
%Update gx with choice;
gxi=func_obv(value,beta,decision,parameters.inG);
%Now we make a decision and see if we get enforeced;
gxs(:,y)=gxi;
[~,decision(1)] = max(binornd(10000,gxi));
[~,reinforced]=max(contengencys(:,y));
if decision(1) == reinforced
    decision(2) = 1;
else 
    decision(2) = 0;
end
decisions(:,y)=decision;

%Now we do a evolution!
fx = func_evo(value,theta,decision,parameters.inF);
values(:,y+1)=fx;
end

end







