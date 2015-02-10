function [cost,constr,e1,e2,e3,Pr1,Pr2,Pr3,delta,echosen,etotal]=rlgetexp3(params)

%% phiwin - learning rate from rewards, philoss - ~from punishments;
%% alpha - memory, c - 1-exploration
%phiwin=params(1); philoss=params(2); alpha=params(3); c=params(4);

% Gamma=params(1); AlphaWin=params(2); AlphaLoss=params(3); %LossAlpha=params(4);
% c=params(4);
%Gamma=.01;
AlphaWin=params(1); AlphaLoss=params(2); %LossAlpha=params(4);
c=params(3);

%% simps won't run without constr
constr=[];
%c=1;
load modeldata; % establishes s
% goodtrials=find(s.choice<4);

%%

e1=0.*ones(1,length(s.choice));
e2=0.*ones(1,length(s.choice));
e3=0.*ones(1,length(s.choice));

e1(1)=0;
e2(1)=0;
e3(1)=0;


Pr1=zeros(1,length(s.choice));
Pr2=zeros(1,length(s.choice));
Pr3=zeros(1,length(s.choice));
delta=zeros(1,length(s.choice));
%delta1=zeros(1,length(s.choice));
% echosen=zeros(1,length(s.choice));
WinDecay=1;
LossDecay=1;


%% get the reward on each trial.
r=s.feed;% 1=win, 0=loose

for ct=1:length(s.choice)-1
    %% Calculate the reward/punishment expectancies associated with each stimulus
    %% On rewarded trials, apply the learning rate for rewards
    if s.feed(ct)>0
        if s.choice(ct)==1
            e1(ct+1)=e1(ct)+ AlphaWin.*(r(ct)-e1(ct));
            e2(ct+1)=LossDecay.*e2(ct);%-(e1(ct)-e1(ct-1))/2;
            e3(ct+1)=LossDecay.*e3(ct);%-(e1(ct)-e1(ct-1))/2;
            delta(ct)=r(ct)-e1(ct);
        elseif s.choice(ct)==2
            e2(ct+1)=e2(ct)+ AlphaWin.*(r(ct)-e2(ct));
            e1(ct+1)=LossDecay.*e1(ct);%-(e2(ct)-e2(ct))/2;
            e3(ct+1)=LossDecay.*e3(ct);%-(e2(ct)-e2(ct))/2;
            delta(ct)=r(ct)-e2(ct);
        elseif s.choice(ct)==3
            e3(ct+1)=e3(ct)+ AlphaWin.*(r(ct)-e3(ct));
            e1(ct+1)=LossDecay.*e1(ct);%-(e3(ct)-e3(ct))/2;
            e2(ct+1)=LossDecay.*e2(ct);%-(e3(ct)-e3(ct))/2;
            delta(ct)=r(ct)-e3(ct);
        elseif s.choice(ct)==999 %meaning a no-response trial
            e1(ct+1)=e1(ct);
            e2(ct+1)=e2(ct);
            e3(ct+1)=e3(ct);
            delta(ct)=0;
        end
        %% On punished trials, apply the learning rate for punishments
        
    elseif s.feed(ct)==0
        if s.choice(ct)==1
            e1(ct+1)=e1(ct)+ AlphaLoss.*(r(ct)-e1(ct));
            e2(ct+1)=WinDecay.*e2(ct);%+(e1(ct)-e1(ct))/2;
            e3(ct+1)=WinDecay.*e3(ct);%+(e1(ct)-e1(ct))/2;
            delta(ct)=r(ct)-e1(ct);
        elseif s.choice(ct)==2
            e2(ct+1)=e2(ct)+ AlphaLoss.*(r(ct)-e2(ct));
            e1(ct+1)=WinDecay.*e1(ct);%+(e2(ct)-e2(ct))/2;
            e3(ct+1)=WinDecay.*e3(ct);%+(e2(ct)-e2(ct))/2;
            delta(ct)=r(ct)-e2(ct);
        elseif s.choice(ct)==3
            e3(ct+1)=e3(ct)+ AlphaLoss.*(r(ct)-e3(ct));
            e1(ct+1)=WinDecay.*e1(ct);%+(e3(ct)-e3(ct))/2;
            e2(ct+1)=WinDecay.*e2(ct);%+(e3(ct)-e3(ct-2))/2;
            delta(ct)=r(ct)-e3(ct);
        elseif s.choice(ct)==999 %meaning a no-response trial
            e1(ct+1)=e1(ct);
            e2(ct+1)=e2(ct);
            e3(ct+1)=e3(ct);
            delta(ct)=0;
        end
    end
    
    
    %% calculate probability of chosing a given stimulus
    
    Pr1=exp(c.*(e1))./(exp(c.*(e1))+exp(c.*(e2))+ exp(c.*(e3)));
    Pr2=exp(c.*(e2))./(exp(c.*(e1))+exp(c.*(e2))+ exp(c.*(e3)));
    Pr3=exp(c.*(e3))./(exp(c.*(e1))+exp(c.*(e2))+ exp(c.*(e3)));
    
    
end
%% calculate expected value for CHOSEN stimulus (not for model fitting)
%find the trials where sub chose each stimulus
chose1=(s.choice==1);
chose2=(s.choice==2);
chose3=(s.choice==3);
echosen=e1.*chose1'+e2.*chose2'+e3.*chose3';
etotal=e1+e2+e3;

%% cost is the

% pmax=max(Pr1,Pr2);
% pmax=max(pmax,Pr3);

% cost=-sum(log(pmax));
% logprob=log(abs(e1-chose1'))+log(abs(e2-chose2'))+log(abs(e3-chose3'));
% logprob=log(abs(Pr1-chose1))+(abs(Pr2-chose2))+(abs(Pr3-chose3));

% logprob=log(abs(Pr1-chos+e1))+(abs(Pr2-chose2))+(abs(Pr3-chose3));
% cost=(sum(logprob));

%cost=sum(abs(Pr1-chose1')+abs(Pr2-chose2')+abs(Pr3-chose3'));
%% let's only worry about predicting the chosen stimulus: cost = 1 - p(chosen_by_subj)
stim_chosen(chose1) = 1;
stim_chosen(chose2) = 2;
stim_chosen(chose3) = 3;
stim_chosen = stim_chosen';
stim_chosen(stim_chosen==0)=[]; %Censor out bad trials


for n = 1:length(stim_chosen)
    if stim_chosen(n) == 1
        subj_choice_prob(n) = Pr1(n);
    elseif stim_chosen(n) == 2
        subj_choice_prob(n) = Pr2(n);
    elseif stim_chosen(n) == 3
        subj_choice_prob(n) = Pr3(n);
    end
    
    max_prob(n) = max([Pr1(n), Pr2(n), Pr3(n)]);
      
end

%% the maximizing probability version
%cost = -log(prod(max_prob));

%% the maximizing probability of option chosen by subject
 %cost = -log(prod(subj_choice_prob));
 cost = -sum(log((subj_choice_prob)));

%cost = sum(cost);
%cost = min(log(cost));

%cost = min(log(max_diff));


%cost=(2-r(prob1,s.choice1)-r(modelswitch,s.switch))/2;%+0.075.*abs(s.switchnum-modelswitchnum);
%cost=1-r(prob1,s.choice1);
% constr=[];

% -Plot figures----------------------------------
% figure(2);
% subplot(7,1,1); barh(c); axis([0 10 0 2]); xlabel 'exploitation';
% subplot(7,1,2); barh(AlphaWin, 'r'); axis([0 1 0 2]); xlabel 'AlphaWin';
% subplot(7,1,3); barh(AlphaLoss, 'g'); axis([0 1 0 2]); xlabel 'AlphaLOss';
% % subplot(1,5,2); plot(TMPe1,'r*'); title('ej-GoodOption');
% subplot(7,1,4); plot(smooth(Pr1)); title('prob  red: choice1'); hold on; plot(e1,'k'); plot(chose1); hold off;
% subplot(7,1,5); plot(smooth(Pr2)); title('prob  red: choice2'); hold on; plot(e2,'k'); plot(chose2,'r'); hold off;
% subplot(7,1,6); plot(smooth(Pr3)); title('prob  red: choice3'); hold on; plot(e3,'k'); plot(chose3,'g'); hold off;
% subplot(7,1,7);  barh(cost, 'k'); axis([0 800 0 2]); xlabel 'log likelihood'; hold off

%subplot(6,1,6); plot(phiwin); title('phiwin; red: philoss'); hold on; plot(philoss,'r'); hold off;

% fprintf('Gammawin=%.3f, Gammaloss=%.3f, Alpha=%.3f, c=%.3f,cost=%.3f\n',phiwin,philoss,WinAlpha,c, cost);

% subplot(6,1,6); plot(echosen); title('Echosen  red: DELTAchosen'); hold on; plot(delta,'r'); hold off;
%fprintf('w=%.3f, phi=%.3f, c=%.3f cost=%.3f\n',w,phi,c,cost);
% fprintf('phi=%.3f, cost=%.3f\n',phi,cost);
% figure(2)
% plot(probBadOption); title('prob-BadOption'); hold on; plot(1-choice(goodtrials),'r'); hold off;

% figure(2);
% subplot(1,5,1); plot(v); title('v');
% % subplot(1,5,2); plot(TMPe1,'r*'); title('ej-GoodOption');
% subplot(1,5,2); plot(probBadOption); title('prob-GoodOption'); hold on; plot(s.choice(goodtrials)-1,'r'); hold off;
% subplot(1,5,3); plot(theta); title('theta');
% subplot(1,5,4); plot(prob1); title('prob-BadOption'); hold on; plot(s.choice(goodtrials)-1,'r'); hold off;
% subplot(1,5,5); plot([w phi c]); xlabel('w phi c'); title('parameters'); axis([1 3 0 1]);
% fprintf('w=%.3f, phi=%.3f, c=%.3f cost=%.3f\n',w,phi,c,cost);
%
