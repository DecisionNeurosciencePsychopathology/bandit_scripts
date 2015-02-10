function s=rlmodel3fmri(b)
% fname=sprintf('bandit%d',id);
% cd('C:\regs');
% b=load(fname);
% b=b.b;
% s.RT=b.showstim_RT;
s.showstim_RESP=b.chosen_stim;
s.showstim_ACC=b.stim_ACC;
s.choice = b.chosen_stim;
s.goodtrials = find(s.choice<999);
s.feed=b.stim_ACC;
s.switchnum=0;
for ct=2:length(s.choice)
    if s.choice(ct)~=s.choice(ct-1)
        s.switch(ct)=1;
        s.switchnum=s.switchnum+1;
    else
        s.switch(ct)=0;
    end
end
s.switch=s.switch';
s.switch=s.switch';
s.stay=1-s.switch;




% start off the learning
% algorithm with some initial values
save modeldata.mat s

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% call Amoeba with our data
options(1)=1;        % To display intermediate results use 1, otherwise use 0 (default)
options(2)=1e-3;     % Relative x-tolerance
options(3)=1e-3;     % Relative f-tolerance
options(14)=100;    % Max. number of f-evaluations per internal
fargs={};

%[A,optionsA]=simps('testmin',3,1,options,-1,1,fargs{:});
% intuitive versions of our calls, which we cannot use....
%[s.noprob.w,s.noprob.phi]=amoeba(rlgetexp,.4,.2);
%[s.prob.w,s.prob.phi,s.prob.c]=amoeba(rlgetexpandprob,.4,.2,.1);
%[s.caltech.w,s.caltech.phi,s.caltech.beta]=amoeba(rlgetexpandprobcaltech,.4,.2,.1);
%no probability function:
%func     init   useboth   don't mess   minvals  maxvals  don't mess
%fittedparameters=simps('rlgetexp',[.5 .5],[1 2],   [options],    [-1 -1], [1  1],  fargs{:});
%s.noprob.w=fittedparameters(1);
%s.noprob.phi=fittedparameters(2);

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% and with the probability function:

%[fittedparameters,options]=simps('rlgetexp3',[.9 0.4 9],[1 2 3],[options],[0.01 0.01 0],[1 1 10 ],fargs{:});
[fittedparameters,options]=simps('rlgetexp3',[.9 0.4 9],[1 2 3],[options],[0.3 0.3 0],[0.3 0.3 10 ],fargs{:});
alphawin1=fittedparameters(1); alphaloss1=fittedparameters(2); %LossAlpha1=fittedparameters(4);
c1=fittedparameters(3);
[cost1, constr,e11,e21,e31,prob11,prob21,prob31,delta1,echosen1,etotal1]=rlgetexp3(fittedparameters);


%fittedparameters=simps('rlgetexp3',[.1 0.01 9],[1 2 3],[options],[0.01 0.01 0],[1 1 10],  fargs{:});
fittedparameters=simps('rlgetexp3',[.1 0.01 9],[1 2 3],[options],[0.3 0.3 0],[0.3 0.3 10],  fargs{:});
alphawin2=fittedparameters(1); alphaloss2=fittedparameters(2); %LossAlpha2=fittedparameters(4);
c2=fittedparameters(3);
[cost2,constr, e12,e22,e32, prob12,prob22,prob32, delta2, echosen2, etotal2]=rlgetexp3(fittedparameters);
%

%fittedparameters=simps('rlgetexp3',[.5 0.5 2],[1 2 3],[options],[0.01 0.01 0],[1 1 10],  fargs{:});
fittedparameters=simps('rlgetexp3',[.5 0.5 2],[1 2 3],[options],[0.3 0.3 0],[0.3 0.3 10],  fargs{:});
alphawin3=fittedparameters(1); alphaloss3=fittedparameters(2); %LossAlpha2=fittedparameters(4);
c3=fittedparameters(3);
[cost3, constr,e13,e23,e33, prob13,prob23,prob33, delta3, echosen3, etotal3]=rlgetexp3(fittedparameters);


% [fittedparameters,options]=simps('rlgetexp3',[0.3 .05 0.06],[1 2 3],[options],[0.01 0.01 0.01],[0.5 0.5 0.5],fargs{:});
% gamma1=fittedparameters(1); alphawin1=fittedparameters(2); WinAlpha1=fittedparameters(3); %LossAlpha1=fittedparameters(4);
% [cost1,constr1, e11,e21,e31,prob11,prob21,prob31,delta1,echosen1]=rlgetexp3(fittedparameters);
%
%
% fittedparameters=simps('rlgetexp3',[0.03 .4 0.01],[1 2 3],[options],[0.01 0.01 0.01],[0.5 0.5 0.5],  fargs{:});
% gamma2=fittedparameters(1); alphawin2=fittedparameters(2); WinAlpha2=fittedparameters(3); %LossAlpha2=fittedparameters(4);
% [cost2,constr2, e12,e22,e32, prob12,prob22,prob32, delta2, echosen2]=rlgetexp3(fittedparameters);
%



% [fittedparameters,options]=simps('rlgetexp3',[0.3 .05 1 10],[1 2 3 4],   [options],    [0 0 0.5 0], [0.5 0.5 1 10],  fargs{:});
% gamma1=fittedparameters(1); alphawin1=fittedparameters(2); alpha1=fittedparameters(3);  c1=fittedparameters(4);
% [cost1,constr1, e11,e21,e31,prob11,prob21,prob31,delta1, echosen1]=rlgetexp3(fittedparameters);
% fittedparameters=simps('rlgetexp3',[0.3 .4 1 3],[1 2 3 4],   [options],    [0 0 0.5 0], [0.5 0.5 1 10],  fargs{:});
% gamma2=fittedparameters(1); alphawin2=fittedparameters(2); alpha2=fittedparameters(3);  c2=fittedparameters(4);
% [cost2,constr2, e12,e22,e32, prob12,prob22,prob32, delta2, echosen2]=rlgetexp3(fittedparameters);

% emsd1=e_max_squared_diff(e11,e21,e31);
% essd1=e_sum_square_ddiff(e11,e21,e31);
% emsd2=e_max_squared_diff(e12,e22,e32);
% essd2=e_sum_square_ddiff(e12,e22,e32);
% emsd3=e_max_squared_diff(e13,e23,e33);
% essd3=e_sum_square_ddiff(e13,e23,e33);

if min([cost1 cost2 cost3])==cost1
    %   s.prob.gamma=gamma1;
    s.prob.alphawin=alphawin1;
    s.prob.alphaloss=alphaloss1;
    %   s.prob.WinAlpha=WinAlpha1;
    %  s.prob.LossAlpha=LossAlpha1;
    s.prob.c=c1;
    s.prob.cost=cost1;
    s.prob.e1=e11;
    s.prob.e2=e21;
    s.prob.e3=e31;
    s.prob.prob1=prob11;
    s.prob.prob2=prob21;
    s.prob.prob3=prob31;
    s.prob.delta=delta1;
    s.prob.echosen=echosen1;
    s.prob.etotal=etotal1;
    s.prob.emsd=e_max_squared_diff(e11,e21,e31);
    s.prob.essd=e_sum_square_ddiff(e11,e21,e31);
elseif min([cost1 cost2 cost3])==cost2
    %   s.prob.gamma=gamma2;
    s.prob.alphawin=alphawin2;
    s.prob.alphaloss=alphaloss2;
    %   s.prob.WinAlpha=WinAlpha2;
    % s.prob.LossAlpha=LossAlpha2;
    s.prob.c=c2;
    s.prob.cost=cost2;
    s.prob.e1=e12;
    s.prob.e2=e22;
    s.prob.e3=e32;
    s.prob.prob1=prob12;
    s.prob.prob2=prob22;
    s.prob.prob3=prob32;
    s.prob.delta=delta2;
    s.prob.echosen=echosen2;
    s.prob.etotal=etotal2;
    s.prob.emsd=e_max_squared_diff(e12,e22,e32);
    s.prob.essd=e_sum_square_ddiff(e12,e22,e32);
elseif min([cost1 cost2 cost3])==cost3
    %   s.prob.gamma=gamma2;
    s.prob.alphawin=alphawin3;
    s.prob.alphaloss=alphaloss3;
    %   s.prob.WinAlpha=WinAlpha2;
    % s.prob.LossAlpha=LossAlpha2;
    s.prob.c=c3;
    s.prob.cost=cost3;
    s.prob.e1=e13;
    s.prob.e2=e23;
    s.prob.e3=e33;
    s.prob.prob1=prob13;
    s.prob.prob2=prob23;
    s.prob.prob3=prob33;
    s.prob.delta=delta3;
    s.prob.echosen=echosen3;
    s.prob.etotal=etotal3;
    s.prob.emsd=e_max_squared_diff(e13,e23,e33);
    s.prob.essd=e_sum_square_ddiff(e13,e23,e33);
end

%s.prob.c=fittedparameters(3);
%fprintf('w=%.3f, phi=%.3f, c=%.3f\n',s.prob.w,s.prob.phi,s.prob.c);
%fprintf('gamma=%.3f, alphawin=%3f, alpha=%3f, c=%3f, cost=%3f\n',s.prob.gamma, s.prob.alphawin, s.prob.alpha, s.prob.c, s.prob.cost);

%fprintf('gamma=%.3f, alphawin=%3f, WinAlpha=%3f,cost=%3f\n',s.prob.gamma, s.prob.alphawin, s.prob.WinAlpha, s.prob.cost);
fprintf('alphawin=%3f, alphaloss=%3f,c=%3f,cost=%3f\n', s.prob.alphawin, s.prob.alphaloss,s.prob.c, s.prob.cost);
% save(sprintf('rl%d',id),'-struct', 's');

%
% %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% % basic idea in curve fitting
% % is to have a function which returns
% % a goodness given some parameters

%Find the squared difference between the max trial value and the trial mean
function foo = e_max_squared_diff(e1, e2, e3)
args = [e1; e2; e3;];
mean_es = mean(args,1);
%[C,I]=max(args(:,i),[],1)
for i = 1:length(e1)
    %mean_es(i) = (e1(i)+ e2(i)+e3(i))/3;
    %mean_es(i) = (args(1,i)+args(2,i)+args(3,i))/3;
    idx(i) = find(args(:,i)==max(args(:,i)),1, 'first');
    foo(i)=(args(idx(i),i)-mean_es(i)).^2;
end
return

%Find the sum of the squared differences between each trial value and the trial mean
function foo = e_sum_square_ddiff(e1, e2, e3)
args = [e1; e2; e3;];
mean_es = mean(args,1);
foo = sum((args-repmat(mean_es,3,1)).^2);
return



