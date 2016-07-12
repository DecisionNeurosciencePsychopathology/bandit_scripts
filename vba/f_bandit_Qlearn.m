function  [fx] = f_bandit_Qlearn(x,theta,u,in)
% evolution function of q-values of a RL agent (3-armed bandit problem)
% [fx,dfdx,dfdP] = f_Qlearn2(x,P,u,in)
% Here, there are three q-values to evolve, i.e. there are three
% actions to reinforce (3-armed bandit problem).
% IN:
%   - x_t : q-values (3x1)
%   - P : (inverse-sigmoid) learning-rate-win and learning rate-loss
%   - u : u(1)=previous action (1 or 0), u(2)=feedback
%   - in : [useless]
% OUT:
%   - fx: evolved q-values (2x1)
%   - dfdx/dfdP: gradient of the q-values evolution function, wrt q-avlues
%   and evolution parameter, respectively.

%Reward
r = u(2); % when subjects chooses correctly reward is 1 unless we use the reward vector, which then the input is rew magnitude

if ~in.decay %This logic is somewhat confusing...
    alpha_win = 1./(1+exp(-theta(1))); % learning rate is bounded between 0 and 1.
    alpha_loss = 1./(1+exp(-theta(2))); % learning rate is bounded between 0 and 1.
    decay=0.5;
elseif in.valence
    %Params
    alpha_win = 1./(1+exp(-theta(1))); % learning rate is bounded between 0 and 1.
    alpha_loss = 1./(1+exp(-theta(2))); % learning rate is bounded between 0 and 1.
    decay = 1./(1+exp(-theta(3))); % decay is bounded between 0 and 1.
    % loss_decay = 1./(1+exp(-theta(3))); % learning rate is bounded between 0 and 1.
    % win_decay = 1./(1+exp(-theta(4))); % learning rate is bounded between 0 and 1.
else
    alpha_win = 1./(1+exp(-theta(1))); % learning rate is bounded between 0 and 1.
    alpha_loss = alpha_win;
    decay = 1./(1+exp(-theta(2))); % decay is bounded between 0 and 1.
end

%Additional steepness to parameterize reward magnitude
%Make sure r is not binary!
if in.utility
    r = r^1./(1+exp(-theta(end)));
end

fx = zeros(length(x),1);

%Determine what value gets the biggest update
choice = u(1);

if isnan(choice)
    fx = x;
    return
end

choices = [1;2;3];
update_index = choices==choice;
delta = r-x(choice); % prediction error

%Update
if u(2)>0
    fx(update_index) = x(update_index) + alpha_win*(delta);
    fx(~update_index) = decay*x(~update_index);
else
    fx(update_index) = x(update_index) + alpha_loss*(delta);
    fx(~update_index) = decay*x(~update_index);
end

fx(4) = delta;
    
% gradients' derivation
% if u(1)==1
%     dfdx = [1-alpha, 0;
%             0, 1];
%     dfdP = [alpha*(1-alpha)*pe(1),0];
% else
%     dfdx = [1, 0;
%             0, 1-alpha];
%     dfdP = [0,alpha*(1-alpha)*pe(2)];
% end


% dfdx = [1-alpha, 0;
%             0, 0];
% dfdP = [alpha*(1-alpha)*delta,0];

%% Previous code from rlgetexmp3 -just as a reference delete later
% if s.feed(ct)>0
%         if s.choice(ct)==1
%             e1(ct+1)=e1(ct)+ AlphaWin.*(r(ct)-e1(ct));
%             e2(ct+1)=LossDecay.*e2(ct);%-(e1(ct)-e1(ct-1))/2;
%             e3(ct+1)=LossDecay.*e3(ct);%-(e1(ct)-e1(ct-1))/2;
%             delta(ct)=r(ct)-e1(ct);
%         elseif s.choice(ct)==2
%             e2(ct+1)=e2(ct)+ AlphaWin.*(r(ct)-e2(ct));
%             e1(ct+1)=LossDecay.*e1(ct);%-(e2(ct)-e2(ct))/2;
%             e3(ct+1)=LossDecay.*e3(ct);%-(e2(ct)-e2(ct))/2;
%             delta(ct)=r(ct)-e2(ct);
%         elseif s.choice(ct)==3
%             e3(ct+1)=e3(ct)+ AlphaWin.*(r(ct)-e3(ct));
%             e1(ct+1)=LossDecay.*e1(ct);%-(e3(ct)-e3(ct))/2;
%             e2(ct+1)=LossDecay.*e2(ct);%-(e3(ct)-e3(ct))/2;
%             delta(ct)=r(ct)-e3(ct);
%         elseif s.choice(ct)==999 %meaning a no-response trial
%             e1(ct+1)=e1(ct);
%             e2(ct+1)=e2(ct);
%             e3(ct+1)=e3(ct);
%             delta(ct)=0;
%         end
%         %% On punished trials, apply the learning rate for punishments
%         
%     elseif s.feed(ct)==0
%         if s.choice(ct)==1
%             e1(ct+1)=e1(ct)+ AlphaLoss.*(r(ct)-e1(ct));
%             e2(ct+1)=WinDecay.*e2(ct);%+(e1(ct)-e1(ct))/2;
%             e3(ct+1)=WinDecay.*e3(ct);%+(e1(ct)-e1(ct))/2;
%             delta(ct)=r(ct)-e1(ct);
%         elseif s.choice(ct)==2
%             e2(ct+1)=e2(ct)+ AlphaLoss.*(r(ct)-e2(ct));
%             e1(ct+1)=WinDecay.*e1(ct);%+(e2(ct)-e2(ct))/2;
%             e3(ct+1)=WinDecay.*e3(ct);%+(e2(ct)-e2(ct))/2;
%             delta(ct)=r(ct)-e2(ct);
%         elseif s.choice(ct)==3
%             e3(ct+1)=e3(ct)+ AlphaLoss.*(r(ct)-e3(ct));
%             e1(ct+1)=WinDecay.*e1(ct);%+(e3(ct)-e3(ct))/2;
%             e2(ct+1)=WinDecay.*e2(ct);%+(e3(ct)-e3(ct-2))/2;
%             delta(ct)=r(ct)-e3(ct);
%         elseif s.choice(ct)==999 %meaning a no-response trial
%             e1(ct+1)=e1(ct);
%             e2(ct+1)=e2(ct);
%             e3(ct+1)=e3(ct);
%             delta(ct)=0;
%         end
%     end
