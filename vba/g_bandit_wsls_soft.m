function  [ gx ] = g_bandit_wsls_soft(x,P,u,in )
% INPUT
% - x : Q-values (2x1)
% - P : inverse temperature (1x1)
% - u : [useless]
% - in : [useless]
% OUTPUT
% - gx : P(a=1|x)
beta = exp(P(1));
r = u(2);
choice = u(1);
p_choice = zeros(1,3);
if choice == 0 || isnan(choice)
    p_choice = 1/3*ones(1,3);
else
if r==1
    p_choice(choice) = 1;
else
    p_choice(choice) = 0;
    p_choice(~choice) = .5;
end
end
p_choice = (exp((p_choice -max(p_choice ))/beta)) / (sum(exp((p_choice -max(p_choice ))/beta))); %Divide by temperature

gx = p_choice;

% dgdx = zeros(size(x,1),1);
% dgdx(1) = beta*gx*(1-gx);
% dgdx(2) = -beta*gx*(1-gx);
% dgdP = [beta*dQ*gx*(1-gx)];

