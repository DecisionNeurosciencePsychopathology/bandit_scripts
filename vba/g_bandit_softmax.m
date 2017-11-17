function  [ gx ] = g_bandit_softmax(x,P,u,in )
% INPUT
% - x : Q-values (2x1)
% - P : inverse temperature (1x1)
% - u : [useless]
% - in : [useless]
% OUTPUT
% - gx : P(a=1|x)

beta = exp(P);

x = x(1:3);

p_choice = (exp((x-max(x))/beta)) / (sum(exp((x-max(x))/beta))); %Divide by temperature
gx = p_choice;

% dgdx = zeros(size(x,1),1);
% dgdx(1) = beta*gx*(1-gx);
% dgdx(2) = -beta*gx*(1-gx);
% dgdP = [beta*dQ*gx*(1-gx)];

