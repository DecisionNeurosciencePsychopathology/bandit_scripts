function  [ gx ] = g_bandit_wsls(x,P,u,in )
% INPUT
% - x : Q-values (2x1)
% - P : inverse temperature (1x1)
% - u : [useless]
% - in : [useless]
% OUTPUT
% - gx : P(a=1|x)

r = u(2);
choice = u(1);
p_choice = zeros(1,3);
if r==1
    p_choice(choice) = 1;
else
    p_choice(choice) = 0;
    p_choice(~choice) = .5;
end
gx = p_choice;

% dgdx = zeros(size(x,1),1);
% dgdx(1) = beta*gx*(1-gx);
% dgdx(2) = -beta*gx*(1-gx);
% dgdP = [beta*dQ*gx*(1-gx)];

