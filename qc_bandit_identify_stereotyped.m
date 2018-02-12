% quality-check behavior

beh_sample = 1;

if beh_sample
    load('~/Box Sync/skinner/projects_analyses/Project Bandit/behav_sample_struct/bandit_data.mat')
else
    load('~/Box Sync/skinner/projects_analyses/Project Bandit/imaging_sample_struct/bandit_data.mat')
end


nsubs = length(ball.behav);

figure(1); clf;
for s = 1:nsubs
subplot(ceil(sqrt((nsubs))),ceil(sqrt((nsubs))),s)
plot(ball.behav(s).chosen_position);
xlabel((ball.id(s)));
% y needs to be 1 3 for behavioral dataset and 1 7 for scanner
if beh_sample
axis([1 300 1 3]);
else
axis([1 300 1 7]);
end
        fprintf('processing id: %6d\t\n%',ball.id(s));
end
% histograms are less informative 
% figure(2); clf;
% for s = 1:nsubs
% subplot(11,11,s)
% hist(ball.behav(s).chosen_position(ball.behav(s).chosen_position>0));
% xlabel((ball.id(s)));
% end
