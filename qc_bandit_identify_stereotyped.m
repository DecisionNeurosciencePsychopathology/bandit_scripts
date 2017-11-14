% quality-check behavior

nsubs = length(ball.behav);

figure(1); clf;
for s = 1:nsubs
subplot(ceil(sqrt((nsubs))),ceil(sqrt((nsubs))),s)
plot(ball.behav(s).chosen_position);
xlabel((ball.id(s)));
% y needs to be 1 3 for behavioral dataset and 1 7 for scanner
axis([1 300 1 3]);
        fprintf('processing id: %6d\t\n%',ball.id(s));
end
% histograms are less informative 
% figure(2); clf;
% for s = 1:nsubs
% subplot(11,11,s)
% hist(ball.behav(s).chosen_position(ball.behav(s).chosen_position>0));
% xlabel((ball.id(s)));
% end
