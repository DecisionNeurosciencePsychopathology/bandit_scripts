%RTT_min_Onset=b.stim_RTTime-b.stim_OnsetTime;




 bRT_round = round(b.stim_RT./100);%*100
 bRT_round_3blk = bRT_round(201:300);
% %true_RT_window = zeros(1,length(epoch_window));
% 
% true_RT_window = 0;
% 
% start_time = floor(b.stim_OnsetTime(201:end)./100)*100+60; %100ms off from epoch window?
% 
% count=1;
% for n=1:100
%     len=length(start_time(n):100:(bRT_round_3blk(n)+start_time(n)));
%     %len=RT_round3(n)/100;
% true_RT_window(count:count+len-1)=start_time(n):100:(bRT_round_3blk(n)+start_time(n));
% count=count+len;
% end
% 
% true_RT_window=true_RT_window';


% i=0;
% ct=0;

%Finds the length of the rts associated with each computed regressor
tmp_rt_len1=0;
tmp_rt_len2=0;
tmp_rt_len3=0;
%for i = 1:100
tmp_rt_len1 = get_reg_rt_length(tmp_reg.regressors1.RT,0,0);
tmp_rt_len2 = get_reg_rt_length(tmp_reg.regressors2.RT,0,0);
tmp_rt_len3 = get_reg_rt_length(tmp_reg.regressors3.RT,0,0);

tmp_rt_len = [tmp_rt_len1 tmp_rt_len2 tmp_rt_len3];
tmp_rt_len = tmp_rt_len';

% for j = 1:length(tmp_reg.regressors3.RT)
%     if tmp_reg.regressors3.RT(j)==1 && tmp_reg.regressors3.RT(j+1)==0
%         ct = ct+1;
%         i=i+1;
%         tmp_rt_len(i) = ct;
%         ct=0;
%     elseif tmp_reg.regressors3.RT(j)==1
%         ct = ct+1;
%     end
% end

% tmp_rt_len = [tmp_rt_len(1:32) 0 tmp_rt_len(33:end)];
% tmp_rt_len = tmp_rt_len';
%bRT_round_3blk = bRT_round_3blk/100;

figure(2)
clf
plot(tmp_rt_len)
hold on
plot(bRT_round_3blk,'r')

figure(3)
plot(tmp_reg.regressors3.RT)
axis([0 7000 0 1.1])




%% Double check Jan's method of acquiring RTs for regressors

    %create onset times (in scanner time)
    bl1onsets=b.stim_OnsetTime(1:100)-b.stim_OnsetTime(1);
    bl2onsets=b.stim_OnsetTime(101:200)-b.stim_OnsetTime(101)+693750;
    bl3onsets=b.stim_OnsetTime(201:end)-b.stim_OnsetTime(201)+1387500;
    blk.time.onsets=[bl1onsets' bl2onsets' bl3onsets'];
    
    %create rts (in scanner time)
    blk.time.rts=blk.time.onsets+b.stim_RT';
    
    %take RTs from trials where subject actually responded
    blk.time.goodrts=blk.time.rts(b.stim_RT'~=0);
    blk.time.goodonsets=blk.time.onsets(b.stim_RT'~=0);
    
    % make reg for deliberation time: onset to RT
    decide10hz=[];
    for ct=1:length(blk.time.goodonsets)
        % decide10hz is the index of non-zero values of the regressor
        % 1 bin = 100ms subtracted to account for the time needed to
        % perform the button press
        int=round(blk.time.goodonsets(ct)./100):round(blk.time.goodrts(ct)./100-1);
        blk.time.rt_length(ct)=length(round(blk.time.goodonsets(ct)./100):round(blk.time.goodrts(ct)./100-1));
        decide10hz=[decide10hz int];
    end
    blk.time.rt_length=blk.time.rt_length';
    decide10hz=decide10hz(2:end); %remove leading 0

%Plots comparing rt lengths from eprime file, current method, and new
%method (see above)
figure(4)
plot(bRT_round(b.stim_RT~=0))
hold on
plot( blk.time.rt_length, 'r')

figure(5)
clf
plot( blk.time.rt_length, 'r')
hold on
plot(tmp_rt_len, 'k')

figure(6)
clf
plot(bRT_round(b.stim_RT~=0))
hold on
plot(tmp_rt_len, 'k')

%See where eprime RT's = calculated RT's
find(bRT_round(b.stim_RT~=0)==blk.time.rt_length)


%% binary array analysis of regressor rt length

%current model code
current_model_rt_length = [tmp_reg.regressors1.RT tmp_reg.regressors2.RT tmp_reg.regressors3.RT];

%using the indices from above, calculate new rt length
decide10hz_rt_length = zeros(1,length(current_model_rt_length));
decide10hz_rt_length(decide10hz)=1;

blck_time = 6938;


%Plots show "ticks" of when rts occur within the block
figure(7)
plot(current_model_rt_length)
hold on
plot(decide10hz_rt_length,'r')
title('All blocks')

figure(8)
plot(current_model_rt_length(1:blck_time))
hold on
plot(decide10hz_rt_length(1:blck_time),'r')
axis([0 7000 0 1.1])
title('First block')

figure(9)
plot(current_model_rt_length(blck_time:blck_time*2))
hold on
plot(decide10hz_rt_length(blck_time:blck_time*2),'r')
axis([0 7000 0 1.1])
title('Second block')

figure(10)
plot(current_model_rt_length(blck_time*2:end))
hold on
plot(decide10hz_rt_length(blck_time*2:end),'r')
axis([0 7000 0 1.1])
title('Third block')

sum(current_model_rt_length)
sum(decide10hz_rt_length)




