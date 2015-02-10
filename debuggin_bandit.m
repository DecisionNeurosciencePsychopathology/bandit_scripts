%% Process all regs for subjects
listing=dir('c:\kod\fMRI\bandit_raw');
for i = 4:length(listing)-1 %202021 = bad subj
bandit_fmri_sub_proc_ad('id',str2double(listing(i).name))
cd ..
end

%% Finding spontaneous errors

%find trials with errors
q_error = (b.stim_ACC == 0);
spont_switch = false(size(q_error));
number_correct = sum(b.stim_ACC);
for n=find(q_error') %has to be transpose to loop
    if n > 1 %nothing before first trial
        %disp(n);
        if b.stim_switch(n) && q_error(n-1)==0
            spont_switch(n)=true;
        end
    end
end

spont_total = sum(spont_switch);

%%PEs are acting funny check them out
if b.hrf_regs.alexFeed == b.hrf_regs.alexDeltaplus_MC
disp('yes')
end 

if b.deltaminus_MC == b.deltaplus_MC
disp('yes')
end %no

if find(b.deltaminus_MC(find(b.stim_RT))) == find(b.deltaplus_MC(find(b.stim_RT)))
disp('yes')
end %yes

if find(b.deltaminus(find(b.stim_RT))) == find(b.deltaplus(find(b.stim_RT)))
disp('yes')
end %NULL

corr(b.hrf_regs.deltaminus',b.hrf_regs.deltaplus')
corr(b.hrf_regs.alexDeltaminus_MC',b.hrf_regs.alexDeltaplus_MC') %1
corr(blk.time.alexDeltaminus_MC',blk.time.alexDeltaplus_MC') %1
corr(blk.time.alexDeltaminus_MC',blk.time.alexFeed') %1
corr(blk.time.alexDeltaminus',blk.time.alexDeltaplus') %-0.02897
corr(b.deltaminus_MC',b.deltaplus_MC') %-0.41508
corr(b.deltaminus_MC(find(b.stim_RT))',b.deltaplus_MC(find(b.stim_RT))') %-0.46976


%% plotting expected value and stim chosen by user
echosen_a = zeros(size(b.echosen));
echosen_b = zeros(size(b.echosen));
echosen_c = zeros(size(b.echosen));


echosen_a(b.chosen_stim==1) = b.echosen(b.chosen_stim==1);
echosen_b(b.chosen_stim==2) = b.echosen(b.chosen_stim==2);
echosen_c(b.chosen_stim==3) = b.echosen(b.chosen_stim==3);

figure(6)
subplot(3,1,1)
plot(smooth(b.chosen_stim==1));
hold on
plot(smooth(echosen_a), 'r');
title('Smooth A vs E(A)');
subplot(3,1,2)
plot(smooth(b.chosen_stim==2));
hold on
plot(smooth(echosen_b), 'r');
title('Smooth B vs E(B)');
subplot(3,1,3)
plot(smooth(b.chosen_stim==3));
hold on
plot(smooth(echosen_c), 'r');
title('Smooth C vs E(C)');



%%
%remove nans from design file
design = bandit_fmri_load_design;
design.Arew(isnan(design.Arew))=[];
design.Brew(isnan(design.Brew))=[];
design.Crew(isnan(design.Crew))=[];

%plot reinforced variables
figure(10)
subplot(3,1,1)
plot(smooth(design.Arew,20))
axis([0 300 0 1.1])
title('Arew');
subplot(3,1,2)
plot(smooth(design.Brew,20), 'r')
axis([0 300 0 1.1])
title('Brew');
subplot(3,1,3)
plot(smooth(design.Crew,20), 'g')
axis([0 300 0 1.1])
title('Crew');


% figure(10)
% subplot(3,1,1)
% 
% plot(smooth(design.reinforced1))
% axis([0 300 0 7])
% title('reinforced A');
% subplot(3,1,2)
% 
% plot(smooth(design.reinforced2))
% axis([0 300 0 7])
% title('reinforced B');
% subplot(3,1,3)
% 
% plot(smooth(design.reinforced3))
% axis([0 300 0 7])
% title('reinforced C');



%% Read in design file and check if plots are the same
fileID = fopen('K:\studies\suicide\3ArmBandit\crdt-sched-vrbl-rich-2013-04-30.txt');
%fileID = fopen('C:\Users\wilsonj3\Desktop\crdt-sched-vrbl-rich-2013-04-30_noh.txt');
%rich_file = textscan(fileID,'%d %s %s %d %d %d %s %s %s %d %d %d %d %d %d %s');
rich_file = textscan(fileID,'%s %s %s %s %s %s %s %s %s %s %s %s %s %s %s %s');
fclose(fileID);

rich_Arew = rich_file{1,3};
rich_Brew = rich_file{1,4};
rich_Crew = rich_file{1,5};

%after quickly deleting the empty and string cells manually
rich_Arew=cell2mat(rich_Arew);
rich_Arew=str2num(rich_Arew);
rich_Brew=cell2mat(rich_Brew);
rich_Brew=str2num(rich_Brew);
rich_Crew=cell2mat(rich_Crew);
rich_Crew=str2num(rich_Crew);

figure(11)
subplot(3,1,1)
plot(smooth(rich_Arew))
hold on 
plot(smooth(design.Arew), 'r')
axis([0 300 0 1.1])
title('Arew');
subplot(3,1,2)
plot(smooth(rich_Brew))
hold on 
plot(smooth(design.Brew), 'r')
axis([0 300 0 1.1])
title('Brew');
subplot(3,1,3)
plot(smooth(rich_Crew))
hold on 
plot(smooth(design.Crew), 'r')
axis([0 300 0 1.1])
title('Crew');

%% looking at correct vs expected value
clf
plot(b.echosen(1:20));
hold on;
plot(b.delta(1:20),'r')

ACC_st1 = b.stim_ACC(find(b.chosen_stim ==1));
ACC_st2 = b.stim_ACC(find(b.chosen_stim ==2));
ACC_st3 = b.stim_ACC(find(b.chosen_stim ==3));
echosen_st1=b.echosen(find(b.chosen_stim ==1))';
echosen_st2=b.echosen(find(b.chosen_stim ==2))';
echosen_st3=b.echosen(find(b.chosen_stim ==3))';

figure(20)
subplot(3,1,1)
plot(ACC_st1)
hold on
plot(echosen_st1, 'r')
subplot(3,1,2)
plot(ACC_st2)
hold on
plot(echosen_st2, 'r')
subplot(3,1,3)
plot(ACC_st3)
hold on
plot(echosen_st3, 'r')

figure(40)
clf
plot(smooth(b.stim_ACC.*(b.chosen_stim==3)))
hold on
plot(smooth(b.chosen_stim==3), 'r')
plot(smooth(design.reinforced3.*(1/3)), 'k')


%% Plotting subjects chosen stim vs reinforcement learning schedule
echosen_a = zeros(size(b.echosen));
echosen_b = zeros(size(b.echosen));
echosen_c = zeros(size(b.echosen));

echosen_a(b.chosen_stim==1) = b.echosen(b.chosen_stim==1);
echosen_b(b.chosen_stim==2) = b.echosen(b.chosen_stim==2);
echosen_c(b.chosen_stim==3) = b.echosen(b.chosen_stim==3);

figure(17)
clf
subplot(3,1,1)
smoothie=20;
plot(smooth(b.chosen_stim==1,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Arew,smoothie), 'r--','LineWidth',2)
plot(smooth(echosen_a,smoothie), 'k','LineWidth',2);
axis([0 300 0 1.1])
title('Arew vs A chosen vs echosen A');
subplot(3,1,2)
plot(smooth(b.chosen_stim==2,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Brew,smoothie), 'r--','LineWidth',2)
plot(smooth(echosen_b,smoothie), 'k','LineWidth',2);
axis([0 300 0 1.1])
title('Brew vs B chosen vs echosen B');
subplot(3,1,3)
plot(smooth(b.chosen_stim==3,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Crew,smoothie), 'r--','LineWidth',2)
plot(smooth(echosen_c,smoothie), 'k','LineWidth',2);
axis([0 300 0 1.1])
title('Crew vs C chosen vs echosen C');
hleg1 = legend('subj choice','reinforcement', 'predicted value');
set(hleg1,'Location','Best')

%% Plot reward schedule and choices made by subject
figure(7)
clf
subplot(3,1,1)
plot(smooth(b.chosen_stim==1),'LineWidth',2);
hold on
plot(smooth(design.Arew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Arew vs A chosen');
subplot(3,1,2)
plot(smooth(b.chosen_stim==2),'LineWidth',2);
hold on
plot(smooth(design.Brew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Brew vs B chosen');
subplot(3,1,3)
plot(smooth(b.chosen_stim==3),'LineWidth',2);
hold on
plot(smooth(design.Crew), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Crew vs C chosen');
hleg1 = legend('subj choice','reinforcement', 'predicted value');
set(hleg1,'Location','Best')

%% Plot reward schedule and choices made by subject (more smooth)
figure(7)
clf
smoothie=20;
subplot(3,1,1)
plot(smooth(b.chosen_stim==1,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Arew,smoothie), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Arew vs A chosen');
subplot(3,1,2)
plot(smooth(b.chosen_stim==2,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Brew,smoothie), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Brew vs B chosen');
subplot(3,1,3)
plot(smooth(b.chosen_stim==3,smoothie),'LineWidth',2);
hold on
plot(smooth(design.Crew,smoothie), 'r--','LineWidth',2)
axis([0 300 0 1.1])
title('Crew vs C chosen');
hleg1 = legend('subj choice','reinforcement', 'predicted value');
set(hleg1,'Location','Best')

%% Save data for correlation table via SPSS
listing=dir('c:\kod\fMRI\bandit_raw');
hdr={'jan_RT', 'jan_Feedback', 'jan_2', 'jan_3', 'jan_7', 'jan_Choice500'...
    'alex_Choice', 'alex_Feedback', 'alex_2', 'alex_3', 'alex_7'};
txt=sprintf('%s\t',hdr{:});
txt(end)='';
for i = 4:length(listing)-1 %202021 = bad subj

fnam = sprintf('C:/Users/wilsonj3/Documents/SPSS_Files/bandit/bandit_corr_data%s.dat',listing(i).name);   
load(['c:\kod\fMRI\bandit_regs\bandit' listing(i).name '.mat'])
%cd('C:\Users\wilsonj3\Documents\SPSS_Files\bandit')
gdlmwrite(fnam,txt,'');
% Using dlmwirte was actually way faster...huh
dlmwrite(fnam,[
    b.hrf_regs.RT' b.hrf_regs.feedback' ... % 1 RT    2 feedback
    b.hrf_regs.action_2' ...%6 right index 
    b.hrf_regs.action_3' ...%7 right middle
    b.hrf_regs.action_7' ...%8 left index 
    b.hrf_regs.choice' ...%10 "choice" - 500ms following stimulus presentation
    b.hrf_regs.alexChoice' ...%17 Alex's older method of determining RT's
    b.hrf_regs.alexFeed' ...%18 Alex's older method of determining RT's, Feedback
    b.hrf_regs.alexAction_2' ...%19 Alex's method actions right index
    b.hrf_regs.alexAction_3' ...%20 Alex's method actions right middle
    b.hrf_regs.alexAction_7' ...%21 Alex's method actions left index
    ],'-append','delimiter','\t');
cd('c:\kod\fMRI')
end
fclose all %close all files

%% Determine if PE+ and PE- overlap
deloverlap=(b.deltaminus>0 & b.deltaplus>0);
find(deloverlap)

%% Best model SPSS correlations
listing=dir('c:\kod\fMRI\bandit_raw');
hdr={'alex_Choice', 'alex_Feedback', 'alex_2', 'alex_3', 'alex_7', 'alexDeltaplus'...
    'alexDeltaminus', 'alexEchosen'};
txt=sprintf('%s\t',hdr{:});
txt(end)='';
for i = 4:length(listing)-1 %202021 = bad subj

fnam = sprintf('C:/Users/wilsonj3/Documents/SPSS_Files/bandit/bandit_bestmodel_data%s.dat',listing(i).name);   
load(['c:\kod\fMRI\bandit_regs\bandit' listing(i).name '.mat'])
%cd('C:\Users\wilsonj3\Documents\SPSS_Files\bandit')
gdlmwrite(fnam,txt,'');
% Using dlmwirte was actually way faster...huh
dlmwrite(fnam,[
    b.hrf_regs.alexChoice' ...%17 Alex's older method of determining RT's
    b.hrf_regs.alexFeed' ...%18 Alex's older method of determining RT's, Feedback
    b.hrf_regs.alexAction_2' ...%19 Alex's method actions right index
    b.hrf_regs.alexAction_3' ...%20 Alex's method actions right middle
    b.hrf_regs.alexAction_7' ...%21 Alex's method actions left index
    b.hrf_regs.alexDeltaplus' ...%22 Alex model PE+ at feedback
    b.hrf_regs.alexDeltaminus' ...%23 Alex model PE- at feedback
    b.hrf_regs.alexEchosen' ...%24 Alex model EV of subsequently chosen stim at choice
    ],'-append','delimiter','\t');
cd('c:\kod\fMRI')
end
fclose all %close all files


%% That time when we ploted the squared differences of the expected values
figure(11)
plot(emsd1)
hold on
plot(essd1,'r')

figure(12)
plot(emsd2)
hold on
plot(essd2,'r')

figure(13)
plot(emsd3)
hold on
plot(essd3,'r')

figure(14)
subplot(2,1,1)
plot(s.prob.e1)
hold on
plot(s.prob.e2,'r')
plot(s.prob.e3,'g')
subplot(2,1,2)
plot(b.emsd)
hold on
plot(b.essd,'r')

%% Finally figured out correlation matrix 
%Probably could make this pretty nice by automating everything, make a gui
%even and pick which variables you want to see from a struct? That's
%probably overkill but it'd be nice to have.


data = [b.deltaplus' b.deltaminus' b.essd' b.emsd'];
[r,p]=corrcoef(data);

names = {'b.deltaplus' 'b.deltaminus' 'b.essd' 'b.emsd'};
CovMat{1,1} =''; %Initialize

for i = 2:length(names)+1
    CovMat{i,1} = names{i-1};
    CovMat{1,i} = names{i-1};
    for j = 2:length(names)+1
        CovMat{i,j} = r(i-1,j-1);
    end 
end