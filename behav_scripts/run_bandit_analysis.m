function q = run_bandit_analysis (varagin)
%Author: Jonathan Wilson
%Date created: 6/30/15
%Last Modified 7/1/15
%This script is to analyze the 3-armed bandit task data we run on subjects
%collected via the tablets.
%
%NOTE: Double check on credit schedule this one is pulled from the the L
%drive, and it's pretty old.

%This was just while I was debugging the code
clc
close all

%Load  in data
try
    load('c:\kod\Neuropsych_preproc\matlab\analysis\bandit\data\bandit_data.mat')
catch
    disp('Can''t find bandit data, please locate the bandit data file')
    [FileName,PathName,FilterIndex] = ...
        uigetfile('*.mat','Choose a file');
    load([PathName FileName]);
end

%load in group numbers to main struct
ball = find_demog_by_id(ball);

%Run model to get EV and other metrics
ball = bandit_rl_model(ball,1,0);

ball = credit_assignment(ball); %takes about 22 seconds to run demog + rlmodel + credit assignment

%Decide which group to investigate 1 = 1-5 2 = 1-7
group_type = 2;
group_num = ball.group_17;
groups = [1 2 4 5 6 7];

if group_type ==1
    %     group_num = ball.group_15;
    %     groups = unique(ball.group_15);
    groups_of_interest = {'CON'; 'DEP'; 'IDE'; 'ATT';};
else
    %I need to hcange this part around either decide to hard code the group
    %numbers like so, or interchange groups of interest later on in the
    %script by another if statement.
    %     group_num = ball.group_17;
    %     %groups = unique(ball.group_17);
    %     groups = [1 2 4 5 6 7];
    groups_of_interest = {'CON'; 'DEP'; 'IDE'; 'LOWLETH'; 'HIGHLETH'};
end

%Add tags to group numbers
%groups = unique(ball.group_15);
group_names = {'CON'; 'DEP'; 'IDE'; 'ATT'; 'LOWLETH'; 'HIGHLETH'};

%Calc overall percent correct for entire trial, pre-reversal, and
%post-reversal, same for win-switch errors
trial_length = length(ball.behav(1,1).stim_ACC);
for i = 1:length(ball.id)
    percent_correct(i,1) = sum(ball.behav(1,i).stim_ACC)/trial_length;
    percent_correct_prereversal(i,1) = sum(ball.behav(1,i).stim_ACC(1:trial_length/2))/(trial_length/2);
    percent_correct_postreversal(i,1) = sum(ball.behav(1,i).stim_ACC((trial_length/2)+1:trial_length))/(trial_length/2);
    win_switch_error(i,1) = sum(ball.behav(1,i).errors.spont)/trial_length;
    win_switch_error_prereversal(i,1) = sum(ball.behav(1,i).errors.before.spont_switch_err)/trial_length;
    win_switch_error_postreversal(i,1) = sum(ball.behav(1,i).errors.after.spont_switch_err)/trial_length;
    %Add this to the errors sub group later--possibly
    lose_switch_error(i,1) = sum(ball.behav(1,i).lose_switch);
    
    %Credit assignment - Backward spread
    Ax1B(i,1) = ball.behav(1,i).back_output.diffAgivena1x;
    Ax23B(i,1) = ball.behav(1,i).back_output.diffAgivena23x;
    Ax47B(i,1) = ball.behav(1,i).back_output.diffAgivena4plusx;
    
    %Credit assignment - Forward spread
    ARAAAB(i,1) = ball.behav(1,i).for_output.diffAgiven_n_5;
    AARAAB(i,1) = ball.behav(1,i).for_output.diffAgiven_n_4;
    AAARAB(i,1) = ball.behav(1,i).for_output.diffAgiven_n_3;
    AAAARB(i,1) = ball.behav(1,i).for_output.diffAgiven_n_2;
    
    %Correct choices based on stimulus probability
    correct_choice(i,1) = ball.behav(1,i).correct_choice;
    correct_choice_prereversal(i,1) = ball.behav(1,i).correct_choice_prereversal;
    correct_choice_postreversal(i,1) = ball.behav(1,i).correct_choice_postreversal;
    
    
end

%Temporary data matrix
bandit_stats = [ball.id group_num percent_correct percent_correct_prereversal ...
    percent_correct_postreversal win_switch_error ...
    win_switch_error_prereversal win_switch_error_postreversal lose_switch_error...
    Ax1B Ax23B Ax47B ARAAAB...
    AARAAB AAARAB AAAARB correct_choice correct_choice_prereversal correct_choice_postreversal];

%Names of all variables in the bandit stats matrix
bandit_stats_names = {'id' 'group number' 'percent_correct' 'percent_correct_prereversal'...
    'precent_correct_postreversal' 'win_switch_error' 'win_switch_error_prereversal'...
    'win_switch_error_postreversal' 'lose_switch_error' 'Ax1B' 'Ax23B' 'Ax47B'...
    'ARAAAB' 'AARAAB' 'AAARAB' 'AAAARB' 'correct_choice' 'correct_choice_prereversal'...
    'correct_choice_postreversal'};

%Print out some data...think you could just use fprintf?
str=sprintf('\nOverall percent correct is: %.2f\n', mean(percent_correct)*100);
disp(str)
str = sprintf('Overall percent by group is..\n');
disp(str)

%Plot lose switches Overall for all subjects
figure(88)
clf
hist(bandit_stats(:,9))
title('Lose switches for all subjects')


%Load design file
design_struct = bandit_tablet_load_design;

%Create group struct
group_struct = struct;

%Main data calculating loop
%Order will be Controls, Dep, Ideators, Att, LL, HL descending down [MEAN STD]
for i = 1:length(group_names)
    if i == 4 %hacked it
        idx = (bandit_stats(:,2)>=5);
    else
        idx = groups(i)==bandit_stats(:,2);
    end
    num_in_group = sum(idx);
    
    %For loop to set individual group data in struct as well as calculate
    %mean and std of said variable in question (i.e percent_correct, 
    %lose_switch_error, ect almost all strings in bandit stats names.
    
    for j = 3:length(bandit_stats_names)
        %group_struct.(group_names{i}).metrics.([bandit_stats_names{j}]) = bandit_stats(idx,j);
        %group_struct.(group_names{i}).metrics.([bandit_stats_names{j} '_mean']) = mean(bandit_stats(idx,j));
        %group_struct.(group_names{i}).metrics.([bandit_stats_names{j} '_std']) = std(bandit_stats(idx,j));
        
        %How to handle Nans?
        temp = bandit_stats(idx,j);
        group_struct.(group_names{i}).metrics.(['individual_' bandit_stats_names{j}]) = temp;
        group_struct.(group_names{i}).metrics.([bandit_stats_names{j} '_mean'])=mean(temp(~isnan(temp)));
        group_struct.(group_names{i}).metrics.([bandit_stats_names{j} '_std'])=std(temp(~isnan(temp)));
    end
    
    
    %Print out some statistics
    str=sprintf('%s with mean %.2f%% correct with std %.2f',group_names{i},...
        group_struct.(group_names{i}).metrics.percent_correct_mean*100, ...
        group_struct.(group_names{i}).metrics.percent_correct_std);
    disp(str)
    
    %Overall contingency (All choices)
    group_struct.(group_names{i}).overall_average_choice=...
        round((sum([ball.behav(1,idx).stim_ACC],2))./num_in_group);
    
    %Individual choices (seperate A, B, C) A=1 B=2 C=3
    temp_ACC = [ball.behav(1,idx).stim_ACC];
    temp_choiceA = ([ball.behav(1,idx).choice_numeric]==1);
    temp_choiceB = ([ball.behav(1,idx).choice_numeric]==2);
    temp_choiceC = ([ball.behav(1,idx).choice_numeric]==3);
    
    %NOTE clairfy with Alex that this 'averag_choice' is when stimACC=1 and
    %when subjects chose either A B or C respectively, not just when the
    %subjects chose a stimulus per trial on average.
    
    group_struct.(group_names{i}).average_choiceA = ...
        round((sum(temp_choiceA,2))./num_in_group);
    group_struct.(group_names{i}).average_choiceB = ...
        round((sum(temp_choiceB,2))./num_in_group);
    group_struct.(group_names{i}).average_choiceC = ...
        round((sum(temp_choiceC,2))./num_in_group);
    
    %Learning curves
    plot_learning_curves(design_struct,group_struct,group_names,i)
    
    %Credit assignment
    plot_credit_assignment(group_struct,group_names,i)
    
    %Histograms for lose switches
    figure(42)
    subplot(2,3,i)
    hist(bandit_stats(idx,9))
    title(['Lose switches per group ' (group_names{i})])
    
    %Plot histograms for credit assignments
    credit_index=find(~cellfun('isempty', strfind(bandit_stats_names,'A')));
    for k = 1:length(credit_index)
        cred_idx = credit_index(k);
        figure(i*1000)
        subplot(3,3,k)
        hist(bandit_stats(idx,cred_idx))
        title(['Credit assignment hist for ' (group_names{i}) ' ' bandit_stats_names{cred_idx}])
    end
end

%It may be helpful to store the means in a substruct called means then call
%field names to grab them
metrics_fieldnames = fieldnames(group_struct.CON.metrics);

%Use regex to grab individual metrics/errors
for i = 1:length(metrics_fieldnames)
    metrics_names(i,1) = regexp(metrics_fieldnames(i),'individual*\w*', 'match');
end
%Remove empty cells
metrics_names = metrics_names(~cellfun(@isempty, metrics_names));


for j = 1:length(metrics_names)
    %Fill anova table with Nans
    anova_data = nan(max(ball.demos),length(groups_of_interest));
    
    
    for i = 1:length(groups_of_interest)
        %Crate anova table of mean percent ocrrect for each group
        anova_data(1:(length(group_struct.(groups_of_interest{i}).metrics.(metrics_names{j}{:}))),i) ...
            = group_struct.(groups_of_interest{i}).metrics.(metrics_names{j}{:})(:);
    end
    
    %Calculate the anova statistic on the mean percent correct
    [p(j), t{j}, st]=anova1(anova_data,groups_of_interest);
    
    %Run post-hoc Tukey
    [c,m,h] = multcompare(st, 'ctype', 'hsd');
    
    %Display ANOVA order
    str=sprintf('ANOVA %d, %s', j,metrics_names{j}{:});
    disp(str)
    
    %Check is anything is significant
    if p(j)<=.05
        str = sprintf('\nSignificant! p=%f Check ANOVA results for: %s',p(j),metrics_names{j}{:});
        disp(str)
    end
end

q = ball;
return


%Plot the learning curves
function plot_learning_curves(design_struct, choice_struct, group_names, fig_num)
figure(fig_num)
i = fig_num;
clf
subplot(3,1,1)
smoothie=20;
plot(smooth(design_struct.Arew,smoothie), 'r--','LineWidth',2)
hold on
plot(smooth(choice_struct.(group_names{i}).average_choiceA,smoothie),'LineWidth',2)
axis([0 300 0 1.1])
title(['Arew ' (group_names{i})]);
subplot(3,1,2)
plot(smooth(design_struct.Brew,smoothie), 'r--','LineWidth',2)
hold on
plot(smooth(choice_struct.(group_names{i}).average_choiceB,smoothie),'LineWidth',2)
axis([0 300 0 1.1])
title('Brew');
subplot(3,1,3)
plot(smooth(design_struct.Crew,smoothie), 'r--','LineWidth',2)
hold on
plot(smooth(choice_struct.(group_names{i}).average_choiceC,smoothie),'LineWidth',2)
axis([0 300 0 1.1])
title('Crew');

function plot_credit_assignment(group_data,group_names, index)
i = index;
figure(i*100)
clf;
labels1={'Ax1B' 'Ax23B' 'Ax4plusB'};
labels2={'A?AAAB' 'AA?AAB' 'AAA?AB'};
%labels2={'A?AAAB' 'AA?AAB' 'AAA?AB' 'AAAA?B'};
subplot(2,1,1)
boxplot([group_data.(group_names{i}).metrics.individual_Ax1B,...
    group_data.(group_names{i}).metrics.individual_Ax23B,...
    group_data.(group_names{i}).metrics.individual_Ax47B],'labels',labels1)
title(['Credit Assignment for: ' (group_names{i})]);
subplot(2,1,2)
boxplot([group_data.(group_names{i}).metrics.individual_ARAAAB,...
    group_data.(group_names{i}).metrics.individual_AARAAB,...
    group_data.(group_names{i}).metrics.individual_AAARAB,...
],'labels',labels2)
% boxplot([group_data.(group_names{i}).metrics.individual_ARAAAB,...
%     group_data.(group_names{i}).metrics.individual_AARAAB,...
%     group_data.(group_names{i}).metrics.individual_AAARAB,...
%     group_data.(group_names{i}).metrics.individual_AAAARB],'labels',labels2)



%% Old code

%     %Individual means and values
%     group_struct.(group_names{i}).metrics.individual_means = bandit_stats(idx,3);
%     group_struct.(group_names{i}).metrics.individual_means_pre = bandit_stats(idx,4);
%     group_struct.(group_names{i}).metrics.individual_means_post = bandit_stats(idx,5);
%     group_struct.(group_names{i}).metrics.individual_lose_switch_means = bandit_stats(idx,9);
%     
%     %Credit assignment
%     group_struct.(group_names{i}).metrics.individual_diffAgivena1x = bandit_stats(idx,10);
%     group_struct.(group_names{i}).metrics.individual_diffAgivena23x = bandit_stats(idx,11);
%     group_struct.(group_names{i}).metrics.individual_diffAgivena4plusx = bandit_stats(idx,12);
%     group_struct.(group_names{i}).metrics.individual_diffAgiven_n_5 = bandit_stats(idx,13);
%     group_struct.(group_names{i}).metrics.individual_diffAgiven_n_4 = bandit_stats(idx,14);
%     group_struct.(group_names{i}).metrics.individual_diffAgiven_n_3 = bandit_stats(idx,15);
%     group_struct.(group_names{i}).metrics.individual_diffAgiven_n_2 = bandit_stats(idx,15);
%     
%     %Overall means by group
%     group_struct.(group_names{i}).metrics.percent_correct_mean = mean(bandit_stats(idx,3));
%     group_struct.(group_names{i}).metrics.percent_correct_std = std(bandit_stats(idx,3));
%     group_struct.(group_names{i}).metrics.percent_correct_mean_pre = mean(bandit_stats(idx,4));
%     group_struct.(group_names{i}).metrics.percent_correct_std_pre = std(bandit_stats(idx,4));
%     group_struct.(group_names{i}).metrics.percent_correct_mean_post = mean(bandit_stats(idx,5));
%     group_struct.(group_names{i}).metrics.percent_correct_std_post = std(bandit_stats(idx,5));
%     
%     %Individual Errors
%     group_struct.(group_names{i}).metrics.individual_win_switch_error = bandit_stats(idx,6);
%     group_struct.(group_names{i}).metrics.individual_win_switch_error_pre = bandit_stats(idx,7);
%     group_struct.(group_names{i}).metrics.individual_win_switch_error_post = bandit_stats(idx,8);
%     
%     %Overall Errors by group
%     group_struct.(group_names{i}).metrics.win_switch_mean = mean(bandit_stats(idx,6));
%     group_struct.(group_names{i}).metrics.win_switch_std = std(bandit_stats(idx,6));
%     group_struct.(group_names{i}).metrics.win_switch_mean_pre = mean(bandit_stats(idx,7));
%     group_struct.(group_names{i}).metrics.win_switch_pre_std = std(bandit_stats(idx,7));
%     group_struct.(group_names{i}).metrics.win_switch_mean_post = mean(bandit_stats(idx,8));
%     group_struct.(group_names{i}).metrics.win_switch_post_std = std(bandit_stats(idx,8));
%     
%     %Lose switches by group
%     group_struct.(group_names{i}).metrics.lose_switch_mean = mean((bandit_stats(idx,9)));




