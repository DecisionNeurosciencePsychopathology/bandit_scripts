function qc_bandit_decisions(months_back)
% in Aug of 2017 there was a malfunction with the right button glove so I
% wrote this function to check on the bandit behavioral responses
% Input arg:
% date_time (in months) - how vary back you want to look at subjects' data
% Outputs:
% Plots

%Set date number from today - date_time
months_back = datenum(datetime('now') - calmonths(months_back));

%Fix data location
data_dir = 'E:/Box Sync/skinner/data/eprime/bandit';

%Get list of dirs
file_paths=glob([data_dir '/*']);

%Create a list of subject who's data file was created in the specified time
%range
for i = 1:length(file_paths)
    listing=dir([file_paths{i} '*.txt']);
    %In case there are multiple take the most recent file
    [~,idx]=max([listing.datenum]);
    listing = listing(idx);
    createdate = listing.datenum;
    if months_back<createdate && createdate<today
        plot_id_list{i} = regexp(file_paths{i},'\d{5,6}','match');
    end
end

%Extract the ids
plot_id_list=plot_id_list(~cellfun('isempty',plot_id_list));
plot_id_list=[plot_id_list{:}];
plot_id_list=cellfun(@str2num,plot_id_list);

%Load in main data struc
load('C:\kod\fMRI\subjects\bandit_data.mat')

%Find indices of subjects to plot
[~,idx]=ismember(plot_id_list,ball.id);

%Remove ids that doesn't exist
idx = idx(idx>0);

%Load in the design
design = bandit_fmri_load_design;

%Filter out computer and msytery trials
design.Arew = design.Arew(~isnan(design.Arew));
design.Brew = design.Brew(~isnan(design.Brew));
design.Crew = design.Crew(~isnan(design.Crew));
filter=ball(1,1).behav.trial_filter;
design.Arew(~filter)=[];
design.Brew(~filter)=[];
design.Crew(~filter)=[];


%plot the individual's behaviors
for i=1:length(idx)
    figure(i)
    clf;
    %plot_behav(ball.behav(idx(i)).choice_numeric,ball.id(idx(i)))
    
    plot_behav_vs_design(ball.behav(idx(i)).choice_numeric,ball.id(idx(i)),design)
    
    %As an additional check pllot the historgram of button presses
%     load(sprintf('subjects/%d.mat',ball.id(idx(i))))
%     figure(1*10)
%     clf
%     histogram(b.stim_RESP(b.stim_RESP>0))
end





function plot_behav(data,id)
%A=1
%B=2
%C=3
subplot(3,1,1)
plot(smooth(double(data==1)), 'r')
title(sprintf('Subject %d choosing A',id))
subplot(3,1,2)
plot(smooth(double(data==2)), 'b')
title('Subject choosing B')
subplot(3,1,3)
title('Subject choosing C')
plot(smooth(double(data==3)), 'g')

function plot_behav_vs_design(data,id,design)
lw=2;
subplot(3,1,1)
plot(smooth(design.Arew),'k--','LineWidth',lw)
hold on
plot(smooth(double(data==1)), 'r','LineWidth',lw)
axis([0 300 0 1.1])
title(sprintf('Subject %d choosing A vs design (blk)',id))
subplot(3,1,2)
plot(smooth(design.Brew), 'k--','LineWidth',lw)
hold on
plot(smooth(double(data==2)), 'b','LineWidth',lw)
axis([0 300 0 1.1])
title('Subject choosing B vs design')
subplot(3,1,3)
plot(smooth(design.Crew), 'k--','LineWidth',lw)
hold on
plot(smooth(double(data==3)), 'g','LineWidth',lw)
axis([0 300 0 1.1])
title('Subject choosing C vs design')

