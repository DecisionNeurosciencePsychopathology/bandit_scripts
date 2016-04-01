%Plotting subjects chosen stim vs reinforcement learning schedule.
%This script will generate behavioral plots for subjects that completed
%the bandit task in the fMRI scanner. The plots will show the subjects
%chosen stimulus, the reward schedule, and the expected value from the
%model.

%Load in the subject ids from the regression folder
dirs=dir('bandit_regs');


%If by some chance these "bad" ids get involved
filter = [202021; 210381];

%load in the design file
design  = bandit_fmri_load_design;

for i = 3:2:length(dirs) %By 2 because of duplicate id numbers
    
    
    %Filter out any bad ids that may have squeaked by
    if ismember(str2num(dirs(i).name),filter);
        continue
    end
    
    
    %load in data
    load([pwd,'\bandit_regs\',dirs(i,1).name])
    
    %Initialize
    echosen_a = zeros(size(b.echosen));
    echosen_b = zeros(size(b.echosen));
    echosen_c = zeros(size(b.echosen));
    echosen_a(b.chosen_stim==1) = b.echosen(b.chosen_stim==1);
    echosen_b(b.chosen_stim==2) = b.echosen(b.chosen_stim==2);
    echosen_c(b.chosen_stim==3) = b.echosen(b.chosen_stim==3);
    
    %grab name
    name=dirs(i,1).name(7:12);
    
    %Begin plotting
    h=figure(i);
    subplot(3,1,1)
    plot(smooth(b.chosen_stim==1),'LineWidth',2);
    hold on
    plot(smooth(design.Arew), 'r--','LineWidth',2)
    plot(smooth(echosen_a), 'k','LineWidth',2);
    axis([0 300 0 1.1])
    title(['Arew vs A chosen vs echosen A ', name]);
    subplot(3,1,2)
    plot(smooth(b.chosen_stim==2),'LineWidth',2);
    hold on
    plot(smooth(design.Brew), 'r--','LineWidth',2)
    plot(smooth(echosen_b), 'k','LineWidth',2);
    axis([0 300 0 1.1])
    title('Brew vs B chosen vs echosen B');
    subplot(3,1,3)
    plot(smooth(b.chosen_stim==3),'LineWidth',2);
    hold on
    plot(smooth(design.Crew), 'r--','LineWidth',2)
    plot(smooth(echosen_c), 'k','LineWidth',2);
    axis([0 300 0 1.1])
    title('Crew vs C chosen vs echosen C');
    hleg1 = legend('subj choice','reinforcement', 'predicted value');
    set(hleg1,'Location','Best')
    
    %Save files as tiff and as fig as needed.
    %saveas(h,[pwd,'\figs\',name,'decision_vs_reinforcement'],'tif')
    saveas(h,[pwd,'\figs\matlabFigs\',name,'decision_vs_reinforcement'],'fig')
end