% send some bandit data to a SPSS importable format file
%
% currently this only outputs error data from the bandit task
%
% Jan Kalkus
% 26 Dec 2012

load('data/bandit_data.mat');
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.3.mat']);
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.5.mat']);
%load([pathroot 'analysis/bandit/data/PersevExp/bandit_data-ps_thr_0.8.mat']);

% open file pointer and print out headers
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss.dat'],'w');
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss_p0.3.dat'],'w');
%fid = fopen([pathroot 'analysis/bandit/data/bandit2spss_p0.5.dat'],'w');
fid = fopen('data/bandit2spss_fMRIData.dat','w');

fprintf(fid,'ID\tprob_switch_err_fMRI\tspont_switch_err_fMRI\tpersev_err_fMRI\tpercent_corr_fMRI\t');
fprintf(fid,'before_prob_sw_fMRI\tbefore_spont_sw_fMRI\tbefore_persev_fMRI\tbefore_percent_corr_fMRI\t');
fprintf(fid,'after_prob_sw_fMRI\tafter_spont_sw_fMRI\tafter_persev_fMRI\tafter_percent_corr_fMRI\t');
fprintf(fid,'sampling_index_fMRI\t');
fprintf(fid,'exploratory_switch_err_fMRI\t');
fprintf(fid,'erratic_spont_fMRI\tdelta_index_fMRI\t');
fprintf(fid,'diffAgivena1x\tdiffAgivena23x\tdiffAgivena4plusx\t');
fprintf(fid,'diffAgiven_n_5\tdiffAgiven_n_4\tdiffAgiven_n_3\tdiffAgiven_n_2\n');


%Add in credit assignment vars after running credit assignment.m.
%Might be a solid idea to just attach cred assignment script at end of
%other behav script?

% percentage correct
f_percent_correct = @(p) 100*sum(ball.behav(p).bestchoice)/numel(ball.behav(p).bestchoice);

for nsubj = 1:numel(ball.id)
    
    % errors as usual
    pse  = sum(ball.behav(nsubj).errors.prob);
    sse  = sum(ball.behav(nsubj).errors.spont);
    per  = sum(ball.behav(nsubj).errors.perseverative);
    pcor = f_percent_correct(nsubj);
    
    % exploratory switch errors
    expl = nansum(ball.behav(nsubj).errors.explore_sw);
    erratic_spont = nansum(ball.behav(nsubj).errors.erratic_spont);
    
    % delta index
    delta_index = ball.behav(nsubj).delta_index;
    
    % split half errors (before and after reversal)
    before_pse = sum(ball.behav(nsubj).errors.before.prob_switch_err);
    before_sse = sum(ball.behav(nsubj).errors.before.spont_switch_err);
    before_per = sum(ball.behav(nsubj).errors.before.perseverative);
    
    after_pse  = sum(ball.behav(nsubj).errors.after.prob_switch_err);
    after_sse  = sum(ball.behav(nsubj).errors.after.spont_switch_err);
    after_per  = sum(ball.behav(nsubj).errors.after.perseverative);
    
    % percent correct
    before_pcor = 100*sum(ball.behav(nsubj).bestchoice(1:150)/numel(ball.behav(nsubj).bestchoice(1:150)));
    after_pcor  = 100*sum(ball.behav(nsubj).bestchoice(151:end)/numel(ball.behav(nsubj).bestchoice(151:end)));
    
    % backward-spread
    diffAgivena1x = ball.behav(nsubj).back_output.diffAgivena1x;
    diffAgivena23x = ball.behav(nsubj).back_output.diffAgivena23x;
    diffAgivena4plusx = ball.behav(nsubj).back_output.diffAgivena4plusx;
    
    % forward-spread
    diffAgiven_n_5 = ball.behav(nsubj).for_output.diffAgiven_n_5;
    diffAgiven_n_4 = ball.behav(nsubj).for_output.diffAgiven_n_4;
    diffAgiven_n_3 = ball.behav(nsubj).for_output.diffAgiven_n_3;
    diffAgiven_n_2 = ball.behav(nsubj).for_output.diffAgiven_n_2;
    
    
    % write to file
    fprintf(fid,'%d\t%d\t%d\t%d\t%g\t',ball.id(nsubj),pse,sse,per,pcor);
    fprintf(fid,'%d\t%d\t%d\t%g\t',before_pse,before_sse,before_per,before_pcor);
    fprintf(fid,'%d\t%d\t%d\t%g\t',after_pse,after_sse,after_per,after_pcor);
    fprintf(fid,'%d\t',ball.behav(nsubj).count_to_first_C);
    fprintf(fid,'%d\t',expl);
    fprintf(fid,'%d\t%g\t',erratic_spont,delta_index);
    fprintf(fid,'%g\t%g\t%g\t',diffAgivena1x,diffAgivena23x,diffAgivena4plusx);
    fprintf(fid,'%g\t%g\t%g\t%g\n',diffAgiven_n_5,diffAgiven_n_4,diffAgiven_n_3,diffAgiven_n_2);
    
end

% kill the pointer
fclose(fid);
