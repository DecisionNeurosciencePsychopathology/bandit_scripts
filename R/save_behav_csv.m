%define dir and get subject data file names
data_dir = '~/code/bandit_scripts/data/scanner/subjects/'; % set data path
all_dir=dir(data_dir);
subj_mats_pre=all_dir([all_dir.isdir]==0);
subj_mats=subj_mats_pre(2:end-3);
if exist([data_dir 'csvs/'],'dir')==0
    mkdir([data_dir 'csvs/'])
    if exist([data_dir 'csvs/reshaped/'],'dir')==0
        mkdir([data_dir 'csvs/reshaped/'])
    end
end



%loop through subjs & read in data
for subj=1:size(subj_mats,1)
    s_pre=load([data_dir subj_mats(subj).name]);
    s=s_pre.out;
    
    %preallocate output file for subj
    data_post=zeros(270,33);
    %fields to pull out: subj ID, errors- prob switch, errors- spont switch,
    % errors- erratic_spont, errors- explore switch, error- NOS, 
    % errors- perseverative, before errors- prob switch, before errors- spont switch,
    % before errors- erratic_spont, before errors- explore switch, before error- NOS, 
    % before errors- perseverative, after errors- prob switch, after errors- spont switch,
    % after errors- erratic_spont, after errors- explore switch, after error- NOS, 
    % after errors- perseverative, RT, ACC, a choice, b choice, c choice, 
    % stim choice, numeric stim choice, prob a, prob b, prob c, 
    % best choice, above chance diff, delta index, counts to first C
    % note that subj ID & last two have one value per subj rather than per
    % trial so used for all trials
    data_post(:,1)=repmat(str2double(subj_mats(subj).name(1:end-4)),270,1);
    data_post(:,2)=s.errors.prob_switch_err;
    data_post(:,3)=s.errors.spont_switch_err;
    data_post(:,4)=s.errors.erratic_spont;
    data_post(:,5)=s.errors.explore_switch;
    data_post(:,6)=s.errors.error_NOS;
    data_post(:,7)=s.errors.perseverative;
    data_post(1:150,8)=s.errors.before.prob_switch_err;
    data_post(1:150,9)=s.errors.before.spont_switch_err;
    data_post(1:150,10)=s.errors.before.erratic_spont;
    data_post(1:150,11)=s.errors.before.explore_switch;
    data_post(1:150,12)=s.errors.before.error_NOS;
    data_post(1:150,13)=s.errors.before.perseverative;
    data_post(1:120,14)=s.errors.after.prob_switch_err;
    data_post(1:120,15)=s.errors.after.spont_switch_err;
    data_post(1:120,16)=s.errors.after.erratic_spont;
    data_post(1:120,17)=s.errors.after.explore_switch;
    data_post(1:120,18)=s.errors.after.error_NOS;
    data_post(1:120,19)=s.errors.after.perseverative;
    data_post(:,20)=s.stim_RT;
    data_post(:,21)=s.stim_ACC;
    data_post(:,22)=s.achoice;
    data_post(:,23)=s.bchoice;
    data_post(:,24)=s.cchoice;
    data_post(:,25)=s.stim_choice;
    data_post(:,26)=s.stim_choice_numeric;
    data_post(:,27)=s.prob(:,1);
    data_post(:,28)=s.prob(:,2);
    data_post(:,29)=s.prob(:,3);
    data_post(:,30)=s.best_choice;
    data_post(:,31)=s.above_chance_diff;
    data_post(:,32)=repmat(s.delta_index,270,1);
    if isempty(s.counts_to_first_C)==1
        data_post(:,33)=NaN;
    else
        data_post(:,33)=repmat(s.counts_to_first_C,270,1);
    end
    
    csvwrite([data_dir 'csvs/reshaped/' subj_mats(subj).name(1:end-4) '_reshaped.csv'],data_post)
    
end