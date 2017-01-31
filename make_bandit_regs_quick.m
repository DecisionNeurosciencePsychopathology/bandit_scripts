%Just run the make regressor script

load('C:/kod\fMRI/idNumbers');
id_list = idNumbers;
fail_ids=[];
for i = 1:length(id_list)
    try
    id = id_list(i);
    id_file_list = glob(sprintf('E:/data/bandit/individual_results/*%d*1_rewVec*',id));
    load(id_file_list{:}) %Load in the data i.e. out, posterior, b
    b = banditmakeregressor_vba(b,out); %Write the regressors to file
    
    all_bandit_data.(['id_' num2str(id)]).b=b;
    all_bandit_data.(['id_' num2str(id)]).out=out;
    all_bandit_data.(['id_' num2str(id)]).post=posterior;
    
%     val_corr(i) = corr(out.suffStat.stake',out.suffStat.value');
%     valChosen_corr(i) = corr(out.suffStat.stake',out.suffStat.value_chosen');
%     valChosenDiff_corr(i) = corr(out.suffStat.stake',out.suffStat.value_chosen_diff');
%     valDiff_corr(i) = corr(out.suffStat.stake',out.suffStat.value_diff');
    catch
        fprintf('FAILED %d \n\n',b.id);
        fail_ids = [fail_ids; b.id];
        continue
    end
end


save all_bandit_data all_bandit_data

% figure(1)
% hist(val_corr)
% title('Val')
% 
% figure(2)
% hist(valChosen_corr)
% title('Chosen')
% 
% figure(3)
% hist(valChosenDiff_corr)
% title('Chosen Diff ')
% 
% 
% figure(4)
% hist(valDiff_corr)
% title('Chosen Diff')