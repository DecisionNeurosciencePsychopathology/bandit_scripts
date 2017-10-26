%Create the 3dRegana input file
load fMRI_ids.mat %loads as ids

%Set up cell
threeDregAnaCmd = table;


for i = 1:length(ids)
    threeDregAnaCmd{i,1} = {'-xydata'}; %Have first row be '-xydata'
    
    %Get group coding
    switch ids(i,2)
        case 1
            coding = {'0' '0' '0'};
        case 2
            coding = {'1' '0' '0'};
        case 4
            coding = {'1' '1' '0'};
        case 5
            coding = {'1' '1' '1'};
    end
    
    threeDregAnaCmd(i,2:4) = coding;
    
    file_str = {sprintf('/Volumes/bek/learn/ssanalysis/bandit/vb_models/value_models/chosen_value_PEchosen/banditDecFeedValueChosenPEChosen_%d+tlrc"[8]" \\',ids(i,1))};
    
    threeDregAnaCmd{i,5} = file_str;
end

writetable(threeDregAnaCmd,'bandit_3dRegAnaCmd.dat','Delimiter','\t');