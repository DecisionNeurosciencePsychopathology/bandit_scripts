function out=pull_all_bandit_data_from_table
load('idNumbers.mat')
bis_table = readtable('BIS/Splash_BIS.xlsx');
idx = ismember(bis_table.IDLISTUnique_withOthersAndControls__ID,idNumbers);
out = bis_table(idx,:);
out.Properties.VariableNames{'IDLISTUnique_withOthersAndControls__ID'}='ID';

%remove any duplicates based on scan date??
%first get the duplicates -- if any
dup_idx=~[any(diff(out.ID,1),2); 1];
dup_ids = out.ID(dup_idx);

%Load in the demographics data
load('bandit_demos.mat')

for i = 1:length(dup_ids)
    d_idx=find(ismember(out.ID,dup_ids(i))); %Grab individual duplicate index
    b_idx=ismember(bandit_demos.ID,dup_ids(i)); %Grab individual demogs index
    scan_date = datenum(bandit_demos.ScanDate(b_idx)); %Grab the dates
    admin_date = datenum(out.CDate(d_idx));%Grab the duplicates dates and compare
    [~,closer_to_scan_date]=min(abs(scan_date-admin_date));
    d_idx(closer_to_scan_date)=[]; %Remove the closer date from list
    out(d_idx,:)=[]; %Remove all suplicate data from main structure
end