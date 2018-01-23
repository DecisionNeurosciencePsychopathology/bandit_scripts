%Okay so bandit_vba_read_in_data runs the sub proc function but the data
%isn't saved until later in the pipeline under b in vba_output for now lets
%just create a seperate folder in e: that contains all bandit behave data.

load('idNumbers.mat')
design_struct=  bandit_fmri_load_design;
for i = 1:length(idNumbers)
    try
        b = bandit_vba_read_in_data( 'id',idNumbers(i),'data_dir','subjects');
        save(sprintf('E:/data/bandit/bandit_scan_data/%d.mat',idNumbers(i)),'b');
    catch
        warning('subproc probably failed...continuing')
        continue
    end
end