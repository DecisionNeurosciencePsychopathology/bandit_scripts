function vba_df=create_vba_mfx_input()

fMRI_flag=0; %1 for fMRI sample 0 for behavioral

%Initialize the tbale
vba_df = table();

%Use glob to pull all the vba files
if fMRI_flag
    vba_files = glob('vba_output/*.mat');
else
    vba_files = glob('E:/data/bandit/bandit_behav_vba_output/*.mat');
end


for vba_file = vba_files'
    load(vba_file{:}) %Load in the file should contain b,out,posterior
    stop=1;
    
    %Initialize temporay dataframe
    tmp_table = table();
    
    %Grab id
    tmp_table.ID = b.id;
    
    %Grab y & u
    tmp_table.y = {out.y};
    tmp_table.u = {out.u};
    
    %Grab the options used or perhaps create a sub function to create this
    tmp_table.options = {out.options};
    
    vba_df = [vba_df; tmp_table];
    
end