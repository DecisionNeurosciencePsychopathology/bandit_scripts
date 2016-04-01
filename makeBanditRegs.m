dirs=dir('bandit_raw');
%If you want to check the names
%dirs(3:end).name

%filter these cases as they have either bad data or something else went
%wrong...
filter = [202021; 210381];

for i = 3:length(dirs)
    % make this a function in which you can overwrite everything, or check
    % typically you only make the regs for subject not yet processed.
    
    %Until I think of a more elegent fix
    if ismember(str2num(dirs(i).name),filter);
        continue
    end
    [b,tmp_reg]=bandit_fmri_sub_proc_ad('id',str2num(dirs(i).name));
    cd ..
end

%make a text file of all the current ids that have reg datra
listofids=dir('bandit_regs');
for i=3:2:length(listofids)
    idNumbers(i-2) = str2num(listofids(i).name(7:12));
end
idNumbers = nonzeros(unique(idNumbers)');
save idNumbers idNumbers