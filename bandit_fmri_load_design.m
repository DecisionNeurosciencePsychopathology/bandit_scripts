% function to load and organize output of the scanner/fmri
% version of the bandit task
%
%   Usage:
%
%       >> % load design file data into data structure
%       >> data_struct = bandit_fmri_load_design;
%
%       >> % plot probability curves
%       >> data_struct.plotBanditProbs(w,data_struct);
%
%       >> % get help on plotting probability curves
%       >> data_struct.plotBanditHelp
%
%
% Jan Kalkus
% 2013-04-02
%
% Jan Kalkus
% 2014-04-01: added text fields in data structure 
%             for user friendliness. 


function c_out = bandit_fmri_load_design

% import raw data
data_struct = importFromTextFile;

% convert from cell to struct
b_struct = storeInStructure(data_struct);

% trim {top,left,right}stim values to one character
c_out = fixStimValues(b_struct);

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function dout = importFromTextFile

% create file pointer, main file
file_name = 'CORRECT-crdt-sched-vrbl-rich-2015-01-23.txt';

%create file pointer practive file
%file_name = ['K:\studies\suicide\3ArmBandit\crdt-sched-PRACTICE.txt'];

fid = fopen(file_name,'r');

% set format strings
data_format   = '%*f %*s %s %f %f %f %s %s %s %f %f %f %f %f %f %*s';
header_format = '%*s %*s %s %s %s %s %s %s %s %s %s %s %s %s %s %*s';

% read in experiment and header data
C = textscan(fid,data_format,'Delimiter','\t','HeaderLines',1); frewind(fid);
H = textscan(fid,header_format,1,'Delimiter','\t'); 
fclose(fid); % close file pointer

% output
dout.data   = C;
dout.fields = H;

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function struct_out = storeInStructure(data_and_header_struct)

field_names = cellfun(@(x) x(:),data_and_header_struct.fields)';

for q_fieldname = 1:numel(field_names)
    struct_out.(field_names{q_fieldname}) = data_and_header_struct.data{q_fieldname};
end

return


% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
function struct_to_fix = fixStimValues(struct_to_fix)

stim_fields = {'topstim','leftstim','rightstim'};

for n = 1:numel(stim_fields)
    
    % find indices that are not empty
    q = ~cellfun(@isempty,struct_to_fix.(stim_fields{n}));
    
    % data to replace current entries
    tmp = cellfun(@(s) s(1), struct_to_fix.(stim_fields{n})(q),'UniformOutput',false);
    
    % execute/store
    struct_to_fix.(stim_fields{n})(q) = tmp;

end

% add some handy function handles
struct_to_fix.rmNan = @(x) x(~isnan(x));
struct_to_fix.plotBanditProbs = @(w,s) plot(filtfilt(ones(1,w)/w,1, ...
    [s.rmNan(s.Arew),s.rmNan(s.Brew),s.rmNan(s.Crew)]));
% and some text explaining one of them
struct_to_fix.plotBanditHelp = sprintf(['usage: plotBanditProbs(w,s) \n\n'...
    '\t w \tis the width in trials of the averaging window\n'...
    '\t s \tis the data structure\n\n' ...
    '\te.g., \n\t\t>> b_struc = bandit_fmri_load_design;\n'...
    '\t\t>> b_struc.plotBanditProbs(10,b_struc);\n']);

return

