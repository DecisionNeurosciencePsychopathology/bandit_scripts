function plot_bandit_vba_data(id_list,varagin)
%This function will plot data returned from the vba script, you can either
%plot all subjects, (specfific groups?), to get an overview of behaviour or
%compare the overall mean value of vba variables against behavioral
%variables.

%If no list is given just use everyone
if nargin<1
    load('C:/kod\fMRI/idNumbers');
    id_list = idNumbers;
end

%Load in previous data if available, otherwise create a new dataset

%Close all the opening figs
close all;

vars_to_plot = {'value', 'reward_stake'};
data_to_plot=compile_data_for_plotting(id_list,vars_to_plot);

%Messy plot probably but shows overall distibution on an individual level
for j = 1:length(vars_to_plot)
    figure(j)
    plot(1:length(data_to_plot.(vars_to_plot{j})),data_to_plot.(vars_to_plot{j}),'o',...
        'MarkerFaceColor',[1 1 1])
    title(vars_to_plot{j})
end

stop=1;


function T=compile_data_for_plotting(id_list,vars_to_plot)
%Complie the dependant variable 'x' into a table format for easy plotting
T = table;
for i = 1:length(id_list)
    id = id_list(i);
    id_file_list = glob(sprintf('E:/data/bandit/individual_results/*%d*1_rewVec*',id));
    load(id_file_list{:}) %Load in the data i.e. out, posterior, b
    for j = 1:length(vars_to_plot)
        tmp_data(i,:,j) = out.suffStat.(vars_to_plot{j});
    end
    id_row_names(i,1) = {num2str(id)};
end


for j = 1:length(vars_to_plot)
    T.(vars_to_plot{j}) = tmp_data(:,:,j);
end

T.Properties.RowNames = id_row_names;