function q = bandit_fMRIproc( varargin )

% processes 3-armed bandit data on all subjects
data_dir = 'data/'; % set data path

% create list of subjects defined by directory names
numlist = num_scan(dir([data_dir]));


%Out msg
fprintf('Starting behavorial processing....\n');

% % run single-subject proc script on each
for sub=1:length(numlist)
        fprintf('processing id: %6d\t\t',numlist(sub));
        
        % load subject's data
        %s = bandit_fMRI_sub_proc(numlist(sub), varargin{:} );
        s=load([data_dir num2str(numlist(sub)) '.mat']);
        s=s.out; %Hard-coded  but meh...
        
        % print some general error counts info
        fprintf('error counts: PS = %3d, SS = %3d, PE = %3d\n', ...
            sum(s.errors.prob_switch_err), ...
            sum(s.errors.spont_switch_err), ...
            sum(s.errors.perseverative) ...
            );
        
        
        ball.id(sub,1) = numlist(sub);
        % the [bellow/deleted] is redundant; we can get the same result using
        % the stored function handle and subject by subject data:
        %
        %     x = ball.fx.choice_to_stimID([ball.behav.choice]);
        %
        % this output is not in a logical/binary format as
        % ball.[a-c]choice were, but it is easity converted to such
        
        ball.behav(sub).choice     = s.stim_choice;
        ball.behav(sub).choice_numeric = s.stim_choice_numeric;
        if(eq(sub,1)) % only need to do this once
            % function handle converts ball.behav.choice from 'char' to 'int'
            ball.fx.choice_to_stimID = @(c) cast(c,'double')-64;
        end
        ball.behav(sub).bestchoice           = s.best_choice;
        ball.behav(sub).errors.spont         = s.errors.spont_switch_err;
        ball.behav(sub).errors.erratic_spont = s.errors.erratic_spont;
        ball.behav(sub).errors.prob          = s.errors.prob_switch_err;
        ball.behav(sub).errors.perseverative = s.errors.perseverative;
        ball.behav(sub).errors.explore_sw    = s.errors.explore_switch;
        
        % save when subject responded correctly
        ball.behav(sub).stim_ACC = s.stim_ACC;
        
        % errors split into before and after reversal
        ball.behav(sub).errors.before = s.errors.before;
        ball.behav(sub).errors.after  = s.errors.after;
        
        % counts to first stim C choice after reversal
        ball.behav(sub).count_to_first_C = s.counts_to_first_C;
        
        % technical specs
        ball.behav(sub).RT              = s.stim_RT;
        %ball.behav(sub).chosen_position = s.chosen_position;
        
        % additional measures
        ball.behav(sub).above_chance_diff = s.above_chance_diff;
        ball.behav(sub).delta_index       = s.delta_index;
        
        % TODO: add routine for 'last_updated' vs. 'last_checked'
        ball.last_updated = datestr(now,'yyyy-mm-dd HH:MM:SS');
        
        % enter descriptive field info
        % -- not done yet --
        
        %Run credit assignment
        ball = credit_assignment(ball);
        
        % save it
        save([data_dir 'bandit_data'],'ball');
        
        % varargout
        if(nargout), q = ball; end
        
        
end

function num_out = num_scan(data_in)

num_out = zeros(length(data_in),1);

for n = 1:length(data_in) %index_array %3:(length(A))-2
    %num_out(n) = str2double(data_in(n).name);
    num_out(n) = str2double(data_in(n).name(1:end-4));
end

q_nan = isnan(num_out);
num_out = num_out(~q_nan);

return