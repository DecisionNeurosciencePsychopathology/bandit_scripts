%Calculate the credit assignment for Bandit tablet data, based on the work
%by Walton 2010.
%Author: Alex Domborvski & Jonathan Wilson
%
%Questions to answer:
%What is the influence of past_choices*current_reward on current choice?
%What is the influence of past_choices*past_reward on current choice?
%Assesses "spread of effect":
%current reward reinforcing earlier and future behaviors
%
%Input is s where "s" is the bandit data struct, i.e. output from
%bandit_proc.m

function s=credit_assignment(s)
%Function will return a struct containing all the credit assignment
%properties

%Verbose will show what the subjects influence of past choices to the user
verbose=0;

%Stimulus A B and C correspond to 1 ,2, 3. Max idx is the maximum
%repeapted choice in a row we try to find.
choice_a = 1;
choice_b = 2;
choice_c = 3;
max_idx = 7;

for i = 1:length(s.id) %subject loop
    %% grab choices made by subj and construct patterned choices (a1x, a2x...)
    choice=s.behav(1,i).choice_numeric';
%     shift=10;
%     a=zeros(shift,size(choice,2)+shift);
%     for ct=1:shift
%         a(ct,ct:ct+size(choice,2)-1)=choice;
%     end    
    
    % find A*B sequences; for now, will only go up to [A*7 then B];
    s=find_past_choices(s,choice,1,choice_a,'a',i);
    s=find_past_choices(s,choice,1,choice_b,'b',i);
    s=find_past_choices(s,choice,1,choice_c,'c',i);
    
    % shorter sequences should not include members of longer sequences
    s = sort_choices(s,'a',max_idx,i);
    s = sort_choices(s,'b',max_idx,i);
    s = sort_choices(s,'c',max_idx,i);
    
    % find rewarded and punished trials
    rew=find(s.behav(1,i).stim_ACC==1);
    pun=find(s.behav(1,i).stim_ACC==0);
    
    %% NOTE FOR JON: THIS REFERS TO FIGURE 6A
    % find sequences where terminal B option was reinforced/punished and
    % see where the terminal option B was followed by a subsequent option B
    s = find_reinforced_and_next_decision(s,rew,pun,i,choice);
    
    %% BACKWARD SPREAD: find differential impact of reward/punishment on subsequent choice by
    %% choice history
    % Calculate the backwards spread according to Walton: "Difference in likelihood
    % of choosing option A on trial n after previously selecting option B on trial n-1
    % as a function of whether or not reward was received for this choice.”
    [s, backward_out] = calculate_choice_history_and_backspread(s,i);
    
    
    
    %% FORWARD SPREAD: find differentibl impact of previous reward/punishment on choice by
    % find sequences where previous A choice was reinforced/punished
    % Set up the variables and calculate forawrd spread
    % Walton: "Difference in likelihood of choosing option B on trial n after previously
    % selecting option A on trials n-2 to n-5 and option B on the previous trial (n-1),
    % as a function of whether a particular previous A choice (A?) was or was not rewarded"
    
    [s, forward_out] = calculate_forwardspread(s,i,rew,pun,choice);
    
    if verbose==1
        fprintf('Results fo subject %d :\n', s.id(i));
        disp(backward_out)
        disp(forward_out)
        input('Press ENTER to continue...');
    end
    
    s.behav(1,i).back_output = backward_out;
    s.behav(1,i).for_output = forward_out;
    
end %end subject loop

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%----------------------------->END OF SCRIPT<-----------------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


function s = find_past_choices(s,choice,j,option,var1,i)
%Find where a certain stimulus is chosen for n-1 times in a row then when
%the subject chosen stimulus x on trial n
indices = zeros(1,length(choice));

% Recursively loop through the choice vector to grab repitive stimuli n-x
% times, followed by a different chosen stimuli at n-1
for t = 1:length(choice)-j
    if find(sum(choice(1,t:(t+j-1))==option)==j & (choice(1,t+j)~=option) & (choice(1,t:(t+j-1))>0)) %The >0 may not be needed now
        indices(1,t) = t+j;
    end
end
str = {[var1 num2str(j) 'x']};
%values are saved to struct s.behav.credit
s.behav(1,i).credit.(str{1}) = indices(indices>0);
j = j+1;
if j<=7
    s=find_past_choices(s,choice,j,option,var1,i); %Yay recursion!
end

function s = sort_choices(s,var1,max_idx,i)
%Sort the data to remove the more rare strings from the shorter sequences
%and build the repeated vectors i.e. a23x, a56x a4plusx
for j = 1:max_idx-1
    str1 = {[var1 num2str(j) 'x']};
    str2 = {[var1 num2str(j+1) 'x']};
    
    s.behav(1,i).credit.(str1{1})=setdiff( s.behav(1,i).credit.(str1{1}), s.behav(1,i).credit.(str2{1}));
    if j==3 || j==5
        
        str1 = {[var1 num2str(j-1) 'x']};
        str2 = {[var1 num2str(j) 'x']};
        str3 = {[var1 num2str(j-1) num2str(j) 'x']};
        %Return values that are in str1 that are not in str2 (Ex. ax1, ax2)
        s.behav(1,i).credit.(str3{1}) = union( s.behav(1,i).credit.(str1{1}),...
            s.behav(1,i).credit.(str2{1}));
    elseif j==6
        str3 = {[var1 num2str(j) num2str(j+1) 'plusx']};
        
        s.behav(1,i).credit.(str3{1}) = union( s.behav(1,i).credit.(str1{1}),...
            s.behav(1,i).credit.(str2{1}));
        
        str4 = {[var1 num2str(4) 'plusx']};
        str1 = {[var1 num2str(45) 'x']};
        
        s.behav(1,i).credit.(str4{1}) = union( s.behav(1,i).credit.(str1{1}),...
            s.behav(1,i).credit.(str3{1}));
    end
end

function s = find_reinforced_and_next_decision(s,rew,pun,i,choice)
%Grab all the variable names in credit and find where they were reinforced
%at (Ex. s.a1xrew=intersect(s.a1x,rew))

%Grab names of sequences (ax1, ax2...)
credit_var = fieldnames(s.behav(1,i).credit);

%Find what the subjects next decision was after the option switch by 
%iteratively looping to find where the n-1 stimulus X is followed by another X 
%on the next(n) trial. (i,e. a1xB = abb)
for j = 1:length(credit_var)
    %Rewards -- Reinforced
    s.behav(1,i).credit.reinforce.([credit_var{j} 'rew']) = ...
        intersect(s.behav(1,i).credit.(credit_var{j}),rew);
    %Punishment -- Not Reinforced
    s.behav(1,i).credit.reinforce.([credit_var{j} 'pun']) = ...
        intersect(s.behav(1,i).credit.(credit_var{j}),pun);
    
    %Grab leading character of var variable
    leading_char = credit_var{j}(1);
    
    switch leading_char
        case 'a'
            temp_1 = (find(choice==1)-1);
            temp_2 = (find(choice==2));
            temp_3 = (find(choice==3));
        case 'b'
            temp_1 = (find(choice==1));
            temp_2 = (find(choice==2)-1);
            temp_3 = (find(choice==3));
        case 'c'
            temp_1 = (find(choice==1));
            temp_2 = (find(choice==2));
            temp_3 = (find(choice==3)-1);
    end
    
    %The index returned is the position of the switched option, 
    %for example 39=A 40=C 41=A, ax1A = 40 ...ect.
    
    %NOTE as the code stands I bleieve it's incorrect because I'm basically
    %saying okay I chose A then chose X, now on the next trial did I chose
    %A, X again, or Y. I need to make sure in the a1xB or a1xC the 'x' is
    %either B or C respectively for the correct analysis I think....
    
    s.behav(1,i).credit.next_choice.([credit_var{j} 'A'])=...
        intersect(intersect(s.behav(1,i).credit.(credit_var{j}), (find(choice==1)-1)),temp_1);
    
    s.behav(1,i).credit.next_choice.([credit_var{j} 'B'])=...
        intersect(intersect(s.behav(1,i).credit.(credit_var{j}), (find(choice==2)-1)),temp_2);
    
    s.behav(1,i).credit.next_choice.([credit_var{j} 'C'])=...
        intersect(intersect(s.behav(1,i).credit.(credit_var{j}), (find(choice==3)-1)),temp_3);
    
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------->BACKWARD SPREAD<-----------------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [s,out] = calculate_choice_history_and_backspread(s,i)
%This function will calculate a subjects "backward spread" or the subject's
%affintity to switch back to a previously chosen stimuli as a function of
%recieving a reward or not.

next_choice_names = fieldnames(s.behav(1,i).credit.next_choice);
reinforce_names = fieldnames(s.behav(1,i).credit.reinforce);

%Grab the variables needed to find difference in likelihood
part1 = {'a' 'b' 'c'};
part2 = {'1x' '23x' '4plusx'};
part3 = {'rew' 'pun'};
part4 = {'A' 'B' 'C'};
for o = 1:length(part1)
    for j = 1:length(part2)
        rew_str = [part1{o} part2{j} part3{1}]; %reward
        pun_str = [part1{o} part2{j} part3{2}]; %punish
        choice_str = [part1{o} part2{j} part4{o}]; %choice
        reinforcemnt_vars{j*2-1,o}=reinforce_names(ismember(reinforce_names,rew_str));
        reinforcemnt_vars{j*2,o}=reinforce_names(ismember(reinforce_names,pun_str));
        choice_vars{j,o} = next_choice_names(ismember(next_choice_names,choice_str));
    end
end


%Find where subject chose option A (either once or repeatedly), chose
%option B, and then returned to option A as a function of being rewarded or
%not on trial n-1 (i.e. the trial the subject chose option B)

for k = 1:length(choice_vars)
    %Name tags for structs
    rew_tag = reinforcemnt_vars{k*2-1}{:};
    pun_tag = reinforcemnt_vars{k*2}{:};
    
    %Rewards
    rew_vectA = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2-1,1}{:});
    rew_vectB = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2-1,2}{:});
    rew_vectC = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2-1,3}{:});
    
    %Punishments
    pun_vectA = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2,1}{:});
    pun_vectB = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2,2}{:});
    pun_vectC = s.behav(1,i).credit.reinforce.(reinforcemnt_vars{k*2,3}{:});
    
    %Choices
    choiceAXA = s.behav(1,i).credit.next_choice.(choice_vars{k,1}{:});
    choiceBXB = s.behav(1,i).credit.next_choice.(choice_vars{k,2}{:});
    choiceCXC = s.behav(1,i).credit.next_choice.(choice_vars{k,3}{:});
    
    %Write to the new struct credit.backspread
    if isempty(rew_vectA) && isempty(rew_vectB) && isempty(rew_vectC)
        s.behav(1,i).credit.backspread.(['probOptionAgiven' rew_tag])=NaN;
    else
        s.behav(1,i).credit.backspread.(['probOptionAgiven' rew_tag])=...
            length(union(union((intersect(choiceAXA,rew_vectA)),...
            (intersect(choiceBXB,rew_vectB))), (intersect(choiceCXC,rew_vectC))))...
            ./(length(rew_vectA)+length(rew_vectB)+length(rew_vectC));
    end
    
    if isempty( pun_vectA) && isempty( pun_vectB) && isempty( pun_vectC)
        s.behav(1,i).credit.backspread.(['probOptionAgiven' pun_tag])=NaN;
    else
        s.behav(1,i).credit.backspread.(['probOptionAgiven' pun_tag])=...
            length(union(union((intersect(choiceAXA,pun_vectA)),(intersect(choiceBXB,pun_vectB))),...
            (intersect(choiceCXC,pun_vectC))))./(length(pun_vectA)+length(pun_vectB)+length(pun_vectC));
    end
    s.behav(1,i).credit.backspread.(['diffAgiven' rew_tag(1:end-3)])=...
        s.behav(1,i).credit.backspread.(['probOptionAgiven' rew_tag])-s.behav(1,i).credit.backspread.(['probOptionAgiven' pun_tag]);
end

%grab output for easy viewing of data
tmp_out = s.behav(1,i).credit.backspread;
out_names = fieldnames(s.behav(1,i).credit.backspread);
for k = 1:length(out_names)
    if strfind(out_names{k},'diff')
        out.(out_names{k}) = tmp_out.(out_names{k});
    end
end




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%--------------------------->FORWARD SPREAD<------------------------------%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [s, out] = calculate_forwardspread(s,i,rew,pun,choice)
%This function will calculate a subjects "forward spread" or the subject's
%affintity to keep choosing an option B on trial n after choosing option B
%on trial n-1, and choosing option A on tials n-2 -> n-5 as a function of
%wheter or not the subject recieved a reward on one of the n-2 ->n-5
%trials.

%Set up the variables needed to automatically construct the strings
part1 = {'aaaax' 'bbbbx' 'ccccx'};
part2 = {'R' '0'};
seq_vectA = s.behav(1,i).credit.a4plusx;
seq_vectB = s.behav(1,i).credit.b4plusx;
seq_vectC = s.behav(1,i).credit.c4plusx;
seq_array = {seq_vectA seq_vectB seq_vectC}; 

%Create a new struct called sequence which contains the sequence of
%repeated variables, and their position of rew/punishment, i.e. aRaaax ect.
for j = 1:length(part1)
    for o = 1:length(part1{1})-1;
        tmp = part1{j}; %placeholder variable
        rew_str = [tmp(1:o) part2{1} tmp(o+1:end)];
        pun_str = [tmp(1:o) part2{2} tmp(o+1:end)];
        
        pos = 5-o;
        index = seq_array{j}-pos; %all places in which reward or punishment happened at specific position, for certain sequence
        
        s.behav(1,i).credit.sequence.(rew_str) = intersect(index, rew)+pos;
        s.behav(1,i).credit.sequence.(pun_str) = intersect(index, pun)+pos;
    end
end

%Grab the newly created sequence variable names for computing choice
%history
seq_var = fieldnames(s.behav(1,i).credit.sequence);

%Find the choice history and save in a new struct "seq_next_choice"
for j = 1:length(seq_var)
    %Grab leading character of var variable
    leading_char = seq_var{j}(1);
    
    
    %I don't really like this part but I needed to make it work...
    %Choice vectors = aRaaaxB = aRaaaBB; aRaaaXC = aRaaaCC ect.
    switch leading_char
        case 'a'
            %temp_1 = (find(choice==1)-1);
            temp_2 = (find(choice==2));
            temp_3 = (find(choice==3));
            
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'B'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==2)-1)),temp_2);
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'C'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==3)-1)),temp_3);
        case 'b'
            temp_1 = (find(choice==1));
            %temp_2 = (find(choice==2)-1);
            temp_3 = (find(choice==3));
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'A'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==1)-1)),temp_1);
            
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'C'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==3)-1)),temp_3);
        case 'c'
            temp_1 = (find(choice==1));
            temp_2 = (find(choice==2));
            % temp_3 = (find(choice==3)-1);
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'A'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==1)-1)),temp_1);
            
            s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'B'])=...
                intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==2)-1)),temp_2);
            
    end
    
    % find sequences where terminal X was followed by X not Y
    
    %Find where the sequence happened and had repeating X options.
    %For example aRaaaxB is all the indices where there were a string
    %of A chosen stimuli with the first on being rewarded and where the
    %n-1 choice (x) = B and the n choice = B.
    %         s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'A'])=...
    %             intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==1)-1)),temp_1);
    %
    %         s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'B'])=...
    %             intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==2)-1)),temp_2);
    %
    %         s.behav(1,i).credit.seq_next_choice.([seq_var{j} 'C'])=...
    %             intersect(intersect(s.behav(1,i).credit.sequence.(seq_var{j}), (find(choice==3)-1)),temp_3);
end


%reshape to organize data better
data_names=fieldnames(s.behav(1,i).credit.seq_next_choice);
seq_data=reshape((data_names),16,3);

%Hacky way to make struct names correct (n-5, n-4...)
count=6;
for k = 1:4:length(seq_data)
    count = count-1;
    rew_tag = seq_data{k};
    pun_tag = seq_data{k+2};
    
    %Choice vectors = aRaaaxB + aRaaaxC ect. Where x = B and C
    %respectively.
    choice_rew_vectA = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k,1}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+1,1}));
    choice_rew_vectB = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k,2}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+1,2}));
    choice_rew_vectC = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k,3}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+1,3}));
    
    choice_pun_vectA = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k+2,1}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+3,1}));
    choice_pun_vectB = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k+2,2}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+3,2}));
    choice_pun_vectC = union(s.behav(1,i).credit.seq_next_choice.(seq_data{k+2,3}),s.behav(1,i).credit.seq_next_choice.(seq_data{k+3,3}));
    
    %Rew and Pun vectors = aRaaax, bRbbbx... c0cccx ect.
    rew_vectA = s.behav(1,i).credit.sequence.(seq_data{k,1}(1:end-1));
    rew_vectB = s.behav(1,i).credit.sequence.(seq_data{k,2}(1:end-1));
    rew_vectC = s.behav(1,i).credit.sequence.(seq_data{k,3}(1:end-1));
    
    pun_vectA = s.behav(1,i).credit.sequence.(seq_data{k+2,1}(1:end-1));
    pun_vectB = s.behav(1,i).credit.sequence.(seq_data{k+2,2}(1:end-1));
    pun_vectC = s.behav(1,i).credit.sequence.(seq_data{k+2,3}(1:end-1));
    
    %Take the total number of times the specific sequences have occured
    %with rewards/no reward at specific time points and divde that number by the
    %total number of times a rew/punishment occured for all sequences.
    %Ex: ARAAABB/ARAAAX = (sum(aRaaabb,aRaaacc,bRbbbaa..ect))/(sum(aRaaaX,
    %bRbbbX, cRcccX))
    
    %n-5 -> n-2
    %Reward
    if isempty(rew_vectA) && isempty(rew_vectB) && isempty(rew_vectC)
        s.behav(1,i).credit.forspread.(['probOptionAgiven' rew_tag])=NaN;
    else
        s.behav(1,i).credit.forspread.(['probOptionAgiven' rew_tag])=...
            (length(choice_rew_vectA)+length(choice_rew_vectB)+length(choice_rew_vectC))./ (length(rew_vectA)+length(rew_vectB)+length(rew_vectC));
        
    end
    
    %Punishment
    if isempty(pun_vectA) && isempty(pun_vectB) && isempty(pun_vectC)
        s.behav(1,i).credit.forspread.(['probOptionAgiven' pun_tag])=NaN;
    else
        s.behav(1,i).credit.forspread.(['probOptionAgiven' pun_tag])=...
            (length(choice_pun_vectA)+length(choice_pun_vectB)+length(choice_pun_vectC))./ (length(pun_vectA)+length(pun_vectB)+length(pun_vectC));
        
    end
    
    %Find the difference in probabilities
    s.behav(1,i).credit.forspread.(['diffAgiven_n_' num2str(count)])=...
        s.behav(1,i).credit.forspread.(['probOptionAgiven' rew_tag])-s.behav(1,i).credit.forspread.(['probOptionAgiven' pun_tag]);
    
end

%grab output for easy viewing of data
tmp_out = s.behav(1,i).credit.forspread;
out_names = fieldnames(s.behav(1,i).credit.forspread);
for k = 1:length(out_names)
    if strfind(out_names{k},'diff')
        out.(out_names{k}) = tmp_out.(out_names{k});
    end
end




