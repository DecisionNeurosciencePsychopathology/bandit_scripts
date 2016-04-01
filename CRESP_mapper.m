
%Find what trials have which stim at that location (Binary arrays)
A_top_idx = zeros(1,length(design.Arew))';
A_left_idx = zeros(1,length(design.Arew))';
A_right_idx = zeros(1,length(design.Arew))';

B_top_idx = zeros(1,length(design.Arew))';
B_left_idx = zeros(1,length(design.Arew))';
B_right_idx = zeros(1,length(design.Arew))';

C_top_idx = zeros(1,length(design.Arew))';
C_left_idx = zeros(1,length(design.Arew))';
C_right_idx = zeros(1,length(design.Arew))';



%Grab where each picture is located for each trial
A_top_idx(find(cellfun(@(x) isequal(x, 'A'), design.topstim)))=1;
A_left_idx(find(cellfun(@(x) isequal(x, 'A'), design.leftstim)))=1;
A_right_idx(find(cellfun(@(x) isequal(x, 'A'), design.rightstim)))=1;

B_top_idx(find(cellfun(@(x) isequal(x, 'B'), design.topstim)))=1;
B_left_idx(find(cellfun(@(x) isequal(x, 'B'), design.leftstim)))=1;
B_right_idx(find(cellfun(@(x) isequal(x, 'B'), design.rightstim)))=1;

C_top_idx(find(cellfun(@(x) isequal(x, 'C'), design.topstim)))=1;
C_left_idx(find(cellfun(@(x) isequal(x, 'C'), design.leftstim)))=1;
C_right_idx(find(cellfun(@(x) isequal(x, 'C'), design.rightstim)))=1;

% checkA = A_top_idx + A_left_idx + A_right_idx
%     B_top_idx + B_left_idx + B_right_idx + ...
%     C_top_idx + C_left_idx + C_right_idx;
% locations = [A_top_idx A_left_idx  A_right_idx  ...
%     B_top_idx  B_left_idx  B_right_idx  ...
%     C_top_idx  C_left_idx  C_right_idx];
% %Initalize
%design_correct_RESP = ones(1,300)';

% for n = 1:length(b.chosen_stim)
%     if design.Arew(n)==1 && design.Brew(n)==1 && design.Crew(n)==1
%         design_correct_RESP{n} = '723';
%     elseif design.Arew(n)==1 && design.Brew(n)==1
%         if A_top_idx(n) && B_left_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif A_top_idx(n) && B_right_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif A_left_idx(n) && B_top_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif A_left_idx(n) && B_right_idx(n)
%             design_correct_RESP{n} = '023';
%             
%         elseif A_right_idx(n) && B_top_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif A_right_idx(n) && B_left_idx(n)
%             design_correct_RESP{n} = '023';
%         end
%     elseif design.Arew(n)==1 && design.Crew(n)==1
%         if A_top_idx(n) && C_left_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif A_top_idx(n) && C_right_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif A_left_idx(n) && C_top_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif A_left_idx(n) && C_right_idx(n)
%             design_correct_RESP{n} = '023';
%             
%         elseif A_right_idx(n) && C_top_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif A_right_idx(n) && C_left_idx(n)
%             design_correct_RESP{n} = '023';
%         end
%     elseif design.Arew(n)==1
%         if A_top_idx(n)
%             design_correct_RESP{n} = '700';
%             
%         elseif A_left_idx(n)
%             design_correct_RESP{n} = '020';
%             
%         elseif A_right_idx(n)
%             design_correct_RESP{n} = '003';
%         end
%             
%     elseif design.Brew(n)==1 && design.Crew(n)==1
%           if B_top_idx(n) && C_left_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif B_top_idx(n) && C_right_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif B_left_idx(n) && C_top_idx(n)
%             design_correct_RESP{n} = '720';
%             
%         elseif B_left_idx(n) && C_right_idx(n)
%             design_correct_RESP{n} = '023';
%             
%         elseif B_right_idx(n) && C_top_idx(n)
%             design_correct_RESP{n} = '703';
%             
%         elseif B_right_idx(n) && C_left_idx(n)
%             design_correct_RESP{n} = '023';
%         end
%     elseif design.Brew(n)==1
%            if B_top_idx(n)
%             design_correct_RESP{n} = '700';
%             
%         elseif B_left_idx(n)
%             design_correct_RESP{n} = '020';
%             
%         elseif B_right_idx(n)
%             design_correct_RESP{n} = '003';
%         end
%     elseif design.Crew(n)==1
%            if C_top_idx(n)
%             design_correct_RESP{n} = '700';
%             
%         elseif C_left_idx(n)
%             design_correct_RESP{n} = '020';
%             
%         elseif C_right_idx(n)
%             design_correct_RESP{n} = '003';
%         end
%     else 
%         design_correct_RESP{n} = '000';
%     end
% end

%Alex and I, (mostly Alex) made a new reward schedule which is why there is
%a new.Arew. Typically though you can just have new.Arew = design.Arew if
%something went haywire and you need to reset the reinforcement schedule

new.Arew = design.Arew;
new.Brew = design.Brew;
new.Crew = design.Crew;


%when the top is correct
new.reinforced1 = (A_top_idx(~isnan(new.Arew)) & new.Arew(~isnan(new.Arew))) | ...
    (B_top_idx(~isnan(new.Arew)) & new.Brew(~isnan(new.Arew))) | ...
    (C_top_idx(~isnan(new.Arew)) & new.Crew(~isnan(new.Arew)));
new.reinforced1 = 7.*new.reinforced1;

new.reinforced2 = (A_left_idx(~isnan(new.Arew)) & new.Arew(~isnan(new.Arew))) | ...
    (B_left_idx(~isnan(new.Arew)) & new.Brew(~isnan(new.Arew))) | ...
    (C_left_idx(~isnan(new.Arew)) & new.Crew(~isnan(new.Arew)));
new.reinforced2 = 2.*new.reinforced2;

new.reinforced3 = 3.*(A_right_idx(~isnan(new.Arew)) & new.Arew(~isnan(new.Arew))) | ...
    (B_right_idx(~isnan(new.Arew)) & new.Brew(~isnan(new.Arew))) | ...
    (C_right_idx(~isnan(new.Arew)) & new.Crew(~isnan(new.Arew)));
new.reinforced3 = 3.*new.reinforced3;

save correct_block_design

new.nanny_reinforced1 = [new.reinforced1(1:100); NaN; new.reinforced1(101:200); NaN; new.reinforced1(201:end)];
new.nanny_reinforced2 = [new.reinforced2(1:100); NaN; new.reinforced2(101:200); NaN; new.reinforced2(201:end)];
new.nanny_reinforced3 = [new.reinforced3(1:100); NaN; new.reinforced3(101:200); NaN; new.reinforced3(201:end)];

%For practice trials only
%save correct_block_design_practice
%new.nanny_reinforced1 = [new.reinforced1(1:52); NaN; new.reinforced1(53:end)];
%new.nanny_reinforced2 = [new.reinforced2(1:52); NaN; new.reinforced2(53:end)];
%new.nanny_reinforced3 = [new.reinforced3(1:52); NaN; new.reinforced3(53:end)];

new.topstim = design.topstim;
new.leftstim = design.leftstim;
new.rightstim = design.rightstim;
fprintf(sprintf('topstim'),cell2mat(new.topstim));

dlmwrite(sprintf('new_design_012315'),[new.Arew new.Brew new.Crew new.nanny_reinforced1 new.nanny_reinforced2 new.nanny_reinforced3],'\t');

%Practice Only
%dlmwrite(sprintf('new_pract_design_21115'),[new.Arew new.Brew new.Crew new.nanny_reinforced1 new.nanny_reinforced2 new.nanny_reinforced3],'\t');


