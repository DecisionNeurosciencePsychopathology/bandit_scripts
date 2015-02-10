design = bandit_fmri_load_design
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

A_top_rew = zeros(1,length(design.Arew))';
A_left_rew = zeros(1,length(design.Arew))';
A_right_rew = zeros(1,length(design.Arew))';

B_top_rew = zeros(1,length(design.Arew))';
B_left_rew = zeros(1,length(design.Arew))';
B_right_rew = zeros(1,length(design.Arew))';

C_top_rew = zeros(1,length(design.Arew))';
C_left_rew = zeros(1,length(design.Arew))';
C_right_rew = zeros(1,length(design.Arew))';


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

  
 A_top_rew = (A_top_idx == 1 & (design.reinforced1 == 7 ));
 A_left_rew = ( A_left_idx == 1 & (design.reinforced2 == 2 ));
 A_right_rew = ( A_right_idx == 1 & (design.reinforced3 == 3 ));

 B_top_rew = (B_top_idx == 1 & (design.reinforced1 == 7 ));
 B_left_rew = ( B_left_idx == 1 & (design.reinforced2 == 2 ));
 B_right_rew = ( B_right_idx == 1 & (design.reinforced3 == 3 ));
 
 C_top_rew = (C_top_idx == 1 & (design.reinforced1 == 7 ));
 C_left_rew = ( C_left_idx == 1 & (design.reinforced2 == 2 ));
 C_right_rew = ( C_right_idx == 1 & (design.reinforced3 == 3 ));
% 

% for n=1:length(design.Arew)
%     if A_top_idx == 1 && (design.reinforced1 == 7 )
%     A_top_rew(n) = 1;
%     end
%     if A_left_idx == 1 && (design.reinforced2 == 2 )
%     A_left_rew(n) = 1;
%     end
%     if A_right_idx == 1 && (design.reinforced3 == 3 )
%     A_right_rew(n) = 1;
%     end
% 
%     if B_top_idx == 1 && (design.reinforced1 == 7 )
%     B_top_rew(n) = 1;
%     end
%     if B_left_idx == 1 && (design.reinforced2 == 2 )
%     B_left_rew(n) = 1;
%     end
%     if B_right_idx == 1 && (design.reinforced3 == 3 )
%     B_right_rew(n) = 1;
%     end
% 
%     if C_top_idx == 1 && (design.reinforced1 == 7 )
%     C_top_rew(n) = 1;
%     end
%     if C_left_idx == 1 && (design.reinforced2 == 2 )
%     C_left_rew(n) = 1;
%     end
%     if C_right_idx == 1 && (design.reinforced3 == 3 )
%     C_right_rew(n) = 1;
%     end
% end
A_real_rew = A_top_rew + A_left_rew + A_right_rew;
B_real_rew = B_top_rew + B_left_rew + B_right_rew;
C_real_rew = C_top_rew + C_left_rew + C_right_rew;

figure(1)
clf;
subplot(3,1,1)
plot(smooth(A_real_rew));
subplot(3,1,2)
plot(smooth(B_real_rew),'r');
subplot(3,1,3)
plot(smooth(C_real_rew),'g'); 


figure(2)
clf;
% subplot(3,1,1)
plot(smooth(design.Arew,10));hold on;
plot(smooth(design.Brew,10),'r');
plot(smooth(design.Crew,10),'g'); 
axis([0 302 -.1 1.1])
subplot(3,1,2)
plot(smooth(design.Brew,20),'r');
axis([0 302 -.1 1.1])
subplot(3,1,3)

axis([0 302 -.1 1.1])

new.Arew = design.Arew;
new.Brew = design.Brew;
new.Crew = design.Crew;
figure(2)
clf;
% subplot(3,1,1)
plot(smooth(new.Arew,10));hold on;
plot(smooth(new.Brew,10),'r');
plot(smooth(new.Crew,10),'g'); 
axis([0 302 -.1 1.1])

save new_contingency_use_this_one new


% Additional instruction: "If you're not having much luck with one picture,
% make sure that you try the others"