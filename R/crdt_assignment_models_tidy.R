#  Alex's tidyverse version of Vanessa's credit assignment models
#  run bandit_beh_analyses.R and make_design_crdt_assignment.R first
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit3.RData")


require(lme4)
library(corrplot)

#run regressions
A_reg_all = glmer(
  A ~
    Amin1rmin1 + Amin1rmin2 + Amin1rmin3 +
    Amin2rmin1 + Amin2rmin2 + Amin2rmin3 +
    Amin3rmin1 + Amin3rmin2 + Amin3rmin3 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

# try longer

A_reg_all5 = glmer(
  A ~
    Amin1 + Amin2 + Amin3 + Amin4 + Amin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Amin1rmin1 + Amin1rmin2 + Amin1rmin3 + Amin1rmin4 + Amin1rmin5 +
    Amin2rmin1 + Amin2rmin2 + Amin2rmin3 + Amin2rmin4 + Amin2rmin5 +
    Amin3rmin1 + Amin3rmin2 + Amin3rmin3 + Amin3rmin4 + Amin3rmin5 +
    Amin4rmin1 + Amin4rmin2 + Amin4rmin3 + Amin4rmin4 + Amin4rmin5 +
    Amin5rmin1 + Amin5rmin2 + Amin5rmin3 + Amin5rmin4 + Amin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
# check coeff matrix
A5 = matrix(data = fixef(A_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(A5,is.corr = FALSE, method = "color")


B_reg_all5 = glmer(
  B ~
    Bmin1 + Bmin2 + Bmin3 + Bmin4 + Bmin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Bmin1rmin1 + Bmin1rmin2 + Bmin1rmin3 + Bmin1rmin4 + Bmin1rmin5 +
    Bmin2rmin1 + Bmin2rmin2 + Bmin2rmin3 + Bmin2rmin4 + Bmin2rmin5 +
    Bmin3rmin1 + Bmin3rmin2 + Bmin3rmin3 + Bmin3rmin4 + Bmin3rmin5 +
    Bmin4rmin1 + Bmin4rmin2 + Bmin4rmin3 + Bmin4rmin4 + Bmin4rmin5 +
    Bmin5rmin1 + Bmin5rmin2 + Bmin5rmin3 + Bmin5rmin4 + Bmin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
# check coeff matrix
B5 = matrix(data = fixef(B_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(B5,is.corr = FALSE, method = "color")

C_reg_all5 = glmer(
  C ~
    Cmin1 + Cmin2 + Cmin3 + Cmin4 + Cmin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Cmin1rmin1 + Cmin1rmin2 + Cmin1rmin3 + Cmin1rmin4 + Cmin1rmin5 +
    Cmin2rmin1 + Cmin2rmin2 + Cmin2rmin3 + Cmin2rmin4 + Cmin2rmin5 +
    Cmin3rmin1 + Cmin3rmin2 + Cmin3rmin3 + Cmin3rmin4 + Cmin3rmin5 +
    Cmin4rmin1 + Cmin4rmin2 + Cmin4rmin3 + Cmin4rmin4 + Cmin4rmin5 +
    Cmin5rmin1 + Cmin5rmin2 + Cmin5rmin3 + Cmin5rmin4 + Cmin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C5 = matrix(data = fixef(C_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(C5,is.corr = FALSE, method = "color")


A_diag_all5 = diag(vcov(A_reg_all5))
B_diag_all5 = diag(vcov(B_reg_all5))
C_diag_all5 = diag(vcov(C_reg_all5))
all_coeffs_combined5 = (1 / rowSums(cbind(1 / A_diag_all5, 1 / B_diag_all5, 1 /
                                           C_diag_all5), na.rm = TRUE)) * (rowSums(
                                             cbind(
                                               fixef(A_reg_all5) / A_diag_all5,
                                               fixef(B_reg_all5) / B_diag_all5,
                                               fixef(C_reg_all5) / C_diag_all5
                                             ),
                                             na.rm = TRUE
                                           ))
all_coeffs_combined_matrix5 = matrix(data = all_coeffs_combined5[12:36],nrow = 5,ncol=5)
colnames(all_coeffs_combined_matrix5) <- c("r(t-1)","r(t-2)", "r(t-3)","r(t-4)","r(t-5)")
rownames(all_coeffs_combined_matrix5) <- c("choice(t-1)","choice(t-2)", "choice(t-3)","choice(t-4)","choice(t-5)")
corrplot(all_coeffs_combined_matrix5,is.corr = FALSE, method = "color")

# try longer with group

A_reg_all5_g = glmer(
  A ~
    Amin1 + Amin2 + Amin3 + Amin4 + Amin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Group*Amin1rmin1 + Group*Amin1rmin2 + Group*Amin1rmin3 + Group*Amin1rmin4 + Group*Amin1rmin5 +
    Group*Amin2rmin1 + Group*Amin2rmin2 + Group*Amin2rmin3 + Group*Amin2rmin4 + Group*Amin2rmin5 +
    Group*Amin3rmin1 + Group*Amin3rmin2 + Group*Amin3rmin3 + Group*Amin3rmin4 + Group*Amin3rmin5 +
    Group*Amin4rmin1 + Group*Amin4rmin2 + Group*Amin4rmin3 + Group*Amin4rmin4 + Group*Amin4rmin5 +
    Group*Amin5rmin1 + Group*Amin5rmin2 + Group*Amin5rmin3 + Group*Amin5rmin4 + Group*Amin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
# check coeff matrix
A5 = matrix(data = fixef(A_reg_all5_g)[40:114],nrow = 3,ncol = 25)
A5ctrl = matrix(data = A5[1,],nrow = 5,ncol = 5)
  corrplot(A5ctrl,is.corr = FALSE, method = "color", title = 'controls vs. attempters', mar=c(0,0,1,0))
  A5dep = matrix(data = A5[2,],nrow = 5,ncol = 5)
  corrplot(A5dep,is.corr = FALSE, method = "color", title = 'NS depressed vs. attempters', mar=c(0,0,1,0))
  A5id = matrix(data = A5[3,],nrow = 5,ncol = 5)
  corrplot(A5id,is.corr = FALSE, method = "color", title = 'ideators vs. attempters', mar=c(0,0,1,0))


B_reg_all5_g = glmer(
  B ~
    Bmin1 + Bmin2 + Bmin3 + Bmin4 + Bmin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Group*Bmin1rmin1 + Group*Bmin1rmin2 + Group*Bmin1rmin3 + Group*Bmin1rmin4 + Group*Bmin1rmin5 +
    Group*Bmin2rmin1 + Group*Bmin2rmin2 + Group*Bmin2rmin3 + Group*Bmin2rmin4 + Group*Bmin2rmin5 +
    Group*Bmin3rmin1 + Group*Bmin3rmin2 + Group*Bmin3rmin3 + Group*Bmin3rmin4 + Group*Bmin3rmin5 +
    Group*Bmin4rmin1 + Group*Bmin4rmin2 + Group*Bmin4rmin3 + Group*Bmin4rmin4 + Group*Bmin4rmin5 +
    Group*Bmin5rmin1 + Group*Bmin5rmin2 + Group*Bmin5rmin3 + Group*Bmin5rmin4 + Group*Bmin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,

  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
alarm()

C_reg_all5_g = glmer(
  C ~
    Cmin1 + Cmin2 + Cmin3 + Cmin4 + Cmin5 +
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 +
    Group*Cmin1rmin1 + Group*Cmin1rmin2 + Group*Cmin1rmin3 + Group*Cmin1rmin4 + Group*Cmin1rmin5 +
    Group*Cmin2rmin1 + Group*Cmin2rmin2 + Group*Cmin2rmin3 + Group*Cmin2rmin4 + Group*Cmin2rmin5 +
    Group*Cmin3rmin1 + Group*Cmin3rmin2 + Group*Cmin3rmin3 + Group*Cmin3rmin4 + Group*Cmin3rmin5 +
    Group*Cmin4rmin1 + Group*Cmin4rmin2 + Group*Cmin4rmin3 + Group*Cmin4rmin4 + Group*Cmin4rmin5 +
    Group*Cmin5rmin1 + Group*Cmin5rmin2 + Group*Cmin5rmin3 + Group*Cmin5rmin4 + Group*Cmin5rmin5 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

A_diag_all5_g = diag(vcov(A_reg_all5_g))
B_diag_all5_g = diag(vcov(B_reg_all5_g))
C_diag_all5_g = diag(vcov(C_reg_all5_g))
all_coeffs_combined5 = (1 / rowSums(cbind(1 / A_diag_all5, 1 / B_diag_all5, 1 /
                                            C_diag_all5), na.rm = TRUE)) * (rowSums(
                                              cbind(
                                                fixef(A_reg_all5) / A_diag_all5,
                                                fixef(B_reg_all5) / B_diag_all5,
                                                fixef(C_reg_all5) / C_diag_all5
                                              ),
                                              na.rm = TRUE
                                            ))



# try with group
A_reg_all_group = glmer(
  A ~
    Group*Amin1rmin1 + Group*Amin1rmin2 + Group*Amin1rmin3 +
    Group*Amin2rmin1 + Group*Amin2rmin2 + Group*Amin2rmin3 +
    Group*Amin3rmin1 + Group*Amin3rmin2 + Group*Amin3rmin3 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


B_reg_all_group = glmer(
  B ~
    Group*Bmin1rmin1 + Group*Bmin1rmin2 + Group*Bmin1rmin3 +
    Group*Bmin2rmin1 + Group*Bmin2rmin2 + Group*Bmin2rmin3 +
    Group*Bmin3rmin1 + Group*Bmin3rmin2 + Group*Bmin3rmin3 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

C_reg_all_group = glmer(
  C ~
    Group*Cmin1rmin1 + Group*Cmin1rmin2 + Group*Cmin1rmin3 +
    Group*Cmin2rmin1 + Group*Cmin2rmin2 + Group*Cmin2rmin3 +
    Group*Cmin3rmin1 + Group*Cmin3rmin2 + Group*Cmin3rmin3 +
    (1 | ID),
  data = bdf,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


A_diag_all_group = diag(vcov(A_reg_all_group))
B_diag_all_group = diag(vcov(B_reg_all_group))
C_diag_all_group = diag(vcov(C_reg_all_group))
all_coeffs_combined_group = (1 / rowSums(cbind(1 / A_diag_all_group, 1 / B_diag_all_group, 1 /
                                           C_diag_all), na.rm = TRUE)) * (rowSums(
                                             cbind(
                                               fixef(A_reg_all_group) / A_diag_all_group,
                                               fixef(B_reg_all_group) / B_diag_all_group,
                                               fixef(C_reg_all_group) / C_diag_all_group
                                             ),
                                             na.rm = TRUE
                                           ))
all_coeffs_combined_matrix_group = matrix(data = all_coeffs_combined_group[14:40],nrow = 9,ncol=3)
all_coeffs_combined_matrix_ctrl = matrix(data = all_coeffs_combined_group[c(14,17,20,23,26,29,32,35,38)],nrow = 3,ncol=3)
corrplot(all_coeffs_combined_matrix_ctrl, method = "color")
all_coeffs_combined_matrix_dep = matrix(data = all_coeffs_combined_group[c(15,18,21,24,27,30,33,36,39)],nrow = 3,ncol=3)
corrplot(all_coeffs_combined_matrix_dep, method = "color")
all_coeffs_combined_matrix_id = matrix(data = all_coeffs_combined_group[c(16,19,22,25,28,31,34,35,40)],nrow = 3,ncol=3)
corrplot(all_coeffs_combined_matrix_id, method = "color")








#plot
range_coeffs_combined = max(all_coeffs_combined[2:10]) - min(all_coeffs_combined[2:10])
coeffs_combined_log_scaled_pre = log((all_coeffs_combined + abs(min(
  all_coeffs_combined[2:10]
)) + range_coeffs_combined + .01) / range_coeffs_combined) #normalize to be between 0 & 1 after log transformation
coeffs_combined_log_scaled = coeffs_combined_log_scaled_pre + (.9 - max(coeffs_combined_log_scaled_pre))

#### plot figures 5C-E two ways ####

plot(
  1:5,
  1:5,
  col = 'white',
  axes = F,
  bty = 'n',
  xlab = '',
  ylab = ''
) #blank fig to start following on new page

#make figs 5C-E using individual-level data points- large points are the median
plot(
  1:5,
  choice_x_reward[1, 2:6],
  ylim = c(-10, 10),
  pch = '.',
  xlab = 'trials in the past',
  ylab = 'choice x reward weight',
  main = 'figure 5C'
)
for (subj in 2:length(subj_ids)) {
  points(1:5, choice_x_reward[subj, 2:6], pch = '.')
}
points(1:5, c(
  median(choice_x_reward[, 2], na.rm = TRUE),
  median(choice_x_reward[, 3], na.rm = TRUE),
  median(choice_x_reward[, 4], na.rm = TRUE),
  median(choice_x_reward[, 5], na.rm = TRUE),
  median(choice_x_reward[, 6], na.rm = TRUE)
), pch = 19)
lines(1:5, c(
  median(choice_x_reward[, 2], na.rm = TRUE),
  median(choice_x_reward[, 3], na.rm = TRUE),
  median(choice_x_reward[, 4], na.rm = TRUE),
  median(choice_x_reward[, 5], na.rm = TRUE),
  median(choice_x_reward[, 6], na.rm = TRUE)
))
abline(h = 0, col = 'gray20', lty = 2)

plot(
  1:5,
  choice_x_past_reward[1, 2:6],
  ylim = c(-10, 10),
  pch = '.',
  xlab = 'trials in the past',
  ylab = 'past choices x immediately previous reward weight',
  main = 'figure 5D'
)
for (subj in 2:length(subj_ids)) {
  points(1:5, choice_x_past_reward[subj, 2:6], pch = '.')
}
points(1:5, c(
  median(choice_x_past_reward[, 2], na.rm = TRUE),
  median(choice_x_past_reward[, 3], na.rm = TRUE),
  median(choice_x_past_reward[, 4], na.rm = TRUE),
  median(choice_x_past_reward[, 5], na.rm = TRUE),
  median(choice_x_past_reward[, 6], na.rm = TRUE)
), pch = 19)
lines(2:5, c(
  median(choice_x_past_reward[, 3], na.rm = TRUE),
  median(choice_x_past_reward[, 4], na.rm = TRUE),
  median(choice_x_past_reward[, 5], na.rm = TRUE),
  median(choice_x_past_reward[, 6], na.rm = TRUE)
))
abline(h = 0, col = 'gray20', lty = 2)

plot(
  1:5,
  past_choice_x_reward[1, 2:6],
  ylim = c(-10, 10),
  pch = '.',
  xlab = 'trials in the past',
  ylab = 'immediately previous choice x past rewards weight',
  main = 'figure 5E'
)
for (subj in 2:length(subj_ids)) {
  points(1:5, past_choice_x_reward[subj, 2:6], pch = '.')
}
points(1:5, c(
  median(past_choice_x_reward[, 2], na.rm = TRUE),
  median(past_choice_x_reward[, 3], na.rm = TRUE),
  median(past_choice_x_reward[, 4], na.rm = TRUE),
  median(past_choice_x_reward[, 5], na.rm = TRUE),
  median(past_choice_x_reward[, 6], na.rm = TRUE)
), pch = 19)
lines(2:5, c(
  median(past_choice_x_reward[, 3], na.rm = TRUE),
  median(past_choice_x_reward[, 4], na.rm = TRUE),
  median(past_choice_x_reward[, 5], na.rm = TRUE),
  median(past_choice_x_reward[, 6], na.rm = TRUE)
))
abline(h = 0, col = 'gray20', lty = 2)

#make figs 5C-E using data from multilevel model- but again note that this is not allowing these variables
# to vary by subject, only uses the fixed effects SE
plot(
  1:5,
  1:5,
  col = 'white',
  axes = F,
  bty = 'n',
  xlab = '',
  ylab = ''
) #blank fig to start on new page
A_reg_SE = sqrt(A_diag_all)
A_reg_coeffs = fixef(A_reg_all)

plot(
  1:5,
  A_reg_coeffs[seq(2, 35, 7)],
  ylim = c(0, 2),
  pch = 16,
  xlab = 'trials in the past',
  ylab = 'choice x reward weight',
  main = 'figure 5C'
)
lines(1:5, A_reg_coeffs[seq(2, 35, 7)])
points(1:5, (A_reg_coeffs[seq(2, 35, 7)] + A_reg_SE[seq(2, 35, 7)]), pch =
         6) # arrows approximate standard error bars
points(1:5, (A_reg_coeffs[seq(2, 35, 7)] - A_reg_SE[seq(2, 35, 7)]), pch =
         2)
abline(h = 0, col = 'gray20', lty = 2)

plot(
  1:5,
  A_reg_coeffs[seq(2, 31, 6)],
  ylim = c(0, 2),
  pch = 16,
  xlab = 'trials in the past',
  ylab = 'past choices x immediately previous reward weight',
  main = 'figure 5D'
)
lines(1:5, A_reg_coeffs[seq(2, 31, 6)])
points(1:5, (A_reg_coeffs[seq(2, 31, 6)] + A_reg_SE[seq(2, 31, 6)]), pch =
         6)
points(1:5, (A_reg_coeffs[seq(2, 31, 6)] - A_reg_SE[seq(2, 31, 6)]), pch =
         2)
abline(h = 0, col = 'gray20', lty = 2)

plot(
  1:5,
  A_reg_coeffs[2:6],
  ylim = c(0, 2),
  pch = 16,
  xlab = 'trials in the past',
  ylab = 'immediately previous choice x past rewards weight',
  main = 'figure 5E'
)
lines(1:5, A_reg_coeffs[2:6])
points(1:5, (A_reg_coeffs[2:6] + A_reg_SE[2:6]), pch = 6)
points(1:5, (A_reg_coeffs[2:6] - A_reg_SE[2:6]), pch = 2)
abline(h = 0, col = 'gray20', lty = 2)


#### fig 5B allowing for random effects in reward*choice interactions ####

#try different combinations of random effects, corresponding to diagonal/vertical/horizonal
# parts of fig 5B as well as combinations and test model fit- that is, does allowing effect of certain
# reward*choice interactions to vary by subject improve model fit?
# note: these take a long time to run
AIC_basic = AIC(A_reg_all) + AIC(B_reg_all) + AIC(C_reg_all)
A_reg_all_prevrew = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 +
                 A1_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_prevrew = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 +
                 B1_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_prevrew = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 +
                 C1_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_prevrew = AIC(A_reg_all_prevrew) + AIC(B_reg_all_prevrew) + AIC(C_reg_all_prevrew)
AIC_basic - AIC_prevrew

A_reg_all_last = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (A1_rew1 |
                 subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_last = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (B1_rew1 |
                 subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_last = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (C1_rew1 |
                 subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_last = AIC(A_reg_all_last) + AIC(B_reg_all_last) + AIC(C_reg_all_last)
AIC_basic - AIC_last

A_reg_all_prevchoice = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (A1_rew1 + A2_rew1 + A3_rew1 + A4_rew1 +
                 A5_rew1 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_prevchoice = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (B1_rew1 + B2_rew1 + B3_rew1 + B4_rew1 +
                 B5_rew1 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_prevchoice = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (C1_rew1 + C2_rew1 + C3_rew1 + C4_rew1 +
                 C5_rew1 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_prevchoice = AIC(A_reg_all_prevchoice) + AIC(B_reg_all_prevchoice) +
  AIC(C_reg_all_prevchoice)
AIC_basic - AIC_prevchoice

A_reg_all_prevint = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (A1_rew1 + A2_rew2 + A3_rew3 + A4_rew4 +
                 A5_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_prevint = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (B1_rew1 + B2_rew2 + B3_rew3 + B4_rew4 +
                 B5_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_prevint = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (C1_rew1 + C2_rew2 + C3_rew3 + C4_rew4 +
                 C5_rew5 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_prevint = AIC(A_reg_all_prevint) + AIC(B_reg_all_prevint) + AIC(C_reg_all_prevint)
AIC_basic - AIC_prevint

A_reg_all_prev1 = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (A1_rew1 + A2_rew2 + A1_rew2 + A2_rew1 +
                 A3_rew3 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_prev1 = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (B1_rew1 + B2_rew2 + B1_rew2 + B2_rew1 +
                 B3_rew3 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_prev1 = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (C1_rew1 + C2_rew2 + C1_rew2 + C2_rew1 +
                 C3_rew3 | subjID),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_prev1 = AIC(A_reg_all_prev1) + AIC(B_reg_all_prev1) + AIC(C_reg_all_prev1)
AIC_basic - AIC_prev1

A_reg_all_prev2 = glmer(
  a_choice ~ A1_rew1 + A1_rew2 + A1_rew3 + A1_rew4 + A1_rew5 + A1_rew6 + A2_rew1 +
    A2_rew2 +
    A2_rew3 + A2_rew4 + A2_rew5 + A2_rew6 + A3_rew1 +
    A3_rew2 + A3_rew3 + A3_rew4 + A3_rew5 +
    A3_rew6 + A4_rew1 + A4_rew2 + A4_rew3 + A4_rew4 +
    A4_rew5 + A4_rew6 + A5_rew1 + A5_rew2 +
    A5_rew3 + A5_rew4 + A5_rew5 + A5_rew6 + A6_rew1 +
    A6_rew2 + A6_rew3 + A6_rew4 + A6_rew5 +
    A6_rew6 + (
      A1_rew1 + A2_rew2 + A1_rew2 + A2_rew1 + A3_rew3 + A1_rew3 + A2_rew3 + A3_rew1 +
        A3_rew2 + A4_rew4 |
        subjID
    ),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
B_reg_all_prev2 = glmer(
  b_choice ~ B1_rew1 + B1_rew2 + B1_rew3 + B1_rew4 + B1_rew5 + B1_rew6 + B2_rew1 +
    B2_rew2 +
    B2_rew3 + B2_rew4 + B2_rew5 + B2_rew6 + B3_rew1 +
    B3_rew2 + B3_rew3 + B3_rew4 + B3_rew5 +
    B3_rew6 + B4_rew1 + B4_rew2 + B4_rew3 + B4_rew4 +
    B4_rew5 + B4_rew6 + B5_rew1 + B5_rew2 +
    B5_rew3 + B5_rew4 + B5_rew5 + B5_rew6 + B6_rew1 +
    B6_rew2 + B6_rew3 + B6_rew4 + B6_rew5 +
    B6_rew6 + (
      B1_rew1 + B2_rew2 + B1_rew2 + B2_rew1 + B3_rew3 + B1_rew3 + B2_rew3 + B3_rew1 +
        B3_rew2 + B4_rew4 |
        subjID
    ),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C_reg_all_prev2 = glmer(
  c_choice ~ C1_rew1 + C1_rew2 + C1_rew3 + C1_rew4 + C1_rew5 + C1_rew6 + C2_rew1 +
    C2_rew2 +
    C2_rew3 + C2_rew4 + C2_rew5 + C2_rew6 + C3_rew1 +
    C3_rew2 + C3_rew3 + C3_rew4 + C3_rew5 +
    C3_rew6 + C4_rew1 + C4_rew2 + C4_rew3 + C4_rew4 +
    C4_rew5 + C4_rew6 + C5_rew1 + C5_rew2 +
    C5_rew3 + C5_rew4 + C5_rew5 + C5_rew6 + C6_rew1 +
    C6_rew2 + C6_rew3 + C6_rew4 + C6_rew5 +
    C6_rew6 + (
      C1_rew1 + C2_rew2 + C1_rew2 + C2_rew1 + C3_rew3 + C1_rew3 + C2_rew3 + C3_rew1 +
        C3_rew2 + C4_rew4 |
        subjID
    ),
  data = all_subj_data,
  family = binomial,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
AIC_prev2 = AIC(A_reg_all_prev2) + AIC(B_reg_all_prev2) + AIC(C_reg_all_prev2)
AIC_basic - AIC_prev2

# plot model fits and note best fitting model (lowest AIC)
par(mfrow = c(1, 1))
barplot(
  c(
    AIC_basic,
    AIC_last,
    AIC_prevint,
    AIC_prevchoice,
    AIC_prevrew,
    AIC_prev1,
    AIC_prev2
  ),
  ylab = 'AIC',
  ylim = c(
    .9 * min(
      AIC_basic,
      AIC_last,
      AIC_prevint,
      AIC_prevchoice,
      AIC_prevrew,
      AIC_prev1,
      AIC_prev2
    ),
    1.1 * max(
      AIC_basic,
      AIC_last,
      AIC_prevint,
      AIC_prevchoice,
      AIC_prevrew,
      AIC_prev1,
      AIC_prev2
    )
  ),
  names.arg = c(
    'basic',
    'last',
    'prev int',
    'prev choices',
    'prev rewards',
    'prev mix 2 back',
    'prev mix 3 back'
  ),
  beside = T,
  xpd = F,
  border = NA
)
abline(
  h = min(
    AIC_basic,
    AIC_last,
    AIC_prevint,
    AIC_prevchoice,
    AIC_prevrew,
    AIC_prev1,
    AIC_prev2
  )
)

#### plot beta coefficients for variables in the best fitting model, including estimates of individual subjects ####
#pull out estimates for each person (random + fixed) and overall estimates (fixed effects)
allfx_A = cbind(
  ranef(A_reg_all_prev1)$subjID[, 2] + fixef(A_reg_all_prev1)[2],
  ranef(A_reg_all_prev1)$subjID[, 4] + fixef(A_reg_all_prev1)[3],
  ranef(A_reg_all_prev1)$subjID[, 3] + fixef(A_reg_all_prev1)[9],
  ranef(A_reg_all_prev1)$subjID[, 5] + fixef(A_reg_all_prev1)[8],
  ranef(A_reg_all_prev1)$subjID[, 6] + fixef(A_reg_all_prev1)[16]
)
allfx_B = cbind(
  ranef(B_reg_all_prev1)$subjID[, 2] + fixef(B_reg_all_prev1)[2],
  ranef(B_reg_all_prev1)$subjID[, 4] + fixef(B_reg_all_prev1)[3],
  ranef(B_reg_all_prev1)$subjID[, 3] + fixef(B_reg_all_prev1)[9],
  ranef(B_reg_all_prev1)$subjID[, 5] + fixef(B_reg_all_prev1)[8],
  ranef(B_reg_all_prev1)$subjID[, 6] + fixef(B_reg_all_prev1)[16]
)
allfx_C = cbind(
  ranef(C_reg_all_prev1)$subjID[, 2] + fixef(C_reg_all_prev1)[2],
  ranef(C_reg_all_prev1)$subjID[, 4] + fixef(C_reg_all_prev1)[3],
  ranef(C_reg_all_prev1)$subjID[, 3] + fixef(C_reg_all_prev1)[9],
  ranef(C_reg_all_prev1)$subjID[, 5] + fixef(C_reg_all_prev1)[8],
  ranef(C_reg_all_prev1)$subjID[, 6] + fixef(C_reg_all_prev1)[16]
)
fix_A1_rew1 = mean(fixef(A_reg_all_prev1)[2],
                   fixef(B_reg_all_prev1)[2],
                   fixef(C_reg_all_prev1)[2])
fix_A1_rew2 = mean(fixef(A_reg_all_prev1)[3],
                   fixef(B_reg_all_prev1)[3],
                   fixef(C_reg_all_prev1)[3])
fix_A2_rew2 = mean(fixef(A_reg_all_prev1)[9],
                   fixef(B_reg_all_prev1)[9],
                   fixef(C_reg_all_prev1)[9])
fix_A2_rew1 = mean(fixef(A_reg_all_prev1)[8],
                   fixef(B_reg_all_prev1)[8],
                   fixef(C_reg_all_prev1)[8])
fix_A3_rew3 = mean(fixef(A_reg_all_prev1)[16],
                   fixef(B_reg_all_prev1)[16],
                   fixef(C_reg_all_prev1)[16])
mean_A1_rew1 = rowMeans(cbind(allfx_A[, 1], allfx_B[, 1], allfx_C[, 1]))
mean_A1_rew2 = rowMeans(cbind(allfx_A[, 2], allfx_B[, 2], allfx_C[, 2]))
mean_A2_rew2 = rowMeans(cbind(allfx_A[, 3], allfx_B[, 3], allfx_C[, 3]))
mean_A2_rew1 = rowMeans(cbind(allfx_A[, 4], allfx_B[, 4], allfx_C[, 4]))
mean_A3_rew3 = rowMeans(cbind(allfx_A[, 5], allfx_B[, 5], allfx_C[, 5]))

#plot- black points are estimates for each subject, filled white circle is fixed effect (overall mean)
par(mfrow = c(1, 1))
max_y = max(mean_A1_rew1,
            mean_A1_rew2,
            mean_A2_rew2,
            mean_A2_rew1,
            mean_A3_rew3)
min_y = min(mean_A1_rew1,
            mean_A1_rew2,
            mean_A2_rew2,
            mean_A2_rew1,
            mean_A3_rew3)
plot(
  1:5,
  c(
    mean_A1_rew1[1],
    mean_A2_rew2[1],
    mean_A3_rew3[1],
    mean_A1_rew2[1],
    mean_A2_rew1[1]
  ),
  xlab = '',
  pch = 20,
  ylab = 'estimated beta',
  ylim = c(floor(min_y), ceiling(max_y)),
  axes = F
)
axis(
  1,
  at = c(1, 2, 3, 4, 5),
  labels = c(
    'choice*rew_1',
    'choice*rew_2',
    'choice*rew_3',
    'choice_1*rew_2',
    'choice_2*rew_1'
  )
)
axis(2, at = seq(floor(min_y), ceiling(max_y), 1))
for (subj in 2:length(mean_A1_rew1)) {
  points(
    1:5,
    c(
      mean_A1_rew1[subj],
      mean_A2_rew2[subj],
      mean_A3_rew3[subj],
      mean_A1_rew2[subj],
      mean_A2_rew1[subj]
    ),
    pch = 20
  )
}
points(
  1:5,
  c(
    fix_A1_rew1,
    fix_A2_rew2,
    fix_A3_rew3,
    fix_A1_rew2,
    fix_A2_rew1
  ),
  pch = 21,
  bg = 'white'
)
abline(h = 0, lty = 2, col = 'gray20')
