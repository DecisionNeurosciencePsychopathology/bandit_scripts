#  related ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_fMRI_df_with_PE/")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(ggbiplot)
library(corrplot)
library(lsmeans)
library(factoextra)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)
library(multcompView)
library(stargazer)
library(psych)

trial_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_fMRI_df_with_PE/bandit_df1.csv")
# View(trial_df)
sub_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_fMRI_df_with_PE/bandit_df2.csv")
# View(sub_df)
sub_df = sub_df %>% as_tibble %>% arrange(ID)

sub_df$group1245 <- as.factor(sub_df$group1245)
sub_df$group12467 <- as.factor(sub_df$group12467)

# quality check: 2 subjects repeatedly pressed the same button
sub_df$bad <- NA
sub_df$bad <- sub_df$ID == 206270 | sub_df$ID == 210100

table(sub_df$bad, sub_df$group1245)
# check missing data
missing_ind_chars = aggr(
  sub_df,
  col = mdc(1:2),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(sub_df),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# all missingness <8%, could impute

# sample characteristics: looks reasonable
chars <- as.data.frame(sub_df[, c(9:13,2:3,18,43:46)])
c1 <-
  compareGroups(
    chars,
    y = sub_df$group1245,
    bivar = TRUE,
    include.miss = FALSE
  )
t1 <-
  createTable(
    c1,
    # hide = c(sex = "FEMALE", list(race = c(
    #   "WHITE", "ASIAN PACIFIC"
    # ))),
    hide.no = 0,
    digits = 1,
    show.n = TRUE
  )
export2html(t1, "t_bandit_beh_scan_by_group.html")

# coarse overview of behavior
hist(sub_df$spont_switch_err, breaks = 50)
hist(sub_df$prob_switch_err, breaks = 50)
hist(sub_df$erratic_spont, breaks = 50)
hist(sub_df$error_NOS)


# summary(m1 <- lm(spont_switch_err ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot, data = sub_df))
# anova(m1)
# summary(m2 <-
#           lm(
#             error_NOS ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot,
#             data = sub_df
#           ))
# anova(m2)
# summary(m3 <-
#           glm.nb(spont_switch_err ~ group1245 +  WTAR_SCALED_SCORE + EXITtot, data = sub_df))
# car::Anova(m3, type = 'III')


# merge trial-by-trial and subject-level data
bdf <- merge(trial_df, sub_df)

summary(bdf)

bdf$Group <-
  dplyr::recode(
    bdf$group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
contrasts(bdf$Group) <- contr.treatment(levels(bdf$Group),
                                        base = which(levels(bdf$Group) == 'Attempters'))

sub_df$Group <-
  dplyr::recode(
    sub_df$group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
contrasts(sub_df$Group) <- contr.treatment(levels(sub_df$Group),
                                           base = which(levels(sub_df$Group) == 'Attempters'))


bdf$stake <- as.factor(bdf$stake)
bdf$reward <- as.factor(bdf$reward)
bdf$comp_trials <- as.factor(bdf$comp_trials)
bdf$mystery_trials <- as.factor(bdf$mystery_trials)
bdf$reinf <- as.factor(bdf$correct_incorrect)
bdf$choice_numeric[bdf$choice_numeric==0] <- NA
bdf$choice_numeric <- as.factor(bdf$choice_numeric)
bdf$best_value_option <- as.factor(bdf$best_value_option)
bdf$choice_numeric[bdf$choice_numeric == 0] <- NA
bdf$iq_scaled <-
  scale(bdf$WTAR_SCALED_SCORE, center = TRUE, scale = TRUE)[, 1]
bdf$exit_scaled <-
  scale(bdf$EXITtot, center = TRUE, scale = TRUE)[, 1]
bdf$reinf_n <- as.numeric(bdf$correct_incorrect)
# add multinom analyses looking at how magnitude of reward influences choice probability (nnet package)

bdf = bdf %>% as_tibble %>% arrange(ID, Trial)
bdf = bdf %>% group_by(ID) %>%
  mutate(
    reinf_lag = lag(reinf),
    stake_lag = lag(stake),
    value_A_lag = lag(value_A_stim),
    value_B_lag = lag(value_B_stim),
    value_C_lag = lag(value_C_stim),
    value_A_lead = lead(value_A_stim),
    value_B_lead = lead(value_B_stim),
    value_C_lead = lead(value_C_stim),
    choice_lag = lag(multinomial_choice),
    choice_num_lag = lag(choice_numeric),
    choice_num_lead = lead(choice_numeric),
    v_chosen_lag = lag(value_chosen),
    v_max_lag = lag(value_max),
    v_max_lag = lag(value_max),
    v_max_lag_mfx = lag(value_max_vba_mfx),
    v_max_lag2_mfx = lag(value_max_vba_mfx,2),
    PE_chosen_vba_lag = lag(PE_chosen_vba_mfx),
    v_chosen_lag_mfx  = lag(value_chosen_vba_mfx),
    h_lag = lag(H),
    h_lag_mfx = lag(H_vba_mfx),
    RT_lag = lag(RT)
  ) %>% ungroup()
bdf$stay <- bdf$choice_numeric == bdf$choice_num_lead
bdf$stay_p <- NA
bdf$stay_p[bdf$stay==TRUE] <- 1
bdf$stay_p[bdf$stay==FALSE] <- 0
bdf = bdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)) %>% ungroup()
bdf$stay <- as.factor(bdf$stay)
bdf$stay_lag <- as.factor(bdf$stay_lag)

bdf$trial_scaled <- scale(bdf$Trial)
bdf$past_rew <-
  dplyr::recode(bdf$reinf_lag, `0` = "After omission", `1` = "After reward")
bdf$correct_incorrect <- as.factor(bdf$correct_incorrect)

bdf$choiceA <- NA
bdf$choiceB <- NA
bdf$choiceC <- NA
bdf$choiceA[bdf$multinomial_choice=="A"] <- 1
bdf$choiceB[bdf$multinomial_choice=="B"] <- 1
bdf$choiceC[bdf$multinomial_choice=="C"] <- 1
bdf$choiceA[bdf$multinomial_choice!="A"] <- 0
bdf$choiceB[bdf$multinomial_choice!="B"] <- 0
bdf$choiceC[bdf$multinomial_choice!="C"] <- 0

bdf$v_chosen_lag_updated_mfx <- NA
bdf$v_chosen_lag_updated_mfx[which(bdf$choice_numeric==1)] <- bdf$value_A_stim_vba_mfx[which(bdf$choice_numeric==1)]
bdf$v_chosen_lag_updated_mfx[which(bdf$choice_numeric==2)] <- bdf$value_B_stim_vba_mfx[which(bdf$choice_numeric==2)]
bdf$v_chosen_lag_updated_mfx[which(bdf$choice_numeric==3)] <- bdf$value_C_stim_vba_mfx[which(bdf$choice_numeric==3)]

bdf$v1 <- NA
bdf$v2 <- NA
bdf$v3 <- NA
values <- cbind(bdf$value_A_stim_vba_mfx,bdf$value_B_stim_vba_mfx,bdf$value_C_stim_vba_mfx)
bdf$v1 <- apply(values,1,max)
bdf$v3 <- apply(values,1,min)
bdf$v2 <- apply(values,1,median)



##############################
# variables legend ###########
##############################
# Trial -- t
# choice_numeric, multinomial_choice -- a(t)
# stay -- a(t)==a(t+1), repetition of a(t) at t+1
# correct_incorrect, reinf -- r(t), credited to a(t)
# value_A_stim, value_B_stim, value_C_stim -- v(t) prior to a(t) and r(t), following r(t-1)
# value_chosen -- value of the action about to be chosen, v(t+1), following r(t)
# value_max -- max(v(t+1)), following r(t) and a(t)
# v_chosen_lag -- value of a(t),  v(t)
# v_chosen_lag_updated -- value of a(t) following r(t), v(a_t,t+1)
# test regression
bdf$v_chosen_lag_mc <- scale(bdf$v_chosen_lag, center = TRUE, scale = TRUE)
bdf$h_lag_mc <- scale(bdf$h_lag)

bdf$v_chosen_lag_mfx_mc <-  scale(bdf$v_chosen_lag_mfx)
bdf$h_lag_mfx_mc <- scale(bdf$h_lag_mfx)
bdf$h_mc <- scale(bdf$H)
bdf$h_mfx_mc <- scale(bdf$H_vba_mfx)

bdf$stake_n <- as.numeric(bdf$stake)
bdf$stake_n_lag <- as.numeric(bdf$stake_lag)
bdf$reinf_n_lag <- as.numeric(bdf$reinf_lag)

# what about the difference between Vmax and Vchosen
bdf$v_ch_diff <- bdf$v_chosen_lag_mfx - bdf$v_max_lag_mfx

bdf$v_ch_logr <- log(bdf$v_chosen_lag_mfx_mc/bdf$v_max_lag_mfx)

# exclude subjects who pressed one button repeatedly
gdf <- bdf[!bdf$bad,]

# check correlations



chars <- as.data.frame(bdf[, c("v_chosen_lag_mfx","v_max_lag_mfx", "v_chosen_lag_updated_mfx", "v_ch_diff", "PE_chosen_vba_mfx","PE_chosen_vba_lag", "reinf_n","reinf_n_lag", "h_lag_mfx","stay_p", "stake_n", "stake_n_lag","RT")])

# pdf("reg correlations.pdf", width=14, height=14)
cors <- corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

# Michelle to check all histograms for herself


corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
# dev.off()


# all analyses will be mfx going forward, ditch the prefix

# final model w/o group
sm3 <-   glmer(
  stay ~ stake + trial_scaled + v_max_lag_mfx*stay_lag  + v_ch_diff*stay_lag  + reinf * stay_lag  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = gdf,
  nAGQ = 0)
summary(sm3)
car::Anova(sm3)
#
# sm3h <-   glmer(
#   stay ~ stake + trial_scaled + I(v_max_lag_mfx/h_lag_mfx)*Group  + v_ch_diff*Group  + reinf * stay_lag * Group +
#     (stay_lag + trial_scaled | ID),
#   family = binomial(),
#   data = gdf,
#   nAGQ = 0)
# summary(sm3h)
# car::Anova(sm3h)
#

# # the impact of value is greater after switches than after stays
# lsm <- lsmeans::lsmeans(sm3g, "v_chosen_lag_mfx_mc",by = "stay_lag", at = list(v_chosen_lag_mfx_mc = c(0,1)))
# plot(lsm, horiz = F)
# # but the impact of current reward is bigger after stays than after switches
# lsm <- lsmeans::lsmeans(sm3g, "reinf",by = "stay_lag")
# plot(lsm, horiz = F)



# final model before sensitivity analyses, a little over-engineered but fits well:
sm3g <-   glmer(
  stay ~ stake + trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = gdf,
  nAGQ = 0)
summary(sm3g)
car::Anova(sm3g, type = 'III')

stargazer(sm3, sm3g, type="html", out="sm3g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

#
# # alternative with v_chosen
# sm4g <-   glmer(
#   stay ~ stake + trial_scaled + v_chosen_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
#     (stay_lag + trial_scaled | ID),
#   family = binomial(),
#   data = gdf,
#   nAGQ = 0)
# summary(sm4g)
# car::Anova(sm4g, type = 'III')
#
# lsm <- lsmeans::lsmeans(sm4g,"Group", by = c("v_chosen_lag_mfx"), at = list(v_chosen_lag_mfx = c(0,1)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(sm4g,"Group", by = c("stay_lag","v_chosen_lag_mfx"), at = list(v_chosen_lag_mfx = c(0,1)))
# plot(lsm, horiz = F)
# stargazer(sm4g, type="html", out="sm4g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
#

# adding group improves model massively
anova(sm3,sm3g)

lsm <- lsmeans::lsmeans(sm3g,"Group", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
print(CLD <- cld(lsm))
lsm <- lsmeans::lsmeans(sm3g,"Group", by = c("v_ch_diff"), at = list(v_ch_diff = c(-.5,0)))
plot(lsm, horiz = F)
print(CLD <- cld(lsm))

# there is a weaker effect of max value in attempters after a switch
lsm <- lsmeans::lsmeans(sm3g,"Group", by = c("stay_lag","v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
CLD <- cld(lsm)
lsm <- lsmeans::lsmeans(sm3g,"Group", by = c("stay_lag","v_ch_diff"), at = list(v_ch_diff = c(0,-1)))
plot(lsm, horiz = F)


# control for beta and fit -- main effects of those do not explain group differences
sm3gBetaL <-   glmer(
  stay ~ reinf * stake * trial_scaled + reinf * Group + v_chosen_lag_mfx_mc * Group + reinf * stay_lag + v_chosen_lag_mfx_mc * stay_lag * Group + beta_mfx_data + L_vba_mfx +
    (stay_lag | ID),
  family = binomial(),
  data = gdf,
  nAGQ = 0)
summary(sm3gBetaL)
car::Anova(sm3gBetaL)
anova(sm3g,sm3gBetaL)

## TO DO with complete data:
# control for EXIT and education: they mostly explain group differences
# sm2mfxG_sens <-   glmer(
#   stay ~ reinf * stake_lag*Group +
#     reinf * stake_lag*exit_scaled +
#     reinf * stake_lag*education + reinf*h_mfx_mc +
#     v_chosen_lag_mfx_mc*h_mfx_mc*Group +
#     v_chosen_lag_mfx_mc*h_mfx_mc*exit_scaled +
#     v_chosen_lag_mfx_mc*h_mfx_mc*education +
#     stake + trial_scaled + stay_lag +
#     (1 | ID),
#   family = binomial(),
#   data = gdf,
#   nAGQ = 0)
# summary(sm2mfxG_sens)
# car::Anova(sm2mfxG_sens)

bdf$stay_lag <- as.factor(bdf$stay_lag)

## archival
#  previous analysis of value of the stimulus about to be chosen:
# m_old <-   glmer(
#   stay ~ reinf * stake_lag + reinf*h_mfx_mc + reinf * Group + value_chosen_vba_mfx*Group*h_mfx_mc + stake + trial_scaled + stay_lag +
#     (1 | ID),
#   family = binomial(),
#   data = bdf,
#   nAGQ = 0)
# summary(m_old)
# car::Anova(m_old)
# ls_m_old <- lsmeans::lsmeans::lsmeans(m_old,"value_chosen_vba_mfx", by = c("Group","h_mfx_mc"), at = list(value_chosen_vba_mfx = c(0.1,0.9), h_mfx_mc = c(-1,1)))
# plot(ls_m_old, horiz = F)

# Michael's suggestion: predict choice value
# current favorite as of 1/11/17
<<<<<<< Updated upstream:R/bandit_beh_analyses_final_models.R
# vm1 <- lmer(
#   value_chosen_vba_mfx ~ reinf * stay_lag * Group + reinf_lag * Group +
#   stake  + trial_scaled +
#     (1 | ID),
#   data = gdf)
# summary(vm1)
# car::Anova(vm1)
# plot(allEffects(vm1))
# lsm <- lsmeans::lsmeans(vm1,"reinf", by = c("Group"))
# plot(lsm, horiz = F)
#
#
# # before we get too excited, control for beta and fit
# vm1betaL <- lmer(
#   value_chosen_vba_mfx ~ reinf * stay_lag * Group + reinf_lag * Group +
#     stake  + trial_scaled +
#     beta_mfx_data + L_vba_mfx +
#     (1 | ID),
#   data = gdf)
# summary(vm1betaL)
# car::Anova(vm1betaL)
#
# # account for previously chosen value or vmax: this model is getting too complicated too quickly
# vm2 <- lmer(
#   value_chosen_vba_mfx ~
#     reinf * stay_lag * Group + reinf_lag * Group +
#     stake  + trial_scaled +
#     v_max_lag_mfx*Group +
#     (1 | ID),
#   data = gdf)
# summary(vm2)
# car::Anova(vm2)
# plot(allEffects(vm2))
# lsm <- lsmeans::lsmeans(vm2,"reinf", by = c("Group"))
# plot(lsm, horiz = F)
#
=======
vm1 <- lmer(
  value_chosen_vba_mfx ~ reinf * stay_lag * Group +
  stake  + trial_scaled +
    (1 | ID),
  data = gdf)
summary(vm1)
car::Anova(vm1)
plot(allEffects(vm1))
lsm <- lsmeans::lsmeans(vm1,"Group", by = c("reinf", "stay_lag"))
plot(lsm, horiz = F)


# before we get too excited, control for beta and fit
vm1betaL <- lmer(
  value_chosen_vba_mfx ~ reinf * stay_lag * Group +
    stake  + trial_scaled +
    beta_mfx_data + L_vba_mfx +
    (1 | ID),
  data = gdf)
summary(vm1betaL)
car::Anova(vm1betaL)

# account for  vmax: big effects, but the distribution is a problem
vm2 <- lmer(
  value_chosen_vba_mfx ~
    reinf * stay_lag * Group +
    stake  + trial_scaled +
    value_max_vba_mfx*stay_lag*Group +
    (1 | ID),
  data = gdf)
summary(vm2)
car::Anova(vm2)
plot(allEffects(vm2))
lsm <- lsmeans::lsmeans(vm2,"reinf", by = c("Group"))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(vm2,"v_max_lag_mfx", by = c("Group"), at = list("v_max_lag_mfx" = c(0,1)))
plot(lsm, horiz = F)

# does third option interfere a la Noonan?
mNoon1 <- glmer(
  choice_numeric==best_value_option ~ v1*v2 + v1*v3 + v2*v3 + reinf_lag +
    (1 | ID),
  family = binomial(),
  data = gdf[bdf$v_chosen_lag_mfx!=bdf$v3,],
  nAGQ = 0)
summary(mNoon1)
car::Anova(mNoon1)
plot(allEffects(mNoon1))

lsm <- lsmeans::lsmeans(mNoon1,"v1", by = c("Group"), at = list("v1" = c(0.5,1)))
plot(lsm, horiz = F)


>>>>>>> Stashed changes:R/old/bandit_beh_analyses_final_models.R



# RTs:
# the best simple model (NB: current stake has no effect):
rt2 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                        reinf_lag + stake_n_lag + stay_lag + trial_scaled +
                        v_max_lag_mfx +
                        (1 | ID),
                      data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt2)
car::Anova(rt2)
lsm <- lsmeans(rt2, "stake_n_lag", by = "stake")
plot(lsm, horiz = F)

# best model with value and without PE
rt2chosen <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                              reinf_lag + stake_n_lag + stay_lag + trial_scaled +
                              v_max_lag_mfx + v_ch_diff +
                              (1 | ID),
                            data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt2chosen)
car::Anova(rt2chosen)
anova(rt2,rt2chosen)

# add PE: seems like a pretty good model
rt2chosenPE <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                              reinf_lag + stake_n_lag + stay_lag + trial_scaled +
                              v_max_lag_mfx + v_ch_diff +
                              abs(PE_chosen_vba_lag) +
                              (1 | ID),
                            data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt2chosenPE)
car::Anova(rt2chosenPE)
plot(allEffects(rt2chosenPE))
anova(rt2chosen,rt2chosenPE)

# what about future choices?  They don't seem to be reflected in prior RT

#
# pdf(file = "rt by ee.pdf",
#     width = 10,
#     height = 6)

ggplot(gdf[gdf$RT>0,],aes(x = v_ch_diff, y = log(RT))) + stat_smooth(method = "gam") #+ facet_wrap(~ID)
# ggplot(gdf[gdf$RT>0,],aes(x = v_max_lag_mfx, y = log(RT))) + geom_point() + facet_wrap(~ID)

# dev.off()

# deep breath
# add group

rt_g1 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                              reinf_lag*Group + stake_lag*Group + stay_lag*Group + trial_scaled +
                          v_max_lag_mfx*Group + v_ch_diff*Group +
                              abs(PE_chosen_vba_lag)*Group +
                              (1 | ID),
                            data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt_g1)
car::Anova(rt_g1, type = 'III')
plot(allEffects(rt_g1))
#########################################################
# this model with higher-order interactions fits better
# current favorite - 01/17/18
# but results appear less consistent between samples than with rt_g1
rt_g2 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*Group + stake_lag*Group + stay_lag*Group + trial_scaled +
                          stay_lag*v_max_lag_mfx*Group + stay_lag*v_ch_diff*Group +
                          abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt_g2)
car::Anova(rt_g2, type = 'III')
plot(allEffects(rt_g2))
# anova(rt_g1,rt_g2)
#########################################################

rt_g1star <- lme4::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*Group + stake_lag*Group + stay_lag*Group + trial_scaled +
                          v_max_lag_mfx*Group + v_ch_diff*Group +
                          abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = gdf[gdf$RT>0 & gdf$RT<4000,])

# remove current choice completely out of the model
rt_g3 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*Group + stake_lag*Group + stay_lag + trial_scaled +
                          v_max_lag_mfx*Group + v_ch_diff*Group +
                          abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = gdf[gdf$RT>0 & gdf$RT<4000,])
summary(rt_g3)
car::Anova(rt_g3, type = 'III')
plot(allEffects(rt_g1))


# NB unfortunately stargazer is not compatible with lmerTest
stargazer(rt_g1star, type="html", out="rt_g1.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))
stargazer(car::Anova(rt_g1star), type="html", out="anova_rt_g1.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

lsm <- lsmeans::lsmeans(rt_g1,"Group", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(rt_g1,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)


print(CLD <- cld(lsm))
lsm <- lsmeans::lsmeans(rt_g1,"PE_chosen_vba_lag", by = c("Group"), at = list(PE_chosen_vba_lag = c(-1,0,1)))
plot(lsm, horiz = F)


# # graphical sanity checks on lagged variables, because order_by=Trial does not seem to work here
# id <-  unique(bdf$ID)[33]
# i <- bdf$ID == id
# d <- 'NA'
# d$trial <- bdf$Trial[i]
# d$stay <- bdf$stay[i]
# d$stay_lag <- bdf$stay[i]
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$value_chosen_vba_mfx[i], color = "v_chosen")) + geom_line(aes(y = bdf$v_chosen_lag_mfx[i], color = "v_chosen_lag"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$value_max_vba_mfx[i], color = "v_max")) + geom_line(aes(y = bdf$v_max_lag_mfx[i], color = "v_max_lag"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$H_vba_mfx[i], color = "v_chosen_f")) + geom_line(aes(y = bdf$h_lag_mfx[i], color = "v_chosen_lag_f"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_point(aes(y = bdf$stay[i], color = "stay")) + geom_point(aes(y = bdf$stay_lag[i], color = "stay_lag"))


#ggplot(na.omit(bdf[, c(1:17, 42:dim(bdf)[2])]), aes(x = v_chosen_lag_mfx_mc, y = stay_p, color = Group)) + stat_smooth(

ggplot(bdf, aes(x = v_chosen_lag_mfx_mc, y = stay_p, color = Group)) + stat_smooth(
  method = "glm",
  method.args = list(family = "binomial"),
  se = TRUE
) + theme_bw(base_size = 20) + labs(x = "Chosen value, mean-centered", y = "Probability of staying with the same choice")


# RL model parameters across groups
ggplot(data = sub_df, aes(y = alpha_win_mfx_data, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = alpha_loss_mfx_data, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = decay_mfx_data, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = beta_mfx_data, x = Group, color = Group)) + geom_boxplot() + geom_jitter() + stat_smooth(method = "lm", formula = beta ~ Group, se=T,
                                                                                                              level=0.95)
ggplot(data = sub_df, aes(y = beta_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

ggplot(data = sub_df, aes(y = L_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

plot(sub_df$alpha_win_df2, sub_df$alpha_win_mfx_data)
plot(sub_df$alpha_win, sub_df$alpha_win_vba_mfx)
plot(sub_df$decay, sub_df$decay_vba_mfx)
plot(sub_df$beta, sub_df$beta_vba_mfx)

pm1 <-
  manova(cbind(alpha_loss_mfx_data, alpha_win_mfx_data, decay_mfx_data, beta_mfx_data) ~ Group, data = sub_df)
summary(pm1)
anova(pm1)

# pm1mfx <-
#   manova(
#     cbind(
#       alpha_win_vba_mfx,
#       alpha_loss_vba_mfx,
#       decay_vba_mfx,
#       beta_vba_mfx
#     ) ~ Group,
#     data = sub_df)
# summary(pm1mfx)
# anova(pm1mfx)
#
#
# pm2mfx <- lm(alpha_win_vba_mfx ~ Group, data = sub_df)
# summary(pm2mfx)
# anova(pm2mfx)
#
# pm3mfx <- lm(alpha_loss_vba_mfx ~ Group, data = sub_df)
# summary(pm3mfx)
# anova(pm3mfx)
#
# pm4mfx <- lm(decay_vba_mfx ~ Group, data = sub_df)
# summary(pm4mfx)
# anova(pm4mfx) #*
#
# pm5mfx <- lm(beta_vba_mfx ~ Group, data = sub_df)
# summary(pm5mfx)
# anova(pm5mfx)
# ggplot(data = sub_df, aes(y = beta_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
#
# pm6mfx <- lm(L_vba_mfx ~ Group, data = sub_df)
# summary(pm6mfx)
# anova(pm6mfx)
# ggplot(data = sub_df, aes(y = L_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()



# plot trialwise choice probability
pdf(file = "attempters exploring.pdf",
    width = 10,
    height = 6)
ggplot(na.omit(bdf), aes(x = Trial, y = stay_p, color = Group)) + stat_smooth(method =
                                                                                "auto") + theme_bw(base_size = 20) + ylab("Probability of staying with the same choice") +
  facet_wrap(~ past_rew) #geom_jitter(alpha=0.2) +
dev.off()



bdf2 <-
  bdf %>% gather(key = "process",
                 value = "z",
                 v_chosen_lag_mfx_mc,
                 h_lag_mfx_mc)

ggplot(na.omit(bdf2[bdf2$ID == 217008, c(1:24, 81:dim(bdf2)[2])]), aes(x = Trial, y = z, color = multinomial_choice)) + geom_jitter(size = 8) + facet_wrap(~
                                                                                                                                                    process, ncol = 1)
pdf(file = "value entropy choice by subject.pdf",
    width = 100,
    height = 100)
ggplot(na.omit(bdf2[, c(1:24, 81:dim(bdf2)[2])]), aes(x = Trial, y = z, color = multinomial_choice)) + geom_jitter(size = .5) + facet_wrap(~
                                                                                                                                    ID + process)
dev.off()
save(list = ls(all.names = TRUE), file = "bandit1.RData")

## proper replication using full behavioral data ________________________________________________________________________________________________________________________________________
# read in data form larger behavioral sample


beh_trial_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_behav_df_with_PE/bandit_df1.csv")
# View(beh_trial_df)
beh_sub_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_behav_df_with_PE/bandit_df2.csv")
# View(beh_sub_df)

beh_ids <- unique(beh_sub_df$ID)
# Josh told me that some peopel from BSocial were inadvertently included: exclude them, turns out to be just one person
bsocials <-
  as.integer(c(
    219084,
    211858,
    211008,
    211973,
    212298,
    220686,
    220622,
    220590,
    220521,
    220562
  ))
beh_sub_df <-
  beh_sub_df[!is.element(beh_sub_df$ID, bsocials),]


#check ID overlap
scan_ids <- unique(bdf$ID)
repeaters <- intersect(beh_ids, scan_ids)
beh_sub_df$scanned <-
  is.element(beh_sub_df$ID, repeaters)

# identify subjects who pressed the same button >10 times
bad_ids <-
  c(206270,
    209460,
    210548,
    212385,
    211705,
    213227,
    215644,
    218207,
    29829,
    881091)
# add individual characteristics from Laura's DB

beh_sub_df$bad <-
  is.element(beh_sub_df$ID, bad_ids)
beh_sub_df$AnxietyLifetime <-
  as.factor(beh_sub_df$AnxietyLifetime)
beh_sub_df$SubstanceLifetime <-
  as.factor(beh_sub_df$SubstanceLifetime)

# I know that a few controls were erroneously coded for substance/anxiety, correct
beh_sub_df$AnxietyLifetime[beh_sub_df$group1245 == 1] <-
  NA
beh_sub_df$SubstanceLifetime[beh_sub_df$group1245 == 1] <-
  NA

# remove missing WTARs
beh_sub_df$WTAR_SCALED_SCORE[beh_sub_df$WTAR_SCALED_SCORE >
                               200] <- NA
# select all good behavioral subjects, including scanned, get group characteristics
c <- beh_sub_df[!beh_sub_df$bad,]
chars <- as.data.frame(c[, c(9:14, 25:40)])
c2 <-
  compareGroups(
    chars,
    y = c$group1245,
    bivar = TRUE,
    include.miss = FALSE
  )
t2 <-
  createTable(
    c2,
    hide = c(sex = "FEMALE", list(race = c(
      "WHITE", "ASIAN PACIFIC"
    ))),
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t2, "beh_t_bandit_beh_by_group.html")

# only unique subjects who have not been scanned
c2 <-
  beh_sub_df[!is.element(beh_sub_df$ID, repeaters) &
               !beh_sub_df$bad,]
chars <- as.data.frame(c2[, c(9:14, 25:40)])
c3 <-
  compareGroups(
    chars,
    y = c2$group1245,
    bivar = TRUE,
    include.miss = FALSE
  )
t3 <-
  createTable(c3,
              hide.no = 0,
              digits = 0,
              show.n = TRUE)
export2html(t3, "unique_beh_t_bandit_beh_by_group.html")

# try excluding older controls/depressed -- still
old_contr_depressed <-
  beh_sub_df$age > 75 & beh_sub_df$group1245 < 4
c4 <-
  beh_sub_df[!is.element(beh_sub_df$ID, repeaters) &
               !beh_sub_df$bad &
               !old_contr_depressed,]
chars <- as.data.frame(c4[, c(9:14, 18,19,43:46,2:3)])
c5 <-
  compareGroups(
    chars,
    y = c4$group1245,
    bivar = TRUE,
    include.miss = FALSE
  )
t5 <-
  createTable(c5,
              hide.no = 0,
              digits = 1,
              show.n = TRUE)
export2html(t5, "age_equated_unique_beh_t_bandit_beh_by_group.html")

beh_sub_df$Group <-
  dplyr::recode(
    beh_sub_df$group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
beh_sub_df$Group <- as.factor(beh_sub_df$Group)
contrasts(beh_sub_df$Group) <-
  contr.treatment(levels(beh_sub_df$Group),
                  base = which(levels(beh_sub_df$Group) == 'Attempters'))

# check missingness

beh_missing_ind_chars = aggr(
  beh_sub_df[colSums(!is.na(beh_sub_df)) > 0],
  col = mdc(1:2),
  numbers = TRUE,
  sortVars = TRUE,
  labels = names(sub_df),
  cex.axis = .7,
  gap = 3,
  ylab = c("Proportion of missingness", "Missingness Pattern")
)

# only subjects who have been scanned
c9 <-
  beh_sub_df[is.element(beh_sub_df$ID, repeaters) &
               !beh_sub_df$bad,]
chars <- as.data.frame(c9[, c(9:14, 25:40)])
c10 <-
  compareGroups(
    chars,
    y = c9$group1245,
    bivar = TRUE,
    include.miss = FALSE
  )
t10 <-
  createTable(c10,
              hide.no = 0,
              digits = 0,
              show.n = TRUE)
export2html(t10, "repeaters_beh_t_bandit_beh_by_group.html")

# rpm1 <-
#   manova(cbind(alpha_win_vba_mfx, alpha_loss_vba_mfx, decay_vba_mfx, beta_vba_mfx) ~ Group, data = beh_sub_df)
# summary(rpm1)
# anova(rpm1)
#
# rpm2 <- lm(alpha_win_vba_mfx ~ Group, data = beh_sub_df)
# summary(rpm2)
# anova(rpm2)
#
# rpm3 <- lm(alpha_loss_vba_mfx ~ Group, data = beh_sub_df)
# summary(rpm3)
# anova(rpm3)
#
# rpm4 <- lm(decay_vba_mfx ~ Group, data = beh_sub_df)
# summary(rpm4)
# anova(rpm4) #.
# ggplot(data = beh_sub_df, aes(y = decay_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
#
# rpm5 <- lm(beta_vba_mfx ~ Group, data = beh_sub_df)
# summary(rpm5)
# anova(rpm5)
# ggplot(data = beh_sub_df, aes(y = beta, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
# ls_rpm5 <- lsmeans::lsmeans(rpm5, "Group")
# cld(ls_rpm5)
#
# rpm6 <- lm(L_vba_mfx ~ Group, data = beh_sub_df)
# summary(rpm6)
# anova(rpm6)
# ggplot(data = beh_sub_df, aes(y = L, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
# ls_rpm6 <- lsmeans::lsmeans(rpm6, "Group")
# cld(ls_rpm6)



# let's try the unique sample first
rdf <- merge(beh_trial_df, c4)
View(rdf)

# first assessments on repeaters
sdf <- merge(beh_trial_df, c9)
View(sdf)
# summary(rdf)
# rdf$bad[is.element(rdf$ID,exclude)] <- TRUE
# rdf$bad[!is.element(rdf$ID,exclude)] <- FALSE

# table(rdf$bad,rdf$Group)
# have 195 left after excluding 6 bad subjects
# rdf <- rdf[!rdf$bad,]

rdf$Group1245 <- as.factor(rdf$group1245)
rdf$Group12467 <- as.factor(rdf$group12467)
rdf$reinf <- as.factor(rdf$correct_incorrect)
rdf$choice_numeric <- as.factor(rdf$choice_numeric)
rdf$choice_numeric[rdf$choice_numeric == 0] <- NA
rdf$age_scaled <- scale(rdf$age)[, 1]
# get_lags
rdf = rdf %>% as_tibble %>% arrange(ID, Trial)
rdf$trial_scaled <- scale(rdf$Trial)
# calculate value of chosen stimulus following the update at t
# get lags and leads
rdf = rdf %>% group_by(ID) %>%
  mutate(
    RT_lag = lag(RT),
    reinf_lag = lag(reinf),
    value_A_lag = lag(value_A_stim),
    value_B_lag = lag(value_B_stim),
    value_C_lag = lag(value_C_stim),
    value_A_lead = lead(value_A_stim),
    value_B_lead = lead(value_B_stim),
    value_C_lead = lead(value_C_stim),
    choice_lag = lag(multinomial_choice),
    choice_num_lag = lag(choice_numeric),
    choice_num_lead = lead(choice_numeric),
    v_chosen_lag = lag(value_chosen),
    v_max_lag = lag(value_max),
    v_max_lag = lag(value_max),
    v_max_lag_mfx = lag(value_max_vba_mfx),
    v_max_lag2_mfx = lag(value_max_vba_mfx,2),
    PE_chosen_vba_lag = lag(PE_chosen_vba_mfx),
        v_chosen_lag_mfx  = lag(value_chosen_vba_mfx),
    h_lag = lag(H),
    h_lag_mfx = lag(H_vba_mfx)
  ) %>% ungroup()
rdf$stay <- rdf$choice_numeric == rdf$choice_num_lead
rdf = rdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)) %>% ungroup()
rdf$stay <- as.factor(rdf$stay)
rdf$stay_lag <- as.factor(rdf$stay_lag)

rdf$stay_p <- NA
rdf$stay_p[rdf$stay] <- 1
rdf$stay_p[!rdf$stay] <- 0
rdf$past_rew <-
  recode(rdf$reinf_lag, `0` = "After omission", `1` = "After reward")
rdf$reinf_n <- as.numeric(rdf$correct_incorrect)

rdf$correct_incorrect <- as.factor(rdf$correct_incorrect)



rdf$choiceA <- NA
rdf$choiceB <- NA
rdf$choiceC <- NA
rdf$choiceA[rdf$multinomial_choice == "A"] <- 1
rdf$choiceB[rdf$multinomial_choice == "B"] <- 1
rdf$choiceC[rdf$multinomial_choice == "C"] <- 1
rdf$choiceA[rdf$multinomial_choice != "A"] <- 0
rdf$choiceB[rdf$multinomial_choice != "B"] <- 0
rdf$choiceC[rdf$multinomial_choice != "C"] <- 0


# value of chosen stimulus incorporating subsequent reward, v(a[t]) after r(t)
rdf$v_chosen_lag_updated <- NA
rdf$v_chosen_lag_updated[which(rdf$choice_numeric==1)] <- rdf$value_A_lead[which(rdf$choice_numeric==1)]
rdf$v_chosen_lag_updated[which(rdf$choice_numeric==2)] <- rdf$value_B_lead[which(rdf$choice_numeric==2)]
rdf$v_chosen_lag_updated[which(rdf$choice_numeric==3)] <- rdf$value_C_lead[which(rdf$choice_numeric==3)]

rdf$v_chosen_lag_mc <- scale(rdf$v_chosen_lag)
rdf$h_lag_mc <- scale(rdf$h_lag)

rdf$v_chosen_lag_mfx_mc <-  scale(rdf$v_chosen_lag_mfx)
rdf$h_lag_mfx_mc <- scale(rdf$h_lag_mfx)
rdf$h_mc <- scale(rdf$H)
rdf$h_mfx_mc <- scale(rdf$H_vba_mfx)
rdf$v_ch_diff <- rdf$v_chosen_lag_mfx - rdf$v_max_lag_mfx


# make sure lags are correctly aligned
lag_test <- rdf[,c(1:3,6:13,122:dim(rdf)[2])]
View(lag_test)


rdf$Group <-
  dplyr::recode(
    rdf$Group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
contrasts(rdf$Group) <-
  contr.treatment(levels(rdf$Group),
                  base = which(levels(rdf$Group) == 'Attempters'))
rdf$GroupLeth <-
  dplyr::recode(
    rdf$Group12467,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `6` = "LL Attempters",
    `7` = "HL Attempters"
  )
contrasts(rdf$GroupLeth) <-
  contr.treatment(levels(rdf$GroupLeth),
                  base = which(levels(rdf$GroupLeth) == 'HL Attempters'))

# replicate basic reinforcement effects -- all are the same except the value * previous stay interaction
r_sm3g <-   glmer(
  stay ~ trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag  + reinf * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = rdf,
  nAGQ = 0)
summary(r_sm3g)
car::Anova(r_sm3g)
# the impact of value is greater after switches than after stays
lsm <- lsmeans::lsmeans(r_sm3, "v_chosen_lag_mfx_mc",by = "stay_lag", at = list(v_chosen_lag_mfx_mc = c(0,1)))
plot(lsm, horiz = F)
# but the impact of current reward is bigger after stays than after switches
lsm <- lsmeans::lsmeans(r_sm3, "reinf",by = "stay_lag")
plot(lsm, horiz = F)

# findings are a bit different from the fMRI sample, but one has to keep in mind the differences in task difficulty and cognitive level
r_sm4g <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = rdf,
  nAGQ = 0)
summary(r_sm4g)
car::Anova(r_sm4g)
lsm <- lsmeans::lsmeans(r_sm4g, "reinf",by = "stay_lag")
plot(lsm, horiz = F)

stargazer(r_sm4g, type="html", out="r_sm4g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# by lethality -- still need group on one participant
r_sm4gl <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *GroupLeth  + reinf * stay_lag  + reinf * GroupLeth  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = rdf[rdf$Group12467!=5,],
  nAGQ = 0)
summary(r_sm4gl)
car::Anova(r_sm4gl)



# replicate RT model: age removed for now
rrt_g1 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          abs(PE_chosen_vba_lag) +
                          reinf_lag*Group + stay_lag*Group + v_ch_diff*Group +
                          trial_scaled + v_max_lag_mfx*Group + abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = rdf[rdf$RT>0 & rdf$RT<6000,])
summary(rrt_g1)
car::Anova(rrt_g1)

rrt_g1star <- lme4::lmer(log(RT)  ~ log(RT_lag) +
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag*Group + v_ch_diff*Group +
                           trial_scaled + v_max_lag_mfx*Group + abs(PE_chosen_vba_lag)*Group +
                               (1 | ID),
                         data = rdf[rdf$RT>0 & rdf$RT<6000,])
stargazer(rrt_g1star, type="html", out="rrt_g1.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


plot(allEffects(rrt_g1))
lsm <- lsmeans::lsmeans(rrt_g1,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)

lsm <- lsmeans::lsmeans(rrt_g1,"reinf_lag", by = "Group")
plot(lsm, horiz = F)

# remove the interaction with last reinforcement since the horizon was shorter in the more difficult study
rrt_g2 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag + v_ch_diff*Group*stay_lag +
                           trial_scaled + v_max_lag_mfx*Group*stay_lag + abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = rdf[rdf$RT>0 & rdf$RT<6000,])
summary(rrt_g2)
car::Anova(rrt_g2, type = 'III')
plot(allEffects(rrt_g2))

anova(rrt_g1,rrt_g2)
# plot 3-way interactions
lsm <- lsmeans::lsmeans(rrt_g2,"v_max_lag_mfx", by = c("stay_lag", "Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(rrt_g1,"age", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1), age = c(50,80)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(rrt_g1,"age", by = c("PE_chosen_vba_lag"), at = list(PE_chosen_vba_lag = c(-1,0,1), age = c(50,80)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(rrt_g1,"reinf_lag", by = "age", at = list(age = c(50,80)))
# plot(lsm, horiz = F)
#

rrt_g1l <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           reinf_lag*GroupLeth + stay_lag*GroupLeth +
                           trial_scaled + v_max_lag_mfx*GroupLeth + abs(PE_chosen_vba_lag)*GroupLeth +
                           (1 | ID),
                         data = rdf[rdf$RT>0 & rdf$RT<6000 & rdf$Group12467!=5,])
summary(rrt_g1l)
car::Anova(rrt_g1l)
lsm <- lsmeans::lsmeans(rt_g1,"GroupLeth", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)

# all the same for first administration in scanner subjects

sdf$Group1245 <- as.factor(sdf$group1245)
sdf$Group12467 <- as.factor(sdf$group12467)
sdf$reinf <- as.factor(sdf$correct_incorrect)
sdf$choice_numeric <- as.factor(sdf$choice_numeric)
sdf$choice_numeric[sdf$choice_numeric == 0] <- NA
sdf$age_scaled <- scale(sdf$age)[, 1]
# get_lags
sdf = sdf %>% as_tibble %>% arrange(ID, Trial)
sdf$trial_scaled <- scale(sdf$Trial)
# calculate value of chosen stimulus following the update at t
# get lags and leads
sdf = sdf %>% group_by(ID) %>%
  mutate(
    RT_lag = lag(RT),
    reinf_lag = lag(reinf),
    value_A_lag = lag(value_A_stim),
    value_B_lag = lag(value_B_stim),
    value_C_lag = lag(value_C_stim),
    value_A_lead = lead(value_A_stim),
    value_B_lead = lead(value_B_stim),
    value_C_lead = lead(value_C_stim),
    choice_lag = lag(multinomial_choice),
    choice_num_lag = lag(choice_numeric),
    choice_num_lead = lead(choice_numeric),
    v_chosen_lag = lag(value_chosen),
    v_max_lag = lag(value_max),
    v_max_lag = lag(value_max),
    v_max_lag_mfx = lag(value_max_vba_mfx),
    v_max_lag2_mfx = lag(value_max_vba_mfx,2),
    PE_chosen_vba_lag = lag(PE_chosen_vba_mfx),
    v_chosen_lag_mfx  = lag(value_chosen_vba_mfx),
    h_lag = lag(H),
    h_lag_mfx = lag(H_vba_mfx)
  ) %>% ungroup()
sdf$stay <- sdf$choice_numeric == sdf$choice_num_lead
sdf = sdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)) %>% ungroup()
sdf$stay <- as.factor(sdf$stay)
sdf$stay_lag <- as.factor(sdf$stay_lag)

sdf$stay_p <- NA
sdf$stay_p[sdf$stay] <- 1
sdf$stay_p[!sdf$stay] <- 0
sdf$past_rew <-
  recode(sdf$reinf_lag, `0` = "After omission", `1` = "After reward")
sdf$reinf_n <- as.numeric(sdf$correct_incorrect)

sdf$correct_incorrect <- as.factor(sdf$correct_incorrect)



sdf$choiceA <- NA
sdf$choiceB <- NA
sdf$choiceC <- NA
sdf$choiceA[sdf$multinomial_choice == "A"] <- 1
sdf$choiceB[sdf$multinomial_choice == "B"] <- 1
sdf$choiceC[sdf$multinomial_choice == "C"] <- 1
sdf$choiceA[sdf$multinomial_choice != "A"] <- 0
sdf$choiceB[sdf$multinomial_choice != "B"] <- 0
sdf$choiceC[sdf$multinomial_choice != "C"] <- 0


# value of chosen stimulus incorporating subsequent reward, v(a[t]) after r(t)
sdf$v_chosen_lag_updated <- NA
sdf$v_chosen_lag_updated[which(sdf$choice_numeric==1)] <- sdf$value_A_lead[which(sdf$choice_numeric==1)]
sdf$v_chosen_lag_updated[which(sdf$choice_numeric==2)] <- sdf$value_B_lead[which(sdf$choice_numeric==2)]
sdf$v_chosen_lag_updated[which(sdf$choice_numeric==3)] <- sdf$value_C_lead[which(sdf$choice_numeric==3)]

sdf$v_chosen_lag_mc <- scale(sdf$v_chosen_lag)
sdf$h_lag_mc <- scale(sdf$h_lag)

sdf$v_chosen_lag_mfx_mc <-  scale(sdf$v_chosen_lag_mfx)
sdf$h_lag_mfx_mc <- scale(sdf$h_lag_mfx)
sdf$h_mc <- scale(sdf$H)
sdf$h_mfx_mc <- scale(sdf$H_vba_mfx)
sdf$v_ch_diff <- sdf$v_chosen_lag_mfx - sdf$v_max_lag_mfx


# make sure lags are correctly aligned
lag_test <- sdf[,c(1:3,6:13,122:dim(sdf)[2])]
View(lag_test)


sdf$Group <-
  dplyr::recode(
    sdf$Group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
contrasts(sdf$Group) <-
  contr.treatment(levels(sdf$Group),
                  base = which(levels(sdf$Group) == 'Attempters'))
sdf$GroupLeth <-
  dplyr::recode(
    sdf$Group12467,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `6` = "LL Attempters",
    `7` = "HL Attempters"
  )
contrasts(sdf$GroupLeth) <-
  contr.treatment(levels(sdf$GroupLeth),
                  base = which(levels(sdf$GroupLeth) == 'HL Attempters'))

# replicate basic reinforcement effects -- all are the same except the value * previous stay interaction
sr_sm3g <-   glmer(
  stay ~ trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag  + reinf * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = sdf,
  nAGQ = 0)
summary(sr_sm3g)
car::Anova(sr_sm3g)
# the impact of value is greater after switches than after stays
lsm <- lsmeans::lsmeans(sr_sm3, "v_chosen_lag_mfx_mc",by = "stay_lag", at = list(v_chosen_lag_mfx_mc = c(0,1)))
plot(lsm, horiz = F)
# but the impact of current reward is bigger after stays than after switches
lsm <- lsmeans::lsmeans(sr_sm3, "reinf",by = "stay_lag")
plot(lsm, horiz = F)

# findings are a bit different from the fMRI sample, but one has to keep in mind the differences in task difficulty and cognitive level
sr_sm4g <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = sdf,
  nAGQ = 0)
summary(sr_sm4g)
car::Anova(sr_sm4g)
lsm <- lsmeans::lsmeans(sr_sm4g, "reinf",by = "Group")
plot(lsm, horiz = F)
CLD <- cld(lsm)
stargazer(sr_sm4g, type="html", out="sr_sm4g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# by lethality -- still need group on one participant
sr_sm4gl <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *GroupLeth  + reinf * stay_lag  + reinf * GroupLeth  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = sdf[sdf$Group12467!=5,],
  nAGQ = 0)
summary(sr_sm4gl)
car::Anova(sr_sm4gl)
lsm <- lsmeans::lsmeans(sr_sm4gl, "reinf",by = "GroupLeth")
plot(lsm, horiz = F)



# replicate RT model: age removed for now
srrt_g1 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag*Group + v_ch_diff*Group +
                           trial_scaled + v_max_lag_mfx*Group + abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = sdf[sdf$RT>0 & sdf$RT<6000,])
summary(srrt_g1)
car::Anova(srrt_g1)

srrt_g1star <- lme4::lmer(log(RT)  ~ log(RT_lag) +
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag*Group + v_ch_diff*Group +
                           trial_scaled + v_max_lag_mfx*Group + abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = sdf[sdf$RT>0 & sdf$RT<6000,])
stargazer(srrt_g1star, type="html", out="srrt_g1.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


plot(allEffects(srrt_g1))
lsm <- lsmeans::lsmeans(srrt_g1,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)

lsm <- lsmeans::lsmeans(srrt_g1,"reinf_lag", by = "Group")
plot(lsm, horiz = F)

# remove the interaction with last reinforcement since the horizon was shorter in the more difficult study
srrt_g2 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag + v_ch_diff*Group*stay_lag +
                           trial_scaled + v_max_lag_mfx*Group*stay_lag + abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = sdf[sdf$RT>0 & sdf$RT<6000,])
summary(srrt_g2)
car::Anova(srrt_g2, type = 'III')
plot(allEffects(srrt_g2))

anova(srrt_g1,srrt_g2)
# plot 3-way interactions
lsm <- lsmeans::lsmeans(srrt_g2,"v_max_lag_mfx", by = c("stay_lag", "Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(srrt_g1,"age", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1), age = c(50,80)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(srrt_g1,"age", by = c("PE_chosen_vba_lag"), at = list(PE_chosen_vba_lag = c(-1,0,1), age = c(50,80)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(srrt_g1,"reinf_lag", by = "age", at = list(age = c(50,80)))
# plot(lsm, horiz = F)
#

srrt_g1l <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                            reinf_lag*GroupLeth + stay_lag*GroupLeth +
                            trial_scaled + v_max_lag_mfx*GroupLeth + abs(PE_chosen_vba_lag)*GroupLeth +
                            (1 | ID),
                          data = sdf[sdf$RT>0 & sdf$RT<6000 & sdf$Group12467!=5,])
summary(srrt_g1l)
car::Anova(srrt_g1l)
lsm <- lsmeans::lsmeans(rt_g1,"GroupLeth", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)


save(list = ls(all.names = TRUE), file = "bandit2.RData")
# in case this script needs to be extended:
# load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit2.RData")
