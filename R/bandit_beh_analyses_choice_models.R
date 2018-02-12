#  using reinforcement hx and value from VBA to predict choice, group differences

#  still need to relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)) to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/")
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

load(file = "bandit1.RData")

##############################
# variables legend
# Trial -- t
# choice_numeric, multinomial_choice -- a(t)
# stay -- a(t)==a(t+1), repetition of a(t) at t+1
# correct_incorrect, reinf -- r(t), credited to a(t)
# value_A_stim, value_B_stim, value_C_stim -- v(t) prior to a(t) and r(t), following r(t-1)
# value_chosen -- value of the action about to be chosen, v(t+1), following r(t)
# value_max -- max(v(t+1)), following r(t) and a(t)
# v_chosen_lag -- value of a(t),  v(t)
# v_chosen_lag_updated -- value of a(t) following r(t), v(a_t,t+1)

########################################
# compare params and fits across groups

params <- as.data.frame(gdf[,c(7,18,43:46)])

##############################
# check correlations

chars <- as.data.frame(bdf[, c("v_chosen_lag_mfx","v_max_lag_mfx", "v_chosen_lag_updated_mfx", "v_ch_diff", "vmaxdiff", "PE_chosen_vba_mfx","PE_chosen_vba_lag", "reinf_n","reinf_n_lag", "h_lag_mfx","stay_p", "stake_n", "stake_n_lag","RT")])

pdf("bandit correlations.pdf", width=14, height=14)
cors <- corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "AOE", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
dev.off()

chars <- as.data.frame(bdf[, c("v_chosen_lag_mfx","v_max_lag_mfx", "v_chosen_lag_updated_mfx", "v_ch_diff", "PE_chosen_vba_mfx","PE_chosen_vba_lag", "reinf_n","reinf_n_lag", "h_lag_mfx","stay_p", "stake_n", "stake_n_lag","RT")])


#################
# choice
# all analyses will be mfx going forward, ditch the prefix

# missed trials
mt <- glmer(
  RT==0 ~ trial_scaled +  reinf_lag +
    Group   +
    (1 | ID),
  family = binomial(),
  data = gdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mt)
car::Anova(mt, type = 'III')
lsm <- lsmeans(mt,"Group")
cld(lsm)

# simplest model without RL
sm0 <- glmer(
  stay ~ stake + trial_scaled + stay_lag * reinf  +  trial_scaled +
    stay_lag *reinf * Group   +
    (1 | ID),
  family = binomial(),
  data = gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sm0)
car::Anova(sm0, type = 'III')
lsm <- lsmeans::lsmeans(sm0, "reinf",by = "Group")
plot(lsm, horiz = F)
#
#
# sm0pre <- glmer(
#   stay ~ stake + trial_scaled + stay_lag * reinf  +  trial_scaled +
#     reinf * Group   +
#     (1 | ID),
#   family = binomial(),
#   data = gdf[gdf$Trial<150,],
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(sm0pre)
# car::Anova(sm0pre, type = 'III')
# lsm <- lsmeans::lsmeans(sm0pre, "reinf",by = "Group")
# plot(lsm, horiz = F)
#
#
# sm0post <-   glmer(
#   stay ~ stake + trial_scaled + stay_lag  * reinf  +  trial_scaled +
#     stay_lag  * reinf * Group   +
#     (1 | ID),
#   family = binomial(),
#   data = gdf[gdf$Trial>150,],
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(sm0post)
# car::Anova(sm0post, type = 'III')
# lsm <- lsmeans::lsmeans(sm0post, "reinf",by = "Group")
# plot(lsm, horiz = F)


# final model w/o group
sm3 <-   glmer(
  stay ~ stake + trial_scaled + reinf * v_max_lag_mfx  + v_ch_diff  + reinf   + reinf * v_ch_diff +
    (1 | ID),
  family = binomial(),
  data = gdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sm3)
car::Anova(sm3)

# impact of current rewards is greater at difficult trials
lsm <- lsmeans::lsmeans(sm3,"reinf", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)

# and after exploitative choices
lsm <- lsmeans::lsmeans(sm3,"reinf", by = c("v_ch_diff"), at = list(v_ch_diff = c(-0.5,0)))
plot(lsm, horiz = F)




# final model before sensitivity analyses, a little over-engineered but fits well:
sm3g <-   glmer(
  stay ~ stake + trial_scaled  + reinf * Group  +
    value_max_vba_mfx * reinf * Group  + v_ch_diff * reinf * Group  +
    (1 | ID),
  family = binomial(),
  data = gdf, #InstEval,
  #   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(sm3g)
car::Anova(sm3g, type = 'III')
plot(allEffects(sm3g))

# # with only chosen value
# sm3gm <-   glmer(
#   stay ~ stake + trial_scaled + v_chosen_lag_mc * stay_lag *Group  + reinf * stay_lag * Group  +
#     (1 | ID),
#   family = binomial(),
#   data = gdf,
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(sm3gm)

# adding group improves model
stargazer(sm3, sm3g, type="html", out="sm3g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


# # alternative with v_chosen
# sm4g <-   glmer(
#   stay ~ stake + trial_scaled + v_chosen_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
#     (1 | ID),
#   family = binomial(),
#   data = gdf,
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(sm4g)
# car::Anova(sm4g, type = 'III')
#
# lsm <- lsmeans::lsmeans(sm4g,"Group", by = c("v_chosen_lag_mfx"), at = list(v_chosen_lag_mfx = c(0,1)))
# plot(lsm, horiz = F)
#
# lsm <- lsmeans::lsmeans(sm4g,"Group", by = c("stay_lag","v_chosen_lag_mfx"), at = list(v_chosen_lag_mfx = c(0,1)))
# plot(lsm, horiz = F)
# stargazer(sm4g, type="html", out="sm4g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))



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


# control for beta and fit -- their interactions with value not surprisingly explain group differences
sm3gBeta <-   glmer(
  stay ~ stake + trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag * Group  +
    v_max_lag_mfx * stay_lag * beta_mfx_data + v_ch_diff * stay_lag * beta_mfx_data + reinf * stay_lag * beta_mfx_data +
    (1 | ID),
  family = binomial(),
  data = gdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sm3gBeta)
car::Anova(sm3gBeta)
sm3gL <-   glmer(
  stay ~ stake + trial_scaled + v_max_lag_mfx * reinf *Group  + v_ch_diff * reinf * Group  +
    v_max_lag_mfx * reinf * L_vba_mfx + v_ch_diff * reinf * L_vba_mfx +
    (1 | ID),
  family = binomial(),
  data = gdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sm3gL)
car::Anova(sm3gL)

# control for demographics and cognition
# demo

sm3g_demo <-   glmer(
  stay ~ stake + trial_scaled + v_max_lag_mfx * reinf *Group  + v_ch_diff * reinf * Group  +
    v_max_lag_mfx * reinf * age_scaled + v_ch_diff * reinf * age_scaled +
    v_max_lag_mfx * reinf * education_scaled + v_ch_diff * reinf * education_scaled +
        (1 | ID),
  family = binomial(),
  data = gdf,
  #   glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
summary(sm3g_demo)
car::Anova(sm3g_demo)



## TO DO with complete data:
# control for EXIT and education: they mostly explain group differences
sm2mfxG_sens <-   glmer(
  stay ~ reinf * stake_lag*Group +
    reinf * stake_lag*exit_scaled +
    reinf * stake_lag*education + reinf*h_mfx_mc +
    v_chosen_lag_mfx_mc*h_mfx_mc*Group +
    v_chosen_lag_mfx_mc*h_mfx_mc*exit_scaled +
    v_chosen_lag_mfx_mc*h_mfx_mc*education +
    stake + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = gdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sm2mfxG_sens)
car::Anova(sm2mfxG_sens)

bdf$stay_lag <- as.factor(bdf$stay_lag)

##################################################################
## archival
#  previous analysis of value of the stimulus about to be chosen:
# m_old <-   glmer(
#   stay ~ reinf * stake_lag + reinf*h_mfx_mc + reinf * Group + value_chosen_vba_mfx*Group*h_mfx_mc + stake + trial_scaled + stay_lag +
#     (1 | ID),
#   family = binomial(),
#   data = bdf,
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(m_old)
# car::Anova(m_old)
# ls_m_old <- lsmeans::lsmeans::lsmeans(m_old,"value_chosen_vba_mfx", by = c("Group","h_mfx_mc"), at = list(value_chosen_vba_mfx = c(0.1,0.9), h_mfx_mc = c(-1,1)))
# plot(ls_m_old, horiz = F)

# # Michael's suggestion: predict choice value
# # current favorite as of 1/11/17
# vm1 <- lmer(
#   value_chosen_vba_mfx ~ reinf * stay_lag * Group +
#   stake  + trial_scaled +
#     (1 | ID),
#   data = gdf)
# summary(vm1)
# car::Anova(vm1)
# plot(allEffects(vm1))
# lsm <- lsmeans::lsmeans(vm1,"Group", by = c("reinf", "stay_lag"))
# plot(lsm, horiz = F)
#
#
# # before we get too excited, control for beta and fit
# vm1betaL <- lmer(
#   value_chosen_vba_mfx ~ reinf * stay_lag * Group +
#     stake  + trial_scaled +
#     beta_mfx_data + L_vba_mfx +
#     (1 | ID),
#   data = gdf)
# summary(vm1betaL)
# car::Anova(vm1betaL)
#
# # account for  vmax: big effects, but the distribution is a problem
# vm2 <- lmer(
#   value_chosen_vba_mfx ~
#     reinf * stay_lag * Group +
#     stake  + trial_scaled +
#     value_max_vba_mfx*stay_lag*Group +
#     (1 | ID),
#   data = gdf)
# summary(vm2)
# car::Anova(vm2)
# plot(allEffects(vm2))
# lsm <- lsmeans::lsmeans(vm2,"reinf", by = c("Group"))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(vm2,"v_max_lag_mfx", by = c("Group"), at = list("v_max_lag_mfx" = c(0,1)))
# plot(lsm, horiz = F)

# does third option interfere a la Noonan?
mNoon1 <- glmer(
  choice_numeric==best_value_option ~ v1*v2 + v1*v3 + v2*v3 + reinf_lag +
    (1 | ID),
  family = binomial(),
  data = gdf[bdf$v_chosen_lag_mfx!=bdf$v3,],
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mNoon1)
car::Anova(mNoon1)
plot(allEffects(mNoon1))

lsm <- lsmeans::lsmeans(mNoon1,"v1", by = c("Group"), at = list("v1" = c(0.5,1)))
plot(lsm, horiz = F)




##############
# replicate in behavioral sample
# almost no missed trials in behavioral study

rsm0pre <- glmer(
  stay ~ trial_scaled + stay_lag  + reinf  +  trial_scaled +
    reinf * Group   +
    (1 | ID),
  family = binomial(),
  data = rdf[rdf$Trial<150,],
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(rsm0pre)
car::Anova(rsm0pre, type = 'III')
lsm <- lsmeans::lsmeans(rsm0pre, "reinf",by = "Group")
plot(lsm, horiz = F)


rsm0post <-   glmer(
  stay ~ trial_scaled + stay_lag  + reinf  +  trial_scaled +
    reinf * Group   +
    (1 | ID),
  family = binomial(),
  data = rdf[rdf$Trial>150,],
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(rsm0post)
car::Anova(rsm0post, type = 'III')
lsm <- lsmeans::lsmeans(rsm0post, "reinf",by = "Group")
plot(lsm, horiz = F)


rsm0 <-   glmer(
  stay ~  trial_scaled + stay_lag  + reinf  +  trial_scaled +
    reinf * Group   +
    (1 | ID),
  family = binomial(),
  data = rdf[rdf$Trial<150,],
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(rsm0)
car::Anova(rsm0, type = 'III')


# replicate basic reinforcement effects -- all are the same except the value * previous stay interaction
r_sm3g <-   glmer(
  stay ~ trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag  + reinf * Group  +
    (1 | ID),
  family = binomial(),
  data = rdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(r_sm3g)
car::Anova(r_sm3g)
# the impact of value is greater after switches than after stays
lsm <- lsmeans::lsmeans(r_sm3g, "v_max_lag_mfx",by = "Group", at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
# but the impact of current reward is bigger after stays than after switches
lsm <- lsmeans::lsmeans(r_sm3g, "reinf",by = "Group")
plot(lsm, horiz = F)

# without chosen value
r_sm3gm <-   glmer(
  stay ~ trial_scaled + v_max_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
    (1 | ID),
  family = binomial(),
  data = rdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(r_sm3gm)



# findings are a bit different from the fMRI sample, but one has to keep in mind the differences in task difficulty and cognitive level
r_sm4g <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *Group  + reinf * stay_lag  + reinf * Group  +
    (1 | ID),
  family = binomial(),
  data = rdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(r_sm4g)
car::Anova(r_sm4g)
lsm <- lsmeans::lsmeans(r_sm4g, "reinf",by = "stay_lag")
plot(lsm, horiz = F)

stargazer(r_sm4g, type="html", out="r_sm4g.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))

# by lethality -- still need group on one participant
r_sm4gl <-   glmer(
  stay ~ trial_scaled + v_chosen_lag_mfx * stay_lag *GroupLeth  + reinf * stay_lag  + reinf * GroupLeth  +
    (1 | ID),
  family = binomial(),
  data = rdf[rdf$Group12467!=5,],
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(r_sm4gl)
car::Anova(r_sm4gl)


# first, behavioral version on scanned subjects


# replicate basic reinforcement effects -- all are the same except the value * previous stay interaction
sr_sm3g <-   glmer(
  stay ~ trial_scaled + v_max_lag_mfx * stay_lag *Group  + v_ch_diff * stay_lag * Group  + reinf * stay_lag  + reinf * Group  +
    (stay_lag + trial_scaled | ID),
  family = binomial(),
  data = sdf,
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
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
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
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
    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(sr_sm4gl)
car::Anova(sr_sm4gl)
lsm <- lsmeans::lsmeans(sr_sm4gl, "reinf",by = "GroupLeth")
plot(lsm, horiz = F)





save(list = ls(all.names = TRUE), file = "bandit2choice.RData")
