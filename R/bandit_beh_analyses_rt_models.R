#  relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
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


##############
# RTs: 

# check RT timecourses for every subject
# ggplot(gdf[gdf$RT>0,],aes(x = Trial, y = (RT))) + geom_line() + facet_wrap(~ID)
# ggplot(rdf[rdf$RT>0 & rdf$RT<10000,],aes(x = Trial, y = (RT))) + geom_line() + facet_wrap(~ID)
# ggplot(rdf[rdf$RT>0 & rdf$RT<10000,],aes(x = Trial, y = log(RT))) + stat_smooth(aes(x = Trial, y = RT), method = "gam")

# ggplot(gdf[gdf$RT>0,],aes(x = v_max_lag_mfx, y = log(RT))) + geom_point() + facet_wrap(~ID)

# censor outlier RTs following Radcliff, 1993
describe(gdf$RT)
describe(rdf$RT)
hist(gdf$RT[gdf$RT<10000 & gdf$RT>0],1000)
hist(rdf$RT[rdf$RT<10000 & rdf$RT>0],1000)

# fmri sample: censor 99th percentile, 2575
# behavioral, self-paced sample: censor 90th percentile, 2365

###############
# v_chosen
# 
# rt_g0 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
#                           reinf_lag*stay_lag + reinf_lag + stay_lag + stake_lag + trial_scaled + 
#                           stay_lag*v_max_lag_mfx + 
#                           abs(PE_chosen_vba_lag) + 
#                           (1 | ID),
#                         data = gdf[gdf$RT>0 & gdf$RT<4000,])
# summary(rt_g0)
# 
# 
# with vmaxdiff = V1 - V2 - V3, fit is poorer


rt_g1 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*stay_lag + reinf_lag*Group + stay_lag*Group + stake_lag*Group + trial_scaled +
                          stay_lag*v_max_lag_mfx + v_max_lag_mfx*Group +
                          abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = gdf[gdf$RT>250 & gdf$RT<2575,])

rt_g1s <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*stay_lag + reinf_lag*Group + stay_lag*Group + stake_lag*Group + trial_scaled +
                          stay_lag*v_max_lag_mfx + v_max_lag_mfx*Group +
                          abs(PE_chosen_vba_lag)*Group +
                          (1 | ID),
                        data = gdf[gdf$RT>250 & gdf$RT<2575,])

summary(rt_g1s)
car::Anova(rt_g1, type = 'III')
plot(allEffects(rt_g1))
anova(rt_g1s,rt_g1)
#######################################
# replicate RT model: age removed for now
rrt_g0 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                          reinf_lag*stay_lag + reinf_lag + stay_lag +  trial_scaled + 
                          stay_lag*v_max_lag_mfx + 
                          abs(PE_chosen_vba_lag) + 
                          (1 | ID),
                         data = rdf[rdf$RT>250 & rdf$RT<3210,])
summary(rrt_g0)


rrt_g1 <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           reinf_lag*stay_lag + trial_scaled + stay_lag*v_max_lag_mfx + abs(PE_chosen_vba_lag) + 
                           reinf_lag*Group +  
                           v_max_lag_mfx*Group + 
                           (1 | ID),
                         data = rdf[rdf$RT>100 & rdf$RT<3210,])
summary(rrt_g1)
car::Anova(rrt_g1)
anova(rrt_g0,rrt_g1)
lsm <- lsmeans::lsmeans(rrt_g1,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
plot(allEffects(rrt_g1))
# the age findings are interesting: slowing to rewards is accentuated, but slowing to PEs and low vmax are reduced
rrt_g1a <- lmerTest::lmer(log(RT)  ~ log(RT_lag) +
                           reinf_lag*stay_lag + trial_scaled + stay_lag*v_max_lag_mfx + 
                           reinf_lag*Group + stay_lag*Group + 
                           v_max_lag_mfx*Group + 
                           abs(PE_chosen_vba_lag)*Group +                            
                           reinf_lag*age + stay_lag*age + 
                           v_max_lag_mfx*age + 
                           abs(PE_chosen_vba_lag)*age + 
                           (1 | ID),
                          data = rdf[rdf$RT>100 & rdf$RT<3210,])
summary(rrt_g1a)
car::Anova(rrt_g1a)
#########################################################

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



rrt_g1star <- lme4::lmer(log(RT)  ~ log(RT_lag) + 
                           abs(PE_chosen_vba_lag) +
                           reinf_lag*Group + stay_lag*Group + v_ch_diff*Group +
                           trial_scaled + v_max_lag_mfx*Group + abs(PE_chosen_vba_lag)*Group + 
                               (1 | ID),
                         data = rdf[rdf$RT>100 & rdf$RT<3210,])
stargazer(rrt_g1star, type="html", out="rrt_g1.htm", digits = 2,single.row=TRUE, star.cutoffs = c(0.05, 0.01, 0.001))


plot(allEffects(rrt_g1))
lsm <- lsmeans::lsmeans(rrt_g1,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)

lsm <- lsmeans::lsmeans(rrt_g1,"reinf_lag", by = "Group")
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


save(list = ls(all.names = TRUE), file = "bandit2rt.RData")
