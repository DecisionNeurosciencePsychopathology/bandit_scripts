#  relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
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

load(file = "bandit1.RData")

# or to continue where I left off:
# load(file = "bandit2rt.RData")



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
# any particular times in the task when we get more outliers?  Not particularly
ggplot(gdf[gdf$RT>2000,],aes(Trial)) + geom_histogram()
ggplot(rdf[rdf$RT>4000,],aes(Trial)) + geom_histogram()
ggplot(sdf[sdf$RT>4000,],aes(Trial)) + geom_histogram()


ggplot(gdf[gdf$RT>0,],aes(x = Trial, y = RT)) + geom_line() + facet_wrap(~ID)
ggplot(rdf[rdf$RT>0 & rdf$RT<4000,],aes(RT)) + geom_histogram() + facet_wrap(~ID)
ggplot(sdf[sdf$RT>0 & sdf$RT<4000,],aes(y = (RT))) + geom_histogram() + facet_wrap(~ID)

hist(gdf$RT[gdf$RT<4000 & gdf$RT>0],1000)
hist(rdf$RT[rdf$RT<4000 & rdf$RT>0],1000)

gdf$invRT <- 1000/gdf$RT
rdf$invRT <- 1000/rdf$RT
sdf$invRT <- 1000/sdf$RT

gdf$invRT_lag <- 1000/gdf$RT_lag
rdf$invRT_lag <- 1000/rdf$RT_lag
sdf$invRT_lag <- 1000/sdf$RT_lag


gdist <- psych::describe(gdf$RT, IQR = TRUE, quant = c(.9, .95,.99))
rdist <- psych::describe(rdf$RT, IQR = TRUE, quant = c(.9,.95,.99))
sdist <- psych::describe(sdf$RT, IQR = TRUE, quant = c(.9,.95,.99))
# upper_gdf <- as.numeric(gdist$Q0.99)
# upper_rdf <- as.numeric(rdist$Q0.99)
# upper_sdf <- as.numeric(sdist$Q0.99)
upper <- 4000
upper_gdf <- upper
upper_rdf <- upper
upper_sdf <- upper

lower <- 200


# basic model, no interactions, looking for value effect


s11_rt000 <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag +
                        abs(PE_chosen_vba_lag)+
                        (1 | ID),
                      data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
summary(s11_rt000)
vif.lme(s11_rt000)

s11_rt00 <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag + stay_lag +
                          abs(PE_chosen_vba_lag)+
                          (1 | ID),
                        data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
summary(s11_rt00)
vif.lme(s11_rt00)

s11_rt00v <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*stay_lag +
                         v_max_lag_mfx +
                          abs(PE_chosen_vba_lag)+
                         (1 | ID),
                       data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
summary(s11_rt00v)
vif.lme(s11_rt00v)


anova(s11_rt000,s11_rt00)


# beh sample
s11_rt0 <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag +
                           v_max_lag_mfx*Group +
                           abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# first exp in fMRI sample 
s12_rt0 <- update(s11_rt0, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf,])
# fMRI exp
s22_rt0 <- update(s11_rt0, . ~ . + stake_lag, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf,])

stargazer(s11_rt0, s12_rt0, s22_rt0,  type="html", out="log_rt_no_interaction_check.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))


# Study 1, sample 1 (beh sample)
s11_rt <- lme4::lmer(log(RT) ~ log(RT_lag) + trial_scaled + stay_lag +
                           reinf_lag*Group +  
                           v_max_lag_mfx*Group +
                           abs(PE_chosen_vba_lag)*Group +
                           (1 | ID),
                         data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# Study 1, sample 2 (first exp in fMRI sample)
s12_rt <- update(s11_rt, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf,])
# Study 2, sample 2 (fMRI exp)
s22_rt <- update(s11_rt, . ~ . + stake_lag, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf,])
stargazer(s11_rt, s12_rt, s22_rt,  type="html", out="log_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))
# 
lsm <- lsmeans::lsmeans(s11_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s12_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s22_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
# 

# also with entropy
h <- ".~. - v_max_lag_mfx*Group + v_max_lag2_mfx*Group + h_lag_mfx_mc*Group"

s11_rtH <- update(s11_rt, h)
s12_rtH <- update(s12_rt, h)
s22_rtH <- update(s22_rt, h)
stargazer(s11_rtH, s12_rtH, s22_rtH,  type="html", out="h_log_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# 
##########
# sensitivity analyses

# 1/RT
s11_inv_rt <- update(s11_rt,invRT~.+invRT_lag-log(RT_lag))
s12_inv_rt <- update(s12_rt,invRT~.+invRT_lag-log(RT_lag))
s22_inv_rt <- update(s22_rt,invRT~.+invRT_lag-log(RT_lag))

stargazer(s11_inv_rt, s12_inv_rt, s22_inv_rt,  type="html", out="inv_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# demo

demog <- ".~.+scale(age)*v_max_lag_mfx + scale(age)*reinf_lag + scale(age)*abs(PE_chosen_vba_lag) +
            scale(education)*v_max_lag_mfx + scale(education)*reinf_lag + scale(education)*abs(PE_chosen_vba_lag) +
            sex*v_max_lag_mfx + sex*reinf_lag + sex*abs(PE_chosen_vba_lag)"
s11_rt_dem <- update(s11_rt, demog)
s12_rt_dem <- update(s12_rt,demog)
s22_rt_dem <- update(s22_rt, demog)
stargazer(s11_rt_dem, s12_rt_dem, s22_rt_dem,  type="html", out="demog_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))
# exit
exit <- ".~.+scale(EXITtot)*v_max_lag_mfx + scale(EXITtot)*reinf_lag + scale(EXITtot)*abs(PE_chosen_vba_lag)"
s11_rt_exit <- update(s11_rt_dem, exit)
s12_rt_exit <- update(s12_rt_dem,exit)
s22_rt_exit <- update(s22_rt_dem, exit)

stargazer(s11_rt_exit, s12_rt_exit, s22_rt_exit,  type="html", out="exit_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))


# DRS
DRS <- ".~.+scale(TOTA_MDRS)*v_max_lag_mfx + scale(TOTA_MDRS)*reinf_lag + scale(TOTA_MDRS)*abs(PE_chosen_vba_lag)"
s11_rt_DRS <- update(s11_rt_dem, DRS)
s12_rt_DRS <- update(s12_rt_dem,DRS)
s22_rt_DRS <- update(s22_rt_dem, DRS)

stargazer(s11_rt_DRS, s12_rt_DRS, s22_rt_DRS,  type="html", out="DRS_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# model fit
fit <- ".~.+scale(L_vba_mfx)*v_max_lag_mfx + scale(L_vba_mfx)*reinf_lag + scale(L_vba_mfx)*abs(PE_chosen_vba_lag)"
s11_rt_fit <- update(s11_rt_dem, fit)
s12_rt_fit <- update(s12_rt_dem,fit)
s22_rt_fit <- update(s22_rt_dem, fit)

stargazer(s11_rt_fit, s12_rt_fit, s22_rt_fit,  type="html", out="fit_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# lethality analyses

s11_rt_leth <- lme4::lmer(log(RT) ~ log(RT_lag) + trial_scaled + stay_lag +
                       reinf_lag*GroupLeth +  
                       v_max_lag_mfx*GroupLeth +
                       abs(PE_chosen_vba_lag)*GroupLeth +
                       (1 | ID),
                     data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# Study 1, sample 2 (first exp in fMRI sample)
s12_rt_leth <- update(s11_rt_leth, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf,])
# Study 2, sample 2 (fMRI exp)
s22_rt_leth <- update(s11_rt_leth, . ~ . + stake_lag, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf,])
stargazer(s11_rt_leth, s12_rt_leth, s22_rt_leth,  type="html", out="leth_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

lsm <- lsmeans::lsmeans(s11_rt_leth,"GroupLeth", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s12_rt_leth,"GroupLeth", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s22_rt_leth,"GroupLeth", by = c("v_max_lag_mfx"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)




# collinearity checks
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }

vif.lme(s11_rt)
vif.lme(s12_rt)
vif.lme(s22_rt)


save(list = ls(all.names = TRUE), file = "bandit2rt.RData")
