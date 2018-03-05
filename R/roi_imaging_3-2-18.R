#  relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/roi")
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

gdf$invRT <- 1000/gdf$RT
rdf$invRT <- 1000/rdf$RT
sdf$invRT <- 1000/sdf$RT

gdf$invRT_lag <- 1000/gdf$RT_lag
rdf$invRT_lag <- 1000/rdf$RT_lag
sdf$invRT_lag <- 1000/sdf$RT_lag

gdist <- psych::describe(gdf$RT, IQR = TRUE, quant = c(.9, .95,.99))
rdist <- psych::describe(rdf$RT, IQR = TRUE, quant = c(.9,.95,.99))
sdist <- psych::describe(sdf$RT, IQR = TRUE, quant = c(.9,.95,.99))
upper <- 4000
upper_gdf <- upper
upper_rdf <- upper
upper_sdf <- upper

lower <- 200

roi_data=read.csv('roi_data_3-1-18.csv')
roi_data$ID=roi_data$Subject
roi_gdf=merge(gdf,roi_data,by="ID")

####RT analyses####
#no ROI
no_roi_R <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                        v_max_lag_mfx*Group +
                        abs(PE_chosen_vba_lag)*Group + stake_lag + 
                        (1 | ID),
                      data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(no_roi_R)
stargazer(no_roi_R,  type="html", out="no_roi_RT.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#vmPFC
vmpfc_reinf <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group*vmPFC +
                         v_max_lag_mfx*Group +
                         abs(PE_chosen_vba_lag)*Group + stake_lag + 
                         (1 | ID),
                       data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_reinf)
stargazer(vmpfc_reinf,  type="html", out="vmpfc_reinf.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_vmaxlag <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                            v_max_lag_mfx*Group*vmPFC +
                            abs(PE_chosen_vba_lag)*Group + stake_lag + 
                            (1 | ID),
                          data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_vmaxlag)
stargazer(vmpfc_vmaxlag,  type="html", out="vmpfc_vmaxlag.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_PE <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                              v_max_lag_mfx +
                              abs(PE_chosen_vba_lag)*Group*vmPFC + stake_lag + 
                              (1 | ID),
                            data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_PE)
stargazer(vmpfc_PE,  type="html", out="vmpfc_PE.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#R striatum
rstr_reinf <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group*R_Str +
                            v_max_lag_mfx*Group +
                            abs(PE_chosen_vba_lag)*Group + stake_lag + 
                            (1 | ID),
                          data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(rstr_reinf)
stargazer(rstr_reinf,  type="html", out="rstr_reinf.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

rstr_vmaxlag <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                              v_max_lag_mfx*Group*R_Str +
                              abs(PE_chosen_vba_lag)*Group + stake_lag + 
                              (1 | ID),
                            data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(rstr_vmaxlag)
stargazer(rstr_vmaxlag,  type="html", out="rstr_vmaxlag.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

rstr_PE <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                         v_max_lag_mfx +
                         abs(PE_chosen_vba_lag)*Group*R_Str + stake_lag + 
                         (1 | ID),
                       data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(rstr_PE)
stargazer(rstr_PE,  type="html", out="rstr_PE.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#L striatum
lstr_reinf <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group*L_Str +
                           v_max_lag_mfx*Group +
                           abs(PE_chosen_vba_lag)*Group + stake_lag + 
                           (1 | ID),
                         data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(lstr_reinf)
stargazer(lstr_reinf,  type="html", out="lstr_reinf.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

lstr_vmaxlag <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                             v_max_lag_mfx*Group*L_Str +
                             abs(PE_chosen_vba_lag)*Group + stake_lag + 
                             (1 | ID),
                           data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(lstr_vmaxlag)
stargazer(lstr_vmaxlag,  type="html", out="lstr_vmaxlag.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

lstr_PE <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*Group +
                        v_max_lag_mfx +
                        abs(PE_chosen_vba_lag)*Group*L_Str + stake_lag + 
                        (1 | ID),
                      data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(lstr_PE)
stargazer(lstr_PE,  type="html", out="lstr_PE.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

####lethality analyses####
#vmPFC, lethality
vmpfc_leth__reinf <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*GroupLeth*vmPFC +
                            v_max_lag_mfx*GroupLeth +
                            abs(PE_chosen_vba_lag)*GroupLeth + stake_lag + 
                            (1 | ID),
                          data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_leth__reinf)
stargazer(vmpfc_leth__reinf,  type="html", out="vmpfc_leth__reinf.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_leth__vmaxlag <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*GroupLeth +
                              v_max_lag_mfx*GroupLeth*vmPFC +
                              abs(PE_chosen_vba_lag)*GroupLeth + stake_lag + 
                              (1 | ID),
                            data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_leth__vmaxlag)
stargazer(vmpfc_leth__vmaxlag,  type="html", out="vmpfc_leth__vmaxlag.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_leth__PE <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag*GroupLeth +
                         v_max_lag_mfx +
                         abs(PE_chosen_vba_lag)*GroupLeth*vmPFC + stake_lag + 
                         (1 | ID),
                       data = roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper_gdf,])
summary(vmpfc_leth__PE)
stargazer(vmpfc_leth__PE,  type="html", out="vmpfc_leth__PE.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

####choice data####
library(lme4)

#no roi
no_roi_C <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(no_roi_C)
stargazer(no_roi_C,  type="html", out="no_roi_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#vmpfc
vmpfc_C <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group*vmPFC  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(vmpfc_C)
stargazer(vmpfc_C,  type="html", out="vmPFC_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_C_red <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group  + stay_lag  * reinf * vmPFC+  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(vmpfc_C_red)
stargazer(vmpfc_C_red,  type="html", out="vmPFC_choice_reduced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

roi_gdf_norein=subset(roi_gdf,reinf==0)
roi_gdf_rein=subset(roi_gdf,reinf==1)

vmpfc_C_noreinf <-   glmer(
  stay ~  trial_scaled + stay_lag  * Group*vmPFC  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf_norein,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(vmpfc_C_noreinf)
stargazer(vmpfc_C_noreinf,  type="html", out="vmPFC_choice_nonreinforced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

vmpfc_C_reinf <-   glmer(
  stay ~  trial_scaled + stay_lag  * Group*vmPFC  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf_rein,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(vmpfc_C_reinf)
stargazer(vmpfc_C_reinf,  type="html", out="vmPFC_choice_reinforced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#R striatum
rstr_C <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group*R_Str  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(rstr_C)
stargazer(rstr_C,  type="html", out="R_striatum_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

rstr_C_red <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group  + stay_lag  * reinf * R_Str+  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(rstr_C_red)
stargazer(rstr_C_red,  type="html", out="R_striatum_choice_reduced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#L striatum
lstr_C <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group*L_Str  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(lstr_C)
stargazer(lstr_C,  type="html", out="L_striatum_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

lstr_C_red <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group  + stay_lag  * reinf * L_Str+  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(lstr_C_red)
stargazer(lstr_C_red,  type="html", out="L_striatum_choice_reduced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#all
allroi_C <-   glmer(
  stay ~  trial_scaled + stay_lag*reinf*Group*vmPFC + stay_lag*reinf*Group*L_Str + stay_lag*reinf*Group*R_Str +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(allroi_C)
stargazer(allroi_C,  type="html", out="all_ROIs_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

allroi_C_red <-   glmer(
  stay ~  trial_scaled + stay_lag*reinf*Group + stay_lag*reinf*vmPFC + stay_lag*reinf*R_Str + stay_lag*reinf*L_Str +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(allroi_C_red)
stargazer(allroi_C_red,  type="html", out="all_ROIs_choice_reduced.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))

#vmpfc: lethality analyses
vmpfc_leth_C <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * GroupLeth*vmPFC  +  
    (stay_lag | ID),
  family = binomial(),
  data = roi_gdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(vmpfc_leth_C)
stargazer(vmpfc_leth_C,  type="html", out="vmPFC_lethality_choice.htm", digits = 3,single.row=T,omit.stat = "bic",
          star.cutoffs = c(0.05, 0.01, 0.001))
