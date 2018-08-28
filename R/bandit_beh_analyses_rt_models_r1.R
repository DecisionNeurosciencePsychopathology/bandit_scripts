#  relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/")
source("~/code/R")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggeffects)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(corrplot)
# library(lsmeans)
library(emmeans)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)
library(multcompView)
library(stargazer)
library(dplyr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggpubr)
theme_set(theme_sjplot())
source('~/code/R/vif.lme.R')

# load(file = "bandit1.RData")

# or to continue where I left off:
 load(file = "bandit2rt.RData")



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


#  Main analysis
s11_rt <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) + scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) +
                          scale(abs(PE_chosen_vba_lag)) * reinf_lag + 
                          reinf_lag * group_recent +
                       scale(abs(PE_chosen_vba_lag)) * group_recent +                     
                          v_max_lag_mfx* group_recent + 
                          (1 | ID),
                        data = rdf_censored)
s12_rt <- update(s11_rt, data = sdf_censored)
s22_rt <- update(s11_rt, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt, s12_rt, s22_rt,  type="html", out="recent_inv_rt.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

vif.lme(s11_rt)
vif.lme(s12_rt)
vif.lme(s22_rt)

# scale lagged RT
rdf_censored$invRT_lag_scaled <- scale(1/rdf_censored$RT_lag,center = TRUE, scale = TRUE)[, 1]
sdf_censored$invRT_lag_scaled <- scale(1/sdf_censored$RT_lag,center = TRUE, scale = TRUE)[, 1]
gdf_censored$invRT_lag_scaled <- scale(1/gdf_censored$RT_lag,center = TRUE, scale = TRUE)[, 1]


# get standardized betas
s11_rt_std <- lme4::lmer(scale(1000/(RT)) ~ invRT_lag_scaled + scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) +
                       scale(abs(PE_chosen_vba_lag)) * reinf_lag + 
                       reinf_lag * Group +
                       scale(abs(PE_chosen_vba_lag)) * Group +                     
                       v_max_lag_mfx* Group + 
                       (1 | ID),
                     data = rdf_censored)
s12_rt_std <- update(s11_rt_std, data = sdf_censored)
s22_rt_std <- update(s11_rt_std, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt_std, s12_rt_std, s22_rt_std,  type="html", out="inv_rt_replication_std.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

out <- car::Anova(s11_rt_std, '3')
sink("s11_rt_std.txt")
print(out,digits = 3)
sink()
# v2 <- c("Lagged decision time", "Trial", "Switch vs. stay", "Reward", "Exploratory choice", "Absolute prediction error (PE)", 
#        "Controls vs. attempters", "Depressed vs. attempters", "Ideators vs. attempters", "Max value",
#        "Stake = 25c", "Stake = 50c",  
#        "Reward * Absolute PE", "Reward * Controls vs attempters", "Reward * Depressed vs attempters", "Reward * Ideators vs attempters",
#        "Absolute PE * Controls vs attempters", "Absolute PE * Depressed vs attempters", "Absolute PE * Ideators vs attempters",
#        "Max value * Controls vs attempters", "Max value * Depressed vs attempters", "Max value * Ideators vs attempters")



v1 <- c("Lagged decision time", "Trial", "Stay vs. switch",  "Exploratory choice", 
        "Controls vs. attempters", "Depressed vs. attempters", "Ideators vs. attempters", 
        "Reward","Reward * Absolute PE", "Reward * Controls vs attempters", "Reward * Depressed vs attempters", "Reward * Ideators vs attempters",
        "Absolute prediction error (PE)", "Absolute PE * Controls vs attempters", "Absolute PE * Depressed vs attempters", "Absolute PE * Ideators vs attempters",
        "Max value","Max value * Controls vs attempters", "Max value * Depressed vs attempters", "Max value * Ideators vs attempters")
model_terms1 <- labels(terms(s11_rt))

s11 <- summary(s11_rt_std)
coef11 <- s11$coefficients
terms11 <- labels(coef11)[[1]]
terms11[2:21]
# term groups: 1 - nuisance, 2 - reward, 3 - PE, 4 - value


s12 <- summary(s12_rt_std)
coef12 <- s12$coefficients
terms12 <- labels(coef12)[[1]]

s22 <- summary(s22_rt_std)
coef22 <- s22$coefficients
terms22 <- labels(coef22)[[1]]



p1 <- plot_model(s11_rt_std,  p.kr = FALSE, terms = terms11, order.terms = c(20:18,10,17:15,6,14:12,11,4,9:7,5,3:1),
           show.p = TRUE, show.values = TRUE,  group.terms = c(1,1,1,2,1,3,1,1,1,4,2,2,2,2,3,3,3,4,4,4),vline.color = "slategray3",
           axis.labels = v1,axis.title = "Slower  < - >  Faster", value.offset = 0.4,colors = c( "gray47", "red3", "green4", "navy"),
           title = "Sample 1, Experiment 1")

p1 <- p1 + theme(axis.text.y = element_text(color = "black"))

p2 <- plot_model(s12_rt_std,  p.kr = FALSE, terms = terms12, order.terms = c(20:18,10,17:15,6,14:12,11,4,9:7,5,3:1),
                 show.p = TRUE, show.values = TRUE,  group.terms = c(1,1,1,2,1,3,1,1,1,4,2,2,2,2,3,3,3,4,4,4),vline.color = "slategray3",
                 axis.labels = rep(" ",20),axis.title = "Slower  < - >  Faster", value.offset = 0.4,colors = c( "gray47", "red3", "green4", "navy"),
                 title = "Sample 2, Experiment 1")

p3 <- plot_model(s22_rt_std,  p.kr = FALSE, terms = terms22, order.terms = c(20:18,10,17:15,6,14:12,11,4,9:7,5,3:1), rm.terms = c("stake_lag25", "stake_lag50"),
                 show.p = TRUE, show.values = TRUE,  group.terms = c(1,1,1,2,1,3,1,1,1,4,2,2,2,2,3,3,3,4,4,4),vline.color = "slategray3",
                 axis.labels = rep(" ",20), axis.title = "Slower  < - >  Faster", value.offset = 0.4,
                 title = "Sample 2, Experiment 2")
p3 = p3 + scale_color_manual(name="Experimental\nCondition", values = c( "gray47", "red3", "green4", "navy"),
                    breaks=c(1,2,3,4),
                    labels=c("Other variables", "Most recent reward", "Absolute prediction error", "Value")) +
     guides(color=guide_legend(title="Groups of predictors", reverse = TRUE))

pdf("rt_models_std_plot3.pdf", width = 12, height = 8)
ggarrange(p1,p2,p3,nrow = 1, ncol = 3, labels = c("A.","B.", "C."), hjust = c(-2,0.25,0.25), widths = c(4,2.75,4.5))
dev.off()

# separately plot stake effects
p4 <- plot_model(s22_rt_std,  p.kr = FALSE, terms =  c("stake_lag25", "stake_lag50"),
                 show.p = TRUE, show.values = TRUE, 
                 axis.labels = c("50c vs. 10c", "25c vs. 10c"), axis.title = "Slower  < - >  Faster", value.offset = 0.4,vline.color = "slategray3",
                 title = "Sample 2, Exp. 2 (cont.)", axis.lim = c(-0.4,0.4), colors = "gray47")
p4 <- p4 + theme(axis.text.y = element_text(color = "black"))

pdf("rt22_stake_std_plot.pdf", width = 3, height = 2)
ggarrange(p4, labels = "D.", hjust = -0.5)
dev.off()

# 
# 
# # multiplot(p1,p2,p3)


pdf("inv_rt_replication_fig.pdf", width = 6, height = 6)
em <- emmeans(s11_rt, ~ v_max_lag_mfx | Group, at = list(v_max_lag_mfx = c(0,1)),p.kr = FALSE)
p1 <- plot(em, horiz = F, col = c("red", "blue"), ylab = "logit(1/RT)", xlab = "Maximum available value, Study 1, Sample 1")
em <- emmeans(s12_rt, ~ v_max_lag_mfx | Group, at = list(v_max_lag_mfx = c(0,1)))
p2 <- plot(em, horiz = F, ylab = "logit(1/RT)", xlab = "Maximum available value, Study 1, Sample 2")
em <- emmeans(s22_rt, ~ v_max_lag_mfx | Group, at = list(v_max_lag_mfx = c(0,1)))
p3 <- plot(em, horiz = F, ylab = "logit(1/RT)", xlab = "Maximum available value, Study 2, Sample 2")
multiplot(p1,p2,p3)
dev.off()
# super-reduced model
s11_rt_red <- lme4::lmer(1000/RT ~ scale(RT_lag) + scale(v_max_lag_mfx)* Group + 
                       (1 | ID),
                     data = rdf_censored)
s12_rt_red <- update(s11_rt_red, data = sdf_censored)
s22_rt_red <- update(s11_rt_red,  data = gdf_censored)
stargazer(s11_rt_red, s12_rt_red, s22_rt_red,  type="html", out="red_inv_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)
lsm <- lsmeans::lsmeans(s11_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s12_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s22_rt,"v_max_lag_mfx", by = c("Group"), at = list(v_max_lag_mfx = c(0,1)))
plot(lsm, horiz = F)
# 

#within-subject, v_max(t-1), replicating s11_rt
s11_wi_v2r <- lme4::lmer(1000/RT ~ I(1000/(RT_lag)) +  scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) + v_max_b + 
                          scale(abs(PE_chosen_vba_lag_wi)) * reinf_lag  + 
                           reinf_lag * Group +
                           scale(abs(PE_chosen_vba_lag_wi)) * Group +                     
                           (v_max_lag2_mfx_wi)* Group + 
                          (1 | ID),
                        data = rdf_censored)
s12_wi_v2r <- update(s11_wi_v2r, data = sdf_censored)
s22_wi_v2r <- update(s11_wi_v2r, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_wi_v2r, s12_wi_v2r, s22_wi_v2r,  type="html", out="inv_rt_wi.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

vif.lme(s11_wi_v2r)
vif.lme(s12_wi_v2r)
vif.lme(s22_wi_v2r)

# 
# lsm <- lsmeans::lsmeans(s11_wi_v2,"v_max_lag2_mfx_wi", by = c("Group"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s12_wi_v2,"v_max_lag2_mfx_wi", by = c("Group"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s22_wi_v2,"v_max_lag2_mfx_wi", by = c("Group"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# 
# lsm <- lsmeans::lsmeans(s11_wi_v2,"v_max_lag2_mfx_wi", by = c("reinf_lag"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s12_wi_v2,"v_max_lag2_mfx_wi", by = c("reinf_lag"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s22_wi_v2,"v_max_lag2_mfx_wi", by = c("reinf_lag"), at = list(v_max_lag2_mfx_wi = c(-2,2)))
# plot(lsm, horiz = F)
# 
# lsm <- lsmeans::lsmeans(s11_wi_v2,"v_max_lag2_mfx_wi", by = c("PE_chosen_vba_lag_wi"), at = list(v_max_lag2_mfx_wi = c(-2,2), PE_chosen_vba_lag_wi = c(0,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s12_wi_v2,"v_max_lag2_mfx_wi", by = c("PE_chosen_vba_lag_wi"), at = list(v_max_lag2_mfx_wi = c(-2,2), PE_chosen_vba_lag_wi = c(0,2)))
# plot(lsm, horiz = F)
# lsm <- lsmeans::lsmeans(s22_wi_v2,"v_max_lag2_mfx_wi", by = c("PE_chosen_vba_lag_wi"), at = list(v_max_lag2_mfx_wi = c(-2,2), PE_chosen_vba_lag_wi = c(0,2)))
# plot(lsm, horiz = F)



s_mega_rt <- update(s11_rt, . ~ . + sample*reinf_lag*Group + sample*v_max_lag_mfx*Group + sample*abs(PE_chosen_vba_lag)*Group, data = mdf[mdf$RT>lower & mdf$RT<upper,])
summary(s_mega_rt)
stargazer(s_mega_rt,  type="html", out="mega_inv_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)


#

s11_rt_lin <- lme4::lmer(RT ~ scale(RT_lag) + scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) +
                         scale(abs(PE_chosen_vba_lag)) * reinf_lag + 
                         reinf_lag * Group +
                         scale(abs(PE_chosen_vba_lag)) * Group +                     
                         v_max_lag_mfx* Group + 
                         (1 | ID),
                       data = rdf_censored)
s12_rt_lin <- update(s11_rt_lin, data = sdf_censored)
s22_rt_lin <- update(s11_rt_lin, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt_lin, s12_rt_lin, s22_rt_lin,  type="html", out="rt_lin_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)


# also with entropy
h <- ".~. - v_max_lag_mfx*Group + h_lag_mfx_mc*Group"

s11_rtH <- update(s11_rt, h)
s12_rtH <- update(s12_rt, h)
s22_rtH <- update(s22_rt, h)
stargazer(s11_rtH, s12_rtH, s22_rtH,  type="html", out="h_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

# 
##########
# sensitivity analyses

# demo

demog <- ".~.+scale(age)*v_max_lag_mfx + scale(age)*reinf_lag + scale(age)*abs(PE_chosen_vba_lag) +
            scale(education)*v_max_lag_mfx + scale(education)*reinf_lag + scale(education)*abs(PE_chosen_vba_lag) +
            sex*v_max_lag_mfx + sex*reinf_lag + sex*abs(PE_chosen_vba_lag)"
s11_rt_dem <- update(s11_rt, demog)
s12_rt_dem <- update(s12_rt,demog)
s22_rt_dem <- update(s22_rt, demog)
stargazer(s11_rt_dem, s12_rt_dem, s22_rt_dem,  type="html", out="demog_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)
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

# ATHF
athf <- ".~.+STRENGTH_6MO*v_max_lag_mfx + STRENGTH_6MO*reinf_lag + STRENGTH_6MO*abs(PE_chosen_vba_lag)"
s11_rt_athf <- update(s11_rt_dem, athf)
s12_rt_athf <- update(s12_rt_dem, athf)
s22_rt_athf <- update(s22_rt_dem, athf)
stargazer(s11_rt_athf, s12_rt_athf, s22_rt_athf,  type="html", out="athf_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

sed <- ".~.+SEDATIVES*v_max_lag_mfx + SEDATIVES*reinf_lag + SEDATIVES*abs(PE_chosen_vba_lag)"
s11_rt_sed <- update(s11_rt_dem, sed)
s12_rt_sed <- update(s12_rt_dem, sed)
s22_rt_sed <- update(s22_rt_dem, sed)
stargazer(s11_rt_sed, s12_rt_sed, s22_rt_sed,  type="html", out="sed_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

op <- ".~.+OPIATES*v_max_lag_mfx + OPIATES*reinf_lag + OPIATES*abs(PE_chosen_vba_lag)"
s11_rt_op <- update(s11_rt_dem, op)
s12_rt_op <- update(s12_rt_dem, op)
s22_rt_op <- update(s22_rt_dem, op)
stargazer(s11_rt_op, s12_rt_op, s22_rt_op,  type="html", out="op_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# brain damage
s11_rt_bd <- update(s11_rt, data = rdf[rdf$RT>lower & rdf$RT<upper_rdf & rdf$no_brain_damage,])
s12_rt_bd <- update(s11_rt, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf & sdf$no_brain_damage,])
s22_rt_bd <- update(s22_rt, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf & gdf$no_brain_damage,])
stargazer(s11_rt_bd, s12_rt_bd, s22_rt_bd,  type="html", out="bd_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

# HRSD16
hrsd_b <- ".~.+HRSD16_BASELINE*v_max_lag_mfx + HRSD16_BASELINE*reinf_lag + HRSD16_BASELINE*abs(PE_chosen_vba_lag)"
hrsd_s <- ".~.+HRSD16_SCAN*v_max_lag_mfx + HRSD16_SCAN*reinf_lag + HRSD16_SCAN*abs(PE_chosen_vba_lag)"
s11_rt_hrsd <- update(s11_rt_dem, hrsd_b)
s12_rt_hrsd <- update(s12_rt_dem, hrsd_b)
s22_rt_hrsd <- update(s22_rt_dem, hrsd_s)
stargazer(s11_rt_hrsd, s12_rt_hrsd, s22_rt_hrsd,  type="html", out="hrsd_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)


ssi <- ".~.+ scale(SSI_baseline)*v_max_lag_mfx + scale(SSI_baseline)*reinf_lag + scale(SSI_baseline)*abs(PE_chosen_vba_lag)"
s11_rt_ssi <- update(s11_rt_dem, ssi, data = rdf_censored[rdf_censored$group1245!=1,])
s12_rt_ssi <- update(s12_rt_dem, ssi)
s22_rt_ssi <- update(s22_rt_dem, ssi)
stargazer(s11_rt_ssi, s12_rt_ssi, s22_rt_ssi,  type="html", out="ssi_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)



# lethality analyses

s11_rt_leth <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) +scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) +
                            scale(abs(PE_chosen_vba_lag)) * reinf_lag + 
                            reinf_lag * GroupLeth +
                            scale(abs(PE_chosen_vba_lag)) * GroupLeth +                     
                            v_max_lag_mfx* GroupLeth + 
                                (1 | ID),
                              data = rdf_censored)
# Study 1, sample 2 (first exp in fMRI sample)
s12_rt_leth <- update(s11_rt_leth, data = sdf_censored)
# Study 2, sample 2 (fMRI exp)
s22_rt_leth <- update(s11_rt_leth, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt_leth, s12_rt_leth, s22_rt_leth,  type="html", out="leth_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

lsm <- lsmeans::lsmeans(s11_rt_leth,"PE_chosen_vba_lag", by = c("GroupLeth"), at = list(PE_chosen_vba_lag = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s12_rt_leth,"PE_chosen_vba_lag", by = c("GroupLeth"), at = list(PE_chosen_vba_lag = c(0,1)))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s22_rt_leth,"PE_chosen_vba_lag", by = c("GroupLeth"), at = list(PE_chosen_vba_lag = c(0,1)))
plot(lsm, horiz = F)

lsm <- lsmeans::lsmeans(s11_rt_leth,"reinf_lag", by = c("GroupLeth"))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s12_rt_leth,"reinf_lag", by = c("GroupLeth"))
plot(lsm, horiz = F)
lsm <- lsmeans::lsmeans(s22_rt_leth,"reinf_lag", by = c("GroupLeth"))
plot(lsm, horiz = F)

# with v(t-2) and decomposition
s11_rt_leth_wi <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) +scale(Trial) + scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) + v_max_b + 
                            scale(abs(PE_chosen_vba_lag_wi)) * reinf_lag  + 
                            reinf_lag * GroupLeth +
                            scale(abs(PE_chosen_vba_lag_wi)) * GroupLeth +                     
                            (v_max_lag2_mfx_wi)* GroupLeth + 
                            (1 | ID),
                          data = rdf_censored)
# Study 1, sample 2 (first exp in fMRI sample)
s12_rt_leth_wi <- update(s11_rt_leth_wi, data = sdf_censored)
# Study 2, sample 2 (fMRI exp)
s22_rt_leth_wi <- update(s11_rt_leth_wi, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt_leth_wi, s12_rt_leth_wi, s22_rt_leth_wi,  type="html", out="leth_wi_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

vif.lme(s11_rt_leth_wi)
vif.lme(s12_rt_leth_wi)
vif.lme(s22_rt_leth_wi)



s11_rt_leth_exit <- update(s11_rt_leth, exit)
s12_rt_leth_exit <- update(s12_rt_leth,exit)
s22_rt_leth_exit <- update(s22_rt_leth, exit)
stargazer(s11_rt_leth_exit, s12_rt_leth_exit, s22_rt_leth_exit,  type="html", out="leth_exit_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

vif.lme(s11_rt_leth_exit)
vif.lme(s12_rt_leth_exit)
vif.lme(s22_rt_leth_exit)



mDiag <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) + trial_scaled + reinf_lag * GroupLeth + stay_lag * GroupLeth +
                                abs(PE_chosen_vba_lag) * GroupLeth  +
                                (1 | ID),
                              data = rdf_censored)
vif.lme(mDiag)



s11_rt_leth_nov <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) +trial_scaled + stay_lag + 
                                reinf_lag * GroupLeth + 
                            (1 | ID),
                          data = rdf_censored)
# Study 1, sample 2 (first exp in fMRI sample)
s12_rt_leth_nov <- update(s11_rt_leth_nov, data = sdf_censored)
# Study 2, sample 2 (fMRI exp)
s22_rt_leth_nov <- update(s11_rt_leth_nov, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt_leth_nov, s12_rt_leth_nov, s22_rt_leth_nov,  type="html", out="leth_nov_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)
vif.lme(s11_rt_leth_nov)
vif.lme(s12_rt_leth_nov)
vif.lme(s22_rt_leth_nov)

# Noonan 2011-type analysis -- suffers from multicollinearity
s11_noon <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) + scale(Trial) + stay_lag   + reinf_lag + scale(v1)*scale(v2)*scale(v3)*Group + 
                       (1 | ID),
                     data = rdf_censored)
summary(s11_noon)
car::Anova(s11_noon)
vif.lme(s11_noon)

# check correlations between random effects of value on RT and reinforcement on choice
s11_rt_r <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) + scale(Trial) + stay_lag  + reinf_lag + 
                       scale(abs(PE_chosen_vba_lag)) * reinf_lag  + 
                       v_max_lag_mfx + 
                       (reinf_lag + v_max_lag_mfx + scale(abs(PE_chosen_vba_lag))| ID),
                     data = rdf_censored)
rt <- ranef(s11_rt_r)
rrt <- rt$ID$reinf
vrt <- rt$ID$v_max_lag_mfx
pert <- rt$ID$`scale(abs(PE_chosen_vba_lag))`
cor.test(rrt,vrt)
cor.test(rrt,pert)
cor.test(pert,vrt)


s11_reinf_r <- glmer(
  stay ~  trial_scaled + stay_lag  * reinf  +  
    (stay_lag* reinf | ID),
  family = binomial(),
  data = rdf_censored,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
re <- ranef(s11_reinf_r)
rew <- re$ID$reinf
stay <- re$ID$stay_lagTRUE
rewBYstay <- re$ID$`stay_lagTRUE:reinf1`
cor.test(rrt,rew)
cor.test(pert,rew)
cor.test(vrt,rew)

cor.test(rrt,stay)
cor.test(pert,stay)
cor.test(vrt,stay)

cor.test(rrt,rewBYstay)
cor.test(pert,rewBYstay)
cor.test(vrt,rewBYstay)


save(list = ls(all.names = TRUE), file = "bandit2rt.RData")
