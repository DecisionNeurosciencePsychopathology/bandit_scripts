#  relate ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/")
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(corrplot)
library(lsmeans)
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
# ggplot(gdf[gdf$RT>2000,],aes(Trial)) + geom_histogram()
# ggplot(rdf[rdf$RT>4000,],aes(Trial)) + geom_histogram()
# ggplot(sdf[sdf$RT>4000,],aes(Trial)) + geom_histogram()
# 
# 
# ggplot(gdf[gdf$RT>0,],aes(x = Trial, y = RT)) + geom_line() + facet_wrap(~ID)
# ggplot(sdf[sdf$RT>0 & sdf$RT<4000,],aes(RT)) + geom_histogram() + facet_wrap(~ID)
# ggplot(sdf[sdf$RT>0 & sdf$RT<4000,],aes(reinf)) + geom_bar() + facet_wrap(~ID)

# ggplot(sdf[sdf$RT>0 & sdf$RT<4000,],aes(y = (RT))) + geom_histogram() + facet_wrap(~ID)


# ggplot(gdf[gdf$RT>0 & gdf$RT<4000,],aes(x = v_chosen_lag, y = RT)) + geom_smooth(method = "loess")

# ggplot(gdf[gdf$RT>0 & gdf$RT<4000 & !is.na(gdf$Group),],aes(x = v_max_lag, y = RT, color = Group)) + geom_smooth(method = "loess") 

###################
# RT data cleaning

hist(rdf$RT[rdf$RT<4000 & rdf$RT>200],100)
hist(sdf$RT[sdf$RT<4000 & sdf$RT>200],100)
hist(gdf$RT[gdf$RT<4000 & gdf$RT>200],100)


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

gdf_censored <- gdf[gdf$RT>lower & gdf$RT<upper_gdf,]
rdf_censored <- rdf[rdf$RT>lower & rdf$RT<upper_rdf,]
sdf_censored <- sdf[sdf$RT>lower & sdf$RT<upper_sdf,]


# find super-slow subjects

r <- (summarise(group_by(rdf,ID),mean(RT)))
s <- (summarise(group_by(sdf,ID),mean(RT)))
rs <- rbind(r,s)
g <- (summarise(group_by(gdf,ID),mean(RT)))
rd <- psych::describe(r$`mean(RT)`, IQR = TRUE, quant = c(.9, .95,.99))
r$ID[r$`mean(RT)`>as.numeric(rd$Q0.99)]
sd <- psych::describe(s$`mean(RT)`, IQR = TRUE, quant = c(.9, .95,.99))
s$ID[s$`mean(RT)`>as.numeric(sd$Q0.99)]

rsd <- psych::describe(rs$`mean(RT)`, IQR = TRUE, quant = c(.9, .95,.99))
s1_slow99 <- rs$ID[rs$`mean(RT)`>as.numeric(rsd$Q0.99)]
s1_slow95 <- rs$ID[rs$`mean(RT)`>as.numeric(rsd$Q0.95)]

gd <- psych::describe(g$`mean(RT)`, IQR = TRUE, quant = c(.9, .95,.99))
s2_slow99 <- g$ID[g$`mean(RT)`>as.numeric(gd$Q0.99)]

rclean <- rdf[rdf$RT>lower & rdf$RT<upper_rdf & !is.element(rdf$ID,s1_slow99),]
sclean <- sdf[sdf$RT>lower & sdf$RT<upper_sdf & !is.element(sdf$ID,s1_slow99),]
gclean <- gdf[gdf$RT>lower & gdf$RT<upper_rdf & !is.element(gdf$ID,s2_slow99),]

# find slow within-subject RTs

g_out <- psych::describe(gdf$RT_wi, IQR = TRUE, quant = c(.9, .95,.99))
g_wi_upper <- g_out$Q0.95 + g_out$IQR
gdf_wi <- gdf[gdf$RT>lower & gdf$RT_wi<g_wi_upper,]

s_out <- psych::describe(sdf$RT_wi, IQR = TRUE, quant = c(.9, .95,.99))
s_wi_upper <- s_out$Q0.95+ s_out$IQR
sdf_wi <- sdf[sdf$RT>lower & sdf$RT_wi<s_wi_upper,]

r_out <- psych::describe(rdf$RT_wi, IQR = TRUE, quant = c(.9, .95,.99))
r_wi_upper <- r_out$Q0.95+ r_out$IQR
rdf_wi <- rdf[rdf$RT>lower & rdf$RT_wi<g_wi_upper,]


##############################
# check correlations
# 
# chars <- as.data.frame(gdf[gdf$RT>lower & gdf$RT<upper_gdf, c("Trial","RT", "reinf_n_lag","stay_lag", "v_chosen_lag_mfx","v_max_lag2_mfx", "v_max_lag_mfx", "v_ch_diff", "PE_chosen_vba_lag")])
# chars$logRT <- log(chars$RT)
# chars$absPE_chosen_vba_lag <- abs(chars$PE_chosen_vba_lag)
# chars$stay_lag <- as.numeric(chars$stay_lag)
# # pdf("bandit correlations 11.pdf", width=14, height=14)
# cors <- psych::corr.test(chars, use = "pairwise",method="spearman", alpha=.05)
# 
# corrplot(cors$r, cl.lim=c(-1,1),
#          method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
#          diag = FALSE,
#          addCoef.col="black", addCoefasPercent = FALSE,
#          p.mat = cors$p, sig.level=0.05, insig = "blank")
# p.mat = 1-abs(cormat), sig.level=0.75, insig = "blank")
# dev.off()
# 
# 
# pdf("bandit rt_val_22.pdf", width=6, height=3)
# ggplot(gdf[gdf$RT>lower & gdf$RT<upper & !is.na(gdf$Group),],aes(x = v_max_lag_mfx, y = log(RT), color = Group)) + geom_smooth(method = "gam") + 
#   xlab("Highest available value") + ggtitle("Study 2, Sample 2")
# dev.off()
# pdf("bandit rt_val_12.pdf", width=6, height=3)
# ggplot(sdf[sdf$RT>lower & sdf$RT<upper & !is.na(sdf$Group),],aes(x = v_max_lag_mfx, y = log(RT), color = Group)) + geom_smooth(method = "gam") + 
#   xlab("Highest available value") + ggtitle("Study 1, Sample 2")
# 
#   dev.off()
# pdf("bandit rt_val_11.pdf", width=6, height=3)
# ggplot(rdf[rdf$RT>lower & rdf$RT<upper & !is.na(rdf$Group) &  !is.na(rdf$stay_lag),],aes(x = v_max_lag_mfx, y = log(RT), color = Group)) + geom_smooth(method = "gam") + facet_wrap(~stay_lag) +
#   xlab("Highest available value") + ggtitle("Study 1, Sample 1")
# dev.off()
# 
# 
# ggplot(gdf,aes(x = Trial, y = v_max_lag_mfx)) + geom_smooth(method = "loess") 
# ggplot(gdf,aes(x = Trial, y = log(RT))) + geom_smooth(method = "loess") 
# 


# for multicollinearity checks
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

# basic model, no interactions, looking for value effect
# 
# 
# s11_rt000 <- lme4::lmer((RT) ~ scale(Trial) + reinf_lag * stay_lag +
#                                                 (1 | ID),
#                       data = rdf[rdf$RT>lower & rdf$RT<upper,])
# summary(s11_rt000)
# 
# s22_rt000 <- lme4::lmer((RT) ~ scale(Trial) + reinf_lag * stay_lag +
#                           (1 | ID),
#                         data = gdf[gdf$RT>lower & gdf$RT<upper,])
# summary(s22_rt000)
# 
# 
# vif.lme(s11_rt000)
# 
# s11_rt00 <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + trial_scaled + reinf_lag + stay_lag +
#                           abs(PE_chosen_vba_lag)+
#                           (1 | ID),
#                         data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# summary(s11_rt00)
# vif.lme(s11_rt00)
# 
# s11_rt00v <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + scale(Trial) +
#                            scale(abs(PE_chosen_vba_lag)) * reinf_lag +
#                           reinf_lag * Group +
#                            scale(v_max_lag_mfx)*  Group +
#                            (1 | ID),
#                          data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# s12_rt00v <- update(s11_rt00v, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf,])
# s22_rt00v <- update(s11_rt00v, . ~ . + stake_lag, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf,])
# stargazer(s11_rt00v, s12_rt00v, s22_rt00v,  type="html", out="log_rt_vgrp_PEbyReward.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
#           column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
#           star.cutoffs = c(0.05, 0.01, 0.001))
# vif.lme(s11_rt00v)
# vif.lme(s12_rt00v)
# vif.lme(s22_rt00v)

################
# use entropy instead of value -- poorer fit
# 
# s11_rth <- lme4::lmer(log(RT) ~ log(RT_lag) + stay_lag + scale(Trial) +
#                         reinf_lag*scale(abs(PE_chosen_vba_lag)) +
#                         scale(h_lag_mfx_mc)*Group +
#                         (1 | ID),
#                       data = rdf[rdf$RT>lower & rdf$RT<upper_rdf,])
# # first exp in fMRI sample 
# s12_rth <- update(s11_rth, data = sdf[sdf$RT>lower & sdf$RT<upper_sdf,])
# # fMRI exp
# s22_rth <- update(s11_rth, . ~ . + stake_lag, data = gdf[gdf$RT>lower & gdf$RT<upper_gdf,])
# 
# stargazer(s11_rth, s12_rth, s22_rth,  type="html", out="log_rt_H.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
#           column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
#           star.cutoffs = c(0.05, 0.01, 0.001))
# 
# vif.lme(s11_rth)
# vif.lme(s12_rth)
# vif.lme(s22_rth)

#  Main analysis
s11_rt <- lme4::lmer(1000/(RT) ~ I(1000/(RT_lag)) + scale(Trial) + stay_lag  + reinf_lag + scale(v_ch_diff) +
                          scale(abs(PE_chosen_vba_lag)) * reinf_lag + 
                          reinf_lag * Group +
                       scale(abs(PE_chosen_vba_lag)) * Group +                     
                          v_max_lag_mfx* Group + 
                          (1 | ID),
                        data = rdf_censored)
s12_rt <- update(s11_rt, data = sdf_censored)
s22_rt <- update(s11_rt, . ~ . + stake_lag, data = gdf_censored)
stargazer(s11_rt, s12_rt, s22_rt,  type="html", out="inv_rt_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

vif.lme(s11_rt)
vif.lme(s12_rt)
vif.lme(s22_rt)

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

vif.lme(s11_rt_wi_v2r)
vif.lme(s12_rt_wi_v2r)
vif.lme(s22_rt_wi_v2r)

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


save(list = ls(all.names = TRUE), file = "bandit2rt.RData")
