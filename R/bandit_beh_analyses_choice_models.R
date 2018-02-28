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

# instead of rerunning:
 # load(file = "bandit2choice.RData")

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

chars <- as.data.frame(bdf[, c("v_chosen_lag_mfx","v_max_lag2_mfx", "v_max_lag_mfx", "v_ch_diff", "vmaxdiff", "PE_chosen_vba_mfx","PE_chosen_vba_lag", "reinf_n","reinf_n_lag", "h_lag_mfx","stay_p", "stake_n", "stake_n_lag","RT")])

pdf("bandit correlations.pdf", width=14, height=14)
cors <- psych::corr.test(chars, use = "pairwise",method="pearson", alpha=.05)

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

# # missed trials
# mt <- glmer(
#   RT==0 ~ trial_scaled +  reinf_lag +
#     Group   +
#     (stay_lag | ID),
#   family = binomial(),
#   data = gdf,
#     glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
# summary(mt)
# car::Anova(mt, type = 'III')
# lsm <- lsmeans(mt,"Group")
# cld(lsm)

# simplest model without RL

# replicate reinforcement effects
s11_reinf <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * Group  +  
    (stay_lag | ID),
  family = binomial(),
  data = rdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(s11_reinf)
car::Anova(s11_reinf, type = 'III')
lsm <- lsmeans::lsmeans(s11_reinf, "reinf",by = "Group")
plot(lsm, horiz = F)

s12_reinf <- update(s11_reinf,data = sdf)
car::Anova(s12_reinf, type = 'III')
lsm <- lsmeans::lsmeans(s12_reinf, "reinf",by = "Group")
plot(lsm, horiz = F)

# need to add stake
s22_reinf <- update(s11_reinf, .~. + stake_lag, data = gdf)
summary(s22_reinf)
car::Anova(s22_reinf, type = 'III')
lsm <- lsmeans::lsmeans(s22_reinf, "reinf",by = "Group")
plot(lsm, horiz = F)


stargazer(s11_reinf, s12_reinf, s22_reinf, type="html", out="reinf_choice_replication.htm", digits = 2,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

# lethality analyses
s11_reinf_leth <-   glmer(
  stay ~  trial_scaled + stay_lag  * reinf * GroupLeth  +  
    (stay_lag | ID),
  family = binomial(),
  data = rdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
s12_reinf_leth <- update(s11_reinf_leth,data = sdf)
s22_reinf_leth <- update(s11_reinf_leth,data = gdf)
stargazer(s11_reinf_leth, s12_reinf_leth, s22_reinf_leth,  type="html", out="leth_reinf_choice_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))

#  do attempters switch away from high-value choices? (exploring win-switches)
# replicate
s11_v <-   lme4::lmer(value_chosen_vba_mfx ~ reinf * stay  +  value_max_vba_mfx * Group + stay * reinf * Group + v_ch_diff * reinf * Group + 
                            (1 | ID),
                          data = rdf)
s12_v <- update(s11_v,data = sdf)
s22_v <- update(s11_v,.~. + stake_lag, data = gdf)
stargazer(s11_v, s12_v, s22_v,  type="html", out="v_choice_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))


models <- c("s11_v","s12_v","s22_v")

for(i in 1:length(models))
{modelname <- models[i]
lsm <- lsmeans::lsmeans(get(modelname), "stay",by = c("Group", "reinf"))
CLD = cld(lsm,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
CLD$.group=gsub(" ", "", CLD$.group)
CLD$win <- NA
CLD$win[CLD$reinf==1] <- "win"
CLD$win[CLD$reinf==0] <- "lose"

CLD$switch <- NA
CLD$switch[CLD$stay==TRUE] <- "stay"
CLD$switch[CLD$stay==FALSE] <- "switch"

filename <- paste0("reinfBYstayBYgroup_on_v_chosen_", modelname, ".pdf")
pdf(filename, width=4, height=6)
pd = position_dodge(.5)    ### How much to jitter the points on the plot
print(ggplot(CLD, aes(x = switch, y = lsmean, color = Group,
               label = .group)) + facet_wrap(~win) +
        geom_point(shape  = 15,
                   size   = 4,
                   position = pd) +
        geom_errorbar(
          aes(ymin  =  lower.CL,
              ymax  =  upper.CL),
          width =  0.2,
          size  =  0.7,
          position = pd
        ) +
        theme_bw() +
        theme(
          axis.title.x=element_blank(),
          # axis.title = NULL,
          axis.text    = element_text(face = "bold", size = 8),
          plot.caption = element_text(hjust = 0)
        ) +
        ylab("Value of next choice \nlower (explore)  <=   =>   higher (exploit)")) #+
      # geom_text(color   = "black") #+
        # scale_color_manual(values = c("blue", "red")))
dev.off()
}

# exploratory analyses of lethality -- not much going on
s11_v_leth <-   lme4::lmer(value_chosen_vba_mfx ~ reinf * stay  +  value_max_vba_mfx * GroupLeth + stay * reinf * GroupLeth + v_ch_diff * reinf * GroupLeth + 
                        (1 | ID),
                      data = rdf)
s12_v_leth <- update(s11_v_leth,data = sdf)
s22_v_leth <- update(s11_v_leth,.~. + stake_lag, data = gdf)
stargazer(s11_v_leth, s12_v_leth, s22_v_leth,  type="html", out="leth_v_choice_replication.htm", digits = 3,single.row=TRUE,omit.stat = "bic",
          column.labels = c("Study 1, sample 1", "Study 1, sample 2", "Study 2, sample 2"),
          star.cutoffs = c(0.05, 0.01, 0.001))





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

vif.lme(s11_reinf)
vif.lme(s12_reinf)
vif.lme(s22_reinf)

vif.lme(s11_v)
vif.lme(s12_v)
vif.lme(s22_v)

save(list = ls(all.names = TRUE), file = "bandit2choice.RData")
