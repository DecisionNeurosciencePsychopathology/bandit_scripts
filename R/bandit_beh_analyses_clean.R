#  related ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper
#  separate sets of scripts for fMRI/behavior-only samples because fMRI does
#  more detailed plots in bandit_beh_analyses

setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R")
library(readr)
library(lme4)
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



trial_df <-
  read_csv("~/code/bandit_scripts/data/scanner/bandit_df1.csv")
View(trial_df)
sub_df <-
  read_csv("~/code/bandit_scripts/data/scanner/bandit_df2.csv")
View(sub_df)
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
chars <- as.data.frame(sub_df[, c(9:12, 14, 20, 22)])
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
    hide = c(sex = "FEMALE", list(race = c(
      "WHITE", "ASIAN PACIFIC"
    ))),
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t1, "t_bandit_beh_by_group.html")

# coarse overview of behavior
hist(sub_df$spont_switch_err, breaks = 50)
hist(sub_df$prob_switch_err, breaks = 50)
hist(sub_df$erratic_spont, breaks = 50)
hist(sub_df$error_NOS)


# summary(m1 <- lm(spont_switch_err ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot, data = sub_df))
# anova(m1)
summary(m2 <-
          lm(
            error_NOS ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot,
            data = sub_df
          ))
anova(m2)
summary(m3 <-
          glm.nb(spont_switch_err ~ group1245 +  WTAR_SCALED_SCORE + EXITtot, data = sub_df))
car::Anova(m3, type = 'III')


# merge trial-by-trial and subject-level data
bdf <- merge(trial_df, sub_df)

summary(bdf)

bdf$Group <-
  recode(
    bdf$group1245,
    `1` = "Controls",
    `2` = "Depressed",
    `4` = "Ideators",
    `5` = "Attempters"
  )
contrasts(bdf$Group) <- contr.treatment(levels(bdf$Group),
                                        base = which(levels(bdf$Group) == 'Attempters'))

sub_df$Group <-
  recode(
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
    v_chosen_lag_mfx  = lag(value_chosen_vba_mfx),
    h_lag = lag(H),
    h_lag_mfx = lag(H_vba_mfx)
  ) %>% ungroup()
bdf$stay <- bdf$choice_numeric == bdf$choice_num_lead
bdf = bdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)) %>% ungroup()

bdf$trial_scaled <- scale(bdf$Trial)
bdf$stay_p <- NA
bdf$stay_p[bdf$stay] <- 1
bdf$stay_p[!bdf$stay] <- 0
bdf$past_rew <-
  recode(bdf$reinf_lag, `0` = "After omission", `1` = "After reward")
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
bdf$v_chosen_lag_mc <- scale(bdf$v_chosen_lag)
bdf$h_lag_mc <- scale(bdf$h_lag)

bdf$v_chosen_lag_mfx_mc <-  scale(bdf$v_chosen_lag_mfx)
bdf$h_lag_mfx_mc <- scale(bdf$h_lag_mfx)
bdf$h_mc <- scale(bdf$H)
bdf$h_mfx_mc <- scale(bdf$H_vba_mfx)

# make sure simple bivariate effect of entropy is negative
sm0 <-
  glmer(
    stay ~ reinf  +  stake_lag + stake + trial_scaled + h_mc + stay_lag +
      (1 | ID),
    family = binomial(),
    data = bdf,
    nAGQ = 0  )
summary(sm0)


# simple, reasonably complete model using individual VBA fits
sm1 <-
  glmer(
    stay ~ reinf * stake_lag + v_chosen_lag_mc + stake + trial_scaled + h_mc + stay_lag +
      (1 | ID),
    family = binomial(),
    data = bdf,
    nAGQ = 0  )
summary(sm1)
car::Anova(sm1)

# mfx improves fits:
sm1mfx <-   glmer(
  stay ~ reinf * stake_lag + v_chosen_lag_mfx_mc + stake + trial_scaled + h_mfx_mc + stay_lag +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0)
summary(sm1mfx)
car::Anova(sm1mfx)
anova(sm1,sm1mfx)

# add reasonable interactions: that also helps
sm2mfx <-   glmer(
  stay ~ reinf * stake_lag + reinf*h_mfx_mc + v_chosen_lag_mfx_mc*h_mfx_mc + stake + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0)
summary(sm2mfx)
car::Anova(sm2mfx)
anova(sm1mfx,sm2mfx)

# final model before sensitivity analyses:
sm2mfxG <-   glmer(
  stay ~ reinf * stake_lag + reinf*h_mfx_mc + reinf * Group + v_chosen_lag_mfx_mc*Group*h_mfx_mc + stake + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0)
summary(sm2mfxG)
car::Anova(sm2mfxG)
anova(sm2mfx,sm2mfxG)

lsm <- lsmeans(sm2mfxG,"Group", by = c("h_mfx_mc","v_chosen_lag_mfx_mc"), at = list(v_chosen_lag_mfx_mc = c(0.1,0.9), h_mfx_mc = c(-1,1)))
plot(lsm, horiz = F)
CLD <- cld(lsm)

# control for beta and fit -- those do explain group differences
sm2mfxGbetaL <-   glmer(
  stay ~ reinf * stake_lag + reinf*h_mfx_mc + reinf * Group + reinf * beta_vba_mfx + reinf * L_vba_mfx + 
    v_chosen_lag_mfx_mc*Group*h_mfx_mc + 
    v_chosen_lag_mfx_mc*beta_vba_mfx*h_mfx_mc + 
    v_chosen_lag_mfx_mc*L_vba_mfx*h_mfx_mc + 
    stake + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0)
summary(sm2mfxGbetaL)
car::Anova(sm2mfxGbetaL)
anova(sm2mfx,sm2mfxG)


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
  data = bdf,
  nAGQ = 0)
summary(sm2mfxG_sens)
car::Anova(sm2mfxG_sens)

bdf$stay_lag <- as.factor(bdf$stay_lag)
# previous analysis of value of the stimulus about to be chosen:
m_old <-   glmer(
  stay ~ reinf * stake_lag + reinf*h_mfx_mc + reinf * Group + value_chosen_vba_mfx*Group*h_mfx_mc + stake + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0)
summary(m_old)
car::Anova(m_old)
plot(allEffects(m_old))

ls_m_old <- lsmeans(m_old,"value_chosen_vba_mfx", by = c("Group","h_mfx_mc"), at = list(value_chosen_vba_mfx = c(0.1,0.9), h_mfx_mc = c(-1,1)))
plot(ls_m_old, horiz = F)



# skip the graphics for now

# groups don't differ significantly in their earnings
rm1 <-   glmer(
  reinf ~ Group + stake + trial_scaled  +
    (1 | ID),
  family = binomial(),
  data = bdf,
  nAGQ = 0
)
summary(rm1)
car::Anova(rm1)


# exclude subjects who pressed one button repeatedly
gdf <- bdf[!bdf$bad,]

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


ggplot(na.omit(bdf[, c(1:17, 42:dim(bdf)[2])]), aes(x = v_chosen_lag_mfx_mc, y = stay_p, color = Group)) + stat_smooth(
  method = "glm",
  method.args = list(family = "binomial"),
  se = TRUE
) + theme_bw(base_size = 20) + labs(x = "Chosen value, mean-centered", y = "Probability of staying with the same choice")


# RL model parameters across groups
ggplot(data = sub_df, aes(y = alpha_win_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = alpha_loss_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = decay_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df, aes(y = beta, x = Group, color = Group)) + geom_boxplot() + geom_jitter() + stat_smooth(method = "lm", formula = beta ~ Group, se=T,
                                                                                                              level=0.95)
ggplot(data = sub_df, aes(y = beta_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

ggplot(data = sub_df, aes(y = L, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

plot(sub_df$alpha_loss, sub_df$alpha_loss_vba_mfx)
plot(sub_df$alpha_win, sub_df$alpha_win_vba_mfx)
plot(sub_df$decay, sub_df$decay_vba_mfx)
plot(sub_df$beta, sub_df$beta_vba_mfx)

pm1 <-
  manova(cbind(alpha_win, alpha_loss, decay, beta) ~ Group, data = sub_df)
summary(pm1)
anova(pm1)

pm1mfx <-
  manova(
    cbind(
      alpha_win_vba_mfx,
      alpha_loss_vba_mfx,
      decay_vba_mfx,
      beta_vba_mfx
    ) ~ Group,
    data = sub_df)
summary(pm1mfx)
anova(pm1mfx)


pm2mfx <- lm(alpha_win_vba_mfx ~ Group, data = sub_df)
summary(pm2mfx)
anova(pm2mfx)

pm3mfx <- lm(alpha_loss_vba_mfx ~ Group, data = sub_df)
summary(pm3mfx)
anova(pm3mfx)

pm4mfx <- lm(decay_vba_mfx ~ Group, data = sub_df)
summary(pm4mfx)
anova(pm4mfx) #*

pm5mfx <- lm(beta_vba_mfx ~ Group, data = sub_df)
summary(pm5mfx)
anova(pm5mfx)
ggplot(data = sub_df, aes(y = beta_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

pm6mfx <- lm(L_vba_mfx ~ Group, data = sub_df)
summary(pm6mfx)
anova(pm6mfx)
ggplot(data = sub_df, aes(y = L_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()



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
  read_csv("~/code/bandit_scripts/data/beh/bandit_df1.csv")
View(beh_trial_df)
beh_sub_df <-
  read_csv("~/code/bandit_scripts/data/beh/bandit_df2.csv")
View(beh_sub_df)

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
chars <- as.data.frame(c4[, c(9:14, 25:40)])
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
              digits = 0,
              show.n = TRUE)
export2html(t5, "age_equated_unique_beh_t_bandit_beh_by_group.html")

beh_sub_df$Group <-
  recode(
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

rpm1 <-
  manova(cbind(alpha_win_vba_mfx, alpha_loss_vba_mfx, decay_vba_mfx, beta_vba_mfx) ~ Group, data = beh_sub_df)
summary(rpm1)
anova(rpm1)

rpm2 <- lm(alpha_win_vba_mfx ~ Group, data = beh_sub_df)
summary(rpm2)
anova(rpm2)

rpm3 <- lm(alpha_loss_vba_mfx ~ Group, data = beh_sub_df)
summary(rpm3)
anova(rpm3)

rpm4 <- lm(decay_vba_mfx ~ Group, data = beh_sub_df)
summary(rpm4)
anova(rpm4) #.
ggplot(data = beh_sub_df, aes(y = decay_vba_mfx, x = Group, color = Group)) + geom_boxplot() + geom_jitter()

rpm5 <- lm(beta_vba_mfx ~ Group, data = beh_sub_df)
summary(rpm5)
anova(rpm5)
ggplot(data = beh_sub_df, aes(y = beta, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ls_rpm5 <- lsmeans(rpm5, "Group")
cld(ls_rpm5)

rpm6 <- lm(L_vba_mfx ~ Group, data = beh_sub_df)
summary(rpm6)
anova(rpm6)
ggplot(data = beh_sub_df, aes(y = L, x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ls_rpm6 <- lsmeans(rpm6, "Group")
cld(ls_rpm6)



# let's try the unique sample first
rdf <- merge(beh_trial_df, c4)


View(rdf)
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
    v_chosen_lag_mfx  = lag(value_chosen_vba_mfx),
    h_lag = lag(H),
    h_lag_mfx = lag(H_vba_mfx)
  ) %>% ungroup()
rdf$stay <- rdf$choice_numeric == rdf$choice_num_lead
rdf = rdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)) %>% ungroup()
rdf$stay_p <- NA
rdf$stay_p[rdf$stay] <- 1
rdf$stay_p[!rdf$stay] <- 0
rdf$past_rew <-
  recode(rdf$reinf_lag, `0` = "After omission", `1` = "After reward")
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


# make sure lags are correctly aligned
lag_test <- rdf[,c(1:3,6:13,122:dim(rdf)[2])]
View(lag_test)


rdf$Group <-
  recode(
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
  recode(
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

# replicate value + H + last reward analyses from fMRI sample

# final model before sensitivity analyses: OK, some group differences, especially w/o entropy
r_sm2mfxG <-   glmer(
  stay ~ reinf *Group + reinf* H_vba_mfx + v_chosen_lag_mfx_mc*H_vba_mfx*Group + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = rdf,
  nAGQ = 0)
summary(r_sm2mfxG)
car::Anova(r_sm2mfxG)

# group differences in value A<D=HC stand after controlling for beta and fit
r_sm2mfxGbetaL <-   glmer(
  stay ~ reinf*h_mfx_mc + reinf * Group + reinf * beta_vba_mfx + reinf * L_vba_mfx + 
    v_chosen_lag_mfx_mc*Group*h_mfx_mc + 
    v_chosen_lag_mfx_mc*beta_vba_mfx*h_mfx_mc + 
    v_chosen_lag_mfx_mc*L_vba_mfx*h_mfx_mc + 
    trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = rdf,
  nAGQ = 0)
summary(r_sm2mfxGbetaL)
car::Anova(r_sm2mfxGbetaL)

r_m_old <-   glmer(
  stay ~ reinf*h_mfx_mc + reinf * Group + value_chosen_vba_mfx*Group*h_mfx_mc + trial_scaled + stay_lag +
    (1 | ID),
  family = binomial(),
  data = rdf,
  nAGQ = 0)
summary(r_m_old)
car::Anova(r_m_old)

ls <- lsmeans(r_m_old,"value_chosen_vba_mfx", by = c("Group","h_mfx_mc"), at = list(value_chosen_vba_mfx = c(0.1,0.9), h_mfx_mc = c(-1,1)))
plot(ls, horiz = F)


## some heritage plots

library(lsmeans)
ls_rvm2 <-
  lsmeans(rvm2,
          "v_chosen_lag",
          by = "Group",
          at = list(v_chosen_lag = c(0.01, 0.99)))
plot(ls_rvm2,
     type ~ stay,
     horiz = F,
     ylab = "logit(probability of staying)",
     xlab = "Value")
leastsquare = lsmeans(
  rvm2,
  at = list(v_chosen_lag = c(0.01, 0.99)),
  pairwise ~ v_chosen_lag:Group,
  adjust = "tukey"
)
CLD = cld(leastsquare,
          alpha = 0.05,
          Letters = letters,
          adjust = "tukey")
###  Remove spaces in .group
CLD$.group = gsub(" ", "", CLD$.group)

pdf(file = "value on choice by group PRETTY 2.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(
  x     = v_chosen_lag,
  y     = lsmean,
  color = Group
  ,
  label = .group
)) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,
    size  =  0.7,
    position = pd
  ) +
  geom_line(position = pd) +
  facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Logit probability of repeating the choice") +
  xlab("Expected value") +
  scale_x_continuous(breaks = c(0, 1)) +
  ggtitle ("Effect of learned value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(
    caption  = paste0(
      "A=C: z=-0.5, A<D: z=12.5***, A<I: z=4.1***, ***p<.00001\n",
      "Boxes: LS mean.",
      "Error bars: 95% CI, \n",
      "Means sharing a letter are ",
      "not significantly different. \n",
      "(Sidak method for 8 estimates)."
    ),
    hjust = 0.5
  ) +
  theme_bw(base_size = 20) +
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
  geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


# what about lethality, controlling for age
lrvm2 <-
  glmer(
    stay ~  GroupLeth * v_chosen_lag + GroupLeth * trial_scaled + age_scaled *
      v_chosen_lag + age_scaled * trial_scaled +
      (1 |
         ID),
    family = binomial(),
    data = rdf,
    nAGQ = 0
  )
summary(lrvm2)
car::Anova(lrvm2)
ls_lrvm2 <-
  lsmeans(lrvm2,
          "v_chosen_lag",
          by = "Group12467",
          at = list(v_chosen_lag = c(0.01, 0.50, 0.99)))
plot(ls_lrvm2,
     type ~ stay,
     horiz = F,
     ylab = "logit(probability of staying)",
     xlab = "Value")

leastsquare = lsmeans(
  lrvm2,
  at = list(v_chosen_lag = c(0.01, 0.99)),
  pairwise ~ v_chosen_lag:GroupLeth,
  adjust = "tukey"
)
CLD = cld(leastsquare,
          alpha = 0.05,
          Letters = letters,
          adjust = "tukey")
###  Remove spaces in .group
CLD$.group = gsub(" ", "", CLD$.group)

pdf(file = "value on choice by groupLeth 2.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD,
       aes(
         x     = v_chosen_lag,
         y     = lsmean,
         color = GroupLeth
         ,
         label = .group
       )) +
  geom_point(shape  = 15,
             size   = 4,
             position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,
    size  =  0.7,
    position = pd
  ) +
  geom_line(position = pd) +
  facet_wrap( ~ GroupLeth, nrow = 1) +
  theme_bw() +
  theme(
    axis.title   = element_text(face = "bold"),
    axis.text    = element_text(face = "bold"),
    plot.caption = element_text(hjust = 0)
  ) +
  ylab("Logit probability of repeating the choice") +
  xlab("Expected value") +
  scale_x_continuous(breaks = c(0, 1)) +
  ggtitle ("Effect of learned value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(
    caption  = paste0(
      "HLA=C: z=0.4, HLA<D: z=12.0***, HLA<I: z=4.5***, HLA<LLA: z=2.7**, \n",
      "***p<.00001, **p<.01\n",
      "Boxes: LS mean.",
      "Error bars: 95% CI, \n",
      "Means sharing a letter are ",
      "not significantly different. ",
      "(Sidak method for 8 estimates)."
    ),
    hjust = 0.5
  ) +
  theme_bw(base_size = 16) +
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
  geom_text(color   = "black", nudge_y = .5) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


agelrvm2 <-
  glmer(
    stay ~  age_scaled * v_chosen_lag + age_scaled * trial_scaled +
      (1 |
         ID),
    family = binomial(),
    data = rdf,
    nAGQ = 0
  )



# do attempters have lower max value overall? Actually, NS higher
rvcheck1 <-
  lmer(value_max ~ trial_scaled + Group + (1 |
                                             ID), data = rdf)
summary(rvcheck1)
car::Anova(rvcheck1)

# do they chose a lower value overall?  NO
rvcheck2 <-
  lmer(value_chosen ~ trial_scaled + Group + (1 |
                                                ID), data = rdf)
summary(rvcheck2)
car::Anova(rvcheck2)




save(list = ls(all.names = TRUE), file = "bandit2.RData")
# in case this script needs to be extended:
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit2.RData")
