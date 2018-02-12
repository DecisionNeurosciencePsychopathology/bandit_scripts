# import, check and prepare for analyses data from two bandit samples

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

###################
# merge and check
trial_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_fMRI_df_with_PE/bandit_df1.csv")
View(trial_df)
sub_df <-
  read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/bandit_fMRI_df_with_PE/bandit_df2.csv")
View(sub_df)
sub_df = sub_df %>% as_tibble %>% arrange(ID)

sub_df$group1245 <- as.factor(sub_df$group1245)
sub_df$group12467 <- as.factor(sub_df$group12467)

# identify subjects who pressed the same button >10 times
# AD checked on final sample of N=127 01/18/18 
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
chars <- as.data.frame(sub_df[!sub_df$bad, c(9:13,2:3,18,43:46)])
c1 <-
  compareGroups(
    chars,
    y = sub_df$group1245[!sub_df$bad],
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

###########################
# preprocess, compute vars

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
# value difference for RT analyses
bdf$vmaxdiff <- bdf$v1 - bdf$v2 - bdf$v3

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

# end of fMRI sample preprocessing

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

##############################
##########
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

#identify subjects who pressed the same button >10 times
# AD's final inspection on 01/18/18, N=286
bad_ids <-
  c(206270,
    209460,
    209468,
    210548,
    211705,
    212385,
    213227,
    215644, # after trial 150
    218207, # after trial 150
    221273, # not in final sample
    29829,
    881091,
    45709)


beh_sub_df$bad <-
  is.element(beh_sub_df$ID, bad_ids)
beh_sub_df$AnxietyLifetime <-
  as.factor(beh_sub_df$AnxietyLifetime)
beh_sub_df$SubstanceLifetime <-
  as.factor(beh_sub_df$SubstanceLifetime)
table(beh_sub_df$Group,beh_sub_df$bad)


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
chars <- as.data.frame(c2[, c(2:3,9:14,18,43:46)])
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
  beh_sub_df$age > 73 & beh_sub_df$group1245 < 4

under50 <-
  beh_sub_df$age < 50


c4 <-
  beh_sub_df[!is.element(beh_sub_df$ID, repeaters) &
               !beh_sub_df$bad &
               !old_contr_depressed,]
chars <- as.data.frame(c4[, c(2:3,9:14,18,43:46)])
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

# let's try the unique sample first
rdf <- merge(beh_trial_df, c4)


# View(rdf)
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
# lag_test <- rdf[,c(1:3,6:13,122:dim(rdf)[2])]
# View(lag_test)


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


save(list = ls(all.names = TRUE), file = "bandit1.RData")
# in case this script needs to be extended:
# load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit2.RData")
