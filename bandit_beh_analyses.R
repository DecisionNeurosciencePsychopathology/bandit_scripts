#  related ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper

setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R")
library(readr)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
library(R.matlab)
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

#load(file="dataframe_for_entropy_analysis_Oct2016.RData")
#this contains data with 24 basis functions and post-Niv learning rule
# load(file="dataframe_for_entropy_analysis_Nov2016.RData")

library(readr)
trial_df <- read_csv("~/code/bandit_scripts/bandit_df1.csv")
View(trial_df)
sub_df <- read_csv("~/code/bandit_scripts/bandit_df2.csv")
View(sub_df)
sub_df$group1245 <- as.factor(sub_df$group1245)
sub_df$group12467 <- as.factor(sub_df$group12467)

# check missing data
library(VIM)
missing_ind_chars = aggr(sub_df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(sub_df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# all missingness <8%, could impute

# sample characteristics: looks reasonable
chars <- as.data.frame(sub_df[,c(9:26)])
c1 <- compareGroups(chars,y = sub_df$group1245, bivar=TRUE, include.miss = FALSE)
t1 <- createTable(c1,hide = NA, hide.no = 0, digits = 1, show.n = TRUE)
export2html(t1, "t_bandit_beh_by_group.html")

# coarse overview of behavior
hist(sub_df$spont_switch_err, breaks = 50)
hist(sub_df$prob_switch_err, breaks = 50)
hist(sub_df$erratic_spont, breaks = 50)
hist(sub_df$error_NOS)

# summary(m1 <- lm(spont_switch_err ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot, data = sub_df))
# anova(m1)
summary(m2 <- lm(error_NOS ~ group1245 + education + WTAR_SCALED_SCORE + EXITtot, data = sub_df))
anova(m2)
summary(m3 <- glm.nb(spont_switch_err ~ group1245 +  WTAR_SCALED_SCORE + EXITtot, data = sub_df))
car::Anova(m3, type = 'III')


# merge trial-by-trial and subject-level data
bdf <- merge(trial_df,sub_df)

summary(bdf)

bdf$stake <- as.factor(bdf$stake)
bdf$reward <- as.factor(bdf$reward)
bdf$comp_trials <- as.factor(bdf$comp_trials)
bdf$mystery_trials <- as.factor(bdf$mystery_trials)
bdf$reinf <- as.factor(bdf$correct_incorrect)
bdf$choice_numeric <- as.factor(bdf$choice_numeric)
bdf$choice_numeric[bdf$choice_numeric==0] <- NA
bdf$iq_scaled <- scale(bdf$WTAR_SCALED_SCORE,center = TRUE, scale = TRUE)
bdf$exit_scaled <- scale(bdf$EXITtot,center = TRUE, scale = TRUE)
# add multinom analyses looking at how magnitude of reward influences choice probability (nnet package)

# get_lags
bdf = bdf %>% group_by(ID) %>%
  mutate(stake_lag = lag(stake, order_by=ID), 
         reinf_lag = lag(reinf, order_by=ID),
         choice_lag = lag(multinomial_choice, order_by=ID),
         choice_num_lag = lag(choice_numeric, order_by=ID)
                           ) %>% ungroup()
bdf$stay <- bdf$choice_numeric==bdf$choice_num_lag
bdf$stay_p[bdf$stay] <- 1
bdf$stay_p[!bdf$stay] <- 0
bdf$Group <- recode(bdf$group1245, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `5` = "Attempters")
contrasts(bdf$Group) <- contr.treatment(levels(bdf$Group),
                                           base=which(levels(bdf$Group) == 'Attempters'))
bdf$past_rew <- recode(bdf$reinf_lag, `0` = "After omission", `1` = "After reward")


View(bdf)
bdf$trial_scaled <- scale(bdf$Trial)

prerevA <- subset(bdf,Trial<150 & choice_lag=="A")
postrev <- subset(bdf,Trial>150)


sm1 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + 
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(sm1)
car::Anova(sm1)
# ls_sm1 <- lsmeans(sm1,"rew_lag", by = "mag_lag", at = list(mag_lag = c(10,25,50)))
ls_sm1 <- lsmeans(sm1,"reinf_lag", by = "stake_lag")
plot(ls_sm1, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "reinforcement")


# start looking at individual differences, starting with cognitive characteristics
im1 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + exit_scaled*reinf_lag +  iq_scaled*reinf_lag + group1245*reinf_lag + 
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(im1)
car::Anova(im1)

im2 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + group1245*reinf_lag + group1245*trial_scaled + 
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(im2)
car::Anova(im2)
ls_im2 <- lsmeans(im2,"trial_scaled", by = "group1245", at = list(trial_scaled=c(-1.5,0,1.5)))
plot(ls_im2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")

# is this really the same for reinforced and unreinforced trials?
ls_im2a <- lsmeans(im2,"trial_scaled", by = "reinf_lag", at = list(trial_scaled=c(-1.5,0,1.5)))
plot(ls_im2a, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")

anova(im2,im1)

im2post <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + group1245*reinf_lag + group1245*trial_scaled + 
                   (1|ID), family = binomial(), data = postrev, nAGQ = 0)
summary(im2post)
car::Anova(im2post)
ls_im2post <- lsmeans(im2post,"trial_scaled", by = "group1245", at = list(trial_scaled=c(0,1,2)))
plot(ls_im2post, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Time after reversal")


# what about controlling for IQ and EXIT?
im3 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + group1245*reinf_lag + group1245*trial_scaled + exit_scaled*trial_scaled + iq_scaled*trial_scaled + 
               (1|ID), family = binomial(), data = bdf, nAGQ=0)
summary(im3)
car::Anova(im3)
anova(im3, im3)
ls_im3 <- lsmeans(im3,"trial_scaled", by = "group1245", at = list(trial_scaled=c(-1.5,0,1.5)))
plot(ls_im3, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")

# does the post-reversal deficit stand controlling for IQ and EXIT?
im4 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled + 
               (1|ID), family = binomial(), data = postrev, nAGQ = 0)
# just a sensitivity analysis with EXIT and WTAR, because some of the scores are missing
im4a <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled + exit_scaled*trial_scaled +  iq_scaled*trial_scaled +
               (1|ID), family = binomial(), data = postrev, nAGQ = 0)

summary(im4)
car::Anova(im4)
ls_im4 <- lsmeans(im4,"trial_scaled", by = "Group", at = list(trial_scaled=c(0,1,2)))
plot(ls_im4, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Time after reversal")
cld(ls_im4)

# make pretty plot for presentation/future paper
library(multcompView)
leastsquare = lsmeans(im4,
                      pairwise ~ trial_scaled:Group,at = list(trial_scaled=c(0,1,2)),
                      adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group  
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "post-reversal switches by group PRETTY.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = trial_scaled,
         y     = lsmean,
         color = Group
         # ,label = .group
         )) +
  geom_point(shape  = 15, size   = 4, position = pd) +
    geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line() +
  facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Log-probability of repeating the choice") +
  xlab("Time after reversal (1 = 50 trials)") +
  scale_x_continuous(breaks=c(0,1,2)) +
  ggtitle ("Behavior after reversal",
           subtitle = "Binary logistic mixed-effects model") +
  labs(caption  = paste0(
      "\n",
      "Boxes indicate the LS mean.\n",
      "Error bars indicate the 95% ",
      "confidence interval of the LS mean, Sidak method for 12 estimates."),
    hjust = 0.5) #+
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
   # geom_text(color   = "black", nudge_y = 1) #+
  # scale_color_manual(values = c("blue", "red"))
dev.off()


# see if they switch to 3rd-best option pre-reversal after large rewards


em1 <- glmer(multinomial_choice=="C" ~ reinf_lag*stake_lag + trial_scaled + 
               (1|ID), family = binomial(), data = prerevA, nAGQ = 0)
summary(em1)
car::Anova(em1)
ls_em1 <- lsmeans(em1,"reinf_lag", by = "stake_lag")
plot(ls_em1, type ~ response, horiz=F,ylab = "logit(p_C_choice)", xlab = "reinforcement")

em2 <- glmer(multinomial_choice=="B" ~ reinf_lag*stake_lag + trial_scaled + 
               (1|ID), family = binomial(), data = prerevA, nAGQ = 0)
summary(em2)
car::Anova(em2)
ls_em2 <- lsmeans(em2,"reinf_lag", by = "stake_lag")
plot(ls_em2, type ~ response, horiz=F,ylab = "logit(p_B_choice)", xlab = "reinforcement")


# plot trialwise choice probability
pdf(file = "attempters exploring.pdf", width = 10, height = 6)
ggplot(na.omit(bdf), aes(x=Trial, y=stay_p, color = Group)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Probability of staying with the same choice") +
facet_wrap(~past_rew) #geom_jitter(alpha=0.2) +
dev.off()

save(list = ls(all.names = TRUE),file = "bandit1.RData")


