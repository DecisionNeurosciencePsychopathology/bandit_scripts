#  related ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits
#  running glmer with nAGQ = 0 to speed it up, can remove for final analysis for the paper

setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R")
library(readr)
library(lme4)
library(ggplot2)
library(dplyr)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
# library(ggbiplot)
library(corrplot)
library(lsmeans)
library(effects)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)

library(readr)
trial_df <- read_csv("~/code/bandit_scripts/data/scanner/bandit_df1.csv")
View(trial_df)
sub_df <- read_csv("~/code/bandit_scripts/data/scanner/bandit_df2.csv")
View(sub_df)
sub_df$group1245 <- as.factor(sub_df$group1245)
sub_df$group12467 <- as.factor(sub_df$group12467)

# quality check: 2 subjects repeatedly pressed the same button
sub_df$bad <- NA
sub_df$bad <- sub_df$ID==206270 | sub_df$ID==210100

table(sub_df$bad,sub_df$group1245)
# check missing data
library(VIM)
# missing_ind_chars = aggr(sub_df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(sub_df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
library(mice)
missing_ind_chars = aggr(sub_df, col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(sub_df), cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))

# all missingness <8%, could impute

# sample characteristics: looks reasonable
chars <- as.data.frame(sub_df[,c(9:12,14,20,22)])
c1 <- compareGroups(chars,y = sub_df$group1245, bivar=TRUE, include.miss = FALSE)
t1 <- createTable(c1,hide = c(sex = "FEMALE"),  hide.no = 0, digits = 0, show.n = TRUE)
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
# summary(m3 <- glm.nb(spont_switch_err ~ group1245 +  WTAR_SCALED_SCORE + EXITtot, data = sub_df))
# car::Anova(m3, type = 'III')


# merge trial-by-trial and subject-level data
bdf <- merge(trial_df,sub_df)
bdf = bdf %>% as_tibble %>% arrange(ID,Trial)

bdf$stake <- as.factor(bdf$stake)
bdf$reward <- as.factor(bdf$reward)
bdf$comp_trials <- as.factor(bdf$comp_trials)
bdf$mystery_trials <- as.factor(bdf$mystery_trials)
bdf$reinf <- as.factor(bdf$correct_incorrect)
bdf$choice_numeric <- as.factor(bdf$choice_numeric)
bdf$choice_numeric[bdf$choice_numeric==0] <- NA
bdf$iq_scaled <- scale(bdf$WTAR_SCALED_SCORE,center = TRUE, scale = TRUE)
bdf$age_scaled <- scale(bdf$age,center = TRUE, scale = TRUE)
bdf$exit_scaled <- scale(bdf$EXITtot,center = TRUE, scale = TRUE)
bdf$reinf_n <- as.numeric(bdf$correct_incorrect)
# add multinom analyses looking at how magnitude of reward influences choice probability (nnet package)

# get_lags
bdf = bdf %>% group_by(ID) %>%
  mutate(stake_lag = lag(stake),
         reinf_lag = lag(reinf),
         choice_lag = lag(multinomial_choice),
         choice_num_lag = lag(choice_numeric),
         v_chosen_lag = lag(value_chosen),
         v_max_lag = lag(value_max),
         v_chosen_lag_f = lag(value_chosen_fixed_params),
         h_lag_f = lag(H_fixed_params)
                                    ) %>% ungroup()
bdf$stay <- bdf$choice_numeric==bdf$choice_num_lag
bdf$stay_p[bdf$stay] <- 1
bdf$stay_p[!bdf$stay] <- 0
bdf = bdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)  ) %>% ungroup()

bdf = bdf %>% group_by(ID) %>%
  mutate(h_lag = lag(H)  ) %>% ungroup()

# graphical sanity checks on lagged variables, because order_by=Trial does not seem to work here
# id <-  unique(bdf$ID)[33]
# i <- bdf$ID==id
# d <- 'NA'
# d$trial <- bdf$Trial[i]
# d$stay <- bdf$stay[i]
# d$stay_lag <- bdf$stay[i]
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$value_chosen[i], color = "v_chosen")) + geom_line(aes(y = bdf$v_chosen_lag[i], color = "v_chosen_lag"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$value_max[i], color = "v_max")) + geom_line(aes(y = bdf$v_max_lag[i], color = "v_max_lag"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_line(aes(y = bdf$value_chosen_fixed_params[i], color = "v_chosen_f")) + geom_line(aes(y = bdf$v_chosen_lag_f[i], color = "v_chosen_lag_f"))
# ggplot(bdf[i,], aes(x = Trial)) + geom_point(aes(y = bdf$stay[i], color = "stay")) + geom_point(aes(y = bdf$stay_lag[i], color = "stay_lag"))


bdf$Group <- recode(bdf$group1245, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `5` = "Attempters")
contrasts(bdf$Group) <- contr.treatment(levels(bdf$Group),
                                           base=which(levels(bdf$Group) == 'Attempters'))

sub_df$Group <- recode(sub_df$group1245, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `5` = "Attempters")
contrasts(sub_df$Group) <- contr.treatment(levels(sub_df$Group),
                                        base=which(levels(bdf$Group) == 'Attempters'))


bdf$past_rew <- recode(bdf$reinf_lag, `0` = "After omission", `1` = "After reward")


View(bdf)


bdf$trial_scaled <- scale(bdf$Trial)

# exclude subjects who pressed one button repeatedly
gdf <- bdf[!bdf$bad,]

# check distribution of performance across subjects
perf <- summarise(group_by(bdf,ID),mean(reinf_n))
hist(perf, nclass=100)
describe(perf$`mean(reinf_n)`)

already_identified_bad_subject <- perf$ID[perf$`mean(reinf_n)`<.2]
additional_bad_subjects <- perf$ID[perf$`mean(reinf_n)`<.3 &perf$`mean(reinf_n)`>.2]
# OK, there were two additional people, who performed below chance, 211101 and 213868
# a total of 15 people performed poorly, but could have been poor learners who actually tried/wrong strategy
questionable_subjects <- perf$ID[perf$`mean(reinf_n)`<.4]

udf <- bdf[!is.element(bdf$ID, questionable_subjects),]

# RL model parameters across groups
ggplot(data = sub_df,aes(y = alpha_win,x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df,aes(y = alpha_loss,x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df,aes(y = decay,x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df,aes(y = beta,x = Group, color = Group)) + geom_boxplot() + geom_jitter()
ggplot(data = sub_df,aes(y = L,x = Group, color = Group)) + geom_boxplot() + geom_jitter()


pm1 <- manova(cbind(alpha_win, alpha_loss, decay, beta) ~ Group, data = sub_df)
summary(pm1)
anova(pm1)

pm2 <- lm(alpha_win ~ Group, data = sub_df)
summary(pm2)
anova(pm2)

pm3 <- lm(alpha_loss ~ Group, data = sub_df)
summary(pm3)
anova(pm3)

pm4 <- lm(decay ~ Group, data = sub_df)
summary(pm4)
anova(pm4) #*

pm5 <- lm(beta ~ Group, data = sub_df)
summary(pm5)
anova(pm5)
ggplot(data = sub_df,aes(y = beta,x = Group, color = Group)) + geom_boxplot() + geom_jitter()

pm6 <- lm(L ~ Group, data = sub_df)
summary(pm6)
anova(pm6)


prerevA <- subset(bdf,Trial<150 & choice_lag=="A")
prerev <- subset(bdf,Trial<150)
postrev <- subset(bdf,Trial>150)


sm1 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(sm1)
car::Anova(sm1)
# ls_sm1 <- lsmeans(sm1,"rew_lag", by = "mag_lag", at = list(mag_lag = c(10,25,50)))
ls_sm1 <- lsmeans(sm1,"reinf_lag", by = "stake_lag")
plot(ls_sm1, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "reinforcement")


# does value help predict better than reinforcement?
sm2 <- glmer(stay ~ v_chosen_lag*stake_lag + stake + trial_scaled +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(sm2)
car::Anova(sm2)

anova(sm1,sm2)

sm3 <- glmer(stay ~ v_max_lag*stake_lag + stake + trial_scaled +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(sm3)
car::Anova(sm3)

anova(sm1,sm2,sm3)

# multinomial choice by group
bdf$multinomial_choice <- as.factor(bdf$multinomial_choice)
mm1 <- glmer(multinomial_choice ~ past_rew*stake_lag*trial_scaled + past_rew*I(trial_scaled^2) + stake +
               age*trial_scaled + age*past_rew + Group*past_rew*trial_scaled + Group*past_rew*I(trial_scaled^2) +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(mm1)
car::Anova(mm1)
ls_mm1 <- lsmeans(mm1, "trial_scaled", by = "Group", at = list(trial_scaled = c(-2,0,2)))
plot(ls_mm1, horiz = F)
# start looking at individual differences, starting with cognitive characteristics
im1 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + exit_scaled*reinf_lag +  iq_scaled*reinf_lag + group1245*reinf_lag +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(im1)
car::Anova(im1)

im2 <- glmer(stay ~ past_rew*stake_lag + stake + trial_scaled + Group*past_rew + Group*trial_scaled +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(im2)
car::Anova(im2)
ls_im2 <- lsmeans(im2,"past_rew", by = "Group")
plot(ls_im2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")

leastsquare = lsmeans(im2, pairwise ~ past_rew:Group,adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "reinf on choice by group orig.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = past_rew,
                y     = lsmean,
                color = Group
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Reinforcement on last trial") +
  scale_x_discrete(labels=c("After reward" = "Yes", "After omission" = "No")) +
  ggtitle ("Effect of reinforcement on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "A<C: z=1.9, p=0.058, A<D: z=1.5, p=0.13, A<I: z=2.6,  p=.002\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n"),
    hjust = 0.5) +
  theme_bw(base_size=20) #+
# geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
#           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
#           color   = "black") +
# geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()



# is this really the same for reinforced and unreinforced trials?
ls_im2a <- lsmeans(im2,"trial_scaled", by = "past_rew", at = list(trial_scaled=c(-1.5,0,1.5)))
plot(ls_im2a, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")

# anova(im2,im1)

# do attempters have lower max value overall? NO
vcheck1 <- lmer(value_max ~ trial_scaled + Group + (1|ID), data = bdf)
summary(vcheck1)
car::Anova(vcheck1)

# do they chose a lower value overall?  NO
vcheck2 <- lmer(value_chosen ~ trial_scaled + Group + (1|ID), data = bdf)
summary(vcheck2)
car::Anova(vcheck2)

bdf$stay_lag <- as.factor(bdf$stay_lag)
# build the best-fitting, but somewhat principled model of value-based choice
vm1 <- glmer(stay ~ v_chosen_lag*stake_lag + stake + trial_scaled + stay_lag +
               (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1)
car::Anova(vm1)

ef1 <- allEffects( vm1)



vm1a <- glmer(stay ~ v_chosen_lag*stake_lag + v_chosen_lag*stake + trial_scaled + stay_lag +
               (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1a)
car::Anova(vm1a)
anova(vm1,vm1a)

vm1b <- glmer(stay ~ v_chosen_lag*stake_lag + v_chosen_lag*stake + stay_lag +
                (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1b)
car::Anova(vm1b)
anova(vm1a,vm1b)

vm1c <- glmer(stay ~ v_chosen_lag*stake_lag*stake + trial_scaled + stay_lag +
                (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1c)
car::Anova(vm1c)
anova(vm1a,vm1c)

# this is a pretty good working model
vm1d <- glmer(stay ~ v_chosen_lag*stake_lag*stake + v_chosen_lag*Group + trial_scaled + stay_lag +
                (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1d)
car::Anova(vm1d)
# the addition of group improves the model
anova(vm1d,vm1c)

# include trial as random
vm1r <- glmer(stay ~ v_chosen_lag*stake_lag*stake + v_chosen_lag*Group + trial_scaled*stay_lag +
                (1 + trial_scaled|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm1r)
car::Anova(vm1r)
plot(allEffects(vm1r))
# the addition of group improves the model
anova(vm1d,vm1r)


# reasonably simple model with plausible predictors
vm2 <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag + trial_scaled + v_chosen_lag*stake_lag + stake + Group*v_chosen_lag + stay_lag +
               (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm2)
car::Anova(vm2)
ls_vm2 <- lsmeans(vm2,"v_chosen_lag", by = "Group", at = list(v_chosen_lag=c(0.01,0.50,0.99)))
plot(ls_vm2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value")
plot(allEffects( vm2))

# what about vmax?
vm2m <- glmer(stay ~ stay_lag + stake + stake_lag + v_max_lag + trial_scaled + v_max_lag*stake_lag + stake + Group*trial_scaled + Group*v_max_lag + stay_lag +
               (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm2m)
car::Anova(vm2m)
plot(allEffects(vm2m))

# v_chosen predicts astronomically better than v_max: AIC 15983 vs.22334
# forget v_max
anova(vm2m,vm2)

ls_vm2 <- lsmeans(vm2,"v_chosen_lag", by = "Group", at = list(v_chosen_lag=c(0.01,0.50,0.99)))
plot(ls_vm2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value")



# exclude people who pressed repeatedly
vm2good <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag + trial_scaled + v_chosen_lag*stake_lag + stake + Group*trial_scaled + Group*v_chosen_lag + stay_lag +
                   (1|ID), family = binomial(), data = gdf,   nAGQ = 0)
summary(vm2good)
car::Anova(vm2good)

# exclude all bad performers
vm2unquest <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag + trial_scaled + v_chosen_lag*stake_lag + stake + Group*trial_scaled + Group*v_chosen_lag + stay_lag +
                      (1|ID), family = binomial(), data = udf,   nAGQ = 0)
summary(vm2unquest)
car::Anova(vm2unquest)

# control for model fit: stands, amazingly
vm2L <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag + trial_scaled + v_chosen_lag*stake_lag + stake + Group*trial_scaled + Group*v_chosen_lag + L*trial_scaled + L*v_chosen_lag + stay_lag +
               (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm2L)
car::Anova(vm2L)

# control for temperature: still stands
vm2beta <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag + trial_scaled + v_chosen_lag*stake_lag + stake + Group*trial_scaled + Group*v_chosen_lag + beta*trial_scaled + beta*v_chosen_lag + stay_lag +
                (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm2beta)
car::Anova(vm2beta)

# now, there is no way it will stand with fixed-parameters fit.  Well, they only differ from ideators, but the ordering is not dissimilar.
vm2fixed <- glmer(stay ~ stay_lag + stake + stake_lag + v_chosen_lag_f + trial_scaled + v_chosen_lag_f*stake_lag + stake + Group*trial_scaled + Group*v_chosen_lag_f + stay_lag +
                (1|ID), family = binomial(), data = bdf,   nAGQ = 0)
summary(vm2fixed)
car::Anova(vm2fixed)


# control for EXIT and IQ
vm3 <- glmer(stay ~ v_chosen_lag*stake_lag + stake + trial_scaled + Group*v_chosen_lag*trial_scaled + age*v_chosen_lag*trial_scaled +
               exit_scaled*v_chosen_lag*trial_scaled + iq_scaled*v_chosen_lag*trial_scaled + BIS_NONPLAN*v_chosen_lag*trial_scaled +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(vm3)
car::Anova(vm3)
ls_vm3 <- lsmeans(vm3,"v_chosen_lag", by = "Group", at = list(v_chosen_lag=c(0.01,0.50,0.99)))
plot(ls_vm3, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value")

# before reversal
vm2a_pre <- glmer(stay ~ v_chosen_lag*stake_lag + stake + trial_scaled + Group*v_chosen_lag + Group*trial_scaled +
                (1|ID), family = binomial(), data = prerev, nAGQ = 0)
summary(vm2a_pre)
car::Anova(vm2a_pre)

# after reversal
vm2a_post <- glmer(stay ~ v_chosen_lag*stake_lag + stake + trial_scaled + Group*v_chosen_lag + Group*trial_scaled +
                    (1|ID), family = binomial(), data = postrev, nAGQ = 0)
summary(vm2a_post)
car::Anova(vm2a_post)


# model-free demonstration: switch after pre-reversal A choices
mfm1 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled +
               (1|ID), family = binomial(), data = prerev, nAGQ = 0)
summary(mfm1)
car::Anova(mfm1)



# pretty plot


library(multcompView)
leastsquare = lsmeans(vm2,
                      pairwise ~ v_chosen_lag:Group,at = list(v_chosen_lag=c(0.1,0.9)),
                      adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "value on choice by group PRETTY 2.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = v_chosen_lag,
                y     = lsmean,
                color = Group
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
   geom_line(position = pd) +
   facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Expected value") +
  scale_x_continuous(breaks=c(0,1)) +
  ggtitle ("Effect of learned value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "A<C: z=4.6, A<D: z=6.1, A<I: z=4.1, all p<.0001\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n",
    "Means sharing a letter are ",
    "not significantly different. \n",
        "(Sidak method for 8 estimates)."),
    hjust = 0.5) +
   theme_bw(base_size=20) +
# geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
#           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
#           color   = "black") +
 geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


# im2post <- glmer(stay ~ past_rew*stake_lag + stake + trial_scaled + Group*past_rew + Group*trial_scaled +
#                    (1|ID), family = binomial(), data = postrev, nAGQ = 0)
# summary(im2post)
# car::Anova(im2post)
# ls_im2post <- lsmeans(im2post,"trial_scaled", by = "Group", at = list(trial_scaled=c(0,1,2)))
# plot(ls_im2post, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Time after reversal")
#
#
# # what about controlling for IQ and EXIT?
# im3 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled + exit_scaled*trial_scaled + iq_scaled*trial_scaled +
#                (1|ID), family = binomial(), data = bdf, nAGQ=0)
# summary(im3)
# car::Anova(im3)
# anova(im3, im3)
# ls_im3 <- lsmeans(im3,"trial_scaled", by = "Group", at = list(trial_scaled=c(-1.5,0,1.5)))
# plot(ls_im3, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Trial (early, middle, late in learning)")
#
# # does the post-reversal deficit stand controlling for IQ and EXIT?
# im4 <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled +
#                (1|ID), family = binomial(), data = postrev, nAGQ = 0)
# # just a sensitivity analysis with EXIT and WTAR, because some of the scores are missing
# im4a <- glmer(stay ~ reinf_lag*stake_lag + stake + trial_scaled + Group*reinf_lag + Group*trial_scaled + exit_scaled*trial_scaled +  iq_scaled*trial_scaled +
#                (1|ID), family = binomial(), data = postrev, nAGQ = 0)
#
# summary(im4)
# car::Anova(im4a)
# ls_im4 <- lsmeans(im4,"trial_scaled", by = "Group", at = list(trial_scaled=c(0,1,2)))
# plot(ls_im4, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Time after reversal")
# cld(ls_im4)
#
# do they win less? No.
rm1 <- glmer(reinf ~  Group*trial_scaled + Group*stake + Group*stake_lag +
               (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(rm1)
car::Anova(rm1)

# do they tend to stick with B?  Yep, they do.
bm1 <- glmer(multinomial_choice=="B" ~  Group*trial_scaled +
               (1|ID), family = binomial(), data = postrev)
summary(bm1)
car::Anova(bm1)
ls_bm1 <- lsmeans(bm1,"trial_scaled", by = "Group", at = list(trial_scaled=c(0,1,2)))
plot(ls_bm1, type ~ response, horiz=F,ylab = "logit(probability of chosing B)", xlab = "Time after reversal")


# after reversal?  Attempters are only worse than controls.
rm2 <- glmer(reinf ~  Group*trial_scaled +
               (1|ID), family = binomial(), data = postrev, nAGQ = 0)
summary(rm2)
car::Anova(rm2)


# make  plot for presentation/future paper
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
  ylab("Logit probability of repeating the choice") +
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
ggplot(na.omit(bdf), aes(x=Trial, y=stay_p, color = Group)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Probability of staying with the same choice") +
facet_wrap(~past_rew) #geom_jitter(alpha=0.2) +
dev.off()


# plot trialwise reward
pdf(file = "bandit wins by group.pdf", width = 10, height = 6)
ggplot(na.omit(bdf), aes(x=Trial, y=correct_incorrect, color = Group)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Probability of winning")
dev.off()

# plot choice
bdf$choiceA[bdf$multinomial_choice=="A"] <- 1
bdf$choiceB[bdf$multinomial_choice=="B"] <- 1
bdf$choiceC[bdf$multinomial_choice=="C"] <- 1
bdf$choiceA[bdf$multinomial_choice!="A"] <- 0
bdf$choiceB[bdf$multinomial_choice!="B"] <- 0
bdf$choiceC[bdf$multinomial_choice!="C"] <- 0

cdf = melt(bdf, na.rm = FALSE, measure.vars = c("choiceA","choiceB","choiceC"))
cdf$option <- cdf$variable
cdf$choice <- cdf$value
View(cdf)

ggplot(na.omit(cdf), aes(x=Trial, y=choice, color = option)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Choice probability")

ggplot(na.omit(cdf), aes(x=Trial, y=reinf_n, color = Group)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Choice probability")

ggplot(na.omit(cdf), aes(x=Trial, y=choice, color = Group)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Choice probability") +
  facet_wrap(~option, ncol = 2) #geom_jitter(alpha=0.2) +

ggplot(na.omit(cdf), aes(x=Trial, y=choice, color = option)) + stat_smooth(method="auto") + theme_gray(base_size=20) + ylab("Choice probability") +
  facet_wrap(~Group) #geom_jitter(alpha=0.2) +

ggplot(na.omit(bdf), aes(x=Trial, y=value_chosen, color = Group)) + stat_smooth(method="loess", alpha=0.3) + theme_gray(base_size=20) + ylab("Reward (behavioral study)")

ggplot(na.omit(bdf[,]), aes(x=v_chosen_lag, y=stay, color = Group)) + geom_count(position = "jitter", size = 1) + theme_grey(base_size=20) + ylab("Choice repeated?") + scale_color_brewer(palette="Set1")
ggplot(na.omit(bdf[as.numeric(bdf$group1245)<3,]), aes(x=v_chosen_lag, y=stay, color = Group)) + geom_count(position = "jitter", size = 2) + theme_gray(base_size=20) + ylab("Choice repeated?") +  scale_fill_brewer(palette="Spectral")

ggplot(na.omit(bdf[,c(1:17,42:55)]), aes(x=value_chosen, y=stay_p, color = Group)) + stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + theme_bw(base_size=20) + labs(x = "Chosen value", y = "Probability of staying with the same choice")


# do value and entropy have separately identifiable effects on choice?
ggplot(na.omit(bdf), aes(x = h_lag, y = stay_p, color = Group)) + stat_smooth(method  = "glm", method.args = list(family = "binomial"), se = TRUE)
ggplot(na.omit(bdf), aes(x = v_chosen_lag, y = stay_p, color = Group)) + stat_smooth(method  = "glm", method.args = list(family = "binomial"), se = TRUE)

ggplot(na.omit(bdf), aes(x = h_lag, y = v_chosen_lag, color = stay)) + stat_smooth(method  = "glm", method.args = list(family = "binomial"), se = TRUE)

ggplot(na.omit(bdf), aes(x = h_lag, y = v_chosen_lag, color = ID)) + geom_jitter() + facet_wrap(~stay)


###########
# entropy #
###########

vhm1 <- glmer(stay ~  v_chosen_lag*h_lag + trial_scaled + stay_lag + stake_lag + stake +
                (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(vhm1)
car::Anova(vhm1)

fvhm1 <- glmer(stay ~  v_chosen_lag_f*h_lag_f + trial_scaled + stay_lag + stake_lag + stake +
                 (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(fvhm1)
car::Anova(fvhm1)

anova(vhm1,fvhm1)

vhm2 <- glmer(stay ~  v_chosen_lag*h_lag + v_chosen_lag*Group + h_lag*Group + trial_scaled + stay_lag + stake_lag + stake +
                (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(vhm2)
car::Anova(vhm2)
ls_vhm2 <- lsmeans(vhm2, "Group", by = c("v_chosen_lag","h_lag"),  at = list(v_chosen_lag = c(0,0.5,1), h_lag = c(1.408,1.518,1.585)))
plot(ls_vhm2, horiz = F)

cld(ls_vhm2)

fvhm2 <- glmer(stay ~  v_chosen_lag_f*Group + h_lag_f*Group +  v_chosen_lag_f*age_scaled + h_lag_f*age_scaled + trial_scaled + stay_lag + stake_lag + stake +
                 (1|ID), family = binomial(), data = bdf, nAGQ = 0)
summary(fvhm2)
car::Anova(fvhm2)
anova(vhm2,fvhm2)



save(list = ls(all.names = TRUE),file = "bandit1.RData")

## proper replication using full behavioral data ________________________________________________________________________________________________________________________________________
# read in data form larger behavioral sample
beh_trial_df <- read_csv("~/code/bandit_scripts/data/beh/bandit_df1.csv")
View(beh_trial_df)
beh_sub_df <- read_csv("~/code/bandit_scripts/data/beh/bandit_df2.csv")
View(beh_sub_df)

beh_ids <- unique(beh_sub_df$ID)
# Josh told me that some peopel from BSocial were inadvertently included: exclude them, turns out to be just one person
bsocials <- as.integer(c(219084,211858,211008,211973,212298,220686,220622,220590,220521,220562))
beh_sub_df <- beh_sub_df[!is.element(beh_sub_df$ID,bsocials),]


#check ID overlap
scan_ids <- unique(bdf$ID)
repeaters <- intersect(beh_ids,scan_ids)
beh_sub_df$scanned <- is.element(beh_sub_df$ID,repeaters)

# identify subjects who pressed the same button >10 times
bad_ids <- c(206270, 209460, 210548, 212385, 211705, 213227, 215644, 218207, 29829, 881091)
# add individual characteristics from Laura's DB

beh_sub_df$bad <-  is.element(beh_sub_df$ID, bad_ids)
beh_sub_df$AnxietyLifetime <- as.factor(beh_sub_df$AnxietyLifetime)
beh_sub_df$SubstanceLifetime <- as.factor(beh_sub_df$SubstanceLifetime)
beh_sub_df$Group <- recode(beh_sub_df$group1245, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `5` = "Attempters")
beh_sub_df$Group <- as.factor(beh_sub_df$Group)
contrasts(beh_sub_df$Group) <- contr.treatment(levels(beh_sub_df$Group),
                                           base=which(levels(beh_sub_df$Group) == 'Attempters'))

# I know that a few controls were erroneously coded for substance/anxiety, correct
beh_sub_df$AnxietyLifetime[beh_sub_df$group1245==1] <- NA
beh_sub_df$SubstanceLifetime[beh_sub_df$group1245==1] <- NA

# remove missing WTARs
beh_sub_df$WTAR_SCALED_SCORE[beh_sub_df$WTAR_SCALED_SCORE>200] <- NA
# select all good behavioral subjects, including scanned, get group characteristics
c <- beh_sub_df[!beh_sub_df$bad,]
chars <- as.data.frame(c[,c(9:14,25:40)])
c2 <- compareGroups(chars,y = c$group1245, bivar=TRUE, include.miss = FALSE)
t2 <- createTable(c2,hide = c(sex = "FEMALE", list(race = c("WHITE", "ASIAN PACIFIC"))),  hide.no = 0, digits = 0, show.n = TRUE)
export2html(t2, "beh_t_bandit_beh_by_group.html")

# only unique subjects who have not been scanned
c2 <- beh_sub_df[!is.element(beh_sub_df$ID,repeaters) & !beh_sub_df$bad,]
chars <- as.data.frame(c2[,c(9:14,25:40)])
c3 <- compareGroups(chars,y = c2$group1245, bivar=TRUE, include.miss = FALSE)
t3 <- createTable(c3,  hide.no = 0, digits = 0, show.n = TRUE)
export2html(t3, "unique_beh_t_bandit_beh_by_group.html")

# try excluding older controls/depressed -- still
old_contr_depressed <- beh_sub_df$age>75 & beh_sub_df$group1245<4
c4 <- beh_sub_df[!is.element(beh_sub_df$ID,repeaters) & !beh_sub_df$bad & !old_contr_depressed,]
chars <- as.data.frame(c4[,c(9:14,25:40)])
c5 <- compareGroups(chars,y = c4$group1245, bivar=TRUE, include.miss = FALSE)
t5 <- createTable(c5,  hide.no = 0, digits = 0, show.n = TRUE)
export2html(t5, "age_equated_unique_beh_t_bandit_beh_by_group.html")

ggplot(data = beh_sub_df,aes(x = alpha_win,y = alpha_loss, color = Group)) + geom_jitter()
ggplot(data = beh_sub_df,aes(x = decay,y = beta, color = Group)) + geom_point()
ggplot(data = beh_sub_df,aes(y = decay,x = Group)) + geom_boxplot()

rpm1 <- manova(cbind(alpha_win, alpha_loss, decay, beta) ~ Group, data = beh_sub_df)
summary(rpm1)
anova(rpm1)

rpm2 <- lm(alpha_win ~ Group, data = beh_sub_df)
summary(rpm2)
anova(rpm2)

rpm3 <- lm(alpha_loss ~ Group, data = beh_sub_df)
summary(rpm3)
anova(rpm3)

rpm4 <- lm(decay ~ Group, data = beh_sub_df)
summary(rpm4)
anova(rpm4)

rpm5 <- lm(beta ~ Group, data = beh_sub_df)
summary(rpm5)
anova(rpm5)
ggplot(data = beh_sub_df,aes(y = beta,x = Group, color = Group)) + geom_boxplot() + geom_jitter()

rpm6 <- lm(L ~ Group, data = beh_sub_df)
summary(rpm6)
anova(rpm6)
ggplot(data = beh_sub_df,aes(y = L,x = Group, color = Group)) + geom_boxplot() + geom_jitter()


# let's try the unique sample first
c4 <- as.tibble(c4)
rdf <- merge(beh_trial_df,c4, by = "ID")
rdf <- as.tibble(rdf)
rdf = rdf %>% as_tibble %>% arrange(ID,Trial)
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
rdf$choice_numeric[rdf$choice_numeric==0] <- NA
rdf$age_scaled <- scale(rdf$age)
# get_lags
rdf = rdf %>% group_by(ID) %>%
  mutate(reinf_lag = lag(reinf),
         choice_lag = lag(multinomial_choice),
         choice_num_lag = lag(choice_numeric),
         v_chosen_lag = lag(value_chosen),
         v_max_lag = lag(value_max),
         v_chosen_lag_f = lag(value_chosen_fixed_params),
         h_lag_f = lag(H_fixed_params)
          ) %>% ungroup()
rdf$stay <- rdf$choice_numeric==rdf$choice_num_lag
rdf$stay_p[rdf$stay] <- 1
rdf$stay_p[!rdf$stay] <- 0
rdf = rdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay)  ) %>% ungroup()

rdf = rdf %>% group_by(ID) %>%
  mutate(h_lag = lag(H)  ) %>% ungroup()


rdf$Group <- recode(rdf$Group1245, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `5` = "Attempters")
contrasts(rdf$Group) <- contr.treatment(levels(rdf$Group),
                                        base=which(levels(rdf$Group) == 'Attempters'))
rdf$GroupLeth <- recode(rdf$Group12467, `1` = "Controls", `2` = "Depressed", `4` = "Ideators", `6` = "LL Attempters", `7` = "HL Attempters")
contrasts(rdf$GroupLeth) <- contr.treatment(levels(rdf$GroupLeth),
                                        base=which(levels(rdf$GroupLeth) == 'HL Attempters'))

rdf$past_rew <- recode(rdf$reinf_lag, `0` = "After omission", `1` = "After reward")
rdf$reinf_n <- as.numeric(rdf$correct_incorrect)


View(rdf)
rdf$trial_scaled <- scale(rdf$Trial)

# graphical sanity checks on lagged variables, because order_by=Trial does not seem to work here
id <-  unique(rdf$ID)[12]
i <- rdf$ID==id
d <- 'NA'
d$trial <- rdf$Trial[i]
d$stay <- rdf$stay[i]
d$stay_lag <- rdf$stay[i]
ggplot(rdf[i,], aes(x = Trial)) + geom_line(aes(y = rdf$value_chosen[i], color = "v_chosen")) + geom_line(aes(y = rdf$v_chosen_lag[i], color = "v_chosen_lag"))
ggplot(rdf[i,], aes(x = Trial)) + geom_line(aes(y = rdf$value_max[i], color = "v_max")) + geom_line(aes(y = rdf$v_max_lag[i], color = "v_max_lag"))
ggplot(rdf[i,], aes(x = Trial)) + geom_line(aes(y = rdf$value_chosen_fixed_params[i], color = "v_chosen_f")) + geom_line(aes(y = rdf$v_chosen_lag_f[i], color = "v_chosen_lag_f"))


rprerevA <- subset(rdf,Trial<150 & choice_lag=="A")
rpostrev <- subset(rdf,Trial>150)

rem1 <- glmer(stay ~ reinf_lag*Group + trial_scaled*Group +
               (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rem1)
car::Anova(rem1)
# ls_sm1 <- lsmeans(sm1,"rew_lag", by = "mag_lag", at = list(mag_lag = c(10,25,50)))
ls_rem1 <- lsmeans(rem1,"reinf_lag", by = "Group")
plot(ls_rem1, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Reward")

# covary for age
rem2 <- glmer(stay ~ past_rew*Group*trial_scaled + past_rew*age_scaled*trial_scaled +
                (1|ID), family = binomial(), data = rdf,nAGQ = 0)
summary(rem2)
car::Anova(rem2)
ls_rem2 <- lsmeans(rem2,"Group", by = "past_rew")
plot(ls_rem2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Group")

stargazer(rem2,  type="html",  out="rep_rem2.htm", digits = 2 ,single.row=TRUE,  star.cutoffs = c(0.05, 0.01, 0.001,0.0001), report = 'vcs*')


leastsquare = lsmeans(rem2, pairwise ~ past_rew:Group,adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "reinf on choice by group rep PRETTY.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = past_rew,
                y     = lsmean,
                color = Group
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Reinforcement on last trial") +
  scale_x_discrete(labels=c("After reward" = "Yes", "After omission" = "No")) +
  ggtitle ("Effect of reinforcement on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "A<C: z=14.0, A<D: z=6.7, A<I: z=3.4, all p<.001\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n"),
    hjust = 0.5) +
  theme_bw(base_size=20) #+
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
  # geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


# check by lethality
lrem2 <- glmer(stay ~ past_rew*GroupLeth*trial_scaled + past_rew*age_scaled*trial_scaled +
                (1|ID), family = binomial(), data = rdf,nAGQ = 0)
summary(lrem2)
car::Anova(lrem2)
ls_lrem2 <- lsmeans(lrem2,"GroupLeth", by = c("past_rew", "trial_scaled"), at = list(trial_scaled = c(-2,0,2)))
plot(ls_lrem2, type ~ stay | GroupLeth, horiz=F,ylab = "logit(probability of staying)", xlab = "Group")

leastsquare = lsmeans(lrem2, pairwise ~ past_rew:GroupLeth:trial_scaled,adjust="tukey", at = list(trial_scaled = c(-2,0,2)))
CLD = cld(ls_lrem2,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "reinf on choice by group rep PRETTY.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = trial_scaled,
                y     = lsmean,
                color = past_rew
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  facet_wrap( ~ GroupLeth, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Trial") +
  scale_x_continuous(breaks=c(-2,0,2), labels=c(1,150,300)) +
  ggtitle ("Effect of reinforcement on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n"),
    hjust = 0.5) +
  theme_bw(base_size=20) #+
# geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
#           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
#           color   = "black") +
# geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


rem2post <- glmer(stay ~ reinf_lag + trial_scaled + Group*reinf_lag + Group*trial_scaled + reinf_lag*BASELINEAGE + trial_scaled*BASELINEAGE +
                   (1|ID), family = binomial(), data = rpostrev, nAGQ = 0)
summary(rem2post)
car::Anova(rem2post)


# use v_chosen instead of reinf
rvm2 <- glmer(stay ~  Group*v_chosen_lag + Group*trial_scaled + stay_lag +
               (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rvm2)
car::Anova(rvm2)

# controlling for WTAR, EXIT, age, and impulsivity
rvm3 <- glmer(stay ~  Group*v_chosen_lag + Group*trial_scaled +
                WTAR_SCALED_SCORE*v_chosen_lag + WTAR_SCALED_SCORE*v_chosen_lag +
                EXITtot*v_chosen_lag + EXITtot*trial_scaled +
                BIS_NONPLAN*v_chosen_lag + BIS_NONPLAN*trial_scaled +
                (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rvm3)
car::Anova(rvm3)


library(lsmeans)
ls_rvm2 <- lsmeans(rvm2,"v_chosen_lag", by = "Group", at = list(v_chosen_lag=c(0.01,0.99)))
plot(ls_rvm2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value")
leastsquare = lsmeans(rvm2, at = list(v_chosen_lag=c(0.01,0.99)), pairwise ~ v_chosen_lag:Group,adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

pdf(file = "value on choice by group PRETTY 2.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = v_chosen_lag,
                y     = lsmean,
                color = Group
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Expected value") +
  scale_x_continuous(breaks=c(0,1)) +
  ggtitle ("Effect of learned value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "A=C: z=-0.5, A<D: z=12.5***, A<I: z=4.1***, ***p<.00001\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n",
    "Means sharing a letter are ",
    "not significantly different. \n",
    "(Sidak method for 8 estimates)."),
    hjust = 0.5) +
  theme_bw(base_size=20) +
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
  geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


# what about lethality, controlling for age
lrvm2 <- glmer(stay ~  GroupLeth*v_chosen_lag + GroupLeth*trial_scaled + age_scaled*v_chosen_lag + age_scaled*trial_scaled +
                (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(lrvm2)
car::Anova(lrvm2)
ls_lrvm2 <- lsmeans(lrvm2,"v_chosen_lag", by = "GroupLeth", at = list(v_chosen_lag=c(0.01,0.50,0.99)))
plot(ls_lrvm2, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value")

leastsquare = lsmeans(lrvm2, at = list(v_chosen_lag=c(0.01,0.99)), pairwise ~ v_chosen_lag:GroupLeth,adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

pdf(file = "value on choice by groupLeth 2.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
ggplot(CLD, aes(x     = v_chosen_lag,
                y     = lsmean,
                color = GroupLeth
                ,label = .group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  asymp.LCL,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  facet_wrap( ~ GroupLeth, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Expected value") +
  scale_x_continuous(breaks=c(0,1)) +
  ggtitle ("Effect of learned value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "HLA=C: z=0.4, HLA<D: z=12.0***, HLA<I: z=4.5***, HLA<LLA: z=2.7**, \n",
    "***p<.00001, **p<.01\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n",
    "Means sharing a letter are ",
    "not significantly different. ",
    "(Sidak method for 8 estimates)."),
    hjust = 0.5) +
  theme_bw(base_size=16) +
  # geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
  #           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
  #           color   = "black") +
  geom_text(color   = "black", nudge_y = .5) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


agelrvm2 <- glmer(stay ~  age_scaled*v_chosen_lag + age_scaled*trial_scaled +
                 (1|ID), family = binomial(), data = rdf, nAGQ = 0)



# do attempters have lower max value overall? Actually, NS higher
rvcheck1 <- lmer(value_max ~ trial_scaled + Group + (1|ID), data = rdf)
summary(vcheck1)
car::Anova(vcheck1)

# do they chose a lower value overall?  NO
rvcheck2 <- lmer(value_chosen ~ trial_scaled + Group + (1|ID), data = rdf)
summary(vcheck2)
car::Anova(vcheck2)


# do they stick with B after reversal?
rbm1 <- glmer(multinomial_choice=="B" ~  Group*trial_scaled +
               (1|ID), family = binomial(), data = rpostrev)
summary(rbm1)
car::Anova(rbm1)
ls_rbm1 <- lsmeans(rbm1,"trial_scaled", by = "Group", at = list(trial_scaled=c(0,1,2)))
plot(ls_rbm1, type ~ response, horiz=F,ylab = "logit(probability of chosing B)", xlab = "Time after reversal")


# transform for plotting
rdf$choiceA[rdf$multinomial_choice=="A"] <- 1
rdf$choiceB[rdf$multinomial_choice=="B"] <- 1
rdf$choiceC[rdf$multinomial_choice=="C"] <- 1
rdf$choiceA[rdf$multinomial_choice!="A"] <- 0
rdf$choiceB[rdf$multinomial_choice!="B"] <- 0
rdf$choiceC[rdf$multinomial_choice!="C"] <- 0
rcdf = melt(rdf, na.rm = FALSE, measure.vars = c("choiceA","choiceB","choiceC"))
rcdf$option <- rcdf$variable
rcdf$choice <- rcdf$value
View(rcdf)

# split chosen value into quantiles for plotting

xs=quantile(rdf$value_chosen,c(.1, .2, .3, .4, .5, .6, .7, .8, .9, .95, 1))
xs[1]=xs[1]-.00005
rdf <- rdf %>% mutate(v_chosen_cat =cut(value_chosen, breaks=xs, labels=c("d1", "d2", "d3","d4", "d5", "d6","d7", "d8", "d9","d10")))
rdf <- rdf %>% mutate(v_chosen_cat_lag =cut(v_chosen_lag, breaks=xs, labels=c("d1", "d2", "d3","d4", "d5", "d6","d7", "d8", "d9","d10")))

boxplot(rdf$value_chosen~rdf$v_chosen_cat,col=3:5)

ggplot(data = na.omit(rdf), aes(x = v_chosen_cat_lag, y = stay)) + geom_smooth(method = "glm", method.args = list(family = "binomial"))
boxplot(rdf$value_chosen~rdf$v_chosen_cat,col=3:5)


# redo the analysis with binned value
rvmb <- glmer(stay ~  Group*v_chosen_cat_lag + Group*trial_scaled + stay_p +
                age*v_chosen_cat_lag + age*trial_scaled +
                (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rvmb)
car::Anova(rvmb)
ls_rvmb <- lsmeans(rvmb,"v_chosen_cat_lag", by = "Group")
plot(ls_rvmb, type ~ stay, horiz=F,ylab = "logit(probability of staying)", xlab = "Value (deciles)")

leastsquare = lsmeans(rvmb, pairwise ~ v_chosen_cat_lag:Group,adjust="tukey")
CLD = cld(leastsquare,
          alpha=0.05,
          Letters=letters,
          adjust="tukey")
###  Remove spaces in .group
CLD$.group=gsub(" ", "", CLD$.group)

### Plot
pdf(file = "value on choice by group rep PRETTY.pdf",
    width = 8,
    height = 6)
pd = position_dodge(0.4)    ### How much to jitter the points on the plot
CLD$v_chosen_decile <- as.numeric((CLD$v_chosen_cat))
ggplot(CLD, aes(x     = v_chosen_decile,
                y     = lsmean,
                color = Group
)) +
  geom_point(shape  = 15, size   = 4, position = pd) +
  geom_errorbar(
    aes(ymin  =  lsmean,
        ymax  =  asymp.UCL),
    width =  0.2,  size  =  0.7, position = pd) +
  geom_line(position = pd) +
  # facet_wrap( ~ Group, nrow = 1) +
  theme_bw() +
  theme(axis.title   = element_text(face = "bold"), axis.text    = element_text(face = "bold"), plot.caption = element_text(hjust = 0)) +
  ylab("Logit probability of repeating the choice") +
  xlab("Chosen value") +
  ggtitle ("Effect of value on choice by group",
           subtitle = "Generalized linear mixed-effects model") +
  labs(caption  = paste0(
    "A<C: z=14.0, A<D: z=6.7, A<I: z=3.4, all p<.001\n",
    "Boxes: LS mean.",
    "Error bars: 95% CI, \n"),
    hjust = 0.5) +
  theme_bw(base_size=20) #+
# geom_text(nudge_x = c(0.1, -0.1, 0.1, -0.1, 0.1, -0.1, -0.1, 0.1),
#           nudge_y = c(4.5,  4.5, 4.5,  4.5, 4.5 , 4.5,  4.5, 4.5),
#           color   = "black") +
# geom_text(color   = "black", nudge_y = 1) #+
# scale_color_manual(values = c("blue", "red"))
dev.off()


ggplot(na.omit(rcdf[,c(2,58,65,66)]), aes(x=Trial, y=choice, color = option)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Choice probability") +
  scale_color_manual(values = c("blue", "red", "green"))

ggplot(na.omit(rcdf[,c(1:2,57:66)]), aes(x=Trial, y=choice, color = Group)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Choice probability") +
  facet_wrap(~option)

rdf_plot <- rdf[,c(1:25,74:98)]
ggplot((rdf_plot), aes(x=Trial, y=reinf_n, color = Group)) + stat_smooth(method = "gam", method.args = list(family = "binomial"), se = TRUE) + theme_gray(base_size=20) + labs(x = "Trial", y = "Reward (replication)")
ggplot(na.omit(rdf[,c(1:16,74:97)]), aes(x=Trial, y=value_max, color = Group)) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("Chosen value (replication)")

ggplot(na.omit(rdf[,c(1:9,74:97)]), aes(x=Trial, y=stay_p, color = Group)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Probability of staying with the same choice") +
  facet_wrap(~v_chosen_cat) #geom_jitter(alpha=0.2) +

ggplot(na.omit(rdf[,c(1:9,74:97)]), aes(x=v_chosen_lag, y=stay_p, color = Group)) + stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + theme_gray(base_size=20) + labs(x = "Chosen value", y = "Probability of staying with the same choice")

ggplot(na.omit(rdf[,c(1:9,74:97)]), aes(x=v_chosen_lag, y=stay_p, color = GroupLeth)) + stat_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) + theme_gray(base_size=20) + labs(x = "Chosen value", y = "Probability of staying with the same choice")
ggplot(na.omit(rdf[,c(1:9,15,47:67)]), aes(x=Trial, y=stay_p, color = GroupLeth)) + stat_smooth(method="loess") + theme_bw(base_size=20) + labs(x = "Chosen value", y = "Probability of staying with the same choice")

ggplot(na.omit(rcdf[,c(1:2,57:68)]), aes(x=Trial, y=choice, color = GroupLeth, linetype = option)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Choice probability") +  facet_wrap(~option, ncol = 2)
ggplot(na.omit(rcdf[,c(1:2,57:68)]), aes(x=Trial, y=reinf_n, color = Group)) + stat_smooth(method="auto") + theme_gray(base_size=20) + ylab("Choice probability")


# plot trialwise stay probability
pdf(file = "attempters exploring rep.pdf", width = 10, height = 6)
ggplot(na.omit(rdf[,c(1:2,47:65)]), aes(x=Trial, y=stay_p, color = Group)) + stat_smooth(method="auto") + theme_bw(base_size=20) + ylab("Probability of staying with the same choice") +
  facet_wrap(~past_rew) #geom_jitter(alpha=0.2) +
dev.off()



###########
# entropy: replication #
###########

rvhm2 <- glmer(stay ~  v_chosen_lag*h_lag + v_chosen_lag*Group + h_lag*Group + trial_scaled + stay_lag +
                (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rvhm2)
car::Anova(rvhm2)

# ls_rvhm2 <- lsmeans(rvhm2, "Group", by = c("v_chosen_lag","h_lag"),  at = list(v_chosen_lag = c(0,0.5,1), h_lag = c(1.408,1.518,1.585)))
# plot(ls_rvhm2, horiz = F)
# cld(ls_rvhm2)

rfvhm2 <- glmer(stay ~  v_chosen_lag_f*Group + h_lag_f*Group +  v_chosen_lag_f*age_scaled + h_lag_f*age_scaled + trial_scaled + stay_lag +
                 (1|ID), family = binomial(), data = rdf, nAGQ = 0)
summary(rfvhm2)
car::Anova(rfvhm2)
anova(rvhm2,rfvhm2)





save(list = ls(all.names = TRUE),file = "bandit2.RData")
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit2.RData")
