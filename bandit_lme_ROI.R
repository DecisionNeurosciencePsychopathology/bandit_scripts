#  related ROI-level beta coefficients from the learned value and magnitude maps to behavior and traits

setwd("/Users/localadmin/Google Drive/skinner/projects_analyses/Project Bandit/R")
library(readr)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
source(file.path(getMainDir(), "Miscellaneous", "Global_Functions.R"))
library(R.matlab)
library(xtable)
library(Hmisc)

#load(file="dataframe_for_entropy_analysis_Oct2016.RData")
#this contains data with 24 basis functions and post-Niv learning rule
# load(file="dataframe_for_entropy_analysis_Nov2016.RData")
bdf <- read_csv("~/Google Drive/skinner/projects_analyses/Project Bandit/R/bandit_long_table.csv",
col_types = cols(`�..ID` = col_skip()))
bdf[[37]] <- NULL
View(bdf)
bdf <- data.frame(bdf)

summary(bdf)
as.factor(bdf$ROI)
as.factor(bdf$signal)
as.factor(bdf$ID)
attach(bdf)

# reduce signal names to the three basic ones
bdf$signal <- strtrim(bdf$signal,4)

# remove rows without Beta

bdf <- bdf[is.na(bdf$Beta)==0,]


#start building a model for double dissociation of signals
summary(m1 <- lmer(Beta ~ signal + (1|ID), data = bdf))
car::Anova(m1)

summary(m2 <- lmer(Beta ~ signal*UPPSPLACKOFPREMED + (1|ID), data = bdf))
car::Anova(m2)
anova(m1,m2)

summary(mval <- lmer(runreward ~ elratio + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)

summary(mval <- lmer(runreward ~ elratioF + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)


summary(mval <- lmer(runreward ~ lateentropy + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)

summary(mval <- lmer(runreward ~ earlyentropy + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)


summary(mval <- lmer(runreward ~ earlyentropyF + lateentropyF + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)



summary(mval <- lmer(runreward ~ lateentropy + allentropy + run + (1|subject) + (1|run), filter(bdfruns, run > 1)))
car::Anova(mval)


summary(mval <- lmer(runreward ~ earlyentropy + lateentropy + (1|subject) + (1|run), bdfruns))
car::Anova(mval)
mval <- lmer(runreward ~ (1|subject), filter(bdfruns, run > 1))

library(lsmeans)
cm <- lmerCellMeans(mval, cont.pts=list(entropyH=c(1,2,3,4), trial=c(1, 10, 25, 40, 50)))
ggplot(cm, aes(x=entropyH, y=ev)) + geom_line() + facet_wrap(~trial)



#compute person means and within-subject centered predictors
bdfcent <- bdf %>% select_(.dots=c("LunaID", "run", "trial", "abstschange", predictors)) %>%
    #mutate_at(wicenter, funs(bwcent=. - mean(., na.rm=TRUE), bwmean=mean(., na.rm=TRUE))) %>% #not really useful
    group_by(LunaID, run) %>%
    mutate_at(wicenter, funs(wicent=. - mean(., na.rm=TRUE), pmean=mean(., na.rm=TRUE))) %>% #within-person centering and person means
    ungroup() %>% mutate_at(c("trial", "abstschangelag", wicenter, paste0(wicenter, "_pmean")), funs(c=. - mean(., na.rm=TRUE))) #between-person centering of person means

#compute further lags for testing
bdfcent <- bdfcent %>% group_by(LunaID, run) %>% mutate(abstschangelag = lag(abstschange, n=1, order_by=trial),
    abstschangelag2 = lag(abstschange, n=2, order_by=trial),
    abstschangelag3 = lag(abstschange, n=3, order_by=trial),
    abstschangelag4 = lag(abstschange, n=4, order_by=trial),
    abstschangelag5 = lag(abstschange, n=5, order_by=trial),
    abstschangelag6 = lag(abstschange, n=6, order_by=trial),
    abstschangelag7 = lag(abstschange, n=7, order_by=trial),
    abstschangelag8 = lag(abstschange, n=8, order_by=trial),
    abstschangelag9 = lag(abstschange, n=9, order_by=trial),
    abstschangelag10 = lag(abstschange, n=10, order_by=trial),
    entropyHlag_wicentlag2 = lag(entropyHlag_wicent, n=2, order_by=trial),
    entropyHlag_wicentlag3 = lag(entropyHlag_wicent, n=3, order_by=trial),
    entropyHlag_wicentlag4 = lag(entropyHlag_wicent, n=4, order_by=trial),
    entropyHlag_wicentlag5 = lag(entropyHlag_wicent, n=5, order_by=trial),
    entropyHlag_wicentlag6 = lag(entropyHlag_wicent, n=6, order_by=trial),
    entropyHlag_wicentlag7 = lag(entropyHlag_wicent, n=7, order_by=trial),
    entropyHlag_wicentlag8 = lag(entropyHlag_wicent, n=8, order_by=trial),
    entropyHlag_wicentlag9 = lag(entropyHlag_wicent, n=9, order_by=trial),
    entropyHlag_wicentlag10 = lag(entropyHlag_wicent, n=10, order_by=trial)
)


mean(bdfcent$entropyHlag_pmean_c)
mean(bdfcent$entropyHlag_wicent, na.rm=TRUE)
mean(bdfcent$trial_c, na.rm=TRUE)
mean(bdfcent$distfromedgelag_c, na.rm=TRUE)


#effect of multiple lags for RT swings
m1 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + (1 | LunaID) + (1 | run), bdfcent)
summary(m1)

# This freezes:
#m1a <- mixed(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + (1 | LunaID) + (1 | run), bdfcent)
#summary(m1)


m2 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + abstschangelag + (1 | LunaID) + (1 | run), bdfcent)
summary(m2)

m3 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + abstschangelag + abstschangelag2 + (1 | LunaID) + (1 | run), bdfcent)
summary(m3)

m4 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + abstschangelag + abstschangelag2 + abstschangelag3 +  (1 | LunaID) + (1 | run), bdfcent)
summary(m4)

m5 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + (1 | LunaID) + (1 | run), bdfcent)
summary(m5)

m6 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + abstschangelag5 + (1 | LunaID) + (1 | run), bdfcent)
summary(m6)

m7 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c +
        abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + abstschangelag5 +
        entropyHlag_pmean_c + (1 | LunaID) + (1 | run), subset(bdfcent, run>1))

m8 <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c +
        abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + abstschangelag5 +
        entropyHlag_pmean_c + entropyHlag_wicent*trial_c*omissionlag + (1 | LunaID) + (1 | run), subset(bdfcent, run>1))

#summary(m7)
car::Anova(m8)

mcrazy <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c +
        abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + abstschangelag5 +
        abstschangelag6 + abstschangelag7 + abstschangelag8 + abstschangelag9 + abstschangelag10 +
        entropyHlag_pmean_c + entropyHlag_wicent*trial_c*omissionlag + (1 | LunaID) + (1 | run), subset(bdfcent, run>1))

summary(mcrazy)

car::Anova(mcrazy)

minsane <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c +
        abstschangelag + abstschangelag2 + abstschangelag3 + abstschangelag4 + abstschangelag5 +
        abstschangelag6 + abstschangelag7 + abstschangelag8 + abstschangelag9 + abstschangelag10 +
        entropyHlag_wicentlag2 + entropyHlag_wicentlag3 + entropyHlag_wicentlag4 + entropyHlag_wicentlag5 +
        entropyHlag_wicentlag6 + entropyHlag_wicentlag7 + entropyHlag_wicentlag8 + entropyHlag_wicentlag9 + entropyHlag_wicentlag10 +
        entropyHlag_pmean_c + entropyHlag_wicent*trial_c*omissionlag + (1 | LunaID) + (1 | run), subset(bdfcent, run>1))

summary(minsane)

car::Anova(minsane)


#acf(na.omit(bdfcent$abstschange))
pdf("Abstract art.pdf", width=12, height=10)
ggplot(bdfcent, aes(x=trial, y=abstschange)) + stat_smooth() + geom_jitter(alpha=0.2) + theme_gray(base_size=20)
dev.off()

ggplot(subset(bdf,run>1), aes(x=trial, y=abstschange*100, color = msplit, )) + stat_smooth(method="loess") + theme_gray(base_size=20) + ylab("RT swings, ms") + labs(colour = "Earnings") #facet_wrap(~msplit) #geom_jitter(alpha=0.2) +



#no gray
levels(bdf$msplit) <- c("< median earnings","=> median earnings" )
j
toresid <- lmer(abstschange ~ distfromedgelag_c*omissionlag*vdevlag_c + trial_c + (1 | LunaID) + (1 | run), bdfcent, na.action=na.exclude)
bdf$leftovers <- resid(toresid)

ggplot(subset(bdf,run>1), aes(x=trial, y=leftovers)) + stat_smooth(method="loess") + theme_gray(base_size=20) + facet_wrap(~msplit) #geom_jitter(alpha=0.2) +






m1 <- lmer(abstschange ~ entropyHlag_pmean_c + entropyHlag_wicent + trial_c + (1 | LunaID), bdfcent)
summary(m1)
car::Anova(m1)

m2 <- lmer(abstschange ~ entropyHlag_pmean_c*distfromedgelag_c + entropyHlag_wicent*distfromedgelag_c + trial_c + (1 | LunaID), bdfcent)
summary(m2)
car::Anova(m2)

m5 <- lmer(abstschange ~ entropyHlag_pmean_c*distfromedgelag_c*omissionlag*vdevlag_c + entropyHlag_wicent*distfromedgelag_c*omissionlag*vdevlag_c + trial_c*entropyHlag_wicent + (1 | LunaID), bdfcent)
summary(m5)
car::Anova(m5)

#current reasonable winner
bdfcent$abstschange_sec <- bdfcent$abstschange*100
m6 <- lmer(abstschange_sec ~ entropyHlag_pmean_c*distfromedgelag_c*omissionlag*vdevlag_c + entropyHlag_wicent*distfromedgelag_c*omissionlag*vdevlag_c + trial_c*entropyHlag_wicent + entropyHlag_wicent*entropyHlag_pmean_c + abstschangelag_c + (1 | LunaID) + (1 | run), bdfcent)
summary(m6)
car::Anova(m6)

#
bdfcent$abstschange_sec <- bdfcent$abstschange*100
m6 <- lmer(abstschange_sec ~ entropyHlag_pmean_c*distfromedgelag_c*omissionlag*vdevlag_c + entropyHlag_wicent*distfromedgelag_c*omissionlag*vdevlag_c + trial_c*entropyHlag_wicent + entropyHlag_wicent*entropyHlag_pmean_c + abstschangelag_c + (1 | LunaID) + (1 | run), bdfcent)
summary(m6)
car::Anova(m6)


msimple <- lmer(abstschange_sec ~ entropyHlag_pmean_c*entropyHlag_wicent + trial_c*entropyHlag_wicent + abstschangelag_c + (1 | LunaID) + (1 | run), bdfcent)
summary(msimple)
car::Anova(msimple)

#spot check
bdfcent %>% group_by(LunaID, run) %>% summarize(mean(entropyHlag_wicent, na.rm=TRUE)) %>% print(n=100)

#Here's my current thinking for PLoS paper
#1) Report simple abstschange ~ entropy + trial
#2) Report effect after throwing in a bunch of other stuff
#3) Report effect dissociating within versus between entropy

msimple <- lmer(abstschange_sec ~ entropyHlag_c*trial_c + abstschangelag_c + (1 | LunaID) + (1 | run), filter(bdfcent, run>1))
summary(msimple)
car::Anova(msimple)


pdf("simple entropy for plos.pdf", width=12, height=7)
cm <- lmerCellMeans(msimple, n.cont=10, fixat0=c("trial_c", "abstschangelag_c"))
cm$entropyHlag_c <- cm$entropyHlag_c + mean(bdfcent$entropyHlag, na.rm=TRUE) #uncenter for plotting
ggplot(cm, aes(x=entropyHlag_c, y=abstschange_sec, ymin=abstschange_sec-se, ymax=abstschange_sec+se)) +
    geom_line(size=2.5) + theme_bw(base_size=24) + geom_pointrange()
dev.off()

msimple2 <- lmer(abstschange_sec ~ entropyHlag_c*trial_c*omissionlag + abstschangelag_c + (1 | LunaID) + (1 | run), filter(bdfcent, run>1))
summary(msimple2)
car::Anova(msimple2)


pdf("simple entropy for plos with omission.pdf", width=12, height=7)
cm <- lmerCellMeans(msimple2, n.cont=10, fixat0=c("trial_c", "abstschangelag_c"))
cm$entropyHlag_c <- cm$entropyHlag_c + mean(bdfcent$entropyHlag, na.rm=TRUE) #uncenter for plotting
ggplot(cm, aes(x=entropyHlag_c, y=abstschange_sec, ymin=abstschange_sec-se, ymax=abstschange_sec+se, color=omissionlag)) +
    geom_line(size=2.5) + theme_bw(base_size=24) + geom_pointrange()
dev.off()


#controlling for various effects and confounders
mcontrol <- lmer(abstschange_sec ~ entropyHlag_c*trial_c + abstschangelag_c +
        evdevlag_c*omissionlag*vdevlag_c*distfromedgelag_c + (1 | LunaID) + (1 | run), filter(bdfcent, run>1))


summary(mcontrol)
car::Anova(mcontrol)


pdf("wi entropy for plos.pdf", width=5, height=4)
cm <- lmerCellMeans(mcontrol, n.cont=10, fixat0=c("evdevlag_c", "trial_c", "distfromedgelag_c", "vdevlag_c", "abstschangelag_c"))
#average over omissions and rewards
cm$predpoint <- 1:10 #rep 2x
cm <- cm %>% group_by(predpoint) %>% summarize_if(is.numeric, mean)
cm$entropyHlag_c <- cm$entropyHlag_c + mean(bdfcent$entropyHlag, na.rm=TRUE) #uncenter for plotting
ggplot(cm, aes(x=entropyHlag_c, y=abstschange_sec, ymin=abstschange_sec-se, ymax=abstschange_sec+se)) +
     geom_line(size=1.5) + theme_bw(base_size=20) + geom_pointrange(size=0.8) + ylab("RT swing (ms)") + xlab("Entropy of value distribution") +
     theme(axis.title.y=element_text(margin=margin(r=15)), axis.title.x=element_text(margin=margin(t=10)))
dev.off()

#divide into between and within
mdivide <- lmer(abstschange_sec ~ entropyHlag_pmean_c*trial_c + entropyHlag_wicent*trial_c + abstschangelag_c +
        evdevlag_c*omissionlag*vdevlag_c*distfromedgelag_c + (1 | LunaID) + (1 | run), filter(bdfcent, run>1))

summary(mdivide)
car::Anova(mdivide)

pdf("wi entropy as a function of person avg entropy.pdf", width=12, height=7)
cm <- lmerCellMeans(m6, n.cont=10, divide="entropyHlag_pmean_c", fixat0=c("trial_c", "distfromedgelag_c", "vdevlag_c", "abstschangelag_c"))
ggplot(cm, aes(x=entropyHlag_wicent, y=abstschange_sec, ymin=abstschange_sec-se, ymax=abstschange_sec+se, color=omissionlag)) +
    facet_wrap(~entropyHlag_pmean_c) + geom_line(size=2.5) + theme_bw(base_size=24) + geom_pointrange()
dev.off()

pdf("wi entropy as a function of trial.pdf", width=12, height=7)
#cm <- lmerCellMeans(m6, n.cont=10, fixat0=c("entropyHlag_pmean_c", "distfromedgelag_c", "vdevlag_c", "abstschangelag_c"), cont.pts=list(trial_c=c(-24, 0, 24)))
cm <- lmerCellMeans(m6, n.cont=10, divide="trial_c", n.divide=3, fixat0=c("entropyHlag_pmean_c", "distfromedgelag_c", "vdevlag_c", "abstschangelag_c"))
levels(cm$trial_c) <- c("Trial 10", "Trial 25", "Trial 40")
#cm$abstschange <- cm$abstschange*100
ggplot(cm, aes(x=entropyHlag_wicent, y=abstschange_sec, ymin=abstschange_sec-se, ymax=abstschange_sec+se, color=omissionlag)) +
    facet_wrap(~trial_c) + geom_line(size=1.5) + theme_bw(base_size=20) + geom_pointrange() + xlab("Entropy (centered, within run)") + ylab("Change in RT (sec)") +
    scale_color_brewer("Prior outcome", palette="Dark2")
dev.off()

#what about predicting quality of choice as a function of entropy
m6 <- lmer(ev ~ entropyHlag_pmean_c*distfromedgelag_c*omissionlag + entropyHlag_wicent*distfromedgelag_c*omissionlag + trial_c*entropyHlag_wicent + entropyHlag_wicent*entropyHlag_pmean_c + (1 | LunaID) + (1 | run), bdfcent)
summary(m6)
car::Anova(m6)




m8 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial_c + abstschangelag_c + (1 | LunaID), bdf)
summary(m8)
car::Anova(m8)




anova(m5, m6)


m1 <- lmer(abstschange ~ entropyHlag + trial + (1 | LunaID), bdf)
summary(m1)

m2 <- lmer(abstschange ~ entropyHlag + trial + distfromedgelag + (1 | LunaID), bdf)
summary(m2)

m3 <- lmer(abstschange ~ entropyHlag*distfromedgelag + trial + (1 | LunaID), bdf)
summary(m3)
car::Anova(m3)


m4 <- lmer(abstschange ~ entropyHlag*distfromedgelag*omissionlag + trial + (1 | LunaID), bdf)
summary(m4)
car::Anova(m4)


m5 <- lmer(abstschange ~ entropyHlag*distfromedgelag*omissionlag*vdevlag + trial + (1 | LunaID), bdf)
summary(m5)
car::Anova(m5)


cm <- lmerCellMeans(m4, n.cont=10, divide="distfromedgelag", fixat0="trial")
pdf("me4_entropy_omission_dist.pdf", width=12, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) + facet_wrap(~distfromedgelag)
dev.off()



m4 <- lmer(abstschange ~ entropyHlag*omissionlag + trial + (1 | LunaID), bdf)
summary(m4)
car::Anova(m4)

cm <- lmerCellMeans(m4, n.cont=10, fixat0="trial")
pdf("me4_entropy_omission.pdf", width=8, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()


anova(m1, m2, m3)

cm <- lmerCellMeans(m1, n.cont=10, fixat0="trial")

pdf("me1_entropyH.pdf", width=8, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

m4 <- lmer(abstschange ~ entropyHlag + trial + omissionlag + distfromedgelag + (1 | LunaID), bdf)
summary(m4)

cm <- lmerCellMeans(m4, n.cont=10, fixat0="trial")

pdf("me4_entropyH_omission.pdf", width=8, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

m5 <- lmer(abstschange ~ entropyHlag*omissionlag + trial + distfromedgelag + (1 | LunaID), bdf)
summary(m5)

anova(m4, m5)

cm <- lmerCellMeans(m3, n.cont=10, fixat0="trial")

pdf("me3_entropyH_omission.pdf", width=8, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

#add deviation of chosen versus max value on prior trial
m4 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag + trial + (1 | LunaID), bdf)
summary(m4)

#anova(m3, m4)

cm <- lmerCellMeans(m4, n.cont=10, divide="evdevlag", fixat0="trial")

pdf("me4_entropyH_omission_evdevlag.pdf", width=14, height=6)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) +
    geom_hline(yintercept=0) + facet_wrap(~evdevlag)
dev.off()


m5 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + (1 | LunaID), bdf)
summary(m5)
car::Anova(m5)

anova(m4, m5)

cm <- lmerCellMeans(m5, n.cont=10, divide=c("evdevlag", "vdevlag"), fixat0="trial")

pdf("me5_entropyH_omission_evdevlag_vdevlag.pdf", width=14, height=14)
ggplot(cm, aes(x=entropyHlag, y=abstschange, ymin=abstschange-se, ymax=abstschange+se, color=omissionlag)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Abs trialwise RT change (cs)") + xlab("Entropy") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) +
    geom_hline(yintercept=0) + facet_grid(vdevlag~evdevlag)
dev.off()

m6 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + (1 | LunaID), subset(bdf, trial > 25))
summary(m6)

m7 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + abstschangelag + (1 | LunaID), subset(bdf, trial > 25))
summary(m7)

m8 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + abstschangelag + (1 | LunaID), bdf)
summary(m8)
car::Anova(m8)

m9 <- lmer(abstschange ~ entropyHlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + abstschangelag + entropyHlag*distfromedgelag + (1 | LunaID), bdf)
summary(m9)
car::Anova(m9)


aggdata = bdf %>% group_by(subject) %>% summarize(totreward=sum(score))
ggplot(aggdata, aes(x=totreward)) + geom_histogram(bins=15)


pdf("rough entropy by trial and performance medsplit.pdf", width=10, height=6)
ggplot(subset(bdf, run>1), aes(x=trial, y=entropyH, color=msplit)) + stat_smooth(alpha=0.1) + theme_bw(base_size=22) + xlab("Trial") + ylab("Entropy of value distribution") +
    scale_color_brewer("Performance", palette="Set1")
dev.off()

pdf("rough entropy by trial and performance medsplit.pdf", width=10, height=6)
ggplot(subset(bdf, run>1), aes(x=trial, y=entropyFixed, color=msplit)) + stat_smooth(alpha=0.1) + theme_bw(base_size=22) + xlab("Trial") + ylab("Entropy of value distribution") +
    scale_color_brewer("Performance", palette="Set1")
dev.off()

entropyagg <- bdf %>% select(LunaID, trial, run, entropyFixed, entropyH, msplit) %>% gather(key="Model", value="entropy", entropyFixed, entropyH) %>%
    mutate(Model=recode(Model, entropyFixed="Fixed LR V", entropyH="Fixed LR V Decay")) %>% filter(run>1) %>% group_by(Model, msplit, trial) %>%
    do(data.frame(rbind(Hmisc::smean.cl.boot(.$entropy)))) %>% ungroup()

pdf("rough entropy by trial and performance medsplit.pdf", width=10, height=6)
ggplot(entropyagg, aes(x=trial, y=Mean, ymin=Lower, ymax=Upper, linetype=msplit, color=Model, fill=Model)) + geom_line(size=1.2) + geom_ribbon(alpha=0.1) +
    theme_bw(base_size=22) +
    xlab("Trial") + ylab("Entropy of value distribution") +
    scale_color_brewer("Performance", palette="Dark2") +
    scale_fill_brewer("Performance", palette="Dark2") +
    scale_linetype("Overall Performance") #+ facet_wrap(~Model)
dev.off()

pdf("rough entropy by trial and performance medsplit decay only.pdf", width=10, height=6)
ggplot(filter(entropyagg, Model=="Fixed LR V Decay"), aes(x=trial, y=Mean, ymin=Lower, ymax=Upper, linetype=msplit)) +
    geom_line(size=1.2) + geom_ribbon(alpha=0.1) +
    theme_bw(base_size=22) +
    xlab("Trial") + ylab("Entropy of value distribution") +
    scale_linetype("Overall Performance") #+ facet_wrap(~Model)
dev.off()


entropysplit <- bdf %>% select(LunaID, trial, run, entropyFixed, entropyH, msplit) %>% gather(key="Model", value="entropy", entropyFixed, entropyH) %>%
    mutate(Model=recode(Model, entropyFixed="Fixed LR V", entropyH="Fixed LR V Decay"))


pdf("rough entropy by trial and performance medsplit.pdf", width=10, height=6)
ggplot(subset(entropysplit, run>1), aes(x=trial, y=entropy, linetype=msplit, color=Model)) + stat_smooth(alpha=0.1, method="loess") + theme_bw(base_size=22) + xlab("Trial") + ylab("Entropy of value distribution") +
    scale_color_brewer("Performance", palette="Dark2") + scale_linetype("Overall Performance") #+ facet_wrap(~Model)
dev.off()

#edesc_aggruns <- edescriptives %>% filter(run > 1) %>% group_by(LunaID, Model, trial) %>% summarize(value=mean(value))


#average magnitude of swings as a function of total reward
m9 <- lmer(abstschange ~ cumreward*trial + abstschangelag + entropyHlag*distfromedgelag + (1 | LunaID), bdf)
summary(m9)
car::Anova(m9)

m9 <- lmer(abstschange ~ totreward*trial + abstschangelag + entropyHlag*distfromedgelag + (1 | LunaID), bdf)
summary(m9)



###
#two ideas: emotion and contingency modulate RT swings and entropy?

bdf %>% filter(trial > 5) %>% group_by(rewFunc, emotion) %>% summarize(mentropyD = mean(entropyH, na.rm=TRUE), mrtswing=mean(abstschange, na.rm=TRUE),
    sdentropyD = sd(entropyH, na.rm=TRUE), sdrtswing=sd(abstschange, na.rm=TRUE))

summary(lmer(abstschange ~ emotion*rewFunc + (1 | LunaID), bdf))

summary(mblock <- lmer(entropyH ~ emotion + rewFunc + (1 | LunaID), subset(bdf, trial_abs > 3)))
summary(mblock <- lmer(entropyH ~ emotion + rewFunc + (1 | LunaID), bdf))
cm <- lmerCellMeans(mblock)

ggplot(cm, aes(x=rewFunc, y=entropyH, color=emotion, ymin=entropyH-se, ymax=entropyH+se)) + geom_pointrange(size=1.5, position=position_dodge(width=0.2))

summary(mblock <- lmer(entropyH ~ rewFunc + (1 | LunaID), subset(bdf, trial_abs> 3)))


cm <- lmerCellMeans(mblock)

pdf("entropy_by_contingency.pdf", width=10, height=8)
ggplot(cm, aes(x=rewFunc, y=entropyH, ymin=entropyH-se, ymax=entropyH+se)) + geom_pointrange(size=1.5) + theme_bw(base_size=15)
dev.off()

summary(mblock <- lmer(abstschange ~ rewFunc + (1 | LunaID), subset(bdf, trial_abs> 3)))

cm <- lmerCellMeans(mblock)
pdf("rtswing_by_contingency.pdf", width=10, height=8)
ggplot(cm, aes(x=rewFunc, y=abstschange, ymin=abstschange-se, ymax=abstschange+se)) + geom_pointrange(size=1.5) + theme_bw(base_size=15)
dev.off()

summary(mblock <- lmer(abstschange ~ emotion + (1 | LunaID), subset(bdf, trial_abs> 3)))

cm <- lmerCellMeans(mblock)
pdf("rtswing_by_emotion.pdf", width=10, height=8)
ggplot(cm, aes(x=emotion, y=abstschange, ymin=abstschange-se, ymax=abstschange+se)) + geom_pointrange(size=1.5) + theme_bw(base_size=15)
dev.off()


#






#fixed LR model (no decay)
m7 <- lmer(abstschange ~ entropyFlag*omissionlag + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + trial + (1 | LunaID), subset(bdf, trial > 25))
summary(m7)

m7 <- lmer(abstschange ~ entropyFlag*omissionlag*trial + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + (1 | LunaID), bdf)
summary(m7)

car::Anova(m7)


m7 <- lmer(abstschange ~ entropyHlag*omissionlag*trial + evdevlag*omissionlag*vdevlag + vdevlag*omissionlag + (1 | LunaID), bdf)
summary(m7)

bdf <- bdf %>% group_by(subject) %>% mutate(medswing = median(abstschange, na.rm=TRUE)) %>% ungroup()
bdf %>% select(subject, run, trial, abstschange, medswing) %>% arrange(subject, run, trial) %>% tail(n=100)

bdf %>% group_by(subject, run) %>% summarize(cent = cor(entropyFixed, entropyH)) %>% summarize()








pdf("swing_spaghetti.pdf", width=15, height=20)
ggplot(bdf, aes(x=trial, y=abstschange, group=LunaID)) + geom_line(alpha=0.2) + stat_smooth(aes(group=NULL))  + facet_wrap(~run, ncol=1) #subset(bdf, subject < 3)
dev.off()


#model 1: RT change predicted by prior omission and prior RT deviation from V max
m1 <- lmer(timestepchange ~ omissionlag*vdevlag + trial + (1|LunaID), bdf)
summary(m1)

cm <- lmerCellMeans(m1, n.cont=10, fixat0="trial")
pdf("m1_vdevtest.pdf", width=8, height=6)
ggplot(cm, aes(x=vdevlag, y=timestepchange, color=omissionlag, ymin=timestepchange-se, ymax=timestepchange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Mean trialwise RT change (ds)") + xlab("RT deviation from maximum value\non prior trial (ms)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

#model 2: Does adding magnitude of chosen option versus max help? yes
m2 <- lmer(timestepchange ~ omissionlag*vdevlag*evdevlag + trial + (1|LunaID), bdf)
summary(m2)

anova(m1, m2)
cm2 <- lmerCellMeans(m2, n.cont=10, divide="evdevlag", fixat0="trial")
pdf("m2_vdevtest_evdiff.pdf", width=15, height=6)
ggplot(cm2, aes(x=vdevlag, y=timestepchange, color=omissionlag, ymin=timestepchange-se, ymax=timestepchange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (ms)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~evdevlag)
dev.off()

#model 3: adding entropy
#try including entropy
m3 <- lmer(timestepchange ~ omissionlag*vdevlag*entropylag*evdevlag + trial + (1|LunaID), bdf)
summary(m3)
car::Anova(m3)
anova(m2, m3)

cm3 <- lmerCellMeans(m3, divide="entropylag", n.cont=10, fixat0=c("trial", "evdevlag"))

pdf("m3_vdevtest_evdiff_entropy.pdf", width=15, height=6)
ggplot(cm3, aes(x=vdevlag, y=timestepchange, color=omissionlag, ymin=timestepchange-se, ymax=timestepchange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (cs)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~entropylag, nrow=1)
dev.off()

##current entropy?
test2 <- lmer(timestepchange ~ omissionlag*vdevlag*entropy + (1|LunaID), bdf)
summary(test2)
car::Anova(test2)
anova(test, test2)

cm2 <- lmerCellMeans(test2, divide="entropy", n.cont=10)

pdf("entropytest_cur.pdf", width=15, height=6)
ggplot(cm2, aes(x=vdevlag, y=timestepchange, color=omissionlag, ymin=timestepchange-se, ymax=timestepchange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + #geom_linerange(size=1.0, width=250) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (cs)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~entropy, nrow=1)
dev.off()

#incorporate U into models. Does the point of maximal uncertainty predict swings in that direction?
m1 <- lmer(timestepchange ~ omissionlag*vdevlag + (1|LunaID), bdf)
summary(m1)

m2 <- lmer(timestepchange ~ omissionlag*udevlag + (1|LunaID), bdf)
summary(m2)

m2 <- lmer(timestepchange ~ udevlag + (1|LunaID), bdf)
summary(m2)


m3 <- lmer(timestepchange ~ omissionlag*entropylag + (1|LunaID), bdf)
summary(m3)

m4 <- lmer(timestepchange ~ omissionlag*vdevlag + omissionlag*udevlag + (1|LunaID), bdf)
summary(m4)

cm <- lmerCellMeans(m4, divide="vdevlag", n.cont=10)
pdf("udevlag_vdevlag_predicted.pdf", width=10, height=8)
ggplot(cm, aes(x=udevlag, y=timestepchange, ymin=timestepchange-se, ymax=timestepchange+se, color=omissionlag)) + geom_line(size=2) + facet_wrap(~vdevlag) +
    geom_pointrange(size=1) +
    theme_bw(base_size=20)
dev.off()
m5 <- lmer(timestepchange ~ omissionlag*vdevlag + omissionlag*udevlag + omissionlag*entropylag + (1|LunaID), bdf)
summary(m5)

#no troubling overall correlations
cor(select(bdf, vdevlag, udevlag, entropylag), use="pairwise.complete.obs")




#try to predict current timestep based on a) prior outcome, b)
test2 <- lmer(timestep ~ timesteplag + entropy + (1|LunaID), bdf)

library(lme4)
summary(m1 <- lmer(timestep ~ timesteplag + rtvmax + rtumax + (1|LunaID), bdf))
car::Anova(m1)

summary(m2 <- lmer(timestep ~ timesteplag + rtvmax * rtumax + (1|LunaID), bdf))
car::Anova(m2)

anova(m1, m2)

summary(m2 <- lmer(timestepchange ~ timesteplag + rtvmax + rtumax + (1|LunaID), bdf))
car::Anova(m2)

#cor.test(~rtumax + rtumaxlag, bdf)

#wi-person scale tschange
bdf <- bdf %>% group_by(LunaID, run) %>% mutate(tschange_z = as.vector(scale(timestepchange))) %>% ungroup()

#should we within-person z-score as in frank?
summary(m2 <- lmer(timestepchange ~ timesteplag + rtvmaxlag + rtumaxlag + omissionlag + (1|LunaID), bdf))
car::Anova(m2)

summary(m2 <- lmer(tschange_z ~ timesteplag + rtvmaxlag + rtumaxlag + omissionlag + (1|LunaID), bdf))
car::Anova(m2)

#what about scaling by parameter. grabbed df from sceptic_external_correlates
bdf2 <- select(df, lunaid, fmri_alpha_t, fmri_gamma_t, fmri_beta_t, fixed_uv_tau) %>% inner_join(bdf, c("lunaid" = "LunaID"))

#summary(m2 <- lmer(timestepchange ~ timesteplag + rtvmaxlag + rtumaxlag*fmri_gamma_t + omissionlag + (1|lunaid), bdf2))
summary(m2 <- lmer(timestep ~ timesteplag + rtvmaxlag + rtumaxlag*fmri_gamma_t + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)

summary(m2 <- lmer(timestep ~ timesteplag + rtvmax + rtumax*fmri_gamma_t*trial + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)

summary(m1 <- lmer(timestep ~ timesteplag + rtvmax + rtumax*trial + omissionlag + (1|lunaid), bdf2))

summary(m2 <- lmer(timestep ~ timesteplag + rtvmax + rtumax*fixed_uv_tau + rtumax*trial + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)

summary(m2 <- lmer(timestep ~ timesteplag + rtvmaxlag + rtumaxlag*fixed_uv_tau + rtumaxlag*trial + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)

summary(m2 <- lmer(timestep ~ timesteplag + rtvmaxlag + rtumaxlag*fixed_uv_tau*trial + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)

summary(m2 <- lmer(timestep ~ timesteplag + rtvmaxlag + rtumaxlag + omissionlag + (1|lunaid), bdf2))
car::Anova(m2)


anova(m1, m2)

#bdf2 <- bdf %>% gather(key=etype, value=entropy_mixlag, entropyHlag, entropyFlag) %>%
#    mutate_at(vars(rtvmaxlag, timesteplag, entropylag, rtumaxlag, entropy_mixlag), funs(cent=. - mean(., na.rm=TRUE))) #%>%

#try standardizing entropy between measures to avoid strange scaling differences
bdf2 <- bdf %>% mutate(entropyHlag=as.vector(scale(entropyHlag)), entropyFlag=as.vector(scale(entropyFlag))) %>%
    gather(key=etype, value=entropy_mixlag, entropyHlag, entropyFlag) %>%
    mutate(inv_trial = 1/trial) %>%
    mutate_at(vars(trial, inv_trial, timesteplag, rtvmaxlag, entropylag, rtumaxlag, entropy_mixlag), funs(cent=. - mean(., na.rm=TRUE)))

library(lme4)
summary(m3 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropy_mixlag_cent*etype*trial_cent + run + (1 + run |LunaID), filter(bdf2, trial_abs > 4))) #omissionlag +
car::Anova(m3)

#break apart for a minute by etype (too many interactions!!)
summary(mfixed <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropy_mixlag_cent*trial_cent + run + (1 |LunaID), filter(bdf2, etype=="entropyFlag" & trial_abs > 4)))
car::Anova(mfixed)

summary(mdecay <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropy_mixlag_cent*trial_cent + run + (1 |LunaID), filter(bdf2, etype=="entropyHlag" & trial_abs > 4)))
car::Anova(mdecay)

cm <- lmerCellMeans(mdecay, fixat0=c("rtvmaxlag_cent", "timesteplag_cent", "run"), divide=c("entropy_mixlag_cent", "trial_cent"), n.cont=10)
ggplot(cm, aes(x=rtumaxlag_cent, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) + geom_line() + geom_pointrange() + facet_wrap(~trial_cent)

#better if we use 1/trial? (asymptotic)
summary(mdecay2 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropy_mixlag_cent*inv_trial_cent + run + (1 |LunaID), filter(bdf2, etype=="entropyHlag" & trial_abs > 4)))
car::Anova(mdecay2)

anova(mdecay, mdecay2) #inv trial model fits *far* better (~90 AIC points)

#trial on x axis
cm <- lmerCellMeans(mdecay2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent", "run"), divide=c("entropy_mixlag_cent", "rtumaxlag_cent"), n.cont=10)
cm$inv_trial_cent <- 1/(cm$inv_trial_cent + mean(bdf2$inv_trial, na.rm=TRUE))
ggplot(cm, aes(x=inv_trial_cent, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) +
    geom_line() + geom_pointrange() + facet_wrap(~rtumaxlag_cent)


#more nuanced prediction (without centering, which makes it hard to see)
summary(mdecay2 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag*entropy_mixlag_cent*inv_trial + run + (1 |LunaID), filter(bdf2, etype=="entropyHlag" & trial_abs > 4)))
car::Anova(mdecay2)

summary(mdecay2 <- lmer(timestep ~ timesteplag_cent*inv_trial + rtvmaxlag_cent + rtumaxlag*entropy_mixlag_cent*inv_trial + run + (1 |LunaID), filter(bdf2, etype=="entropyHlag" & trial_abs > 4)))
car::Anova(mdecay2)


cm <- lmerCellMeans(mdecay2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent", "run"), divide=c("entropy_mixlag_cent"), n.cont=10,
    cont.pts=list(rtumaxlag=c(5, 15, 25, 35, 45), inv_trial=1/rev(c(3, 5, 15, 25, 35, 45, 47))))

cm$inv_trial <- 1/(cm$inv_trial)
#cm$timestep <- cm$timestep/10
#cm$se <- cm$se/10
pdf("for consideration.pdf", width=20, height=10)
ggplot(cm, aes(x=inv_trial, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) +
    geom_line(position=position_dodge(width=1.5)) + geom_pointrange(position=position_dodge(width=1.5)) +
    facet_wrap(~rtumaxlag, scales="free_y") + theme_bw(base_size=18) + xlab("Trial") + ylab("Predicted timestep (1-40)") +
    ggtitle("Trial (inverse) on X, Umax in panels")

#get U back on x axis...
ggplot(cm, aes(x=rtumaxlag, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) +
    geom_line(position=position_dodge(width=1.5)) + geom_pointrange(position=position_dodge(width=1.5)) +
    facet_wrap(~inv_trial, scales="free_y") + theme_bw(base_size=18) + xlab("Timestep of Umax") + ylab("Predicted timestep (1-40)") +
    ggtitle("Umax on X, trial in panels")
dev.off()


#try linear version (although I know it fits worse...)
summary(mdecay2 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag*entropy_mixlag_cent*trial + run + (1 |LunaID), filter(bdf2, etype=="entropyHlag" & trial_abs > 4)))
car::Anova(mdecay2)

cm <- lmerCellMeans(mdecay2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent", "run"), divide=c("entropy_mixlag_cent"), n.cont=10,
    cont.pts=list(rtumaxlag=c(5, 25, 45), trial=c(1, 3, 5, 15, 25, 35, 45, 47, 49)))

#cm$inv_trial <- 1/(cm$inv_trial)
pdf("for consideration.pdf", width=9, height=6)
ggplot(cm, aes(x=trial, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) +
    geom_line() + geom_pointrange() + facet_wrap(~rtumaxlag, scales="free_y") + theme_bw(base_size=18)
dev.off()




#okay, see if we can handle the etype 4-way interaction conceptually...
bdf2$etype <- factor(bdf2$etype) #needed for lmerCellMeans to pick it up properly
summary(mboth <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropy_mixlag_cent*trial_cent*etype + run + (1 |LunaID), filter(bdf2, trial_abs > 4)))
car::Anova(mboth)
cm <- lmerCellMeans(mboth, fixat0=c("rtvmaxlag_cent", "timesteplag_cent", "run"), divide=c("entropy_mixlag_cent", "trial_cent"), n.cont=10)

ggplot(cm, aes(x=rtumaxlag_cent, y=timestep, color=entropy_mixlag_cent, ymin=timestep-se, ymax=timestep+se)) + geom_line() + geom_pointrange() + facet_grid(etype~trial_cent)



bdf2cent <- bdf %>% gather(key=etype, value=entropy_mixlag, entropyHlag, entropyFlag) %>% group_by(LunaID, run) %>%
    mutate_at(vars(rtvmaxlag, timesteplag, entropylag, rtumaxlag, entropy_mixlag), funs(wicent=. - mean(., na.rm=TRUE), pmean=mean(., na.rm=TRUE))) %>% #within-person centering and person means
    ungroup() %>% mutate_at(vars(timesteplag, trial, rtvmaxlag_pmean, entropylag_pmean, rtumaxlag_pmean, entropy_mixlag_pmean), funs(c=. - mean(., na.rm=TRUE))) #between-person centering of person means


summary(m3 <- lmer(timestep ~ timesteplag_c + rtvmaxlag_pmean_c + rtvmaxlag_wicent + rtumaxlag_pmean_c*entropy_mixlag_pmean_c + rtumaxlag_wicent*entropy_mixlag_wicent + (1|LunaID), bdf2cent)) #omissionlag +
car::Anova(m3)

summary(m3 <- lmer(timestep ~ timesteplag_c + rtvmaxlag_pmean_c + rtvmaxlag_wicent + rtumaxlag_pmean_c*entropy_mixlag_pmean_c*etype + rtumaxlag_wicent*entropy_mixlag_wicent*etype + run + (1 |LunaID), filter(bdf2cent, trial_abs > 5))) #omissionlag +
car::Anova(m3)

bdf2cent$invtrial <- 1/bdf2cent$trial
summary(m4 <- lmer(timestep ~ timesteplag_c + rtvmaxlag_pmean_c + rtvmaxlag_wicent + rtumaxlag_pmean_c*entropy_mixlag_pmean_c*etype + rtumaxlag_wicent*entropy_mixlag_wicent*etype*trial + run + (1 + run|LunaID), filter(bdf2cent, trial_abs > 5))) #omissionlag +
car::Anova(m4)

summary(m5 <- lmer(timestep ~ timesteplag_c + rtvmaxlag_pmean_c + rtvmaxlag_wicent + rtumaxlag_pmean_c*entropy_mixlag_pmean_c*etype + rtumaxlag_wicent*entropy_mixlag_wicent*etype*invtrial + run + (1 + run|LunaID), filter(bdf2cent, trial_abs > 5))) #omissionlag +
car::Anova(m5)

anova(m4, m5)


#try to look at effects in plot...
#pull out prior RT and value signals
bdf2cent$timestep_cleanup <- resid(lmer(timestep ~ timesteplag_c + rtvmaxlag_pmean_c + rtvmaxlag_wicent + (1|LunaID), bdf2cent, na.action=na.exclude))
summary(m3 <- lmer(timestep_cleanup ~ rtumaxlag_pmean_c*entropy_mixlag_pmean_c*etype + rtumaxlag_wicent*entropy_mixlag_wicent*etype + (1|LunaID), bdf2cent)) #omissionlag +
car::Anova(m3)



summary(m2 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropylag_cent + (1|LunaID), bdf2)) #omissionlag +
car::Anova(m2)

cm <- lmerCellMeans(m2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent"), divide="entropylag_cent")

summary(m3 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropyHlag_cent + (1 + run|LunaID), bdf2)) #omissionlag +
car::Anova(m3)
cm3 <- lmerCellMeans(m3, fixat0=c("rtvmaxlag_cent", "timesteplag_cent"), divide="entropyHlag_cent", n.cont=10)

ggplot(cm3, aes(x=rtumaxlag_cent, y=timestep, ymin=timestep-se, ymax=timestep+se, color=entropyHlag_cent)) + geom_line() + geom_pointrange()

summary(m4 <- lmer(timestep ~ timesteplag_cent + rtvmaxlag_cent + rtumaxlag_cent*entropyFlag_cent + (1 + run |LunaID), bdf2)) #omissionlag +
car::Anova(m4)
cm4 <- lmerCellMeans(m4, fixat0=c("rtvmaxlag_cent", "timesteplag_cent"), divide="entropyFlag_cent", n.cont=10)

ggplot(cm4, aes(x=rtumaxlag_cent, y=timestep, ymin=timestep-se, ymax=timestep+se, color=entropyFlag_cent)) + geom_line() + geom_pointrange()

#mm <- merge(cm3, cm4, by="rtumaxlag_cent")
cm3$timestep <- as.vector(cm3$timestep)
cm4$timestep <- as.vector(cm4$timestep)
cm3 <- cm3 %>% rename(entropy = entropyHlag_cent) %>% mutate(model="Decay")
cm4 <- cm4 %>% rename(entropy = entropyFlag_cent) %>% mutate(model="Fixed")

mm <- rbind(cm3, cm4)
mm <- mm %>% mutate(entropy_labeled=sub("(entropyHlag_cent|entropyFlag_cent)", "Entropy", entropy))
pdf("draft fig9.pdf", width=10, height=8)
ggplot(mm, aes(x=rtumaxlag_cent, y=timestep, ymin=timestep-se, ymax=timestep+se, color=entropy_labeled)) + geom_line(position=position_dodge(width=2)) +
    geom_pointrange(position=position_dodge(width=2)) + facet_wrap(~model) + theme_bw(base_size=20)
dev.off()


#cm <- lmerCellMeans(m2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent"), divide="rtumaxlag_cent")
#ggplot(cm, aes(x=entropylag_cent, y=timestep, ymin=timestep-se, ymax=timestep+se, color=omissionlag)) + geom_line() + geom_pointrange() + facet_wrap(~rtumaxlag_cent)

cm <- lmerCellMeans(m2, fixat0=c("rtvmaxlag_cent", "timesteplag_cent"), divide="entropylag_cent")
ggplot(cm, aes(x=rtumaxlag_cent, y=timestep, ymin=timestep-se, ymax=timestep+se, color=omissionlag)) + geom_line() + geom_pointrange() + facet_wrap(~entropylag_cent)


summary(m2 <- lmer(timestep ~ timesteplag + entropylag + omissionlag + (1|LunaID), bdf))
car::Anova(m2)



summary(m2 <- lmer(timestep ~ rtumax*trial  + (1|lunaid), bdf2))
car::Anova(m2)

mm <- lmerCellMeans(m2, divide="trial")
pdf("u effects.pdf", width=10, height=8)
ggplot(mm, aes(x=rtumax, y=timestep, color=trial)) + geom_line()
dev.off()


#bdf2 <- bdf2 %>% group_by(lunaid, run) %>% mutate(timestepchangelag=lag(timestepchange, order_by=trial)) %>% ungroup()
#summary(m2 <- lmer(timestepchange ~ timestepchangelag + rtvmaxlag + rtumaxlag*fmri_gamma_t + omissionlag + distfromedgelag + (1|lunaid), bdf2))
#car::Anova(m2)



#what about absolute timestep change?

test2 <- lmer(abstschange ~ omissionlag*absvdevlag*entropylag + (1|LunaID), bdf)

summary(test2)
cm3 <- lmerCellMeans(test2, divide="entropylag", n.cont=10)

pdf("entropytest_abs.pdf", width=15, height=6)
ggplot(cm3, aes(x=absvdevlag, y=abstschange, color=omissionlag, ymin=abstschange-se, ymax=abstschange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + geom_linerange(size=1.0) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (cs)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~entropylag, nrow=1)
dev.off()

summary(lmer(abstschange ~ absvdevlag + (1|LunaID), bdf))


#current entropy versus past trial?
test_past <- lmer(abstschange ~ omissionlag*absvdevlag*entropylag + (1|LunaID), bdf)
#test_cur <- lmer(abstschange ~ omissionlag*absvdevlag*entropy*entropylag + (1|LunaID), bdf)
test_cur <- lmer(abstschange ~ omissionlag*absvdevlag*entropy + (1|LunaID), bdf)

cm4 <- lmerCellMeans(test_cur, divide="entropy", n.cont=10)

pdf("entropytest_cur_abs.pdf", width=15, height=6)
ggplot(cm4, aes(x=absvdevlag, y=abstschange, color=omissionlag, ymin=abstschange-se, ymax=abstschange+se)) + geom_line(size=2.5) + theme_bw(base_size=24) + geom_linerange(size=1.0) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (cs)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0) +
    facet_wrap(~entropy, nrow=1)
dev.off()

test_cur <- lmer(abstschange ~ omissionlag*absvdevlag*wizentropy*entropy + (1|LunaID), bdf)

summary(lmer(abstschange ~ entropy + (1|LunaID), bdf))

#build up: abs rt change as a function of prior omission and deviation from max value
m1 <- lmer(abstschange ~ omissionlag*absvdevlag + trial + run + (1 |LunaID), bdf)
cm1 <- lmerCellMeans(m1, n.cont=10)

pdf("m1_abschange.pdf", width=15, height=6)
ggplot(cm1, aes(x=absvdevlag, y=abstschange, color=omissionlag, ymin=abstschange-se, ymax=abstschange+se)) + geom_line(size=1.5) + theme_bw(base_size=24) + geom_linerange(size=1.5) +
    ylab("Mean trialwise RT change (cs)") + xlab("RT deviation from maximum value\non prior trial (cs)") + scale_color_brewer("Prior Outcome", palette="Set2") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

#similar to frank: relative RT swing size as a function of relative entropy (within run)
m2 <- lmer(wizabstschange ~ wizentropy + trial + run + (1 |LunaID), bdf)
cm2 <- lmerCellMeans(m2, n.cont=10, fixat0=c("trial", "run"))

pdf("m2abschange_wizentropy.pdf", width=15, height=6)
ggplot(cm2, aes(x=wizentropy, y=wizabstschange, ymin=wizabstschange-se, ymax=wizabstschange+se)) + geom_line(size=1.5) + theme_bw(base_size=24) + geom_linerange(size=1.5) +
    ylab("z abs trialwise RT change") + xlab("within-run relative entropy") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()


pdf("m2abschange_wizentropy_scatter.pdf", width=15, height=6)
ggplot(bdf, aes(x=wizentropy, y=wizabstschange, group=LunaID)) + geom_point(size=1.0, alpha=0.5) + theme_bw(base_size=24) + stat_smooth(aes(group=NULL), se=TRUE) +
    ylab("z abs trialwise RT change") + xlab("within-run relative entropy") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

pdf("m2abschange_entropy_scatter.pdf", width=15, height=6)
ggplot(bdf, aes(x=entropy, y=wizabstschange, group=LunaID)) + geom_point(size=1.0, alpha=0.5) + theme_bw(base_size=24) + stat_smooth(aes(group=NULL), se=TRUE) +
    ylab("z abs trialwise RT change") + xlab("within-run relative entropy") + geom_vline(xintercept=0) + geom_hline(yintercept=0)
dev.off()

am1 <- lmer(rt ~ rtvmax*vmax + (1 + run|LunaID),bdf)
anova(am1)

am2 <- lmer(rt ~ rtvmax*vmax + emotion +(1 + run|LunaID),bdf)
anova(am1, am2)

# get lagged PE
bdf[, "PElag":=c(NA, maxPE[-.N]), by=rewFunc]
bdf <- bdf %>% group_by(LunaID, run) %>% mutate(pelag = lag(pemax, n=1, order_by=trial))

                                                # sanity check
ggplot(bdf,aes(x=pelag, y=abspelag)) + geom_point()

am3 <- lmer(rt ~ rtvmax*vmax + pelag*emotion +(1 + run|LunaID),bdf)
summary(am3)
anova(am3)

bdf <- bdf %>% group_by(LunaID, run) %>% mutate(abspelag = abs(pelag))

#bad
am4 <- lmer(rt ~ rtvmax*vmax*trial + abspelag*omissionlag + emotion*omissionlag + trial + (1 + run|LunaID),bdf)
summary(am4)
car::Anova(am4)
anova(am3,am4)

# add lagged rts

bdf <- bdf %>% group_by(LunaID, run) %>% mutate(rtlag = lag(rt, n=1, order_by=trial),
                                                        rtlag2 = lag(rt, n=2, order_by=trial),
                                                        rtlag3 = lag(rt, n=3, order_by=trial),
                                                        rtlag4 = lag(rt, n=4, order_by=trial),
                                                        rtlag5 = lag(rt, n=5, order_by=trial),
                                                        rtlag6 = lag(rt, n=6, order_by=trial),
                                                        rtlag7 = lag(rt, n=7, order_by=trial),
                                                        rtlag8 = lag(rt, n=8, order_by=trial),
                                                        rtlag9 = lag(rt, n=9, order_by=trial),
                                                        rtlag10 = lag(rt, n=10, order_by=trial)
)


am5 <- lmer(rt ~ rtvmax*vmax + vmax*trial + emotion*omissionlag + trial + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 + (1 + run|LunaID),bdf)
summary(am5)
car::Anova(am5)
anova(am4,am5)
 # add entropy
am6 <- lmer(rt ~ rtvmax*vmax + vmax*trial + emotion*omissionlag + trial + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 + entropy + (1 + run|LunaID),bdf)
summary(am6)
car::Anova(am6)
anova(am5,am6)

# plot results

library("lsmeans")
rg_am6 = ref.grid(am6)

#ls_am6 <- lsmeans(am6,"feedback", at = list(feedback = c(-2,-1,1,2)))
# # interaction plot
# object = f_m3;
# formula =  ~ stim_rating*stim;
# type = "response"
# lsmip(object, formula, type,
#       pch = c(1,2,6,7,9,10,15:20),
#       lty = 1, plotit = TRUE)

# add rtvmaxlag
am7 <- lmer(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 + entropy + (1 + run|LunaID),bdf)
summary(am7)
car::Anova(am7)
anova(am6,am7)


am8 <- lmer(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 + entropyHlag + distfromedgelag + (1 + run|LunaID),bdf)
summary(am8)
car::Anova(am8)
anova(am7,am8)


pdf("rtvmaxBYvmax.pdf", width=10, height=6)
ls_am8 <- lsmeans(am8,"rtvmax", by = "vmax", at = list(vmax = c(1,40,80), rtvmax = c(1,20,40)))
plot(ls_am8, type ~ rt, horiz=F,ylab = "RT", xlab = "Location of value maximum, 100-ms bins")
dev.off()

pdf("emotionBYreward.pdf", width=10, height=6)
ls_am8 <- lsmeans(am8,"emotion",by = "omissionlag")
plot(ls_am8, type ~ rt, horiz=F,ylab = "RT", xlab = "Emotion")
dev.off()



am9 <- lmer(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial*omissionlag + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 + entropyHlag + distfromedgelag + rewFunc*trial + (1 + run|LunaID),bdf)
summary(am9)
car::Anova(am9)
anova(am8,am9)

# switch to lme for specifying AR1
am10 <- lme(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial*omissionlag + rtlag + entropyHlag + distfromedgelag + rewFunc*trial, random = ~1|run/LunaID, bdf, na.action=na.exclude)
summary(am10)
car::Anova(am10)
# anova(am9,am10)

# specify AR1
am11 <- lme(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial*omissionlag + rtlag + entropyHlag + distfromedgelag + rewFunc*trial, random = ~1|run/LunaID, bdf, na.action=na.exclude ,correlation = corAR1())
summary(am11)
car::Anova(am11)
anova(am10,am11)

# add lags
am12 <- lme(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial*omissionlag + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 +  entropyHlag + distfromedgelag + rewFunc*trial, random = ~1|run/LunaID, bdf, na.action=na.exclude ,correlation = corAR1())
summary(am12)
car::Anova(am12)
# anova(am10,am11)

# without AR1
am13 <- lme(rt ~ rtvmax*vmax + rtvmaxlag + vmax*trial + emotion*omissionlag + trial*omissionlag + rtlag + rtlag2 +rtlag3 +rtlag4 + rtlag5 + rtlag6 +rtlag7 +rtlag8+rtlag9 +  entropyHlag + distfromedgelag + rewFunc*trial, random = ~1|run/LunaID, bdf, na.action=na.exclude )
summary(am13)
car::Anova(am13)
anova(am12,am13)



save(list = ls(all=TRUE), file = ".RData")


#setwd("/Users/localadmin/code/clock_smoothoperator/clock_task/non_model_based_analysis//")

# load(".Rdata")
