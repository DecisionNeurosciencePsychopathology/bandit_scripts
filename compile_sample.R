# compile bandit sample

library(haven)
library(psych)
library(MASS)
library(ggplot2)
library(lme4)
library(lmerTest)
library(pscl)
library(boot)
library(nortest)
library(MBESS)
library(sem)
library(Amelia)
library(ROCR)
library(stargazer)
library(multcomp)
library(lsmeans)
library(Hmisc)
library(mice)
library(doBy)
library(xtable)
library(corrplot)
library(readxl)
#fmri
#fmri_bandit_demos <- read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit_demos_8_3_17/fmri_bandit_demos.csv")
#f <- fmri_bandit_demos

#all
f <- read_csv("~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit_demos_8_3_17/all_bandit_demos.csv")


f$COMMENT[f$COMMENT=="DEPRESSION-IDEATOR"] <- "IDEATOR"
f$COMMENT[f$COMMENT=="IDEATOR-ATTEMPTER"] <- "ATTEMPTER"


describeBy(f$LEARNAGE, group = f$COMMENT)
describeBy(f$PROTECT2AGE, group = f$COMMENT)

describe(f$COMMENT)

hist(f$LEARNAGE[f$COMMENT=="CONTROL"])
hist(f$LEARNAGE[f$COMMENT=="DEPRESSION"])
hist(f$LEARNAGE[f$COMMENT=="IDEATOR"])
hist(f$LEARNAGE[f$COMMENT=="ATTEMPTER"])
# need controls in their early 50s


sext <- table(f$GENDERTEXT, f$COMMENT)
chisq.test(sext)

racet <- table(f$RACETEXT, f$COMMENT)
chisq.test(racet)

summary(lm(f$EDUCATION~f$COMMENT))