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
library(nnet)
library(reshape2)
library(ggbiplot)
library(corrplot)
library(lsmeans)

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

# check the betas by ROI

mot <- bdf[bdf$signal=="moto",]
boxplot(Beta~ROI, data=mot, varwidth=TRUE, notch=TRUE, main="Beta", xlab="ROI")

mag <- bdf[bdf$signal=="rewM",]
boxplot(Beta~ROI, data=mag, varwidth=TRUE, notch=TRUE, main="Beta", xlab="ROI")

lv <- bdf[bdf$signal=="valu",]
boxplot(Beta~ROI, data=lv, varwidth=TRUE, notch=TRUE, main="Beta", xlab="ROI")

hdf <- dcast(bdf, ID+SPSI_ICSSUB+UPPSPNEGURGENCY+UPPSPPOSURGENCY+UPPSPLACKOFPERSEV+
               UPPSPLACKOFPREMED+EXITtot+DRS_TOTAL+WTAR_SCALED_SCORE+BIS_COGNIT+
               BIS_NONPLAN+BIS_MOTOR+BIS_TOTALMEAN+prob_switch_err_fMRI+spont_switch_err_fMRI+
               persev_err_fMRI+percent_corr_fMRI+before_percent_corr_fMRI+before_spont_sw_fMRI+
               before_prob_sw_fMRI+before_persev_fMRI+after_percent_corr_fMRI+after_persev_fMRI+
               after_prob_sw_fMRI+after_spont_sw_fMRI+ PATTYPE+LEARNAGE+GENDERTEXT+RACETEXT~ROI, value.var = "Beta")
hdf <- hdf[,-grep("left_ACC",names(hdf))]

View(hdf)
# correlation heatmaps

sdf <- dcast(bdf, ID+SPSI_ICSSUB+UPPSPNEGURGENCY+UPPSPPOSURGENCY+UPPSPLACKOFPERSEV+
               UPPSPLACKOFPREMED+EXITtot+DRS_TOTAL+WTAR_SCALED_SCORE+ BIS_TOTALMEAN+prob_switch_err_fMRI+spont_switch_err_fMRI+
               persev_err_fMRI+percent_corr_fMRI+LEARNAGE+GENDERTEXT+RACETEXT~ROI, value.var = "Beta")
sdf <- sdf[,-grep("left_ACC",names(sdf))]

View(sdf)


nums <- sapply(sdf, is.numeric)
ndf <- sdf[ , nums]
View(ndf)
ndf <-  ndf[,-grep("motor",names(ndf))]
View(ndf)
ndf <-  ndf[,-grep("rewMag",names(ndf))]
ndf <-  ndf[,-grep("ID",names(ndf))]

View(ndf)
ndf$DRS_TOTAL <- -ndf$DRS_TOTAL
ndf$WTAR_SCALED_SCORE <- -ndf$WTAR_SCALED_SCORE
ndf$percent_corr_fMRI <- -ndf$percent_corr_fMRI
cormat <- cor(ndf, use = "complete.obs")
# order <- corrMatOrder(cormat, order="AOE")
# rcormat <- cormat[order,order]
corrplot(cormat, type = "upper", order = "AOE", tl.cex = 0.9)
all.pca = prcomp(na.omit(ndf),scale = TRUE)
summary(all.pca)
plot(all.pca,type = 'l')
ggbiplot(all.pca, choices = 2:3, varname.size = 2)



#value
val_rois <-  hdf[,grep("value",names(hdf),TRUE)]
val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cormat <- (cor(val_rois))
corrplot(cormat, order = "hclust")
value.pca = prcomp((val_rois),scale = TRUE, scores = TRUE)
summary(value.pca)
plot(value.pca,type = 'l')
ggbiplot(value.pca, choices = 1:2)


#magnitude
mag_rois <-  hdf[,grep("mag",names(hdf),TRUE)]
cormat <- (cor(mag_rois))
order <- corrMatOrder(cormat, order="AOE")
rcormat <- cormat[order,order]
corrplot(rcormat)
mag.pca = prcomp(mag_rois,scal = TRUE,scores = TRUE)
summary(mag.pca)
plot(mag.pca,type = 'l')
eig <- mag.pca$sdev^2
plot(eig)
ggbiplot(mag.pca)

#motor
mot_rois <-  hdf[,grep("motor_",names(hdf),TRUE)]
cormat <- (cor(mot_rois))
order <- corrMatOrder(cormat, order="AOE")
rcormat <- cormat[order,order]
corrplot(rcormat)
mot.pca = prcomp(mot_rois,scal = TRUE,scores = TRUE)
summary(mot.pca)
plot(mot.pca,type = 'l')
ggbiplot(mot.pca)





vmpfc_lv <- lv[lv$ROI=="vmPFC_value_17",]
summary(vm1 <- lm(Beta ~ SPSI_ICSSUB, data = vmpfc_lv))
car::Anova(vm1)


summary(bm1 <- lm(Beta ~ SPSI_ICSSUB, data = vmpfc_lv))
car::Anova(bm1)



ofc_mag <- mag[mag$ROI=="right_mid_orbital_gy_rewMag_2" | mag$ROI== "left_mid_orbital_gy_rewMag_4" | mag$ROI== "left_mid_orbital_rewMag_10",]
boxplot(Beta~ROI, data=ofc_mag, varwidth=TRUE, notch=TRUE, main="Beta", xlab="ROI")

# learned value
summary(lm1 <- lmer(Beta ~ ROI*spont_switch_err_fMRI+(1|ID), data = lv))
car::Anova(lm1)

summary(lm2 <- lmer(Beta ~ ROI*spont_switch_err_fMRI+ROI*WTAR_SCALED_SCORE + (1|ID), data = lv))
car::Anova(lm2)
anova(lm1,lm2)

# try to dissociated SSE and WTAR effects from motor
summary(lmm2 <- lmer(Beta ~  signal*spont_switch_err_fMRI + signal*WTAR_SCALED_SCORE + (1|ID), data = bdf))
car::Anova(lmm2)
anova(lm1,lm2)


# adding PS errors does not improve the learned value model
summary(lm3 <- lmer(Beta ~ ROI*spont_switch_err_fMRI+ROI*WTAR_SCALED_SCORE + ROI*prob_switch_err_fMRI + (1|ID), data = lv))
car::Anova(lm3)
anova(lm2,lm3)

# magnitude
summary(mm1 <- lmer(Beta ~ ROI + (1|ID), data = mag))
car::Anova(mm1)
# no individual differences scale with magnitude
summary(mm2 <- lmer(Beta ~ ROI*LEARNAGE + (1|ID), data = mag))
car::Anova(mm2)
anova(mm1,mm2)



summary(m2 <- lmer(Beta ~ ROI + BIS_NONPLAN + (1|ID), data = ofc_mag))
car::Anova(m2)

summary(m2 <- lmer(Beta ~ ROI + BIS_NONPLAN + (1|ID), data = ofc_mag))
car::Anova(m2)


summary(m1 <- lmer(Beta ~ ROI + (1|ID), data = mag))
car::Anova(m1)



lv <- bdf[bdf$signal=="valu",]
boxplot(Beta~ROI, data=lv, varwidth=TRUE, notch=TRUE, main="Beta", xlab="ROI")

summary(m1 <- lmer(Beta ~ ROI* + (1|ID), data = mag))
car::Anova(m1)

#start building a model for double dissociation of signals
summary(m1 <- lmer(Beta ~ signal + (1|ID), data = bdf))
car::Anova(m1)
ls_m1 <- lsmeans(m1,"signal")
contrast(ls_m1, method = "pairwise", adjust ="tukey")
plot(ls_m1, type ~ signal, horiz=F, ylab = "Activation", xlab = "signal")




summary(m2 <- lmer(Beta ~ signal + LEARNAGE + EDUCATION + (1|ID), data = bdf))
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

# add multinom analyses looking at how magnitude of reward influences choice probability (nnet package)
library(readr)
trialwise_stay_switch_subj_responses <- read_csv("~/Google Drive/skinner/projects_analyses/Project Bandit/R/trialwise_stay_switch_subj_responses.csv")
sw <- trialwise_stay_switch_subj_responses
sw$reward <- as.factor(sw$reward)

sw = sw %>% group_by(ID) %>%
  mutate(rew_lag = lag(reward, order_by=ID), mag_lag = lag(Magnitude, order_by=ID)) %>% ungroup()


View(sw)
sw$stay <- sw$switch_vector
sw$magf_lag <- as.factor(sw$mag_lag)
sw$magf <- as.factor(sw$Magnitude)

sm1 <- glmer(stay ~ rew_lag*magf_lag + Trial + (1|ID), family = binomial(), data = sw)
summary(sm1)
car::Anova(sm1)
# ls_sm1 <- lsmeans(sm1,"rew_lag", by = "mag_lag", at = list(mag_lag = c(10,25,50)))
ls_sm1 <- lsmeans(sm1,"rew_lag", by = "magf_lag")

plot(ls_sm1, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

sm2 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*magf + Trial + (1|ID), family = binomial(), data = sw)
summary(sm2)
car::Anova(sm2)
# ls_sm1 <- lsmeans(sm1,"rew_lag", by = "mag_lag", at = list(mag_lag = c(10,25,50)))
ls_sm2 <- lsmeans(sm2,"rew_lag", by = "magf_lag")
plot(ls_sm2, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

ls_sm2_1 <- lsmeans(sm2,"magf")
plot(ls_sm2_1, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "Current magnitude")

# plot trialwise choice probability

ggplot(subset(df), aes(x=trial, y=probA)) + stat_smooth(method="loess") + theme_gray(base_size=20) #+ facet_wrap(~msplit) #geom_jitter(alpha=0.2) +

rdf <- merge(hdf,sw, by = "ID", all.x = TRUE)
rdf$trial_sc <- scale(rdf$Trial)
View(rdf)
rdf$vmPFC_value_17_sc <- scale(rdf$vmPFC_value_17)
rm1 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*vmPFC_value_17_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm1)
car::Anova(rm1)
ls_rm1 <- lsmeans(rm1,"rew_lag", by = "vmPFC_value_17_sc", at = list(vmPFC_value_17_sc = c(-2,0,2)))
plot(ls_rm1, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rdf$left_IFG_value_20_sc <- scale(rdf$left_IFG_value_20)
rm2 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*left_IFG_value_20_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm2)
car::Anova(rm2)
ls_rm2 <- lsmeans(rm2,"rew_lag", by = "left_IFG_value_20_sc", at = list(left_IFG_value_20_sc = c(-2,0,2)))
plot(ls_rm2, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rdf$SMA_value_4_sc <- scale(rdf$SMA_value_4)
rm3 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*SMA_value_4_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm3)
car::Anova(rm3)
ls_rm3 <- lsmeans(rm3,"rew_lag", by = "SMA_value_4_sc", at = list(SMA_value_4_sc = c(-2,0,2)))
plot(ls_rm3, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rdf$right_mid_orbital_gy_rewMag_2_sc <- scale(rdf$right_mid_orbital_gy_rewMag_2)
rm4 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*right_mid_orbital_gy_rewMag_2_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm4)
car::Anova(rm4)
ls_rm4 <- lsmeans(rm4,"rew_lag", by = "right_mid_orbital_gy_rewMag_2_sc", at = list(right_mid_orbital_gy_rewMag_2_sc = c(-2,0,2)))
plot(ls_rm4, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")



