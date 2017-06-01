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
library(factoextra)

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


nums <- sapply(hdf, is.numeric)
ndf <- hdf[ , nums]
View(ndf)
ndf <-  ndf[,-grep("motor",names(ndf))]
View(ndf)
ndf <-  ndf[,-grep("rewMag",names(ndf))]
ndf <-  ndf[,-grep("value",names(ndf))]
ndf <-  ndf[,-grep("ID",names(ndf))]

View(ndf)
# ndf$DRS_TOTAL <- -ndf$DRS_TOTAL
# ndf$WTAR_SCALED_SCORE <- -ndf$WTAR_SCALED_SCORE
# ndf$percent_corr_fMRI <- -ndf$percent_corr_fMRI

cormat <- cor(ndf, use = "complete.obs")
# order <- corrMatOrder(cormat, order="AOE")
# rcormat <- cormat[order,order]
corrplot(cormat, type = "upper", order = "hclust", tl.cex = 0.9)
all.pca = prcomp(na.omit(ndf),scale = TRUE)
summary(all.pca)
plot(all.pca,type = 'l')
ggbiplot(all.pca, choices = 2:3, varname.size = 2)



#value
val_rois <-  hdf[,grep("value",names(hdf),TRUE)]
val_rois <- val_rois[,-grep("ACC",names(val_rois))]
cormat <- (cor(val_rois))
corrplot(cormat, order = "hclust", type = "upper")
value.pca = prcomp((val_rois),scale = TRUE)
value_pcas <- get_pca_ind(value.pca)
hdf$val1 <- value_pcas$coord[,1]
hdf$val2 <- value_pcas$coord[,2]
hdf$val3 <- value_pcas$coord[,3]
hdf$val4 <- value_pcas$coord[,4]


summary(value.pca)
plot(value.pca,type = 'l')
ggbiplot(value.pca, choices = 3:4)


#magnitude
mag_rois <-  hdf[,grep("mag",names(hdf),TRUE)]
cormat <- (cor(mag_rois))
order <- corrMatOrder(cormat, order="AOE")
rcormat <- cormat[order,order]
corrplot(rcormat)
mag.pca = prcomp(mag_rois,scal = TRUE,scores = TRUE)
summary(mag.pca)
plot(mag.pca,type = 'l')
ggbiplot(mag.pca)

mag_pcas <- get_pca_ind(mag.pca)
hdf$mag1 <- mag_pcas$coord[,1]



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

# try to dissociate SSE and WTAR effects from motor
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
rdf$SPSI_IC_sc <- scale(rdf$SPSI_ICSSUB)
rdf$UPPSPPOSURGENCY_sc <- scale(rdf$UPPSPPOSURGENCY)
rdf$UPPSPNEGURGENCY_sc <- scale(rdf$UPPSPNEGURGENCY)
rdf$UPPSPLACKOFPERSEV_sc <- scale(rdf$UPPSPLACKOFPERSEV)
rdf$UPPSPLACKOFPREMED_sc <- scale(rdf$UPPSPLACKOFPREMED)
rdf$EXITtot_sc <- scale(rdf$EXITtot)
rdf$DRS_TOTAL_sc <- scale(rdf$DRS_TOTAL)
rdf$WTAR_SCALED_SCORE_sc <- scale(rdf$WTAR_SCALED_SCORE)
rdf$BIS_TOTALMEAN_sc <- scale(rdf$BIS_TOTALMEAN)

# scale ROI components
rdf$val1 <- scale(rdf$val1)
rdf$val2 <- scale(rdf$val2)
rdf$val3 <- scale(rdf$val3)
rdf$val4 <- scale(rdf$val4)
rdf$mag1 <- scale(rdf$mag1)


rdf$male[rdf$GENDERTEXT=="MALE"] <- 1;
rdf$male[rdf$GENDERTEXT=="FEMALE"] <- 0;
rdf$male <- as.factor(rdf$male)
rdf$left_IFG_value_20_sc <- scale(rdf$left_IFG_value_20)
rdf$SMA_value_4_sc <- scale(rdf$SMA_value_4)
rdf$right_mid_orbital_gy_rewMag_2_sc <- scale(rdf$right_mid_orbital_gy_rewMag_2)

rdf = rdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay, order_by=ID)) %>% ungroup()


rm1 <- glmer(stay ~ rew_lag*magf_lag*vmPFC_value_17_sc + rew_lag*vmPFC_value_17_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm1)
car::Anova(rm1)
ls_rm1 <- lsmeans(rm1,"rew_lag", by = "vmPFC_value_17_sc", at = list(vmPFC_value_17_sc = c(-2,0,2)))
plot(ls_rm1, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rm2 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*left_IFG_value_20_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm2)
car::Anova(rm2)
ls_rm2 <- lsmeans(rm2,"rew_lag", by = "left_IFG_value_20_sc", at = list(left_IFG_value_20_sc = c(-2,0,2)))
plot(ls_rm2, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rm3 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*SMA_value_4_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm3)
car::Anova(rm3)
ls_rm3 <- lsmeans(rm3,"rew_lag", by = "SMA_value_4_sc", at = list(SMA_value_4_sc = c(-2,0,2)))
plot(ls_rm3, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

rm4 <- glmer(stay ~ rew_lag*magf_lag*right_mid_orbital_gy_rewMag_2_sc + rew_lag*right_mid_orbital_gy_rewMag_2_sc +  rew_lag*trial_sc + (1|ID), family = binomial(), data = rdf)
summary(rm4)
car::Anova(rm4)
ls_rm4 <- lsmeans(rm4,"rew_lag", by = "right_mid_orbital_gy_rewMag_2_sc", at = list(right_mid_orbital_gy_rewMag_2_sc = c(-2,0,2)))
plot(ls_rm4, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")


bm1 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*WTAR_SCALED_SCORE +  rew_lag*trial_sc*WTAR_SCALED_SCORE + (1|ID), family = binomial(), data = rdf)
summary(bm1)
car::Anova(bm1)

bm2 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*trial_sc*WTAR_SCALED_SCORE_sc + rew_lag*trial_sc*UPPSPPOSURGENCY_sc + (1|ID), family = binomial(), data = rdf)
summary(bm2)
car::Anova(bm2)
anova(bm1,bm2)

ls_bm2 <- lsmeans(bm2,"rew_lag", by = "UPPSPPOSURGENCY_sc", at = list(UPPSPPOSURGENCY_sc = c(-2,0,2)))
plot(ls_bm2, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

ls_bm2a <- lsmeans(bm2,"rew_lag", by =  "trial_sc","WTAR_SCALED_SCORE_sc", at = list(WTAR_SCALED_SCORE_sc = c(-2,0,2), trial_sc = c(-2,0,2)))
summary(ls_bm2a)
plot(ls_bm2a, type ~ stay, horiz=F,ylab = "effect of WTAR on p_stay", xlab = "reinforcement")

bm3 <- glmer(stay ~ rew_lag*magf_lag + rew_lag*trial_sc*WTAR_SCALED_SCORE_sc + rew_lag*male + rew_lag*UPPSPPOSURGENCY_sc + (1|ID), family = binomial(), data = rdf)
summary(bm3)
car::Anova(bm3)
anova(bm2,bm3)

ls_bm3 <- lsmeans(bm3,"rew_lag", by = "male")
plot(ls_bm3, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")
ls_bm3a <- lsmeans(bm3,"rew_lag", by = "UPPSPPOSURGENCY_sc", at = list(UPPSPPOSURGENCY_sc = c(-2,0,2)))
plot(ls_bm3a, type ~ stay, horiz=F,ylab = "logit(p_stay)", xlab = "reinforcement")

bm4 <- glmer(stay ~ stay_lag + rew_lag*magf_lag + rew_lag*trial_sc*WTAR_SCALED_SCORE_sc + rew_lag*male + rew_lag*UPPSPPOSURGENCY_sc + (1|ID), family = binomial(), data = rdf)
summary(bm4)
car::Anova(bm4)
anova(bm3,bm4)

bm5 <- glmer(stay ~ stay_lag + rew_lag*magf_lag + rew_lag*trial_sc*EXITtot_sc + rew_lag*male + rew_lag*UPPSPPOSURGENCY_sc + (1|ID), family = binomial(), data = rdf)
summary(bm5)
car::Anova(bm5)
anova(bm4,bm5)

#  value responses modulate response to reinforcement
bm6 <- glmer(stay ~ stay_lag + rew_lag*magf_lag + rew_lag*trial_sc + rew_lag*male + rew_lag*val1 + rew_lag*val2 + (1|ID), family = binomial(), data = rdf)
summary(bm6)
car::Anova(bm6)
anova(bm5,bm6)

# do magnitude responses modulate response to magnitude?  Very weakly

bm7 <- glmer(stay ~ stay_lag + rew_lag*magf_lag*mag1 + rew_lag*trial_sc + rew_lag*male + rew_lag*val1 + rew_lag*val2 + (1|ID), family = binomial(), data = rdf)
summary(bm7)
car::Anova(bm7)
anova(bm6,bm7)

ls_bm7 <- lsmeans(bm7,"rew_lag", by =  "val1", at = list(val1 = c(-2,0,2)))
summary(ls_bm7)
plot(ls_bm7, type ~ stay, horiz=F,ylab = "p_stay", xlab = "reinforcement")

ls_bm7a <- lsmeans(bm7,"rew_lag", by =  "val2", at = list(val2 = c(-2,0,2)))
summary(ls_bm7a)
plot(ls_bm7a, type ~ stay, horiz=F,ylab = "p_stay", xlab = "reinforcement")

bm8 <- glmer(stay ~ stay_lag + rew_lag*magf_lag*mag1 + rew_lag*trial_sc + rew_lag*male + rew_lag*val1 + rew_lag*val2 + rew_lag*LEARNAGE + (1|ID), family = binomial(), data = rdf)
summary(bm8)
car::Anova(bm8)
anova(bm7,bm8)


