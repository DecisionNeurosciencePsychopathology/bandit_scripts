## load Vanessa's ROI scores for behavioral and RT analyses
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
# library(lsmeans)
library(emmeans)
library(ggfortify)
#library(compareGroups)
library(RColorBrewer)
library(effects)
library(readr)
library(multcompView)
library(stargazer)
library(psych)
library(corrplot)


setwd("~/Box Sync/skinner/personal_folders/Vanessa")
load(file='all_lmer_data.Rdata')
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/roi_zscored_dmublock1/")


#### collinearity checks ####
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

#### correlations with individual differences ####
# one row per subject for correlations
roi_df <- filter(roi_gdf,Trial==1)
sub_chars <- as.data.frame(roi_df[, c(49,51,58:68,71:78,175:178,181)])
# ggplot(sub_chars,aes(x = Group, y = vmPFC)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = func_vmPFC)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = RStr)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = LStr)) + geom_boxplot()

chars <- sub_chars[,c(1:13,22:25)]
cors <- psych::corr.test(chars, use = "pairwise",method="pearson", alpha=.05)
pdf("bandit roi correlations.pdf", width=14, height=14)
corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "hclust", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
dev.off()
# the only somewhat notable correlation is with EXIT (r= -0.19)

#### int of group and vmPFC on exploitation: effect of ROIs ####
# does it impact exploitation?
m_v1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf + 
                        (1 | ID),
                      data = roi_gdf)
summary(m_v1)
car::Anova(m_v1,'3')
vif.lme(m_v1)
#create plot of effect of vmPFC value by reinforcement by group
pre_m_v1=predict(m_v1)
eff_m_v1=Effect(c('vmPFC','reinf','Group'),m_v1)
dfeff_m_v1=as.data.frame(eff_m_v1)
dfeff_m_v1$fitpse=dfeff_m_v1$fit+dfeff_m_v1$se
dfeff_m_v1$fitmse=dfeff_m_v1$fit-dfeff_m_v1$se
theme_set(theme_bw())
ggplot(dfeff_m_v1,
       aes(vmPFC,fit,colour=Group,fill=Group,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=fitmse,ymax=fitpse))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='vmPFC Long-term Value',y='Value of Next Choice',linetype='Reinforcement')
#emmeans plot
em1 <- emmeans(m_v1, "reinf",by = c("Group", "vmPFC"), at = list(vmPFC = c(-.02,0.03,0.08)))
plot(em1, horiz = F)
C <- cld(em1)
# colorful, but not necessarily more convincing:
ggplot(C, aes(x = vmPFC, y = emmean, ymin = asymp.LCL, ymax = asymp.UCL, color = Group)) + geom_errorbar(position = "dodge", width = .2)
#  geom_errorbar(position = "dodge", width = .2) + facet_wrap(reinf~Group, ncol = 4)

#split by reinforcement
roi_gdf_r0=subset(roi_gdf,reinf==0)
m_v1r0 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) + 
                       (1 | ID),
                     data = roi_gdf_r0)
summary(m_v1r0) #different slope than controls & overall effect of ROI than depressed
car::Anova(m_v1r0,'3')

roi_gdf_r1=subset(roi_gdf,reinf==1)
m_v1r1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) + 
                         (1 | ID),
                       data = roi_gdf_r1)
summary(m_v1r1) #different slope than ideators & overall effect of ROI than depresed
car::Anova(m_v1r1,'3')

#effect with R striatum ROI
m_rstr1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(RStr) * reinf + 
                       (1 | ID),
                     data = roi_gdf)
summary(m_rstr1)
car::Anova(m_rstr1,'3')

#effect with L striatum ROI
m_lstr1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(LStr) * reinf + 
                          (1 | ID),
                        data = roi_gdf)
summary(m_lstr1)
car::Anova(m_lstr1,'3')

#base model
m_1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * reinf + 
                       (1 | ID),
                     data = roi_gdf)
anova(m_v1,m_1) #chi sq = 288.7, df = 8, p<.001
anova(m_rstr1,m_1) #chi sq = 42.89, df = 8, p<.001
anova(m_lstr1,m_1) #ns: chi sq = 12.312, df = 8, p = .1378

#add vmPFC and str ROIs to see if additional explanatory power with striatum ROIs
m_vrstr1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(RStr) * reinf + Group * scale(vmPFC) * reinf +
                          (1 | ID),
                        data = roi_gdf)
summary(m_vrstr1)
car::Anova(m_vrstr1,'3')
anova(m_vrstr1,m_v1) #chi sq = 134.2, df = 8, p<.001

m_vlstr1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(LStr) * reinf + Group * scale(vmPFC) * reinf +
                           (1 | ID),
                         data = roi_gdf)
summary(m_vlstr1)
car::Anova(m_vlstr1,'3')
anova(m_vlstr1,m_v1) #chi sq = 129.9, df = 8, p<.001

m_vlrstr1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(RStr) * reinf + Group * scale(LStr) * reinf + Group * scale(vmPFC) * reinf +
                           (1 | ID),
                         data = roi_gdf)
summary(m_vlrstr1)
car::Anova(m_vlrstr1,'3')
anova(m_vlrstr1,m_vrstr1) # chi sq = 114.22, df = 8, p<.001
anova(m_vlrstr1,m_vlstr1) # chi sq = 118.62, df = 8, p<.001

#plot effects of each ROI
pre_m_rstr1=predict(m_rstr1)
eff_m_rstr1=Effect(c('RStr','reinf','Group'),m_rstr1)
dfeff_m_rstr1=as.data.frame(eff_m_rstr1)
theme_set(theme_bw())
ggplot(dfeff_m_rstr1,
       aes(RStr,fit,colour=Group,fill=Group,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='R striatum Long-term Value',y='Value of Next Choice',linetype='Reinforcement')

pre_m_lstr1=predict(m_lstr1)
eff_m_lstr1=Effect(c('LStr','reinf','Group'),m_lstr1)
dfeff_m_lstr1=as.data.frame(eff_m_lstr1)
theme_set(theme_bw())
ggplot(dfeff_m_lstr1,
       aes(LStr,fit,colour=Group,fill=Group,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='L striatum Long-term Value',y='Value of Next Choice',linetype='Reinforcement')


#combine clinical groups and test difference in model fit
roi_gdf$Group12=ifelse(roi_gdf$Group=='Controls',0,1)
m_v1g <-   lme4::lmer(value_chosen_vba_mfx ~ Group12 * scale(vmPFC) * reinf + 
                        (1 | ID),
                      data = roi_gdf)
anova(m_v1,m_v1g) #significant improvement of m_v1 over m_v1g
#chi sq = 39.425, df = 8, p < .001

m_rstr1g <-   lme4::lmer(value_chosen_vba_mfx ~ Group12 * scale(RStr) * reinf + 
  (1 | ID),data = roi_gdf)
anova(m_rstr1,m_rstr1g) #also sig

m_lstr1g <-   lme4::lmer(value_chosen_vba_mfx ~ Group12 * scale(LStr) * reinf + 
  (1 | ID),data = roi_gdf)
anova(m_lstr1,m_lstr1g) #also sig

#Bartra ROI controlling for previous max value, model fit, & differences in value
summary(m_v5 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf + v_max_lag_mfx * reinf + v_ch_diff * reinf + L_vba_mfx * reinf +
                               (1 | ID),
                             data = roi_gdf))
vif.lme(m_v5)

#### int of group and vmPFC on exploitation: other measures of exploitation ####
#use objectively better stimulus rather than model-calculated expected probability
#no significant impact of group except in interaction with prev. reinforcement
#but correct_incorrect is only if choice is rewarded, not if it has the highest probability
m_v2 <-   lme4::glmer(correct_incorrect ~ Group * scale(vmPFC) * reinf_lag + 
                       (1 | ID),family=binomial(),
                     data = roi_gdf)
summary(m_v2)
car::Anova(m_v2,'3')

#read in task file to get objective probabilities of outcomes
task_info=read.table('/Users/brownv/Documents/dnpl/Bandit Scripts/CORRECT-crdt-sched-vrbl-rich-2015-01-23.txt',
                     header=T,sep="\t",stringsAsFactors=F)
task_info$Trial <- seq.int(nrow(task_info))
task_info$prev_Arew=c(NA,task_info$Arew[1:(dim(task_info)[1]-1)])
task_info$prev_Brew=c(NA,task_info$Brew[1:(dim(task_info)[1]-1)])
task_info$prev_Crew=c(NA,task_info$Crew[1:(dim(task_info)[1]-1)])
roi_gdf=merge(roi_gdf,task_info,by='Trial')
roi_gdf$obj_corr=ifelse(roi_gdf$multinomial_choice=='A',
  roi_gdf$Arew,ifelse(roi_gdf$multinomial_choice=='B',
  roi_gdf$Brew,ifelse(roi_gdf$multinomial_choice=='C',roi_gdf$Crew,NA)))

#effect on choosing objectively better stimulus- probabilities from task file
#no additional effects of group
m_v2o <-   lme4::glmer(obj_corr ~ Group * scale(vmPFC) * reinf_lag + 
                        (1 | ID),family=binomial(),
                      data = roi_gdf)
summary(m_v2o)
car::Anova(m_v2o,'3')

#create variable to bin chosen values, since it is mostly 0/1 anyway
roi_gdf$bin_value_chosen_vba_mfx=ifelse(roi_gdf$value_chosen_vba_mfx>.9,1,
  ifelse(roi_gdf$value_chosen_vba_mfx<.1,0,NA))
m_v3 <-   lme4::glmer(bin_value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf + 
  (1 | ID),family=binomial(),data = roi_gdf)
summary(m_v3)
car::Anova(m_v3,'3')

#create variable for trials where model-calculated best value and objective best value agree
roi_gdf$bin_lag_value_chosen_vba_mfx=ifelse(roi_gdf$v_chosen_lag_updated_mfx>.9,1,ifelse(roi_gdf$v_chosen_lag_updated_mfx<.1,0,NA))
roi_gdf$lag_obj_value_chosen_vba_mfx=ifelse(roi_gdf$bin_lag_value_chosen_vba_mfx==1&roi_gdf$obj_corr==1,1,ifelse(roi_gdf$bin_lag_value_chosen_vba_mfx==0&roi_gdf$obj_corr==0,0,NA))
m_v4 <-   lme4::lmer(lag_obj_value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf_lag + 
                       (1 | ID),data = roi_gdf)
summary(m_v4)
car::Anova(m_v4,'3')

  
#### int of group and vmPFC on exploitation: functional ROI ####
#functional ROI w/controls
summary(m_v1f <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(func_vmPFC) * reinf + 
                               (1 | ID),
                             data = roi_gdf))
car::Anova(m_v1f,'3')
vif.lme(m_v1f)
em1f <- emmeans(m_v1f, "reinf",by = c("Group", "func_vmPFC"), at = list(func_vmPFC = c(-.02,0.03,0.08)))
plot(em1, horiz = F)

#functional ROI w/o controls
roi_gdf_nocon=subset(roi_gdf,Group!='Controls')
summary(m_v1f_nc <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(func_vmPFC) * reinf + 
                                (1 | ID),
                              data = roi_gdf_nocon))
car::Anova(m_v1f_nc,'3')
vif.lme(m_v1f_nc)
em1f_nc <- emmeans(m_v1f_nc, "reinf",by = c("Group", "func_vmPFC"), at = list(func_vmPFC = c(-.02,0.03,0.08)))
plot(em1f_nc, horiz = F)

#### individual differences ####
#EXIT
m_v1_exit=lme4::lmer(value_chosen_vba_mfx~EXITtot*Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_exit)
car::Anova(m_v1_exit,'3') #all group effects but int with reinforcement mediated by EXIT
roi_gdf_lowe=subset(roi_gdf,EXITtot<mean(roi_gdf$EXITtot,na.rm=T))
roi_gdf_highe=subset(roi_gdf,EXITtot>mean(roi_gdf$EXITtot,na.rm=T))
m_v1_le=lme4::lmer(value_chosen_vba_mfx~Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf_lowe)
summary(m_v1_le)
m_v1_he=lme4::lmer(value_chosen_vba_mfx~Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf_highe)
summary(m_v1_he)
#plot high/low EXIT
pre_m_v1_le=predict(m_v1_le)
eff_m_v1_le=Effect(c('vmPFC','reinf','Group'),m_v1_le)
dfeff_m_v1_le=as.data.frame(eff_m_v1_le)
theme_set(theme_bw())
ggplot(dfeff_m_v1_le,
       aes(vmPFC,fit,colour=Group,fill=Group,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='vmPFC Long-term Value',y='Value of Next Choice',linetype='Reinforcement')
pre_m_v1_he=predict(m_v1_he)
eff_m_v1_he=Effect(c('vmPFC','reinf','Group'),m_v1_he)
dfeff_m_v1_he=as.data.frame(eff_m_v1_he)
theme_set(theme_bw())
ggplot(dfeff_m_v1_he,
       aes(vmPFC,fit,colour=Group,fill=Group,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='vmPFC Long-term Value',y='Value of Next Choice',linetype='Reinforcement')

#DRS
m_v1_drs=lme4::lmer(value_chosen_vba_mfx~TOTA_MDRS*Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_drs)
car::Anova(m_v1_drs,'3') #sig but not a mediator

#WTAR
m_v1_wtr=lme4::lmer(value_chosen_vba_mfx~WTARSS*Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_wtr)
car::Anova(m_v1_wtr,'3') #somewhat mediates

#barratt nonplanning
m_v1_bnp=lme4::lmer(value_chosen_vba_mfx~NONPLAN*Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_bnp)
car::Anova(m_v1_bnp,'3') #also sig but not a mediator

#upps negative urgency
m_v1_unu=lme4::lmer(value_chosen_vba_mfx~UPPSPNEGURGENCY*Group*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_unu)
car::Anova(m_v1_unu,'3') #also sig but not a mediator

#baseline ideation: no group variable
m_v1_bssi=lme4::lmer(value_chosen_vba_mfx~`BASELINE SSI`*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_bssi)
car::Anova(m_v1_bssi,'3') #sig int

#max lethality: no group variable
m_v1_mlt=lme4::lmer(value_chosen_vba_mfx~max_lethality*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_mlt)
car::Anova(m_v1_mlt,'3')

#binary lethality in attempters
roi_gdf_att=subset(roi_gdf,Group=='Attempters')
m_v1_glt=lme4::lmer(value_chosen_vba_mfx~GroupLeth*scale(vmPFC)*reinf+(1|ID),data=roi_gdf_att)
summary(m_v1_glt)
car::Anova(m_v1_glt,'3')

pre_m_v1_glt=predict(m_v1_glt)
eff_m_v1_glt=Effect(c('vmPFC','reinf','GroupLeth'),m_v1_glt)
dfeff_m_v1_glt=as.data.frame(eff_m_v1_glt)
theme_set(theme_bw())
ggplot(dfeff_m_v1_glt,
       aes(vmPFC,fit,colour=GroupLeth,fill=GroupLeth,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='vmPFC Long-term Value',y='Value of Next Choice',color='Attempt Lethality',linetype='Reinforcement')

#all groups w/high vs. low lethality
m_v1_glta=lme4::lmer(value_chosen_vba_mfx~GroupLeth*scale(vmPFC)*reinf+(1|ID),data=roi_gdf)
summary(m_v1_glta)
car::Anova(m_v1_glta,'3')

pre_m_v1_glta=predict(m_v1_glta)
eff_m_v1_glta=Effect(c('vmPFC','reinf','GroupLeth'),m_v1_glta)
dfeff_m_v1_glta=as.data.frame(eff_m_v1_glta)
theme_set(theme_bw())
ggplot(dfeff_m_v1_glta,
       aes(vmPFC,fit,colour=GroupLeth,fill=GroupLeth,linetype=reinf))+
  geom_ribbon(colour=NA,alpha=0.1,
              aes(ymin=lower,ymax=upper))+
  geom_line()+
  theme(panel.border = element_blank(),panel.grid.minor=element_blank())+
  labs(x='vmPFC Long-term Value',y='Value of Next Choice',linetype='Reinforcement')


#### RT analyses ####
#(although careful with interpretation -- the value regressor was RT-convolved)

roi_gdf$invRT <- 1000/roi_gdf$RT
roi_gdf$invRT_lag <- 1000/roi_gdf$RT_lag
upper <- 4000
lower <- 200

rtdf <- roi_gdf[roi_gdf$RT>lower & roi_gdf$RT<upper,]

m_rt_roi <- lme4::lmer(scale(invRT) ~ scale(RT_lag) + trial_scaled + stay_lag +
                       reinf_lag*Group +  
                       v_max_lag_mfx*Group*scale(vmPFC) + (1 | ID),
                     data = rtdf)

summary(m_rt_roi)
car::Anova(m_rt_roi)
vif.lme(m_rt_roi)

#### save data ####
save(roi_gdf,m_lstr1,m_rstr1,m_rt_roi,m_v1,m_v1f,m_v1f_nc,m_v1g,m_v2,m_v2o,m_v3,m_v4,file='lmer_data_5_5_18.Rdata')
