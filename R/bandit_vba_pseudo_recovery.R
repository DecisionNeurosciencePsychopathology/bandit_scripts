#Read in VBA posteriors;
source('~/code/R/vif.lme.R')
#####
findbox<-function() {
  if (Sys.getenv("USER")=="jiazhouchen") {boxdir <- "/Users/jiazhouchen/Box Sync"
  } else if (Sys.getenv("USER")=="jiazhou") {boxdir <- "/Volumes/bek/Box Sync"} else {
    boxdir<-system("find ~ -iname 'Box*' -maxdepth 2 -type d",intern = T)}
  return(boxdir)
}

boxidir<-findbox()
#or specify box directory there:
#boxidir<-''
#####
setwd('~/code/bandit_scripts/R')

pseudosub<-lapply(c(0,2,4),function(x) {
  xz<-read.csv(paste0('noise0.',x,'.csv'))
  xz$NOISE<-as.numeric(paste0('0.',x))
  return(xz)
  })
pseudosub<-do.call(rbind,pseudosub)
pseudosub$Type<-'pseudo'
pseudosub<-pseudosub[order(pseudosub$ID),]
pseudosub$count<-unlist(lapply(split(pseudosub$ID,pseudosub$ID),seq_along))
pseudosub$cbID<-paste0(pseudosub$ID,"_",pseudosub$count)
pseudo1<-pseudosub[which(pseudosub$count==1),]
original<-read.csv('original.csv')
original$Type<-'original'
original$count<-1
original$NOISE<-0
original$cbID<-paste0(original$ID,"_",original$count)
neworiginal<-do.call("rbind", replicate(3, original, simplify = FALSE))
neworiginal<-neworiginal[order(neworiginal$ID),]
neworiginal$count<-unlist(lapply(split(neworiginal$ID,neworiginal$ID),seq_along))
neworiginal$cbID<-paste0(neworiginal$ID,"_",neworiginal$count)
neworiginal$NOISE<-0

#joint them by each subject;

jointsub<-lapply(unique(neworiginal$cbID),function(x) {
  list(pseudo=pseudosub[which(pseudosub$cbID == x),],
       original=neworiginal[which(neworiginal$cbID == x),])
})
names(jointsub)<-unique(neworiginal$cbID)

rbind(pseudosub,neworiginal)->jointall
#rbind(pseudo1,original)->joint1

joint_reshape<-reshape(data = jointall,timevar = 'Type',direction = 'wide',idvar = c("cbID"),drop = c("count","ID"))
joint_reshape$ID<-gsub("_[0-9]*","",joint_reshape$cbID)
joint_reshape$NOISE<-as.factor(joint_reshape$NOISE.pseudo)
joint_reshape$NOISE.original<-NULL
joint_reshape$NOISE.pseudo<-NULL
joint_reshape$cbID<-paste0(joint_reshape$ID,"_",joint_reshape$NOISE)


# allstats<-list(lm=list(),lmer=list())
# for (todo in c('muTheta_1','muTheta_2','muTheta_3','muPhi')){
#   joint_reshape$pseudo<-joint_reshape[[paste0(todo,".","pseudo")]]
#   joint_reshape$original<-joint_reshape[[paste0(todo,".","original")]]
#   allstats$lm[[paste0(todo,"_lm")]]<-lm(pseudo ~ original, data=joint_reshape)  # build linear regression model on full data
#   #allstats$lmer[[paste0(todo,"_lmer")]] <- lmer(pseudo ~ original + (original | ID), data=joint_reshape)  # build linear regression model on full data
# }



##################GET GENERATIVE ORIGINAL
#rootp<-'/Users/jiazhouchen/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data/2lr_decay/csvoutput'
getprocdata<-function(rootp) {
list.files(rootp,pattern = "*.csv") ->listf
index<-data.frame(file=listf,
           ID=sapply(strsplit(listf,split="_"),"[[",2),
           count=unlist(lapply(split(sapply(strsplit(listf,split="_"),"[[",2),sapply(strsplit(listf,split="_"),"[[",2)),seq_along)))
rownames(index)<-NULL
# alldata<-apply(index,1,function(x) {
#   fname<-as.character(x[1])
#   id<-x[2]
#   count<-x[3]
#   list(d=read.csv(file.path(rootp,fname),as.is = T,stringsAsFactors = F),
#        id=id,
#        count=count)
#   })
alldata<-lapply(unique(index$ID),function(x) {
  id<-as.character(x)
  subindex<-index[which(index$ID==id),]
  subindex$fullpath<-file.path(rootp,subindex$file)
  subdata<-lapply(subindex$fullpath,function(x) {
    xd<-read.csv(x)
    xd$PE_actual<-xd$values_4
    xd$values_4<-NULL
    xd$value_max_actual<-apply(xd[grep("value",names(xd))],1,max)
    xd$value_choosen_actual<-apply(xd[grep(paste("decision_1","value",sep = "|"),names(xd))],1, function(x) {
      ixd<-x[1]
      dx<-x[2:4]
      j<-dx[ixd]
      })
    xd$PE_recover<-xd$muXs_4
    xd$muX_4<-NULL
    xd$value_max_recover<-apply(xd[grep("muXs",names(xd))],1,max)
    xd$value_choosen_recover<-apply(xd[grep(paste("decision_1","muXs",sep = "|"),names(xd))],1, function(x) {
      ixd<-x[5]
      dx<-x[1:4]
      j<-dx[ixd]
    })
    
    #xd$value_max<-value_max
    #xd$value_choosen<-value_choosen
    xe<-data.frame(ID=xd$ID,
                   Trial=seq_along(xd$ID),
                   choice_numeric=xd$decision_1,
                   correct_incorrect=xd$decision_2,
                   PE_actual=xd$PE_actual,
                   value_max_actual=xd$value_max_actual,
                   value_choosen_actual=xd$value_choosen_actual,
                   PE_recover=xd$PE_recover,
                   value_max_recover=xd$value_max_recover,
                   value_choosen_recover=xd$value_choosen_recover,
                   NOISE=xd$noise
                   )
    return(xe)
  }) 
  names(subdata)<-subindex$count
  return(subdata)
})
names(alldata)<-unique(index$ID)
return(alldata)
}

alldata<-list(
zeronoise=getprocdata(file.path(boxidir,'skinner','data','eprime','bandit','vba_pseudosub','data_unbound','2lr_decay','0','csvoutput')),
onenoise=getprocdata(file.path(boxidir,'skinner','data','eprime','bandit','vba_pseudosub','data_unbound','2lr_decay','0.2','csvoutput')),
twonoise=getprocdata(file.path(boxidir,'skinner','data','eprime','bandit','vba_pseudosub','data_unbound','2lr_decay','0.4','csvoutput'))
)
adata<-lapply(alldata,function(xk) {
do.call(rbind,lapply(xk,function(x){
  #x<-x[1]  #Use this to get only one
  do.call(rbind,x)
}))
})
alldata_com<-do.call(rbind,adata)
rownames(alldata_com)<-NULL

cleanorg<-original[c(1:4,6)]

alldata_com_w_para<-merge(alldata_com,cleanorg,all = T)

bdf<-alldata_com_w_para

library(tidyr)
library(tibble)
library(dplyr)
library(ggplot2)
# classify some subjects as noisy
bdf$noisy <- scale(bdf$muPhi)>0 
bdf$noisy <- bdf$noisy[,1]
bdf = bdf %>% as_tibble %>% arrange(ID, Trial)
bdf$reinf <- as.factor(bdf$correct_incorrect==1)
bdf$NOISE <- as.factor(bdf$NOISE)
bdf$newID <- interaction(bdf$ID,bdf$NOISE)
bdf = bdf %>% arrange(newID, Trial) %>% group_by(newID) %>% 
  mutate(
    reinf_lag = lag(reinf ),
    choice_num_lag = lag(choice_numeric ),
    choice_num_lead = lead(choice_numeric )
  ) %>% ungroup()

bdf = bdf %>% group_by(newID) %>%
  mutate(stay = as.factor(choice_numeric == choice_num_lead),
    stay_lag = lag(stay )
  ) %>% ungroup()

bdf$trial_scaled <- scale(bdf$Trial)
bdf$trial_scaled <- bdf$trial_scaled[,1]


bdf$NOISE<-plyr::mapvalues(bdf$NOISE,c(0,0.2,0.4),c("sigma(noise)=0","sigma(noise)=0.2","sigma(noise)=0.4"))


joint_reshape$NOISE<-plyr::mapvalues(joint_reshape$NOISE,c(0,0.2,0.4),c("sigma(noise)=0","sigma(noise)=0.2","sigma(noise)=0.4"))

write.csv(bdf,'NOISE_LONG_bandit_vba_pseudosub_w_actual_recovered_value.csv')
wdf<-reshape(data = bdf,timevar = 'NOISE',direction = 'wide',idvar = c('ID','Trial'),sep = "_")
write.csv(wdf,'NOISE_WIDE_bandit_vba_pseudosub_w_actual_recovered_value.csv')

stop("Here we done with preproc")


#########################################################
##############END OF DATA PRE-PROC ######################
#########################################################

##############bdf is the data that has info on trial level 
##############joint_reshape is the data that info on subject level



###################################
m1 <- lm(data=joint_reshape,formula = muPhi.pseudo ~ muPhi.original * muTheta_1.original * muTheta_2.original * muTheta_3.original * NOISE)
summary(m1)

summary(m1 <- lm(data = joint_reshape[joint_reshape$NOISE=="sigma(noise)=0",],formula = muPhi.pseudo ~ muPhi.original))
summary(m2 <- lm(data = joint_reshape[joint_reshape$NOISE=="sigma(noise)=0.2",],formula = muPhi.pseudo ~ muPhi.original))
summary(m3 <- lm(data = joint_reshape[joint_reshape$NOISE=="sigma(noise)=0.4",],formula = muPhi.pseudo ~ muPhi.original))

##############################
library(lme4)

mr0 <-   glmer(
  stay ~  trial_scaled + stay_lag + reinf +  
    (1 | newID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr0)
car::Anova(mr0, type = 'III')


# recover 
summary(mr1_0 <-   glmer(
  stay ~  reinf * muPhi    +  
    (1 | newID),
  family = binomial(),
  data = bdf[bdf$NOISE=='sigma(noise)=0',],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))
car::Anova(mr1, type = 'III')

summary(mr1_02 <-   glmer(
  stay ~  reinf * muPhi    +  
    (1 | newID),
  family = binomial(),
  data = bdf[bdf$NOISE=='sigma(noise)=0.2',],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))
summary(mr1_02)

summary(mr1_04 <-   glmer(
  stay ~  reinf * muPhi    +  
    (1 | newID),
  family = binomial(),
  data = bdf[bdf$NOISE=='sigma(noise)=0.4',],
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))
summary(mr1_04)

anova(mr1_0,mr1_02,mr1_04)

summary(mr_all <-   glmer(
  stay ~  reinf * muPhi * NOISE  +  
    (1 | newID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))
car::Anova(mr1a, type = 'III')

# plot modeling results

library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggpubr)
theme_set(theme_sjplot())

stargazer(m1,m2,m3,  type="html", out="mu_recovery.htm", digits = 2,single.row=TRUE,omit.stat = c("bic","adj.rsq", "f", "ser"),
          column.labels = c("Sigma(noise) = 0", "Sigma(noise) = 0.2", "Sigma(noise) = 0.4"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001), report = "vt", 
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

stargazer(mr1_0,mr1_02,mr1_04,  type="html", out="mlm_mu_recovery.htm", digits = 2,single.row=TRUE,omit.stat = c("bic", "aic"),
          column.labels = c("Sigma(noise) = 0", "Sigma(noise) = 0.2", "Sigma(noise) = 0.4"),
          star.char = c("+", "*", "**", "***"),
          star.cutoffs = c(0.1, 0.05, 0.01, 0.001),report = "vt", 
          notes = c("+ p<0.1; * p<0.05; ** p<0.01; *** p<0.001"), 
          notes.append = F)

stargazer(mr1_0,mr1_02,mr1_04)
stargazer(m1,m2,m3)



library(effects)
plot(allEffects(mr1a))

library(emmeans)
plot(emmeans(mr1a,"reinf", by = "NOISE"), horiz = F)

mr2 <-   glmer(
  stay ~  trial_scaled +  reinf  * noisy  +  
    (1 | ID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr2)
car::Anova(mr2, type = 'III')


# look at beta recovery by level of noise

summary(lm(data = joint_reshape[joint_reshape$NOISE=='sigma(noise)=0',],formula = muPhi.pseudo ~ muPhi.original))
#summary(lm(data = joint_reshape[joint_reshape$NOISE=='0.1',],formula = muPhi.pseudo ~ muPhi.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=="sigma(noise)=0.2",],formula = muPhi.pseudo ~ muPhi.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=="sigma(noise)=0.4",],formula = muPhi.pseudo ~ muPhi.original))


summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID), bdf[bdf$NOISE=='sigma(noise)=0',]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=='sigma(noise)=0',]))

summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID), bdf[bdf$NOISE=="sigma(noise)=0.2",]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=="sigma(noise)=0.2",]))

summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID), bdf[bdf$NOISE=="sigma(noise)=0.4",]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=="sigma(noise)=0.4",]))




setwd('~/Box Sync/skinner/projects_analyses/Project Bandit/figs/')
#ggplot(joint_reshape,aes(NOISE,muPhi.pseudo)) + geom_boxplot()
#ggplot(joint_reshape,aes(muPhi.original,muPhi.pseudo)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0)
names(joint_reshape)[11] <- name
  p <- ggplot(joint_reshape,aes(muPhi.original,muPhi.pseudo)) + geom_point() + facet_wrap(~`sigma[noise]`) + geom_abline(slope = 1, intercept = 0)+ xlab("Original Temperature") + ylab("Recovered Temperature")
ggsave("mu_recovery.pdf", p, device = "pdf", scale = .4)
#ggplot(joint_reshape,aes(muTheta_3.original,muTheta_3.pseudo)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0)
ggplot(bdf,aes(value_max_actual,value_max_recover)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0) + xlab("Actaul Value (muX)") + ylab("Recovered Value (muX')")
ggplot(bdf,aes(PE_actual,PE_recover)) + geom_smooth() + facet_wrap(~NOISE)  + xlab("Actual Prediction Error (PE)") + ylab("Recovered Prediction Error (PE')")

# sanity checks to deal with low z-stats for the interaction between muPhi and last reward

# fig s1 -- MLM vs RL for estimating individual differences

test <- merge(joint_reshape[joint_reshape$NOISE=='sigma(noise)=0',],cleanorg, by = "ID")
cor(test$muPhi.original,test$muPhi)
trl <- c(23.87,18.01, 12.35)
tmlm <- c(22.93,18.67,13.57)
df <- as.data.frame(c(trl,tmlm),col.names = "t")
df$t <- df$`c(trl, tmlm)`
df$sigma_noise <- c(0,0.2,0.4,0,0.2,0.4)
df$method <- c("RL","RL","RL","MLM","MLM","MLM")
p <- ggplot(df,aes(sigma_noise,t, lty = method)) + geom_point() + geom_line() + xlab(expression(sigma[noise])) + ylab("t-statistic")
ggsave("figs1_t.pdf",p,scale = .3)
# audit bdf
# check values signals across noise levels



