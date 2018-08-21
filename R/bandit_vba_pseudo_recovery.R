#Read in VBA posteriors;

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
zeronoise=getprocdata('/Users/jiazhouchen/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data/2lr_decay/0/csvoutput'),
onenoise=getprocdata('/Users/jiazhouchen/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data/2lr_decay/0.2/csvoutput'),
twonoise=getprocdata('/Users/jiazhouchen/Box Sync/skinner/data/eprime/bandit/vba_pseudosub/data/2lr_decay/0.4/csvoutput')
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


#########################################################
##############END OF DATA PRE-PROC ######################
#########################################################

##############bdf is the data that has info on trial level 
##############joint_reshape is the data that info on subject level



library(tidyr)
library(tibble)
library(dplyr)

# classify some subjects as noisy
bdf$noisy <- scale(bdf$muPhi)>0 
bdf$noisy <- bdf$noisy[,1]
bdf = bdf %>% as_tibble %>% arrange(ID, Trial)
bdf$reinf <- bdf$correct_incorrect==1
bdf$NOISE <- as.factor(bdf$NOISE)
bdf = bdf %>% arrange(ID, Trial) %>% group_by(ID) %>% 
  mutate(
    reinf_lag = lag(reinf ),
    choice_num_lag = lag(choice_numeric ),
    choice_num_lead = lead(choice_numeric )
  ) %>% ungroup()

bdf$stay <- bdf$choice_numeric == bdf$choice_num_lead
bdf = bdf %>% group_by(ID) %>%
  mutate(stay_lag = lag(stay )) %>% ungroup()
bdf$stay <- as.factor(bdf$stay)
bdf$stay_lag <- as.factor(bdf$stay_lag)

bdf$trial_scaled <- scale(bdf$Trial)
bdf$trial_scaled <- bdf$trial_scaled[,1]

stop("Here we done with preproc")
###################################
m1 <- lm(data=joint_reshape,formula = muPhi.pseudo ~ muPhi.original * muTheta_1.original * muTheta_2.original * muTheta_3.original * NOISE)
summary(m1)




summary(lm(data = joint_reshape[joint_reshape$NOISE=='0',],formula = muTheta_1.pseudo ~ muTheta_1.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=='0.2',],formula = muTheta_1.pseudo ~ muTheta_1.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=='0.4',],formula = muTheta_1.pseudo ~ muTheta_1.original))






##############################


mr0 <-   glmer(
  stay ~  trial_scaled + stay_lag + reinf +  
    (1 | ID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr0)
car::Anova(mr0, type = 'III')



mr1 <-   glmer(
  stay ~  trial_scaled + stay_lag + reinf  * scale(muPhi)  +  
    (1 | ID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr1)
car::Anova(mr1, type = 'III')

mr1a <-   glmer(
  stay ~  trial_scaled + stay_lag + reinf  * scale(muPhi) * noise  +  
    (1 | ID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr1a)
car::Anova(mr1a, type = 'III')

mr2 <-   glmer(
  stay ~  trial_scaled +  reinf  * noisy  +  
    (1 | ID),
  family = binomial(),
  data = bdf,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
summary(mr2)
car::Anova(mr2, type = 'III')

summary(lmer(data = bdf,formula = ))



write.csv(bdf,'NOISE_LONG_bandit_vba_pseudosub_w_actual_recovered_value.csv')
wdf<-reshape(data = bdf,timevar = 'NOISE',direction = 'wide',idvar = c('ID','Trial'),sep = "_")
write.csv(wdf,'NOISE_WIDE_bandit_vba_pseudosub_w_actual_recovered_value.csv')


bdf$NOISE<-plyr::mapvalues(bdf$NOISE,c(0,0.2,0.4),c("No Noise","20% Noise","40% Noise"))
joint_reshape$NOISE<-plyr::mapvalues(joint_reshape$NOISE,c(0,0.2,0.4),c("No Noise","20% Noise","40% Noise"))

# look at beta recovery by level of noise

summary(lm(data = joint_reshape[joint_reshape$NOISE=='No Noise',],formula = muPhi.pseudo ~ muPhi.original))
#summary(lm(data = joint_reshape[joint_reshape$NOISE=='0.1',],formula = muPhi.pseudo ~ muPhi.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=="20% Noise",],formula = muPhi.pseudo ~ muPhi.original))
summary(lm(data = joint_reshape[joint_reshape$NOISE=="40% Noise",],formula = muPhi.pseudo ~ muPhi.original))


summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID), bdf[bdf$NOISE=='No Noise',]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=='No Noise',]))

summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID/NOISE), bdf[bdf$NOISE=="20% Noise",]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=="20% Noise",]))

summary(vm1 <- lme4::lmer(value_max_actual ~ value_max_recover + (1|ID/NOISE), bdf[bdf$NOISE=="40% Noise",]))
summary(vm0 <- lm(value_max_actual ~ value_max_recover, bdf[bdf$NOISE=="40% Noise",]))



#ggplot(joint_reshape,aes(NOISE,muPhi.pseudo)) + geom_boxplot()
#ggplot(joint_reshape,aes(muPhi.original,muPhi.pseudo)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0)
ggplot(joint_reshape,aes(muPhi.original,muPhi.pseudo)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0)+ xlab("Original Temperature") + ylab("Recovered Temperature")
#ggplot(joint_reshape,aes(muTheta_3.original,muTheta_3.pseudo)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0)
ggplot(bdf,aes(value_max_actual,value_max_recover)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0) + xlab("Actaul Value (muX)") + ylab("Recovered Value (muX')")
ggplot(bdf,aes(PE_actual,PE_recover)) + geom_point() + facet_wrap(~NOISE) + geom_abline(slope = 1, intercept = 0) + xlab("Actual Prediction Error (PE)") + ylab("Recovered Prediction Error (PE')")
