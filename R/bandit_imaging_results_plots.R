color_palette <- c("#56B4E9", "#009E73", "#CC79A7","#E69F00","#D55E00", "#0072B2")

# differences in vmPFC ROI value ####

# #get data if needed ####
# ROI_data_dir='T:/learn/ssanalysis/bandit/vb_models/value_models/ROI_files'
# ROI_basename1='_ROI_betas_DecValueDiffZscored_dmUBLOCK1_CONp001_ZdmU1_vmPFC_mask'
# ROI_basename2='_ROI_betas_DecValueDiffZscored_dmUBLOCK1_Bartra_5wayconj_LStr_mask'
# ROI_basename3='_ROI_betas_DecValueDiffZscored_dmUBLOCK1_Bartra_5wayconj_RStr_mask'
# ROI_basename4='_ROI_betas_DecValueDiffZscored_dmUBLOCK1_Bartra_5wayconj_vmPFC_mask'
# ID_file='T:/learn/scripts/subj_ids_2162018.txt'
# 
# #### go to directory and preallocate data, etc.
# setwd(ROI_data_dir)
# func_vmPFC_ROI_files=list.files('.',pattern=paste0('*',ROI_basename1))
# LStr_ROI_files=list.files('.',pattern=paste0('*',ROI_basename2))
# RStr_ROI_files=list.files('.',pattern=paste0('*',ROI_basename3))
# vmPFC_ROI_files=list.files('.',pattern=paste0('*',ROI_basename4))
# num_subjs=length(vmPFC_ROI_files)
# out_data=matrix(data=NA,ncol=6,nrow=num_subjs)
# skip=0
# 
# #### loop through files and extract subj ID and avg beta 
# for (s in 1:num_subjs) {
#   #get subj ID
#   if (substr(vmPFC_ROI_files[s],6,6)=='_') { #five digit ID
#     out_data[s,1]=as.numeric(substr(vmPFC_ROI_files[s],1,5))
#   } 
#   else if (substr(vmPFC_ROI_files[s],7,7)=='_') { #six digit ID
#     out_data[s,1]=as.numeric(substr(vmPFC_ROI_files[s],1,6))
#   }
#   else skip=1
#   #read in file info and extract avg beta value, skipping subjects w/o maps
#   if (skip==0 && out_data[s,1]!=210100 && out_data[s,1]!=221050) {
#     #print(paste0('running ID #',as.character(out_data[s,1])))
#     out_data[s,2]=as.numeric(levels(factor(strsplit(as.character(read.csv(func_vmPFC_ROI_files[s],header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#     out_data[s,3]=as.numeric(levels(factor(strsplit(as.character(read.csv(RStr_ROI_files[s],header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#     out_data[s,4]=as.numeric(levels(factor(strsplit(as.character(read.csv(LStr_ROI_files[s],header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#     out_data[s,5]=as.numeric(levels(factor(strsplit(as.character(read.csv(vmPFC_ROI_files[s],header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#   } else print(paste0("file .",vmPFC_ROI_files[s]," does not fit format, skipped"))
#   skip=0
# }
# 
# 
# #### get group IDs and add 
# IDs_grp=read.table(ID_file,header=F,stringsAsFactors=F)
# for (s in 1:num_subjs) {
#   subj_line=IDs_grp[IDs_grp[,1]==out_data[s,1],]
#   out_data[s,6]=as.numeric(subj_line[2])
# }
# #eliminate skipped subjects & add column names
# out_data=out_data[out_data[,6]!=5,] #not sure about this subj
# out_data_red=as.data.frame(out_data[!is.na(out_data[,2]),])
# 
# names(out_data_red)=c("ID","func_vmPFC","RStr","LStr","vmPFC","Group")
# out_data_red$Group1245=out_data_red$Group
# out_data_red$Group1245[out_data_red$Group1245>5]=5
#
# vmPFC_roi_data=out_data_red

# read in data and plot ####
vmPFC_roi_data=read.csv("con_meta_vmPFC_RStr_LStr_data_5_4_18.csv",header=T,stringsAsFactors=F,sep=',')

con_vmPFC_roi_data=subset(vmPFC_roi_data,Group==1)
dep_vmPFC_roi_data=subset(vmPFC_roi_data,Group==2)
ide_vmPFC_roi_data=subset(vmPFC_roi_data,Group==4)
hlatt_vmPFC_roi_data=subset(vmPFC_roi_data,Group==7)
llatt_vmPFC_roi_data=subset(vmPFC_roi_data,Group==6)
att_vmPFC_roi_data=rbind(hlatt_vmPFC_roi_data,llatt_vmPFC_roi_data)
nocon_vmPFC_roi_data=rbind(dep_vmPFC_roi_data,ide_vmPFC_roi_data,att_vmPFC_roi_data)

vmPFC_roi_data$Groupl = relevel(as.factor(vmPFC_roi_data$Group),ref=5)
vmPFC_roi_data$Group1245l = relevel(as.factor(vmPFC_roi_data$Group1245),ref=4)
vmPFC_roi_data$Group=factor(vmPFC_roi_data$Group,levels=c('Controls','Depressed','Ideators','Attempters'))

summary(lm(vmPFC~as.factor(Group1245l),data=vmPFC_roi_data)) #att & con sig. different, att vs ide trending
summary(lm(vmPFC~as.factor(Groupl),data=vmPFC_roi_data)) #con trending
summary(aov(vmPFC~as.factor(Group1245),data=vmPFC_roi_data)) #p=.09
summary(aov(vmPFC~as.factor(Groupl),data=vmPFC_roi_data)) #p=.15

library(beeswarm)
beeswarm(vmPFC~Group1245,data=vmPFC_roi_data,labels=c('Controls','Depressed','Ideators','Attempters'),
         ylim=c(-.1,.127),pch=c(19,19,19,19),col=color_palette,
         ylab="vmPFC Value Beta",xlab="Group",bty='n',cex=1,cex.lab=1.5,cex.axis=1.5)
axis(2,labels=T,col="gray",cex.axis=1.5)
#abline(h=0)
boxplot(vmPFC~Group1245,data=vmPFC_roi_data,labels=c('Controls','Depressed','Ideators','Attempters'),
        ylim=c(-.1,.127),border=color_palette,lwd=.5,#lty=2,
        ylab="vmPFC Value Beta",xlab="Group",add=T,col=rgb(0,0,0,0,alpha=.1),
        names=c('Attempters','Controls','Depressed','Ideators'),axes=F,cex.lab=1.5)
abline(h=0,col='gray40')
#abline(h=mean(con_data$vmPFC),lty=3)

# UPPS PPI interaction plots ####

# # get data if needed
# # define paths and ROI/regressor types
# ROI_data_dir='T:/learn/ssanalysis/bandit/vb_models/value_models/ROI_files/PPI_ROIs/output'
# ROI_basename1='_PPI_ROI_betas_Feed_MaxNextValueDiff_Zscored_DMU1_'
# ROI_basename2='_Con_t2_k50_mask_09-30-18.csv'
# ID_file='T:/learn/scripts/subj_ids_2162018.txt'
# covariate_names=c('UPPSPNEGURGENCY','WTARSS')
# 
# #set up output
# setwd(ROI_data_dir)
# subj_info=read.table(ID_file,col.names=c('ID','Group'))
# num_subjs=dim(subj_info)[1]
# out_data=matrix(data=NA,ncol=4,nrow=num_subjs)
# 
# #loop through and extract beta estimates
# for (s in 1:num_subjs) {
#   subj=subj_info$ID[s]
#   out_data[s,1]=subj
#   
#   ROI_filename=paste0(subj,ROI_basename1,covariate_names[1],ROI_basename2)
#   if (subj!=210100 && subj!=221050 && subj!=881100 && subj!=220740 && file.exists(ROI_filename)) {
#     out_data[s,2]=as.numeric(levels(factor(strsplit(as.character(read.csv(ROI_filename,header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#     ROI_filename2=paste0(subj,ROI_basename1,covariate_names[2],ROI_basename2)
#     out_data[s,3]=as.numeric(levels(factor(strsplit(as.character(read.csv(ROI_filename2,header=F,sep=',',stringsAsFactors=F)$V1)," ")[[1]][1])))
#   }
#   
#   out_data[s,4]=subj_info$Group[s]
# }
# out_data=as.data.frame(out_data)
# colnames(out_data)=c('ID','UPPS_neg_ROI','WTAR_ROI','Group')
# out_data=out_data[out_data$Group!=5,] #not sure about this subj
# out_data_red=as.data.frame(out_data[!is.na(out_data[,2]),])
# out_data_red$group1245=out_data_red$Group
# out_data_red$group1245[out_data_red$group1245>5]=5
# 
# # add covariates and subset data
# load('C:/Users/brownvm2/Box Sync/skinner/personal_folders/Vanessa/roi_gdf.rdata')
# first_rg=subset(roi_gdf,Trial==1)
# out_data_red$Groupl = relevel(as.factor(out_data_red$Group),ref=5)
# out_data_red$Group1245l = relevel(as.factor(out_data_red$group1245),ref=4)
# out_data_all=merge(out_data_red,first_rg,by='ID')
# out_data_all$upppos_scale=scale(out_data_all$UPPSPPOSURGENCY)
#
# PPI_data=out_data_all

PPI_data=read.csv("PPI_UPPS_data_103018.csv",header=T,stringsAsFactors=F,sep=',')
PPI_data$Group1245f=factor(PPI_data$Group1245,labels=c('Controls','Depressed','Ideators','Attempters'))

con_data=subset(PPI_data,Group==1)
dep_data=subset(PPI_data,Group==2)
ide_data=subset(PPI_data,Group==4)
att_data=subset(PPI_data,Group>5)
hlatt_data=subset(att_data,Group==7)
llatt_data=subset(att_data,Group==6)
nocon_data=subset(PPI_data,Group>1)
con_data_wupps=con_data[!is.na(con_data$UPPSPNEGURGENCY),]
library(ggplot2)
con_lm=coef(lm(con_data_wupps$UPPS_neg_ROI~con_data_wupps$UPPSPNEGURGENCY))

main_plot=ggplot(nocon_data,aes(x = UPPSPNEGURGENCY, y = UPPS_neg_ROI, color=Group1245f)) +
  geom_point() + theme_minimal() + xlab('UPPS Negative Urgency scale') + ylab('Beta value of vmPFC connectivity with control ROI') +
  geom_smooth(aes(color=Group1245f, fill=Group1245f),method=lm,alpha=0.1,fullrange=T,level=.68) + 
  scale_fill_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) + 
  scale_color_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) +
  geom_rug(sides='bl',position='jitter')# + 
  #geom_abline(intercept=con_lm[1],slope=con_lm[2],aes(color='gray'),fullrange=T,level=.68)
main_plot

# connect to behavior ####

# # get data if needed
# load('C:/Users/brownvm2/Box Sync/skinner/personal_folders/Vanessa/roi_gdf.rdata')
# out_data_red$Groupl = relevel(as.factor(out_data_red$Group),ref=5)
# out_data_red$Group1245l = relevel(as.factor(out_data_red$group1245),ref=4)
# 
# # add variables
# roi_gdf$stay_lag2=NA
# roi_gdf$reinf_lag2=NA
# roi_gdf$rew_lag=NA
# roi_gdf$rew_lag2=NA
# roi_gdf$stake_lag2=NA
# roi_gdf$best_value_corr=NA
# roi_gdf$best_value_corr_lag=NA
# roi_gdf$best_value_corr_lag2=NA
# roi_gdf$best_value_corr_lag3=NA
# roi_gdf$best_value_corr_lag4=NA
# roi_gdf$bvc_bvi=NA
# roi_gdf$bvc_bvi_lag=NA
# roi_gdf$bvc_bvi_lag2=NA
# roi_gdf$bvc_bvi_lag3=NA
# roi_gdf$bvc_bvi_lag4=NA
# for (t in 1:dim(roi_gdf)[1]) {
#   if (roi_gdf$best_value_option_vba_mfx[t]==1) {
#     roi_gdf$best_value_corr[t]=roi_gdf$best_value_option_vba_mfx[t]*
#       unclass(roi_gdf$correct_incorrect[t])
#     roi_gdf$bvc_bvi[t]=roi_gdf$best_value_option_vba_mfx[t]*
#       unclass(roi_gdf$correct_incorrect[t])
#   } else {
#     roi_gdf$bvc_bvi[t]=(1-roi_gdf$best_value_option_vba_mfx[t])*
#       (1-unclass(roi_gdf$correct_incorrect[t]))
#   }
#   if (t>1 && roi_gdf$Trial[t]>1 && !is.na(roi_gdf$stay[t-1])) {
#     roi_gdf$rew_lag[t]=roi_gdf$reward[t-1]
#     roi_gdf$best_value_corr_lag[t]=roi_gdf$best_value_corr[t-1]
#     roi_gdf$bvc_bvi_lag[t]=roi_gdf$bvc_bvi[t-1]
#   }
#   if (t>2 && roi_gdf$Trial[t]>2 && !is.na(roi_gdf$stay[t-2])) {
#     roi_gdf$stay_lag2[t]=roi_gdf$stay[t-2]
#     roi_gdf$reinf_lag2[t]=roi_gdf$reinf[t-2]
#     roi_gdf$rew_lag2[t]=roi_gdf$reward[t-2]
#     roi_gdf$stake_lag2[t]=roi_gdf$stake[t-2]
#     roi_gdf$best_value_corr_lag2[t]=roi_gdf$best_value_corr[t-2]
#     roi_gdf$bvc_bvi_lag2[t]=roi_gdf$bvc_bvi[t-2]
#   }
#   if (t>3 && roi_gdf$Trial[t]>3 && !is.na(roi_gdf$stay[t-3])) {
#     roi_gdf$best_value_corr_lag3[t]=roi_gdf$best_value_corr[t-3]
#     roi_gdf$bvc_bvi_lag3[t]=roi_gdf$bvc_bvi[t-3]
#   }
#   if (t>4 && roi_gdf$Trial[t]>4 && !is.na(roi_gdf$stay[t-4])) {
#     roi_gdf$best_value_corr_lag4[t]=roi_gdf$best_value_corr[t-4]
#     roi_gdf$bvc_bvi_lag4[t]=roi_gdf$bvc_bvi[t-4]
#   }
# }
# roi_gdf$stake_vec=recode(unclass(roi_gdf$stake),"'1'=10;'2'=25;'3'=50")
# roi_gdf$stake_lag_vec=recode(unclass(roi_gdf$stake_lag),"'1'=10;'2'=25;'3'=50")
# roi_gdf$stake_lag2_vec=recode(unclass(roi_gdf$stake_lag2),"'1'=10;'2'=25;'3'=50")
# roi_gdf$reward_vec=recode(unclass(roi_gdf$reward),"'2'=10;'3'=25;'4'=50;'1'=0")
# roi_gdf$rew_lag_vec=recode(unclass(roi_gdf$rew_lag),"'2'=10;'3'=25;'4'=50;'1'=0")
# roi_gdf$rew_lag2_vec=recode(unclass(roi_gdf$rew_lag2),"'2'=10;'3'=25;'4'=50;'1'=0")
# 
# roi_gdf$group1245 = relevel(as.factor(roi_gdf$group1245),ref=4)
# roi_gdf$group12467 = relevel(as.factor(roi_gdf$group12467),ref=5)
# 
# PPI_behav_data=merge(PPI_data[,2:5],roi_gdf,by='ID')
# write.csv(PPI_behav_data,'PPI_behavioral_data_103018.csv')

PPI_behav_data=read.csv('PPI_behavioral_data_103018.csv',header=T,stringsAsFactors=F,sep=',')
library(lme4)
library(car)
PPI_behav_data$Group1245 = relevel(as.factor(PPI_behav_data$Group1245),ref=4)
PPI_behav_data$Group12467 = relevel(as.factor(PPI_behav_data$group12467),ref=5)
PPI_behav_nocon_data=PPI_behav_data[PPI_behav_data$Group1245!=1,]
PPI_behav_data_wupps=PPI_behav_nocon_data[!is.na(PPI_behav_nocon_data$UPPSPNEGURGENCY),]
PPI_behav_data_wupps$reinf_lag=as.factor(PPI_behav_data_wupps$reinf_lag)
PPI_behav_data_wupps$Group1245=as.factor(PPI_behav_data_wupps$Group1245)
PPI_behav_data_wcon_wupps=PPI_behav_data[!is.na(PPI_behav_data$UPPSPNEGURGENCY),]
PPI_behav_data_wcon_wupps$reinf_lag=as.factor(PPI_behav_data_wcon_wupps$reinf_lag)
PPI_behav_data_wcon_wupps$Group1245=as.factor(PPI_behav_data_wcon_wupps$Group1245)

# reinf=lmer(value_chosen~reinf_lag+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
# summary(reinf)
# reinf_grp=lmer(value_chosen~reinf_lag*Group1245+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
# summary(reinf_grp)
# anova(reinf,reinf_grp)
# reinf_UPPSNU=lmer(value_chosen~reinf_lag*scale(UPPSPNEGURGENCY)+reinf_lag2+stay_lag2+(1|ID),PPI_behav_data_wupps)
# summary(reinf_UPPSNU)
# reinf_ROI=lmer(value_chosen~reinf_lag*scale(UPPS_neg_ROI)+reinf_lag2+stay_lag2+(1|ID),PPI_behav_data_wupps)
# summary(reinf_ROI)
# anova(reinf,reinf_ROI) #w/ROI is better
reinf_ROI_grp=lmer(value_chosen~reinf_lag*Group1245*UPPS_neg_ROI+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
# summary(reinf_ROI_grp)
# anova(reinf_grp,reinf_ROI_grp)

reinf_ROI_grp_wcon=lmer(value_chosen~reinf_lag*Group1245*UPPS_neg_ROI+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wcon_wupps)


# reinf_UPPSNU_ROI=lmer(value_chosen~reinf_lag*scale(UPPSPNEGURGENCY)*UPPS_neg_ROI+reinf_lag2+stay_lag2+(1|ID),PPI_behav_data_wupps)
# summary(reinf_UPPSNU_ROI)

reinf_UPPSNU_grp=lmer(value_chosen~reinf_lag*scale(UPPSPNEGURGENCY)*Group1245+reinf_lag2+stay_lag2+(1|ID),PPI_behav_data_wupps)
# summary(reinf_UPPSNU_grp)
# reinf_UPPSNU_ROI_grp=lmer(value_chosen~reinf_lag*scale(UPPSPNEGURGENCY)*Group1245*scale(UPPS_neg_ROI)+reinf_lag2+stay_lag2+(1|ID),PPI_behav_data_wupps)
# summary(reinf_UPPSNU_ROI_grp)
# anova(reinf_UPPSNU_grp,reinf_UPPSNU_ROI_grp) #w/ROI is better
# 
# all_AIC=c(AIC(reinf),AIC(reinf_grp),AIC(reinf_UPPSNU),AIC(reinf_ROI),AIC(reinf_UPPSNU_grp),AIC(reinf_UPPSNU_ROI),AIC(reinf_UPPSNU_ROI_grp))-AIC(reinf)
# barplot(all_AIC)
# 
# car::Anova(reinf_ROI_grp)
# car::Anova(reinf_UPPSNU_ROI_grp)

library(effects)
testeff=effect(term='reinf_lag*Group1245*UPPS_neg_ROI',reinf_ROI_grp)
testedf=as.data.frame(testeff)
testedf$Group1245=as.factor(testedf$Group1245)
testedf$reinf_lag=as.factor(testedf$reinf_lag)

testeff_wc=effect(term='reinf_lag*Group1245*UPPS_neg_ROI',reinf_ROI_grp_wcon)
testedf_wc=as.data.frame(testeff_wc)
testedf_wc$Group1245=as.factor(testedf_wc$Group1245)
testedf_wc$reinf_lag=as.factor(testedf_wc$reinf_lag)



# plot
ROI_eff=ggplot(testedf,aes(x = UPPS_neg_ROI, y = fit, color=Group1245,linetype = reinf_lag,fill=Group1245)) +
  theme_minimal() + xlab('Beta value of vmPFC connectivity with control ROI') + ylab('Value of Next Option') +
  geom_line() + geom_ribbon(aes(ymin=fit-2*se,ymax=fit+2*se),alpha=0.3,color=NA) + 
  scale_color_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) +
  scale_linetype_manual(name="Previous \nReinforcement",labels=c('Not \nReinforced','Reinforced'),values=c(1,2)) +
  scale_fill_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) +
  geom_rug(data=PPI_behav_data_wupps,inherit.aes=F,aes(x=UPPS_neg_ROI,color=Group1245)) + coord_cartesian(ylim=c(0,1))
ROI_eff

#w/controls
ROI_eff_wc=ggplot(testedf_wc,aes(x = UPPS_neg_ROI, y = fit, color=Group1245,linetype = reinf_lag,fill=Group1245)) +
  theme_minimal() + xlab('Beta value of vmPFC connectivity with control ROI') + ylab('Value of Next Option') +
  geom_line() + geom_ribbon(aes(ymin=fit-2*se,ymax=fit+2*se),alpha=0.3,color=NA) + 
  scale_color_manual(name="Groups",labels=c('Controls','Depressed Controls','Ideators','Attempters'),values=color_palette[1:4]) +
  scale_linetype_manual(name="Previous \nReinforcement",labels=c('Not \nReinforced','Reinforced'),values=c(1,2)) +
  scale_fill_manual(name="Groups",labels=c('Controls','Depressed Controls','Ideators','Attempters'),values=color_palette[1:4]) +
  geom_rug(data=PPI_behav_data_wcon_wupps,inherit.aes=F,aes(x=UPPS_neg_ROI,color=Group1245)) + coord_cartesian(ylim=c(0,1))
ROI_eff_wc

testeffu=Effect(c('UPPSPNEGURGENCY','reinf_lag','Group1245'),reinf_UPPSNU_grp)
testedfu=as.data.frame(testeffu)
testedfu$Group1245=as.factor(testedfu$Group1245)
testedfu$reinf_lag=as.factor(testedfu$reinf_lag)
UPPS_eff=ggplot(testedfu,aes(x = UPPSPNEGURGENCY, y = fit, color=Group1245,linetype = reinf_lag,fill=Group1245)) +
  theme_minimal() + xlab('UPPS Negative Urgency scale') + ylab('Value of Next Option') +
  geom_line() + geom_ribbon(aes(ymin=fit-2*se,ymax=fit+2*se),alpha=0.3,color=NA) + 
  scale_color_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) +
  scale_linetype_manual(name="Previous \nReinforcement",labels=c('Not \nReinforced','Reinforced'),values=c(1,2)) +
  scale_fill_manual(name="Groups",labels=c('Depressed Controls','Ideators','Attempters'),values=color_palette[2:4]) +
  geom_rug(data=PPI_behav_data_wupps,inherit.aes=F,aes(x=UPPSPNEGURGENCY,color=Group1245)) + coord_cartesian(ylim=c(0,1))
UPPS_eff


# #effects on overall behavior
# PPI_behav_data$stay_num=as.numeric(PPI_behav_data$stay)
# perf_by_subj=aggregate(data.frame(PPI_behav_data$group1245,PPI_behav_data$group12467,PPI_behav_data$correct_incorrect,PPI_behav_data$UPPS_neg_ROI,
#                                   PPI_behav_data$UPPSPNEGURGENCY,PPI_behav_data$stay_num), list(PPI_behav_data$ID),mean,na.rm=T)
# perf_lm=lm(PPI_behav_data.correct_incorrect~as.factor(PPI_behav_data.group1245)*PPI_behav_data.UPPS_neg_ROI,data=perf_by_subj)
# summary(perf_lm)
# car::Anova(perf_lm)
# stay_lm=lm(PPI_behav_data.stay_num~as.factor(PPI_behav_data.group1245)*PPI_behav_data.UPPS_neg_ROI,data=perf_by_subj)
# summary(stay_lm)
# car::Anova(stay_lm)


### replot choices over time by group in color scheme
library(data.table)
recode_behav=melt(PPI_behav_data,na.rm = FALSE,measure.vars = c("choiceA", "choiceB", "choiceC"))
ggplot(recode_behav, aes(x = Trial, y = value, color = Group1245, linetype=variable)) + 
  stat_smooth(aes(fill=Group1245),method = "auto",size=.7,alpha=.2) + 
  theme_minimal() + xlab('Trial Number') + ylab("Choice probability")+
  theme(text=element_text(size=20)) +
  labs(linetype='Option') +
  scale_color_manual(name="Groups",labels=c('Attempters','Controls','Depressed','Ideators'),values=c(color_palette[4],color_palette[1:3])) +
  scale_fill_manual(name="Groups",labels=c('Attempters','Controls','Depressed','Ideators'),values=c(color_palette[4],color_palette[1:3])) +
  scale_linetype_discrete(name="Choice",
                          breaks=c("choiceA", "choiceB", "choiceC"),
                          labels=c("Choice A", "Choice B","Choice C"))
