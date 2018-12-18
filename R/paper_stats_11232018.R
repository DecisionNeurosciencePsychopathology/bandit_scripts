library(car)
library(lme4)
# overall performance ####
load("roi_gdf.rdata")
perf_by_subj=aggregate(as.numeric(roi_gdf$correct_incorrect)-1,by=list("ID"=roi_gdf$ID),sum)
perf_by_subj$prop_correct=perf_by_subj$x/300
subj_group=aggregate(roi_gdf$Group1245,by=list("ID"=roi_gdf$ID),mean)
perf_by_subj_wgroup=merge(subj_group,perf_by_subj,by="ID")
perf_by_group_aov=aov(perf_by_subj_wgroup[,3]~perf_by_subj_wgroup[,2])
perf_by_group=aggregate(perf_by_subj_wgroup$x.y,by=list("group"=perf_by_subj_wgroup$x.x),mean)
perf_by_group_sd=aggregate(perf_by_subj_wgroup$x.y,by=list("group"=perf_by_subj_wgroup$x.x),sd)
perf_by_group_wsd=merge(perf_by_group,perf_by_group_sd,by='group')
perf_by_group_prop=aggregate(perf_by_subj_wgroup$prop_correct,by=list("group"=perf_by_subj_wgroup$x.x),mean)
perf_by_group_sd_prop=aggregate(perf_by_subj_wgroup$prop_correct,by=list("group"=perf_by_subj_wgroup$x.x),sd)
perf_by_group_wsd_prop=merge(perf_by_group_prop,perf_by_group_sd_prop,by='group')

# vmPFC value ROI ####
vmPFC_roi_data=read.csv("con_meta_vmPFC_RStr_LStr_data_5_4_18.csv",header=T,stringsAsFactors=F,sep=',')

con_vmPFC_roi_data=subset(vmPFC_roi_data,Group==1)
dep_vmPFC_roi_data=subset(vmPFC_roi_data,Group==2)
ide_vmPFC_roi_data=subset(vmPFC_roi_data,Group==4)
hlatt_vmPFC_roi_data=subset(vmPFC_roi_data,Group==7)
llatt_vmPFC_roi_data=subset(vmPFC_roi_data,Group==6)
att_vmPFC_roi_data=rbind(hlatt_vmPFC_roi_data,llatt_vmPFC_roi_data)

t.test(con_vmPFC_roi_data$vmPFC,att_vmPFC_roi_data$vmPFC,var.equal = T)
t.test(dep_vmPFC_roi_data$vmPFC,att_vmPFC_roi_data$vmPFC,var.equal = T)
t.test(ide_vmPFC_roi_data$vmPFC,att_vmPFC_roi_data$vmPFC,var.equal = T)
t.test(llatt_vmPFC_roi_data$vmPFC,hlatt_vmPFC_roi_data$vmPFC,var.equal = T)
summary(aov(vmPFC~as.factor(Group1245),data=vmPFC_roi_data)) 

# vmPFC value connectivity w/UPPS ####
PPI_data=read.csv("PPI_UPPS_data_103018.csv",header=T,stringsAsFactors=F,sep=',')
PPI_data$Group1245f=factor(PPI_data$Group1245,labels=c('Controls','Depressed','Ideators','Attempters'))
PPI_data$Group1245f=relevel(PPI_data$Group1245f,ref='Attempters')

nocon_data=subset(PPI_data,Group>1)

nocon_PPI_UPPS_int=lm(UPPS_neg_ROI~Group1245f*UPPSPNEGURGENCY,data=nocon_data)
summary(nocon_PPI_UPPS_int)
car::Anova(nocon_PPI_UPPS_int)

nocon_PPI_WTAR_int=lm(UPPS_neg_ROI~Group1245f*WTARSS,data=nocon_data)
summary(nocon_PPI_WTAR_int)
car::Anova(nocon_PPI_WTAR_int)

nocon_PPI_EXIT_int=lm(UPPS_neg_ROI~Group1245f*EXITtot,data=nocon_data)
summary(nocon_PPI_EXIT_int)
car::Anova(nocon_PPI_EXIT_int)

# relationship of vmPFC value connectivity w/behavior ####
PPI_behav_data=read.csv('PPI_behavioral_data_103018.csv',header=T,stringsAsFactors=F,sep=',')
PPI_behav_data$Group1245 = relevel(as.factor(PPI_behav_data$Group1245),ref=4)
PPI_behav_nocon_data=PPI_behav_data[PPI_behav_data$Group1245!=1,]
PPI_behav_data_wupps=PPI_behav_nocon_data[!is.na(PPI_behav_nocon_data$UPPSPNEGURGENCY),]

reinf_ROI_grp=lmer(value_chosen~reinf_lag*Group1245*UPPS_neg_ROI+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
summary(reinf_ROI_grp)
car::Anova(reinf_ROI_grp)
reinf_ROI_grp_noint=lmer(value_chosen~reinf_lag*UPPS_neg_ROI+Group1245+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
anova(reinf_ROI_grp_noint,reinf_ROI_grp)

reinf_UPPS_grp=lmer(value_chosen~reinf_lag*Group1245*UPPSPNEGURGENCY+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
summary(reinf_UPPS_grp)
car::Anova(reinf_UPPS_grp)
reinf_UPPS_grp_noint=lmer(value_chosen~reinf_lag*UPPSPNEGURGENCY+Group1245+reinf_lag2+stay_lag2+(1|ID),data = PPI_behav_data_wupps)
anova(reinf_UPPS_grp_noint,reinf_UPPS_grp)

