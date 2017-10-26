# Vanessa's ROI analysis code with Alex's tweaks

#read in data & average values within ROIs
# data_dir='/Users/brownv/Documents/dnpl'
data_dir='~/code/bandit_scripts/roi_analyses/'

setwd(data_dir)
# subjects=unlist(read.table(paste0(data_dir,'/all_subj_ids.txt')))
subjects=unlist(read.table(paste0(data_dir,'/idlog_bandit_processed.txt')))

setwd('ROI_files_new')
roi_coords=c('12_10_n2','12_10_n6','2_46_n8','32_20_n6','4_22_44','4_34_n6',
'40_22_n6','6_n8_6','n12_12_n6','n12_4_2','n2_16_46','n2_28_28','n2_n22_n12',
'n30_22_n6','n36_20_n6','n4_n30_36','n6_28_n20','n6_n8_6')
out_data=matrix(data=NA,ncol=3,nrow=length(subjects)*length(roi_coords))

count=1
missing_subjects <- logical()
missing_list <- integer()
for (s in 1:length(subjects)) {
  if (subjects[s]!=210100) {
    for (r in 1:length(roi_coords)) {
      out_data[count,1]=subjects[s]
      if (file.exists(paste0(subjects[s],'_t_value_ROIs_6mm_',roi_coords[r],'.csv'))) {
      in_data=read.csv(paste0(subjects[s],'_t_value_ROIs_6mm_',roi_coords[r],'.csv'),header=FALSE,sep=' ')
      out_data[count,2]=mean(in_data[,4])
      out_data[count,3]=r
      count=count+1
      missing_subjects[s] <- 0
      } else if (r==1) {
        print(paste0('no data for subject # ',subjects[s]))
        missing_subjects[s] <- 1
          missing_list <- c(missing_list, subjects[s])
      }
    }
}
 }

# Alex's hacky way of picking existing subjects:
actual_subjects <- unique(na.omit(out_data[,1]))
out_data_red=cbind(na.omit(out_data),matrix(data=NA,nrow=(length(actual_subjects))*(length(roi_coords)-1),ncol=1))

# out_data_red=cbind(na.omit(out_data),matrix(data=NA,nrow=(length(subjects)-1)*length(roi_coords),ncol=1))

#plot histograms for each roi to check for outliers
for (r in 1:(length(roi_coords)-1)) {
  roi_data=out_data_red[out_data_red[,3]==r,]
  hist(roi_data[,2])
}

#get group numbers and add to matrix
setwd(data_dir)
group_ids=read.csv('ID_groups.csv',header=TRUE,sep=',')
for (c in 1:(dim(out_data_red)[1])) {
  subj=out_data_red[c,1]
  grp_subj=as.numeric(group_ids[group_ids[,1]==subj,][2])
  out_data_red[c,dim(out_data_red)[2]]=grp_subj
}
out_data_red=as.data.frame(out_data_red)
names(out_data_red)=c('Subject','Beta','ROI_num','Group')
out_data_red$ROI_num <- as.factor(out_data_red$ROI_num)
out_data_red$Group <- as.factor(out_data_red$Group)

library(ggplot2)
ggplot(out_data_red, aes(ROI_num, Beta, color = Group)) +
  geom_boxplot(notch = TRUE) #+ geom_jitter(width = 0.2)

ggplot(out_data_red, aes(Group, Beta, color = Group)) +
  geom_boxplot(notch = TRUE) #+ geom_jitter(width = 0.2)


library(lme4)
library(lsmeans)
lm_rois=lmer(Beta~Group*ROI_num + (1|Subject),data=out_data_red)
summary(lm_rois) #group 2 vs. controls: t=-1.36; group 3 vs. controls: t=-1.348; 
car::Anova(lm_rois)
ls1 <- lsmeans(lm_rois, "Group", by = "ROI_num")
plot(ls1, horiz = F)
#group 4 vs. controls: t=-0.834

#without interactions
lm_rois1=lmer(Beta~Group + ROI_num + (1|Subject),data=out_data_red)
anova(lm_rois,lm_rois1)

#ROIs individually
for (r in 1:length(roi_coords)-1) {
  roi_data=out_data_red[out_data_red[,3]==r,]
  lm_roi_ind=lm(Beta~as.factor(Group),data=roi_data)
  print(r)
  print(roi_coords[r])
  print(summary(lm_roi_ind)) 
  #significant differences vs. controls: -6,28,-20 (groups 2&3, OFC); -4,-30,26 (group 3; PCC); 4,22,44 (group 3; dmPFC)
}
