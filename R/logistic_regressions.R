###########
#behavioral analyses: after Walton et al., 2010
#note: csv files are from preprocess_r.R script
##########

#to do: figure 6

#### set up data ####

#toggle whether to print individual subjects' figures or only aggregates and set fig size
make_subj_figs=1
par(mfrow=c(2,2))

#read in data
data_dir='/Users/brownv/Documents/dnpl/bandit_scripts/subjects/csvs/'
subj_list=list.files(data_dir,recursive=FALSE)
all_subj_data=read.csv(paste0(data_dir,'all_reshaped_preprocessed.csv'),header=TRUE,sep=',')
subj_ids=all_subj_data$subjID[seq(1,dim(all_subj_data)[1],270)]

#set up output matrices to store regressions weights per subj
choice_x_reward=matrix(data=NA,ncol=6,nrow=length(subj_ids))
past_choice_x_reward=matrix(data=NA,ncol=6,nrow=length(subj_ids))
choice_x_past_reward=matrix(data=NA,ncol=6,nrow=length(subj_ids))
all_choice_x_reward=matrix(data=NA,ncol=26,nrow=length(subj_ids))

#### loop through subjects and plot individual regression coefficients ####

for (subj in 1:length(subj_ids)) {
  subj_data=all_subj_data[all_subj_data$subjID==subj_ids[subj],]
  
  #regressions for each stimulus
  A_reg=glm(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+
             A2_rew1+A2_rew2+A2_rew3+A2_rew4+A2_rew5+A2_rew6+
             A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+A3_rew6+
             A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+
             A5_rew1+A5_rew2+A5_rew3+A5_rew4+A5_rew5+A5_rew6+
             A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+A6_rew6,data=subj_data,family=binomial)
  B_reg=glm(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+
             B2_rew1+B2_rew2+B2_rew3+B2_rew4+B2_rew5+B2_rew6+
             B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+B3_rew6+
             B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+
             B5_rew1+B5_rew2+B5_rew3+B5_rew4+B5_rew5+B5_rew6+
             B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+B6_rew6,data=subj_data,family=binomial)
  C_reg=glm(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+
             C2_rew1+C2_rew2+C2_rew3+C2_rew4+C2_rew5+C2_rew6+
             C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+C3_rew6+
             C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+
             C5_rew1+C5_rew2+C5_rew3+C5_rew4+C5_rew5+C5_rew6+
             C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+C6_rew6,data=subj_data,family=binomial)
  
  #variances of coefficients for each regression
  A_diag=diag(vcov(A_reg))
  B_diag=diag(vcov(B_reg))
  C_diag=diag(vcov(C_reg))
  #note: in Walton 2010 they call them covariances but that would be a matrix, plus it 
  # makes more sense to weight by variance, but the initial step to use the full 
  # variance-covariance matrix is below:
  #
  # A_inv_vcov=solve(vcov(A_reg))
  # B_inv_vcov=solve(vcov(B_reg))
  # C_inv_vcov=solve(vcov(C_reg))
  
  #for some subjects some regression coefficients are undefined: check and fill in NAs if needed
  if (sum(is.na(A_reg$coefficients))>0) {
    A_diag_short=A_diag
    A_diag=rep(NA,37,1)
    A_diag[!is.na(A_reg$coefficients)]=A_diag_short
  }
  if (sum(is.na(B_reg$coefficients))>0) {
    B_diag_short=B_diag
    B_diag=rep(NA,37,1)
    B_diag[!is.na(B_reg$coefficients)]=B_diag_short
  }
  if (sum(is.na(C_reg$coefficients))>0) {
    C_diag_short=C_diag
    C_diag=rep(NA,37,1)
    C_diag[!is.na(C_reg$coefficients)]=C_diag_short
  }
  
  #variance-weighted average of regression weights across stimuli (using variances and not covariances, see note above)
  all_coeffs=(1/rowSums(cbind(1/A_diag,1/B_diag,1/C_diag),na.rm=TRUE))*(rowSums(cbind(A_reg$coefficients/A_diag,B_reg$coefficients/B_diag,C_reg$coefficients/C_diag),na.rm=TRUE))
  
  #check that no NAs remain after combining regression, otherwise skip
  if (sum(is.na(all_coeffs))>0) {
    print(paste0('subject #',subj_ids[subj],' not used, skipping.'))
  } else {
    
    #plot coefficients to check (black = averaged)
    if (make_subj_figs==1) {
      min_val=min(c(A_reg$coefficients[2:37],B_reg$coefficients[2:37],C_reg$coefficients[2:37],all_coeffs[2:37]),na.rm=TRUE)
      max_val=max(c(A_reg$coefficients[2:37],B_reg$coefficients[2:37],C_reg$coefficients[2:37],all_coeffs[2:37]),na.rm=TRUE)
      plot(1:36,A_reg$coefficients[2:37],col='blue',ylim=c(min_val,max_val),pch=16,ylab='beta weights',xlab='coefficient',main=paste0('subject #',subj_ids[subj]))
      lines(1:36,A_reg$coefficients[2:37],col='blue')
      points(1:36,B_reg$coefficients[2:37],col='seagreen',pch=16)
      lines(1:36,B_reg$coefficients[2:37],col='seagreen')
      points(1:36,C_reg$coefficients[2:37],col='purple',pch=16)
      lines(1:36,C_reg$coefficients[2:37],col='purple')
      points(1:36,all_coeffs[2:37],col='black',pch=16)
      lines(1:36,all_coeffs[2:37],col='black',lwd=2)
    }
    
    #add to output matrices, omitting 6-back coefficients & intercept
    choice_x_reward[subj,]=c(subj_ids[subj],all_coeffs[seq(2,30,7)]) #diagonal in fig 5A
    past_choice_x_reward[subj,]=c(subj_ids[subj],all_coeffs[seq(2,30,6)]) #vertical column in 5A
    choice_x_past_reward[subj,]=c(subj_ids[subj],all_coeffs[2:6]) #horizontal column in 5A
    all_choice_x_reward[subj,]=c(subj_ids[subj],all_coeffs[-c(seq(1,37,6),32:36)]) #all
    
    #make fig 5B for subject
    if (make_subj_figs==1) {
      range_coeffs=max(all_coeffs[2:31])-min(all_coeffs[2:31])
      subj_coeffs_log_scaled=log((all_coeffs+abs(min(all_coeffs[2:31]))+range_coeffs+.01)/range_coeffs) #normalize to be between 0 & 1 after log transformation
      plot(5:1,5:1,xlab='',ylab='',col='white',xlim=c(.5,5.5),ylim=c(.5,5.5),bty='n',axes=FALSE)
      axis(2,at=c(1,2,3,4,5),labels=c('t-5','t-4','t-3','t-2','t-1'),lty=0) 
      axis(3,at=c(1,2,3,4,5),labels=c('t-1','t-2','t-3','t-4','t-5'),lty=0) 
      mtext('reward',side=3,line=2)
      mtext('choice',side=2,line=2)
      mtext(subj_ids[subj],side=3,line=3,font=2)
      rect(.5,4.5,1.5,5.5,angle=45,col=gray(subj_coeffs_log_scaled[2]),border='darkred')
      rect(1.5,4.5,2.5,5.5,angle=45,col=gray(subj_coeffs_log_scaled[3]),border=NA)
      rect(2.5,4.5,3.5,5.5,angle=45,col=gray(subj_coeffs_log_scaled[4]),border=NA)
      rect(3.5,4.5,4.5,5.5,angle=45,col=gray(subj_coeffs_log_scaled[5]),border=NA)
      rect(4.5,4.5,5.5,5.5,angle=45,col=gray(subj_coeffs_log_scaled[6]),border=NA)
      rect(.5,3.5,1.5,4.5,angle=45,col=gray(subj_coeffs_log_scaled[8]),border=NA)
      rect(1.5,3.5,2.5,4.5,angle=45,col=gray(subj_coeffs_log_scaled[9]),border='darkred')
      rect(2.5,3.5,3.5,4.5,angle=45,col=gray(subj_coeffs_log_scaled[10]),border=NA)
      rect(3.5,3.5,4.5,4.5,angle=45,col=gray(subj_coeffs_log_scaled[11]),border=NA)
      rect(4.5,3.5,5.5,4.5,angle=45,col=gray(subj_coeffs_log_scaled[12]),border=NA)
      rect(.5,2.5,1.5,3.5,angle=45,col=gray(subj_coeffs_log_scaled[14]),border=NA)
      rect(1.5,2.5,2.5,3.5,angle=45,col=gray(subj_coeffs_log_scaled[15]),border=NA)
      rect(2.5,2.5,3.5,3.5,angle=45,col=gray(subj_coeffs_log_scaled[16]),border='darkred')
      rect(3.5,2.5,4.5,3.5,angle=45,col=gray(subj_coeffs_log_scaled[17]),border=NA)
      rect(4.5,2.5,5.5,3.5,angle=45,col=gray(subj_coeffs_log_scaled[18]),border=NA)
      rect(.5,1.5,1.5,2.5,angle=45,col=gray(subj_coeffs_log_scaled[20]),border=NA)
      rect(1.5,1.5,2.5,2.5,angle=45,col=gray(subj_coeffs_log_scaled[21]),border=NA)
      rect(2.5,1.5,3.5,2.5,angle=45,col=gray(subj_coeffs_log_scaled[22]),border=NA)
      rect(3.5,1.5,4.5,2.5,angle=45,col=gray(subj_coeffs_log_scaled[23]),border='darkred')
      rect(4.5,1.5,5.5,2.5,angle=45,col=gray(subj_coeffs_log_scaled[24]),border=NA)
      rect(.5,.5,1.5,1.5,angle=45,col=gray(subj_coeffs_log_scaled[26]),border=NA)
      rect(1.5,.5,2.5,1.5,angle=45,col=gray(subj_coeffs_log_scaled[27]),border=NA)
      rect(2.5,.5,3.5,1.5,angle=45,col=gray(subj_coeffs_log_scaled[28]),border=NA)
      rect(3.5,.5,4.5,1.5,angle=45,col=gray(subj_coeffs_log_scaled[29]),border=NA)
      rect(4.5,.5,5.5,1.5,angle=45,col=gray(subj_coeffs_log_scaled[30]),border='darkred')
    }
  }
}

#### plot figure 5B 3 ways ####
# note: figure 5B can be made three different ways-averaging individual coefficients, 
# running a fixed effects model, or running a mixed effects multilevel model. 
# The latter is the most appropriate statistically but also the most complicated

#fig 5B for everyone using averaged (median due to outliers) individual coefficients
require(matrixStats)
median_all_choice_x_reward=colMedians(all_choice_x_reward[,2:26],na.rm=TRUE)
range_coeffs=max(median_all_choice_x_reward)-min(median_all_choice_x_reward)
med_coeffs_log_scaled_pre=log((median_all_choice_x_reward+abs(min(median_all_choice_x_reward))+range_coeffs+.01)/range_coeffs) #normalize to be between 0 & 1 after log transformation
med_coeffs_log_scaled=med_coeffs_log_scaled_pre+(.9-max(med_coeffs_log_scaled_pre))

plot(5:1,5:1,xlab='',ylab='',col='white',xlim=c(.5,5.5),ylim=c(.5,5.5),bty='n',axes=FALSE)
axis(2,at=c(1,2,3,4,5),labels=c('t-5','t-4','t-3','t-2','t-1'),lty=0) 
axis(3,at=c(1,2,3,4,5),labels=c('t-1','t-2','t-3','t-4','t-5'),lty=0) 
mtext('reward',side=3,line=2)
mtext('choice',side=2,line=2)
mtext('All subjects: median of individual coefficients',side=3,line=3,font=2)

rect(.5,4.5,1.5,5.5,angle=45,col=gray(med_coeffs_log_scaled[1]),border='darkred')
rect(1.5,4.5,2.5,5.5,angle=45,col=gray(med_coeffs_log_scaled[2]),border=NA)
rect(2.5,4.5,3.5,5.5,angle=45,col=gray(med_coeffs_log_scaled[3]),border=NA)
rect(3.5,4.5,4.5,5.5,angle=45,col=gray(med_coeffs_log_scaled[4]),border=NA)
rect(4.5,4.5,5.5,5.5,angle=45,col=gray(med_coeffs_log_scaled[5]),border=NA)
rect(.5,3.5,1.5,4.5,angle=45,col=gray(med_coeffs_log_scaled[6]),border=NA)
rect(1.5,3.5,2.5,4.5,angle=45,col=gray(med_coeffs_log_scaled[7]),border='darkred')
rect(2.5,3.5,3.5,4.5,angle=45,col=gray(med_coeffs_log_scaled[8]),border=NA)
rect(3.5,3.5,4.5,4.5,angle=45,col=gray(med_coeffs_log_scaled[9]),border=NA)
rect(4.5,3.5,5.5,4.5,angle=45,col=gray(med_coeffs_log_scaled[10]),border=NA)
rect(.5,2.5,1.5,3.5,angle=45,col=gray(med_coeffs_log_scaled[11]),border=NA)
rect(1.5,2.5,2.5,3.5,angle=45,col=gray(med_coeffs_log_scaled[12]),border=NA)
rect(2.5,2.5,3.5,3.5,angle=45,col=gray(med_coeffs_log_scaled[13]),border='darkred')
rect(3.5,2.5,4.5,3.5,angle=45,col=gray(med_coeffs_log_scaled[14]),border=NA)
rect(4.5,2.5,5.5,3.5,angle=45,col=gray(med_coeffs_log_scaled[15]),border=NA)
rect(.5,1.5,1.5,2.5,angle=45,col=gray(med_coeffs_log_scaled[16]),border=NA)
rect(1.5,1.5,2.5,2.5,angle=45,col=gray(med_coeffs_log_scaled[17]),border=NA)
rect(2.5,1.5,3.5,2.5,angle=45,col=gray(med_coeffs_log_scaled[18]),border=NA)
rect(3.5,1.5,4.5,2.5,angle=45,col=gray(med_coeffs_log_scaled[19]),border='darkred')
rect(4.5,1.5,5.5,2.5,angle=45,col=gray(med_coeffs_log_scaled[20]),border=NA)
rect(.5,.5,1.5,1.5,angle=45,col=gray(med_coeffs_log_scaled[21]),border=NA)
rect(1.5,.5,2.5,1.5,angle=45,col=gray(med_coeffs_log_scaled[22]),border=NA)
rect(2.5,.5,3.5,1.5,angle=45,col=gray(med_coeffs_log_scaled[23]),border=NA)
rect(3.5,.5,4.5,1.5,angle=45,col=gray(med_coeffs_log_scaled[24]),border=NA)
rect(4.5,.5,5.5,1.5,angle=45,col=gray(med_coeffs_log_scaled[25]),border='darkred')

#fig 5B for everyone using a fixed effects model
A_reg_all_fixed=glm(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+
                        A2_rew1+A2_rew2+A2_rew3+A2_rew4+A2_rew5+A2_rew6+
                        A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+A3_rew6+
                        A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+
                        A5_rew1+A5_rew2+A5_rew3+A5_rew4+A5_rew5+A5_rew6+
                        A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+A6_rew6,
                      data=all_subj_data,family=binomial)
B_reg_all_fixed=glm(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+
                  B2_rew1+B2_rew2+B2_rew3+B2_rew4+B2_rew5+B2_rew6+
                  B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+B3_rew6+
                  B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+
                  B5_rew1+B5_rew2+B5_rew3+B5_rew4+B5_rew5+B5_rew6+
                  B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+B6_rew6,
                data=all_subj_data,family=binomial)
C_reg_all_fixed=glm(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+
                  C2_rew1+C2_rew2+C2_rew3+C2_rew4+C2_rew5+C2_rew6+
                  C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+C3_rew6+
                  C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+
                  C5_rew1+C5_rew2+C5_rew3+C5_rew4+C5_rew5+C5_rew6+
                  C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+C6_rew6,
                data=all_subj_data,family=binomial)

A_diag_all_fixed=diag(vcov(A_reg_all_fixed))
B_diag_all_fixed=diag(vcov(B_reg_all_fixed))
C_diag_all_fixed=diag(vcov(C_reg_all_fixed))
all_coeffs_fixed=(1/rowSums(cbind(1/A_diag_all_fixed,1/B_diag_all_fixed,1/C_diag_all_fixed),na.rm=TRUE))*
  (rowSums(cbind(A_reg_all_fixed$coefficients/A_diag_all_fixed,B_reg_all_fixed$coefficients/B_diag_all_fixed,
                 C_reg_all_fixed$coefficients/C_diag_all_fixed),na.rm=TRUE))

range_coeffs_fixed=max(all_coeffs_fixed[2:31])-min(all_coeffs_fixed[2:31])
coeffs_fixed_log_scaled_pre=log((all_coeffs_fixed+abs(min(all_coeffs_fixed[2:31]))+range_coeffs_fixed+.01)/range_coeffs_fixed) #normalize to be between 0 & 1 after log transformation
coeffs_fixed_log_scaled=coeffs_fixed_log_scaled_pre+(.9-max(coeffs_fixed_log_scaled_pre))

plot(5:1,5:1,xlab='',ylab='',col='white',xlim=c(.5,5.5),ylim=c(.5,5.5),bty='n',axes=FALSE)
axis(2,at=c(1,2,3,4,5),labels=c('t-5','t-4','t-3','t-2','t-1'),lty=0) 
axis(3,at=c(1,2,3,4,5),labels=c('t-1','t-2','t-3','t-4','t-5'),lty=0) 
mtext('reward',side=3,line=2)
mtext('choice',side=2,line=2)
mtext('All subjects: fixed effects',side=3,line=3,font=2)
rect(.5,4.5,1.5,5.5,angle=45,col=gray(coeffs_fixed_log_scaled[2]),border='darkred')
rect(1.5,4.5,2.5,5.5,angle=45,col=gray(coeffs_fixed_log_scaled[3]),border=NA)
rect(2.5,4.5,3.5,5.5,angle=45,col=gray(coeffs_fixed_log_scaled[4]),border=NA)
rect(3.5,4.5,4.5,5.5,angle=45,col=gray(coeffs_fixed_log_scaled[5]),border=NA)
rect(4.5,4.5,5.5,5.5,angle=45,col=gray(coeffs_fixed_log_scaled[6]),border=NA)
rect(.5,3.5,1.5,4.5,angle=45,col=gray(coeffs_fixed_log_scaled[8]),border=NA)
rect(1.5,3.5,2.5,4.5,angle=45,col=gray(coeffs_fixed_log_scaled[9]),border='darkred')
rect(2.5,3.5,3.5,4.5,angle=45,col=gray(coeffs_fixed_log_scaled[10]),border=NA)
rect(3.5,3.5,4.5,4.5,angle=45,col=gray(coeffs_fixed_log_scaled[11]),border=NA)
rect(4.5,3.5,5.5,4.5,angle=45,col=gray(coeffs_fixed_log_scaled[12]),border=NA)
rect(.5,2.5,1.5,3.5,angle=45,col=gray(coeffs_fixed_log_scaled[14]),border=NA)
rect(1.5,2.5,2.5,3.5,angle=45,col=gray(coeffs_fixed_log_scaled[15]),border=NA)
rect(2.5,2.5,3.5,3.5,angle=45,col=gray(coeffs_fixed_log_scaled[16]),border='darkred')
rect(3.5,2.5,4.5,3.5,angle=45,col=gray(coeffs_fixed_log_scaled[17]),border=NA)
rect(4.5,2.5,5.5,3.5,angle=45,col=gray(coeffs_fixed_log_scaled[18]),border=NA)
rect(.5,1.5,1.5,2.5,angle=45,col=gray(coeffs_fixed_log_scaled[20]),border=NA)
rect(1.5,1.5,2.5,2.5,angle=45,col=gray(coeffs_fixed_log_scaled[21]),border=NA)
rect(2.5,1.5,3.5,2.5,angle=45,col=gray(coeffs_fixed_log_scaled[22]),border=NA)
rect(3.5,1.5,4.5,2.5,angle=45,col=gray(coeffs_fixed_log_scaled[23]),border='darkred')
rect(4.5,1.5,5.5,2.5,angle=45,col=gray(coeffs_fixed_log_scaled[24]),border=NA)
rect(.5,.5,1.5,1.5,angle=45,col=gray(coeffs_fixed_log_scaled[26]),border=NA)
rect(1.5,.5,2.5,1.5,angle=45,col=gray(coeffs_fixed_log_scaled[27]),border=NA)
rect(2.5,.5,3.5,1.5,angle=45,col=gray(coeffs_fixed_log_scaled[28]),border=NA)
rect(3.5,.5,4.5,1.5,angle=45,col=gray(coeffs_fixed_log_scaled[29]),border=NA)
rect(4.5,.5,5.5,1.5,angle=45,col=gray(coeffs_fixed_log_scaled[30]),border='darkred')

#fig 5B for everyone using multilevel regression- note that this model assumes only the intercept
# varies among people, see below for more complex models that allow previous choice*reward interactions
# to also vary
require(lme4)
#run regressions
A_reg_all=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                  A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                  A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                  A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                  A6_rew6+(1|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                  B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                  B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                  B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                  B6_rew6+(1|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                  C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                  C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                  C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                  C6_rew6+(1|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))

A_diag_all=diag(vcov(A_reg_all))
B_diag_all=diag(vcov(B_reg_all))
C_diag_all=diag(vcov(C_reg_all))
all_coeffs_combined=(1/rowSums(cbind(1/A_diag_all,1/B_diag_all,1/C_diag_all),na.rm=TRUE))*(rowSums(cbind(fixef(A_reg_all)/A_diag_all,fixef(B_reg_all)/B_diag_all,fixef(C_reg_all)/C_diag_all),na.rm=TRUE))

#plot
range_coeffs_combined=max(all_coeffs_combined[2:31])-min(all_coeffs_combined[2:31])
coeffs_combined_log_scaled_pre=log((all_coeffs_combined+abs(min(all_coeffs_combined[2:31]))+range_coeffs_combined+.01)/range_coeffs_combined) #normalize to be between 0 & 1 after log transformation
coeffs_combined_log_scaled=coeffs_combined_log_scaled_pre+(.9-max(coeffs_combined_log_scaled_pre))

plot(5:1,5:1,xlab='',ylab='',col='white',xlim=c(.5,5.5),ylim=c(.5,5.5),bty='n',axes=FALSE)
axis(2,at=c(1,2,3,4,5),labels=c('t-5','t-4','t-3','t-2','t-1'),lty=0)
axis(3,at=c(1,2,3,4,5),labels=c('t-1','t-2','t-3','t-4','t-5'),lty=0)
mtext('reward',side=3,line=2)
mtext('choice',side=2,line=2)
mtext('all subjects: mixed effects',side=3,line=3,font=2)
rect(.5,4.5,1.5,5.5,angle=45,col=gray(coeffs_combined_log_scaled[2]),border='darkred')
rect(1.5,4.5,2.5,5.5,angle=45,col=gray(coeffs_combined_log_scaled[3]),border=NA)
rect(2.5,4.5,3.5,5.5,angle=45,col=gray(coeffs_combined_log_scaled[4]),border=NA)
rect(3.5,4.5,4.5,5.5,angle=45,col=gray(coeffs_combined_log_scaled[5]),border=NA)
rect(4.5,4.5,5.5,5.5,angle=45,col=gray(coeffs_combined_log_scaled[6]),border=NA)
rect(.5,3.5,1.5,4.5,angle=45,col=gray(coeffs_combined_log_scaled[8]),border=NA)
rect(1.5,3.5,2.5,4.5,angle=45,col=gray(coeffs_combined_log_scaled[9]),border='darkred')
rect(2.5,3.5,3.5,4.5,angle=45,col=gray(coeffs_combined_log_scaled[10]),border=NA)
rect(3.5,3.5,4.5,4.5,angle=45,col=gray(coeffs_combined_log_scaled[11]),border=NA)
rect(4.5,3.5,5.5,4.5,angle=45,col=gray(coeffs_combined_log_scaled[12]),border=NA)
rect(.5,2.5,1.5,3.5,angle=45,col=gray(coeffs_combined_log_scaled[14]),border=NA)
rect(1.5,2.5,2.5,3.5,angle=45,col=gray(coeffs_combined_log_scaled[15]),border=NA)
rect(2.5,2.5,3.5,3.5,angle=45,col=gray(coeffs_combined_log_scaled[16]),border='darkred')
rect(3.5,2.5,4.5,3.5,angle=45,col=gray(coeffs_combined_log_scaled[17]),border=NA)
rect(4.5,2.5,5.5,3.5,angle=45,col=gray(coeffs_combined_log_scaled[18]),border=NA)
rect(.5,1.5,1.5,2.5,angle=45,col=gray(coeffs_combined_log_scaled[20]),border=NA)
rect(1.5,1.5,2.5,2.5,angle=45,col=gray(coeffs_combined_log_scaled[21]),border=NA)
rect(2.5,1.5,3.5,2.5,angle=45,col=gray(coeffs_combined_log_scaled[22]),border=NA)
rect(3.5,1.5,4.5,2.5,angle=45,col=gray(coeffs_combined_log_scaled[23]),border='darkred')
rect(4.5,1.5,5.5,2.5,angle=45,col=gray(coeffs_combined_log_scaled[24]),border=NA)
rect(.5,.5,1.5,1.5,angle=45,col=gray(coeffs_combined_log_scaled[26]),border=NA)
rect(1.5,.5,2.5,1.5,angle=45,col=gray(coeffs_combined_log_scaled[27]),border=NA)
rect(2.5,.5,3.5,1.5,angle=45,col=gray(coeffs_combined_log_scaled[28]),border=NA)
rect(3.5,.5,4.5,1.5,angle=45,col=gray(coeffs_combined_log_scaled[29]),border=NA)
rect(4.5,.5,5.5,1.5,angle=45,col=gray(coeffs_combined_log_scaled[30]),border='darkred')


#### plot figures 5C-E two ways ####

plot(1:5,1:5,col='white',axes=F,bty='n',xlab='',ylab='') #blank fig to start following on new page

#make figs 5C-E using individual-level data points- large points are the median
plot(1:5,choice_x_reward[1,2:6],ylim=c(-10,10),pch='.',xlab='trials in the past',ylab='choice x reward weight',main='figure 5C')
for (subj in 2:length(subj_ids)) {
  points(1:5,choice_x_reward[subj,2:6],pch='.')
}
points(1:5,c(median(choice_x_reward[,2],na.rm=TRUE),median(choice_x_reward[,3],na.rm=TRUE),
             median(choice_x_reward[,4],na.rm=TRUE),median(choice_x_reward[,5],na.rm=TRUE),
             median(choice_x_reward[,6],na.rm=TRUE)),pch=19)
lines(1:5,c(median(choice_x_reward[,2],na.rm=TRUE),median(choice_x_reward[,3],na.rm=TRUE),
             median(choice_x_reward[,4],na.rm=TRUE),median(choice_x_reward[,5],na.rm=TRUE),
             median(choice_x_reward[,6],na.rm=TRUE)))
abline(h=0,col='gray20',lty=2)

plot(1:5,choice_x_past_reward[1,2:6],ylim=c(-10,10),pch='.',xlab='trials in the past',ylab='past choices x immediately previous reward weight',main='figure 5D')
for (subj in 2:length(subj_ids)) {
  points(1:5,choice_x_past_reward[subj,2:6],pch='.')
}
points(1:5,c(median(choice_x_past_reward[,2],na.rm=TRUE),median(choice_x_past_reward[,3],na.rm=TRUE),
             median(choice_x_past_reward[,4],na.rm=TRUE),median(choice_x_past_reward[,5],na.rm=TRUE),
             median(choice_x_past_reward[,6],na.rm=TRUE)),pch=19)
lines(2:5,c(median(choice_x_past_reward[,3],na.rm=TRUE),
            median(choice_x_past_reward[,4],na.rm=TRUE),median(choice_x_past_reward[,5],na.rm=TRUE),
            median(choice_x_past_reward[,6],na.rm=TRUE)))
abline(h=0,col='gray20',lty=2)

plot(1:5,past_choice_x_reward[1,2:6],ylim=c(-10,10),pch='.',xlab='trials in the past',ylab='immediately previous choice x past rewards weight',main='figure 5E')
for (subj in 2:length(subj_ids)) {
  points(1:5,past_choice_x_reward[subj,2:6],pch='.')
}
points(1:5,c(median(past_choice_x_reward[,2],na.rm=TRUE),median(past_choice_x_reward[,3],na.rm=TRUE),
             median(past_choice_x_reward[,4],na.rm=TRUE),median(past_choice_x_reward[,5],na.rm=TRUE),
             median(past_choice_x_reward[,6],na.rm=TRUE)),pch=19)
lines(2:5,c(median(past_choice_x_reward[,3],na.rm=TRUE),
            median(past_choice_x_reward[,4],na.rm=TRUE),median(past_choice_x_reward[,5],na.rm=TRUE),
            median(past_choice_x_reward[,6],na.rm=TRUE)))
abline(h=0,col='gray20',lty=2)

#make figs 5C-E using data from multilevel model- but again note that this is not allowing these variables
# to vary by subject, only uses the fixed effects SE
plot(1:5,1:5,col='white',axes=F,bty='n',xlab='',ylab='') #blank fig to start on new page
A_reg_SE=sqrt(A_diag_all)
A_reg_coeffs=fixef(A_reg_all) 

plot(1:5,A_reg_coeffs[seq(2,35,7)],ylim=c(0,2),pch=16,xlab='trials in the past',ylab='choice x reward weight',main='figure 5C')
lines(1:5,A_reg_coeffs[seq(2,35,7)])
points(1:5,(A_reg_coeffs[seq(2,35,7)]+A_reg_SE[seq(2,35,7)]),pch=6) # arrows approximate standard error bars
points(1:5,(A_reg_coeffs[seq(2,35,7)]-A_reg_SE[seq(2,35,7)]),pch=2)
abline(h=0,col='gray20',lty=2)

plot(1:5,A_reg_coeffs[seq(2,31,6)],ylim=c(0,2),pch=16,xlab='trials in the past',ylab='past choices x immediately previous reward weight',main='figure 5D')
lines(1:5,A_reg_coeffs[seq(2,31,6)])
points(1:5,(A_reg_coeffs[seq(2,31,6)]+A_reg_SE[seq(2,31,6)]),pch=6)
points(1:5,(A_reg_coeffs[seq(2,31,6)]-A_reg_SE[seq(2,31,6)]),pch=2)
abline(h=0,col='gray20',lty=2)

plot(1:5,A_reg_coeffs[2:6],ylim=c(0,2),pch=16,xlab='trials in the past',ylab='immediately previous choice x past rewards weight',main='figure 5E')
lines(1:5,A_reg_coeffs[2:6])
points(1:5,(A_reg_coeffs[2:6]+A_reg_SE[2:6]),pch=6)
points(1:5,(A_reg_coeffs[2:6]-A_reg_SE[2:6]),pch=2)
abline(h=0,col='gray20',lty=2)


#### fig 5B allowing for random effects in reward*choice interactions ####

#try different combinations of random effects, corresponding to diagonal/vertical/horizonal
# parts of fig 5B as well as combinations and test model fit- that is, does allowing effect of certain
# reward*choice interactions to vary by subject improve model fit?
# note: these take a long time to run
AIC_basic=AIC(A_reg_all)+AIC(B_reg_all)+AIC(C_reg_all)
A_reg_all_prevrew=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                  A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                  A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                  A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                  A6_rew6+(A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_prevrew=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                  B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                  B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                  B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                  B6_rew6+(B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_prevrew=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                  C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                  C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                  C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                  C6_rew6+(C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5|subjID),data=all_subj_data,family=binomial,
                glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_prevrew=AIC(A_reg_all_prevrew)+AIC(B_reg_all_prevrew)+AIC(C_reg_all_prevrew)
AIC_basic-AIC_prevrew

A_reg_all_last=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                          A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                          A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                          A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                          A6_rew6+(A1_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_last=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                          B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                          B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                          B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                          B6_rew6+(B1_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_last=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                          C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                          C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                          C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                          C6_rew6+(C1_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_last=AIC(A_reg_all_last)+AIC(B_reg_all_last)+AIC(C_reg_all_last)
AIC_basic-AIC_last

A_reg_all_prevchoice=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                          A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                          A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                          A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                          A6_rew6+(A1_rew1+A2_rew1+A3_rew1+A4_rew1+A5_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_prevchoice=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                          B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                          B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                          B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                          B6_rew6+(B1_rew1+B2_rew1+B3_rew1+B4_rew1+B5_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_prevchoice=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                          C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                          C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                          C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                          C6_rew6+(C1_rew1+C2_rew1+C3_rew1+C4_rew1+C5_rew1|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_prevchoice=AIC(A_reg_all_prevchoice)+AIC(B_reg_all_prevchoice)+AIC(C_reg_all_prevchoice)
AIC_basic-AIC_prevchoice

A_reg_all_prevint=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                             A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                             A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                             A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                             A6_rew6+(A1_rew1+A2_rew2+A3_rew3+A4_rew4+A5_rew5|subjID),data=all_subj_data,family=binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_prevint=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                             B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                             B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                             B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                             B6_rew6+(B1_rew1+B2_rew2+B3_rew3+B4_rew4+B5_rew5|subjID),data=all_subj_data,family=binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_prevint=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                             C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                             C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                             C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                             C6_rew6+(C1_rew1+C2_rew2+C3_rew3+C4_rew4+C5_rew5|subjID),data=all_subj_data,family=binomial,
                           glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_prevint=AIC(A_reg_all_prevint)+AIC(B_reg_all_prevint)+AIC(C_reg_all_prevint)
AIC_basic-AIC_prevint

A_reg_all_prev1=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                          A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                          A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                          A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                          A6_rew6+(A1_rew1+A2_rew2+A1_rew2+A2_rew1+A3_rew3|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_prev1=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                          B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                          B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                          B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                          B6_rew6+(B1_rew1+B2_rew2+B1_rew2+B2_rew1+B3_rew3|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_prev1=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                          C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                          C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                          C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                          C6_rew6+(C1_rew1+C2_rew2+C1_rew2+C2_rew1+C3_rew3|subjID),data=all_subj_data,family=binomial,
                        glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_prev1=AIC(A_reg_all_prev1)+AIC(B_reg_all_prev1)+AIC(C_reg_all_prev1)
AIC_basic-AIC_prev1

A_reg_all_prev2=glmer(a_choice ~ A1_rew1+A1_rew2+A1_rew3+A1_rew4+A1_rew5+A1_rew6+A2_rew1+A2_rew2+
                        A2_rew3+A2_rew4+A2_rew5+A2_rew6+A3_rew1+A3_rew2+A3_rew3+A3_rew4+A3_rew5+
                        A3_rew6+A4_rew1+A4_rew2+A4_rew3+A4_rew4+A4_rew5+A4_rew6+A5_rew1+A5_rew2+
                        A5_rew3+A5_rew4+A5_rew5+A5_rew6+A6_rew1+A6_rew2+A6_rew3+A6_rew4+A6_rew5+
                        A6_rew6+(A1_rew1+A2_rew2+A1_rew2+A2_rew1+A3_rew3+A1_rew3+A2_rew3+A3_rew1+
                                   A3_rew2+A4_rew4|subjID),data=all_subj_data,family=binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
B_reg_all_prev2=glmer(b_choice ~ B1_rew1+B1_rew2+B1_rew3+B1_rew4+B1_rew5+B1_rew6+B2_rew1+B2_rew2+
                        B2_rew3+B2_rew4+B2_rew5+B2_rew6+B3_rew1+B3_rew2+B3_rew3+B3_rew4+B3_rew5+
                        B3_rew6+B4_rew1+B4_rew2+B4_rew3+B4_rew4+B4_rew5+B4_rew6+B5_rew1+B5_rew2+
                        B5_rew3+B5_rew4+B5_rew5+B5_rew6+B6_rew1+B6_rew2+B6_rew3+B6_rew4+B6_rew5+
                        B6_rew6+(B1_rew1+B2_rew2+B1_rew2+B2_rew1+B3_rew3+B1_rew3+B2_rew3+B3_rew1+
                                   B3_rew2+B4_rew4|subjID),data=all_subj_data,family=binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
C_reg_all_prev2=glmer(c_choice ~ C1_rew1+C1_rew2+C1_rew3+C1_rew4+C1_rew5+C1_rew6+C2_rew1+C2_rew2+
                        C2_rew3+C2_rew4+C2_rew5+C2_rew6+C3_rew1+C3_rew2+C3_rew3+C3_rew4+C3_rew5+
                        C3_rew6+C4_rew1+C4_rew2+C4_rew3+C4_rew4+C4_rew5+C4_rew6+C5_rew1+C5_rew2+
                        C5_rew3+C5_rew4+C5_rew5+C5_rew6+C6_rew1+C6_rew2+C6_rew3+C6_rew4+C6_rew5+
                        C6_rew6+(C1_rew1+C2_rew2+C1_rew2+C2_rew1+C3_rew3+C1_rew3+C2_rew3+C3_rew1+
                                   C3_rew2+C4_rew4|subjID),data=all_subj_data,family=binomial,
                      glmerControl(optimizer="bobyqa", optCtrl = list(maxfun = 100000)))
AIC_prev2=AIC(A_reg_all_prev2)+AIC(B_reg_all_prev2)+AIC(C_reg_all_prev2)
AIC_basic-AIC_prev2

# plot model fits and note best fitting model (lowest AIC)
par(mfrow=c(1,1))
barplot(c(AIC_basic,AIC_last,AIC_prevint,AIC_prevchoice,AIC_prevrew,AIC_prev1,AIC_prev2),ylab='AIC',
        ylim=c(.9*min(AIC_basic,AIC_last,AIC_prevint,AIC_prevchoice,AIC_prevrew,AIC_prev1,AIC_prev2),
               1.1*max(AIC_basic,AIC_last,AIC_prevint,AIC_prevchoice,AIC_prevrew,AIC_prev1,AIC_prev2)),
        names.arg=c('basic','last','prev int','prev choices','prev rewards','prev mix 2 back','prev mix 3 back'),
        beside=T,xpd=F,border=NA)
abline(h=min(AIC_basic,AIC_last,AIC_prevint,AIC_prevchoice,AIC_prevrew,AIC_prev1,AIC_prev2))

#### plot beta coefficients for variables in the best fitting model, including estimates of individual subjects ####
#pull out estimates for each person (random + fixed) and overall estimates (fixed effects)
allfx_A=cbind(ranef(A_reg_all_prev1)$subjID[,2]+fixef(A_reg_all_prev1)[2],
              ranef(A_reg_all_prev1)$subjID[,4]+fixef(A_reg_all_prev1)[3],
              ranef(A_reg_all_prev1)$subjID[,3]+fixef(A_reg_all_prev1)[9],
              ranef(A_reg_all_prev1)$subjID[,5]+fixef(A_reg_all_prev1)[8],
              ranef(A_reg_all_prev1)$subjID[,6]+fixef(A_reg_all_prev1)[16])
allfx_B=cbind(ranef(B_reg_all_prev1)$subjID[,2]+fixef(B_reg_all_prev1)[2],
              ranef(B_reg_all_prev1)$subjID[,4]+fixef(B_reg_all_prev1)[3],
              ranef(B_reg_all_prev1)$subjID[,3]+fixef(B_reg_all_prev1)[9],
              ranef(B_reg_all_prev1)$subjID[,5]+fixef(B_reg_all_prev1)[8],
              ranef(B_reg_all_prev1)$subjID[,6]+fixef(B_reg_all_prev1)[16])
allfx_C=cbind(ranef(C_reg_all_prev1)$subjID[,2]+fixef(C_reg_all_prev1)[2],
              ranef(C_reg_all_prev1)$subjID[,4]+fixef(C_reg_all_prev1)[3],
              ranef(C_reg_all_prev1)$subjID[,3]+fixef(C_reg_all_prev1)[9],
              ranef(C_reg_all_prev1)$subjID[,5]+fixef(C_reg_all_prev1)[8],
              ranef(C_reg_all_prev1)$subjID[,6]+fixef(C_reg_all_prev1)[16])
fix_A1_rew1=mean(fixef(A_reg_all_prev1)[2],fixef(B_reg_all_prev1)[2],fixef(C_reg_all_prev1)[2])
fix_A1_rew2=mean(fixef(A_reg_all_prev1)[3],fixef(B_reg_all_prev1)[3],fixef(C_reg_all_prev1)[3])
fix_A2_rew2=mean(fixef(A_reg_all_prev1)[9],fixef(B_reg_all_prev1)[9],fixef(C_reg_all_prev1)[9])
fix_A2_rew1=mean(fixef(A_reg_all_prev1)[8],fixef(B_reg_all_prev1)[8],fixef(C_reg_all_prev1)[8])
fix_A3_rew3=mean(fixef(A_reg_all_prev1)[16],fixef(B_reg_all_prev1)[16],fixef(C_reg_all_prev1)[16])
mean_A1_rew1=rowMeans(cbind(allfx_A[,1],allfx_B[,1],allfx_C[,1]))
mean_A1_rew2=rowMeans(cbind(allfx_A[,2],allfx_B[,2],allfx_C[,2]))
mean_A2_rew2=rowMeans(cbind(allfx_A[,3],allfx_B[,3],allfx_C[,3]))
mean_A2_rew1=rowMeans(cbind(allfx_A[,4],allfx_B[,4],allfx_C[,4]))
mean_A3_rew3=rowMeans(cbind(allfx_A[,5],allfx_B[,5],allfx_C[,5]))

#plot- black points are estimates for each subject, filled white circle is fixed effect (overall mean)
par(mfrow=c(1,1))
max_y=max(mean_A1_rew1,mean_A1_rew2,mean_A2_rew2,mean_A2_rew1,mean_A3_rew3)
min_y=min(mean_A1_rew1,mean_A1_rew2,mean_A2_rew2,mean_A2_rew1,mean_A3_rew3)
plot(1:5,c(mean_A1_rew1[1],mean_A2_rew2[1],mean_A3_rew3[1],mean_A1_rew2[1],mean_A2_rew1[1]),
     xlab='',pch=20,ylab='estimated beta',ylim=c(floor(min_y),ceiling(max_y)),axes=F)
axis(1,at=c(1,2,3,4,5),labels=c('choice*rew_1','choice*rew_2','choice*rew_3','choice_1*rew_2','choice_2*rew_1'))
axis(2,at=seq(floor(min_y),ceiling(max_y),1))
for (subj in 2:length(mean_A1_rew1)) {
  points(1:5,c(mean_A1_rew1[subj],mean_A2_rew2[subj],mean_A3_rew3[subj],mean_A1_rew2[subj],mean_A2_rew1[subj]),pch=20)
}
points(1:5,c(fix_A1_rew1,fix_A2_rew2,fix_A3_rew3,fix_A1_rew2,fix_A2_rew1),pch=21,bg='white')
abline(h=0,lty=2,col='gray20')
