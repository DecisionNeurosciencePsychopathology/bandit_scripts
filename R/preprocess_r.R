###########
#preprocesses behavioral data
#note: csv files are written from .mat files using save_behav_csv.m
##########

#get subject names from directory
data_dir='/Users/brownv/Documents/dnpl/bandit_scripts/subjects/csvs/'
subj_list=list.files(paste0(data_dir,'reshaped/'),recursive=FALSE)
all_subj_data=matrix(data=NA,nrow=270*length(subj_list),ncol=160)

#loop through subjs
for (subj in 1:length(subj_list)) {
  
  #### prep data ####
  
  #pull out subj number & print
  curr_subj=substr(subj_list[subj],1,nchar(subj_list[subj])-13)
  print(paste0('processing subject ',curr_subj))
  
  #read in subj data
  subj_data=read.csv(paste0(data_dir,'reshaped/',subj_list[subj]),header=FALSE,sep=',')
  names(subj_data)=c('subjID','all_errors_prob_switch','all_errors_spont_switch',
  'all_errors_erratic_spont','all_errors_explore_switch','all_error_NOS',
  'all_errors_perseverative','before_errors_prob_switch','before_errors_spont_switch',
  'before_errors_erratic_spont','before_errors_explore_switch','before_error_NOS',
  'before_errors_perseverative','after_errors_prob_switch',
  'after_errors_spont_switch','after_errors_erratic_spont',
  'after_errors_explore_switch','after_error_NOS',
  'after_errors_perseverative','RT','ACC','a_choice','b_choice',
  'c_choice','stim_choice','numeric_stim_choice','prob_a','prob_b',
  'prob_c','best_choice','above_chance_diff','delta_index','counts_to_first_C')
  
  #### add switching, choice & reward data for previous trials ####
  
  #define new variables
  subj_data$switch_from_prev=subj_data$switch_1=subj_data$switch_2=subj_data$switch_3=subj_data$switch_4=NA
  subj_data$switch_5=subj_data$trials_since_switch=NA
  subj_data$choice_1=subj_data$choice_2=subj_data$choice_3=subj_data$choice_4=subj_data$choice_5=subj_data$choice_6=NA
  subj_data$rew_1=subj_data$rew_2=subj_data$rew_3=subj_data$rew_4=subj_data$rew_5=subj_data$rew_6=NA
  subj_data$A1_rew1=subj_data$A1_rew2=subj_data$A1_rew3=subj_data$A1_rew4=subj_data$A1_rew5=subj_data$A1_rew6=NA
  subj_data$A2_rew1=subj_data$A2_rew2=subj_data$A2_rew3=subj_data$A2_rew4=subj_data$A2_rew5=subj_data$A2_rew6=NA
  subj_data$A3_rew1=subj_data$A3_rew2=subj_data$A3_rew3=subj_data$A3_rew4=subj_data$A3_rew5=subj_data$A3_rew6=NA
  subj_data$A4_rew1=subj_data$A4_rew2=subj_data$A4_rew3=subj_data$A4_rew4=subj_data$A4_rew5=subj_data$A4_rew6=NA
  subj_data$A5_rew1=subj_data$A5_rew2=subj_data$A5_rew3=subj_data$A5_rew4=subj_data$A5_rew5=subj_data$A5_rew6=NA
  subj_data$A6_rew1=subj_data$A6_rew2=subj_data$A6_rew3=subj_data$A6_rew4=subj_data$A6_rew5=subj_data$A6_rew6=NA
  subj_data$B1_rew1=subj_data$B1_rew2=subj_data$B1_rew3=subj_data$B1_rew4=subj_data$B1_rew5=subj_data$B1_rew6=NA
  subj_data$B2_rew1=subj_data$B2_rew2=subj_data$B2_rew3=subj_data$B2_rew4=subj_data$B2_rew5=subj_data$B2_rew6=NA
  subj_data$B3_rew1=subj_data$B3_rew2=subj_data$B3_rew3=subj_data$B3_rew4=subj_data$B3_rew5=subj_data$B3_rew6=NA
  subj_data$B4_rew1=subj_data$B4_rew2=subj_data$B4_rew3=subj_data$B4_rew4=subj_data$B4_rew5=subj_data$B4_rew6=NA
  subj_data$B5_rew1=subj_data$B5_rew2=subj_data$B5_rew3=subj_data$B5_rew4=subj_data$B5_rew5=subj_data$B5_rew6=NA
  subj_data$B6_rew1=subj_data$B6_rew2=subj_data$B6_rew3=subj_data$B6_rew4=subj_data$B6_rew5=subj_data$B6_rew6=NA
  subj_data$C1_rew1=subj_data$C1_rew2=subj_data$C1_rew3=subj_data$C1_rew4=subj_data$C1_rew5=subj_data$C1_rew6=NA
  subj_data$C2_rew1=subj_data$C2_rew2=subj_data$C2_rew3=subj_data$C2_rew4=subj_data$C2_rew5=subj_data$C2_rew6=NA
  subj_data$C3_rew1=subj_data$C3_rew2=subj_data$C3_rew3=subj_data$C3_rew4=subj_data$C3_rew5=subj_data$C3_rew6=NA
  subj_data$C4_rew1=subj_data$C4_rew2=subj_data$C4_rew3=subj_data$C4_rew4=subj_data$C4_rew5=subj_data$C4_rew6=NA
  subj_data$C5_rew1=subj_data$C5_rew2=subj_data$C5_rew3=subj_data$C5_rew4=subj_data$C5_rew5=subj_data$C5_rew6=NA
  subj_data$C6_rew1=subj_data$C6_rew2=subj_data$C6_rew3=subj_data$C6_rew4=subj_data$C6_rew5=subj_data$C6_rew6=NA
  
  #loop through trials (note that all values for first trial for new variables are NA so start w/trial 2)
  for (t in 2:270) {
    if (subj_data$numeric_stim_choice[t]>0) { #trials w/a response, otherwise keep all as NA
      
      #most recent trial's choices, rewards, switches, & rewards*choices
      if (subj_data$numeric_stim_choice[t-1]!=subj_data$numeric_stim_choice[t]) { #switch
        subj_data$trials_since_switch[t]=0
        subj_data$switch_from_prev[t]=1
      } else {
        subj_data$switch_from_prev[t]=0
        if (is.na(subj_data$trials_since_switch[t-1])==0) { #if switch occurred since last no response trial, otherwise keep as NA
          subj_data$trials_since_switch[t]=subj_data$trials_since_switch[t-1]+1 #count from last switch
        }
      }
      subj_data$choice_1[t]=subj_data$numeric_stim_choice[t-1]
      subj_data$rew_1[t]=subj_data$ACC[t-1]
      
      if (subj_data$numeric_stim_choice[t-1]==1) { #previous choice 'A'
        subj_data$A1_rew1[t]=subj_data$ACC[t-1] #1 if reward, 0 otherwise
        subj_data$B1_rew1[t]=subj_data$C1_rew1[t]=-1*subj_data$ACC[t-1] #-1 if reward, 0 otherwise
        if (t>2) {
          subj_data$A1_rew2[t]=subj_data$ACC[t-2]
          subj_data$B1_rew2[t]=subj_data$C1_rew2[t]=-1*subj_data$ACC[t-2] 
        }
        if (t>3) {
          subj_data$A1_rew3[t]=subj_data$ACC[t-3]
          subj_data$B1_rew3[t]=subj_data$C1_rew3[t]=-1*subj_data$ACC[t-3] 
        }
        if (t>4) {
          subj_data$A1_rew4[t]=subj_data$ACC[t-4]
          subj_data$B1_rew4[t]=subj_data$C1_rew4[t]=-1*subj_data$ACC[t-4] 
        }
        if (t>5) {
          subj_data$A1_rew5[t]=subj_data$ACC[t-5]
          subj_data$B1_rew5[t]=subj_data$C1_rew5[t]=-1*subj_data$ACC[t-5] 
        }
        if (t>6) {
          subj_data$A1_rew6[t]=subj_data$ACC[t-6]
          subj_data$B1_rew6[t]=subj_data$C1_rew6[t]=-1*subj_data$ACC[t-6] 
        }
      } else if (subj_data$numeric_stim_choice[t-1]==2) { #previous choice 'B'
        subj_data$B1_rew1[t]=subj_data$ACC[t-1]
        subj_data$A1_rew1[t]=subj_data$C1_rew1[t]=-1*subj_data$ACC[t-1]
        if (t>2) {
          subj_data$B1_rew2[t]=subj_data$ACC[t-2]
          subj_data$A1_rew2[t]=subj_data$C1_rew2[t]=-1*subj_data$ACC[t-2] 
        }
        if (t>3) {
          subj_data$B1_rew3[t]=subj_data$ACC[t-3]
          subj_data$A1_rew3[t]=subj_data$C1_rew3[t]=-1*subj_data$ACC[t-3] 
        }
        if (t>4) {
          subj_data$B1_rew4[t]=subj_data$ACC[t-4]
          subj_data$A1_rew4[t]=subj_data$C1_rew4[t]=-1*subj_data$ACC[t-4] 
        }
        if (t>5) {
          subj_data$B1_rew5[t]=subj_data$ACC[t-5]
          subj_data$A1_rew5[t]=subj_data$C1_rew5[t]=-1*subj_data$ACC[t-5] 
        }
        if (t>6) {
          subj_data$B1_rew6[t]=subj_data$ACC[t-6]
          subj_data$A1_rew6[t]=subj_data$C1_rew6[t]=-1*subj_data$ACC[t-6] 
        }
      } else if (subj_data$numeric_stim_choice[t-1]==3) { #previous choice 'C'
        subj_data$C1_rew1[t]=subj_data$ACC[t-1]
        subj_data$A1_rew1[t]=subj_data$B1_rew1[t]=-1*subj_data$ACC[t-1]
        if (t>2) {
          subj_data$C1_rew2[t]=subj_data$ACC[t-2]
          subj_data$B1_rew2[t]=subj_data$A1_rew2[t]=-1*subj_data$ACC[t-2] 
        }
        if (t>3) {
          subj_data$C1_rew3[t]=subj_data$ACC[t-3]
          subj_data$B1_rew3[t]=subj_data$A1_rew3[t]=-1*subj_data$ACC[t-3] 
        }
        if (t>4) {
          subj_data$C1_rew4[t]=subj_data$ACC[t-4]
          subj_data$B1_rew4[t]=subj_data$A1_rew4[t]=-1*subj_data$ACC[t-4] 
        }
        if (t>5) {
          subj_data$C1_rew5[t]=subj_data$ACC[t-5]
          subj_data$B1_rew5[t]=subj_data$A1_rew5[t]=-1*subj_data$ACC[t-5] 
        }
        if (t>6) {
          subj_data$C1_rew6[t]=subj_data$ACC[t-6]
          subj_data$B1_rew6[t]=subj_data$A1_rew6[t]=-1*subj_data$ACC[t-6] 
        }
      }
      
      #previous choices, rewards, switches, & rewards*choices, if previous trials exist
      if (t>2) {
        subj_data$choice_2[t]=subj_data$numeric_stim_choice[t-2]
        subj_data$rew_2[t]=subj_data$ACC[t-2]
        subj_data$switch_1[t]=subj_data$switch_from_prev[t-1]
        
        if (subj_data$numeric_stim_choice[t-2]==1) { #2-back choice
          subj_data$A2_rew1[t]=subj_data$ACC[t-1]
          subj_data$A2_rew2[t]=subj_data$ACC[t-2]
          subj_data$B2_rew1[t]=subj_data$C2_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$B2_rew2[t]=subj_data$C2_rew2[t]=-1*subj_data$ACC[t-2]
          if (t>3) {
            subj_data$A2_rew3[t]=subj_data$ACC[t-3]
            subj_data$B2_rew3[t]=subj_data$C2_rew3[t]=-1*subj_data$ACC[t-3]
          }
          if (t>4) {
            subj_data$A2_rew4[t]=subj_data$ACC[t-4]
            subj_data$B2_rew4[t]=subj_data$C2_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$A2_rew5[t]=subj_data$ACC[t-5]
            subj_data$B2_rew5[t]=subj_data$C2_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$A2_rew6[t]=subj_data$ACC[t-6]
            subj_data$B2_rew6[t]=subj_data$C2_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-2]==2) {
          subj_data$B2_rew1[t]=subj_data$ACC[t-1]
          subj_data$B2_rew2[t]=subj_data$ACC[t-2]
          subj_data$A2_rew1[t]=subj_data$C2_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A2_rew2[t]=subj_data$C2_rew2[t]=-1*subj_data$ACC[t-2]
          if (t>3) {
            subj_data$B2_rew3[t]=subj_data$ACC[t-3]
            subj_data$A2_rew3[t]=subj_data$C2_rew3[t]=-1*subj_data$ACC[t-3]
          }
          if (t>4) {
            subj_data$B2_rew4[t]=subj_data$ACC[t-4]
            subj_data$A2_rew4[t]=subj_data$C2_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$B2_rew5[t]=subj_data$ACC[t-5]
            subj_data$A2_rew5[t]=subj_data$C2_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$B2_rew6[t]=subj_data$ACC[t-6]
            subj_data$A2_rew6[t]=subj_data$C2_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-2]==3) {
          subj_data$C2_rew1[t]=subj_data$ACC[t-1]
          subj_data$C2_rew2[t]=subj_data$ACC[t-2]
          subj_data$A2_rew1[t]=subj_data$B2_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A2_rew2[t]=subj_data$B2_rew2[t]=-1*subj_data$ACC[t-2]
          if (t>3) {
            subj_data$C2_rew3[t]=subj_data$ACC[t-3]
            subj_data$B2_rew3[t]=subj_data$A2_rew3[t]=-1*subj_data$ACC[t-3]
          }
          if (t>4) {
            subj_data$C2_rew4[t]=subj_data$ACC[t-4]
            subj_data$B2_rew4[t]=subj_data$A2_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$C2_rew5[t]=subj_data$ACC[t-5]
            subj_data$B2_rew5[t]=subj_data$A2_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$C2_rew6[t]=subj_data$ACC[t-6]
            subj_data$B2_rew6[t]=subj_data$A2_rew6[t]=-1*subj_data$ACC[t-6]
          }
        }
      }
      
      if (t>3) {
        subj_data$choice_3[t]=subj_data$numeric_stim_choice[t-3]
        subj_data$rew_3[t]=subj_data$ACC[t-3]
        subj_data$switch_2[t]=subj_data$switch_from_prev[t-2]
        
        if (subj_data$numeric_stim_choice[t-3]==1) { #3-back choice
          subj_data$A3_rew1[t]=subj_data$ACC[t-1]
          subj_data$A3_rew2[t]=subj_data$ACC[t-2]
          subj_data$A3_rew3[t]=subj_data$ACC[t-3]
          subj_data$B3_rew1[t]=subj_data$C3_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$B3_rew2[t]=subj_data$C3_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$B3_rew3[t]=subj_data$C3_rew3[t]=-1*subj_data$ACC[t-3]
          if (t>4) {
            subj_data$A3_rew4[t]=subj_data$ACC[t-4]
            subj_data$B3_rew4[t]=subj_data$C3_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$A3_rew5[t]=subj_data$ACC[t-5]
            subj_data$B3_rew5[t]=subj_data$C3_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$A3_rew6[t]=subj_data$ACC[t-6]
            subj_data$B3_rew6[t]=subj_data$C3_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-3]==2) {
          subj_data$B3_rew1[t]=subj_data$ACC[t-1]
          subj_data$B3_rew2[t]=subj_data$ACC[t-2]
          subj_data$B3_rew3[t]=subj_data$ACC[t-3]
          subj_data$A3_rew1[t]=subj_data$C3_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A3_rew2[t]=subj_data$C3_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A3_rew3[t]=subj_data$C3_rew3[t]=-1*subj_data$ACC[t-3]
          if (t>4) {
            subj_data$B3_rew4[t]=subj_data$ACC[t-4]
            subj_data$A3_rew4[t]=subj_data$C3_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$B3_rew5[t]=subj_data$ACC[t-5]
            subj_data$A3_rew5[t]=subj_data$C3_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$B3_rew6[t]=subj_data$ACC[t-6]
            subj_data$A3_rew6[t]=subj_data$C3_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-3]==3) {
          subj_data$C3_rew1[t]=subj_data$ACC[t-1]
          subj_data$C3_rew2[t]=subj_data$ACC[t-2]
          subj_data$C3_rew3[t]=subj_data$ACC[t-3]
          subj_data$A3_rew1[t]=subj_data$B3_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A3_rew2[t]=subj_data$B3_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A3_rew3[t]=subj_data$B3_rew3[t]=-1*subj_data$ACC[t-3]
          if (t>4) {
            subj_data$C3_rew4[t]=subj_data$ACC[t-4]
            subj_data$B3_rew4[t]=subj_data$A3_rew4[t]=-1*subj_data$ACC[t-4]
          }
          if (t>5) {
            subj_data$C3_rew5[t]=subj_data$ACC[t-5]
            subj_data$B3_rew5[t]=subj_data$A3_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$C3_rew6[t]=subj_data$ACC[t-6]
            subj_data$B3_rew6[t]=subj_data$A3_rew6[t]=-1*subj_data$ACC[t-6]
          }
        }
      }
      
      if (t>4) {
        subj_data$choice_4[t]=subj_data$numeric_stim_choice[t-4]
        subj_data$rew_4[t]=subj_data$ACC[t-4]
        subj_data$switch_3[t]=subj_data$switch_from_prev[t-3]
        
        if (subj_data$numeric_stim_choice[t-4]==1) { #4-back choice
          subj_data$A4_rew1[t]=subj_data$ACC[t-1]
          subj_data$A4_rew2[t]=subj_data$ACC[t-2]
          subj_data$A4_rew3[t]=subj_data$ACC[t-3]
          subj_data$A4_rew4[t]=subj_data$ACC[t-4]
          subj_data$B4_rew1[t]=subj_data$C4_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$B4_rew2[t]=subj_data$C4_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$B4_rew3[t]=subj_data$C4_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$B4_rew4[t]=subj_data$C4_rew4[t]=-1*subj_data$ACC[t-4]
          if (t>5) {
            subj_data$A4_rew5[t]=subj_data$ACC[t-5]
            subj_data$B4_rew5[t]=subj_data$C4_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$A4_rew6[t]=subj_data$ACC[t-6]
            subj_data$B4_rew6[t]=subj_data$C4_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-4]==2) {
          subj_data$B4_rew1[t]=subj_data$ACC[t-1]
          subj_data$B4_rew2[t]=subj_data$ACC[t-2]
          subj_data$B4_rew3[t]=subj_data$ACC[t-3]
          subj_data$B4_rew4[t]=subj_data$ACC[t-4]
          subj_data$A4_rew1[t]=subj_data$C4_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A4_rew2[t]=subj_data$C4_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A4_rew3[t]=subj_data$C4_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A4_rew4[t]=subj_data$C4_rew4[t]=-1*subj_data$ACC[t-4]
          if (t>5) {
            subj_data$B4_rew5[t]=subj_data$ACC[t-5]
            subj_data$A4_rew5[t]=subj_data$C4_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$B4_rew6[t]=subj_data$ACC[t-6]
            subj_data$A4_rew6[t]=subj_data$C4_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-4]==3) {
          subj_data$C4_rew1[t]=subj_data$ACC[t-1]
          subj_data$C4_rew2[t]=subj_data$ACC[t-2]
          subj_data$C4_rew3[t]=subj_data$ACC[t-3]
          subj_data$C4_rew4[t]=subj_data$ACC[t-4]
          subj_data$A4_rew1[t]=subj_data$B4_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A4_rew2[t]=subj_data$B4_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A4_rew3[t]=subj_data$B4_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A4_rew4[t]=subj_data$B4_rew4[t]=-1*subj_data$ACC[t-4]
          if (t>5) {
            subj_data$C4_rew5[t]=subj_data$ACC[t-5]
            subj_data$B4_rew5[t]=subj_data$A4_rew5[t]=-1*subj_data$ACC[t-5]
          }
          if (t>6) {
            subj_data$C4_rew6[t]=subj_data$ACC[t-6]
            subj_data$B4_rew6[t]=subj_data$A4_rew6[t]=-1*subj_data$ACC[t-6]
          }
        }
      }
      
      if (t>5) {
        subj_data$choice_5[t]=subj_data$numeric_stim_choice[t-5]
        subj_data$rew_5[t]=subj_data$ACC[t-5]
        subj_data$switch_4[t]=subj_data$switch_from_prev[t-4]
        
        if (subj_data$numeric_stim_choice[t-5]==1) { #5-back choice
          subj_data$A5_rew1[t]=subj_data$ACC[t-1]
          subj_data$A5_rew2[t]=subj_data$ACC[t-2]
          subj_data$A5_rew3[t]=subj_data$ACC[t-3]
          subj_data$A5_rew4[t]=subj_data$ACC[t-4]
          subj_data$A5_rew5[t]=subj_data$ACC[t-5]
          subj_data$B5_rew1[t]=subj_data$C5_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$B5_rew2[t]=subj_data$C5_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$B5_rew3[t]=subj_data$C5_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$B5_rew4[t]=subj_data$C5_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$B5_rew5[t]=subj_data$C5_rew5[t]=-1*subj_data$ACC[t-5]
          if (t>6) {
            subj_data$A5_rew6[t]=subj_data$ACC[t-6]
            subj_data$B5_rew6[t]=subj_data$C5_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-5]==2) {
          subj_data$B5_rew1[t]=subj_data$ACC[t-1]
          subj_data$B5_rew2[t]=subj_data$ACC[t-2]
          subj_data$B5_rew3[t]=subj_data$ACC[t-3]
          subj_data$B5_rew4[t]=subj_data$ACC[t-4]
          subj_data$B5_rew5[t]=subj_data$ACC[t-5]
          subj_data$A5_rew1[t]=subj_data$C5_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A5_rew2[t]=subj_data$C5_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A5_rew3[t]=subj_data$C5_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A5_rew4[t]=subj_data$C5_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$A5_rew5[t]=subj_data$C5_rew5[t]=-1*subj_data$ACC[t-5]
          if (t>6) {
            subj_data$B5_rew6[t]=subj_data$ACC[t-6]
            subj_data$A5_rew6[t]=subj_data$C5_rew6[t]=-1*subj_data$ACC[t-6]
          }
        } else if (subj_data$numeric_stim_choice[t-5]==3) {
          subj_data$C5_rew1[t]=subj_data$ACC[t-1]
          subj_data$C5_rew2[t]=subj_data$ACC[t-2]
          subj_data$C5_rew3[t]=subj_data$ACC[t-3]
          subj_data$C5_rew4[t]=subj_data$ACC[t-4]
          subj_data$C5_rew5[t]=subj_data$ACC[t-5]
          subj_data$A5_rew1[t]=subj_data$B5_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A5_rew2[t]=subj_data$B5_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A5_rew3[t]=subj_data$B5_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A5_rew4[t]=subj_data$B5_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$A5_rew5[t]=subj_data$B5_rew5[t]=-1*subj_data$ACC[t-5]
          if (t>6) {
            subj_data$C5_rew6[t]=subj_data$ACC[t-6]
            subj_data$B5_rew6[t]=subj_data$A5_rew6[t]=-1*subj_data$ACC[t-6]
          }
        }
      }
      
      if (t>6) {
        subj_data$choice_6[t]=subj_data$numeric_stim_choice[t-6]
        subj_data$rew_6[t]=subj_data$ACC[t-6]
        subj_data$switch_5[t]=subj_data$switch_from_prev[t-5]
        
        if (subj_data$numeric_stim_choice[t-6]==1) { #6-back choice
          subj_data$A6_rew1[t]=subj_data$ACC[t-1]
          subj_data$A6_rew2[t]=subj_data$ACC[t-2]
          subj_data$A6_rew3[t]=subj_data$ACC[t-3]
          subj_data$A6_rew4[t]=subj_data$ACC[t-4]
          subj_data$A6_rew5[t]=subj_data$ACC[t-5]
          subj_data$A6_rew6[t]=subj_data$ACC[t-6]
          subj_data$B6_rew1[t]=subj_data$C6_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$B6_rew2[t]=subj_data$C6_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$B6_rew3[t]=subj_data$C6_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$B6_rew4[t]=subj_data$C6_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$B6_rew5[t]=subj_data$C6_rew5[t]=-1*subj_data$ACC[t-5]
          subj_data$B6_rew6[t]=subj_data$C6_rew6[t]=-1*subj_data$ACC[t-6]
        } else if (subj_data$numeric_stim_choice[t-6]==2) {
          subj_data$B6_rew1[t]=subj_data$ACC[t-1]
          subj_data$B6_rew2[t]=subj_data$ACC[t-2]
          subj_data$B6_rew3[t]=subj_data$ACC[t-3]
          subj_data$B6_rew4[t]=subj_data$ACC[t-4]
          subj_data$B6_rew5[t]=subj_data$ACC[t-5]
          subj_data$B6_rew6[t]=subj_data$ACC[t-6]
          subj_data$A6_rew1[t]=subj_data$C6_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A6_rew2[t]=subj_data$C6_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A6_rew3[t]=subj_data$C6_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A6_rew4[t]=subj_data$C6_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$A6_rew5[t]=subj_data$C6_rew5[t]=-1*subj_data$ACC[t-5]
          subj_data$A6_rew6[t]=subj_data$C6_rew6[t]=-1*subj_data$ACC[t-6]
        } else if (subj_data$numeric_stim_choice[t-6]==3) {
          subj_data$C6_rew1[t]=subj_data$ACC[t-1]
          subj_data$C6_rew2[t]=subj_data$ACC[t-2]
          subj_data$C6_rew3[t]=subj_data$ACC[t-3]
          subj_data$C6_rew4[t]=subj_data$ACC[t-4]
          subj_data$C6_rew5[t]=subj_data$ACC[t-5]
          subj_data$C6_rew6[t]=subj_data$ACC[t-6]
          subj_data$A6_rew1[t]=subj_data$B6_rew1[t]=-1*subj_data$ACC[t-1]
          subj_data$A6_rew2[t]=subj_data$B6_rew2[t]=-1*subj_data$ACC[t-2]
          subj_data$A6_rew3[t]=subj_data$B6_rew3[t]=-1*subj_data$ACC[t-3]
          subj_data$A6_rew4[t]=subj_data$B6_rew4[t]=-1*subj_data$ACC[t-4]
          subj_data$A6_rew5[t]=subj_data$B6_rew5[t]=-1*subj_data$ACC[t-5]
          subj_data$A6_rew6[t]=subj_data$B6_rew6[t]=-1*subj_data$ACC[t-6]
        }
      }
    }
  }
  write.csv(subj_data, file=paste0(data_dir,curr_subj,'_reshaped_preprocessed.csv'),col.names=TRUE)
  all_subj_data[(1+(subj-1)*270):(subj*270),]=as.matrix(subj_data)
}

colnames(all_subj_data)=colnames(subj_data)
write.csv(all_subj_data, file = paste0(data_dir,'all_reshaped_preprocessed.csv'),col.names=TRUE)