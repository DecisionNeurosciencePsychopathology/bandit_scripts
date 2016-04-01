#!/bin/csh
# single subject regression
# starts out with basic behavioral regressors: switch, error/correct

#set id = $1 - for entering the name of subject on
#set id = 216806
#set regid = 0716

#set time_stamp = `date +%y_%m_%d_%H_%M_%S`
#set suff = "_trust_prelim"
#mkdir "/Volumes/bek/trust_analyses/ssanal/${time_stamp}" #make date dir


#foreach line ( "`cat trust_subjectlist`" )
#echo $line
#set id = ($line)
#echo $id


#cd /Volumes/bek/learn/MR_Proc/*${id}*/trustGame_proc
set id = 112415_jo
set funcpath =  /Volumes/bek/explore/MR_Proc/*${id}*/shark_proc/shark
set regpath = /Volumes/bek/explore/shark/regs/JANO
#set block_specific_regpath = /Volumes/bek/trust_analyses/regs/single_block_regs
#cat `ls *rust*/motion.par | sort -n` > trust_motions

## ask Michael for the thing that makes censor_union.1D
#cat `ls *rust*/motion_info/censor_union.1D | sort -n` > trust_censor_motion.1D


## THIS DID NOT RUN: fix motion censoring 082015s
#3dcalc -DAFNI_USE_FGETS=YES -a "${funcpath}/trust_censor_motion" -b "/Volumes/bek/trust_analyses/regs/trust${id}.regs[0]" -expr 'a*b'
#1deval -expr 'a*b' -a "${funcpath}/trust_censor_motion.1D" -b "${regpath}/trust${id}.regs[0]" > "${regpath}/trust${id}.regs[0]"

# this does not create the right file for some reason
1deval -expr 'a*b' -a "${funcpath}/motion_info/censor_union.1D" -b "${regpath}/shark${id}.regs[0]" > "${regpath}/shark_motion_censor"

# model with block, right-left
#cd /Volumes/bek/trust_analyses/ssanal
#cd /Volumes/bek/trust_analyses/ssanal/${time_stamp}


cd /Volumes/bek/explore/shark/ssanal/

        
#Just look at the first block of trust game
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 \
    -stim_file 1  "${regpath}/shark${id}.regs[1]"    -stim_label 1 decision1 \
    -stim_file 2  "${regpath}/shark${id}.regs[2]"    -stim_label 2 decision2 \
    -stim_file 3  "${regpath}/shark${id}.regs[4]"    -stim_label 3 feedback \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_trial_phases${id} \
    -xjpeg ${id}_basic_design.png
    
    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 1 \
    -stim_file 1  "${regpath}/shark${id}.regs[3]"    -stim_label 1 trial \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_trial_Only${id} \
    -xjpeg ${id}_basic_design.png
    
    
    
    
#Let give FSL version a whirl, since we're 'unfortunate' people
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_fslTimes.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_fslTimes.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_fslTimes.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_fslTimes${id} \
    -xjpeg ${id}_basic_design.png
    

/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 5 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_fslTimes.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_fslTimes.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_fslTimes.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/win_loss_fslTimes.dat" 'dmUBLOCK' -stim_label 4 win_loss \
    -stim_times_FSL 5 "${regpath}/switch_stay_fslTimes.dat" 'dmUBLOCK' -stim_label 5 switch_stay \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_dec_feed_winLoss_switchStay_updated${id} \
    -xjpeg ${id}_basic_design.png
    
    
    
#Trial only    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 1 \
    -stim_times 1 "${regpath}/trial_Times_noFSL.dat" 'SPMG2(1)' -stim_label 1 decision1 \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_trialOnly_SPMG1_${id} \
    -xjpeg ${id}_basic_design.png
    
    
    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 7 \
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(1)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/decision_level_2_Times_noFSL.dat" 'SPMG2(1)' -stim_label 2 decision2 \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(1)' -stim_label 3 feedback \
    -stim_times 4 "${regpath}/win_Times_noFSL.dat" 'SPMG2(1)' -stim_label 4 win \
    -stim_times 5 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(1)' -stim_label 5 loss \
    -stim_times 6 "${regpath}/left_index_noFSL.dat" 'SPMG2(1)' -stim_label 6 left_index \
    -stim_times 7 "${regpath}/right_index_noFSL.dat" 'SPMG2(1)' -stim_label 7 right_index \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_dec_feed_winLoss_SPMG1_${id} \
    -xjpeg ${id}_basic_design.png
    
    
    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 7 \
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/decision_level_2_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 decision2 \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 feedback \
    -stim_times 4 "${regpath}/stay_Times_noFSL.dat" 'SPMG2(0)' -stim_label 4 stay \
    -stim_times 5 "${regpath}/switch_Times_noFSL.dat" 'SPMG2(0)' -stim_label 5 switch \
    -stim_times 6 "${regpath}/left_index_noFSL.dat" 'SPMG2(0)' -stim_label 6 left_index \
    -stim_times 7 "${regpath}/right_index_noFSL.dat" 'SPMG2(0)' -stim_label 7 right_index \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_staySwitch_SPMG0_${id} \
    -xjpeg ${id}_basic_design.png
    
    
    
#Becase of a colinearity error tried removing some regressors
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 5 \
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/decision_level_2_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 decision2 \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 feedback \
    -stim_times 4 "${regpath}/left_index_noFSL.dat" 'SPMG2(0)' -stim_label 4 left_index \
    -stim_times 5 "${regpath}/right_index_noFSL.dat" 'SPMG2(0)' -stim_label 5 right_index \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_motor_SPMG0_${id} \
    -xjpeg ${id}_basic_design.png
    
#Becase of a colinearity error tried removing some regressors
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 \
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/decision_level_2_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 decision2 \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 feedback \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_SPMG0_${id} \
    -xjpeg ${id}_basic_design.png
    
    
#SPMG@ run of just decision 1 and feedback    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 2 \
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(1)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(1)' -stim_label 2 feedback \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec1Only_feed_SPMG1_${id} \
    -xjpeg ${id}_basic_design.png
    
    
#No data run
/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 4 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/win_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 win \
    -stim_times 4 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(0)' -stim_label 4 loss \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames dec1dt dec1 feeddt feed windt win lossdt loss\
    
# look at'em one by one

/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 1 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames dec1dt dec1\


/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 loss \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames null dec1 dec1dt feed feeddt loss lossdt \

/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/win_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 win \
    -stim_times 2 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 loss \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 feedback \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames null win windt loss lossdt \
    
    
    
    
    
    
    
#Trying dec feed and stay switch / win loss...   
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2(0)' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 loss \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_lossOnly_SPMG0_${id} \
    -xjpeg ${id}_basic_design.png
    
#Trying stim_timesAM...win=1 loss=-1
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFL.dat" 'SPMG2(0)' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/loss_Times_noFSL.dat" 'SPMG2(0)' -stim_label 3 loss \
    -censor "${regpath}/shark${id}.regs[0]" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_lossOnly_SPMG0_${id} \
    -xjpeg ${id}_basic_design.png
    
    
#12/1/15    
    
#Trying dec feed and stay switch / win loss... couldn't read in censor file...
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG2' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG2' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/loss_Times_noFSL.dat" 'SPMG2' -stim_label 3 loss \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_lossOnly_SPMG_no_0_${id} \
    -xjpeg ${id}_basic_design.png
    
    
#Try using SPMG1 for dec feed and loss
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG1(0)' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/feedback_Times_noFSL.dat" 'SPMG1(0)' -stim_label 2 feedback \
    -stim_times 3 "${regpath}/loss_Times_noFSL.dat" 'SPMG1(0)' -stim_label 3 loss \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feed_lossOnly_SPMG1_with_0_${id} \
    -xjpeg ${id}_basic_design.png
    
#Try using SPMG1 with just dec 1 and 2 and feedback
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times 1 "${regpath}/decision_level_1_Times_noFSL.dat" 'SPMG1' -stim_label 1 decision1 \
    -stim_times 2 "${regpath}/decision_level_2_Times_noFSL.dat" 'SPMG1' -stim_label 2 decision2 \
    -stim_times 3 "${regpath}/feedback_Times_noFSL.dat" 'SPMG1' -stim_label 3 feedback \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec1_2_feed_SPMG1_no_0_${id} \
    -xjpeg ${id}_basic_design.png
    
    
#Just looking at trial no ITI or ISI ie the jitter that is fixed at 1.5 secs    
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 1 \
    -stim_times_FS1 1 "${regpath}/trialNoJitters.dat" 'dmUBLOCK' -stim_label 1 trial \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_trial_noJitters_dmUBlock${id} \
    -xjpeg ${id}_basic_design.png


#Investigate both decision levels feedback  
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 3 -global_times\
    -stim_times_FS1 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_polort12${id} \
    -xjpeg ${id}_basic_design.png
    
    
#Investigate both decision levels feedback and win loss   
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 4 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 4 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_winLoss${id} \
    -xjpeg ${id}_basic_design.png
    
#Investigate both decision levels feedback and win loss polort 12  
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 4 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 4 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_winLoss_polort12_${id} \
    -xjpeg ${id}_basic_design.png    
       
    
    
#Investigate both decision levels feedback and switch stay  
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 4 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/switch_stay_Times.dat" 'dmUBLOCK' -stim_label 4 switch_stay \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 2 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_switchStay${id} \
    -xjpeg ${id}_basic_design.png
    
#Investigate both decision levels feedback and switch stay polort 12
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 4 \
    -stim_times_FS1 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FS1 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FS1 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/switch_stay_Times.dat" 'dmUBLOCK' -stim_label 4 switch_stay \
    -censor "${regpath}/shark_motion_censor" \
    -stim_minlag 1 0 -stim_maxlag 1 0 -polort 12 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_switchStay_polort12_${id} \
    -xjpeg ${id}_basic_design.png    
    
    
    
    
#No data    
/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 4 \
    -stim_times_FSL 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FSL 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FSL 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 4 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -nlast 200 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames null dec1 dec2 feed winLoss \    
    


#12/2/15 Start here...

#Investigate both decision levels feedback and win loss with win being negative  
#DONT USE NO DIFFERENCE!
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 4 \
    -stim_times_FSL 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FSL 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FSL 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/win_loss_Times_win_minus1.dat" 'dmUBLOCK' -stim_label 4 win_loss_winMinus1 \
    -censor "${regpath}/shark_motion_censor" \
    -polort 2 -fout -rout -full_first \
    -bucket shark_dec1-2_feedback_winLoss_winMiunsOne${id} \
    -xjpeg ${id}_basic_design.png
        
    
#collinear issue with motors left lvl 1 right lvl 1 and 2's
/usr/local/ni_tools/afni/3dDeconvolve \
    -nodata 200 1 \
    -num_stimts 6 \
    -stim_times_FSL 1 "${regpath}/left_index_level_1.dat" 'dmUBLOCK' -stim_label 1 leftLvL1 \
    -stim_times_FSL 2 "${regpath}/right_index_level_1.dat" 'dmUBLOCK' -stim_label 2 rightLvL1 \
    -stim_times_FSL 3 "${regpath}/left_index_level_2.dat" 'dmUBLOCK' -stim_label 3 leftLvL2 \
    -stim_times_FSL 4 "${regpath}/right_index_level_2.dat" 'dmUBLOCK' -stim_label 4 rightLvL2 \
    -stim_times_FSL 5 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 5 feedback \
    -stim_times_FSL 6 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 6 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -nlast 200 -polort 0 -fout -rout -full_first \
    -x1D stdout: | 1dplot -stdin -one -thick -ynames null left1 right1 left2 right2 feed winLoss\
    
    
#Replace decision regs with motor polort 12  
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 6 \
    -stim_times_FSL 1 "${regpath}/left_index_level_1.dat" 'dmUBLOCK' -stim_label 1 leftLvL1 \
    -stim_times_FSL 2 "${regpath}/right_index_level_1.dat" 'dmUBLOCK' -stim_label 2 rightLvL1 \
    -stim_times_FSL 3 "${regpath}/left_index_level_2.dat" 'dmUBLOCK' -stim_label 3 leftLvL2 \
    -stim_times_FSL 4 "${regpath}/right_index_level_2.dat" 'dmUBLOCK' -stim_label 4 rightLvL2 \
    -stim_times_FSL 5 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 5 feedback \
    -stim_times_FSL 6 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 6 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -polort 12 -fout -rout -full_first \
    -bucket shark_motor_feedback_winLoss_${id} \
    -xjpeg ${id}_basic_design.png      
    
    
#Combine decision and motor with win/loss 
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 6 \
    -stim_times_FSL 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FSL 2 "${regpath}/right_left_index_level_1.dat" 'dmUBLOCK' -stim_label 2 rightLeft1 \
    -stim_times_FSL 3 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 3 decision2 \
    -stim_times_FSL 4 "${regpath}/right_left_index_level_2.dat" 'dmUBLOCK' -stim_label 4 rightLeft2 \
    -stim_times_FSL 5 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 5 feedback \
    -stim_times_FSL 6 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 6 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -polort 12 -fout -rout -full_first \
    -bucket shark_dec_motor_feedback_winLoss_${id} \
    -xjpeg ${id}_basic_design.png 
    
    
#Combine decision and motor with freq/infreq
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 6 \
    -stim_times_FSL 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FSL 2 "${regpath}/right_left_index_level_1.dat" 'dmUBLOCK' -stim_label 2 rightLeft1 \
    -stim_times_FSL 3 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 3 decision2 \
    -stim_times_FSL 4 "${regpath}/right_left_index_level_2.dat" 'dmUBLOCK' -stim_label 4 rightLeft2 \
    -stim_times_FSL 5 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 5 feedback \
    -stim_times_FSL 6 "${regpath}/freq_infreq_Times.dat" 'dmUBLOCK' -stim_label 6 freqInfreq \
    -censor "${regpath}/shark_motion_censor" \
    -polort 12 -fout -rout -full_first \
    -bucket shark_dec_motor_feedback_freqInfreq_${id} \
    -xjpeg ${id}_basic_design.png         
    
    
#12/3/15 Start here...    
#Full model so far dec motor feedback freq/infreq win/loss
/usr/local/ni_tools/afni/3dDeconvolve \
    -input ${funcpath}/nf*functional*.nii.gz \
    -mask ${funcpath}/subject_mask.nii.gz -nfirst 1 \
    -num_stimts 6 \
    -stim_times_FSL 1 "${regpath}/decision_level_1_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
    -stim_times_FSL 2 "${regpath}/decision_level_2_Times.dat" 'dmUBLOCK' -stim_label 2 decision2 \
    -stim_times_FSL 3 "${regpath}/feedback_Times.dat" 'dmUBLOCK' -stim_label 3 feedback \
    -stim_times_FSL 4 "${regpath}/right_left_index.dat" 'dmUBLOCK' -stim_label 4 rightLeftIndex \
    -stim_times_FSL 5 "${regpath}/freq_infreq_Times.dat" 'dmUBLOCK' -stim_label 5 freqInfreq \
    -stim_times_FSL 6 "${regpath}/win_loss_Times.dat" 'dmUBLOCK' -stim_label 6 win_loss \
    -censor "${regpath}/shark_motion_censor" \
    -polort 12 -fout -rout -full_first \
    -bucket shark_dec_feedback_motor_freqInfreq_winLoss_${id} \
    -xjpeg ${id}_basic_design.png 

    
    
    
end


#Looking at the difference in dmUBlock unit setttings
    3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 3 \
                 -stim_times_FSL 1 q2.1D 'dmUBLOCK'      \
                 -stim_times_FSL 2 q2.1D 'dmUBLOCK(1)'   \
                 -stim_times_FSL 3 q2.1D 'dmUBLOCK(-4)'  \
                 -x1D stdout: |                         \
     1dplot -stdin -thick                               \
            -ynames 'dmUBLOCK' 'dmUB(1)' 'dmUB(-4)'  
	    
	    
#Looking 
    3dDeconvolve -nodata 350 1 -polort -1 -num_stimts 3 \
                 -stim_times_FSL 1 q2.1D 'dmUBLOCK'      \
                 -stim_times_FSL 2 q2.1D 'dmUBLOCK(1)'   \
                 -stim_times_FSL 3 q2.1D 'dmUBLOCK(-4)'  \
                 -x1D stdout: |                         \
     1dplot -stdin -thick                               \
            -ynames 'dmUBLOCK' 'dmUB(1)' 'dmUB(-4)'  

