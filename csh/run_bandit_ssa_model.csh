#!/bin/csh
# single subject regression

#Shell function ran in conjunciton with python to parallelize the process of making single subject models
#Arg 1 should be idnumber
#Arg 2 can be the file destination but let's just make it default for now to get it to work



#If arg1 is 'time' make a tmp time stamp dir to run everyone, else if arg1 is 'theo' use the temporary theortical_models dir to
#run everyone who didn't run or anyone new, else run models in the mainSSADir, but this mostly likey won't run
#Until I can tell the script to run models that haven't been created yet <- this really is the optimal solution instead of all these
#temporary dirs!
#if ($# > 0) then
#   if ($1 == "time") then
#   	set out_dir = `date +%y_%m_%d_%H_%M_%S`
#   	mkdir "/Volumes/bek/learn/ssanalysis/bandit/${out_dir}" #make date dir
#   else if ($1 == "working") then
#   	set out_dir = "working_models"
#   endif
#else
#   set out_dir = "mainSSADir"
#endif

set out_dir = "testing_ground"


#copying template brain #make this work!
#cp ../template_barin.nii /Volumes/bek/skinner/LearnAnalysis/ssanalysis/bandit/${time_stamp}/

set id = ($1)
echo $id

chdir /Volumes/bek/learn/ssanalysis/bandit/${out_dir}

#Check if the subject has the required number of blocks, what to do if they don't have 3....
set num_bandit_blocks = `ls /Volumes/bek/learn/MR_Proc/*${id}*/bandit_MB_proc/bandit*/nf*bandit*_7.nii.gz | wc -l`
set num_func_blocks = `ls /Volumes/bek/learn/MR_Proc/*${id}*/bandit_MB_proc/bandit*/nf*functional*_7.nii.gz | wc -l`
set does_exist = `ls /Volumes/bek/learn/ssanalysis/bandit/${out_dir}/*${id}* | wc -l` 

echo "Number of bandit blocks: $num_bandit_blocks"
echo "Number of func blocks: $num_func_blocks"
echo "Already preprocessed: $does_exist"

if (($num_bandit_blocks == 3 || $num_func_blocks == 3)  && $does_exist == 0) then

   set funcpath = /Volumes/bek/learn/MR_Proc/*${id}*/bandit_MB_proc	
   set regpath = /Volumes/bek/learn/regs/bandit/fsl_version
   
   #Because some files are bandit1.2.3.nii.gz and other are funcitonal.nii.gz
   set bandit_func_name = `ls $funcpath/bandit1/bandit1.nii.gz | wc -l`
   if ($bandit_func_name == 1) then
   	set func_name = "_bandit"
   else
   	set func_name = "_functional"
   endif

   echo "Begin construction of regressors..."
   ## making a censor regressor based on motion
   cat `ls ${funcpath}/bandit*/motion_info/censor_union.1D | sort -n` > ${funcpath}/bandit_censor_motion.1D
   1deval -expr 'a*b' -a "${funcpath}/bandit_censor_motion.1D" -b "${regpath}/${id}banditCensorOnly.regs[0]" > "${regpath}/bandit${id}_motion_censor"
      

   #No data    
   #/usr/local/ni_tools/afni/3dDeconvolve \
   #    -nodata 200 1 \
   #    -num_stimts 4 \
   #    -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
   #    -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
   #    -stim_times_FSL 3 "${regpath}/${id}right.dat" 'dmUBLOCK' -stim_label 3 right \
   #    -stim_times_FSL 4 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 4 left \
   #    -censor "${regpath}/bandit${id}_motion_censor" \
   #    -nlast 200 -polort 0 -fout -rout -full_first \
   #    -x1D stdout: | 1dplot -stdin -one -thick -ynames null dec feed right left \   


   #Basic task map
   # /usr/local/ni_tools/afni/3dDeconvolve \
   #    -input /${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
   #     -mask /${funcpath}/*bandit1*/subject_mask.nii.gz \
   #     -num_stimts 3 \
   #     -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
   #     -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
   #     -stim_times_FSL 3 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 3 left \
   #     -censor "${regpath}/${id}banditCensorOnly.regs[0]" \
   #     -polort 12 -fout -rout -full_first \
   #     -bucket bandit_dec_feed_motor_${id} \
   #     -xjpeg ${id}_basic_design.png 


   # #With pos PEs
   #     /usr/local/ni_tools/afni/3dDeconvolve \
   #    -input /${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
   #     -mask /${funcpath}/*bandit1*/subject_mask.nii.gz \
   #     -num_stimts 5 \
   #     -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
   #     -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
   #     -stim_times_FSL 3 "${regpath}/${id}posPEs.dat" 'dmUBLOCK' -stim_label 3 posPE \
   #     -stim_times_FSL 4 "${regpath}/${id}right.dat" 'dmUBLOCK' -stim_label 4 right \
   #     -stim_times_FSL 5 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 5 left \
   #     -censor "${regpath}/${id}banditCensorOnly.regs[0]" \
   #     -polort 12 -fout -rout -full_first \
   #     -bucket bandit_dec_feed_posPE_motor_${id} \
   #     -xjpeg ${id}_basic_design.png 


   # #With neg PEs
   #     /usr/local/ni_tools/afni/3dDeconvolve \
   #    -input /${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
   #     -mask /${funcpath}/*bandit1*/subject_mask.nii.gz \
   #     -num_stimts 5 \
   #     -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
   #     -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
   #     -stim_times_FSL 3 "${regpath}/${id}negPEs.dat" 'dmUBLOCK' -stim_label 3 negPE \
   #     -stim_times_FSL 4 "${regpath}/${id}right.dat" 'dmUBLOCK' -stim_label 4 right \
   #     -stim_times_FSL 5 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 5 left \
   #     -censor "${regpath}/${id}banditCensorOnly.regs[0]" \
   #     -polort 12 -fout -rout -full_first \
   #     -bucket bandit_dec_feed_negPE_motor_${id} \
   #     -xjpeg ${id}_basic_design.png 


   #With pos and neg PEs
#       /usr/local/ni_tools/afni/3dDeconvolve \
#      -input ${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
#             ${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
#             ${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
#       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#       -num_stimts 5 \
#       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#       -stim_times_FSL 3 "${regpath}/${id}posPEs.dat" 'dmUBLOCK' -stim_label 3 posPE \
#       -stim_times_FSL 4 "${regpath}/${id}negPEs.dat" 'dmUBLOCK' -stim_label 4 negPE \
#       -stim_times_FSL 5 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 5 left \
#       -censor "${regpath}/bandit${id}_motion_censor" \
#       -polort 12 -fout -rout -full_first \
#       -bucket bandit_dec_feed_posAndNegPE_motor_${id} \
#       -xjpeg ${id}_basic_design.png 


   # #With value
   #     /usr/local/ni_tools/afni/3dDeconvolve \
   #    -input /${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
   #           /${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
   #     -mask /${funcpath}/*bandit1*/subject_mask.nii.gz \
   #     -num_stimts 4 \
   #     -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
   #     -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
   #     -stim_times_FSL 3 "${regpath}/${id}value.dat" 'dmUBLOCK' -stim_label 3 value \
   #     -stim_times_FSL 4 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 4 left \
   #     -censor "${regpath}/${id}banditCensorOnly.regs[0]" \
   #     -polort 12 -fout -rout -full_first \
   #     -bucket bandit_dec_feed_value_motor_${id} \
   #     -xjpeg ${id}_basic_design.png 


       #With value Diff centered
#       /usr/local/ni_tools/afni/3dDeconvolve \
#      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
#       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#       -num_stimts 4 \
#       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#       -stim_times_FSL 3 "${regpath}/${id}valueFeedbackAligned_diff.dat" 'dmUBLOCK' -stim_label 3 valueDiffFeedAligned \
#       -stim_times_FSL 4 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 4 left \
#       -censor "${regpath}/bandit${id}_motion_censor" \
#       -polort 12 -fout -rout -full_first \
#       -bucket bandit_valueDiff_${id} \
#       -xjpeg ${id}_basic_design.png 


       #With value and PEs
#       /usr/local/ni_tools/afni/3dDeconvolve \
# -input ${funcpath}/*bandit1*/nf*_bandit*.nii.gz \
#        ${funcpath}/*bandit2*/nf*_bandit*.nii.gz \
#        ${funcpath}/*bandit3*/nf*_bandit*.nii.gz \
#  -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#  -num_stimts 7 \
#  -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#  -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#  -stim_times_FSL 3 "${regpath}/${id}valueFeedbackAligned_diff.dat" 'dmUBLOCK' -stim_label 3 valueDiffFeedAligned \
#  -stim_times_FSL 4 "${regpath}/${id}posPEs.dat" 'dmUBLOCK' -stim_label 4 posPE \
#  -stim_times_FSL 5 "${regpath}/${id}negPEs.dat" 'dmUBLOCK' -stim_label 5 negPE \
#  -stim_times_FSL 6 "${regpath}/${id}right.dat" 'dmUBLOCK' -stim_label 6 right \
#  -stim_times_FSL 7 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 7 left \
#  -censor "${regpath}/bandit${id}_motion_censor" \
#  -polort 12 -fout -rout -full_first \
#  -bucket bandit_dec_feed_value_posnegPEs_motor_${id} \
#  -xjpeg ${id}_basic_design.png 
       
       #With value and PEsigned
#       /usr/local/ni_tools/afni/3dDeconvolve \
#      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
#       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#       -num_stimts 5 \
#       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#       -stim_times_FSL 3 "${regpath}/${id}valueFeedbackAligned_diff.dat" 'dmUBLOCK' -stim_label 3 valueDiffFeedAligned \
#       -stim_times_FSL 4 "${regpath}/${id}signedPEs.dat" 'dmUBLOCK' -stim_label 4 PEsigned \
#       -stim_times_FSL 5 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 5 left \
#       -censor "${regpath}/bandit${id}_motion_censor" \
#       -polort 12 -fout -rout -full_first \
#       -bucket bandit_dec_feed_valueDiff_PEsigned_${id} \
#       -xjpeg ${id}_basic_design.png 
       
       
       #3-30-2016
#       #With value chosen normalized and PE chosen
#       /usr/local/ni_tools/afni/3dDeconvolve \
#      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
#       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#       -num_stimts 5 \
#       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#       -stim_times_FSL 3 "${regpath}/${id}valueFeedbackAligned_chosen.dat" 'dmUBLOCK' -stim_label 3 valueChosenFeedAligned \
#       -stim_times_FSL 4 "${regpath}/${id}chosenPEs.dat" 'dmUBLOCK' -stim_label 4 PEChosen \
#       -stim_times_FSL 5 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 5 left \
#       -censor "${regpath}/bandit${id}_motion_censor" \
#       -polort 12 -fout -rout -full_first \
#       -bucket banditDecFeedValueChosenPEChosen_${id} \
#       -xjpeg ${id}_basic_design.png 
#       
#      #3-30-2016 
#      #With value chosen normalized and PE chosen
#       /usr/local/ni_tools/afni/3dDeconvolve \
#      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
#             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
#       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
#       -num_stimts 6 \
#       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
#       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
#       -stim_times_FSL 3 "${regpath}/${id}valueFeedbackAligned_chosen.dat" 'dmUBLOCK' -stim_label 3 valueChosenFeedAligned \
#       -stim_times_FSL 4 "${regpath}/${id}chosenPEs.dat" 'dmUBLOCK' -stim_label 4 PEChosen \
#       -stim_times_FSL 5 "${regpath}/${id}reward_trial_stake.dat" 'dmUBLOCK' -stim_label 5 rewardTrialStake \
#       -stim_times_FSL 6 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 6 left \
#       -censor "${regpath}/bandit${id}_motion_censor" \
#       -polort 12 -fout -rout -full_first \
#       -bucket banditDecFeedValueChosenPEChosenStake_${id} \
#       -xjpeg ${id}_basic_design.png 
       
       
       
       #3-31-2016
       #With value chosen minus the mean not chosen and stake aligned with decision
       /usr/local/ni_tools/afni/3dDeconvolve \
      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
       -num_stimts 6 \
       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
       -stim_times_FSL 3 "${regpath}/${id}stakeDecisionAligned.dat" 'dmUBLOCK' -stim_label 3 StakeDecAligned \
       -stim_times_FSL 4 "${regpath}/${id}valueDecisionAligned_chosen_diff.dat" 'dmUBLOCK' -stim_label 4 valueChosenDiffDecAligned \
       -stim_times_FSL 5 "${regpath}/${id}chosenPEs.dat" 'dmUBLOCK' -stim_label 5 PEChosen \
       -stim_times_FSL 6 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 6 left \
       -censor "${regpath}/bandit${id}_motion_censor" \
       -polort 12 -fout -rout -full_first \
       -bucket banditDecFeedValueChosenDiffStakeDecPEChosen_${id} \
       -xjpeg ${id}_basic_design.png 
       
       #3-31-2016
       #With value chosen minus the mean not chosen and stake aligned with decision and feedback
       /usr/local/ni_tools/afni/3dDeconvolve \
      -input ${funcpath}/*bandit1*/nf*$func_name*.nii.gz \
             ${funcpath}/*bandit2*/nf*$func_name*.nii.gz \
             ${funcpath}/*bandit3*/nf*$func_name*.nii.gz \
       -mask ${funcpath}/*bandit1*/subject_mask.nii.gz \
       -num_stimts 7 \
       -stim_times_FSL 1 "${regpath}/${id}decision_Times.dat" 'dmUBLOCK' -stim_label 1 decision1 \
       -stim_times_FSL 2 "${regpath}/${id}feedback_Times.dat" 'dmUBLOCK' -stim_label 2 feedback \
       -stim_times_FSL 3 "${regpath}/${id}stakeDecisionAligned.dat" 'dmUBLOCK' -stim_label 3 StakeDecAligned \
       -stim_times_FSL 4 "${regpath}/${id}stakeFeedbackAligned.dat" 'dmUBLOCK' -stim_label 4 StakeFeedAligned \
       -stim_times_FSL 5 "${regpath}/${id}valueDecisionAligned_chosen_diff.dat" 'dmUBLOCK' -stim_label 5 valueChosenDiffDecAligned \
       -stim_times_FSL 6 "${regpath}/${id}chosenPEs.dat" 'dmUBLOCK' -stim_label 6 PEChosen \
       -stim_times_FSL 7 "${regpath}/${id}left.dat" 'dmUBLOCK' -stim_label 7 left \
       -censor "${regpath}/bandit${id}_motion_censor" \
       -polort 12 -fout -rout -full_first \
       -bucket banditDecFeedValueChosenDiffStakeDecFeedPEChosen_${id} \
       -xjpeg ${id}_basic_design.png 
       

else if ($does_exist > 0) then
   echo 'subject '${id}' already preprocessed'
else
   echo 'subject '${id}' not preprocessed...Investigate'
endif

