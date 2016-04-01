#My attempt to use gen_group_command.py
   
#Arg 1 = prefix Ex: tt++PREFIX+tlrc.BRIK.gz 
#Arg 2 = set labels
#Arg 3 = sub betas

#Quick arg check
if [ $# -lt 3 ]; then
  echo 1>&2 "$0: not enough arguments"
  exit 2
elif [ $# -gt 3 ]; then
  echo 1>&2 "$0: too many arguments"
  exit 2
fi


gen_group_command.py -command 3dttest++ \
                     -dsets /Volumes/bek/learn/ssanalysis/bandit/testing_ground/banditDecFeedValueChosenDiffStakeDecFeedPEChosen_*+tlrc.HEAD \
		     -write_script .tmp \
		     -prefix tt++$1 \
		     -set_labels $2 \
		     -subs_betas $3
		    
		    		     
#Remoe the .HEAD part of the file Finish this up also neeed to add #!/bin/csh to top of file and fix curly brackets?
sed -e 's/.HEAD//g' .tmp > cmd.t++.3
echo '#!/bin/csh' | cat - cmd.t++.3 > temp_text && mv temp_text cmd.t++.3

#Run command
chmod 775 cmd.t++.3
./cmd.t++.3

#Move output to group analysis dir
mv *+tlrc.* ../grpanalysis/working_group_analysis/
