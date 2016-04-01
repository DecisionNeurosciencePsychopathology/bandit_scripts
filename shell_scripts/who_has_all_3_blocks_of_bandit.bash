#!/bin/bash

#Quick one liner to grab all bandit ids that have completed 3rd run of bandit, HOWEVER this is assuming 
find /Volumes/bek/learn/MR_Proc/*/bandit*/bandit3/ -iname "nfsw*bandit*.nii.gz" -maxdepth 1 | grep -Eo '[0-9]{4,6}' > idlog.txt

#Some subjects don't have bandit1 2 3 they have funcitonal as the filename
find /Volumes/bek/learn/MR_Proc/*/bandit*/bandit3/ -iname "nfsw*functional*.nii.gz" -maxdepth 1 | grep -Eo '[0-9]{4,6}' >> idlog.txt

#sort the file and display it
sort idlog.txt -o idlog.txt

cat idlog.txt

