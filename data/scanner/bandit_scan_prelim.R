
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R")
library(readr)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(ggbiplot)
library(corrplot)
library(lsmeans)
library(factoextra)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(MASS)
library(effects)
library(readr)
library(VIM)
library(mice)

bandit_scan_df <-
  read_csv("bandit_scan_overview.csv")
View(bandit_scan_df)

df <- bandit_scan_df
df = df %>% as_tibble %>% arrange(ID)

sum(df$bandit_fMRI_unusable & df$bandit_behave_completed)
sum(df$bandit_fMRI_usable)
sum(df$reject_by_ra)
sum(df$reject_by_ra & df$bandit_behave_completed)
df$completed_bandit_and_rejected <- df$reject_by_ra*df$bandit_behave_completed
sum(df$completed_bandit_and_rejected)
sum(df$reject_by_ra*df$bandit_behav_usable)
sum(df$bandit_fMRI_processed)

# email to Jon:
# Rejected by RA:
# 202021 — no behavior
# 210381 — reject for bad behavior, but check learning curves anyway in hope for a miracle
# 217487 — was not scanned
# 219619 — behavioral data complete, first imaging block missing
# 220017 — suspicious behavior, check learning curves

df$alex_qc_comment <- NA
df$alex_qc_comment[df$ID==202021] <- 'no behavior'
df$alex_qc_comment[df$ID==210381] <- 'probably reject for bad behavior, but check learning curves'
df$alex_qc_comment[df$ID==217487] <- 'do not use, not scanned'
df$alex_qc_comment[df$ID==219619] <- 'behavioral data complete, first imaging block missing'
df$alex_qc_comment[df$ID==220017] <- 'suspicious behavior, check learning curves'

df$use_behavior <- df$bandit_behave_completed
df$use_behavior[df$ID==202021] <- 0
df$use_behavior[df$ID==210381] <- 1
df$use_behavior[df$ID==217487] <- 0
df$use_behavior[df$ID==219619] <- 1
df$use_behavior[df$ID==220017] <- 1

sum(df$use_behavior)

df <- df[,c(1:5,81:83,6:80)]

# df$fMRI_unusable <- df$bandit_fMRI_processed==1 & df$bandit_fMRI_usable==0

chars <- as.data.frame(df[df$use_behavior==1, c(57,40,13,76, 77, 73)])
c1 <-
  compareGroups(
    chars,
    y = bandit_scan_df$GROUP1245[df$use_behavior==1],
    bivar = TRUE,
    include.miss = FALSE
  )
t1 <-
  createTable(
    c1,
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t1, "bandit_scan_beh_final_by_group.html")

c1l <-
  compareGroups(
    chars,
    y = df$GROUP12467[df$use_behavior==1],
    bivar = TRUE,
    include.miss = FALSE
  )
t1l <-
  createTable(
    c1l,
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t1l, "bandit_scan_final_by_group_leth.html")


chars_good <- as.data.frame(df[df$bandit_fMRI_usable==1, c(7,10,51, 67, 69,70)])
c2 <-
  compareGroups(
    chars_good,
    y = df$GROUP1245[df$bandit_fMRI_usable==1],
    bivar = TRUE,
    include.miss = FALSE
  )
t2 <-
  createTable(
    c1,
    hide.no = 0,
    digits = 0,
    show.n = TRUE
  )
export2html(t2, "bandit_scan_currently_usable_by_group.html")



head(melt(bandit_scan_df))

#Multiple hists
ggplot(data = melt(bandit_scan_df), mapping = aes(x = value)) + 
  geom_histogram(bins = 10) + facet_wrap(~variable, scales = 'free_x')

#Age
hist(bandit_scan_df$LEARNAGE)

#Age by group
ggplot(bandit_scan_df, aes(x=bandit_scan_df$LEARNAGE)) +
  geom_histogram() +
  facet_wrap(~bandit_scan_df$GROUP1245)



