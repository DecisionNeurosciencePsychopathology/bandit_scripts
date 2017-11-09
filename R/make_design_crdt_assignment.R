# make design matrices for credit assignment models

library(readr)
library(lme4)
library(ggplot2)
library(dplyr)
library(tidyr)
library(psych)
library(gdata)
library(R.matlab)
library(xtable)
library(Hmisc)
library(nnet)
library(reshape2)
library(lsmeans)
library(compareGroups)

# load preprocessed data from both studies
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit2.RData")

# imaging study first
# rename for clarity and get lags
bdf$r <- bdf$reinf_n
bdf$A <- bdf$choiceA
bdf$B <- bdf$choiceB
bdf$C <- bdf$choiceC
bdf = bdf %>% group_by(ID) %>%
  mutate(
    rmin1 = lag(r, n = 1, order_by = Trial),
    rmin2 = lag(r, n = 2, order_by = Trial),
    rmin3 = lag(r, n = 3, order_by = Trial),
    rmin4 = lag(r, n = 4, order_by = Trial),
    rmin5 = lag(r, n = 5, order_by = Trial),

    Amin1 = lag(A, n = 1, order_by = Trial),
    Amin2 = lag(A, n = 2, order_by = Trial),
    Amin3 = lag(A, n = 3, order_by = Trial),
    Amin4 = lag(A, n = 4, order_by = Trial),
    Amin5 = lag(A, n = 5, order_by = Trial),

    Bmin1 = lag(B, n = 1, order_by = Trial),
    Bmin2 = lag(B, n = 2, order_by = Trial),
    Bmin3 = lag(B, n = 3, order_by = Trial),
    Bmin4 = lag(B, n = 4, order_by = Trial),
    Bmin5 = lag(B, n = 5, order_by = Trial),

    Cmin1 = lag(C, n = 1, order_by = Trial),
    Cmin2 = lag(C, n = 2, order_by = Trial),
    Cmin3 = lag(C, n = 3, order_by = Trial),
    Cmin4 = lag(C, n = 4, order_by = Trial),
    Cmin5 = lag(C, n = 5, order_by = Trial)
  ) %>% ungroup()

# get interactions
bdf = bdf %>% group_by(ID) %>%
  mutate(
    # lags of A

    Armin1 = A * rmin1,
    Armin2 = A * rmin2,
    Armin3 = A * rmin3,
    Armin4 = A * rmin4,
    Armin5 = A * rmin5,

    Amin1rmin1 = Amin1 * rmin1,
    Amin1rmin2 = Amin1 * rmin2,
    Amin1rmin3 = Amin1 * rmin3,
    Amin1rmin4 = Amin1 * rmin4,
    Amin1rmin5 = Amin1 * rmin5,

    Amin2rmin1 = Amin2 * rmin1,
    Amin2rmin2 = Amin2 * rmin2,
    Amin2rmin3 = Amin2 * rmin3,
    Amin2rmin4 = Amin2 * rmin4,
    Amin2rmin5 = Amin2 * rmin5,

    Amin3rmin1 = Amin3 * rmin1,
    Amin3rmin2 = Amin3 * rmin2,
    Amin3rmin3 = Amin3 * rmin3,
    Amin3rmin4 = Amin3 * rmin4,
    Amin3rmin5 = Amin3 * rmin5,

    Amin4rmin1 = Amin4 * rmin1,
    Amin4rmin2 = Amin4 * rmin2,
    Amin4rmin3 = Amin4 * rmin3,
    Amin4rmin4 = Amin4 * rmin4,
    Amin4rmin5 = Amin4 * rmin5,

    Amin5rmin1 = Amin5 * rmin1,
    Amin5rmin2 = Amin5 * rmin2,
    Amin5rmin3 = Amin5 * rmin3,
    Amin5rmin4 = Amin5 * rmin4,
    Amin5rmin5 = Amin5 * rmin5,


    # B
    Brmin1 = B * rmin1,
    Brmin2 = B * rmin2,
    Brmin3 = B * rmin3,
    Brmin4 = B * rmin4,
    Brmin5 = B * rmin5,

    Bmin1rmin1 = Bmin1 * rmin1,
    Bmin1rmin2 = Bmin1 * rmin2,
    Bmin1rmin3 = Bmin1 * rmin3,
    Bmin1rmin4 = Bmin1 * rmin4,
    Bmin1rmin5 = Bmin1 * rmin5,

    Bmin2rmin1 = Bmin2 * rmin1,
    Bmin2rmin2 = Bmin2 * rmin2,
    Bmin2rmin3 = Bmin2 * rmin3,
    Bmin2rmin4 = Bmin2 * rmin4,
    Bmin2rmin5 = Bmin2 * rmin5,

    Bmin3rmin1 = Bmin3 * rmin1,
    Bmin3rmin2 = Bmin3 * rmin2,
    Bmin3rmin3 = Bmin3 * rmin3,
    Bmin3rmin4 = Bmin3 * rmin4,
    Bmin3rmin5 = Bmin3 * rmin5,

    Bmin4rmin1 = Bmin4 * rmin1,
    Bmin4rmin2 = Bmin4 * rmin2,
    Bmin4rmin3 = Bmin4 * rmin3,
    Bmin4rmin4 = Bmin4 * rmin4,
    Bmin4rmin5 = Bmin4 * rmin5,

    Bmin5rmin1 = Bmin5 * rmin1,
    Bmin5rmin2 = Bmin5 * rmin2,
    Bmin5rmin3 = Bmin5 * rmin3,
    Bmin5rmin4 = Bmin5 * rmin4,
    Bmin5rmin5 = Bmin5 * rmin5,


    #C
    # lags of C
    Crmin1 = C * rmin1,
    Crmin2 = C * rmin2,
    Crmin3 = C * rmin3,
    Crmin4 = C * rmin4,
    Crmin5 = C * rmin5,

    Cmin1rmin1 = Cmin1 * rmin1,
    Cmin1rmin2 = Cmin1 * rmin2,
    Cmin1rmin3 = Cmin1 * rmin3,
    Cmin1rmin4 = Cmin1 * rmin4,
    Cmin1rmin5 = Cmin1 * rmin5,

    Cmin2rmin1 = Cmin2 * rmin1,
    Cmin2rmin2 = Cmin2 * rmin2,
    Cmin2rmin3 = Cmin2 * rmin3,
    Cmin2rmin4 = Cmin2 * rmin4,
    Cmin2rmin5 = Cmin2 * rmin5,

    Cmin3rmin1 = Cmin3 * rmin1,
    Cmin3rmin2 = Cmin3 * rmin2,
    Cmin3rmin3 = Cmin3 * rmin3,
    Cmin3rmin4 = Cmin3 * rmin4,
    Cmin3rmin5 = Cmin3 * rmin5,

    Cmin4rmin1 = Cmin4 * rmin1,
    Cmin4rmin2 = Cmin4 * rmin2,
    Cmin4rmin3 = Cmin4 * rmin3,
    Cmin4rmin4 = Cmin4 * rmin4,
    Cmin4rmin5 = Cmin4 * rmin5,

    Cmin5rmin1 = Cmin5 * rmin1,
    Cmin5rmin2 = Cmin5 * rmin2,
    Cmin5rmin3 = Cmin5 * rmin3,
    Cmin5rmin4 = Cmin5 * rmin4,
    Cmin5rmin5 = Cmin5 * rmin5

  ) %>% ungroup()

save(list = ls(all.names = TRUE), file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit3.RData")
