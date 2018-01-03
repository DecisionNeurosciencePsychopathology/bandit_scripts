#  Alex's tidyverse version of Vanessa's credit assignment models
#  run bandit_beh_analyses.R and make_design_crdt_assignment.R first
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit3.RData")


require(lme4)
library(corrplot)

# really dirty trick, make sure you clear the workspace afterward
# bdf <- rdf

#run regressions

A_reg_all5 = glmer(
  A ~
    Amin1 + Amin2 + Amin3 + Amin4 + Amin5 + 
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 + 
    Amin1rmin1 + Amin1rmin2 + Amin1rmin3 + Amin1rmin4 + Amin1rmin5 + 
    Amin2rmin1 + Amin2rmin2 + Amin2rmin3 + Amin2rmin4 + Amin2rmin5 + 
    Amin3rmin1 + Amin3rmin2 + Amin3rmin3 + Amin3rmin4 + Amin3rmin5 + 
    Amin4rmin1 + Amin4rmin2 + Amin4rmin3 + Amin4rmin4 + Amin4rmin5 + 
    Amin5rmin1 + Amin5rmin2 + Amin5rmin3 + Amin5rmin4 + Amin5rmin5 + 
    Amin6 + rmin6 +
    Amin1rmin6 + Amin2rmin6 + Amin3rmin6 + Amin4rmin6 + Amin5rmin6 +
    Amin6rmin1 + Amin6rmin2 + Amin6rmin3 + Amin6rmin4 + Amin6rmin5 + Amin6rmin6 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ = 0
  # glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
# check coeff matrix
A5 = matrix(data = fixef(A_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(A5,cl.lim = c(-3, 3),is.corr = FALSE, method = "color")

Az <- coef(summary(A_reg_all5))[,"z value"]  ## identical



B_reg_all5 = glmer(
  B ~
    Bmin1 + Bmin2 + Bmin3 + Bmin4 + Bmin5 + 
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 + 
    Bmin1rmin1 + Bmin1rmin2 + Bmin1rmin3 + Bmin1rmin4 + Bmin1rmin5 + 
    Bmin2rmin1 + Bmin2rmin2 + Bmin2rmin3 + Bmin2rmin4 + Bmin2rmin5 + 
    Bmin3rmin1 + Bmin3rmin2 + Bmin3rmin3 + Bmin3rmin4 + Bmin3rmin5 + 
    Bmin4rmin1 + Bmin4rmin2 + Bmin4rmin3 + Bmin4rmin4 + Bmin4rmin5 + 
    Bmin5rmin1 + Bmin5rmin2 + Bmin5rmin3 + Bmin5rmin4 + Bmin5rmin5 + 
    Bmin6 + rmin6 +
    Bmin1rmin6 + Bmin2rmin6 + Bmin3rmin6 + Bmin4rmin6 + Bmin5rmin6 +
    Bmin6rmin1 + Bmin6rmin2 + Bmin6rmin3 + Bmin6rmin4 + Bmin6rmin5 + Bmin6rmin6 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ = 0
  # glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
# check coeff matrix
B5 = matrix(data = fixef(B_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(B5,cl.lim = c(-3, 3),is.corr = FALSE, method = "color")

C_reg_all5 = glmer(
  C ~
    Cmin1 + Cmin2 + Cmin3 + Cmin4 + Cmin5 + 
    rmin1 + rmin2 + rmin3 + rmin4 + rmin5 + 
    Cmin1rmin1 + Cmin1rmin2 + Cmin1rmin3 + Cmin1rmin4 + Cmin1rmin5 + 
    Cmin2rmin1 + Cmin2rmin2 + Cmin2rmin3 + Cmin2rmin4 + Cmin2rmin5 + 
    Cmin3rmin1 + Cmin3rmin2 + Cmin3rmin3 + Cmin3rmin4 + Cmin3rmin5 + 
    Cmin4rmin1 + Cmin4rmin2 + Cmin4rmin3 + Cmin4rmin4 + Cmin4rmin5 + 
    Cmin5rmin1 + Cmin5rmin2 + Cmin5rmin3 + Cmin5rmin4 + Cmin5rmin5 + 
    Cmin6 + rmin6 +
    Cmin1rmin6 + Cmin2rmin6 + Cmin3rmin6 + Cmin4rmin6 + Cmin5rmin6 +
    Cmin6rmin1 + Cmin6rmin2 + Cmin6rmin3 + Cmin6rmin4 + Cmin6rmin5 + Cmin6rmin6 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ = 0
  # glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)
C5 = matrix(data = fixef(C_reg_all5)[12:36],nrow = 5,ncol=5)
corrplot(C5,is.corr = FALSE, method = "color")

Az <- coef(summary(A_reg_all5))[,"z value"]  ## identical
Bz <- coef(summary(A_reg_all5))[,"z value"]  ## identical
Cz <- coef(summary(A_reg_all5))[,"z value"]  ## identical
all_z <- (Az + Bz + Cz)/3

all_coeffs_combined_matrix5 = matrix(data = all_z[12:36],nrow = 5,ncol=5)
colnames(all_coeffs_combined_matrix5) <- c("r(t-1)","r(t-2)", "r(t-3)","r(t-4)","r(t-5)")
rownames(all_coeffs_combined_matrix5) <- c("choice(t-1)","choice(t-2)", "choice(t-3)","choice(t-4)","choice(t-5)")
corrplot(all_coeffs_combined_matrix5,cl.lim = c(-7,7), is.corr = FALSE, method = "color", title = paste0("Interactions between past choices \n",
         "and rewards predicting choice, \n",
          "z statistics"), mar=c(0,0,3,0))

#  with group

A_reg_all5_g = glmer(
  A ~
    Group*Amin1 + Group*Amin2 + Group*Amin3 + Group*Amin4 + Group*Amin5 +
    Group*rmin1 + Group*rmin2 + Group*rmin3 + Group*rmin4 + Group*rmin5 +
    Group*Amin1rmin1 + Group*Amin1rmin2 + Group*Amin1rmin3 + Group*Amin1rmin4 + Group*Amin1rmin5 +
    Group*Amin2rmin1 + Group*Amin2rmin2 + Group*Amin2rmin3 + Group*Amin2rmin4 + Group*Amin2rmin5 +
    Group*Amin3rmin1 + Group*Amin3rmin2 + Group*Amin3rmin3 + Group*Amin3rmin4 + Group*Amin3rmin5 +
    Group*Amin4rmin1 + Group*Amin4rmin2 + Group*Amin4rmin3 + Group*Amin4rmin4 + Group*Amin4rmin5 +
    Group*Amin5rmin1 + Group*Amin5rmin2 + Group*Amin5rmin3 + Group*Amin5rmin4 + Group*Amin5rmin5 +
    Group*Amin6 + Group*rmin6 +
    Group*Amin1rmin6 + Group*Amin2rmin6 + Group*Amin3rmin6 + Group*Amin4rmin6 + Group*Amin5rmin6 +
    Group*Amin6rmin1 + Group*Amin6rmin2 + Group*Amin6rmin3 + Group*Amin6rmin4 + Group*Amin6rmin5 + Group*Amin6rmin6 +
    
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


B_reg_all5_g = glmer(
  B ~
    Group*Bmin1 + Group*Bmin2 + Group*Bmin3 + Group*Bmin4 + Group*Bmin5 +
    Group*rmin1 + Group*rmin2 + Group*rmin3 + Group*rmin4 + Group*rmin5 +
    Group*Bmin1rmin1 + Group*Bmin1rmin2 + Group*Bmin1rmin3 + Group*Bmin1rmin4 + Group*Bmin1rmin5 +
    Group*Bmin2rmin1 + Group*Bmin2rmin2 + Group*Bmin2rmin3 + Group*Bmin2rmin4 + Group*Bmin2rmin5 +
    Group*Bmin3rmin1 + Group*Bmin3rmin2 + Group*Bmin3rmin3 + Group*Bmin3rmin4 + Group*Bmin3rmin5 +
    Group*Bmin4rmin1 + Group*Bmin4rmin2 + Group*Bmin4rmin3 + Group*Bmin4rmin4 + Group*Bmin4rmin5 +
    Group*Bmin5rmin1 + Group*Bmin5rmin2 + Group*Bmin5rmin3 + Group*Bmin5rmin4 + Group*Bmin5rmin5 +
    Group*Bmin6 + Group*rmin6 +
    Group*Bmin1rmin6 + Group*Bmin2rmin6 + Group*Bmin3rmin6 + Group*Bmin4rmin6 + Group*Bmin5rmin6 +
    Group*Bmin6rmin1 + Group*Bmin6rmin2 + Group*Bmin6rmin3 + Group*Bmin6rmin4 + Group*Bmin6rmin5 + Group*Bmin6rmin6 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,

  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

C_reg_all5_g = glmer(
  C ~
    Group*Cmin1 + Group*Cmin2 + Group*Cmin3 + Group*Cmin4 + Group*Cmin5 +
    Group*rmin1 + Group*rmin2 + Group*rmin3 + Group*rmin4 + Group*rmin5 +
    Group*Cmin1rmin1 + Group*Cmin1rmin2 + Group*Cmin1rmin3 + Group*Cmin1rmin4 + Group*Cmin1rmin5 +
    Group*Cmin2rmin1 + Group*Cmin2rmin2 + Group*Cmin2rmin3 + Group*Cmin2rmin4 + Group*Cmin2rmin5 +
    Group*Cmin3rmin1 + Group*Cmin3rmin2 + Group*Cmin3rmin3 + Group*Cmin3rmin4 + Group*Cmin3rmin5 +
    Group*Cmin4rmin1 + Group*Cmin4rmin2 + Group*Cmin4rmin3 + Group*Cmin4rmin4 + Group*Cmin4rmin5 +
    Group*Cmin5rmin1 + Group*Cmin5rmin2 + Group*Cmin5rmin3 + Group*Cmin5rmin4 + Group*Cmin5rmin5 +
    Group*Cmin6 + Group*rmin6 +
    Group*Cmin1rmin6 + Group*Cmin2rmin6 + Group*Cmin3rmin6 + Group*Cmin4rmin6 + Group*Cmin5rmin6 +
    Group*Cmin6rmin1 + Group*Cmin6rmin2 + Group*Cmin6rmin3 + Group*Cmin6rmin4 + Group*Cmin6rmin5 + Group*Cmin6rmin6 +
    (1 | ID),
  data = bdf,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


# check coeff matrix
Az_g <- coef(summary(A_reg_all5_g))[,"z value"]
Bz_g <- coef(summary(A_reg_all5_g))[,"z value"]
Cz_g <- coef(summary(A_reg_all5_g))[,"z value"]
all_z_g <- (Az_g + Bz_g + Cz_g)/3

all5_g = matrix(data = all_z_g[40:114],nrow = 3,ncol = 25)
all5ctrl = matrix(data = all5_g[1,],nrow = 5,ncol = 5)
corrplot(all5ctrl,cl.lim = c(-6, 6),is.corr = FALSE, method = "color", title = 'controls vs. attempters', mar=c(0,0,1,0))
all5dep = matrix(data = all5_g[2,],nrow = 5,ncol = 5)
corrplot(all5dep,cl.lim = c(-6, 6),is.corr = FALSE, method = "color", title = 'NS depressed vs. attempters', mar=c(0,0,1,0))
all5id = matrix(data = all5_g[3,],nrow = 5,ncol = 5)
corrplot(all5id,cl.lim = c(-6, 6),is.corr = FALSE, method = "color", title = 'ideators vs. attempters', mar=c(0,0,1,0))
all5_att_vs_rest <- (all5ctrl + all5dep + all5id)/3
colnames(all5_att_vs_rest) <- c("r(t-1)","r(t-2)", "r(t-3)","r(t-4)","r(t-5)")
rownames(all5_att_vs_rest) <- c("choice(t-1)","choice(t-2)", "choice(t-3)","choice(t-4)","choice(t-5)")
corrplot(all5_att_vs_rest,cl.lim = c(-3, 3),is.corr = FALSE, method = "color", title = 'all vs. attempters', mar=c(0,0,1,0))
d <- diag(all5_att_vs_rest)
off <- all5_att_vs_rest[all5_att_vs_rest!=diag(all5_att_vs_rest)]
mean(off)
t.test(d,off)