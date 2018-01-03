#  Alex's tidyverse version of Vanessa's credit assignment models
#  run bandit_beh_analyses.R and make_design_crdt_assignment.R first
load(file = "~/Box Sync/skinner/projects_analyses/Project Bandit/R/bandit3.RData")
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/credit_predictors")

require(lme4)
require(corrplot)

## for original sample
# df <- bdf
# replic <- "fMRI"
# 
# ## for replication
df <- rdf
replic <- "replication_"


#  with predictor
# vars <- c("age","education", "EXITtot", "WTAR_SCALED_SCORE", "DRS_TOTAL", "SPSI_ICSSUB", "BIS_TOTALMEAN", "UPPSPNEGURGENCY", "UPPSPPOSURGENCY", "UPPSPLACKOFPREMED","UPPSPLACKOFPERSEV")
vars <- c("WTAR_SCALED_SCORE")

# vars <- c( "DRS_TOTAL", "SPSI_ICSSUB", "BIS_TOTALMEAN", "UPPSPNEGURGENCY", "UPPSPPOSURGENCY", "UPPSPLACKOFPREMED","UPPSPLACKOFPERSEV")

for(i in 1:length(vars))
{varname <- vars[i]
print(varname)
df$predictor <- as.numeric(as.matrix(df[,varname]))

A_reg_all5_g = glmer(
  A ~
    predictor*Amin1 + predictor*Amin2 + predictor*Amin3 + predictor*Amin4 + predictor*Amin5 +
    predictor*rmin1 + predictor*rmin2 + predictor*rmin3 + predictor*rmin4 + predictor*rmin5 +
    predictor*Amin1rmin1 + predictor*Amin1rmin2 + predictor*Amin1rmin3 + predictor*Amin1rmin4 + predictor*Amin1rmin5 +
    predictor*Amin2rmin1 + predictor*Amin2rmin2 + predictor*Amin2rmin3 + predictor*Amin2rmin4 + predictor*Amin2rmin5 +
    predictor*Amin3rmin1 + predictor*Amin3rmin2 + predictor*Amin3rmin3 + predictor*Amin3rmin4 + predictor*Amin3rmin5 +
    predictor*Amin4rmin1 + predictor*Amin4rmin2 + predictor*Amin4rmin3 + predictor*Amin4rmin4 + predictor*Amin4rmin5 +
    predictor*Amin5rmin1 + predictor*Amin5rmin2 + predictor*Amin5rmin3 + predictor*Amin5rmin4 + predictor*Amin5rmin5 +
    predictor*Amin6 + predictor*rmin6 +
    predictor*Amin1rmin6 + predictor*Amin2rmin6 + predictor*Amin3rmin6 + predictor*Amin4rmin6 + predictor*Amin5rmin6 +
    predictor*Amin6rmin1 + predictor*Amin6rmin2 + predictor*Amin6rmin3 + predictor*Amin6rmin4 + predictor*Amin6rmin5 + predictor*Amin6rmin6 +
    
    (1 | ID),
  data = df,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


B_reg_all5_g = glmer(
  B ~
    predictor*Bmin1 + predictor*Bmin2 + predictor*Bmin3 + predictor*Bmin4 + predictor*Bmin5 +
    predictor*rmin1 + predictor*rmin2 + predictor*rmin3 + predictor*rmin4 + predictor*rmin5 +
    predictor*Bmin1rmin1 + predictor*Bmin1rmin2 + predictor*Bmin1rmin3 + predictor*Bmin1rmin4 + predictor*Bmin1rmin5 +
    predictor*Bmin2rmin1 + predictor*Bmin2rmin2 + predictor*Bmin2rmin3 + predictor*Bmin2rmin4 + predictor*Bmin2rmin5 +
    predictor*Bmin3rmin1 + predictor*Bmin3rmin2 + predictor*Bmin3rmin3 + predictor*Bmin3rmin4 + predictor*Bmin3rmin5 +
    predictor*Bmin4rmin1 + predictor*Bmin4rmin2 + predictor*Bmin4rmin3 + predictor*Bmin4rmin4 + predictor*Bmin4rmin5 +
    predictor*Bmin5rmin1 + predictor*Bmin5rmin2 + predictor*Bmin5rmin3 + predictor*Bmin5rmin4 + predictor*Bmin5rmin5 +
    predictor*Bmin6 + predictor*rmin6 +
    predictor*Bmin1rmin6 + predictor*Bmin2rmin6 + predictor*Bmin3rmin6 + predictor*Bmin4rmin6 + predictor*Bmin5rmin6 +
    predictor*Bmin6rmin1 + predictor*Bmin6rmin2 + predictor*Bmin6rmin3 + predictor*Bmin6rmin4 + predictor*Bmin6rmin5 + predictor*Bmin6rmin6 +
    (1 | ID),
  data = df,
  family = binomial,
  nAGQ=0,

  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)

C_reg_all5_g = glmer(
  C ~
    predictor*Cmin1 + predictor*Cmin2 + predictor*Cmin3 + predictor*Cmin4 + predictor*Cmin5 +
    predictor*rmin1 + predictor*rmin2 + predictor*rmin3 + predictor*rmin4 + predictor*rmin5 +
    predictor*Cmin1rmin1 + predictor*Cmin1rmin2 + predictor*Cmin1rmin3 + predictor*Cmin1rmin4 + predictor*Cmin1rmin5 +
    predictor*Cmin2rmin1 + predictor*Cmin2rmin2 + predictor*Cmin2rmin3 + predictor*Cmin2rmin4 + predictor*Cmin2rmin5 +
    predictor*Cmin3rmin1 + predictor*Cmin3rmin2 + predictor*Cmin3rmin3 + predictor*Cmin3rmin4 + predictor*Cmin3rmin5 +
    predictor*Cmin4rmin1 + predictor*Cmin4rmin2 + predictor*Cmin4rmin3 + predictor*Cmin4rmin4 + predictor*Cmin4rmin5 +
    predictor*Cmin5rmin1 + predictor*Cmin5rmin2 + predictor*Cmin5rmin3 + predictor*Cmin5rmin4 + predictor*Cmin5rmin5 +
    predictor*Cmin6 + predictor*rmin6 +
    predictor*Cmin1rmin6 + predictor*Cmin2rmin6 + predictor*Cmin3rmin6 + predictor*Cmin4rmin6 + predictor*Cmin5rmin6 +
    predictor*Cmin6rmin1 + predictor*Cmin6rmin2 + predictor*Cmin6rmin3 + predictor*Cmin6rmin4 + predictor*Cmin6rmin5 + predictor*Cmin6rmin6 +
    (1 | ID),
  data = df,
  family = binomial,
  nAGQ=0,
  glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))
)


# check coeff matrix
Az_p <- coef(summary(A_reg_all5_g))[,"z value"]
Bz_p <- coef(summary(B_reg_all5_g))[,"z value"]
Cz_p <- coef(summary(C_reg_all5_g))[,"z value"]

# all_z_p <- (Az_p + Bz_p + Cz_p)/3
all_z_p <- Az_p * mean(na.omit(df$choiceA)) + Bz_p * mean(na.omit(df$choiceB))  + Cz_p  * mean(na.omit(df$choiceC))

all5_p = matrix(data = all_z_p[61:85],nrow = 5,ncol = 5,byrow = TRUE)
colnames(all5_p) <- c("r(t-1)","r(t-2)", "r(t-3)","r(t-4)","r(t-5)")
rownames(all5_p) <- c("choice(t-1)","choice(t-2)", "choice(t-3)","choice(t-4)","choice(t-5)")
# all5dep = matrix(data = all5_g[2,],nrow = 5,ncol = 5)
# corrplot(all5dep,cl.lim = c(-6, 6),is.corr = FALSE, method = "color", title = 'NS depressed vs. attempters', mar=c(0,0,1,0))
# all5id = matrix(data = all5_g[3,],nrow = 5,ncol = 5)
# corrplot(all5id,cl.lim = c(-6, 6),is.corr = FALSE, method = "color", title = 'ideators vs. attempters', mar=c(0,0,1,0))
# all5_att_vs_rest <- (all5ctrl + all5dep + all5id)/3
# corrplot(all5_att_vs_rest,cl.lim = c(-3, 3),is.corr = FALSE, method = "color", title = 'all vs. attempters', mar=c(0,0,1,0))
d <- diag(all5_p)
off <- all5_p[all5_p!=diag(all5_p)]
backward <- all5_p[lower.tri(all5_p,diag = FALSE)]
forward <- all5_p[upper.tri(all5_p,diag = FALSE)]
mean(d)
mean(off)
mean(backward)
mean(forward)
t.test(d)
t.test(backward)
t.test(forward)
filename <- paste0(replic,varname,"_credit.pdf")
pdf(file = filename,width = 8, height = 8)
corrplot(all5_p,cl.lim = c(-10, 10),is.corr = FALSE, method = "color",addCoef.col="black", title = c(varname), mar=c(0,0,1,0))
dev.off()
}