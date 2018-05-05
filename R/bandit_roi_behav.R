## load Vanessa's ROI scores for behavioral and RT analyses
library(readr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(tibble)
library(xtable)
library(Hmisc)
# library(lsmeans)
library(emmeans)
library(ggfortify)
library(compareGroups)
library(RColorBrewer)
library(effects)
library(readr)
library(multcompView)
library(stargazer)
library(psych)
library(corrplot)


setwd("~/Box Sync/skinner/personal_folders/Vanessa")
load(file='all_lmer_data.Rdata')
setwd("~/Box Sync/skinner/projects_analyses/Project Bandit/R/finalized_samples/roi_zscored_dmublock1/")


# collinearity checks
vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  nam <- names(fixef(fit))
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }


# one row per subject for correlations
roi_df <- filter(roi_gdf,Trial==1)
sub_chars <- as.data.frame(roi_df[, c(49,51,58:68,71:78,175:178,181)])
# ggplot(sub_chars,aes(x = Group, y = vmPFC)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = func_vmPFC)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = RStr)) + geom_boxplot()
# ggplot(sub_chars,aes(x = Group, y = LStr)) + geom_boxplot()

chars <- sub_chars[,c(1:13,22:25)]
cors <- psych::corr.test(chars, use = "pairwise",method="pearson", alpha=.05)
pdf("bandit roi correlations.pdf", width=14, height=14)
corrplot(cors$r, cl.lim=c(-1,1),
         method = "circle", tl.cex = 1.5, type = "upper", tl.col = 'black',
         order = "hclust", diag = FALSE,
         addCoef.col="black", addCoefasPercent = FALSE,
         p.mat = cors$p, sig.level=0.05, insig = "blank")
dev.off()
# the only somewhat notable correlation is with EXIT (r= -0.19)

# does it impact exploitation?
summary(m_v1 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf + 
                        (1 | ID),
                      data = roi_gdf))
summary(m_v1)
car::Anova(m_v1,'3')
vif.lme(m_v1)
em1 <- emmeans(m_v1, "reinf",by = c("Group", "vmPFC"), at = list(vmPFC = c(-.02,0.03,0.08)))
plot(em1, horiz = F)
C <- cld(em1)
# colorful, but not necessarily more convincing:
ggplot(C, aes(x = vmPFC, y = emmean, ymin = asymp.LCL, ymax = asymp.UCL, color = Group)) + geom_errorbar(position = "dodge", width = .2)
  geom_errorbar(position = "dodge", width = .2) + facet_wrap(reinf~Group, ncol = 4)

summary(m_v1f <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(func_vmPFC) * reinf + 
                               (1 | ID),
                             data = roi_gdf))
car::Anova(m_v1f,'3')
vif.lme(m_v1f)
em1f <- emmeans(m_v1f, "reinf",by = c("Group", "func_vmPFC"), at = list(func_vmPFC = c(-.02,0.03,0.08)))
plot(em1, horiz = F)

summary(m_v2 <-   lme4::lmer(value_chosen_vba_mfx ~ Group * scale(vmPFC) * reinf + v_max_lag_mfx * reinf + v_ch_diff * reinf + L_vba_mfx * reinf +
                               (1 | ID),
                     data = roi_gdf))
vif.lme(m_v2)
