# {artemis} modeling GVL session 1
# M. Johnston
# Mon Aug 23 11:08:29 2021 ------------------------------
library(artemis)
cores = getOption("mc.cores")

lnconc_to_cq = function(lneDNA,
                        beta, 
                        alpha) {
  cq = lneDNA*beta + alpha
  return(cq)
}

# exploratory analysis
head(eDNA_data)
d = eDNA_data

d$ln_conc = cq_to_lnconc(Cq_values = d$Cq, 
                         std_curve_alpha = d$StdCrvAlpha_lnForm, 
                         std_curve_beta = d$StdCrvBeta_lnForm)

plot(d$ln_conc)

# Single predictor variable:

m1 = eDNA_lm(Cq ~ 1,
             data = eDNA_data,
             std_curve_alpha = eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1])

summary(m1)
mean(d$ln_conc)

d2 = readRDS("~/NonDropboxRepos/Expt5_design/data/all_unidirectional_expts.rds")
d2$Distance = as.numeric(d2$Distance)

#d2 = subset(d2, Experiment == "delta4")

ggplot(d2, aes(x = Distance, y = Cq)) +
  geom_jitter(aes(color = factor(Volume)), size = 1, alpha = 0.75)

m1.5 = eDNA_lm(Cq ~ 1,
               data = d2,
               d$StdCrvAlpha_lnForm[1],
               d$StdCrvBeta_lnForm[1],
               cores = 4L
               )

summary(m1.5)

#-------------------------------------------------------#
# with small dataset, what is the effect of distance?
m2 = eDNA_lm(Cq ~ Distance_m,
             data = d,
             std_curve_alpha = eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1],
             seed = 20)

summary(m2)

#-------------------------------------------------------#
# Larger dataset, how does the effect of distance change?

m2_big = eDNA_lm(Cq ~ Distance,
             data = d2,
             std_curve_alpha = eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1],
             seed = 20)

summary(m2_big)

lnconc_to_cq(3.77, 
             -1.53, 
             21.1)


#-------------------------------------------------------#
# Two predictors
m3 = eDNA_lm(Cq ~ Distance_m + Volume_mL,
             data = d,
             std_curve_alpha = eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1],
             seed = 20)

summary(m3)

# Two predictors, big dataset
m3_big = eDNA_lm(Cq ~ Distance + Volume,
             data = d2,
             std_curve_alpha = eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1],
             seed = 20)

summary(m3_big)

# Mixed effects models
#-------------------------------------------------------#

m4 = eDNA_lmer(Cq ~ Distance_m + 
                 Volume_mL + 
                 (1|FilterID),
               data = d,
               eDNA_data$StdCrvAlpha_lnForm[1],
             std_curve_beta = eDNA_data$StdCrvBeta_lnForm[1],
             seed = 20
               )

summary(m3)
summary(m4)
