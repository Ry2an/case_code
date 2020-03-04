####Info####
# BIOS 621 Assignment 3
# Ruoyan Han 23831597

####import libraries####
library(survival)
library(survminer)

####data process####
#import data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
tb <- readxl::read_excel("journal.pone.0122587.s002.XLSX")
colnames(tb) <- make.names(colnames(tb))

#clean data
tb$time <- tb$Exp.wk.48
tb$cens <- tb$Exp.48 == "Yes"
tb$rand_p <- factor(tb$Random, levels=1:3, labels=c("week1", "week4", "week8"))
tb$rand <- relevel(tb$rand_p, ref="week8")
tb$cd4c <- factor(ifelse(tb$CD4.0 < 50, 0, 1))
levels(tb$cd4c) <- c("<50", ">=50")
tb$cd4c <- relevel(tb$cd4c, ref = ">=50")
tb$aluc <- factor(ifelse(tb$Albumin < 3, 0, 1))
levels(tb$aluc) <- c("<3", ">=3")
tb$aluc <- relevel(tb$aluc, ref = ">=3")

#remove not start cART
use_data <- tb[tb$HAART != "Not",]
use_data <- tb
use_data <- data.frame(
  use_data$PID,
  use_data$time,
  use_data$cd4c,
  use_data$aluc,
  use_data$rand,
  use_data$cens,
  use_data$BMI,
  use_data$rand_p
)
names(use_data) <- c("id", "time", "cd4c", "aluc", "random", "censor", "bmi", "treatment")

use_data$week1yn <- factor(ifelse(use_data$random == "week1", 1, 0))
use_data$week4yn <- factor(ifelse(use_data$random == "week4", 1, 0))
levels(use_data$week1yn) <- c("Yes", "No")
levels(use_data$week4yn) <- c("Yes", "No")
use_data$week1yn <- relevel(use_data$week1yn, ref = "No")
use_data$week4yn <- relevel(use_data$week4yn, ref = "No")

# show discrepance with data in paper 
c(summary(use_data$random)[2], summary(use_data$random)[3], summary(use_data$random)[1])

####Kaplan Meier and log rank####
fit_km <- survfit(Surv(time, censor) ~ treatment, data = use_data)
summary(fit_km)
a <- survdiff(Surv(time, censor) ~ treatment, data = use_data)

a$n
a$obs
a$exp
((a$obs - a$exp)^2)/a$exp
((a$obs - a$exp)^2)/c(a$var[1,1], a$var[2,2], a$var[3,3])

a$chisq
pchisq(a$chisq, df=2, lower.tail=FALSE)
outout_chi <- data.frame(
  c("week 1", "week4", "week8"),
  a$n,
  a$obs,
  a$exp,
  ((a$obs - a$exp)^2)/a$exp,
  ((a$obs - a$exp)^2)/c(a$var[1,1], a$var[2,2], a$var[3,3]),
  c(a$chisq, pchisq(a$chisq, df=2, lower.tail=FALSE), 0)
)
write.csv(outout_chi, "chi_out.csv", row.names = F)

plot(fit_km)
ggsurvplot(fit_km, xlab = "time", ylab = "Cumulative Survival Probability", risk.table = T)

####univariate analysis####
fit_t <- coxph(Surv(time, censor) ~ random, data = use_data)
fit_c <- coxph(Surv(time, censor) ~ cd4c, data = use_data)
fit_a <- coxph(Surv(time, censor) ~ aluc, data = use_data)
fit_b <- coxph(Surv(time, censor) ~ bmi, data = use_data)

summary(fit_t)
summary(fit_c)
summary(fit_a)
summary(fit_b)

temp <- summary(fit_t)
temp$coefficients
temp$conf.int


####multivariable analysis####
fit_all <- coxph(Surv(time, censor) ~ random + cd4c + aluc + bmi, data = use_data)
fit_all_p <- coxph(Surv(time, censor) ~ week1yn + week4yn + cd4c + aluc + bmi, data = use_data)
####diagnose####
par(mfcol = c(2,3))
cox.zph(fit_all)
par(mfcol = c(2,3))
plot(cox.zph(fit_all_p))
par(mfcol = c(2,3))
plot(cox.zph(fit_all_p)[1], main = "week1 vs week8(ref)")
plot(cox.zph(fit_all_p)[2], main = "week4 vs week8(ref)")
plot(cox.zph(fit_all_p)[3], main = "CD4 <50 vs >=50(ref)")
plot(cox.zph(fit_all_p)[4], main = "Albumin <3 vs >=3(ref)")
plot(cox.zph(fit_all_p)[5], main = "BMI")
summary(use_data$random)

####residuals####
par(mar = c(5, 5, 1, 1))
plot(use_data$time, resid(fit_all, type = "martingale"), xlab = "Time", ylab = "Martingale Residuals", las = 1)
abline(h = 0, lty = 2)
lines(lowess(use_data$time, resid(fit_all)), col = "red")

plot(use_data$time, resid(fit_all, type = "deviance"), xlab = "Time", ylab = "Deviance Residuals", las = 1)
abline(h = 0, lty = 2)
lines(lowess(use_data$time, resid(fit_all)), col = "red")

sresids <- residuals(fit_all, type = "scaledsch")
colnames(sresids) <- names(fit_all$coefficients)
time <- as.numeric(rownames(sresids))
par(mar = c(5, 5, 1, 1))
plot(time, sresids[, 1], xlab = "Time", ylab = "Scaled Schoenfeld Residual")
lines(smooth.spline(time, sresids[, 1]), col = "red", lwd = 2)

plot(time, sresids[, 2], xlab = "Time", ylab = "Scaled Schoenfeld Residual")
lines(smooth.spline(time, sresids[, 2]), col = "red", lwd = 2)

plot(time, sresids[, 3], xlab = "Time", ylab = "Scaled Schoenfeld Residual")
lines(smooth.spline(time, sresids[, 3]), col = "red", lwd = 2)

plot(time, sresids[, 4], xlab = "Time", ylab = "Scaled Schoenfeld Residual")
lines(smooth.spline(time, sresids[, 4]), col = "red", lwd = 2)

plot(time, sresids[, 5], xlab = "Time", ylab = "Scaled Schoenfeld Residual")
lines(smooth.spline(time, sresids[, 5]), col = "red", lwd = 2)

summary(fit_all_p)

summary(fit_all)
