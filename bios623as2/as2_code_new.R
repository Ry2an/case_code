####info####
#BIOS 621 Assignment 2 code
#Ruoyan Han 23831597

#import packages
library(MASS)
library(ggplot2)
library(plotrix)
library(plyr)
library(countreg)
library(gridExtra)
####data processs####
#import data
setwd("D:\\CUNY\\BIOS621\\Assignment2")
# setwd("/Users/hanruoyan/Google Drive/Daily_file/Oct/1022")
raw_data <- read.csv("AUdeaths_curated.csv", header = T)

#change title (add row_number)
names_raw <- names(raw_data)
names_raw[1] <- "row_number"
names(raw_data) <- names_raw

#add law value
vec_law <- rep(0, dim(raw_data)[1])
for(i in 1:dim(raw_data)[1]){
  if(raw_data$year[i] <= 1996){
    vec_law[i] <- 0
  }else{
    vec_law[i] <- 1
  }
}

fac_law <- factor(vec_law)
levels(fac_law) <- c("Before", "After")
fac_law <- relevel(fac_law, ref = "Before")

raw_data <- data.frame(raw_data, fac_law, vec_law)

#generate total no firearm and total death
for(i in 1:dim(raw_data)[1]){
  raw_data$tnf[i] <- raw_data$suicidenotgun[i] + raw_data$homocidenotgun[i]
  raw_data$td[i] <- raw_data$suicidetotal[i] + raw_data$homocidetotal[i]
}

#generate rate
for(i in 1:dim(raw_data)[1]){
  raw_data$rate[i] <- (raw_data$tnf[i] / raw_data$personyearsatrisk[i]) * 100000
}

#generate use_data
use_data <- data.frame(raw_data[,c("year", "fac_law", "tnf", "td", "rate", "vec_law", "personyearsatrisk")])
head(use_data, 2)
summary(use_data)

#generate centered year
use_data$year_s <- use_data$year - 1996

####descriptive####
#mean and sd of rate
log(mean(use_data$rate))
log(sd(use_data$rate))

mean_b <- mean(use_data$rate[use_data$fac_law == "Before"])
sd_b <- sd(use_data$rate[use_data$fac_law == "Before"])
log(mean_b)
log(sd_b)

mean_a <- mean(use_data$rate[use_data$fac_law == "After"])
sd_a <- sd(use_data$rate[use_data$fac_law == "After"])
log(mean_a)
log(sd_a)

# result: not same

####fit models####
####~~~see ave before and after####
#solution1
upper_b <- qt(0.975, df = (dim(use_data)[1] - 1) * sd(use_data$tnf[use_data$fac_law == "Before"]) / sqrt(dim(use_data)[1]))
lower_b <- qt(0.025, df = (dim(use_data)[1] - 1) * sd(use_data$tnf[use_data$fac_law == "After"]) / sqrt(dim(use_data)[1]))
before_mean_95_s1 <- c(mean_b, mean_b + lower_b, mean_b + upper_b)

upper_a <- qt(0.975, df = (dim(use_data)[1] - 1) * sd(use_data$tnf[use_data$fac_law == "Before"]) / sqrt(dim(use_data)[1]))
lower_a <- qt(0.025, df = (dim(use_data)[1] - 1) * sd(use_data$tnf[use_data$fac_law == "After"]) / sqrt(dim(use_data)[1]))
after_mean_95_s1 <- c(mean_a, mean_a + lower_a, mean_a + upper_a)


#solution2
fit_mean_s2 <- lm(rate ~ 1 + fac_law, data = use_data)
summary(fit_mean_s2)
confint(fit_mean_s2)


before_mean_95_s2 <- c(summary(fit_mean_s2)$coefficients[1, 1],
                       summary(fit_mean_s2)$coefficients[1, 1] - summary(fit_mean_s2)$coefficients[1, 2] * 2.042,
                       summary(fit_mean_s2)$coefficients[1, 1] + summary(fit_mean_s2)$coefficients[1, 2] * 2.042)

after_mean_95_s2 <- c(summary(fit_mean_s2)$coefficients[1, 1] + summary(fit_mean_s2)$coefficients[2, 1],
                      summary(fit_mean_s2)$coefficients[1, 1] + summary(fit_mean_s2)$coefficients[2, 1] - summary(fit_mean_s2)$coefficients[1, 2] * 2.042,
                      summary(fit_mean_s2)$coefficients[1, 1] + summary(fit_mean_s2)$coefficients[2, 1] + summary(fit_mean_s2)$coefficients[1, 2] * 2.042)

#solution3
fit_mean_s3 <- glm.nb(tnf ~ 1 + fac_law + offset(log(personyearsatrisk)), data = use_data)
summary(fit_mean_s3)$coefficient
confint(fit_mean_s3)

before_mean_95_s3 <- c(exp(summary(fit_mean_s3)$coefficients[1, 1]) * 100000,
                       exp(summary(fit_mean_s3)$coefficients[1, 1] - 
                             summary(fit_mean_s3)$coefficients[1, 2] * 1.96) * 100000,
                       exp(summary(fit_mean_s3)$coefficients[1, 1] + 
                             summary(fit_mean_s3)$coefficients[1, 2] * 1.96) * 100000)

after_mean_95_s3 <- c(exp(summary(fit_mean_s3)$coefficients[1, 1] + summary(fit_mean_s3)$coefficients[2, 1]) * 100000,
                      exp(summary(fit_mean_s3)$coefficients[1, 1] + summary(fit_mean_s3)$coefficients[2, 1] - 
                            summary(fit_mean_s3)$coefficients[1, 2] * 1.96) * 100000,
                      exp(summary(fit_mean_s3)$coefficients[1, 1] + summary(fit_mean_s3)$coefficients[2, 1] + 
                            summary(fit_mean_s3)$coefficients[1, 2] * 1.96) * 100000)

mean_plot_mean <- c(before_mean_95_s1[1],
                    before_mean_95_s2[1],
                    before_mean_95_s3[1],
                    after_mean_95_s1[1],
                    after_mean_95_s2[1],
                    after_mean_95_s3[1])

mean_plot_low <- c(before_mean_95_s1[2],
                   before_mean_95_s2[2],
                   before_mean_95_s3[2],
                   after_mean_95_s1[2],
                   after_mean_95_s2[2],
                   after_mean_95_s3[2])

mean_plot_up <- c(before_mean_95_s1[3],
                  before_mean_95_s2[3],
                  before_mean_95_s3[3],
                  after_mean_95_s1[3],
                  after_mean_95_s2[3],
                  after_mean_95_s3[3])

mean_plot_points <- c(mean_plot_mean, mean_plot_low, mean_plot_up)
mean_plot_shape <- c(rep("mean", 6), rep("lower bound 95%CI", 6), rep("upper bound 95%CI", 6))
mean_plot_color <- factor(rep(c(1, 2, 3, 4, 5, 6), 3))
mean_plot_type <- factor(rep(c(1, 2, 3), 6))
levels(mean_plot_color) <- c("solution 1 before", "solution 2 before", "solution 3 before", 
                             "solution 1 after", "solution 2 after", "solution 3 after")

mean_plot <- data.frame(mean_plot_points, mean_plot_shape, mean_plot_color, mean_plot_type)
names(mean_plot) <- c("mean_and_95CI", "point_type", "solution_time", "solution_order")

#plot_1
ggplot(mean_plot, aes(x = solution_order, y = mean_and_95CI, color = solution_time, shape = point_type)) + 
  geom_point(size = 4) + xlab("Solution Type") + ylab("Mean and 95% Confidence Interval") + 
  ggtitle("Three solutions to calculate mean and confidence interval")

####~~~seperate analysis (model a and b)####
fit_a <- glm.nb(tnf ~ 1 + year + offset(log(personyearsatrisk)), data = use_data[use_data[,"fac_law"] == "Before",])
summary(fit_a)
confint(fit_a)

fit_b <- glm.nb(tnf ~ 1 + year + offset(log(personyearsatrisk)), data = use_data[use_data[,"fac_law"] == "After",])
summary(fit_b)
confint(fit_b)

####~~~negative binomial model####
fit_nb <- glm.nb(tnf ~ 1 + I(year-1996) + fac_law + I(year-1996):fac_law + offset(log(personyearsatrisk)), data = use_data)
summary(fit_nb)$coefficients
confint(fit_nb)

####~~~poisson model####
fit_poi <- glm(tnf ~ 1 + I(year-1996) + fac_law + I(year-1996):fac_law + offset(log(personyearsatrisk)), data = use_data, family = poisson(link = "log"))
summary(fit_poi)
exp(summary(fit_poi)$coefficients)
confint(fit_poi)

####plotting####
####~~~baisc description####
#plot_2
par(mfrow = c(1, 2))
hist(use_data$rate[use_data$fac_law == "Before"], main = "Before the Law Change", 
     xlab = "Total non-firearm death rate", xlim = c(5, 15), ylim = c(0, 8), breaks = 5)
hist(use_data$rate[use_data$fac_law == "After"], main = "After the Law Change", 
     xlab = "Total non-firearm death rate", xlim = c(5, 15), ylim = c(0, 8), breaks = 5)


####~~~mean trend####
#solution 1
#plot_3 no need any more
par(mfrow = c(1,1))
plot(use_data$rate~use_data$year)
lines(use_data$year[order(use_data$year)], use_data$rate[order(use_data$year)])
abline(v = 1996)

ablineclip(lm(use_data$rate[use_data$year <= 1996]~use_data$year[use_data$year <= 1996]),x1=1979,x2=1996,col="red",lwd=2.0)
ablineclip(lm(use_data$rate[use_data$year >= 1997]~use_data$year[use_data$year >= 1997]),x1=1997,x2=2013,col="blue",lwd=2.0)

#solution 2
#plot_3 no need any more
plot_year <- use_data$year
plot_rate <- c()
plot_ori <- rep(0, 2 * dim(use_data)[1])

plot_result <- lm(rate ~ year + fac_law + year:fac_law, data = use_data)
summary(plot_result)
for(i in 1:dim(use_data)[1]){
  if(plot_year[i] <= 1996){
    plot_rate[i] <- (- 429.45547 + 0.22141 * use_data$year[i])
    plot_ori[i + dim(use_data)[1]] <- 1
  }else{
    plot_rate[i] <- (- 429.45547 + 780.35092 + (0.22141 - 0.39054) * use_data$year[i])
    plot_ori[i + dim(use_data)[1]] <- 2
  }
}
final_plot_year <- c(use_data$year, plot_year)
final_plot_rate <- c(use_data$rate, plot_rate)
plot_data <- data.frame(final_plot_year, final_plot_rate, factor(plot_ori))
names(plot_data) <- c("year", "rate", "ori")
final_plot <- ddply(plot_data, c("ori", "year"), summarise, rate = mean(rate))
ggplot(final_plot, aes(x = year, y = rate, 
                       color = ori, shape = ori)) + geom_line(size = 1.5) + 
  geom_point(size = 2.5) +
  scale_color_manual(values = c("black","red","blue"))


####~~~model plotting####
#negative binomial
summary(fit_nb)
plot_year_nb <- seq(from = 1979, to = 2013, by = 0.1)
plot_rate_nb <- rep(0, length(plot_year_nb))
plot_law_nb <- c(rep(1, length(seq(from = 1979, to = 1996.9, by = 0.1))), 
                 rep(2, length(seq(from = 1997, to = 2013, by = 0.1)))
)
plot_ori_nb <- c(rep("Before Law Change", length(seq(from = 1979, to = 1996.9, by = 0.1))), 
                 rep("After Law Change", length(seq(from = 1997, to = 2013, by = 0.1)))
)

for(i in 1:length(plot_year_nb)){
  plot_rate_nb[i] <- exp(-8.977601 + 0.021154 * (plot_year_nb[i] - 1996) + (plot_law_nb[i] - 1) * 0.052847 -
                           0.034787 * (plot_year_nb[i] - 1996) * (plot_law_nb[i] - 1)) * 100000
}
final_plot_year_nb <- c(use_data$year, plot_year_nb)
final_plot_rate_nb <- c(use_data$rate, plot_rate_nb)
final_plot_color_nb <- c(rep("Raw rate", dim(use_data)[1]), plot_ori_nb)
plot_data_nb <- data.frame(final_plot_year_nb, final_plot_rate_nb, factor(final_plot_color_nb))
final_plot_nb <- ddply(plot_data_nb, c("final_plot_color_nb", "final_plot_year_nb"), summarise, rate = mean(final_plot_rate_nb))
nb_plot <- ggplot(plot_data_nb, aes(x = final_plot_year_nb, y = final_plot_rate_nb, 
                         color = final_plot_color_nb)) + geom_line(size = 1.5) +
  scale_color_manual(values = c("red","blue","black")) +
  xlab("Year") + ylab("Total Non-Firearm Deaths") + ggtitle("Log Negative Binomial Regression") + 
  labs(color = "Line Type")
  # + geom_smooth()
  # stat_smooth()

#poisson
summary(fit_poi)
plot_year_poi <- seq(from = 1979, to = 2013, by = 0.1)
plot_rate_poi <- rep(0, length(plot_year_nb))
plot_law_poi <- c(rep(1, length(seq(from = 1979, to = 1996.9, by = 0.1))), 
                  rep(2, length(seq(from = 1997, to = 2013, by = 0.1)))
)
plot_ori_poi <- c(rep("Before Law Change", length(seq(from = 1979, to = 1996.9, by = 0.1))), 
                  rep("After Law Change", length(seq(from = 1997, to = 2013, by = 0.1)))
)

for(i in 1:length(plot_year_poi)){
  plot_rate_poi[i] <- exp(-8.980947 + 0.020727 * (plot_year_poi[i] - 1996) + (plot_law_poi[i] - 1) * 0.056346 -
                            0.034332 * (plot_year_poi[i] - 1996) * (plot_law_poi[i] - 1)) * 100000
}
final_plot_year_poi <- c(use_data$year, plot_year_poi)
final_plot_rate_poi <- c(use_data$rate, plot_rate_poi)
final_plot_color_poi <- c(rep("Raw rate", dim(use_data)[1]), plot_ori_poi)
plot_data_poi <- data.frame(final_plot_year_poi, final_plot_rate_poi, factor(final_plot_color_poi))
poi_plot <- ggplot(plot_data_poi, aes(x = final_plot_year_poi, y = final_plot_rate_poi, 
                          color = final_plot_color_poi)) + geom_line(size = 1.5) +
  scale_color_manual(values = c("red","blue","black")) + 
  xlab("Year") + ylab("Total Non-Firearm Deaths") + ggtitle("Log Poisson Regression") + 
  labs(color = "Line Type")

#plot_4
grid.arrange(nb_plot, poi_plot, ncol = 2)

#plot_5 plot_6
par(mfrow=c(2, 2))
plot(fit_nb, which=c(1,2,3,4))
par(mfrow=c(2, 2))
plot(fit_poi, which=c(1,2,3,4))

par(mfrow=c(2, 4))
plot(fit_nb, which=c(1,2))
plot(fit_poi, which=c(1,2))
plot(fit_nb, which=c(3,4))
plot(fit_poi, which=c(3,4))




library(countreg)
par(mfrow = c(1, 2))
# rootogram(fit_a, style = "hanging", main = "nb", xlim = c(1100, 2200), breaks = 5)
# rootogram(fit_b, style = "hanging", main = "poi", xlim = c(1100, 2200), breaks = 5)
rootogram(fit_nb, style = "hanging", main = "Log Negative Binomial RegressionS", xlim = c(1100, 2200), breaks = 5, xlab = "Year")
rootogram(fit_poi, style = "hanging", main = "Log Poisson Regression", xlim = c(1100, 2200), breaks = 5, xlab = "Year")

exp(logLik(fit_nb))
exp(logLik(fit_poi))


