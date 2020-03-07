#3.1 Compute both nonparametric and parametric 95% interval estimates for the median of the granodiorite data of exercise 2.3. Which is more appropriate for these data? Why?
# Nonparametric Interval Estimate for the Median
Granodiorite <- c(6.0, 0.5, 0.4, 0.7, 0.8, 6.0, 5.0, 0.6, 1.2, 0.3, 0.2, 0.5, 0.5, 10, 0.2, 0.2, 1.7, 3.0)
n <- length(Granodiorite)
sorted <- sort(Granodiorite)
hist(sorted)
alpha <- 0.05
# A critical value x' is obtained from Table B5 corresponding to α/2
x1 <- 7
# The lower confidence limit
R1 <- x1 + 1
# The upper confidence limit
R2 <- n - x1
# Based on R1 = 8 and R2 = 11, the 95.6% confidence limit for C0.5 is the interval between the 8th and 18th ranked observations
low.non <- sorted[R1]
up.non <- sorted[R2]
low.non
up.non
# Parametric Interval Estimate for the Median
logdata <- log(sorted)
hist(logdata)
y <- length(logdata)
mean.y <- mean(logdata)
sd.y <- sd(logdata)
t <- qt(c(0.025, 0.975), 17)
# calculate the lower and upper boundary 
lim.para <- exp(mean.y + t * (sd.y/sqrt(y)))
lim.para
# From the calculation above, 0.506 <= log(median) <= 1.804 at α/2 = 0.025
# The nonparametric estimate is more appropriate since the original data or the log data doesn't show a no obvious normal distribution.

# 3.4 Construct the most appropriate 95 percent interval estimates for the mean and median annual streamflows for the Conecuh River at Brantley, Alabama (data in Appendix C2).
library(tidyverse)
streamflow <- read_csv('D:/Users/guoha/Desktop/ES_UCM/Spring_2020/ES_207/Documents/Tem/ES207_hw4/Annual_streamflows_for_the_Conecuh_River_AL.csv')
#Change working directory to 'ES207_hw4' and read in the specfic .csv file to shorten path name
#Preview data with head() to ensure correct data was loaded

hist(log(streamflow$`Flow (cfs)`))
# since the log histogram shows a roughly symmetrical image, we use parametric interval estimation
# median
s <- length(streamflow$`Flow (cfs)`)
sdata <- log(streamflow$`Flow (cfs)`)
mean.s <- mean(sdata)
sd.s <- sd(sdata)
t.s <- qt(c(0.025, 0.975), (s-1))
# calculate the lower and upper boundary 
lim.s <- exp(mean.s + t.s * (sd.s/sqrt(s)))
lim.s
# So the log(median) is in the range of 524.10 to 764.42 when alpha is 0.05
# mean
me <- qt(0.95,(s-1))*sd(sdata)/sqrt(s)
low <- mean(sdata)-me
up <- mean(sdata)+me
print(low)
print(up)
# so the true log(mean) is in the range of 6.29449 to 6.60631 when alpha is 0.05