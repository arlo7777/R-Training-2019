#Introduction to R: Lecture 8

#Permutation Tests: we have a sample but know nothing about the population
#These are data collected by a student in a Minneapolis pub
#Do this many times, and plot a histogram of the results
#Reload our fabricated data as if it were real data

#Mathematical Statistics with Resampling and R
setwd("/Users/abhishekmalani/Desktop/Freshman Year/Math 23C/Data")
BW <- read.csv("Beerwings.csv"); head(BW,10)
sum(BW$Gender == "M");sum(BW$Gender == "F")
#Calculate the observed beer consumption difference by gender
MaleAvg <- sum(BW$Beer*(BW$Gender=="M"))/sum(BW$Gender=="M"); MaleAvg
FemaleAvg <- sum(BW$Beer*(BW$Gender=="F"))/sum(BW$Gender=="F"); FemaleAvg
observed <- MaleAvg - FemaleAvg; observed     #the men drank more beer

#Now replace Male with a random sample of 15 customers
Gender <- sample(BW$Gender); Gender   #permuted gender column
sum(Gender == "M")  #still 15 men but they will match up with random beer consumption
MaleAvg <- sum(BW$Beer*(Gender=="M"))/sum(Gender=="M"); MaleAvg
FemaleAvg <- sum(BW$Beer*(Gender=="F"))/sum(Gender=="F"); FemaleAvg
MaleAvg - FemaleAvg    #as likely to be negative or positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(BW$Gender); Gender   #permuted gender column
  MaleAvg <- sum(BW$Beer*(Gender=="M"))/sum(Gender=="M"); MaleAvg
  FemaleAvg <- sum(BW$Beer*(Gender=="F"))/sum(Gender=="F"); FemaleAvg
  diffs[i] <- MaleAvg - FemaleAvg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero
head(diffs)

hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue


#Example 2

library(readxl)
college <- read_excel("~/Desktop/Business Analytics/Data Files/CollegesandUniversities.xlsx", 
                      skip = 2)

libavg <- sum(college$`Median SAT`*(college$Type == "Lib Arts"))/sum(college$Type=="Lib Arts")
uniavg <- sum(college$`Median SAT`*(college$Type != "Lib Arts"))/sum(college$Type !="Lib Arts")
observed <- libavg - uniavg; observed

#Now replace Type with a random sample of the college types
Type <- sample(college$Type); Type   #permuted Type column
sum(Type == "Lib Arts")  #still 25 Lib Arts but they will match up with random median SAT 
libavg <- sum(college$`Median SAT`*(Type == "Lib Arts"))/sum(Type =="Lib Arts");libavg
uniavg <- sum(college$`Median SAT`*(Type != "Lib Arts"))/sum(Type !="Lib Arts");uniavg
libavg - uniavg   #as likely to be negative or positive
#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Type <- sample(college$Type); Type   #permuted Type column
  sum(Type == "Lib Arts")  #still 25 Lib Arts but they will match up with random median SAT 
  libavg <- sum(college$`Median SAT`*(Type == "Lib Arts"))/sum(Type =="Lib Arts");libavg
  uniavg <- sum(college$`Median SAT`*(Type != "Lib Arts"))/sum(Type !="Lib Arts");uniavg
  diffs[i] <- libavg - uniavg   #as likely to be negative or positive
}
mean(diffs) #should be close to zero
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- 1 - (sum(diffs >= observed)+1)/(N+1); pvalue

## Confidence Intervals ##

require(stats)

n     <- 20 ## Sample size
sigma <- 2  ## Known standard deviation
mu    <- 4  ## The mean of the normal distribution

## Generate our sample
sample <- rnorm(n, mean = mu, sd = sigma)  

## Look at our samples
hist(sample) 

stErr <- sigma/sqrt(n) ## Standard error of the mean
stErr

a = 0.05                    ## For our 95% CI
z_alpha <- qnorm(1 - a/2)   ## Use the inverse of the normal cumulative distribution function
error <- z_alpha*stErr      ## Multiply the z term and standard error

xBar <- mean(sample) ## Sample mean
xBar

left  <- xBar - error ## Left margin
right <- xBar + error ## Right margin

CI <- c(left, right)  ## Our confidence interval
CI

s <- sd(sample) ## Sample standard deviation

## Use the inverse of the Student-t cumulative distribution function, qt
## Degrees of freedom df = n - 1
t_alpha <- qt(1 - a/2, df=n-1) 

## Get the error term for the sample:
## Multiply the t term and standard error
e.sample <- t_alpha*s/sqrt(n)      

## Construct our confidence interval:
## Add the sample error to the mean
CI.sample <- c(xBar-e.sample, xBar+e.sample)
CI.sample

# Z-Test (Known Population SD)
mu_0 <- 4   ## Hypothesized mean

zStat <- (xBar - mu_0)/(sigma/sqrt(n))
zStat

#to find the p-value (notice this is half of alpha because it is only one side)
pnorm(zStat)

#visualizing what this does: finds area left of zStat
x <- seq(-3, 3, length=1000)
y <- dnorm(x, mean=0, sd=1)
plot(x, y, type="l", lwd=1)
abline(v = zStat, col = "red")

#T-Test (Unknown Population SD)
tStat <- (xBar - mu_0)/(s/sqrt(n))
tStat

curve(dt(x, n-1), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
legend("topleft", legend = paste0("DF = ", c(n-1)),
       col = c("orange"),
       lty = 1, lwd = 2)
abline(v=tStat, col = "red")

## For a one-sided hypothesis test:
## Use pt() for the Student's t probability distribution
## Use abs() to take the absolute value of the t test statistic
pVal.sample <- pt(-abs(tStat),df=length(sample)-1)
pVal.sample

## Use t.test to test the sample against the hypothesis that mu = 4
t.test(sample, mu = 4, alternative = "less") 

## Problem 1 ##

# The operations manager of a large production plant would like to estimate the mean amount
# of time a worker takes to assemble a new electronic component. Assume that the standard
# deviation of this assembly time is 3.6 minutes.
# a) After observing 120 workers assembling similar devices, the manager noticed that their 
# average time was 16.2 minutes. Construct a 92% confidence interval for the mean assembly time.
# b) How many workers should be involved in this study in order to have the mean assembly time
# estimated up to ±15 seconds with 92% confidence?

#Set Up





#Part A




 

#Part B  





## Problem 2 ##
# A car company claims that their Super Spiffy Sedan averages 31 mpg. 
# You randomly select 8 Super Spiffies from local car dealerships and test their
# gas mileage under similar conditions.
# 
# You get the following MPG scores:
#   
# MPG:	30	28	32	26	33	25	28	30
# Does the actual gas mileage for these cars deviate significantly from 31 (alpha = .05)?












