#Introduction to R: Lecture 9

## Confidence Intervals Continued##

## Problem 1 ##

# The operations manager of a large production plant would like to estimate the mean amount
# of time a worker takes to assemble a new electronic component. Assume that the standard
# deviation of this assembly time is 3.6 minutes.
# a) After observing 120 workers assembling similar devices, the manager noticed that their 
# average time was 16.2 minutes. Construct a 92% confidence interval for the mean assembly time.
# b) How many workers should be involved in this study in order to have the mean assembly time
# estimated up to ±15 seconds with 92% confidence?

#Set Up
sigma <- 3.6 #known population sd
n <- 120 #number of workers
mu <- 16.2 #observed sample mean
alpha <- 1-0.92 #level of confidence
sample <- rnorm(n,mu,sigma)  
hist(sample)

#Part A
stErr <- sigma/sqrt(n)
a = 0.08                    ## For our 92% CI
z_alpha <- qnorm(1 - a/2)   ## Use the inverse of the normal cumulative distribution function
error <- z_alpha*stErr      ## Multiply the z term and standard error

Left <- mu - error
Right <- mu + error
CI <- c(Left,Right)
CI  

#Part B  
error <- .25 #given 15 seconds == 0.25 min
(sigma/(error/z_alpha))^2

## Problem 2 ##
# A car company claims that their Super Spiffy Sedan averages 31 mpg. 
# You randomly select 8 Super Spiffies from local car dealerships and test their
# gas mileage under similar conditions.
# 
# You get the following MPG scores:
#   
# MPG:	30	28	32	26	33	25	28	30
# Does the actual gas mileage for these cars deviate significantly from 31 (alpha = .05)?


sample <- c(30,28,32,26,33,25,28,30)
sample

mu_0 <- 31 #claimed mpg
xBar <- mean(sample) #sample mean
s <- sd(sample) #sample sd
n <- 8 #number of cars samples

#T-Test (Unknown Population SD)
tStat <- (xBar - mu_0)/(s/sqrt(n))
tStat

curve(dt(x, n-1), from = -5, to = 5, col = "orange", 
      xlab = "quantile", ylab = "density", lwd = 2)
legend("topleft", legend = paste0("DF = ", c(n-1)),
       col = c("orange"),
       lty = 1, lwd = 2)
abline(v=tStat, col = "red")

## For a two-sided hypothesis test:
## Use pt() for the Student's t probability distribution
## Use abs() to take the absolute value of the t test statistic
pVal.sample <- 2*pt(-abs(tStat),df=length(sample)-1)
pVal.sample

## Use t.test to test the sample against the hypothesis that mu = 4
t.test(sample, mu = mu_0, alternative = "two.sided") 


## Two-Sample T-Test ##
require(ggplot2)

#uncomment if uninstalled
#install.packages("GGally") 
require(GGally)      ## For additional variable plots and parallel axis plots

data(iris)
head(iris)

## ggpairs from GGally
## ggpairs plot with density on diagonal, scatter plots with correlations, histograms + barplots
GGP.iris.dens <- ggpairs(iris, aes(colour = Species, alpha = 0.75))
GGP.iris.dens

ggplot(iris, aes()) + geom_histogram(aes(x=Sepal.Length, fill = Species), alpha = 0.5)
## Subset setosa
setosa <- iris[as.character(iris$Species)=="setosa",]
## Subset versicolor
versicolor <- iris[as.character(iris$Species)=="versicolor",]

## Compare the two distributions, with null hypothesis that the means of the populations are equal
t.test(setosa$Sepal.Length, versicolor$Sepal.Length, alternative = "two.sided")
#zero isn't even included in the 95% CI, so we can reject the null hypothesis with strong confidence

## Compare the two distributions, with null hypothesis that the variances of the populations are equal
var.test(setosa$Sepal.Length, versicolor$Sepal.Length, alternative = "two.sided")
# One (the ratio of the variances) is not included in the 95% CI, so we can reject the null hypothesis that the population variances are equal

#Chi-Squared Test of Independence
# The chi-square test for independence tests whether two categorical variables are independent:
#   H0: two categorical variables are independent 
#   H1: two categorical variables are dependent

data(diamonds)
head(diamonds)

chisq.test(diamonds$cut, diamonds$clarity)
#The results suggest that it is very unlikely that “cut” and “clarity” are independent;
#we can reject the null hypothesis with a high degree of certainty.

#Example 2 (with data cleaning) Credit: Paul Bamberg

setwd("/Users/abhishekmalani/Desktop/Freshman Year/Math 23C/data")

#Building and analyzing a data frame with nothing but logical columns
#Load the General Social Survey data set for 2002 (other years available online)
GSS<-read.csv("GSS2002.csv"); head(GSS)

#Identify the rows with non-empty entries for Gender, Marital, Religion, PolParty, OwnGun, and Pres00
index<-which(!is.na(GSS$Gender) & !is.na(GSS$Marital)& !is.na(GSS$Religion)&
               !is.na(GSS$PolParty)& !is.na(GSS$OwnGun)& !(GSS$OwnGun=="Refused") & 
               !is.na(GSS$Pres00) & ((GSS$Pres00 == "Bush")|(GSS$Pres00 == "Gore")))
#which() returns a vector of the rows that satisfy the given criterion

length(index)    #how many rows did we select?

GSSCleaned <- GSS[index,]

#Extract the six desired columns as vectors, keeping just the selected rows
Gender2 <-GSS$Gender[index] 
Marital2 <-GSS$Marital[index] 
Religion2 <-GSS$Religion[index] 
PolParty2 <-GSS$PolParty[index] 
OwnGun2 <-GSS$OwnGun[index] 
Pres002 <-GSS$Pres00[index] 

#Now we can use the vectorized == operator to create six logical columns
Male <- Gender2=="Male"
head(Gender2); head(Male)  #check that it is working
Married <- Marital2=="Married"
Protestant <- Religion2=="Protestant"

#Extracting party is tricky
PolParty2    #there are three types of Republican

Republican <- (PolParty2=="Strong Rep")|(PolParty2=="Ind, Near Rep")|
  (PolParty2=="Not Str Rep")

head(PolParty2); head(Republican)

GunOwner <- OwnGun2=="Yes"
Bush <- Pres002=="Bush"

#Combine the six logical vectors into a data frame
GSSLogical <- data.frame(Male, Married, Protestant, Republican, GunOwner, Bush)
head(GSSLogical)

#Now we can use logical operations to search for rows
Guns <- which(GSSLogical$GunOwner); head(Guns); length(Guns)  #all gun owners

#Use AND to find gun owners who voted for Bush
GunsBush <- which(GSSLogical$GunOwner&GSSLogical$Bush);head(GunsBush);length(GunsBush)

#Use AND along with NOT to find gun owners who voted for Gore
GunsGore <- which(GSSLogical$GunOwner&!GSSLogical$Bush);head(GunsGore);length(GunsGore)

#All four alternatives are shown in a contingency table
tbl <- table(GSSLogical$GunOwner,GSSLogical$Bush); tbl

#The first argument corresponds to the row labels.
#We could have done this with the "factor" columns in the cleaned data frame.
table(GSSCleaned$OwnGun,GSSCleaned$Pres00)
#In this case, after we know the one nonzero value
#the row sums and column sums determine the last three entries.
#Compare with the table that would be expected if the factors were independent
tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
#These table look quite different. Is the difference significant?
#R has a mysterious built-in test for doing this.
chisq.test(GSSLogical$GunOwner,GSSLogical$Bush)
#The low p-value means there is about 3  in 10,000,000 probability that it arose by chance.

#Another example: did Republicans vote for Bush?
tbl <- table(GSSLogical$Republican,GSSLogical$Bush); tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(GSSLogical$Republican,GSSLogical$Bush)   #even more extreme 

#Perhaps religion is independent of gender
tbl <- table(GSSLogical$Protestant,GSSLogical$Male); tbl
Expected <- outer(rowSums(tbl), colSums(tbl))/sum(tbl); Expected
chisq.test(GSSLogical$Protestant,GSSLogical$Male)   #14% chance to arise by chance



