# Lecture 2: Introduction to R

#Dataframes
Day <- 1:5;Day
Number_Sales <- c(5, 15, 7, 9, 12);Number_Sales
Daily_Sales <- c(100, 1000, 300, 200, 500)

#main way to create a dataframe
Sales <- data.frame(Day, Number_Sales, Daily_Sales) 
Sales
#alternative method
Sales <- as.data.frame(cbind(Day, Number_Sales, Daily_Sales))
Sales


#Viewing a dataframe
Sales[,2] #look at column 2
Sales$Number_Sales #alternative approach
colnames(Sales) #if you don't know the names you can call this function

head(Sales) #this is a very useful function to see the first few rows of a dataframe

#Making a new column
Sales$AveDailySale <- Sales$Daily_Sales/Sales$Number_Sales
Sales$AveDailySale
Sales #added the column
mean(Sales$AveDailySale)

sum(Sales$Daily_Sales)/sum(Sales$Number_Sales)

#Extension Problem

#Truth Tables
p <- c(TRUE, TRUE, FALSE, FALSE); p
q <- c(T,F,T,F); q
#The second line works because T is a predefined variable with value TRUE
T; F    #careful -- don't assign a new value to T or F!
#It is important to use the "vectorized" operators & and |, not && and ||
pANDq <- p&q; pANDq
p&&q #&& does not produce a vector. It looks just at the first component.
pORq <- p|q; pORq
p||q #|| does not produce a vector
#Now we can assemble the four vectors into a data frame
AndOrTable <- as.data.frame(cbind(p,q,pANDq,pORq)); AndOrTable
AndOrTable <- data.frame(p,q,pANDq, pORq); AndOrTable


#Example 2: What is the probablity of rolling a 7?













Dice1 <- 1:6
Dice2 <- 1:6

Dice <- expand.grid(Dice1,Dice2); Dice
Dice$Total <- Dice$Var1+Dice$Var2; Dice
length(Dice$Total[Dice$Total==7])
length(Dice$Total[Dice$Total==7])/length(Dice$Total)==1/6

##Packages##
  #You can install packages using install.packages('package name')
  #You can load a package either through library('package name') or require('package name')

#Loading Built in Data

library(datasets)
data()
data("iris")
head(iris)

library(readxl)
#if you want to see your current working dir. type getwd() in command line
setwd("/Users/abhishekmalani/Desktop/Business Analytics/Data Files")
RepairTimes <- read_excel("ComputerRepairTimes.xlsx", skip =2)

head(RepairTimes)
histInfo <- hist(RepairTimes$`Repair Time (Days)`)
histInfo

max(RepairTimes$`Repair Time (Days)`) - min(RepairTimes$`Repair Time (Days)`)

histInfo <- hist(RepairTimes$`Repair Time (Days)`, breaks = 35)
histInfo

# Function/model choice:
  # What type of distribution do I think this might be?
  # Estimate parameters
  # Evaluate quality of fit
  # Goodness of fit tests


histInfo <- hist(log(RepairTimes$`Repair Time (Days)`), breaks = 42)
histInfo

#Testing our hypothesis that it is a log-normal dist.
#Number of observations in sample data
nRepairs <- length(RepairTimes$`Repair Time (Days)`)
nRepairs

#mean of the log is different from the log of the mean
mean(log(RepairTimes$`Repair Time (Days)`)) == log(mean(RepairTimes$`Repair Time (Days)`))

RepairTimes
RepairTimes$log.RepairTime <- log(RepairTimes$`Repair Time (Days)`)
RepairTimes

log.meanRepTime <- mean(RepairTimes$log.RepairTime)
log.meanRepTime

log.sdRepTime <- sd(RepairTimes$log.RepairTime)
log.sdRepTime

set.seed(154)
RepairTimes$lnRepairTimes <- rlnorm(nRepairs, log.meanRepTime, log.sdRepTime)
head(RepairTimes)

library("ggplot2")
ggplot(data = RepairTimes) + 
  geom_histogram(binwidth = 1, aes(x = `Repair Time (Days)`, fill = "turquoise"), alpha = 0.5) +
  geom_histogram(binwidth = 1, aes(x = lnRepairTimes, fill = "coral1"), alpha = 0.5) + xlim(0, 50)

## Generate two samples of 100 observations
## from the normal distributions using the "rnorm" function
norm.1 <- rnorm(100)
norm.2 <- rnorm(100)
## Then take their quantiles, with proabilities from 0 to 1 in increments of 0.01
q.norm.1 <- quantile(norm.1, seq(0, 1, 0.01))
q.norm.2 <- quantile(norm.2, seq(0, 1, 0.01))
## Create a new dataframe from these observations
df.norm <- as.data.frame(cbind(q.norm.1, q.norm.2))
df.norm <- data.frame(q.norm.1, q.norm.2)
## Plot the quantiles
ggplot(data = df.norm) + 
  geom_point(aes(x = q.norm.1, y = q.norm.2), alpha = 0.7)

## Generate two samples of 1000 observations
## from the normal distributions using the "rnorm" function
norm.1 <- rnorm(10000)
norm.2 <- rnorm(10000)
## Then take their quantiles, with proabilities from 0 to 1 in increments of 0.1
q.norm.1 <- quantile(norm.1, seq(0, 1, 0.01))
q.norm.2 <- quantile(norm.2, seq(0, 1, 0.01))
## Create a new dataframe from these observations
df.norm <- as.data.frame(cbind(q.norm.1, q.norm.2))
## Plot the quantiles
ggplot(data = df.norm) + 
  geom_point(aes(x = q.norm.1, y = q.norm.2), alpha = 0.7)

## Find the quantiles of the logarithms of Repair Times and the modeled Repair Times
q_log.obs <- quantile(log(RepairTimes$`Repair Time (Days)`), probs = seq(0, 1, 0.01))
q_log.mod <- quantile(log(RepairTimes$lnRepairTimes), probs = seq(0, 1, 0.01))
## Combine these quantiles in a dataframe
qs <- as.data.frame(cbind(q_log.obs, q_log.mod))
## Plot these values
ggplot(data = qs) + 
  geom_point(aes(x = q_log.obs, y = q_log.mod), alpha = 0.7) +
  xlab("Quantiles of the log of Repair Times") +
  ylab("Quantiles of the log of Modeled Repair Times\n(Log-Normal)")

RepairTimes
## Number of Repairs less than or equal to twenty:
nRepairs.leq20 <- length(RepairTimes$`Repair Time (Days)`[RepairTimes$`Repair Time (Days)`<=20])
nRepairs.leq20

## Number of Repairs less than or equal to twenty
nRepairs.leq20 /nRepairs

## Probability of our computer repair time at most 20 days:
plnorm(20, log.meanRepTime, log.sdRepTime)

## Time by which our computer repair will be finished with 98% probability:
qlnorm(0.9, log.meanRepTime, log.sdRepTime)
