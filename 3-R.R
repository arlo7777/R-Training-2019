#Lecture 3: Introduction to R

## For Loops
n <- 5 
result <- 1 
for (i in 1:n){
  result <- result * i}
result

#Function for Fibonacci
Fibonacci <- function(n){
  numbers <- numeric(n) 
  numbers[1] <- numbers[2] <- 1 
  for (i in 3:n){
    numbers[i] <- numbers[i - 2] + numbers[i - 1]
  }
  return(numbers)
}
Fibonacci(10)



#Problem: Find Prime Numbers

#Solution:



## Basics of ggplot2
#Learn more: https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(RColorBrewer)) install.packages("RColorBrewer"); library(RColorBrewer)
if (!require(reshape2)) install.packages("reshape2"); library(reshape2)
if (!require(plyr)) install.packages("plyr"); library(plyr)
if (!require(dplyr)) install.packages("dplyr"); library(dplyr)
if (!require(scales)) install.packages("scales"); library(scales)
if (!require(modeest)) install.packages("modeest"); library(modeest)

data(diamonds) ## Load the "diamonds" dataset, available from ggplot2
head(diamonds) ## Look at the diamonds dataset

#Basic Format ggplot(data = df, aes(x = x, y = y)) + geom_function() where 
# function is the type of graph

#Things to put in aes (aesthetic mapping -- things that vary across the graph)
  # x and y
  # size
  # fill and color
  # lineType


#Basic plot
ggplot(data = diamonds) + geom_histogram(binwidth = 500, aes(x = price))

#Advanced Plot
ggplot(data= diamonds) +
  geom_histogram(binwidth = 500, alpha = 0.75, aes(x=price, fill = cut))

data(mtcars) ### A dataset with information about cars
head(mtcars) ### There are 11 variables

ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() 

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour=cyl)) 

ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour=factor(cyl)))

#Bubble Chart
ggplot(mtcars, aes(wt, mpg)) + geom_point(aes(colour=factor(cyl), size=hp), alpha = 0.5) 

## Linear Regressions

require(stats)

head(mtcars)
plot(mtcars)

## Save a copy of a dataframe
new.mtcars <- mtcars
## List of names that need to be factored:
to.factor <- c("cyl", "vs", "am", "gear", "carb")
for(i in 1:length(to.factor)){
  colName <- to.factor[i]
  new.mtcars[,colName] <- factor(new.mtcars[,colName])
}
head(new.mtcars)
num.mtcars <- dplyr::select_if(new.mtcars, is.numeric)
# pairs(~mpg+disp+drat+wt,data=mtcars, 
# main="Simple Scatterplot Matrix")
plot(num.mtcars)

mtcars$car_name <- row.names(mtcars)
head(mtcars)
ggplot(mtcars, aes(x= wt, y = mpg)) + geom_point() + geom_text(aes(label=mtcars$car_name), alpha =0.5)

## Find a linear fit to our data
## "wt" is the predictor/independent variable
## "mpg" is the response/dependent variable
fit.mpgVwt <- lm(mpg ~ wt, data=mtcars)

summary(fit.mpgVwt)

## Coefficients of our linear fit: 
coef.fit.mpgVwt <- coefficients(fit.mpgVwt)
coef.fit.mpgVwt

#Getting Coefficients by Index
  ## Intercept
  coefficients(fit.mpgVwt)[[1]]
  
  ## Slope
  fit.mpgVwt$coefficients[[2]]
  
#Getting Coefficients by name
  ## Slope
  coefficients(fit.mpgVwt)[["wt"]]
  
  ## Intercept
  fit.mpgVwt$coefficients[["(Intercept)"]]

#Just a plot of mpg vs wt
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() 
#added in line of best fit
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() + geom_abline(intercept=fit.mpgVwt$coefficients[[1]], slope=fit.mpgVwt$coefficients[[2]])

ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point() + 
  geom_smooth(method=lm, color='#2C3E50', level = 0.95)

mtcars$res.lm <- residuals(fit.mpgVwt)  ## Another way to get residuals
ggplot(lm(mpg~wt, data=mtcars)) + 
  geom_point(aes(x=.fitted, y=.resid))

summary(fit.mpgVwt)[["r.squared"]]     ## R-squared

summary(fit.mpgVwt)[["adj.r.squared"]] ## Adjusted R-squared residuals

help("summary.lm") #if you want to look into other things the summary function can do for lm































