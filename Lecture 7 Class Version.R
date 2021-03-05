#Introduction to R: Lecture 7

#Markov Chains
# Markov chains are sequences of random variables X0,X1,... where the distribution of Xt+1,
# conditional on all previous values, depends only on Xt.

#Example: Predictive Text

#Chocolate Company Example
#Two different stages: 
#Cadbury User or Nestle User
#Probabilities (by Month)
#If you are a Cadbury user, there is a 60% chance you will stick with it and there
#is a 40% chance you move to Nestle
#If you are a Nestle user there is a 70% chance you will stick with it and there
#is a 30% chance you move to Cadbury
#Starting Condition
#Currently both companies have split the market share (50% each)
P <- matrix(c(.6,.4,.3,.7), nrow = 2,ncol = 2, byrow = TRUE); P
current <- matrix(c(.5,.5),1,2);current

#Month 2
current <- current %*% P; current

#Month 3
current <- current %*% P; current

#Month 3 Directly
current <- matrix(c(.5,.5),1,2);current
current <- current %*% P %*% P; current

#Longterm
for(i in 1:100){
  current <- current %*% P; current
}
current

#Check if it stays the same after another month
current <- current %*% P; current
#stabilized (in actuality you need to see if there are eigenvectors for you P matrix to see if stabilzation is possible,
#but here you can check brute force)



#QUESTION: In the Dark Ages, Harvard, Dartmouth, and Yale admitted only male students.
# Assume that, at that time, 80 percent of the sons of Harvard men went to Harvard and the
# rest went to Yale,40 percent of the sons of Yale men went to Yale, and the rest split evenly
# between Harvard and Dartmouth; and of the sons of Dartmouth men, 70 percent went to Dartmouth,
# 20 percent to Harvard, and 10 percent to Yale. (i) Find the probability that the grandson of a
# man from Harvard went to Harvard. (ii) Modify the above by assuming that the son of a Harvard man
# always went to Harvard. Again, find the probability that the grandson of a man from Harvard went
# to Harvard.

#i)


#ii)



#Stabilization (brute force)
Pnew <- P
for(i in 1:100){
  Pnew <- Pnew %*% P
}
Pnew

#Lets say you start with 100 men at each of Harvard, Yale, and Dartmouth
P <- matrix(c(.8,.2,0,.3,.4,.3,.2,.1,.7),3,3,byrow=TRUE)
current <- matrix(,50,3)
current[1,] <- c(100,100,100); current
for(i in 1:(49)){
  current[i+1,] <- current[i,] %*% P
}
current

#Lets say once Harvard has more than 150 students it implements stricter rules

strictP <- matrix(c(.6,.3,.1,.2,.45,.35,.1,.2,.7),3,3,byrow=TRUE);strictP
P
current <- matrix(,50,3)
current[1,] <- c(100,100,100)
for(i in 1:(49)){
  if(current[i,1] <=150){
    current[i+1,] <- current[i,] %*% P
  }
  else
    current[i+1,] <- current[i,] %*% strictP
}
current


#Permutation Tests: we have a sample but know nothing about the population
#These are data collected by a student in a Minneapolis pub
#Do this many times, and plot a histogram of the results
#Reload our fabricated data as if it were real data

#Mathematical Statistics with Resampling and R
setwd("/Users/abhishekmalani/Desktop/Freshman Year/Math 23C/Data")
BW <- read.csv("Beerwings.csv"); head(BW)
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
hist(diffs, breaks = "FD")
#Now display the observed difference on the histogram
abline(v = observed, col = "red")
#What is the probability (the P value) that a difference this large
#could have arisen with a random subset?
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue



