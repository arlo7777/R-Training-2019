# Lecture 6 R Script

set.seed(12345)

# A binomial distribution is the frequency distribution of the possible number of successful outcomes in a given number of trials in each of which there is the same probability of success.

#QUESTION: What is the probability of flipping exactly 4 heads in a fair coin when tossed 6 times?
dbinom(x = 4, size = 6, prob = 0.5)

#QUESTION: What  is the probability of flipping 4 or less heads in a fair coin when tossed 6 times?
pbinom(q =4, size = 6, prob = 0.5)

#QUESTION: With 98% certainty, how many heads will at land when a fair coin is tossed 6 times? 
qbinom(0.98, 6, 0.5)

#Expected Value in a binomial distribution is m*p; where m is the number of trials and p is prob

#PROBLEM: Suppose 10% of the water heater dip tubes produced by a machine are defective, 
# and suppose 15 tubes are produced each hour. Each tube is independent of all other tubes. 
# This process is judged to be out of control when more than four defective tubes are produced 
# in any single hour. Simulate the number of defective tubes produced by the machine for each hour 
# over a 24-hour period, and determine whether any process should have been judged out of control 
# at any point in that simulation run.

  defectives <- rbinom(n=24,size=15,prob=0.1); defectives
  table(defectives > 4)
  
  #QUESTION: What is the percent of times you expect a process to be judged out of control?
  (1-pbinom(4,15,0.1))
  
  #Lets test it
  test <- numeric(1000)
  for(i in 1:length(test)){
    defectives <- rbinom(n=24,size=15,prob=0.1)
    test[i] <- length(defectives[defectives > 4])/length(defectives)
  }
  mean(test)

# Write an R function which simulates the outcomes of a student guessing at a True–False test consisting of n questions.
# (a) Use the function to simulate one student guessing the answers to a
# test with 10 questions; calculate the number of correct answers for
# this student.
  
  student <- runif(n=10, min=0, max =1);student
  student > 0.5
  table(student>0.5)
  
# (b) Simulate the number of correct answers for a student who guesses
# at a test with 1000 questions.

  student <- runif(n=1000, min=0, max =1)
  table(student>0.5)
  
# 2 Suppose a class of 100 writes a 20-question True–False test, and everyone in the class guesses at the answers.
# (a) Use simulation to estimate the average mark on the test as well as
# the standard deviation of the marks.
  
  questions <- 20
  success <- 0.5
  class <- rbinom(n = 100, size = questions, prob = success)
  mean(class); sd(class)
  #theoretical mean and sd
  questions*success
  sqrt(questions*success*(1-success))

#Looking at the binomal distribution by the probability of success
require(ggplot2)
binom.1 <- rbinom(1000,100,0.1)
binom.2 <- rbinom(1000,100,0.2)
binom.3 <- rbinom(1000,100,0.3)
binom.4 <- rbinom(1000,100,0.4)
binom.5 <- rbinom(1000,100,0.5)
binom.6 <- rbinom(1000,100,0.6)
binom.7 <- rbinom(1000,100,0.7)
binom.8 <- rbinom(1000,100,0.8)
binom.9 <- rbinom(1000,100,0.9)

binom <- data.frame(binom.1,binom.2, binom.3, binom.4, binom.5, binom.6, binom.7,binom.8,binom.9)
ggplot(binom) + geom_histogram(data = binom, aes(x =binom.1),binwidth = 1, fill = "blue", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.2),binwidth = 1, fill = "red", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.3),binwidth = 1, fill = "green", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.4),binwidth = 1, fill = "orange", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.5),binwidth = 1, fill = "purple", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.6),binwidth = 1, fill = "orange", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.7),binwidth = 1, fill = "green", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.8),binwidth = 1, fill = "red", alpha = 0.5) +
 geom_histogram(data = binom, aes(x=binom.9),binwidth = 1, fill = "blue", alpha = 0.5) +
 labs(title="Binomal Distributions by Probability of Success",x="Successes", y = "Count")+
  theme_classic()



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
P <- matrix(c(.8,.2,0,.3,.4,.3,.2,.1,.7),3,3,byrow=TRUE);P
P %*% P
#ii)
P <- matrix(c(1,0,0,.3,.4,.3,.2,.1,.7),3,3,byrow=TRUE);P
P %*% P

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













