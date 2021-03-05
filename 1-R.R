# Lecture 1: Introduction to R

# BASIC OPERATIONS
# Addition
5+7 #answer: 12

# Subtraction
3-2 #answer: 1

# Multiplication
3*2 #answer: 6

# Division
27/3 #answer: 9

# Exponentiation
3^2 #answer: 9

# Modulus
3%%2 #answer: 1

# Integer Division
7%/%3 #answer: 2

# Complex Expressions
2*(3 + 1) #answer: 8



((10/5)+8)%%9
##Take Random Numbers##


#LOGICAL OPERATORS: useful for subsetting data, comparisions, and conditioning

3>=2 #true
!T   #false
3==4 #false
3!=4

#VECTORS
#c is short for concatenate
c(1,2,3)

#vector operations
2*c(1,2,3)
c(1,2,3)^2
2^c(1,2,3)  #works both ways but different results

c(2,4,8) == 2^c(1,2,3) #comparisons of vectors also work!

#sequences
:10 ## Sequence of integers from 1 to 10 in increments of 1
seq(1, 10, by = 4) ## Generate a sequence of integers from 1 to 10 in increments of 1
seq(0, 1, 0.25) ## Generate a sequence of integers from 0 to 1 in increments of 0.25
seq(1, 10, 2) ## Generate a sequence of integers from 1 to 10 in increments of 2

## Question ## 
  #What is the difference between these two?
  48:14*3
  48:(14*3)
  
#repetition 
rep(5, 3) ## A vector where the value 5 is repeated 3 times
rep(c(1, 2, 3), 2) 
c(1,2,3)


#NAMED STORAGE
#Assignment always uses <- not = (Good R Convention)
x <- 5
x
y <- c(1, 2, 3);y #if you want to name a variable and see it right away use a ;

x*y #you do the same operations to named variables

#Assignment doesn't create an active link
x <- 5
y <- x
x;y #both values are the same

x<- 7
x;y #however, if you change x after, you don't automatically change y

y <- x

x;y
#LOOKING AT VECTORS
x <- seq(1, 10, 1) ## Define x as a sequence of integers from 1 to 10
x[5] #this shows you the fifth element in vector x

min(x)
max(x)
sum(x)
mean(x)
length(x)

#Conditional functions
length(x[x>=5])

#Example 1: Sum of First N Odd Numbers
  #lets test this
  x <- seq(1,33,2);x
  
  #what is the length of the vector
  length(x)
  
  #what is the sum of the first 10 odd numbers; should be n^2 or 100
  sum(x)
  
#Example 2: Summary Statistics
mystery1 <- c(20.9,27.2,31.4,27.3,30.0,28.4,29.7)
mystery2 <- c(28.2,22.7,37.1,35.0,32.5,33.6,31.5)

mean(mystery1)
mean(mystery2)
min(mystery1)
min(mystery2)

#Combining Vectors
x1 <- seq(1, 10, 1)      ## Sequence from 1 to 10
x2 <- rep(5, length(x1)) ## Repeat 5 ten times
length(x1) == length(x2) ## Check that the two vectors are equal    

XCol <- cbind(x1, x2)
XCol

XRow <- rbind(x1, x2)
XRow

XCol[5, 2]
XRow[1,5]

#Arrays
a <- array(1:24, c(3, 4, 2));a
dim(a)
a[,,1]
a[3,1,2]

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
Dice$Total <- Dice$Var1+Dice$Var2
Dice
length(Dice$Total[Dice$Total==7])/length(Dice$Total)
1/6







# Extra (Miscellaneous) 

# Value <- 2:12
# Data <- data.frame(Value); Data
# Data$Count
# for (i in 2:12) {
#   Data$Count[i-1] <- length(Dice$Total[Dice$Total==i])
# }
# Data
# sum(Data$Count)
# Data$Prob <- Data$Count/sum(Data$Count);Data
# sum(Data$Prob)
# Data$Expected <- Data$Value*Data$Prob
# Data
# sum(Data$Expected)



# 
# Dice <- expand.grid(Dice1,Dice2,Dice3);Dice
# Dice$Total <- Dice$Var1+Dice$Var2+Dice$Var3
# Dice
# min(Dice$Total)
# max(Dice$Total)
# 
# Value <- 3:18
# Data <- data.frame(Value); Data
# Data$Count
# for (i in 3:18) {
#   Data$Count[i-2] <- length(Dice$Total[Dice$Total==i])
# }
# sum(Data$Count)
# Data$Prob <- Data$Count/sum(Data$Count);Data
# Data$Expected <- Data$Count*Data$Prob
# sum(Data$Expected)
