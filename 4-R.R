setwd("/Users/abhishekmalani/Desktop/Business Analytics/Data Files")

library(readxl)
if (!require(ggplot2)) install.packages("ggplot2"); library(ggplot2)
if (!require(RColorBrewer)) install.packages("RColorBrewer"); library(RColorBrewer)


sat <- read.csv("sat.csv")
head(sat)

ggplot(data = sat) + 
  geom_histogram(binwidth = 10, aes(x = Math, fill = "turquoise"), alpha = 0.5) +
  geom_histogram(binwidth = 10, aes(x = Verbal, fill = "coral1"), alpha = 0.5) + xlim(400,800)

#I want to make a histogram for total SAT scores... how do I do this?









sat$total <- sat$Math + sat$Verbal;sat

ggplot(data = sat) + 
  geom_histogram(binwidth = 10, aes(x = total, fill = "turquoise"), alpha = 0.5)

#limits the data to total scores between 800 and 1600
ggplot(data = sat) + 
  geom_histogram(binwidth = 10, aes(x = total, fill = "turquoise"), alpha = 0.5) + xlim (800,1600)

norm.1 <- rnorm(length(sat$total))
## Then take their quantiles, with proabilities from 0 to 1 in increments of 0.01
q.norm.1 <- quantile(norm.1, seq(0, 1, 0.01))
q.sat <- quantile(sat$total, seq(0, 1, 0.01))
## Create a new dataframe from these observations
df.norm <- as.data.frame(cbind(q.norm.1, q.sat))
df.norm <- data.frame(q.norm.1, q.sat)
## Plot the quantiles
ggplot(data = df.norm) + 
  geom_point(aes(x = q.norm.1, y = q.sat), alpha = 0.7)

#finding mean and sd for sat total
sat.mean <- mean(sat$total);sat.mean
sat.sd <- sd(sat$total); sat.sd



#Donald wants to get into Harvard where the most kids score in the top 5% on the SAT
#He sets a goal to get a 1400. Is his goal good enough? If not, what should his goal be?

pnorm(1400,sat.mean,sat.sd) #not good enough
qnorm(.95,sat.mean,sat.sd)  #Goal should be 1430




data(mtcars)
head(mtcars)

#Learning how to order data
ggplot(data=mtcars) + geom_bar(stat="identity", aes(x=row.names(mtcars), y = mpg)) + 
  theme(axis.text.x = element_text(angle = 90, hjust =1)) + 
  xlab("Car Model") + ylab("Miles per Gallon")

mtcars.sort <- mtcars[order(mtcars$mpg), ]
mtcars.sort #Cars are now ordered by MPG!

ggplot(data=mtcars.sort) + geom_bar(stat="identity", aes(x=row.names(mtcars.sort), y = mpg)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Car Model") + ylab("Miles Per Gallon")

#Although we ordered the data, we are plotting the car name on the x axis so we need to try something else
#A workaround is the create a couple of new columns.
                          
mtcars$car_name <- row.names(mtcars)
mtcars$mpg_rank <- factor(mtcars$car_name, levels = mtcars$car_name[order(mtcars$mpg, decreasing = F)])

#The following command factors the car names in “mtcars”, and assigns them a level equal to position 
#of that car when the cars are ordered by miles per gallon:

ggplot(data=mtcars) + geom_bar(stat="identity", aes(x=mpg_rank, y = mpg)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Car Model") + ylab("Miles Per Gallon")


#Call Center Data 

call <- read_excel("CallCenterData2.xlsx", skip = 3)
head(call)

## Permutation Test: To see if differences between length of service is significant between girls and guys

#average length of service spent for males and females
femaleavg <- sum(call$`Length of Service (years)`[call$Gender=="0"])/length(call$Gender[call$Gender=="0"]); femaleavg
maleavg <- sum(call$`Length of Service (years)`[call$Gender=="1"])/length(call$Gender[call$Gender=="1"]); maleavg

#observed difference
observed <- maleavg - femaleavg; observed
#Now replace all the gender identities with a random sample of all the data
Gender <- sample(call$Gender); Gender   #permuted Gender column
Favg <- sum(call$`Length of Service (years)`[Gender=="0"])/length(call$Gender[Gender=="0"]); Favg
Mavg <- sum(call$`Length of Service (years)`[Gender=="1"])/length(call$Gender[Gender=="1"]); maleavg

Mavg - Favg #just as likely to be positive as negative

#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(call$Gender); Gender   #permuted Gender column
  Favg <- sum(call$`Length of Service (years)`[Gender=="0"])/length(call$Gender[Gender=="0"]); Favg
  Mavg <- sum(call$`Length of Service (years)`[Gender=="1"])/length(call$Gender[Gender=="1"]); maleavg
  diffs[i] <- Mavg - Favg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero

hist(diffs, breaks = "FD", col="light blue")

#Now display the observed difference on the histogram
abline(v = observed, col = "red")

#What is the probability (the P value) that a difference this large could have arisen with a random subset?

pvalue <- 2*(1 -(sum(diffs >= observed)+1)/(N+1)); pvalue #two sided significance test
#Statistically significant because P-Value is less than 0.05


## Facebook
fb <- read_excel("FacebookSurvey.xlsx", skip = 2)
head(fb)

ggplot(data = fb) + geom_point(aes(Friends, `Hours online/week`))+ 
  geom_smooth(method=lm, aes(Friends, `Hours online/week`))

ggplot(data = fb) + geom_point(aes(Friends, `Hours online/week`, color = Gender))+ 
  geom_smooth(method=lm, aes(Friends, `Hours online/week`))

fb.model <- lm(`Hours online/week`~Friends, data = fb)
summary(fb.model)    

coefficients(fb.model)

#How long do you estimate someone with 3000 friends will spend per day?
intercept <- coefficients(fb.model)[[1]]
slope <- coefficients(fb.model)[[2]]

(slope*2000 + intercept)/7


## Permutation Test: To see if differences between hours spent on FB is significant between girls and guys

#average time spent for males and females
femaleavg <- sum(fb$`Hours online/week`[fb$Gender=="female"])/length(fb$Gender[fb$Gender=="female"]); femaleavg
maleavg <- sum(fb$`Hours online/week`[fb$Gender=="male"])/length(fb$Gender[fb$Gender=="male"]); maleavg

#observed difference
observed <- maleavg - femaleavg; observed
#Now replace all the gender identities with a random sample of all the data
Gender <- sample(fb$Gender); Gender   #permuted Gender column
Favg <- sum(fb$`Hours online/week`[Gender=="female"])/length(fb$Gender[Gender=="female"]); Favg
Mavg <- sum(fb$`Hours online/week`[Gender=="male"])/length(fb$Gender[Gender=="male"]); Mavg

Mavg - Favg #just as likely to be positive as negative

#Repeat 10000 times
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(fb$Gender); Gender   #permuted Gender column
  Favg <- sum(fb$`Hours online/week`[Gender=="female"])/length(fb$Gender[Gender=="female"]); Favg
  Mavg <- sum(fb$`Hours online/week`[Gender=="male"])/length(fb$Gender[Gender=="male"]); Mavg
  diffs[i] <- Mavg - Favg    #as likely to be negative or positive
}
mean(diffs) #should be close to zero

hist(diffs, breaks = "FD", col="light blue")

#Now display the observed difference on the histogram
abline(v = observed, col = "red")

#What is the probability (the P value) that a difference this large could have arisen with a random subset?
pvalue <- 2*((sum(diffs >= observed)+1)/(N+1)); pvalue #two sided significance test
#Not statistically significant because P-Value is greater than 0.05


##Olympics

olympics <- read_excel("OlympicTrackandFieldData.xlsx", skip = 2)
head(olympics)

#only data up until 1980
olympics.sample <- subset(olympics, olympics$Year <= 1980)

#plotting the three sports
ggplot(data=olympics.sample) + geom_point(aes(Year,`High Jump (in.)`)) 
ggplot(data=olympics.sample) + geom_point(aes(Year,`Long Jump (in.)`)) 
ggplot(data=olympics.sample) +geom_point(aes(Year,`Discus (in.)`))

#linear regressions
high <- lm(data= olympics.sample,`High Jump (in.)` ~ Year)
long <- lm(data= olympics.sample,`Long Jump (in.)` ~ Year)
discus <- lm(data= olympics.sample,`Discus (in.)` ~ Year)


#Lets try to predict year 2000
coefficients(high)[[2]]*2000 + coefficients(high)[[1]]
#comparing to actual
olympics[olympics$Year==2000,]

#not always so accurate
coefficients(discus)[[2]]*2000 + coefficients(discus)[[1]]
coefficients(long)[[2]]*2000 + coefficients(long)[[1]]


##NFL (multivariable regression)

nfl <- read_excel("NationalFootballLeague.xlsx", skip = 2)
head(nfl)

model.nfl <- lm(nfl$`Points/Game`~nfl$`Yards/Game`+nfl$`Opponent Yards/Game`+nfl$`Rushing Yards/Game`+nfl$`Passing Yards/Game`)
summary(model.nfl)

wins <- c(8,4,5,7,7,7,7,10,13,7,7,13,8,13,11,4,1,8,16,7,10,4,4,8,10,11,5,10,3,9,10,9)
nfl$wins <- wins

model.nfl <- lm(nfl$wins ~ nfl$`Points/Game` + nfl$`Yards/Game` + nfl$`Opponent Yards/Game` + nfl$Penalties +
                  nfl$`Penalty Yards` + nfl$Interceptions + nfl$Fumbles + nfl$`Passes Intercepted` + nfl$`Fumbles Recovered`)
summary(model.nfl)




## Time Permitting

#Find Prime Numbers

#Solution:
Finder <- function(n) {
  if(n>=2){
    numbers <- seq(2,n)
    primes <- c()
    for(i in seq(2,n)){
      if(any(numbers==i)){
        primes <- c(primes,i)
        numbers <- c(numbers[(numbers %% i)!=0],i)
      }
    }
    return(primes)
  }
  else{
    stop("Pick a number greater than or equal to 2")}
}
Finder(1)
Finder(100)






 
 



