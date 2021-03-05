#Introduction to R: Lecture 5

if (!require(lpSolve)) install.packages("lpSolve"); library(lpSolve)          ## C-based functions for linear programming
if (!require(lpSolveAPI)) install.packages("lpSolveAPI"); library(lpSolveAPI) ## The R interface for "lpSolve" 
if (!require(linprog)) install.packages("linprog"); library(linprog)          ## R-based functions for linear programming
if (!require(quadprog)) install.packages("quadprog"); library(quadprog)       ## For quadratic programming

## Vector 1: The following sequences
v1 <- 1:9
v1

## Construct the previous matrix using "cbind"
cbind(1:3, 4:6, 7:9)

## Construct a matrix using "matrix", fill by row instead
## of column
matrix(v1, nrow=3, byrow=T)

## Construct the previous matrix using "cbind"
rbind(1:3, 4:6, 7:9)

#PROBLEM: 3x1−4x2 = 6
#          x1+2x2 =−3

## A matrix
A <- cbind(c(3, 1), c(-4, 2))
A

## b matrix
b <- matrix(c(6, -3))
b

## Solve the system using "solve"
solve(A, b)

round(solve(A, b), 1)

#REFERENCE TO MATRIX OPERATIONS: https://www.statmethods.net/advstats/matrix.html


# Sklenka Ski Company (SSC) is a small manufacturer of two types of popular all-terrain snow skis,
# the Jordanelle and the Deercrest models. The manufacturing process consists of two principal 
# departments: fabrication and finishing. The fabrication department has 12 skilled workers, each
# of whom works 7 hours per day. The finishing department has 3 workers, who also work a 7-hour shift.
# Each pair of Jordanelle skis requires 3.5 labor-hours in the fabricating department and 1 
# labor-hour in finishing. The Deercrest model requires 4 labor-hours in fabricating and 1.5 
# labor-hours in finishing. The company oper- ates 5 days per week. SSC makes a net profit of $50
# on the Jordanelle model and $65 on the Deercrest model. In anticipation of the next ski-sale season,
# SSC must plan its production of these two models. Because of the pop- ularity of its products
# and limited production capacity, its products are in high demand, and SSC can sell all it can
# produce each season. The company anticipates selling at least twice as many Deercrest models
# as Jordanelle models. The company wants to determine how many of each model should be produced
# on a daily basis to max- imize net profit.


#STEPS
# 1) Define Objective Function
# 2) Define Constraints

## Load lpSolveAPI to access lpSolve
require(lpSolveAPI) 

## Our objective function is a vector containing the coefficients 
## of our x variables
## P(x)=50x1+65x2

f.obj <- c(50, 65)

#Constraint Functions:
# 3.5x1 + 4x2 ≤ 84
# 1x1 + 1.5x2 ≤ 21
# -2x1 + 1x2 ≥ 0
# 1x1 + 0x2 ≥ 0
# 0x1 + 1x2 ≥ 0

## Constraints: left hand side (LHS)
## Enter the coefficients of the LHS as a matrix 
## “lp” automatically enters the constraint equations for non-negativity for us, unless we specify different constraints.

## It can be useful for large systems to separate the data somehow
## E.g., by columns or rows
## Here, we use columns, corresponding to coefficients for a single variable:
col.1   <- c(3.5, 1, -2)       ## First column: x1 coefficients
col.2   <- c(4, 1.5,  1)       ## Second column: x2 coefficients
f.con.c <- cbind(col.1, col.2) ## Combine the columns
f.con.c

## Alternatively, we could instead have used rows, corresponding
## to coefficients for a single constraint:
row.1   <- c(3.5, 4)           ## Row 1: Constraint 1
row.2   <- c(1, 1.5)           ## Row 2: Constraint 2
row.3   <- c(-2,  1)           ## Row 3: Constraint 3
f.con.r <- rbind(row.1, row.2, row.3) ## Combine the columns
f.con.r

## Or we could have filled the matrix with a single vector
## But this can lead to errors for large matrices if entering by hand
f.con.m <- matrix(c(3.5,   1, -2, 
                    4, 1.5,  1), nrow=3)
f.con.m

## Check equivalency
f.con.c == f.con.r

f.con.c == f.con.m

## Only need one matrix
f.con<- f.con.c

## Enter the middle part of the constraint as a character vector
f.dir <- c("<=", "<=", ">=")

## Enter the right hand side of the constraint as a vector
f.rhs <- c(84, 21, 0)

## Set up and run our linear program with "lp"
P.lp <- lp(direction    = "max", ## Direction: "min" or "max", default "min"
           objective.in = f.obj, ## Objective
           const.mat    = f.con, ## Constraints: LHS coefficients matrix
           const.dir    = f.dir, ## Constraints: directions vector
           const.rhs    = f.rhs  ## Constraints: RHS coefficients vector
)
P.lp

## Set up and run our linear program with "lpSolveAPI" 
## First, make an "lp" object, verbose defines error reporting ... neutral means no reporting
P.lpObj <- make.lp(nrow = 3, ncol = 2, verbose = "neutral")
P.lpObj

## Set up and run our linear program with "lpSolveAPI" 
## Set the goal of our lp object using "lp.control"
## Use "set.objfn"
## Set goal: sense is either min or max depending on optimization problem
lp.control(P.lpObj, sense="max")

P.lpObj

## Set up and run our linear program with "lpSolveAPI" 
## Add constraints to our "lp" object using "add.constraint"
## Note: here, J=x1 and D=x2
## We have three constraints
## Constraint 1: 3.5J +   4D <= 84
## Constraint 2:   1J + 1.5D <= 21
## Constraint 3:  -2J +   1D >= 0

## Set constraint coefficients for J
set.column(P.lpObj, 1, c(3.5, 1, -2))
## Set constraint coefficients for D
set.column(P.lpObj, 2, c(4, 1.5, 1))
P.lpObj

## Set the type of constraint
set.constr.type(P.lpObj, c("<=", "<=", ">="))
P.lpObj

## Set the right hand side of the constraints
set.rhs(P.lpObj, c(84, 21, 0))
P.lpObj

## Set up and run our linear program with "lpSolveAPI" 
## Set the bounds of our decision variables
## Note: here, J=x1 and D=x2
## x1 and x2 >= 0
set.bounds(P.lpObj, lower = c(0, 0), columns = 1:2)
P.lpObj

## Set up and run our linear program with "lpSolveAPI" 
## Add the objective function to our "lp" object 
## Use "set.objfn"
## Note: here, J=x1 and D=x2

## Objective: 50*J +   65*D 
set.objfn(P.lpObj, c(50, 65))
P.lpObj

## Rename the rows and columns of our "lp" object
## Constraints/Rows
RowNames <- paste0("const ", 1:3)
RowNames


## Variables/Columns
ColNames <- c("Jordanelle", "Deercrest")
## Set the names using "dimnames": row names first
dimnames(P.lpObj) <- list(RowNames, ColNames)
P.lpObj

## Set up and run our linear program with "lpSolveAPI" 
## Solve our lp object 
solve(P.lpObj)

## Look at the solution to our linear program

get.objective(P.lpObj)    ## Get objective function value

get.variables(P.lpObj)    ## Get variables

get.constraints(P.lpObj)  ## Get constraints
get.sensitivity.obj(P.lpObj)
require(ggplot2)

Jordanelle <- 1:100
Deercrest <- 1: 100
df.skis <- as.data.frame(cbind(Jordanelle, Deercrest)); df.skis
## Slope and intercept are found by solving constraints for "Deercrest"
ggplot(df.skis, aes(x=Jordanelle, y=Deercrest)) + 
  xlim(0, 30) + ylim(0, 30) +
  ## Fabrication Constraint/Constraint1
  geom_abline(slope=-3.5/4, intercept=84/4, colour="tomato3", alpha = 0.65) +
  ## Finishing Constraint/Constraint2
  geom_abline(slope=-1/1.5, intercept=21/1.5, colour="green4", alpha = 0.65) +
  ## Market Mix/Constraint 3
  geom_abline(slope=2, intercept=0, colour="purple3", alpha = 0.65) 



#OH SAMPLE PROBLEM (Innis Investments)

# Innis Investments is a small, family-owned business that manages personal financial portfolios. 
# The company manages six mutual funds and has a client that has ac- quired $500,000 from an inheritance. 
# Characteristics of the funds are given in Table below.
# Innis Investments uses a proprietary algorithm to establish a measure of risk for its funds based on the 
# historical volatility of the investments. The higher the volatility, the greater the risk. The company recommends
# that no more than $200,000 be invested in any in- dividual fund, that at least $50,000 be invested in each of the
# multinational and balanced funds, and that the total amount invested in income equity and balanced funds be at
# least 40% of the total investment, or $200,000. The client would like to have an average return of at least 5%
# but would like to minimize risk. What portfolio would achieve this?

# Fund                            Expected annual return    Risk Measure
# 1. Innis Low-priced Stock Fund  8.13%                     10.57% 
# 2. Innis Multinational Fund     9.02%                     13.22% 
# 3. Innis Mid-cap Stock Fund     7.56%                     14.02%
# 4. Innis Mortgage Fund          3.62%                     2.39% 
# 5. Innis Income Equity Fund     7.79%                     9.30%
# 6. Innis Balanced Fund          4.40%                     7.61%


M.lpObj <- make.lp(nrow = 3, ncol = 6, verbose = "neutral")
M.lpObj


set.column(M.lpObj,1,c(1,8.13/500000,0))
set.column(M.lpObj,2,c(1,9.02/500000,0))
set.column(M.lpObj,3,c(1,7.56/500000,0))
set.column(M.lpObj,4,c(1,3.62/500000,0))
set.column(M.lpObj,5,c(1,7.79/500000,1))
set.column(M.lpObj,6,c(1,4.40/500000,1))
M.lpObj

f.obj <- c(10.57/500000,13.22/500000,14.02/500000,2.39/500000,9.30/500000,7.61/500000)

set.objfn(M.lpObj, f.obj);M.lpObj

set.constr.type(M.lpObj, c("=", ">=", ">="))
M.lpObj

set.constr.value(M.lpObj, c(500000,5,200000))
M.lpObj

set.bounds(M.lpObj,lower = c(0,50000,0,0,50000,0), upper = c(rep(200000,6)))
M.lpObj

ColNames <- c("Low-priced","Multinational", "Mid-cap", "Mortgage", "Income Equity", "Balanced")
RowNames <- paste0("const",1:3)
dimnames(M.lpObj)<- list(RowNames,ColNames)
M.lpObj
solve(M.lpObj)

get.objective(M.lpObj)
get.variables(M.lpObj)
get.constraints(M.lpObj)
