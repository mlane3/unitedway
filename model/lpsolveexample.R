#An LPsovler example
library(lpSolve)
costs <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
nr <- nrow(costs)
nc <- ncol(costs)
columns <- t(sapply(1:nc, function(x) rep(c(0, 1, 0), c(nr*(x-1), nr, nr*(nc-x)))))
rows <- t(sapply(1:nr, function(x) rep(rep(c(0, 1, 0), c(x-1, 1, nr-x)), nc)))
mod <- lp("max", as.vector(costs), rbind(columns, rows), "<=", rep(1, nr+nc), binary.vec=rep(TRUE, nr*nc))

#  What eric and I will go over when we meet ----
# LPsovler example we are working from ----
library(lpSolve)
library(lpSolveAPI)
# Set the number of vars
model <- make.lp(0, 3)
# Define the object function: for Minimize, use -ve
set.objfn(model, c(-0.05, -0.04, -0.06)) #Replica of Child well being index
# Add the constraints
add.constraint(model, c(1, 1, 1), "=", 1)
add.constraint(model, c(1, 1, -1), "<=", 0)
add.constraint(model, c(1, -2, 0), "<", 0)
# Set the upper and lower bounds
set.bounds(model, lower=c(0.1, 0.1, 0.1), upper=c(1, 1, 1)) #***We are using for constraints
# Compute the optimized model
solve(model) #[1] 0
# Get the value of the optimized parameters
print(get.variables(model)) #[1] 0.3333333 0.1666667 0.5000000
# Get the value of the objective function
print(get.objective(model)) #[1] -0.05333333
# Get the value of the constraint
get.constraints(model) #[1] 1 0 0

# What else eric and I will go over when we meet ----

#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1

Value = .689
maxCWB_Z = 1.380706 # maxCWB_Z = min(df_index$CWB_Z)
minCWB_Z = -1.969282 # minCWB_Z = max(df_index$CWB_Z)
Value = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z

library(lpSolve)
library(lpSolveAPI)
test <- function(){
  # Set the number of vars
  model <- make.lp(0, 3)
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(7/(7*3), 3/(3*3), 4/(4*3))) #Replica of Child well being index but by sub-indexes
  # Add the constraints
  add.constraint(model, c(7/(7*3), 3/(3*3), 4/(4*3)), "<=", Value)
  add.constraint(model, c(1, 0, 0), ">", 0) #average x1 = .518
  add.constraint(model, c(0, 0, 1), ">", 0) #average x2 = .500
  add.constraint(model, c(0, 1, 0), ">", 0) #average x3 = .469
  lp.control(model,sense='max')
  # Set the upper and lower bounds
  set.bounds(model, lower=c(0.0, 0.0, 0.0), upper=c(1.0, 1.0, 1.0)) #***We are using for constraints
  # Compute the optimized model
  print(solve(model))
  # Get the value of the optimized parameters
  print(get.variables(model)) 
  # Get the value of the objective function
  print(get.objective(model))
  # Get the value of the constraint
  print(get.constraints(model))
  model
}
test()
## Attempt without constraints ----
###It makes zero sense....
test <- function(){
  # Set the number of vars
  model <- make.lp(0, 3)
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(.518, .500, .469)) #Use averages as CWBI
  # Add the constraints
  add.constraint(model, c(1, 1, 1), "=", Value)
  add.constraint(model, c(1, 0, 0), ">", 0.1) #average x1 = .518
  add.constraint(model, c(0, 0, 1), ">", 0.1) #average x2 = .500
  add.constraint(model, c(0, 1, 0), ">", 0.1) #average x3 = .469
  lp.control(model,sense='max')
  # Set the upper and lower bounds
  set.bounds(model, lower=c(0.0, 0.0, 0.0), upper=c(1.0, 1.0, 1.0)) #***We are using for constraints
  # Compute the optimized model
  print(solve(model))
  # Get the value of the optimized parameters
  print(get.variables(model)) 
  # Get the value of the objective function
  print(get.objective(model))
  # Get the value of the constraint
  print(get.constraints(model))
  model
}


## Attempts with optim() ----
testfunction <-  function (myCoef,data){
  # source("model/coefficents.R") #run this if you are missing the coefficents
  rowMeans(myCoef$coefficients*data["Mean",] - myCoef$B) #***We are optimizing this
  return(CWBZ)
}
optim(testfunction,data = df2)
optim(par= 2,testfunction,data = df2)

##