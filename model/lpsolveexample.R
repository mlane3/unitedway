#An LPsovler example
library(lpSolve)
costs <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
nr <- nrow(costs)
nc <- ncol(costs)
columns <- t(sapply(1:nc, function(x) rep(c(0, 1, 0), c(nr*(x-1), nr, nr*(nc-x)))))
rows <- t(sapply(1:nr, function(x) rep(rep(c(0, 1, 0), c(x-1, 1, nr-x)), nc)))
mod <- lp("max", as.vector(costs), rbind(columns, rows), "<=", rep(1, nr+nc), binary.vec=rep(TRUE, nr*nc))

# What else eric and I will go over when we meet ----

#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1

Value = .689
maxCWB_Z = 1.380706 # maxCWB_Z = min(df_index$CWB_Z)
minCWB_Z = -1.969282 # minCWB_Z = max(df_index$CWB_Z)
ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization

library(lpSolve)
library(lpSolveAPI)
test <- function(){
  # Set the number of vars
  model <- make.lp(0, 14)
  fx <- function(n){
    ans <- myCoef$coefficients[n]*df2["df0_ave",n] - myCoef$B[n]
    return(ans) }
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(myCoef$coefficients[1], myCoef$coefficients[2], myCoef$coefficients[3], myCoef$coefficients[4],
                     myCoef$coefficients[1], myCoef$coefficients[1], myCoef$coefficients[1], myCoef$coefficients[1],
                     myCoef$coefficients[1], myCoef$coefficients[1], myCoef$coefficients[1], myCoef$coefficients[1],
                     myCoef$coefficients[1], myCoef$coefficients[1]
                     )) #Replica of Child well being index but by sub-indexes
  # Add the constraints
  add.constraint(model, c(7/(7*3), 3/(3*3), 4/(4*3)), "<=", 0.003860932 - ValueZ)
  # add.constraint(model, c(1, 0, 0), "<", 1) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 1), "<", 1) #average x2 = .500 Family
  # add.constraint(model, c(0, 1, 0), "<", 1) #average x3 = .469 Community
  lp.control(model,sense='min')
  # Set the upper and lower bounds
  set.bounds(model, lower=c(df2$gradrate[1], df2$ccrpi[1], 0.0), upper=c(df2$gradrate[2], df2$ccrpi[2], 1.0)) #***We are using for constraints
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


# library(lpSolve)
# library(lpSolveAPI)
# test <- function(){
#   # Set the number of vars
#   model <- make.lp(0, 3)
#   # Define the object function: for Minimize, use -ve
#   set.objfn(model, c(7/(7*3), 3/(3*3), 4/(4*3))) #Replica of Child well being index but by sub-indexes
#   # Add the constraints
#   add.constraint(model, c(7/(7*3), 3/(3*3), 4/(4*3)), "<=", 0.003860932 - ValueZ)
#   # add.constraint(model, c(1, 0, 0), "<", 1) #average x1 = .518 Child
#   # add.constraint(model, c(0, 0, 1), "<", 1) #average x2 = .500 Family
#   # add.constraint(model, c(0, 1, 0), "<", 1) #average x3 = .469 Community
#   lp.control(model,sense='min')
#   # Set the upper and lower bounds
#   set.bounds(model, lower=c(0.0, 0.0, 0.0), upper=c(1.0, 1.0, 1.0)) #***We are using for constraints
#   # Compute the optimized model
#   print(solve(model))
#   # Get the value of the optimized parameters
#   print(get.variables(model)) 
#   # Get the value of the objective function
#   print(get.objective(model))
#   # Get the value of the constraint
#   print(get.constraints(model))
#   model
# }
# test()