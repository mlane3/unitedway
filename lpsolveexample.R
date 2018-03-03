# Load in the original script.  We need df0 and dfindex from it
source('UW_R_Script_final.R')
# Load in the coeffcients. I have written the algorithm
source('coefficents.R')
# coefficents.R contains some test script and you don't need the excess stuff
rm(dfNorm)

#  What eric and I will go over when we meet ----
# LPsovler example we are working from ----
library(lpSolve)
library(lpSolveAPI)
Value = .689
maxCWB_Z = 1.380706 # maxCWB_Z = min(df_index$CWB_Z)
minCWB_Z = -1.969282 # minCWB_Z = max(df_index$CWB_Z)
Value = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z

test <- function(){
  # Set the number of vars
  model <- make.lp(0, 14)
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(7/(7*3), 3/(3*3), 4/(4*3))) #Replica of Child well being index but by sub-indexes
  # Add the constraints
  add.constraint(model, c(7/(7*3), 3/(3*3), 4/(4*3)), "<=", Value)
  add.constraint(model, c(1, 0, 0), ">", 0) #average x1 = .518
  add.constraint(model, c(0, 0, 1), ">", 0) #average x2 = .500
  add.constraint(model, c(0, 1, 0), ">", 0) #average x3 = .469
  lp.control(model,sense='max')
  # Set the upper and lower bounds
  set.bounds(model, lower=c(df2$gradrate[1], 0.0, 0.0), upper=c(1.0, 1.0, 1.0)) #***We are using for constraints
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