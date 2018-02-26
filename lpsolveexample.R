#An LPsovler example we are working from
library(lpSolve)
costs <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
nr <- nrow(costs)
nc <- ncol(costs)
columns <- t(sapply(1:nc, function(x) rep(c(0, 1, 0), c(nr*(x-1), nr, nr*(nc-x)))))
rows <- t(sapply(1:nr, function(x) rep(rep(c(0, 1, 0), c(x-1, 1, nr-x)), nc)))
mod <- lp("max", as.vector(costs), rbind(columns, rows), "<=", rep(1, nr+nc), binary.vec=rep(TRUE, nr*nc))

#There is a close form solutoin 
#You can take a derivative (which pioint is minimum and maxima)
#df/dx = 3/3 = 1

Value = .689

 library(lpSolve)
 library(lpSolveAPI)
 # Set the number of vars
   model <- make.lp(0, 3)
 # Define the object function: for Minimize, use -ve
   set.objfn(model, c(-0.05, -0.04, -0.06)) #Replica of Child well being index
# Add the constraints
add.constraint(model, c(1, 1, 1), "=", 1)
add.constraint(model, c(1, 1, -1), "", 0)
add.constraint(model, c(1, -2, 0), "<", 0)
# Set the upper and lower bounds
   set.bounds(model, lower=c(0.1, 0.1, 0.1), upper=c(1, 1, 1)) #***We are using
 # Compute the optimized model
   solve(model) #[1] 0
 # Get the value of the optimized parameters
   get.variables(model) #[1] 0.3333333 0.1666667 0.5000000
 # Get the value of the objective function
   get.objective(model) #[1] -0.05333333
 # Get the value of the constraint
   get.constraints(model) #[1] 1 0 0