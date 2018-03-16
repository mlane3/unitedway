#An LPsovler example
library(lpSolve)
# costs <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
# nr <- nrow(costs)
# nc <- ncol(costs)
# columns <- t(sapply(1:nc, function(x) rep(c(0, 1, 0), c(nr*(x-1), nr, nr*(nc-x)))))
# rows <- t(sapply(1:nr, function(x) rep(rep(c(0, 1, 0), c(x-1, 1, nr-x)), nc)))
# mod <- lp("max", as.vector(costs), rbind(columns, rows), "<=", rep(1, nr+nc), binary.vec=rep(TRUE, nr*nc))

# Current lpSolve version ----

#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1
df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

Value = .689
maxCWB_Z = maxCWB_Z = min(df_index$CWB_Z) # Value is 1.380706
minCWB_Z = minCWB_Z = max(df_index$CWB_Z) # -1.969282
ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization

source("model/coefficents.R")
mycoef <- NULL
mycoef <- as.data.table(pop.Coef(df0)) #MIke: fix this!
names(mycoef) <- c("names","coeff","B") #I rename this table to simplify


library(lpSolve)
library(lpSolveAPI)


lptest <- function(){
  # Set the number of vars
  model <- make.lp(0, 14)
  fx <- function(n){
    #This is writting just to show you how CWB Z-score is caculated
    ans <- mycoef$coeff[n]*df2["df0_ave",n] - mycoef$B[n]
    return(ans) }
  for (i in 1:nrow(df2)) {
    if(df2[1,i] == 0){
      df2[1,i] <- min(df0[,i+2])
    }
  }
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(mycoef$coeff[1],mycoef$coeff[2], 
                     mycoef$coeff[3],mycoef$coeff[4],
                     mycoef$coeff[5],
                     mycoef$coeff[6],
                     mycoef$coeff[7], 
                     mycoef$coeff[8],  
                     mycoef$coeff[9], 
                     mycoef$coeff[10], 
                     mycoef$coeff[11], 
                     mycoef$coeff[12],
                     mycoef$coeff[13],
                     mycoef$coeff[14]))
                    # Replica of Child well being index but by sub-indexes
                    #0.003860932 #average Z scofre
  # Add the constraints
  # 1/n*coeff
  add.constraint(model, c(mycoef$coeff[1],mycoef$coeff[2], mycoef$coeff[3],mycoef$coeff[4],
                          mycoef$coeff[5],mycoef$coeff[6], mycoef$coeff[7], mycoef$coeff[8],  
                          mycoef$coeff[9], mycoef$coeff[10],mycoef$coeff[11],mycoef$coeff[12],mycoef$coeff[13],
                          mycoef$coeff[14]-sum(mycoef$B)), "<=",  ValueZ - 0.003860932)
  # add.constraint(model, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$gradrate[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$ccrpi[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$grade3[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$grade8[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$lbw[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), ">", df2$childnohealth[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), ">", df2$childpoverty[1]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), ">", df2$povertyrate[1]) 
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), ">", df2$housingburden[1]) 
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), ">", df2$momsnohs[1]) 
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), ">", df2$collegerate[1]) 
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), ">", df2$adultsnoedu[1]) 
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), ">", df2$adultnohealth[1])
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), ">", df2$unemployment[1])
  # add.constraint(model, c(0, 0, 1), "<", 1) #average x2 = .500 Family
  # add.constraint(model, c(0, 1, 0), "<", 1) #average x3 = .469 Community
  lp.control(model,sense='min')
  # Set the upper and lower bounds
  set.bounds(model,
             lower=c(df2$gradrate[1], df2$ccrpi[1], df2$grade3[1], df2$grade8[1], df2$lbw[1],
                     df2$childnohealth[1], df2$childpoverty[1], df2$povertyrate[1], df2$housingburden[1], 
                     df2$momsnohs[1], df2$collegerate[1], df2$adultsnoedu[1], df2$adultnohealth[1], df2$unemployment[1]),
             upper=c(df2$gradrate[2], df2$ccrpi[2], df2$grade3[2], df2$grade8[2], df2$lbw[2],
                     df2$childnohealth[2], df2$childpoverty[2], df2$povertyrate[2], df2$housingburden[2],
                     df2$momsnohs[2], df2$collegerate[2], df2$adultsnoedu[2], df2$adultnohealth[2], df2$unemployment[2])) #***We are using for constraints
  # Compute the optimized model
  print("hello")
  solve(model)
  # Get the value of the optimized parameters
  print(get.variables(model)) 
  # Get the value of the objective function
  print(get.objective(model))
  # Get the value of the constraint
  print(get.constraints(model))
  print(model)
  return(get.variables(model))
}
lineartest <- lptest()
lineartest <- rowSums(mycoef$coeff*t(test2) - mycoef$B)
lineartest <- 100*(test2 - minCWB_Z)/(maxCWB_Z - minCWB_Z)
print(paste0("test equals"," ",test2))

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