#An LPsovler example

#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1
df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

Value = .689
maxCWB_Z = max(df_index$CWB_Z) # Value is 1.380706
minCWB_Z = min(df_index$CWB_Z) # -1.969282
ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization
print((.08 - minCWB_Z)/(maxCWB_Z-minCWB_Z))
source("model/coefficents.R")
mycoef <- NULL
mycoef <- as.data.table(pop.Coef(df0)) #To Mike fix pop.Coef later!!
names(mycoef) <- c("names","coeff","B") #I rename this table to simplify

library(lpSolve)
library(lpSolveAPI)

fx <- function(n){
  #This is function just to show you how CWB Z-score is caculated
  ans <- mycoef$coeff[n]*df2["df0_ave",n] - mycoef$B[n]
  return(ans) }

# Current lpSolve version - ---
lptest <- function(df2){
  # Set the number of vars
  model <- make.lp(0, 14)
  # Fix data so minimums are not zero To Mike fix so not needed
  # df2 need to be replaced with better bounds!
  for (i in 1:length(df2)) {
    if(df2[1,i] == 0){
      myname <- names(df2[i])
      df2[1,i] <- df2["df0_ave",i]
      df2["df0_ave",i]
      if (df2["df0_ave",i] == 0)
        df2[1,i] <- 1
    }
  }
  
  print("hello")
  # Define the object function: for Minimize, use -ve or lp.control
  set.objfn(model, c(mycoef$coeff[1],mycoef$coeff[2],mycoef$coeff[3],mycoef$coeff[4],
                     mycoef$coeff[5],mycoef$coeff[6],mycoef$coeff[7],mycoef$coeff[8],  
                     mycoef$coeff[9],mycoef$coeff[10],mycoef$coeff[11],mycoef$coeff[12],
                     mycoef$coeff[13],mycoef$coeff[14]))
                    # The actual Child well being index but linearized
                    #0.003860932 is average Z-score of the orignal data
  # Add the constraints -sum(mycoef$B)
  # 1/n*coeff is how we will add it for multple contraints A*x = Y +B 
  add.constraint(model, c(mycoef$coeff[1],mycoef$coeff[2], mycoef$coeff[3],mycoef$coeff[4],
                          mycoef$coeff[5],mycoef$coeff[6], mycoef$coeff[7], mycoef$coeff[8],  
                          mycoef$coeff[9], mycoef$coeff[10],mycoef$coeff[11],mycoef$coeff[12],
                          mycoef$coeff[13],mycoef$coeff[14]), "<=", (0.00386-ValueZ+sum(mycoef$B)))^2
  # Add New Constraints: Hypothesis Auxilary Regressions ----
  # Here are the equation written out from the power point slides
  # -6.408 >= 0.0474*childpoverty+0.562*povertyrate+0.577*momsnohs+0.311*adultsnoedu - adultsnohealth 
  # -1.26 >= 0.005*grade3+0.105*povertyrate+0.583*momsnohs+residualC - adultsnoedu
  # 0.302 >= -0.302*povertyrate+0.087*childpoverty+residualB - momsnohs
  # 4.38 >= -0.397*povertyrate+0.13*housingburnden+residualA - childpoverty
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0.0474, 0.562, 0, 0.577, 0, 0.311, -1, 0), ">=", -6.408) #adulthealth insurance
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, -1, -0.397, 0.13, 0, 0, 0, 0, 0), "<=", 4.38) #childpoverty
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0.087, -0.302, 0, -1, 0, 0, 0, 0), "<=", 0.302) #momsnohs
  # add.constraint(model, c(0, 0, 0.005, 0, 0, 0, 0, 0.105, 0, 0.583, 0, -1, 0, 0), ">=", -1.26) #adultsnoedu
  # Rest of the Model ----
  add.constraint(model, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$gradrate[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$ccrpi[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$grade3[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$grade8[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$lbw[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), ">=", df2$childnohealth[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), ">=", df2$childpoverty[1]) #average x1 = .518 Child
  add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), ">=", df2$povertyrate[1]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), ">=", df2$housingburden[1]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), ">=", df2$momsnohs[1]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), ">", df2$collegerate[1]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), ">", df2$adultsnoedu[1]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), ">", df2$adultnohealth[1]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), ">", df2$unemployment[1]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 1), "<", 1) #average 
  # add.constraint(model, c(0, 1, 0), "<", 1) #average x3 = .469 Community
  
  lp.control(model,sense='min',verbose='normal')
  model3 <<- model
  # Set the upper and lower bounds
  set.bounds(model,
             lower=c(df2$gradrate[1], df2$ccrpi[1], df2$grade3[1], df2$grade8[1], df2$lbw[1],
                     df2$childnohealth[1], df2$childpoverty[1], df2$povertyrate[1], df2$housingburden[1],
                     df2$momsnohs[1], df2$collegerate[1], df2$adultsnoedu[1], df2$adultnohealth[1], df2$unemployment[1]),
             upper=c(df2$gradrate[2], df2$ccrpi[2], df2$grade3[2], df2$grade8[2], df2$lbw[2],
                     df2$childnohealth[2], df2$childpoverty[2], df2$povertyrate[2], df2$housingburden[2],
                     df2$momsnohs[2], df2$collegerate[2], df2$adultsnoedu[2], df2$adultnohealth[2], df2$unemployment[2])) #***We are using for constraints
  # Compute the optimized model
  print("start model: 0 done and 2 is infeasible, use verbose = 'normal' to get full output")
  # model0 <<- model
  solve(model)
  print(solve(model))
  # model1 <<- model
  # Get the value of the optimized parameters
  print("final results")
  print(get.variables(model)) 
  # print(get.bounds(model))
  # Get the value of the objective function
  print(get.objective(model))
  # Get the value of the constraint
  # print(get.constraints(model))
  return(get.variables(model))
}
#This is a script to show the output of lptest()
final <- lptest(df2) #lptest takes in original_constrants or df2
final <- final[1:14]
CWBZ <- rowSums(mycoef$coeff*t(final) - mycoef$B)
CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
print(paste0("CWB Index equals"," ",CWBI))
print(final - df2["Mean",])

library(lpSolve)

#This is the orignal lpsolve example ----
library(lpSolve)
library(lpSolveAPI)
test <- function(){
  # Set the number of vars
  model <- make.lp(0, 3)
  # Define the object function: for Minimize, use -ve
  set.objfn(model, c(7/(7*3), 3/(3*3), 4/(4*3))) #Replica of Child well being index but by sub-indexes
  # Add the constraints
  add.constraint(model, c(7/(7*3), 3/(3*3), 4/(4*3)), "<=", 0.003860932 - ValueZ)
  # add.constraint(model, c(1, 0, 0), "<", 1) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 1), "<", 1) #average x2 = .500 Family
  # add.constraint(model, c(0, 1, 0), "<", 1) #average x3 = .469 Community
  lp.control(model,sense='min')
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
  model2 <<- model
}
test()



# This is an lpsolve() example code I need to delete ----
# costs <- matrix(c(9, 10, 11, 4, 5, 10, 1, 3, 5, 7, 5, 4), nrow=3)
# nr <- nrow(costs)
# nc <- ncol(costs)
# columns <- t(sapply(1:nc, function(x) rep(c(0, 1, 0), c(nr*(x-1), nr, nr*(nc-x)))))
# rows <- t(sapply(1:nr, function(x) rep(rep(c(0, 1, 0), c(x-1, 1, nr-x)), nc)))
# mod <- lp("max", as.vector(costs), rbind(columns, rows), "<=", rep(1, nr+nc), binary.vec=rep(TRUE, nr*nc))
