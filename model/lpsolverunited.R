#An LPsovler example
#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1
# mydata <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

Value = .689
maxCWB_Z <- 1.380706 # max(df_index$CWB_Z)  
minCWB_Z <- -1.969282 # min(df_index$CWB_Z) 
ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization
# print((.08 - minCWB_Z)/(maxCWB_Z-minCWB_Z))
source("model/coefficents.R")
mycoef <- NULL
mycoef <- as.data.table(pop.Coef(df0)) #To Mike fix pop.Coef later!!
names(mycoef) <- c("names","A","B") #I rename this table to simplify

library(lpSolve)
library(lpSolveAPI)

fx <- function(n){
  #This is function just to show you how CWB Z-score is caculated
  ans <- mycoef$A[n]*mydata["df0_ave",n] - mycoef$B[n]
  return(ans) }

# Current lpSolve version - ---
lptest <- function(mydata){
  # Fix data so minimums are not zero To Mike fix so not needed
  # mydata need to be replaced with better bounds!
  print(mydata[1,1])
  for (i in 1:length(mydata)) {
    if(mydata[1,i] == 0){
      myname <- names(mydata[i])
      mydata[1,i] <- abs(min(df0[,i+2]))
      min(df0[,i+2])
      if (min(df0[,i+2]) == 0)
        mydata[1,i] <- 1
    }
  }
  
  # Set the number of vars
  model <- make.lp(0, 15)
  # Define the object function: for Minimize, use -ve or lp.control ----
  set.objfn(model, c(mycoef$A[1],mycoef$A[2],mycoef$A[3],mycoef$A[4],
                     mycoef$A[5],mycoef$A[6],mycoef$A[7],mycoef$A[8],  
                     mycoef$A[9],mycoef$A[10],mycoef$A[11],mycoef$A[12],
                     mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))
                    # The actual Child well being index but linearized
                    # 0.003860932 is average Z-score of the orignal data
  # Set the upper and lower bounds ----
  set.bounds(model, #mydata$gradrate[1],
             lower=c(mydata$gradrate[1], mydata$ccrpi[1], mydata$grade3[1], mydata$grade8[1], mydata$lbw[1],
                     mydata$childnohealth[1], mydata$childpoverty[1], mydata$povertyrate[1], mydata$housingburden[1],
                     mydata$momsnohs[1], mydata$collegerate[1], mydata$adultsnoedu[1], mydata$adultnohealth[1], mydata$unemployment[1],.90),
             upper=c(mydata$gradrate[1], mydata$ccrpi[2], mydata$grade3[2], mydata$grade8[2], mydata$lbw[2],
                     mydata$childnohealth[2], mydata$childpoverty[2], mydata$povertyrate[2], mydata$housingburden[2],
                     mydata$momsnohs[2], mydata$collegerate[2], mydata$adultsnoedu[2], mydata$adultnohealth[2], mydata$unemployment[2],1.0)) #***We are using for constraints
  # intialize model ----
  # add.constraint(model, c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
  #                             mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
  #                             mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
  #                             mycoef$A[13],mycoef$A[14],-sum(mycoef$B)), "=", 1.04455e-10)
  # rowSums(mycoef$A*mydata["df0_ave",] - mycoef$B)
  # add.constraint(model, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$gradrate[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$ccrpi[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$grade3[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$grade8[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$lbw[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$childnohealth[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), "=", mydata$childpoverty[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), "=", mydata$povertyrate[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), "=", mydata$housingburden[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), "=", mydata$momsnohs[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), "=", mydata$collegerate[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), "=", mydata$adultnoedu[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), "=", mydata$adultsnohealth[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), "=", mydata$unemployment[4]) # x3 = .469 Community
  # solve(model)
  # length(get.constraints(model))
  # delete.constraint(model,1:length(get.constraints(model)))
  # print(model)
  # Add the constraints -sum(mycoef$B) ----
  # # 1/n*coeff is how we will add it for multple contraints A*x = Y +B
  # add.constraint(model, (c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
  #                         mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
  #                         mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
  #                         mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))^2, "<=", (0.00386-ValueZ)^2) #0.00386
  
  add.constraint(model, c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
                          mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
                          mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
                          mycoef$A[13],mycoef$A[14],0), ">=", (ValueZ+sum(mycoef$B)))
  # Where does 0.00386 come from?
  # add.constraint(model, (c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
  #                          mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
  #                          mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
  #                          mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))^2, "=", (0.00386))
  add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), "=", 1)
  # Add New Constraints: Hypothesis Auxilary Regressions ----
  # Here are the equation written out from the power point slides
  # -6.408 >= 0.0474*childpoverty+0.562*povertyrate+0.577*momsnohs+0.311*adultnoedu - adultsnohealth 
  # -1.26 >= 0.005*grade3+0.105*povertyrate+0.583*momsnohs+residualC - adultnoedu
  # 0.302 >= -0.302*povertyrate+0.087*childpoverty+residualB - momsnohs
  # 4.38 >= -0.397*povertyrate+0.13*housingburnden+residualA - childpoverty
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0.0474, 0.562, 0, 0.577, 0, 0.311, -1, 0, 0), ">=", -6.408) #adulthealth insurance
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, -1, -0.397, 0.13, 0, 0, 0, 0, 0, 0), "<=", 4.38) #childpoverty
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0.087, -0.302, 0, -1, 0, 0, 0, 0, 0), "<=", 0.302) #momsnohs
  # add.constraint(model, c(0, 0, 0.005, 0, 0, 0, 0, 0.105, 0, 0.583, 0, -1, 0, 0, 0), ">=", -1.26) #adultnoedu
  # Rest of the Model ----
  # add.constraint(model, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", mydata$gradrate[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", mydata$ccrpi[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", mydata$grade3[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", mydata$grade8[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", mydata$lbw[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0), "<=", mydata$childnohealth[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0), "<=", mydata$childpoverty[4]) #average x1 = .518 Child
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0), "<=", mydata$povertyrate[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0), "<=", mydata$housingburden[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0), "<=", mydata$momsnohs[4]) # x2 = .500 Family
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0), ">=", mydata$collegerate[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0), "<=", mydata$adultnoedu[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), "<=", mydata$adultsnohealth[4]) # x3 = .469 Community
  # add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0), "<=", mydata$unemployment[4]) # x3 = .469 Community
  # Compute the optimized model ----
  lp.control(model,sense='min', verbose='normal') #
  model3 <<- model 
  print("start model: 0 done and 2 is infeasible, use verbose = 'normal' to get full output")
  solve(model)
  print(solve(model))
  # Get the value of the optimized parameters
  print("final results")
  print(get.variables(model)) 
  # Get the value of the objective function
  print(get.objective(model))
  # Get the value of the constraint
  # print(get.constraints(model))
  # print(get.bounds(model))
  return(get.variables(model))
}
#This is a script to show the output of lptest()
# final <- lptest(df2) #lptest takes in original_constrants or mydata
# final <- final[1:14]
# CWBZ <- rowSums(mycoef$A*t(final) - mycoef$B)
# CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
# print(paste0("CWB Index equals"," ",CWBI))
# print(final - mydata["df0_ave",])