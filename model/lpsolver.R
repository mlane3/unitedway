"*****************
   THE LP SOLVER
******************"
#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1
# mydata <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

# Intialization ----
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


# Current lpSolve OPtimizer ----
lptest <- function(mydata,variablenamelist){
  # You can ignore this loop
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
  
  # Set the number of variables ----
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
             upper=c(mydata$gradrate[2], mydata$ccrpi[2], mydata$grade3[2], mydata$grade8[2], mydata$lbw[2],
                     mydata$childnohealth[2], mydata$childpoverty[2], mydata$povertyrate[2], mydata$housingburden[2],
                     mydata$momsnohs[2], mydata$collegerate[2], mydata$adultsnoedu[2], mydata$adultnohealth[2], mydata$unemployment[2],1.0)) #***We are using for constraints
  # FOR TOYIN Add Fixed/Not fixed constraints ----
  I = diag(15)
  for(i in 1:14){
    if(variablenamelist$plotbutton[i] !=0){
      print(I[i,])
      add.constraint(model, I[i,], "=", variablenamelist$plotbutton[i])
    }
    else{
      #add.constraint(model, I[i,], ">", mydata[unlist(variablenamelist[i,1]),1])
    }
  }

  # FOR TOYIN The other fixed constraints ----
  # add.constraint(model, c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), ">=", mydata$gradrate[4]) #average x1 = .518 Child
  
  # Rest of the Model ----  
  # # 1/n*coeff is how we will add it for multple contraints A*x = Y +B
  # add.constraint(model, (c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
  #                         mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
  #                         mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
  #                         mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))^2, "<=", (0.00386-ValueZ)^2) #0.00386

  add.constraint(model, c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
                          mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
                          mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
                          mycoef$A[13],mycoef$A[14],0), ">=", (ValueZ+sum(mycoef$B)))
  #add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), "=", 1)
  # add.constraint(model, c(0.46602304, -1, 0.01449261, 0.23347020, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 26.95612297), "=", 0)
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

# final <- lptest(df2) #lptest takes in original_constrants or df2
# final <- final[1:14]
# CWBZ <- rowSums(mycoef$A*t(final) - mycoef$B)
# CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
# test <- data.frame(CWBI = CWBI, final = final)
# print(paste0("CWB Index equals"," ",CWBI))
# print(final) #print(final - df2["df0_ave",])

#mydata[unlist(variablenamelist[i,1])
#add.constraint(model, I[i,], ">=", mydata[unlist(variablenamelist[i,1]),1])
