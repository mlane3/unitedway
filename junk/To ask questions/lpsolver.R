#An LPsovler example
#There is a close form solution 
#You can take a derivative to figure which point is minimum and maxima
#df/dx = (7/7+3/3+4/4)/3 = 3/3 = 1
library(data.table)
df2 <- data.frame(Min = c(37.60854752,37.37701388,0.1,0.1,5.3,4.9,5.9,0.1,4.798530355,0.1,42.06143662,0.1,12.6,3.7),
Max = c(110.3940298,102.2361305,110.7274779,91.55844297,12.7,14.6,41,93.67152116,72.16193614,50.57695033,108.0549693,42.41675112,36.3,13.5),
Mean = c(76.97083248,70.67638458,47.05573221,32.682569,8.822164413,9.86521465,20.18908765,28.47120279,35.98333562,13.66541189,73.53394391,13.34574153,21.09052071,11.66727383),
df0_ave = c(74.00128866,69.80657216,46.03772629,33.23891753,9.273624098,10.90399884,24.06608544,30.84305541,38.48023325,13.8957549,75.05820296,12.27670284,23.39983698,12.22409549),
Deviation = c(11.73959392,10.46114783,20.86766181,18.81275014,2.800196126,8.375752635,19.90408851,20.26724702,10.86506545,11.83264369,10.64411817,9.722596219,13.9247831,6.665234405)
)
rownames(df2) <- c('gradrate','ccrpi',
                     'grade3','grade8','lbw','childnohealth',
                     'childpoverty','povertyrate','housingburden','momsnohs',
                     'collegerate','adultsnoedu','adultnohealth','unemployment')
df2 <- data.frame(t(df2))
Value = .689
maxCWB_Z <- 1.380706
minCWB_Z <- -1.969282
ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization
print((.08 - minCWB_Z)/(maxCWB_Z-minCWB_Z))
pop.Coef <- function(){
  # library(data.table)
  #Calculate df0_average
  #the df0_average (weighted by track/row/county)
  df0_ave = c(74.00129,69.80657,46.03773,33.23892,9.273624,10.904,24.06609,30.84306,38.48023,13.89575,75.0582,
      12.2767,23.39984,12.2241)
  #Calculate STDEV
  df0_sd = c(11.73959, 10.46115, 20.86766, 18.81275, 2.800196, 8.375753,
  19.90409, 20.26725, 10.86507, 11.83264, 10.64412, 9.722596, 13.92478,
  6.665234)
  names(df0_ave) = c('gradrate','ccrpi',
                     'grade3','grade8','lbw','childnohealth',
                     'childpoverty','povertyrate','housingburden','momsnohs',
                     'collegerate','adultsnoedu','adultnohealth','unemployment')
  names(df0_sd) = c('gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultsnoedu','adultnohealth','unemployment')


  
  # Define the Coefficent and Intercept B for each variable
  ChildA <- function(df0_sd){return(1/(3*7*df0_sd))} #Child Coefficents
  FamilyA <- function(df0_sd){ return(1/(3*3*df0_sd)) } #Family Coefficents
  ComA <- function(df0_sd){ return(1/(3*4*df0_sd)) } #Community Coefficents
  
  ChildB <- function(df0_ave,df0_sd){ return(df0_ave/(3*7*df0_sd)) } #Child Intercepts
  FamilyB <- function(df0_ave,df0_sd){ return(df0_ave/(3*3*df0_sd)) } #Family Intercepts
  ComB <- function(df0_ave,df0_sd){ return(df0_ave/(3*4*df0_sd)) } #Community Intercepts
  
  A <- list(
    ChildA(df0_sd["gradrate"]),ChildA(df0_sd["ccrpi"]),
    ChildA(df0_sd["grade3"]),ChildA(df0_sd["grade8"]),
    -1*ChildA(df0_sd["lbw"]),-1*ChildA(df0_sd["childnohealth"]),
    -1*ChildA(df0_sd["childpoverty"]),
    -1*FamilyA(df0_sd["povertyrate"]),
    -1*FamilyA(df0_sd["housingburden"]),
    -1*FamilyA(df0_sd["momsnohs"]),
    ComA(df0_sd["collegerate"]),
    -1*ComA(df0_sd["adultsnoedu"]),
    -1*ComA(df0_sd["adultnohealth"]),
    -1*ComA(df0_sd["unemployment"]))
  B <- list(
    ChildB(df0_ave["gradrate"],df0_sd["gradrate"]),
    ChildB(df0_ave["ccrpi"],df0_sd["ccrpi"]),
    ChildB(df0_ave["grade3"],df0_sd["grade3"]),
    ChildB(df0_ave["grade8"],df0_sd["grade8"]),
    -1*ChildB(df0_ave["lbw"],df0_sd["lbw"]),
    -1*ChildB(df0_ave["childnohealth"],df0_sd["childnohealth"]),
    -1*ChildB(df0_ave["childpoverty"],df0_sd["childpoverty"]),
    -1*FamilyB(df0_ave["povertyrate"],df0_sd["povertyrate"]),
    -1*FamilyB(df0_ave["housingburden"],df0_sd["housingburden"]),
    -1*FamilyB(df0_ave["momsnohs"],df0_sd["momsnohs"]),
    ComB(df0_ave["collegerate"],df0_sd["collegerate"]),
    -1*ComB(df0_ave["adultsnoedu"],df0_sd["adultsnoedu"]),
    -1*ComB(df0_ave["adultnohealth"],df0_sd["adultnohealth"]),
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"])
  )
  rownames(A)
  names = c('gradrate','ccrpi',
            'grade3','grade8','lbw','childnohealth',
            'childpoverty','povertyrate','housingburden','momsnohs',
            'collegerate','adultsnoedu','adultnohealth','unemployment')
  df0_coeff <- data.frame(name = names,
                          A = as.numeric(A),
                          B = as.numeric(B))
  rm(B,A,ChildA,ChildB,FamilyA,FamilyB,ComA,ComB)
  return(df0_coeff)
}

mycoef <- NULL
mycoef <- as.data.table(pop.Coef()) #To Mike fix pop.Coef later!!
names(mycoef) <- c("names","A","B") #I rename this table to simplify

library(lpSolve)
library(lpSolveAPI)

fx <- function(n){
  #This is function just to show you how CWB Z-score is caculated
  ans <- mycoef$A[n]*df2["df0_ave",n] - mycoef$B[n]
  return(ans) }

# Current lpSolve version - ---
lptest <- function(df2,initial){
  # Fix data so minimums are not zero To Mike fix so not needed
  # df2 need to be replaced with better bounds!
  # grangertest(formula, data = list(), ...)
  model <- make.lp(0, 15)
  # Define the object function: for Minimize, use -ve or lp.control ----
  set.objfn(model, c(mycoef$A[1],mycoef$A[2],mycoef$A[3],mycoef$A[4],
                     mycoef$A[5],mycoef$A[6],mycoef$A[7],mycoef$A[8],  
                     mycoef$A[9],mycoef$A[10],mycoef$A[11],mycoef$A[12],
                     mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))
                    # The actual Child well being index but linearized
                    # 0.003860932 is average Z-score of the orignal data
  # Set the upper and lower bounds ----
  set.bounds(model,
             lower=c(df2$gradrate[1], df2$ccrpi[1], df2$grade3[1], df2$grade8[1], df2$lbw[1],
                     df2$childnohealth[1], df2$childpoverty[1], df2$povertyrate[1], df2$housingburden[1],
                     df2$momsnohs[1], df2$collegerate[1], df2$adultsnoedu[1], df2$adultnohealth[1], df2$unemployment[1],.90),
             upper=c(df2$gradrate[2], df2$ccrpi[2], df2$grade3[2], df2$grade8[2], df2$lbw[2],
                     df2$childnohealth[2], df2$childpoverty[2], df2$povertyrate[2], df2$housingburden[2],
                     df2$momsnohs[2], df2$collegerate[2], df2$adultsnoedu[2], df2$adultnohealth[2], df2$unemployment[2],1.0)) #***We are using for constraints
  # 1/n*coeff is how we will add it for multple contraints A*x = Y +B
  add.constraint(model, (c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
                          mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
                          mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
                          mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))^2, "<=", -(0.00386-ValueZ))
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
  
  # Compute the optimized model ----
  lp.control(model,sense='min', verbose='normal') #
  model3 <<- model 
  print("start model: 0 done and 2 is infeasible, use verbose = 'normal' to get full output")
  # model0 <<- model
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
final <- lptest(df2) #lptest takes in original_constrants or df2
final <- final[1:14]
CWBZ <- rowSums(mycoef$A*t(final) - mycoef$B)
CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
print(paste0("CWB Index equals"," ",CWBI))
print(final - df2["df0_ave",])