

#Intialize ----
# library(data.table)
# overall_constraints <- df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
# variablenamelist <- as.data.frame(data.table( variable = c( "gradrate",
#                                                             "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "childpoverty",
#                                                             "povertyrate", "housingburden", "momsnohs", "collegerate", "adultsnoedu",
#                                                             "adultnohealth", "unemployment" ), number = 1:14, title = c( 'S: HS Graduation
#                                                                                                                          Rate', 'S: HS College&Career Readiness', 'S: % Exceed 3rd Gr Reading Std', 'S:
#                                                                                                                          % Exceed 8th Gr Math Std', 'S: % Low Weight Births', 'S: % Children w/o Health
#                                                                                                                          Ins', 'S: % Children in Poverty', 'S: % Families Not Finan Stable', 'S: %
#                                                                                                                          Families w/Housing Burden', 'S: % Moms w/o HS Diploma', 'S: % Enrolled
#                                                                                                                          Post-Second Educ', 'S: % Adults w/o HS Diploma', 'S: % Adults w/o Health Ins',
#                                                                                                                          'S: Unemployment Rate'), plotbutton = c(0,rep(0,13)), starttitle = c( 'S: HS
# Graduation Rate', 'S: HS College&Career Readiness', 'S: % Exceed 3rd Gr
# Reading Std', 'S: % Exceed 8th Gr Math Std', 'S: % Low Weight Births', 'S: %
# Children w/o Health Ins', 'S: % Children in Poverty', 'S: % Families Not Finan
# Stable', 'S: % Families w/Housing Burden', 'S: % Moms w/o HS Diploma', 'S: %
# Enrolled Post-Second Educ', 'S: % Adults w/o HS Diploma', 'S: % Adults w/o
# Health Ins', 'S: Unemployment Rate'), resulttitle = c( 'R: HS  Graduation
# Rate', 'R: HSCollege&Career Readiness', 'R: % Exceed 3rd Gr Reading Std', 'R:
# % Exceed 8th Gr Math Std', 'R:  % Low Weight Births', 'R: % Children w/o
# Health Ins', 'R: % Children in Poverty', 'R: % Families Not Finan Stable', 'R:
# % Families w/Housing Burden', 'R: % Moms w/o HS Diploma', 'R: % Enrolled
# Post-Second Educ', 'R: % Adults w/o HS Diploma', 'R: % Adults w/o Health Ins',
#                                                        'R: Unemployment Rate')) )
new.Coef <- function(){
  # library(data.table)
  #Calculate df0_average
  #the df0_average (weighted by track/row/county)
  df0_ave = c(74.00129,69.80657,46.03773,33.23892,9.273624,10.904,24.06609,30.84306,38.48023,13.89575,75.0582,
              12.2767,23.39984,12.2241)
  #Calculate STDEV #This are the orginal standard deviations of the 2014 that we fix to compare
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
new.lp <- function(df0_ave){
  mycoef <- NULL
  mycoef <- as.data.table(new.Coef()) #To Mike fix new.Coef later!!
  names(mycoef) <- c("name","A","B") #I rename this table to simplify
  Value = .689
  maxCWB_Z <- 1.380706
  minCWB_Z <- -1.969282
  ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization
  x <- mycoef$A*df0_ave
  #CWBZ <- base::rowSums(mycoef$A*df0_ave - mycoef$B)
  CWBZ <- sum(x - mycoef$B)
  CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
  x1 <- (CWBZ - ValueZ)
  x2 <- (x1)^2
  CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
  CWBI2 <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
  return(x2)
}

# The New Optimizer ---------------------------------------------------------------


optim_solver <- function(mydata,variablenamelist){
  # Define variables ----
  x <- mydata["df0_ave",]
  lower <- mydata["Min",]
  upper <- mydata["Max",]
  df0_sd = c(11.73959, 10.46115, 20.86766, 18.81275, 2.800196, 8.375753,
             19.90409, 20.26725, 10.86507, 11.83264, 10.64412, 9.722596,
             13.92478, 6.665234) #instead of mydata["Deviation",] 
  #Parscale: controls how fast the algorithm processes. ----
  #parscale = rep.int(23,length(x))
  parscale = df0_sd #we need to normalize certain optimization algorithms so parscale required!
  #p arscale also speed up the algorithm of CG and Nelder-Mead
  #We are using deviation as a scaling or attr(scale(orginal),"scaled:scale")
  # As an alternative, could we derive a a scaling from min/max or feature scaling?
  
  means <- numeric()
  # Step 0: Futher define Bounds ----
  for(i in 1:14){
    if(variablenamelist$plotbutton[i] !=0){
      # Throw Error if user enters below minimum or above the maximum boundary
      message = paste("Error",variablenamelist$starttitle[i],"must be between",
                      mydata["Min",i],"&",mydata["Max",i],"OR zero (not a fixed constraint)")
      validate(need(variablenamelist$plotbutton[i] > mydata["Min",i],message))
      validate(need(variablenamelist$plotbutton[i] < mydata["Max",i],message))
      # Add constraint to fix indicator
      mydata[i] <- variablenamelist$plotbutton[i]
      upper[i] <- variablenamelist$plotbutton[i] + .001
      lower[i] <- variablenamelist$plotbutton[i] - .001
    }
   }
  
  # Step 1: Find the Best Unbound Optimization Model ----
  ##Hint the answer to this just so happens to be x2_3 
  ##x2_1 to x2_10 are all tests of different optimizations conditions Beside x4,
  ##most of the one suppress don't give good results. The idea was to pull to
  ##pick the best test by something like RMSE But I didn't have time to code it.

  # x2_1 <- optim(x, new.lp, NULL, method = "BFGS", control = list(parscale = parscale), hessian = TRUE)
  # means <- append(means,as.numeric(base::rowMeans((x - x2_1$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_1$par)/x*100)))
  # x2_2 <- optim(x, new.lp, method = "CG", control = list(parscale = parscale))
  # means <- append(means,as.numeric(base::rowMeans((x - x2_2$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_2$par)/x*100)))
  x2_3 <- optim(x, new.lp, method = "CG", control = list(maxit=200,type = 2,parscale = parscale))
  means <- append(means,as.numeric(base::rowMeans((x - x2_3$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_3$par)/x*100)))
  # x2_4 <- optim(x, new.lp, method = "L-BFGS-B", control = list(parscale = parscale))
  # plot(stats::density(as.numeric((x-x2_4$par)/x*100)))
  # means <- append(means,as.numeric(base::rowMeans((x - x2_4$par)/x)*100))
  x2_5 <- optim(x, new.lp, method="Nelder-Mead", control= list(maxit=500,parscale = parscale))
  means <- append(means,as.numeric(base::rowMeans((x - x2_5$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_5$par)/x*100)))
  # x2_7 <- optim(x,new.lp,method='SANN',control = list(maxit = 20000, temp = 2, parscale = parscale))
  # x2_8 <- optim(x,new.lp,method='SANN',control = list(maxit = 20000, temp = 2, parscale = parscale))
  # plot(stats::density(as.numeric((x-x2_8$par)/x*100)))
  # means <- append(means,as.numeric(base::rowMeans((x - x2_8$par)/x)*100))
  # x2_9 <- optim(x,new.lp,method="Nelder-Mead",hessian = TRUE,control = list(parscale = parscale))
  # means <- append(means,as.numeric(base::rowMeans((x - x2_9$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_9$par)/x*100)))
  # x2_10 <- optim(x,new.lp,method='SANN',hessian = TRUE,control = list(parscale = parscale))
  # means <- append(means,as.numeric(base::rowMeans((x - x2_10$par)/x)*100))
  # plot(stats::density(as.numeric((x-x2_10$par)/x*100)))
  # plot(as.numeric((x-x2_1$par)))
  # plot(as.numeric((x-x2_2$par)))
  # plot(as.numeric((x-x2_4$par)))
  # plot(as.numeric((x-x2_3$par)))
  # plot(as.numeric((x-x2_5$par)))
  # plot(as.numeric((x-x2_10$par)))

  
  # Step 2: Use the best optimizer ----
  # ##These are the ways based on research that I found to compare optimizers
  # ##0) An error metric like stdev, RSME, or mean absolute deviation
  # ##1) RPD relative percentage deviation = (Alg - Minimum Solution)/(Minimum Solution)
  # ##2) RPI relative deviation index = (Alg - min sol) / (max sol - min sol) * 100
  # ##combination Pareto set is constructed and the percentage of the solution
  # #3belonging to each test is computed (Quality metric). Closeness to the true
  # ##Pareto front, also referred as convergence to the known Pareto optimal
  # ##front, can be measured using distance from true Pareto front
  # 
  # 
  # ##We have already calculated these absolute mean deviations. So here is the min:
  # index <- base::which(means == min(as.numeric(means),na.rm = TRUE))
  # ##This index is almost always 3 so why do I just set x2 = x2_3?
  # 
  # ##Just to be sure let do relative percent deviation (RPD) and RPI just to be sure
  # testdata <- data.frame(x = t(x), BFGS = x2_1$par, CG1 = x2_2$par, CG2 = x2_3$par,
  #               LBFGSB = x2_5$par, NelderMead = x2_5$par, SANN=x2_10$par)
  # testdata$min <- apply(testdata[-1], 1, FUN=min) #find min 
  # testdata$max <- apply(testdata[-1], 1, FUN=max)
  # RPD <- (testdata[2:7]-testdata$min)/testdata$min
  # RPI <- (testdata[2:7]-testdata$min)/(testdata$max-testdata$min)
  # library(ggplot2)
  # ggplot(stack(RPD), aes(x = ind, y = values))+
  #   geom_boxplot(aes(colour = ind))+geom_jitter(width = 0.2)
  # ggplot(stack(RPI), aes(x = ind, y = values))+
  #   geom_boxplot(aes(colour = ind))+geom_jitter(width = 0.2)
  # 
  # ##This index is almost always 3 so why don't I just set x2 = x2_3?
  x2 <- x2_3
  
  # Step 3: Bound Optimization ----
  x3 <- optim(x2$par,new.lp,lower = lower,upper = upper,method="L-BFGS-B",control = list(parscale = parscale))
  # RPD$x3 <- (x3$par-testdata$min)/testdata$min
  # RPI$x3 <- (x3$par-testdata$min)/(testdata$max-testdata$min)
  return(x3)
}
# optim_solver(overall_constraints, variablenamelist)
# system.time(ro <- optim_solver(overall_constraints, variablenamelist))