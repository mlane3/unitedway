#Intialize ----
#IN BETA: This code is in beta as optimization speed is not as desired and
#the algorithm is a bit unstable.

new.Coef <- function(){
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
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"]))
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


county.lp <- function(df0_ave){
  #This county linear problem function calculates the average county CWBI.
  
  #Below is important code for understanding how create spatial LOD
  #x <- c(1,2,3)
  #y <- rep(x,13) #13 = number of counties
  #z <- matrix(y,ncol=13)
  #names(z) <- paste0("graduate",1:13) 
  numberofcounties <- 13
  mycoef <- NULL
  mycoef <- new.Coef() #To Mike: fix new.Coef later
  mycoef <- mycoef[rep(seq_len(nrow(mycoef)),numberofcounties),]
  names(mycoef) <- c("name","A","B")
  Value = .689
  maxCWB_Z <- 1.380706
  minCWB_Z <- -1.969282
  #inverse formula for normalization
  ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z 
  foo <- mycoef$A*df0_ave
  #foo <- mycoef$A*as.numeric(test2[3,])
  #We need to divide CWBZ by 13 here. Sum(mycoef$A*df0_ave- mycoef$B) is the
  #linearized equation for calc CWBI sum(CWBI per county)/13 = county CWBI
  #average
  CWBZ <- sum(foo - mycoef$B)/numberofcounties  
  CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
  # We are optimizing the normalized child well being
  bar <- (CWBZ - ValueZ) 
  bar <- (bar)^2 #we minimize ((Ax-b) - Goal)^2)
  return(bar)
}

# The New Optimizer ---------------------------------------------------------------
county_solver <- function(mydata,variablenamelist){
  # Define variables ----
  require(data.table)
  #the df0_sd and mydata are weighted by track/row/county 
  df0_sd = c(11.73959, 10.46115, 20.86766, 18.81275, 2.800196, 8.375753,
             19.90409, 20.26725, 10.86507, 11.83264, 10.64412, 9.722596, 13.92478,
             6.665234)
  ##This optimization from using Congugate Gradient Optimization. Here we are
  ##using data.table instead of dplyr because in UI side the process takes too
  ##long. With without doing a deep dive, Basically  here every second of
  ##calculation time is like about $4.98 and we are disk limited instead of
  ##memory limited. So standard data libraries like dplyr, will not work here.
  ##Ironically I should mention if I were not bound by server side resources and
  ##deadlines, Hadely Wickham's dplyr library is awesome and you should all
  ##check it out.  Hadely if you are ever reading this, please let me know if
  ##there is a way to use optimr and dplyr better.
  
  ##For those reading this code.  Thank you for taking the time to do so.
  ##As thanks I will tell you about a "little" data set that can be used as a benchmark
  ##reveals the differences between pandas, sparlyr, pyspark, bigmemory, Tableau's hyper
  ##data.table, and even dplyr.  https://www.kaggle.com/c/favorita-grocery-sales-forecasting/

  mydata <- fread(input="data/county constrants.csv")
  names(mydata) <- c("id",names(mydata)[2:length(mydata)])
  rownames(mydata) <- variablenamelist$variable
  nm <- names(mydata)
  selected <- nm %in% grep("mean", nm, value = TRUE)
  selected[1] <- TRUE
  x <- base::subset(mydata,select = selected) #select only the means
  x1 <- data.table::melt(data = x,id="id")
  x2 <- as.data.table(t(x1))
  names(x2) <- c(paste0(x1$id,"_",x1$variable))
  xframe <- as.data.frame(lapply(x2[3,],as.double))
  lower <- rep.int(3,nrow(x1)) # lower <- mydata["Min",] #Mike: need to add boundar
  upper <- rep.int(96,nrow(x1)) # upper <- mydata["Max",] #Mike: need to add 
  # parscale improves how fast optimizer solves.
  parscale <- rep.int(round(df0_sd,1),13)
  # Note 23 derived from Z-score scaling using scale()
  # parscale = rep(floor(min(as.numeric(x))),length(x))
  # Note min(x) = 9.27... , so 9 derived from min/max scaling
  means <- numeric()
 
  # Step 1: Find the Best Unbound Optimization Model ----
  x2_3 <- optim(xframe, county.lp, method = "CG", control = list(maxit=101,type = 2,parscale = parscale))
  # Step 2: Use the best optimizer ----
  #This index is almost always 2 so why do I just set x3 = x2_3?
  x2 <- x2_3
  # Step 3: Bound Optimization ----
  # the number of variables is fixed to 14.  Index below would change if we 
  # add more variables.
  x3 <- optim(x2$par,new.lp,lower= lower,upper= upper,
        method="L-BFGS-B",control= list(maxit=100,parscale = parscale))
  final <- data.frame(row.names = variablenamelist$variable,
                      id = variablenamelist$title,
                      Butts = x3$par[1:14],Cherokee = x3$par[15:28],
                      Clayton = x3$par[29:42],Cobb = x3$par[43:56],
                      Coweta = x3$par[57:70], DeKalb = x3$par[71:84],
                      Douglas = x3$par[85:98], Fayette = x3$par[99:112],
                      Fulton = x3$par[113:126], Gwinnett=  x3$par[127:140],
                      Henry = x3$par[141:154], Paulding = x3$par[155:168],
                      Rockdale = x3$par[169:182])
                      return(final)
}
# county_solver(overall_constraints, variablenamelist)
# system.time(ro <- county_solver(overall_constraints, variablenamelist))