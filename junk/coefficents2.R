########### ---- Linear Optimizer TEST CODE ---- ###########
# Contributors: Michael Lane
# This creates a coefficent table for linear optimization
#Z score of 0.33886 or CWB_index = .689 is the goal!
# It requires you to preload the other functions besides pop.cwbcalc()
source('UW_R_Script_final.R')
# df_index_100 <- as.data.frame(pop.cwbcalc(df0[,3:16])) #line to get CWBI

# Getting Started EXample --
# Run lines below 33 if you get an error.
########### ---- Create a Coefficent Table: Code ---- ###########
pop.Coef <- function(df0){
  library(data.table)
  #Calculate df0_average
  
  df0_ave = colMeans(df0[,c(-1,-2)]) #the df0_average (weighted by track/row/county)
  #Calculate STDEV
  df0_sd = sapply(df0[,c(-1,-2)],pop.sd) #the population deviation
  
  # Define the Coefficent and Intercept B for each variable
  ChildCoef <- function(x){return(1/(3*7*x))} #Child Coefficents
  FamilyCoef <- function(df0_sd){ return(1/(3*3*df0_sd)) } #Family Coefficents
  ComCoef <- function(df0_sd){ return(1/(3*4*df0_sd)) } #Community Coefficents
  
  ChildB <- function(df0_ave,df0_sd){ return(df0_ave/(3*7*df0_sd)) } #Child Intercepts
  FamilyB <- function(df0_ave,df0_sd){ return(df0_ave/(3*3*df0_sd)) } #Family Intercepts
  ComB <- function(df0_ave,df0_sd){ return(df0_ave/(3*4*df0_sd)) } #Community Intercepts
  
  Coeff <- list(
    ChildCoef(df0_sd["gradrate"]),ChildCoef(df0_sd["ccrpi"]),
    ChildCoef(df0_sd["grade3"]),ChildCoef(df0_sd["grade8"]),
    -1*ChildCoef(df0_sd["lbw"]),-1*ChildCoef(df0_sd["childnohealth"]),
    -1*ChildCoef(df0_sd["childpoverty"]),
    -1*FamilyCoef(df0_sd["povertyrate"]),
    -1*FamilyCoef(df0_sd["housingburden"]),
    -1*FamilyCoef(df0_sd["momsnohs"]),
    ComCoef(df0_sd["collegerate"]),
    -1*ComCoef(df0_sd["adultsnoedu"]),
    -1*ComCoef(df0_sd["adultsnohealth"]),
    -1*ComCoef(df0_sd["unemployment"]))
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
    -1*ComB(df0_ave["adultsnohealth"],df0_sd["adultsnohealth"]),
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"])
  )
  df0_Coef <- data.table(name = names(df0[,c(-1,-2)]),
                         coefficients = Coeff,
                         B = B)
  rm(B,Coeff,ChildCoef,ChildB,FamilyCoef,FamilyB,ComCoef,ComB)
  return(df0_Coef)
}



mean(df_index_100$CWB_Index) #the actual CWBI 
minCWB_Z = min(df_index$CWB_Z) #-1.969282
maxCWB_Z = max(df_index$CWB_Z) #1.380706
df2 <- as.data.frame(read.csv("overall constrants.csv", skip = 2, row.names = 1)) 
#^***Sifeal df2 is what your taking in for this one and can use to modify the sliders
myCoef <- pop.Coef(df0) #use for the orginal data frame to get y = m*x - b coeff
CWBZ <- rowMeans(myCoef$coefficients*df2["Mean",] - myCoef$B) #***We are optimizing this
CWB_Index <- (CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z) 
#which should equal 0.5895567 our test CWBI and .001 away from actual CWBI

# Test Code Case 1: For True Average or Average weighted by each track/row ----
# **And no I am not making a pivot table for original data again
CWB1 <- rowMeans(myCoef$coefficients*df2["df0_ave",] - myCoef$B)
CWB2 <- (CWB1  - minCWB_Z)/(maxCWB_Z - minCWB_Z) 
CWB2 #is .5878474 which the same as CWBI we find from excel! and dfindex_100
# Case 2: for 0 ----
CWB3 <- (0 - minCWB_Z)/(maxCWB_Z - minCWB_Z) #is literally .5878474
#This is why we don't use average of the columns to test!!!

#rm(CWB1,CWB2,CWB3,CWBZ,df_complete)