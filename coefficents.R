########### ---- Create a Coefficent Table ---- ###########
# Contributors: Michael Lane
# This creates a coefficent table for linear optimization
# It requires you to preload the other functions besides pop.cwbcalc()

# pop.Coef <- function(df0){
  library(data.table)
  #Calculate df0_average
  df0_ave = colMeans(df0[,c(-1,-2)]) #the df0_average (weighted by track/row/county)
  #Calculate STDEV
  df0_sd = sapply(df0[,c(-1,-2)],pop.sd) #the population deviation
  # Define the Coefficent and Intercept B for each variable
  ChildCoef <- function(x){
    return(1/(3*7*x))
    } #Child Coefficents
  FamilyCoef <- function(df0_sd){ return(1/(3*3*df0_sd)) } #Family Coefficents
  ComCoef <- function(df0_sd){ return(1/(3*4*df0_sd)) } #Community Coefficents
  ChildB <- function(df0_ave,df0_sd){ return(df0_ave/(3*7*df0_sd)) } #Child Intercepts
  FamilyB <- function(df0_ave,df0_sd){ return(df0_ave/(3*3*df0_sd)) } #Family Intercepts
  ComB <- function(df0_ave,df0_sd){ return(df0_ave/(3*4*df0_sd)) } #Community Intercepts
  Coeff <- list(ChildCoef(df0_sd["gradrate"]),ChildCoef(df0_sd["ccrpi"]),
    ChildCoef(df0_sd["grade3"]),ChildCoef(df0_sd["grade8"]),
    -1*ChildCoef(df0_sd["lbw"]),-1*ChildCoef(df0_sd["childnohealth"]),
    -1*ChildCoef(df0_sd["childpoverty"]),
    -1*FamilyCoef(df0_sd["povertyrate"]),
    -1*FamilyCoef(df0_sd["housingburden"]),
    -1*FamilyCoef(df0_sd["momsnohs"]),
    ComCoef(df0_sd["collegerate"]),
    -1*ComCoef(df0_sd["adultnoedu"]),
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
    -1*ComB(df0_ave["adultnoedu"],df0_sd["adultnoedu"]),
    -1*ComB(df0_ave["adultsnohealth"],df0_sd["adultsnohealth"]),
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"])
  )
  df0_Coef <- data.table(name = names(df0[,c(-1,-2)]),
                         coefficients = Coeff,
                         B = B)
  rm(B,Coeff,ChildCoef,ChildB,FamilyCoef,FamilyB,ComCoef,ComB)
#   return df0_Coef
# }
