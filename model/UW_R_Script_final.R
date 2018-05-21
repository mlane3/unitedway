#2/16/18 This is the algorithm to calculate CWBI for united way
# search for *** for important comments
# CC BY-NC-SA 4.0

# Contributors: Michael Lane, Dan Donlensky, Matma Basnet, Vincent Jullien
# also by Purushothama Ranganatha, Sifael Karla Brown, Betty Glover
# under direction by: Vincent Jullien, Eric Sonmezer, and Hamid Arjmand
# for contact permission to reproduce please contact united way info @ unitedwayatlanta.org 

########### ---- unpacking libraries ---- ########### 
library(readxl)
# library(arules)
# library(tidyr)
library(dplyr)
# library(datasets)
# library(tidyverse)
library(data.table)

########### ---- Setting Working Directory and load data ---- ########### 
# setwd("~/R/unitedway")
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','weave_ct2010','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultsnoedu','adultnohealth','unemployment')
dfNorm <- df0[,3:16] # ***sifeal manipulate sliders around this line right here: average per county and average again


# lapply function's  -----------------------------------
# dfNorm <- as.data.frame(sapply(dfNorm, normalize))
pop <- function (x, ...){UseMethod("pop", x)}
#***pop is just a very basic R S3 object to group. R6 might be better
pop.normalize <- function(x) {
  #*this is the proper arthematic normalization
  return ((x - min(x)) / (max(x) - min(x)))
}
pop.zscore <- function(x) {
  #Calculated the population standard deviation or uncorrected sd()
  deviation <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  output <- (x-mean(x))/deviation
  return(output)
}
pop.sd <- function(x) {
  #Caclulated the population standard deviation or uncorrected sd()
  # It is kept as a reference
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}

########### ---- Child Well Being Code ---- ###########

pop.cwbcalc <- function(dfNorm, ...){
  #function currently requires you load in dfNorm and df0.  Call dfNorm
  ########### ---- 1) Calculate Z-scores for each indicator ---- ###########
  dfNormZ <- as.data.frame(sapply(dfNorm, pop.zscore))#Does Zscore
  # summarydfNormZ <- summary(dfNormZ) 
  
  ########### ---- 2) Make the correct variables negative & Limit from -3.1 to 3.1 ---- ########### 
  
  # Transform values of "lower-is-better" variables by multiplying them by -1
  change_valence <- function(x) {
    return (x * -1) 
  }
  dfNormZV <- as.data.frame(sapply(dfNormZ[5:10], change_valence))
  dfNormZV2 <- as.data.frame(sapply(dfNormZ[12:14], change_valence))
  # Combine transformed columns into a single df
  df_transformed <- as.data.frame(c(dfNormZ[,1:4], dfNormZV, dfNormZ[11], dfNormZV2))
  # summarydf_transformed <- summary(df_transformed)
  
  # Limit the z-scores to -3.1 <= x <= 3.1
  df_trans_zlim <- as.data.frame(sapply(df_transformed,function(x) ifelse(x > 3.1, 3.1, x)))
  # summarydf_trans_zlim <- summary(df_trans_zlim)
  df_trans_zlim2 <- as.data.frame(sapply(df_trans_zlim,function(x) ifelse(x < -3.1, -3.1, x)))
  # summarydf_trans_zlim2 <- summary(df_trans_zlim2)
  
  # Reunite the categorical and transformed variables into a new complete dataset
  df_Zscore <- as.data.frame(c(df0[,1:2], df_trans_zlim2))
  #***this is the Z-scores***
  
  rm(list = "df_trans_zlim", "df_trans_zlim2", "dfNorm", "dfNormZ", "dfNormZV", "dfNormZV2")
  # summary <- summary(df_Zscore) # These values match the 'Original Data' tab, cells S29:S806
  # write.csv(df_Zscore, file = "Complete Z-score table.csv")
  
  ########### ---- Getting to Index scores 0-100 and Summary Indexes ---- #########
  
  # 1) Average all the attributes in the particular required for the 3 sub-index ----
  # df0_ave = colMeans(df0[,c(-1,-2)])
  # df0_sd = sapply(df0[,c(-1,-2)],pop.sd)
  
  df_index <-  df_Zscore[,FALSE] #intialize a data frame of same number of rows
  df_index$ChildZ = rowMeans(subset(df_Zscore, select = c(gradrate,ccrpi,
                                                          grade3,grade8,
                                                          lbw,childnohealth,
                                                          childpoverty)))
  df_index$FamilyZ <- rowMeans(subset(df_Zscore, select = c(povertyrate, 
                                                            housingburden, 
                                                            momsnohs)))
  df_index$CommunityZ <- rowMeans(subset(df_Zscore, select = c(collegerate,
                                                               adultsnoedu,
                                                               adultnohealth,
                                                               unemployment)))
  # 3)  Average the 3 sub-indexes ----
  df_index$CWB_Z <- rowMeans(subset(df_index, select = c(ChildZ, FamilyZ, CommunityZ)))
  df_index <<- df_index <- as.data.frame(c(df0[,1:2], df_index))
  #***so df_index is All the indexes before we scale to 100. <<- sets to global
  # summarydfindex <- summary(df_index)
  
  # 4)  Rescale/ Denormalize the Score -----
  #Formula =+(AG7-MIN(AG$7:AG$782))/(MAX(AG$7:AG$782)-MIN(AG$7:AG$782)
  df_index_100 <- df_index[,FALSE] 
  
  # dfindex_100 <- 100*(dfindex - min(dfindex))/(max(dfindex)-min(dfindex)) #this is the scale score 1-100
  
  df_index_100$Child_Index <- (df_index$ChildZ - min(df_index$ChildZ)) / (max(df_index$ChildZ) - min(df_index$ChildZ))
  # summary(df_index$ChildZ)
  # summary(df_index_100$ChildZ)
  
  df_index_100$Family_Index <- (df_index$FamilyZ - min(df_index$FamilyZ)) / (max(df_index$FamilyZ) - min(df_index$FamilyZ))
  # summary(df_index$FamilyZ)
  # summary(df_index_100$FamilyZ)
  
  df_index_100$Community_Index <- (df_index$CommunityZ - min(df_index$CommunityZ)) / (max(df_index$CommunityZ) - min(df_index$CommunityZ))
  # summary(df_index$CommunityZ)
  # summary(df_index_100$CommunityZ)
  
  df_index_100$CWB_Index <- (df_index$CWB_Z - min(df_index$CWB_Z)) / (max(df_index$CWB_Z) - min(df_index$CWB_Z))
  # summary(df_index$CWB_Z)
  # summary(df_index_100$CWB_Index)
  
  # Add in the categoricals (county and census tract) and reorder the columns
  df_index_100$county <- df_Zscore$county
  df_index_100$weave_ct2010 <- df_Zscore$weave_ct2010 
  df_index_100 <- as.data.table(df_index_100)
  df_index_100 <- setcolorder(df_index_100,c("county","weave_ct2010","Child_Index","Family_Index","Community_Index","CWB_Index"))
  # These values match the 'Original Data' tab, cells AK29:AN806
  
  ##### --- Return Output ---- #####
  # df_complete 
  df_complete <- merge(as.data.table(df0),as.data.table(df_index_100),by="weave_ct2010")
  df_complete <<- merge(as.data.table(df_complete),as.data.table(df_index),by="weave_ct2010")
  
  #*** df_complete is the complete data table. By using <<- we set to global
  # print(df_index_100$CWB_Index) #***This is the Output!!!
  write.csv(df_index_100, file = "Complete index table.csv")
  return(df_index_100)
}
df_index_100 <- pop.cwbcalc(dfNorm)