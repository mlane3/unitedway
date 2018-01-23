# United Way
#12/21/17

# Start the clock!
# ptm <- proc.time()
#delte this file


#unpacking libraries ----
library(tidyverse)
library(readxl)
library(arules)
library(tidyr)
library(dplyr)
library(datasets)

########### ------------Getting to a table of z-scores --------------- #########


# Setting Working Directory ------
setwd("~/Documents/DATA/United Way project 2017-2018")
# read in the data
df0 <- read_xlsx("2016 Original Data.xlsx")
# create a copy to transform
df <- df0
summary(df)
# create and apply functions: Normalize
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) #*this is the proper arithmetic normalization
}

dfNorm <- df[,3:16]
dfNorm <- as.data.frame(sapply(dfNorm, normalize))
dfNorm

# create and apply functions: Population z-score
pop.zscore <- function(x) {
  deviation <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  output <- (x-mean(x))/deviation
  return(output)
}

dfNormZ <- as.data.frame(sapply(dfNorm, pop.zscore))
dfNormZ
summary(dfNormZ)

# Transform values of "lower-is-better" variables by multiplying them by -1
change_valence <- function(x) {
  return (x * -1) 
}

dfNormZV <- as.data.frame(sapply(dfNormZ[5:10], change_valence))
dfNormZV2 <- as.data.frame(sapply(dfNormZ[12:14], change_valence))

# Combine transformed columns into a single df
df_transformed <- as.data.frame(c(dfNormZ[,1:4], dfNormZV, dfNormZ[11], dfNormZV2))
summary(df_transformed)

# Limit the z-scores to -3.1 <= x <= 3.1
df_trans_zlim <- as.data.frame(sapply(df_transformed,function(x) ifelse(x > 3.1, 3.1, x)))
summary(df_trans_zlim)
df_trans_zlim2 <- as.data.frame(sapply(df_trans_zlim,function(x) ifelse(x < -3.1, -3.1, x)))
summary(df_trans_zlim2)

# Reunite the categorical and transformed variables into a new complete dataset
df_complete <- as.data.frame(c(df0[,1:2], df_trans_zlim2))
summary(df_complete)
# These values match the 'Original Data' tab, cells S29:S806

rm(list = "df_trans_zlim", "df_trans_zlim2", "dfNorm", "dfNormZ", "dfNormZV", "dfNormZV2")

write.csv(df_complete, file = "Complete Z-score table.csv")


########### --- Getting to Index scores 0-100 and Summary Indexes ---- #########

# not sure we need this pop.sd function or df_ave and df0_sd... I don't see these get used below
# create and apply functions: Population standard deviation
pop.sd <- function(x) {
  #Calculated the population standard deviation or uncorrected sd()
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}
df0_ave = colMeans(df_complete[,c(-1,-2)])
df0_sd = sapply(df_complete[,c(-1,-2)],pop.sd)

# 2)  Average subsets of the attributes as needed for the 3 sub-indexes

df_index <-  df_complete[,FALSE] #intialize a data frame of same number of rows
# df_index$county <- df_complete$county
# df_index$weave_ct2010 <- df_complete$weave_ct2010
df_index$ChildZ = rowMeans(subset(df_complete, select = c( Educ_HS_GradRateF, 
                                                           Educ_HS_CCRPI_ScoreF,
                                                           Educ_pcExceeding_3Grade_ReadingF,
                                                           Educ_pc_Exceeding_8Grade_MathF,
                                                           Health_pcLBW_BirthsF,
                                                           Health_pc_Under18_no_Health_InsF,
                                                           Income_pcUnder18_in_povF)))
df_index$FamilyZ <- rowMeans(subset(df_complete, select = c(Income_pcFam_below200pc_povF, 
                                                            Income_pcHH_HousingBurdenF, 
                                                            Birth_to_Moms_noHSF)))
df_index$CommunityZ <- rowMeans(subset(df_complete, select = c(Educ_pc_postsecF,
                                                               Educ_pcAdults_no_HSF,
                                                               Health_pcAdults_no_Health_InsF,
                                                               Income_Unemployment_rateF)))
# 3)  Average the 3 sub-indexes to get the Child Well-Being Index
df_index$CWB_Z <- rowMeans(subset(df_index, select = c(ChildZ, FamilyZ, CommunityZ)))
summary(df_index)
# These values match the 'Original Data' tab, cells AG29:AJ806


# 4)  Rescale/ Denormalize the Score

# Formula like =+(AG7-MIN(AG$7:AG$782))/(MAX(AG$7:AG$782)-MIN(AG$7:AG$782))

# create an empty table with the right number of rows
df_index_100 <- df_index[,FALSE] 

# For each column in df_index, compute the scaled values in the range 0-100
df_index_100$ChildZ <- (df_index$ChildZ - min(df_index$ChildZ)) / (max(df_index$ChildZ) - min(df_index$ChildZ))
summary(df_index$ChildZ)
summary(df_index_100$ChildZ)

df_index_100$FamilyZ <- (df_index$FamilyZ - min(df_index$FamilyZ)) / (max(df_index$FamilyZ) - min(df_index$FamilyZ))
summary(df_index$FamilyZ)
summary(df_index_100$FamilyZ)

df_index_100$CommunityZ <- (df_index$CommunityZ - min(df_index$CommunityZ)) / (max(df_index$CommunityZ) - min(df_index$CommunityZ))
summary(df_index$CommunityZ)
summary(df_index_100$CommunityZ)

df_index_100$CWB_Z <- (df_index$CWB_Z - min(df_index$CWB_Z)) / (max(df_index$CWB_Z) - min(df_index$CWB_Z))
summary(df_index$CWB_Z)
summary(df_index_100$CWB_Z)

# Add in the categoricals (county and census tract)
df_index_100$county <- df_complete$county
df_index_100$weave_ct2010 <- df_complete$weave_ct2010

# reorder the columns 
df_index_100 <- df_index_100[c(5, 6, 4, 1, 2, 3)]

summary(df_index_100)
# These values match the 'Original Data' tab, cells AK29:AN806

write.csv(df_index_100, file = "Complete index table.csv")

