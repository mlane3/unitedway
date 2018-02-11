
#12/10/17

#unpacking libraries ----
library(readxl)
library(arules)
library(tidyr)
library(dplyr)
library(datasets)
library(tidyverse)
# library(readxl)
# library(arules)
# library(tidyr)
# library(dplyr)
# library(datasets)
# Setting Working Directory ------
setwd("~/R/unitedway")

df0 <- read_xlsx("2016 Original Data.xlsx")
dfNorm <- df0[,3:16]
# dfNorm <- as.data.frame(sapply(dfNorm, normalize))

# df <- select_if(df, is.numeric)
##lapply function's  -----
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) #*this is the proper arthematic normalization
}
pop.zscore <- function(x) {
  deviation <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  output <- (x-mean(x))/deviation
  return(output)
}
pop.sd <- function(x) {
  #Caclulated the population standard deviation or uncorrected sd()
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}

########ACTUAL CODE######----- 
# dfNorm <- as.data.frame(lapply(df[2:ncol(df)], normalize)) #This function will be useful later
dfNormZ <- as.data.frame(sapply(dfNorm, pop.zscore))#Does Zscore
dfNormZ
summarydfNormZ <- summary(dfNormZ) 

#####Limit to -3.1 and 3.1 ----

# ##This is loop is very memory intensive and not optmized
# for (index in 1:nrow(dfNormZ)){ 
#   for (n in 3:length(dfNormZ)){
#     #doing it this way as an if for loop wastes alot of resources in memory and time
#     cells_zscore <- dfNormZ[index,]
#     if (cells_zscore[n] <= 3.1) {
#       if (cells_zscore[n] >= -3.1){
#         df2[index,n] <- cells_zscore[n]
#       }
#       else {
#         df2[index,n] <- -3.1
#       }
#     }
#     else {
#       df2[index,n] <- 3.1
#     }
#   }
# }


# Transform values of "lower-is-better" variables by multiplying them by -1
change_valence <- function(x) {
  return (x * -1) 
}

dfNormZV <- as.data.frame(sapply(dfNormZ[5:10], change_valence))
dfNormZV2 <- as.data.frame(sapply(dfNormZ[12:14], change_valence))

# Combine transformed columns into a single df
df_transformed <- as.data.frame(c(dfNormZ[,1:4], dfNormZV, dfNormZ[11], dfNormZV2))
summarydf_transformed <- summary(df_transformed)

# Limit the z-scores to -3.1 <= x <= 3.1
df_trans_zlim <- as.data.frame(sapply(df_transformed,function(x) ifelse(x > 3.1, 3.1, x)))
summarydf_trans_zlim <- summary(df_trans_zlim)
df_trans_zlim2 <- as.data.frame(sapply(df_trans_zlim,function(x) ifelse(x < -3.1, -3.1, x)))
summary <- summary(df_trans_zlim2)

# Reunite the categorical and transformed variables into a new complete dataset
df_Zscore <- as.data.frame(c(df0[,1:2], df_trans_zlim2)) #*** this is the Z-scores
summary <- summary(df_Zscore)
# These values match the 'Original Data' tab, cells S29:S806

# rm(list = "df_trans_zlim", "df_trans_zlim2", "dfNorm", "dfNormZ", "dfNormZV", "dfNormZV2")

write.csv(df_Zscore, file = "Complete Z-score table.csv")

#  Also, create box/quintile plots of each Z-score


########### --- Getting to Index scores 0-100 and Summary Indexes ---- #########

# 2)  Average all the attributes in the particular required for the 3 sub-index ----

df0_ave = colMeans(df0[,c(-1,-2)])
df0_sd = sapply(df0[,c(-1,-2)],pop.sd)
#df_index <-  df0[,FALSE] #intialize a data frame of same number of rows
# dfindex$childindex = rowSums(subset(df2, select = c( Educ_HS_GradRateF, Educ_HS_CCRPI_ScoreF,
#                                                      Educ_pcExceeding_3Grade_ReadingF,
#                                                      Educ_pc_Exceeding_8Grade_MathF,
#                                                      Health_pcLBW_BirthsF,
#                                                      Health_pc_Under18_no_Health_InsF,
#                                                      Income_Unemployment_rateF)))
# dfindex$FamilyZ <- rowMeans(subset(df2, select = c(Income_pcFam_below200pc_povF,
#                                                    Income_pcHH_HousingBurdenF,
#                                                    Birth_to_Moms_noHSF)))
# dfindex$CommunityZ <- rowMeans(subset(df2, select = c(Educ_pc_postsecF,
#                                                       Health_pc_Under18_no_Health_InsF,
#                                                       Income_Unemployment_rateF)))

df_index <-  df_Zscore[,FALSE] #intialize a data frame of same number of rows
# df_index$county <- df_Zscore$county
# df_index$weave_ct2010 <- df_Zscore$weave_ct2010
df_index$ChildZ = rowMeans(subset(df_Zscore, select = c( Educ_HS_GradRateF, 
                                                           Educ_HS_CCRPI_ScoreF,
                                                           Educ_pcExceeding_3Grade_ReadingF,
                                                           Educ_pc_Exceeding_8Grade_MathF,
                                                           Health_pcLBW_BirthsF,
                                                           Health_pc_Under18_no_Health_InsF,
                                                           Income_pcUnder18_in_povF)))
df_index$FamilyZ <- rowMeans(subset(df_Zscore, select = c(Income_pcFam_below200pc_povF, 
                                                            Income_pcHH_HousingBurdenF, 
                                                            Birth_to_Moms_noHSF)))
df_index$CommunityZ <- rowMeans(subset(df_Zscore, select = c(Educ_pc_postsecF,
                                                               Educ_pcAdults_no_HSF,
                                                               Health_pcAdults_no_Health_InsF,
                                                               Income_Unemployment_rateF)))
#****SO dfindex is the index values and df2
df_index <- as.data.frame(c(df0[,1:2], df_index)) #***All the indexes before we scale to 100

# 3)  Average the 3 sub-indexes ----
df_index$CWB_Z <- rowMeans(subset(df_index, select = c(ChildZ, FamilyZ, CommunityZ)))
summarydfindex <- summary(df_index)
# 4)  Rescale/ Denormalize the Score -----
#Need to talk about
#Calculate Means
#Calculate STDEV
#Formula =+(AG7-MIN(AG$7:AG$782))/(MAX(AG$7:AG$782)-MIN(AG$7:AG$782)
df_index_100 <- df_index[,FALSE] 

# dfindex100 <- 100*(dfindex - min(dfindex))/(max(dfindex)-min(dfindex)) #this is the scale score 1-100

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
df_index_100$county <- df_Zscore$county
df_index_100$weave_ct2010 <- df_Zscore$weave_ct2010

# reorder the columns 
# df_index_100 <- as.data.frame(df_index_100[c(5, 6, 4, 1, 2, 3)]) #*** Mike check reordering
# df_index_100 <- as.data.frame(df_index_100)
# df_index_100 <- summary(df_index_100)
# These values match the 'Original Data' tab, cells AK29:AN806

#write.csv(df_index_100, file = "Complete index table.csv") ##** This c

# # 5)  Create box plots and quartile plots -----
# ##
# quintiles <- sapply(dfindex, function(x) split(x, cut(x, quantile(x, prob = 0:10 / 10, names = FALSE), include = TRUE)))
# 
# #A simple plot histogram
# library(reshape2)
# library(ggplot2)
# myplot <- melt(df) #replace df with what you want to plot
# g <- ggplot(myplot,aes(x=value))
# g <- g + geom_histogram()
# g <- g + facet_wrap(~weave_ct2010)
# g
# 
# #A simple plot histogram
# library(reshape2)
# library(ggplot2)
# myplot <- melt(dfindex) #replace df with what you want to plot
# g <- ggplot(myplot,aes(x=value))
# g <- g + geom_histogram()
# g <- g + facet_wrap(~weave_ct2010)
# g

