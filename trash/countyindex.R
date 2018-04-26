
# library(data.table)
# 
# # 1)  Z-normalize all attributes over correct domain per region. ----  
# #The Excel Function is
# #=IF((G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782)<3.1, 
# #    (IF((G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782)>-3.1, 
# #        (G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782), -3.1)), 3.1)
# #Define Functions
# pop.sd <- function(x) {
#   #Caclulated the population standard deviation or uncorrected sd()
#   output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
#   return(output)
# }
# pop.zscore <- function(x) {
#   #Calculates the z-score based on pop deviation
#   #May throw errors because it is expecting a vector only
#   
#   # output <- (x-mean(x))/pop_sd(x) #Original method
#   pop_deviation <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
#   output <- (x-mean(x))/pop_deviation #Original method
#   return(output)
# }
# 
# df2 <- df1 <- df0
# library(readxl)
# df0 <- read_excel("~/R/unitedway/2016 Original Data.xlsx")
# 
# 
# #####The problem child script ----
# 
# #This could be looped over or if you could fix the next part just to properly match and do if
# df1[,c(-1,-2)] <- sapply(df0[,c(-1,-2)], pop.zscore)
# 
# ##This is loop is very memory intensive and not optmized
# for (index in 1:nrow(df1)){ 
#   for (n in 3:length(df1)){
#     #doing it this way as an if for loop wastes alot of resources in memory and time
#     cells_zscore <- df1[index,]
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
# 
# #  Also, create box/quintile plots of each Z-score
# 
# # 2)  Average all the attributes in the particular required for the 3 sub-index ----
# 
# df0_ave = colMeans(df0[,c(-1,-2)])
# df0_sd = sapply(df0[,c(-1,-2)],pop.sd)
# dfindex <-  df0[,FALSE] #intialize a data frame of same number of rows
# dfindex$childindex = rowSums(subset(df2, select = c( Educ_HS_GradRateF, Educ_HS_CCRPI_ScoreF,
#                                                Educ_pcExceeding_3Grade_ReadingF,
#                                                Educ_pc_Exceeding_8Grade_MathF,
#                                                Health_pcLBW_BirthsF,
#                                                Health_pc_Under18_no_Health_InsF,
#                                                Income_Unemployment_rateF)))
# dfindex$FamilyZ <- rowMeans(subset(df2, select = c(Income_pcFam_below200pc_povF,
#                                                    Income_pcHH_HousingBurdenF,
#                                                    Birth_to_Moms_noHSF)))
# dfindex$CommunityZ <- rowMeans(subset(df2, select = c(Educ_pc_postsecF,
#                                                       Health_pc_Under18_no_Health_InsF,
#                                                       Income_Unemployment_rateF)))
# #****SO dfindex is the index values and df2
# 
# # 3)  Average the 3 sub-indexes ----
# dfindex$CWBZ <- rowMeans(dfindex)
# 
# # 4)  Rescale/ Denormalize the Score
#   #Need to talk about
#   #Calculate Means
#   #Calculate STDEV
#   #Formula =+(AG7-MIN(AG$7:AG$782))/(MAX(AG$7:AG$782)-MIN(AG$7:AG$782))
# 
# dfindex100 <- 100*(dfindex - min(dfindex))/(max(dfindex)-min(dfindex)) #this is the scale score 1-100
# 
# # 5)  Create box plots and quartile plots -----
# ##
# quintiles <- sapply(dfindex, function(x) split(x, cut(x, quantile(x, prob = 0:10 / 10, names = FALSE), include = TRUE)))
# 
# # 6)  Classify the quartile ranges
# 
# #Classify the quartile ranges and whiskers as Very Low, Low, Average, High and Very High.
# #I let someone else figure this one out.
# 
# # 7)  Calculate improvement
# 
# ##Need more informationa bout how to calculatet
# 
# # 8)  Choose the most optimized code
# 
# ##Pick from the other options people do.
