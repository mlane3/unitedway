library(data.table)
#library(dplyrAssist)
#library(dplyr)



library(lineprof)
#unpacking libraries ----
library(readxl)
library(arules)
library(tidyr)
library(dplyr)
library(datasets)




# Setting Working Directory ------
setwd("C:/Users/Mamta/Desktop/UW")

df0 <- read_xlsx("2016 Original Data.xlsx")
df <- df0
df <- select_if(df, is.numeric)
##lapply function's  -----
#Below we have included the basic formulas that you might need to do the z-score
#by using the population standard deviation
#The Excel Function is
#=IF((G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782)<3.1, 
#    (IF((G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782)>-3.1, 
#        (G8-AVERAGE(G$7:G$782))/STDEV.P(G$7:G$782), -3.1)), 3.1)
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) #*this is the proper arthematic normalization
}
pop.zscore <- function(x) {
  devation <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  output <- (x-mean(x))/deviation
  return(output)
}
pop.sd <- function(x) {
  #Caclulated the population standard deviation or uncorrected sd()
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}

########ACTUAL CODE######   ----- 

#Z-normalize all attributes over correct domain per region.  
# dfNorm <- as.data.frame(lapply(df[2:ncol(df)], normalize)) #This function will be useful later
dfNormZ <- as.data.frame(lapply(df, pop.zscore)) #this is the core function objective 2 will edit

##The problem child script ----

##This is loop is very memory intensive and not optmized
for (index in 1:nrow(dfNormZ)){ 
  for (n in 3:length(dfNormZ)){
    #doing it this way as an if for loop wastes alot of resources in memory and time
    cells_zscore <- dfNormZ[index,]
    if (cells_zscore[n] <= 3.1) {
      if (cells_zscore[n] >= -3.1){
        df2[index,n] <- cells_zscore[n]
      }
      else {
        df2[index,n] <- -3.1
      }
    }
    else {
      df2[index,n] <- 3.1
    }
  }
}



# 3)  Average all the attributes in the particular required for the 3 sub-index ----

# 4)  Rescale/ Denormalize the Score ----
##Use mean and deviation from step 2

# 5)  Create quartiles (box plot) and quintile (5 equal divisions). ----


# 6)  Classify the quintile ranges -----

#Classify the quartile ranges and whiskers as

#Goal of This week ---- 
#Based on what people do we will Pick from the other options people do
##and choose the most optimized code
##.



# 7)  Calculate improvement ----
#This will will do after based on excel sheet

