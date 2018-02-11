# United Way
#12/10/17

#unpacking libraries

library(arules)
library(tidyr)
library(dplyr)
library(datasets)

# Setting Working Directory
setwd("C:/Users/Mamta/Desktop/UW")

df <- read_xlsx("2016 Original Data.xlsx")
df
select_if(df, is.numeric)
##lapply function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

dfNorm <- as.data.frame(lapply(df, normalize))
# One could also use sequence such as df[1:2]
dfNorm <- as.data.frame(lapply(df[1:2], normalize))
dfNormZ <- as.data.frame( scale(df[1:2] ))

df
  
#histogram
hist(df)

