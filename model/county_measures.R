#####  Create data frame with county level mean and standard dev #####
# Authors:  Brian Herbert, Michael Lane
# Milestone V1: Aggregation of Original County Data
# should we use dplyr::data.tables? or just df's?
# Logical steps
# 1. init and load libraries - need standard init function for version checks, local setup, env. checks - United Way may run wide range of hw/sw/configuration setups
# 2. set v_debug T/F, allows toggle of tools/code only used during dev/test
# 3. read in United Way excel data (readxl::, dplyr::)
# 3. extract lists and counts of counties & measures
# Naming:  tbl_ for tibbles, v_ for values (incl vectors and matrices), 14 "measures" for 13 "counties", this script doesn't deal with aggregate level- 3 groups(pillars) or CWBI
# counties and measures are quite static and could have been hard-coded as constants, but for adaptability we get values from vectors and refrain from writing values in code
# 4. create and populate tbl_COUNTY_max, tbl_COUNTY_min,tbl_COUNTY_mean,and tbl_COUNTY_stdev
# max, min, mean, std. dev. for: 14 measures x 13 counties
# 4 x 14 x 13 = 728 values
v_run_init <- as.logical(T)
v_debug <- as.logical(T)


# initialization and set platform-dependent constants ----
if(v_run_init) {
   options(readr.default_locale=readr::locale(tz="America/New_York"))
      #setwd("~/myDocuments/Emory Analytics/UnitedWayCWBI/Mar17Merge/mar19/unitedway/model")
}

# library(RGtk2)
# library(RGtk2Extras)
library(readxl)
library(tidyverse)
library(tidyselect)
library(dplyr)
library(tibble)
library(stats)
library(data.table)

pop.sd <- function(x) {
  #Caclulated the population standard deviation or uncorrected sd()
  # It is kept as a reference
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}


# debug for dev tools and env checking ----
if(v_debug) {
   library(codetools)
   library(pkgconfig)
   default_locale()
   installed.packages()

   # readline("press return to resume script execution")
}

v_input_type <- c('text', matrix('numeric', 1, 15))
df0 <- read_xlsx("2016 Original Data.xlsx", range="A2:P777", col_names = FALSE, col_types=v_input_type, trim_ws=TRUE)
names(df0) <- c('county','TRACT','gradrate','ccrpi','grade3','grade8','lbw','childnohealth','childpoverty','povertyrate','housingburden','momsnohs','collegerate','adultsnoedu','adultnohealth','unemployment')
df0 <- df0[,-2]


if(v_debug) {
   typeof(df0)
   mode(df0)
   is.matrix(df0)
   is.numeric(df0)
   is.numeric(df0[,3])
   is.ordered(df0)
   storage.mode(df0)

   # identical(df0,df1)
   getRversion()
}
# setup ----

v_colnames <- as.vector(names(df0))
v_measures <- v_colnames[-1]
v_m <- as.integer(length(v_measures))

tbl_bycounty <- as.tibble(df0)
tbl_bycounty <- group_by(tbl_bycounty, county)
tbl_county_measures <- distinct(tbl_bycounty, county, count=as.integer(n()))
v_counties <- select(tbl_county_measures, county)


log_mean_fx = function(county_ave,county_sd){
  log_mean <- exp(county_ave + (county_sd)^2/2)
}
log_sd_fx = function(county_ave,county_sd){
  log_sd <- sqrt((exp(county_sd^2-1))*exp(2*county_ave+county_sd^2))
}
min_fx <- function(county_ave,county_sd){
  x <- log_mean_fx(county_ave,county_sd) - log_sd_fx(county_ave,county_sd)
  return(x)
}
max_fx <- function(county_ave,county_sd){
  x <- log_mean_fx(county_ave,county_sd) + log_sd_fx(county_ave,county_sd)
  return(x)
}
# transpose so counties are columns in tbl_COUNTY ----

v_counties <- as.vector(t(v_counties), mode = "character")
v_c <- as.integer(length(v_counties))


tbl_this_county <- filter(tbl_bycounty, county==v_counties[1])
tbl_COUNTY_max <- data.frame(matrix(NA, nrow = v_m, ncol = v_c))
tbl_COUNTY_min <- data.frame(matrix(NA, nrow = v_m, ncol = v_c))
tbl_COUNTY_mean <- data.frame(matrix(NA, nrow = v_m, ncol = v_c))
tbl_COUNTY_stdev <- data.frame(matrix(NA, nrow = v_m, ncol = v_c))


names(tbl_COUNTY_mean) <- c(v_counties)
names(tbl_COUNTY_min) <- names(tbl_COUNTY_max) <- names(tbl_COUNTY_stdev) <- names(tbl_COUNTY_mean)
row.names(tbl_COUNTY_mean) <- c(v_measures)
row.names(tbl_COUNTY_min) <- row.names(tbl_COUNTY_max) <- row.names(tbl_COUNTY_stdev) <- row.names(tbl_COUNTY_mean)

# creatin data structure with NA ensures numeric storage allocation
# this_calc <- matrix(data=NA, nrow=1, ncol = v_m)
this_mean <- numeric(length = v_m)
# debug block: test for numeric & lib version issues
if(v_debug) {
   is.matrix(tbl_this_county)
   is.numeric(tbl_this_county)
   is.ordered(tbl_this_county)
   storage.mode(tbl_bycounty)
   i <- 1
   j <- 1
}

# <------------> placeholder: tbl_cube <--------------> #

# <-----------------> end tbl_cube <------------------> #

for(j in 1:v_c)  {
    tbl_bycounty <- group_by(tbl_bycounty, county)
    tbl_this_county <- filter(tbl_bycounty, county==v_counties[j])
    tbl_this_county <- data.matrix(tbl_this_county[,-1])
    this_county <- v_counties[j]
    v_obs <- filter(tbl_county_measures, county==v_counties[j])
    v_obs <- as.integer(v_obs[,-1])
    v_max <- data.matrix(apply(tbl_this_county, 2, max))
    v_min <- data.matrix(apply(tbl_this_county, 2, min))
    tbl_COUNTY_mean[,j] <- data.matrix(apply(tbl_this_county, 2, mean))
    tbl_COUNTY_stdev[,j] <- data.matrix(apply(tbl_this_county, 2, stats::sd))
    v_mean  <- tbl_COUNTY_mean[,j]/100; v_sd <- tbl_COUNTY_stdev[,j]/100
    log_max <- max_fx(v_mean,v_sd)
    log_min <- min_fx(v_mean,v_sd)
    for(i in 1:14){
      if(log_max[i] < 1){tbl_COUNTY_max[i,j] <- 100*log_max[i]
      }else{
        tbl_COUNTY_max[i,j] <- v_max[i]}
      if(log_min[i] < v_min[i]/100 && log_min[i] > 0){tbl_COUNTY_min[i,j] <- 100*log_min[i]
      }else{
        tbl_COUNTY_min[i,j] <- v_min[i]}
    }
}
#Fix of tbl_COUNTY_min ----
# Both child poverty and momsnohs have negative values which is odd.  Could these be typos?
tbl_COUNTY_min <- abs(tbl_COUNTY_min)

#browser()
# Write as one big dataframe ----

#From https://stackoverflow.com/questions/17499013/how-do-i-make-a-list-of-data-frames
# Alternatives method: big_data = dplyr::bind_col(df_list)
# Get them all in a list

df_list <- mget(c("tbl_COUNTY_min","tbl_COUNTY_max","tbl_COUNTY_mean","tbl_COUNTY_stdev"))
# Merge them into a dataframe
tbl_COUNTY <- do.call(what = cbind, args = df_list)
# Remove the added "tbl_COUNTY_" to make the names shorter
# From https://stackoverflow.com/questions/39670918/replace-characters-in-column-names-gsub
names(tbl_COUNTY) <- gsub("tbl_COUNTY_","",names(tbl_COUNTY))
# Replace "Mean" with "ave" so naming structure is consitent
names(tbl_COUNTY) <- gsub("tbl_COUNTY_","",names(tbl_COUNTY))
# Write.csv
# tbl_COUNTY <- write.csv(tbl_COUNTY,"data/county constrants.csv")

# cleanup all tasks including debug/test mode ----
if(v_debug) {
  #From https://stackoverflow.com/questions/6190051/how-can-i-remove-all-objects-but-one-from-the-workspace-in-r
  keep <- c("df0","tbl_COUNTY")
  rm(list=setdiff(ls(), keep))
 # put cleanup code here
}
write.csv(tbl_COUNTY,"data/county constrants.csv")
