pop.sd <- function(x) {
  #Caclulated the population standard deviation or uncorrected sd()
  # It is kept as a reference
  output <- sd(x, na.rm = TRUE)*sqrt((length(x)-1)/(length(x)))
  return(output)
}
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','TRACT','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultsnoedu','adultnohealth','unemployment')
#Calculate the aggreate by stdev and by average
county_ave = NULL
county_sd = NULL

log_mean_fx = function(county_ave,county_sd){
  log_mean <- exp(county_ave + (county_sd)^2/2)
}
log_sd_fx = function(county_ave,county_sd){
  log_sd <- (exp(county_sd^2)-1)*exp(2*county_ave+county_sd^2)
}


#Calculate df0_average
df0_ave = colMeans(df0[,c(-1,-2)]) #the df0_average (weighted by track/row/county)
#Calculate STDEV
df0_sd = sapply(df0[,c(-1,-2)],pop.sd) #the population deviation
dplyr::