install.packages('rsconnect')
rsconnect::setAccountInfo(name='mlane6',
                          token='remove',
                          secret='oops')
library(rsconnect)
setwd("~/R/unitedway")
rsconnect::deployApp()
