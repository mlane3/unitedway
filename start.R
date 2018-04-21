install.packages('rsconnect')
rsconnect::setAccountInfo(name='mlane6',
                          token='8E3D801F1878AD352F1289A5F0BF233E',
                          secret='XpEkK9WI7OnOwk5ILq8q2wdl335mAT2GF/foNyOV')
library(rsconnect)
setwd("~/R/unitedway")
rsconnect::deployApp()
