
# #####  Data Analysis Script #####
# # Authors:  Toyin, Karla, and Michael Lane
# # Milestone V2: Use Data Analysis to figure out the relationships between variables
# 
# # This script does 2 important things.  First it figure out the granger
# # causality based on different variables, which is helpful to Karla/Brian.  It
# # looks for leads and lags in terms of the number n itself.  I wrote it intially
# # in reverse chronological order, so its easier to read for Toyin
# 
# setwd("~/R/unitedway") #Set working directory to whatever your unitedway folder is
# source("model/UW_R_Script_final.R") # get by re-running the original CWBI Calculation
# library(rattle)
# library(dplyr)
# rm(df_index_100,df_index,dfNorm,pop,pop.normalize,pop.sd,pop.zscore)
# df_complete$county.x <- NULL
# 
# # Linear Regressions and Prep ---------------------------------------------
# # This prepares the R-files for Toyin (to do by Wednesday evening?) How to use:
# # You source the data above and run the function below.  You type what county
# # you want to use.  It will filter dfcomplete or df0 by county, then store it in
# # the datafilter variable. You then run the regession we talked about in rattle 
# # I know this is slightly different, but I realized that df0 might be faster to
# # use in rattle considering your not on the lastest version of R
# # 1) Load function 
# # 2) Pick a county after butts and do regression for ccrpi
# # 3) Tell me if regression was linear and save as V1 .rattle
# # 4) Pick a new county and repeat steps 3 and 4 till you get all counites (except Butts)
# 
# # 5) Pick a county and do regression for housingburden
# # 6) Tell me if regression was linear and save as V2 .rattle
# # 7) Pick a new county and repeat step 7
# 
# # Step 1) Load the fuctions above and below
# countylist <- unique(df0$county) 
# print(unique(df0$county))
# myfilter <- function(df0,countyname){
#   datafilter <<- df0 %>% filter(county == countyname)
# }
# 
# # Step 2) Pick one of the counties in countylist (ignore Butts).
# # Examples
#   # datafilter <- myfilter(df0,countylist[2]) For "Butts"
#   datafilter <- myfilter(df0,countylist[2]) #For "Cherokee"
#   datafilter <- myfilter(df0,countylist[3]) #For "Clayton"
#   datafilter <- myfilter(df0,countylist[13]) #For "Rockdale"
#   datafilter <- myfilter(df_complete,countylist[13]) #For "Rockdale" but using complete
#   
# # Step 3) Do linear regression in Rattle for ccrpi.
#   # ccpri ~ gradrate + grade3 + grade8
#   # load rattle
#   # fit <- lm(ccpri ~ gradrate + grade3 + grade8,df0)
# 
# 
# 
# # 1) Part 1: Granger Causality Prep -------------------------------------------------------
# #Karla, so I know its been a while, but I did background research and I wanted 
# #to make the data science part a bit easier to manage.  My suggestion would be 
# #to take these functions and write a for loop.  Scroll down to the next section 
# #as it will help
# 
# # Run this line of code
# if("lmtest" %in% rownames(installed.packages()) == FALSE) {install.packages("lmtest")}
# library(lmtest) 
# #copy paste of https://stats.stackexchange.com/questions/160671/estimate-lag-for-granger-causality-test
# select.lags<-function(x,y,max.lag=60) {
#   y<-as.numeric(y)
#   y.lag<-embed(y,max.lag+1)[,-1,drop=FALSE]
#   x.lag<-embed(x,max.lag+1)[,-1,drop=FALSE]
#   
#   t<-tail(seq_along(y),nrow(y.lag))
#   
#   ms=lapply(1:max.lag,function(i) lm(y[t]~y.lag[,1:i]+x.lag[,1:i]))
#   
#   pvals<-mapply(function(i) anova(ms[[i]],ms[[i-1]])[2,"Pr(>F)"],max.lag:2)
#   ind<-which(pvals<0.05)[1]
#   ftest<-ifelse(is.na(ind),1,max.lag-ind+1)
#   
#   aic<-as.numeric(lapply(ms,AIC))
#   bic<-as.numeric(lapply(ms,BIC))
#   lagstats <<- structure(list(ic=cbind(aic=aic,bic=bic),pvals=pvals,
#                  selection=list(aic=which.min(aic),bic=which.min(bic),ftest=ftest)))
#   return(lagstats$selection) # while all this structure is good stats
#   # we want to know what the optimum lag
#   #the AIC, BIC, and F-test chose you pick the number that repeats
# }
# 
# # 1) Part 2: Granger Causality -------------------------------------------------------
# # Karla, I was not sure if I was clear in my direction so I started writting 
# # some of the Granger Test code.  Both these articles were useful: 
# # https://stats.stackexchange.com/questions/131261/granger-causality-interpretation-using-r
# # https://stats.stackexchange.com/questions/160671/estimate-lag-for-granger-causality-test
# # https://www.youtube.com/watch?v=spHcV1Z4f10 Set them equal to x and y and run
# # the for loop.  Y is the dependent variable
# 
# # Fill with new variables here:
# y <- df0$housingburden 
# x <- df0$povertyrate
# 
# # Step 2) Find the lag between x and y.  
# laglist <- select.lags(x,y) #lag found using forecasting/time series analysis
# lag <- mean(as.numeric(laglist)) #Really here we should pick the mode.
# #^to simplify I use min
# 
# # Normally with time series or any data with seasonality data this is a very
# # important step. In math terms, The x variable might have autocorrelation
# # issues especially if its not a geographic variable. I think we could assume
# # the lag = 0 because there is no time based seasonality across data for the
# # same date. Also periodic motion (and autocorrelation in general) is a good
# # thing for geographic data However, as a teachable moment, I included it.
# 
# # Step 3) Run this loop
# # Optional: If you wanted to create for loop to run throug each case feel free
# # Format is grangertest(yvariable ~ xvariable, order = lag, data = df0)
# grangertest(y~x, order = lag , data=df0)
# grangertest(x~y, order = lag , data=df0)
# grangertest(y~x^2, order = lag , data=df0)
# grangertest(x~y^2, order = lag , data=df0)
# grangertest(y~x^3, order = lag , data=df0)
# grangertest(y~x^5, order = lag , data=df0)
# grangertest(y~log(x), order = lag , data=df0)
# grangertest(y~exp(x), order = lag , data=df0)
# # Step 4) repeat
# y <- df0$housingburden
# x <- df0$momsnohs
# laglist <- select.lags(x,y) #lag found using forecasting/time series analysis
# lag <- min(as.numeric(laglist)) #normally we would pick the mode
# grangertest(y~x, order = lag , data=df0)
