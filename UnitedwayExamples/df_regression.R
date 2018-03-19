#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2018-03-16 13:21:32 x86_64-w64-mingw32 

# Rattle version 5.1.0 user 'dusky'

# This log captures Rattle interactions as an R script. 

# For repeatability export this log of all activity to a 
# file using the Export button or the Tools menu. This 
# script can serve as a starting point for developing your 
# own scripts. Exporting to a file called 'model.R' will 
# allow you to type into a new R Console the command 
#"source('model.R')" and so repeat all actions. Generally, 
# you will want to edit the file to suit your own needs. 
# You can also edit this log in place to record additional 
# information before exporting the script. 
 
# Note that saving/loading projects retains this log.

# We begin most scripts by loading the required packages.
# Here are some initial packages to load and others will be
# identified as we proceed through the script. When writing
# our own scripts we often collect together the library
# commands at the beginning of the script here.

library(rattle)   # Access weather dataset and utilities.
library(magrittr) # For the %>% and %<>% pipeline operators.

# This log generally records the process of building a model. 
# However, with very little effort the log can also be used 
# to score a new dataset. The logical variable 'building' 
# is used to toggle between generating transformations, 
# when building a model and using the transformations, 
# when scoring a dataset.

building <- TRUE
scoring  <- ! building

# A pre-defined value is used to reset the random seed 
# so that results are repeatable.

#crv$seed <- 42 

#============================================================
# Rattle timestamp: 2018-03-16 13:22:04 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- df_complete

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2018-03-16 13:22:05 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3",
                   "grade8", "lbw", "childnohealth", "childpoverty",
                   "povertyrate", "housingburden", "momsnohs", "collegerate",
                   "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x",
                   "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y",
                   "FamilyZ.y", "CommunityZ.y", "CWB_Z")

crs$numeric   <- c("weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8",
                   "lbw", "childnohealth", "childpoverty", "povertyrate",
                   "housingburden", "momsnohs", "collegerate", "adultsnoedu",
                   "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x",
                   "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y",
                   "CommunityZ.y", "CWB_Z")

crs$categoric <- "county"

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:24:15 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("childpoverty", "povertyrate", "momsnohs", "adultsnoedu")

crs$numeric   <- c("childpoverty", "povertyrate", "momsnohs", "adultsnoedu")

crs$categoric <- NULL

crs$target    <- "adultnohealth"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:24:17 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2018-03-16 13:33:00 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("childpoverty", "povertyrate", "momsnohs", "adultsnoedu")

crs$numeric   <- c("childpoverty", "povertyrate", "momsnohs", "adultsnoedu")

crs$categoric <- NULL

crs$target    <- "adultnohealth"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:33:07 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(adultnohealth ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.05 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:36:28 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("childpoverty", "povertyrate")

crs$numeric   <- c("childpoverty", "povertyrate")

crs$categoric <- NULL

crs$target    <- "momsnohs"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:36:52 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2018-03-16 13:37:32 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(momsnohs ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.01 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:38:07 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(momsnohs ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], family=gaussian(identity))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:39:57 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("childpoverty", "povertyrate")

crs$numeric   <- c("childpoverty", "povertyrate")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "momsnohs", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:41:19 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("ccrpi", "childpoverty", "povertyrate")

crs$numeric   <- c("ccrpi", "childpoverty", "povertyrate")

crs$categoric <- NULL

crs$target    <- "momsnohs"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:41:27 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("ccrpi", "childpoverty", "povertyrate")

crs$numeric   <- c("ccrpi", "childpoverty", "povertyrate")

crs$categoric <- NULL

crs$target    <- "momsnohs"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:41:53 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(momsnohs ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.01 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:45:52 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("povertyrate", "housingburden")

crs$numeric   <- c("povertyrate", "housingburden")

crs$categoric <- NULL

crs$target    <- "childpoverty"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "momsnohs", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:45:55 x86_64-w64-mingw32 

# Generate a correlation plot for the variables. 

# The 'corrplot' package provides the 'corrplot' function.

library(corrplot, quietly=TRUE)

# Correlations work for numeric variables only.

crs$cor <- cor(crs$dataset[crs$sample, crs$numeric], use="pairwise", method="pearson")

# Order the correlations by their strength.

crs$ord <- order(crs$cor[1,])
crs$cor <- crs$cor[crs$ord, crs$ord]

# Display the actual correlations.

print(crs$cor)

# Graphically display the correlations.

corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2018-03-16 13:46:08 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(childpoverty ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-03-16 13:46:22 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(childpoverty ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], family=gaussian(identity))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

#============================================================
# Rattle timestamp: 2018-03-16 13:46:28 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(childpoverty ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], family=gaussian(identity))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:47:36 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(childpoverty ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.01 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 13:47:42 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("povertyrate", "housingburden")

crs$numeric   <- c("povertyrate", "housingburden")

crs$categoric <- NULL

crs$target    <- "childpoverty"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "momsnohs", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:47:45 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(312179)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("povertyrate", "housingburden")

crs$numeric   <- c("povertyrate", "housingburden")

crs$categoric <- NULL

crs$target    <- "childpoverty"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "momsnohs", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:47:48 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(673962)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("povertyrate", "housingburden")

crs$numeric   <- c("povertyrate", "housingburden")

crs$categoric <- NULL

crs$target    <- "childpoverty"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "momsnohs", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 13:47:59 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(childpoverty ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.
# residualsA <- residuals(crs$glm)
print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 14:18:10 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(673962)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("childpoverty", "povertyrate")

crs$numeric   <- c("childpoverty", "povertyrate")

crs$categoric <- NULL

crs$target    <- "momsnohs"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "housingburden", "collegerate", "adultsnoedu", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 14:20:23 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(momsnohs ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.
residualsB <- residuals(crs$glm)
print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 14:31:55 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(673962)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- #c("povertyrate")

crs$numeric   <- #c("povertyrate")

crs$categoric <- NULL

crs$target    <- "adultsnoedu"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("mcounty", "weave_ct2010", "gradrate", "ccrpi", "grade8", "lbw", "childnohealth", "childpoverty", "housingburden", "collegerate", "adultnohealth", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-03-16 14:32:03 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(adultsnoedu ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.
residualsC <- residuals(crs$glm)
print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.02 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-03-16 14:47:48 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(673962)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("povertyrate", "momsnohs", "adultsnoedu","childpoverty")

crs$numeric   <- c("povertyrate", "momsnohs", "adultsnoedu","childpoverty")

crs$categoric <- NULL

crs$target    <- "adultnohealth"
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- c("county", "weave_ct2010", "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth", "childpoverty", "housingburden", "collegerate", "unemployment", "ChildZ.x", "FamilyZ.x", "CommunityZ.x", "CWB_Index", "ChildZ.y", "FamilyZ.y", "CommunityZ.y", "CWB_Z")
crs$weights   <- NULL
data2 <- data.frame(crs$dataset[crs$train,c(crs$input, crs$target)])
data2$momsnohs <- residualsB
data2$adultsnoedu <- residualsC
data2$childpoverty <- residualsA
names(data2) <- c("povertyrate","residualsB","residualsC","residualsB","adultnohealth")
crs$glm <- lm(adultnohealth ~ povertyrate+residualsA+residualsB+residualsC, data2)

## THE RESULTS ---- #####
# 0 => -.0346*residualA+.2647*povertyrate+0.316*residualB+0.483*residualC+5.555 - adultsnohealth 
# 0 => 0.005*grade3+0.105*povertyrate+0.583*residualB+1.26+residualC - adultsnoedu
# 0 => -0.302*povertyrate+0.087*residualA+-0.302+residualB+momsnohs
# 0 => -0.397*povertyrate+0.13*housingburnden-4.38+residualA+Childpoverty


#============================================================
# Rattle timestamp: 2018-03-16 14:48:02 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

#save(crs, file="C:\Users\dusky\Documents\R\unitedway\model\df_complete2rattle.rattle", compress=TRUE)
