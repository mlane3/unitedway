#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2018-02-04 19:55:17 x86_64-w64-mingw32 

# Rattle version 5.1.0 user 'mylan'

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

crv$seed <- 42 

#============================================================
# Rattle timestamp: 2018-02-04 19:59:30 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- df0

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2018-02-04 19:59:30 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("county", "weave_ct2010", "Educ_HS_GradRateF",
                   "Educ_HS_CCRPI_ScoreF", "Educ_pcExceeding_3Grade_ReadingF",
                   "Educ_pc_Exceeding_8Grade_MathF", "Health_pcLBW_BirthsF",
                   "Health_pc_Under18_no_Health_InsF",
                   "Income_pcUnder18_in_povF", "Income_pcFam_below200pc_povF",
                   "Income_pcHH_HousingBurdenF", "Birth_to_Moms_noHSF",
                   "Educ_pc_postsecF", "Educ_pcAdults_no_HSF",
                   "Health_pcAdults_no_Health_InsF",
                   "Income_Unemployment_rateF")

crs$numeric   <- c("weave_ct2010", "Educ_HS_GradRateF",
                   "Educ_HS_CCRPI_ScoreF", "Educ_pcExceeding_3Grade_ReadingF",
                   "Educ_pc_Exceeding_8Grade_MathF", "Health_pcLBW_BirthsF",
                   "Health_pc_Under18_no_Health_InsF",
                   "Income_pcUnder18_in_povF", "Income_pcFam_below200pc_povF",
                   "Income_pcHH_HousingBurdenF", "Birth_to_Moms_noHSF",
                   "Educ_pc_postsecF", "Educ_pcAdults_no_HSF",
                   "Health_pcAdults_no_Health_InsF",
                   "Income_Unemployment_rateF")

crs$categoric <- "county"

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 19:59:36 x86_64-w64-mingw32 

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
title(main="Correlation df0 using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2018-02-04 20:01:17 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- df_complete

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2018-02-04 20:01:17 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("wv_c2010", "county.x", "E_HS_GRF", "E_HS_CCR",
                   "E_E_3G_R", "E__E_8G_", "H_LBW_BF", "H__U18__", "I_U18__F",
                   "I_F_200_", "I_HH_HBF", "B__M_HSF", "Edc_pc_F", "E_A__HSF",
                   "H_A__H_I", "Incm_U_F", "ChildZ", "FamilyZ", "CommntyZ",
                   "CWB_Z")

crs$numeric   <- c("wv_c2010", "E_HS_GRF", "E_HS_CCR", "E_E_3G_R",
                   "E__E_8G_", "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_",
                   "I_HH_HBF", "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I",
                   "Incm_U_F", "ChildZ", "FamilyZ", "CommntyZ", "CWB_Z")

crs$categoric <- "county.x"

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- NULL
crs$ignore    <- NULL
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 20:01:27 x86_64-w64-mingw32 

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
# Rattle timestamp: 2018-02-04 21:01:26 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "ChildZ", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:01:31 x86_64-w64-mingw32 

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

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2018-02-04 21:02:30 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ", "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ", "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- "county.x"
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:02:47 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ", "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ", "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- "county.x"
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:02:50 x86_64-w64-mingw32 

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
# Rattle timestamp: 2018-02-04 21:03:22 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "ChildZ", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:03:25 x86_64-w64-mingw32 

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

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2018-02-04 21:03:56 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:04:12 x86_64-w64-mingw32 

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
# Rattle timestamp: 2018-02-04 21:04:47 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "CWB_Z")

crs$categoric <- NULL

crs$target    <- NULL
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "ChildZ", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:04:56 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "ChildZ", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:05:03 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.38 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-02-04 21:07:17 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

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
# Rattle timestamp: 2018-02-04 21:08:13 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:08:19 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

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
# Rattle timestamp: 2018-02-04 21:08:58 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:09:12 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ", "FamilyZ", "CommntyZ")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- "county.x"
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:09:17 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- lm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)])

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
# Rattle timestamp: 2018-02-04 21:11:34 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], family=gaussian(identity))

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
# Rattle timestamp: 2018-02-04 21:12:02 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "ChildZ", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:12:10 x86_64-w64-mingw32 

# Regression model 

# Build a Regression model.

crs$glm <- glm(CWB_Z ~ ., data=crs$dataset[crs$train,c(crs$input, crs$target)], family=gaussian(identity))

# Generate a textual view of the Linear model.

print(summary(crs$glm))
cat('==== ANOVA ====

')
print(anova(crs$glm))
print("
")

# Time taken: 0.03 secs

# Plot the model evaluation.

ttl <- genPlotTitleCmd("Linear Model",crs$dataname,vector=TRUE)
plot(crs$glm, main=ttl[1])

#============================================================
# Rattle timestamp: 2018-02-04 21:13:22 x86_64-w64-mingw32 

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

opar <- par(cex=0.5)
corrplot(crs$cor, mar=c(0,0,1,0))
title(main="Correlation df_complete using Pearson",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
par(opar)

#============================================================
# Rattle timestamp: 2018-02-04 21:14:19 x86_64-w64-mingw32 

# Note the user selections. 

# Build the train/validate/test datasets.

# nobs=776 train=543 validate=116 test=117

set.seed(crv$seed)

crs$nobs     <- nrow(crs$dataset)
crs$train    <- crs$sample <- sample(crs$nobs, 0.7*crs$nobs)
crs$validate <- sample(setdiff(seq_len(crs$nobs), crs$train), 0.15*crs$nobs)
crs$test     <- setdiff(setdiff(seq_len(crs$nobs), crs$train), crs$validate)

# The following variable selections have been noted.

crs$input     <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$numeric   <- c("E_HS_GRF", "E_HS_CCR", "E_E_3G_R", "E__E_8G_",
                   "H_LBW_BF", "H__U18__", "I_U18__F", "I_F_200_", "I_HH_HBF",
                   "B__M_HSF", "Edc_pc_F", "E_A__HSF", "H_A__H_I", "Incm_U_F",
                   "ChildZ")

crs$categoric <- NULL

crs$target    <- "CWB_Z"
crs$risk      <- NULL
crs$ident     <- "wv_c2010"
crs$ignore    <- c("county.x", "FamilyZ", "CommntyZ")
crs$weights   <- NULL

#============================================================
# Rattle timestamp: 2018-02-04 21:14:47 x86_64-w64-mingw32 

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
# Rattle timestamp: 2018-02-04 21:29:45 x86_64-w64-mingw32 

# Principal Components Analysis (on numerics only).

pc <- prcomp(na.omit(crs$dataset[crs$sample, crs$numeric]), scale=TRUE, center=TRUE, tol=0)

# Show the output of the analysis.

pc

# Summarise the importance of the components found.

summary(pc)

# Display a plot showing the relative importance of the components.

plot(pc, main="")
title(main="Principal Components Importance df_complete",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))
axis(1, at=seq(0.7, ncol(pc$rotation)*1.2, 1.2), labels=colnames(pc$rotation), lty=0)

# Display a plot showing the two most principal components.

biplot(pc, main="")
title(main="Principal Components df_complete",
    sub=paste("Rattle", format(Sys.time(), "%Y-%b-%d %H:%M:%S"), Sys.info()["user"]))

#============================================================
# Rattle timestamp: 2018-02-04 21:30:12 x86_64-w64-mingw32 

# BiCluster 

# The 'biclust' package provides the 'biclust' function.

library(biclust, quietly=TRUE)

# Reset the random number seed to obtain the same results each time.

set.seed(crv$seed)

# Generate BiCluster using method 'BCCC'.

crs$biclust <- biclust(as.matrix(na.omit(crs$dataset[crs$sample, crs$numeric])),method=BCCC)

# Generate a textual view of the BiCluster model.

print(crs$biclust)

# Time taken: 1.61 secs

#============================================================
# Rattle timestamp: 2018-02-04 22:57:48 x86_64-w64-mingw32 

# Save the project data (variable crs) to file.

save(crs, file="C:\Users\mylan\Documents\R\unitedway\df_complete.rattle", compress=TRUE)
