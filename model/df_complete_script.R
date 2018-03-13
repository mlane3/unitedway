#For united way and new contributors to R stuff
#============================================================

# Rattle is Copyright (c) 2006-2017 Togaware Pty Ltd.
# It is open source software and is freely available.
# It is licensed under the GNU General Public License,
# Version 2. Rattle comes with ABSOLUTELY NO WARRANTY.
# Rattle was written by Graham Williams with contributions
# from others as acknowledged in 'library(help=rattle)'.
# Visit https://rattle.togaware.com/ for details.

#============================================================
# Rattle timestamp: 2018-03-09 20:40:43 x86_64-w64-mingw32 

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
# Rattle timestamp: 2018-03-09 20:43:28 x86_64-w64-mingw32 

# Load an R data frame.

crs$dataset <- df_complete

# Display a simple summary (structure) of the dataset.

str(crs$dataset)

#============================================================
# Rattle timestamp: 2018-03-09 20:44:45 x86_64-w64-mingw32 

# Note the user selections. 

# The following variable selections have been noted.

crs$input     <- c("gradrate", "ccrpi", "grade3", "grade8", "lbw",
                   "childnohealth", "childpoverty", "povertyrate",
                   "housingburden", "momsnohs", "collegerate", "adultsnoedu",
                   "adultnohealth", "unemployment", "gradrate", "ccrpi",
                   "lbw", "CWB_Index")

crs$numeric   <- c("gradrate", "ccrpi", "grade3", "grade8", "lbw",
                   "childnohealth", "childpoverty", "povertyrate",
                   "housingburden", "momsnohs", "collegerate", "adultsnoedu",
                   "adultnohealth", "unemployment", "gradrate", "ccrpi",
                   "lbw", "CWB_Index")

crs$categoric <- NULL

crs$target    <- "county"
crs$risk      <- NULL
crs$ident     <- "weave_ct2010"
crs$ignore    <- NULL
crs$weights   <- NULL


#============================================================
# Rattle timestamp: 2018-03-09 20:42:32 x86_64-w64-mingw32 

# Display histogram plots for the selected variables. 

# Use ggplot2 to generate histogram plot for gradrate

# Generate the plot.



p01 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(county=as.factor(county)) %>%
  dplyr::select(gradrate, county) %>%
  ggplot2::ggplot(ggplot2::aes(x=gradrate)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=county, colour=county), alpha=0.55) +
  ggplot2::xlab("gradrate\n\nRattle 2018-Mar-09 20:42:32 dusky") +
  ggplot2::ggtitle("Distribution of gradrate\nby county") +
  ggplot2::labs(fill="county", y="Density")

# Use ggplot2 to generate histogram plot for ccrpi

# Generate the plot.

p02 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(county=as.factor(county)) %>%
  dplyr::select(ccrpi, county) %>%
  ggplot2::ggplot(ggplot2::aes(x=ccrpi)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=county, colour=county), alpha=0.55) +
  ggplot2::xlab("ccrpi\n\nRattle 2018-Mar-09 20:42:32 dusky") +
  ggplot2::ggtitle("Distribution of ccrpi\nby county") +
  ggplot2::labs(fill="county", y="Density")

# Use ggplot2 to generate histogram plot for lbw

# Generate the plot.

p03 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(county=as.factor(county)) %>%
  dplyr::select(lbw, county) %>%
  ggplot2::ggplot(ggplot2::aes(x=lbw)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=county, colour=county), alpha=0.55) +
  ggplot2::xlab("lbw\n\nRattle 2018-Mar-09 20:42:32 dusky") +
  ggplot2::ggtitle("Distribution of lbw\nby county") +
  ggplot2::labs(fill="county", y="Density")

# Use ggplot2 to generate histogram plot for grade3

# Generate the plot.

p04 <- crs %>%
  with(dataset[,]) %>%
  dplyr::mutate(county=as.factor(county)) %>%
  dplyr::select(grade3, county) %>%
  ggplot2::ggplot(ggplot2::aes(x=grade3)) +
  ggplot2::geom_density(lty=3) +
  ggplot2::geom_density(ggplot2::aes(fill=county, colour=county), alpha=0.55) +
  ggplot2::xlab("grade3\n\nRattle 2018-Mar-09 20:42:32 dusky") +
  ggplot2::ggtitle("Distribution of grade3\nby county") +
  ggplot2::labs(fill="county", y="Density")

# Display the plots.

gridExtra::grid.arrange(p01, p02, p03, p04)






