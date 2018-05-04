# Data Dictionary and Key ----
# This is a helpful list of each indicators name

# CWBI = Child Well Being Index
# variables = list of indicators (this is used to define row and columns)
# number = the column or row number of the variable usually in the data
# mynames = a placeholder for variable
# title = the label the user sees for each variable 
# resulttitle = the label the user sees when the optimizer is turned on.
##think of title as S the start values label and result as R final value label
# plotbutton = what stores the user input when they want to fix the constraints
##if plotbutton = 0 

# A good reference can be found at:
# variablenamelist2 <- read_xlsx("data/VariableNames-Suggested20180430.xlsx")
variablenamelist <- data.table(
  variable = c( "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth",
                "childpoverty", "povertyrate", "housingburden", "momsnohs", "collegerate",
                "adultsnoedu", "adultnohealth", "unemployment" ),
  number = 1:14,
  title = c( 'S: HS Graduation Rate', 'S: HS College&Career Readiness', 'S: %
             Exceed 3rd Gr Reading Std', 'S: % Exceed 8th Gr Math Std', 'S: % Low Weight
             Births', 'S: % Children w/o Health Ins', 'S: % Children in Poverty', 'S: %
             Families Not Finan Stable', 'S: % Families w/Housing Burden', 'S: % Moms w/o
             HS Diploma', 'S: % Enrolled Post-Second Educ', 'S: % Adults w/o HS Diploma',
             'S: % Adults w/o Health Ins', 'S: Unemployment Rate'),
  plotbutton = c(0,69.8,rep(0,12)),
  resulttitle = c( 'R: HS  Graduation Rate', 'R: HSCollege&Career Readiness',
                   'R: % Exceed 3rd Gr Reading Std', 'R: % Exceed 8th Gr Math Std', 'R:  % Low
                   Weight Births', 'R: % Children w/o Health Ins', 'R: % Children in Poverty',
                   'R: % Families Not Finan Stable', 'R: % Families w/Housing Burden', 'R: % Moms
                   w/o HS Diploma', 'R: % Enrolled Post-Second Educ', 'R: % Adults w/o HS
                   Diploma', 'R: % Adults w/o Health Ins', 'R: Unemployment Rate')
  )

# Guide of certain variable names in other functions ----
# original = original from "2016 Original Data"
# df0 = another name for original data need to be replaced in the code with the word orginal

# overall_constraints = boundary conditions and average values for each of the indicators
##these boundary conditions are dervived from original
##the average is the average over all the census tracks.
# df2 = the intialized values for overall_constraints

# Script
# lpsolver.R = the heart of the code that runs the optimiztaion
# getCWBI and lp.test the functions that actually run the optimization
# getCWBI calles lp.test and lp.test runs the optimization

# metric = the moving sliders
# pop.Coeff() = the Weighted Coefficents of Child Well Being
##pop.[...] = auxilary functions use to calculate the original CWBI



## .ignore after this line
# title = c( "Graduation Rate", "College and Career Readiness Score", "%
# Childern exceed 3rd grade reading standard", "% Childern exceed 8th grade math
#            standard", "Low Birth Weight", "% Children without health insurance", "% Childern
#            in poverty", "% families not financially stable", "% Familes With Housing Cost
#            burden", "% of Moms with no high school", "% Adults in Post-Secondary
#            Education", "% Adults with no high school", "% Adults without health insurance",
#            "Unemployment Rate"),
