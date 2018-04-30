#This is a helpful list of each indicators name
variablenamelist <- data.table(
variable = c( "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth",
"childpoverty", "povertyrate", "housingburden", "momsnohs", "collegerate",
"adultsnoedu", "adultnohealth", "unemployment" ),
number = 1:14,
title = c( "Graduation Rate", "College and Career Readiness Score", "%
Childern exceed 3rd grade reading standard", "% Childern exceed 8th grade math
standard", "Low Birth Weight", "% Children without health insurance", "% Childern
in poverty", "% families not financially stable", "% Familes With Housing Cost
burden", "% of Moms with no high school", "% Adults in Post-Secondary
Education", "% Adults with no high school", "% Adults without health insurance",
"Unemployment Rate"),
plotbutton = c(1,rep(0,13)))
write.csv(variablenamelist,"data/variablenames.csv")
