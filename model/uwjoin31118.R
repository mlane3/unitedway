library(readxl)
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','TRACT','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultsnoedu','adultnohealth','unemployment')


UWcensuszip <- read_excel("data/ZIP_TRACT_122010.xlsx")

# merge two data frames by ID
dfzipmap <- merge(df0,UWcensuszip,by="TRACT")
