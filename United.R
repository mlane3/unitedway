"*************************************************
          UNITED WAY SHINY DASHBOARD
        Date: February 28th 2018

*************************************************"

# NECESSARY PACKAGES ----------------------------------------------------
library(shiny)
library(shinydashboard)
source("UW_R_Script_final.R")
source('coefficents.R')
library(readxl)
library(dplyr)
library(data.table)

# setwd("~/R/unitedway")
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','weave_ct2010','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultnoedu','adultsnohealth','unemployment')

"===========================================
                HEADER
==========================================="
header = dashboardHeader(title = 'United Way App')


"===========================================
                SIDEBAR
==========================================="
sidebar = dashboardSidebar(
  
  sidebarMenu( 
                menuItem( text = "Select County:",
                          icon = icon("th"),
                          selectInput(inputId = 'county',
                                      label = "",
                                      choices = c("Michael",
                                                  "Purush",
                                                  "Sifael"))
                          
                         )
    
    
              )
  
)

"===========================================
                  BODY
==========================================="
body = dashboardBody()
# Getting Started EXample --

# mean(df_index_100$CWB_Index) #the actual CWBI 
minCWB_Z = min(df_index$CWB_Z) #-1.969282
maxCWB_Z = max(df_index$CWB_Z) #1.380706
df2 <- as.data.frame(read.csv("overall constrants.csv", skip = 2, row.names = 1)) 
#^***Sifeal df2 is what your taking in for this one and can use to modify the sliders
myCoef <- pop.Coef(df0) #use for the orginal data frame to get y = m*x - b coeff
CWBZ <- rowMeans(myCoef$coefficients*df2["Mean",] - myCoef$B) #***We are optimizing this
CWB_Index <- (CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z) 
#which should equal 0.5895567 our test CWBI and .001 away from actual CWBI

"===========================================
                SERVER
==========================================="
server = function(input, output){}


"===========================================
                RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'yellow',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)


