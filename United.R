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
#Mike will to clean this

# setwd("~/R/unitedway")
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','weave_ct2010','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultnoedu','adultsnohealth','unemployment')
minCWB_Z = min(df_index$CWB_Z) #-1.969282
maxCWB_Z = max(df_index$CWB_Z) #1.380706
df2 <- as.data.frame(read.csv("overall constrants.csv", skip = 2, row.names = 1)) 

"===========================================
                HEADER
==========================================="
header = dashboardHeader(title = 'United Way App')

"===========================================
                SIDEBAR
==========================================="
choices <- unique("overall",df0$county)
sidebar = dashboardSidebar(

  sidebarMenu( 
                menuItem( text = "Select County:",
                          icon = icon("th"),
                          selectInput(inputId = 'county',
                                      label = "",
                                      choices = c("Michael",
                                                  "Purush",
                                                  "Sifael"))
                          
                         ),
    
                sliderInput("lbw",
                            "Low Weight Births",
                            min = df2$lwb[1],
                            max = df2$lwb[2],
                            value = df2$lwb[3])
              ),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("lbw",
                  "Low Weight Births",
                  min = df2$lwb[1],
                  max = df2$lwb[2],
                  value = df2$lwb[3])
    ),
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

"===========================================
                  BODY
==========================================="
body = dashboardBody()
# Getting Started EXample --


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


