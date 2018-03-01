"*************************************************
          UNITED WAY SHINY DASHBOARD
        Date: February 28th 2018

*************************************************"

# NECESSARY PACKAGES ----------------------------------------------------
library(shiny)
library(flexdashboard)
library(shinydashboard)
source('model/UW_R_Script_final.R')
source('model/coefficents.R')
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
df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

# "===========================================
#                 HEADER -----
# ==========================================="
header = dashboardHeader(title = 'United Way App')

# "===========================================
#                 SIDEBAR
# ==========================================="
choices <- unique(c("overall",df0$county))
mynames <- names(df2)
sidebar = dashboardSidebar(

  sidebarMenu( 
                menuItem( text = "Select County:",
                          icon = icon("th"),
                          selectInput(inputId = 'county',
                                      label = "",
                                      choices = choices)
                          
                         ),
                menuItem( text = "Select Variable (just a demo)",
                          icon = icon("th"),
                          selectInput(inputId = 'county',
                                      label = "",
                                      choices = mynames)
                          
                )
    
              ),
  sliderInput("lwb",
              "Low Weight Births Range",
              min = df2$lwb[1],
              max = df2$lwb[2],
              value = c(df2$lwb[1],df2$lwb[3]),
              sep ="",step = .01),
  sliderInput("lwbfinal",
              "Low Weight Births Improvement",
              min = df2$lwb[1],
              max = df2$lwb[2],
              value = df2$lwb[3],
              sep ="",step = .01)
)

# "===========================================
#                   BODY
# ==========================================="
body = dashboardBody(
  fluidRow(
    gaugeOutput("gauge")
  )
)
# Getting Started EXample --


"===========================================
                SERVER
==========================================="
#I am kind of stuck on this line.  How can I append the input in Rshiny??
# input <- append(input,df2) #right now just using df2 into server instead

server = function(input, output){
  
  output$gauge = renderGauge({
    finalvalue = input$lwbfinal
    gauge(finalvalue, 
          min = df2$lwb[1], 
          max = df2$lwb[2],
          sectors = gaugeSectors(success = c(input$lwb[1], df2$lwb[3]),
                                 danger = c(df2$lwb[3], input$lwb[2]))
    )
  })
}


"===========================================
                RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'yellow',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)


