"*************************************************
          UNITED WAY SHINY DASHBOARD
Date: February 28th 2018

*************************************************"
# Getting Started EXample --
library(ggplot2)
# NECESSARY PACKAGES ----------------------------------------------------
library(shiny)
library(flexdashboard)
library(shinydashboard)
source('model/UW_R_Script_final.R')
source('model/coefficents.R')
library(readxl)
library(dplyr)
library(data.table)
library(plotly)
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
    menuItem( text = "Select Variable",
              icon = icon("th"),
              selectInput(inputId = 'county',
                          label = "",
                          choices = mynames)
    ),
    sliderInput("final",
                "% Improvement (will be replaced later)",
                min = -20,
                max = 20,
                value = -3,
                sep ="",step = 1,ticks = FALSE)
  ),
  #To Sifael I tried to put this in it own drop down but now luck :/
  sidebarMenu(
    sliderInput("gradrate","Gradrate Range",
                min = df2$gradrate[1],max = df2$gradrate[2],
                value = c(df2$gradrate[3],df2$gradrate[2]), #for the upper range
                sep ="",step = .01, ticks = FALSE),
    sliderInput("ccrpi","College and Career Readiness",
                min = df2$ccrpi[1],max = df2$ccrpi[2],
                value = c(df2$ccrpi[3],df2$ccrpi[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("grade3","% Exceed 3rd Grade Reading",
                min = df2$grade3[1],max = df2$grade3[2],
                value = c(df2$grade3[3],df2$grade3[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("grade8","% Exceed 8th Grade Math",
                min = df2$grade8[1],max = df2$grade8[2],
                value = c(df2$grade8[3],df2$grade8[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("lwb","Low Weight Births",
                min = df2$lwb[1],
                max = df2$lwb[2],
                value = c(df2$lwb[1],df2$lwb[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("childnohealth","% Childern No Health Ins",
                min = df2$childnohealth[1],
                max = df2$childnohealth[2],
                value = c(df2$childnohealth[1],df2$childnohealth[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("childpoverty", "Child Poverty Rate",
                min = df2$childpoverty[1],
                max = df2$childpoverty[2],
                value = c(df2$childpoverty[1],df2$childpoverty[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("povertyrate",
                "Family Poverty Rate",
                min = df2$povertyrate[1],
                max = df2$povertyrate[2],
                value = c(df2$povertyrate[1],df2$povertyrate[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("housingburden",
                "% of Housing Burden",
                min = df2$housingburden[1],
                max = df2$housingburden[2],
                value = c(df2$housingburden[1],df2$housingburden[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("momsnohs",
                "% Moms with no Highschool Diploma",
                min = df2$momsnohs[1],
                max = df2$momsnohs[2],
                value = c(df2$momsnohs[1],df2$momsnohs[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("collegerate",
                "% Enrolled in Post Secondary Edu",
                min = df2$collegerate[1],
                max = df2$collegerate[2],
                value = c(df2$collegerate[3],df2$collegerate[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("adultsnoedu",
                "% Adults with no Highschool Diploma",
                min = df2$adultsnoedu[1],
                max = df2$adultsnoedu[2],
                value = c(df2$adultsnoedu[1],df2$adultsnoedu[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("adultnohealth",
                "% Adults wit no Health Ins",
                min = df2$adultnohealth[1],
                max = df2$adultnohealth[2],
                value = c(df2$adultnohealth[1],df2$adultnohealth[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("unemployment",
                "Unemployment Rate",
                min = df2$unemployment[1],
                max = df2$unemployment[2],
                value = c(df2$unemployment[1],df2$unemployment[3]), sep ="",step = .01, ticks = FALSE)
  )
)

# "===========================================
#                   BODY
# ==========================================="
body = dashboardBody(
  # source(coefficients) #to get pop.Coef
  fluidRow(column = 2,gaugeOutput("gauge"),gaugeOutput("gauge2"))
  #, fluidRow(
  #   plotlyOutput("gauge2")
  # )
)

# "===========================================
# SERVER
# ==========================================="
#I am kind of stuck on this line.  How can I append the input in Rshiny??
# input <- append(input,df2) #How do I make reactive?

server = function(input, output){
  #I tried to make a reactive dashboard but I am really not sure how to hook this up
  # df3 <- reactive({
  #   df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 
  #   return(df2)
  # })
  # Actual Calculation and react step----
  # To Do later: I still cannot get this to work right. 
  CWBI <- reactive({
    #From Mike to Sifael: this and the next line are the react steps I am stuck on
    df3 <- df4 <- df2
    df3["Mean",] <- c(median(input$gradrate),median(input$ccrpi),median(input$grade3),
             median(input$grade8),median(input$lwb),median(input$childnohealth),
             median(input$childpoverty),median(input$povertyrate),median(input$housingburden),
             median(input$momsnohs),median(input$collegerate),median(input$adultsnoedu),
             median(input$adultnohealth),median(input$unemployment))
    myCoef <- pop.Coef(df0) #prep step from coefficents.R
    minCWB_Z = min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
    maxCWB_Z = max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
    #***We are optimizing this CwBZ
    Mean <- df3["Mean",]*(1+input$final/100) #(1+input$final)/100 is a placeholder for optimizer) 
    CWBZ <- rowMeans(myCoef$coefficients*Mean - myCoef$B)
    CWBI <- round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z),3)*(1+input$final/100)
    unname(CWBI)
    # CWBI <- median(input$gradrate)*(1+input$final/100)
  })
  output$gauge = renderGauge({
    CWBI <- CWBI()
    gauge(CWBI, 
          min = 0,
          max = 1,
          sectors = gaugeSectors(success = c(.589, 1.0),
                                 danger = c(0, .589))
    )
  })
  output$gauge2 = renderGauge({finalvalue = median(input$lwb)
    gauge(finalvalue, min = df2$lwb[1], max = df2$lwb[2],
          sectors = gaugeSectors(success = c(0, df2$lwb[3]),
                                 danger = c(df2$lwb[3], 100)))})
}


"===========================================
RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)
