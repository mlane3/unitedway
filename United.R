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
  ),
  fluidRow(
    plotlyOutput("gauge2")
  )
)
# Getting Started EXample --
library(ggplot2)
"===========================================
                SERVER
==========================================="
#I am kind of stuck on this line.  How can I append the input in Rshiny??
# input <- append(input,df2) #right now just using df2 into server instead

#Fluid  attempt
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
  output$gauge2 <-renderPlotly({plot_ly(
      type = "pie",
      values = c(40, 10, 10, 10, 10, 10, 10),
      labels = c("-", "0", "20", "40", "60", "80", "100"),
      rotation = 108,
      direction = "clockwise",
      hole = 0.4,
      textinfo = "label",
      textposition = "outside",
      hoverinfo = "none",
      domain = list(x = c(0, 0.48), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)')),
      showlegend = FALSE
    )
    base_plot <- add_trace(
      base_plot,
      type = "pie",
      values = c(50, 10, 10, 10, 10, 10),
      labels = c("Error Log Level Meter", "Debug", "Info", "Warn", "Error", "Fatal"),
      rotation = 90,
      direction = "clockwise",
      hole = 0.3,
      textinfo = "label",
      textposition = "inside",
      hoverinfo = "none",
      domain = list(x = c(0, 0.48), y = c(0, 1)),
      marker = list(colors = c('rgb(255, 255, 255)', 'rgb(232,226,202)', 'rgb(226,210,172)', 'rgb(223,189,139)', 'rgb(223,162,103)', 'rgb(226,126,64)')),
      showlegend= FALSE
    )
    a <- list(
      showticklabels = FALSE,
      autotick = FALSE,
      showgrid = FALSE,
      zeroline = FALSE)
    
    b <- list(
      xref = 'paper',
      yref = 'paper',
      x = 0.23,
      y = 0.45,
      showarrow = FALSE,
      text = str(input$lwbfinal))
    
    base_chart <- layout(
      base_plot,
      shapes = list(
        list(
          type = 'path',
          line = list(width = .5)
          path = 'M 0.235 0.5 L 0.24 0.62 L 0.245 0.5 Z',
          xref = 'paper',
          yref = 'paper',
          fillcolor = 'rgba(44, 160, 101, 0.5)'
        )
      ),
      xaxis = a,
      yaxis = a,
      annotations = b
    )
  })
}


"===========================================
                RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)


