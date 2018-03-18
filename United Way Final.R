"*********************************************************

            UNITED WAY FINAL APPLICATION
                Date: March 6th 2018

*********************************************************"

# NECESSARY PACKAGES
# Shiny Dependencies
#library(leaflet)
library(rAmCharts)
library(shiny)
library(shinydashboard)
library(flexdashboard)
# Plotting Dependencies
library(ggplot2)
library(plotly)
# Data Processing Dependencies
library(dplyr)
library(readxl)
library(data.table)
# Sourcing Prior Scripts
source('model/UW_R_Script_final.R')
source('model/coefficents.R')


# DATA CLEANING BEFORE SHINY
# Original dataset
original = df0 = read_xlsx("2016 Original Data.xlsx")
names(original) = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultnoedu','adultsnohealth','unemployment')


# Overall Constraints
overall_constraints <- df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
# county_constraints <- df1 = as.data.frame(read.csv("data/county constrants.csv"))
overall_constraints[1:3,] = round(overall_constraints[1:3,],.1)

#full_names = as.data.frame(read.csv("data/overall constrants.csv", nrows = 3, row.names = 1)

"*********************************************
                  HEADER
*********************************************"
header = dashboardHeader(title = 'United Way App')

"*********************************************
                 SIDEBAR
*********************************************"
counties = unique(c("overall", original$county))

variables = names(overall_constraints)

sidebar = dashboardSidebar(
  
      sidebarMenu( 
                    menuItem( text = "County Selection",
                              icon = icon('th'),
                              selectInput(inputId = 'county',
                                          label = 'Select County:',
                                          choices = counties ))),
      
      sidebarMenu( 
                    menuItem( text = "Variable Selection",
                              icon = icon('th'),
                              selectInput(inputId = 'variable',
                                          label = 'Select a Variable:',
                                          choices = variables )),
                    menuItemOutput('metric_slider'))
      

)

"*********************************************
                  BODY
*********************************************"
body = dashboardBody(uiOutput("MainGrid"))


"*********************************************
                 SERVER
*********************************************"
server = function(input, output){
  
# COUNTRY REACTIVE
variable_reactive = eventReactive(input$variable, 
{
  min_value = overall_constraints[1, input$variable]
  max_value = overall_constraints[2, input$variable]
  values <- c( overall_constraints[3, input$variable], max_value ) #not sure about this change

  # browser() #debug code
  if (length(input$variable == 1) )
    
      sidebarMenu( 
                    menuItem( text = "Metric Slider",
                              icon = icon('th'),
                              sliderInput( inputId = "metric",
                                           label = input$variable,
                                           min = min_value,
                                           max = max_value,
                                           value = values,
                                           sep ="",
                                           step = .1)
                              
                              
        )
                  
      )
  # CWBI <- median(input$gradrate)*(1+input$final/100)
})
# getCWBI <- reactive(input$variable,overall_constraints){
#   myCoef <- pop.Coef(df0) #prep step from coefficents.R
#   final <- (overall_constraints) # this used to be test2 <- overall_constraints
#   minCWB_Z = min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
#   maxCWB_Z = max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
#   #***We are optimizing this CwBZ
#   final2 <- final["Mean",] #input$final is a placeholder for optimizer)
#   CWBZ <- sum(myCoef$coefficients*final2 - myCoef$B)
#   CWBI <- round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3)
# }
myCoef <- pop.Coef(df0) #prep step from coefficents.R
final <- (overall_constraints) # this used to be test2 <- overall_constraints
minCWB_Z = min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
maxCWB_Z = max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
#***We are optimizing this CwBZ
final2 <- final["Mean",] #input$final is a placeholder for optimizer)
CWBZ <- sum(myCoef$coefficients*final2 - myCoef$B)
CWBI <- round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3)



# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )


# PLOTTING THE GAUGE
# output$gauge = renderGauge({
#   #str(input$attribs)
#   #str(input$lbw)
#   #typeof(input$lbw
#   gauge(CWBI, min = 0, max = 100,
#         sectors = gaugeSectors(success = c(58.9, 100),danger = c(0, 58.9))
#   )
# })
output$GaugeCWBI = renderAmCharts({
  test <- getCWBI()
  # getCWBI (Load child well being)
  # AM Angular Gauge
  # browser()
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100), 
                     color = c("#00CC00", "#ea3838"),
                     stringsAsFactors = FALSE)
  # browser()
  amAngularGauge(x = CWBI,
                 start = 0, end = 100,
                 main = "CWBI", bands = bands)}) 

output$sample = renderText({ input$metric })

output$GaugePlot = renderAmCharts({
    START = round(overall_constraints[1, input$variable],.1)
    value = round(overall_constraints[3, input$variable],.1)
    END = round(overall_constraints[2, input$variable],.1)
    # AM Angular Gauge
    bands = data.frame(start = c(START,value), end = c(value, END), 
                       color = c("#00CC00", "#ea3838"),
                       stringsAsFactors = FALSE)
    amAngularGauge(x = median(as.vector(input$metric)), 
                   start = START, end = END,
                   main = input$variable, bands = bands)}) 

# output$test = renderTable(append(input$metric))

"*******************************
          MAIN GRID
*******************************"
output$MainGrid = renderUI({
  
      # Evaluating the Overall Page
      if (is.null(input$county))
      {
        p("Welcome to United Way App", br,
          "Please Select a county to begin")
      } else {
        tabsetPanel(tabPanel("Gauge Plots Here", amChartsOutput('GaugePlot')),
                    tabPanel("Additional Content here", amChartsOutput('GaugeCWBI')))        
        # textOutput('sample')
        
      }
        
})


}

"*********************************************
                 RUNAPP
*********************************************"
# display.mode="showcase" #debug code
# options(shiny.reactlog=TRUE) #debug code
app <- shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body),
          server = server)
