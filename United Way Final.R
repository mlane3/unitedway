"*********************************************************

            UNITED WAY FINAL APPLICATION
                Date: March 6th 2018

*********************************************************"

# NECESSARY PACKAGES
# Shiny Dependencies
library(shiny)
library(shinydashboard)
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
original = read_xlsx("2016 Original Data.xlsx")
names(original) = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultnoedu','adultsnohealth','unemployment')


# Overall Constraints
overall_constraints = as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
full_names = as.data.frame(read.csv("data/overall constrants.csv", nrows = 3, row.names = 1))

#"*********************************************
#                  HEADER
#*********************************************"
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
  values = c( overall_constraints[2, input$variable], max_value )
  
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
  
})

# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )

  
# PLOTTING THE GAUGE
output$GaugePlot = renderAmCharts(
  
    
    # AM Angular Gauge
    amAngularGauge(x = median(as.vector(input$metric)), 
                   start = overall_constraints[1, input$variable], 
                   end = overall_constraints[2, input$variable],
                   main = input$variable)
  
  
) 


output$sample = renderText({ input$metric })


"*******************************
          MAIN GRID
*******************************"
output$MainGrid = renderUI({
  
      # Evaluating the  Overall Page
      if (is.null(input$county))
      {
        p("Welcome to United Way App", br,
          "Please Select a county to begin")
      } else {
        tabsetPanel(tabPanel("Gauge Plots Here", amChartsOutput('GaugePlot')),
                    tabPanel("Additional Content here", textOutput('test')))        
        
        
      }
        
})


}

"*********************************************
                 RUNAPP
*********************************************"
shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body),
          server = server)