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
# library(flexdashboard)
library(leaflet)
library(rAmCharts)
# Data Processing Dependencies
library(dplyr)
library(readxl)
library(data.table)
# Sourcing Prior Scripts
source('model/UW_R_Script_final.R')
source('model/coefficents.R')
source('model/lpsolverunited.R')

# DATA CLEANING BEFORE SHINY
# Original dataset
original = read_xlsx("2016 Original Data.xlsx")
names(original) = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultsnoedu','adultnohealth','unemployment')


# Overall Constraints
overall_constraints <- df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
# county_constraints <- df1 = as.data.frame(read.csv("data/county constrants.csv"))
overall_constraints[1:3,] = df2[1:3,] = round(overall_constraints[1:3,],.01)
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
                                      min_value = df2[1, input$variable]
                                      max_value = df2[2, input$variable]
                                      if(df2[3,input$variable] == overall_constraints[3,input$variable]){ #I am trying to intalize values here.
                                        values <- c(df2[3, input$variable], max_value) #not sure about this change
                                      } else {
                                        # min_value = overall_constraints[1, input$variable]
                                        # max_value = overall_constraints[2, input$variable]
                                        values <- c(overall_constraints[3, input$variable], max_value) #not sure about this change
                                      }
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
                                      
                                    })
  # Update Slider ----
  observeEvent(input$metric,{
    overall_constraints[3, input$variable] <<- input$metric[1]
    # overall_constraints[2, input$variable] <<- input$metric[2]
  })
  # Calc CWBI ----
  getCWBI <- eventReactive(input$metric,{
    # req(overall_constraints)
    req(original)
    final <- overall_constraints # this used to be test2 <- overall_constraints
    mycoef <- pop.Coef(original) # prep step from coefficents.R
    minCWB_Z <- min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
    maxCWB_Z <- max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
    final[1,input$variable] <- input$metric[1] # I wante to store metric changes to go to optimizer
    final[2,input$variable] <- input$metric[2]  # I wante to store metric changes to go to optimizer
    if(overall_constraints[3,input$variable] != input$metric[1] 
       && final["Mean",input$variable] != median(as.vector(input$metric))){
      final["Mean",input$variable] <- median(as.vector(input$metric)) #This is just a placeholder line for sending intial guess
    } else {
      final["Mean",input$variable] <- overall_constraints[3,input$variable]}
    # browser()
    #***We are optimizing this CWBZ
    # final2 <- final["Mean",] # input$final2 is a placeholder for optimizer)
    final2 <- lptest(final) #lptest takes in original_constrants or df2
    final2 <- final2[1:14]
    CWBZ <- rowSums(mycoef$A*t(final2) - mycoef$B)
    # CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z
    CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
    # CWBI <- median(input$gradrate)*(1+input$final/100)
    CWBI <- abs(CWBI)
    CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
    CWBI <- abs(CWBI)
    # browser()
    return(CWBI)
    
    # if(is.null(CWBI)){CWBI <- as.vector(58.9)} # useful for debugging
  })
  
  # Rest of Server -----
  
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
    # getCWBI (Load child well being)
    # AM Angular Gauge
    # browser()
    bands = data.frame(start = c(0,58.9), end = c(58.9, 100), 
                       color = c("#ea3838", "#00CC00"),
                       stringsAsFactors = FALSE)
    # browser()
    amAngularGauge(x = getCWBI(),
                   start = 0, end = 100,
                   main = "CWBI", bands = bands)}) 
  
  output$sample = renderText({ input$metric })
  
  output$GaugePlot7 = output$GaugePlot6 = output$GaugePlot5 = output$GaugePlot4 = output$GaugePlot3 = output$GaugePlot2 = output$GaugePlot = renderAmCharts({
    START = round(df2[1, input$variable],.1)
    value = round(df2[3, input$variable],.1)
    END = round(df2[2, input$variable],.1)
    # AM Angular Gauge
    # bands = data.frame(start = c(START,value), end = c(value, END), 
    #                    color = c("#00CC00", "#ea3838"),
    #                    stringsAsFactors = FALSE)
    
    #PURU_COMMENT_START
    # Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
    #PURU_COMMENT_END
    if((input$variable == 'gradrate') || (input$variable == 'ccrpi') || (input$variable == 'grade3') || (input$variable == 'grade8') || (input$variable == 'collegerate'))
    {
      bands <- data.frame(start = c(START,value), end = c(value, END), 
                          color = c("#ea3838","#00CC00"),
                          stringsAsFactors = FALSE)
    }
    else
    {  
      bands <- data.frame(start = c(START,value), end = c(value, END), 
                          color = c("#00CC00", "#ea3838"),
                          stringsAsFactors = FALSE)
    }
    amAngularGauge(x = median(as.vector(input$metric)), 
                   start = START, end = END,
                   main = input$variable, bands = bands)
  }) 
  
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
      tabsetPanel(tabPanel("Gauge Plots Here", amChartsOutput('GaugePlot'),
                           amChartsOutput('GaugePlot2'),
                           amChartsOutput('GaugePlot3')),
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
