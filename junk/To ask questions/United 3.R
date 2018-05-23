"*********************************************************

           UW Reproduciable Example Version 4 Mark 5

*********************************************************"

# NECESSARY PACKAGES
packages = c("shiny","lpSolve","lpSolveAPI","shinydashboard","ggplot2","plotly","leaflet",
             "rAmCharts","dplyr","readxl","data.table","shinyWidgets","ggmap","rgdal","mapview")
lapply(packages, FUN = function(x){if(x %in% rownames(installed.packages())==FALSE){install.packages(x,dependencies = TRUE)}});
rm(packages)
# Shiny Dependencies
library(shiny)
library(shinydashboard)
library(htmltools)
# Plotting Dependencies
library(ggplot2)
library(rAmCharts)
library(leaflet)
# Data Processing Dependencies
library(dplyr)
library(data.table)

# DATA CLEANING BEFORE SHINY ----
# Original dataset
df2 <- data.frame(Min = c(37.60854752,37.37701388,0.1,0.1,5.3,4.9,5.9,0.1,4.798530355,0.1,42.06143662,0.1,12.6,3.7),
                  Max = c(110.3940298,102.2361305,110.7274779,91.55844297,12.7,14.6,41,93.67152116,72.16193614,50.57695033,108.0549693,42.41675112,36.3,13.5),
                  Mean = c(76.97083248,70.67638458,47.05573221,32.682569,8.822164413,9.86521465,20.18908765,28.47120279,35.98333562,13.66541189,73.53394391,13.34574153,21.09052071,11.66727383),
                  df0_ave = c(74.00128866,69.80657216,46.03772629,33.23891753,9.273624098,10.90399884,24.06608544,30.84305541,38.48023325,13.8957549,75.05820296,12.27670284,23.39983698,12.22409549),
                  Deviation = c(11.73959392,10.46114783,20.86766181,18.81275014,2.800196126,8.375752635,19.90408851,20.26724702,10.86506545,11.83264369,10.64411817,9.722596219,13.9247831,6.665234405))
variables <- rownames(df2) <- mynames <- c('gradrate','ccrpi',
                   'grade3','grade8','lbw','childnohealth',
                   'childpoverty','povertyrate','housingburden','momsnohs',
                   'collegerate','adultsnoedu','adultnohealth','unemployment')
overall_constraints<- df2 <- data.frame(t(df2))
counties = unique(... )
pop.Coef <- function(){... }
library(lpSolve)
library(lpSolveAPI)
lptest <- function(df2){...}
# Header ----
header = dashboardHeader(title = 'United Way App')

# Sidebar ----
sidebar = dashboardSidebar(... )

# Body ----
body = dashboardBody(uiOutput("MainGrid"))
# Server ----
server = function(input, output){
# Metric Slider Reactive ----
variable_reactive = eventReactive(input$variable, 
{
  min_value = df2[1, input$variable]
  max_value = df2[2, input$variable]
  if(df2[3,input$variable] == overall_constraints[3,input$variable]){
    values <- c(df2[3, input$variable], max_value)
  } else {
    values <- c(overall_constraints[3, input$variable], max_value)
  }
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
# Update Reactive ----
myupdate <- eventReactive(input$metric,{
  overall_constraints[1, input$variable] <<- input$metric[1]
  overall_constraints[2, input$variable] <<- input$metric[2]
  updated <<- overall_constraints <<- overall_constraints
  return(overall_constraints)
})
switch <- eventReactive(input$calculate,{
  overall_constraints <<- myupdate()
  req(overall_constraints)
  if(input$calculate==TRUE){
    value<-getCWBI()
  }else{value<-getoriginalvalues()}
  return(value)
},ignoreNULL = FALSE,ignoreInit = FALSE)
# Other Reactives ----
getoriginalvalues <- eventReactive(input$metric,{.... },ignoreNULL = FALSE)
getCWBI <- eventReactive(input$metric,{.... },ignoreNULL = FALSE)

# Rest of Server -----

# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )

# PLOTTING THE GAUGE
output$GaugeCWBI = renderAmCharts({
  final <- switch() #(Load child well being)
  # if(is.null(final)){final <- as.vector(58.9)} # useful for debugging
  value = unname(unlist(final[1]))
  # AM Angular Gauge
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100), 
                     color = c("#ea3838", "#00CC00"),
                     stringsAsFactors = FALSE)
  
  amAngularGauge(x = value,
                 start = 0, end = 100,
                 main = "CWBI", bands = bands)}) 
output$GaugePlot1 = renderAmCharts({.... }) 
output$GaugePlot2 = renderAmCharts({.... })
output$GaugePlot = renderAmCharts({.... })
# Main Grid ----
output$MainGrid = renderUI({
    tabsetPanel(
    tabPanel("all_plots", amChartsOutput('GaugePlot'),
             amChartsOutput('GaugePlot1'),
             amChartsOutput('GaugePlot2'), amChartsOutput('GaugeCWBI')))
  
})}
# Run app ---- 
# options(shiny.reactlog=TRUE) #debug code
app <- shinyApp(ui = dashboardPage(skin = 'blue',header = header,
                 sidebar = sidebar,body = body), server = server)
