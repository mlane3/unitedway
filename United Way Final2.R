"*********************************************************

            UNITED WAY FINAL APPLICATION
                Date: March 6th 2018

*********************************************************"
# Authors: Eva, Betty Toyin, and Michael.
# Eva below I have done this # followed by "----" notation.  These should help you go
# through the file quickly.  It is separeted like it is with Sifael's stuff

# NECESSARY PACKAGES ----
#this simple script installs packages
packages = c("shiny","lpSolve","lpSolveAPI","shinydashboard","ggplot2","plotly","leaflet",
             "rAmCharts","dplyr","readxl","data.table","shinyWidgets","ggmap","rgdal","mapview")
lapply(packages, FUN = function(x){if(x %in% rownames(installed.packages())==FALSE){install.packages(x,dependencies = TRUE)}});
rm(packages)

# Shiny Dependencies
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# Plotting Dependencies
library(ggplot2)
library(plotly)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(RColorBrewer)
library(sp)
library(raster)
# library(flexdashboard)
library(leaflet)
library(rAmCharts)
# Data Processing Dependencies
library(dplyr)
library(readxl)
library(data.table)
library(readxl)

# Sourcing Prior Scripts
source('model/UWCWBI_final.R')
source('model/lpsolverunited.R')
source("variablenameslist.R")

# DATA CLEANING BEFORE SHINY ----
# # Original dataset
original <- read_xlsx("2016 Original Data.xlsx")
names(original) = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultsnoedu','adultnohealth','unemployment')
# Overall Constraints
overall_constraints <- reactiveValues()
overall_constraints <- df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
# county_constraints <- df1 = as.data.frame(read.csv("data/county constrants.csv"))
overall_constraints[1:3,] = round(overall_constraints[1:3,],.01) # = df2[1:3,]
#full_names = as.data.frame(read.csv("data/overall constrants.csv", nrows = 3, row.names = 1)

"*********************************************
                  HEADER
*********************************************"
# Header ----
header = dashboardHeader(title = 'United Way App')

"*********************************************
                 SIDEBAR
*********************************************"
counties = unique(c("overall", original$county))
variables = names(overall_constraints)
colors <- c("Quantile","RdGy",
"RdYlBu", "RdYlGn", "Spectral")

#Side Bar ----
sidebar = dashboardSidebar(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      sidebarMenu(  menuItem( text = "Multiple Plots",
                            tabName = "all_plots" )),
      sidebarMenu( 
                    menuItem( text = "County Selection",
                              icon = icon('th'),
                              selectInput(inputId = 'county',
                                          label = 'Select County:',
                                          choices = counties, selected = counties[1]))),
      sidebarMenu( 
                    menuItem( text = "Variable Selection",
                              icon = icon('th'),
                              selectInput(inputId = 'variable',
                                          label = 'Select a Variable:',
                                          choices = variables, selected = variables[1]),
                              p("Selected:"),
                              tags$div(class="header", checked=NA,
                                       tags$p(textOutput(outputId = "sample2"))
                              ),
                              switchInput(label = 'Start Optimization?',inputId = "calculate", value = FALSE),
                              p("Map Controls:"),
                              switchInput(label = 'Display CWBI',inputId = "mapcwbi",value = FALSE),
                              selectInput(inputId="mapcolor",label="Pick a map Color",
                                          choices = colors, selected = "RdYlBu" )
                              ),
                    menuItemOutput('metric_slider'))
      

)

"*********************************************
                  BODY
*********************************************"

body = dashboardBody(
  
w
  uiOutput("MainGrid"))


"*********************************************
                 SERVER
*********************************************"
# Server ----
server = function(input, output){

# Select County  ----
variablenamelist <- as.data.frame(variablenamelist)
row.names(variablenamelist) <- unlist(variablenamelist[,1])
output$sample2 = output$sample = renderText({variablenamelist[input$variable,3]})

rv <- reactiveValues()
rv$run2 <- 0
rv$run3 <- 0
rv$run4 <- 0 
rv$run5 <- 0
rv$run1 <- 0
#overall_constraints <- df2$ reactiveValues(overall_constraints)

# Reactive Input ----

variable_reactive = eventReactive(input$variable, 
{
  rv$run2 <- rv$run2 + 1
  min_value = df2[1, input$variable]
  max_value = df2[2, input$variable]
  if(df2[3,input$variable] == overall_constraints[3,input$variable]){ #I am trying to intalize values here.
    # values <- c(min_value, df2[3, input$variable])
    values <- c(df2[4, input$variable], max_value) #not sure about this change
  } else {
    # min_value = overall_constraints[1, input$variable]
    # max_value = overall_constraints[2, input$variable]
    values <- c(overall_constraints[3, input$variable], max_value) #not sure about this change
  }
    
  if (length(input$variable == 1) )
      sidebarMenu( menuItem( text = "Metric Slider",
                              icon = icon('th'),
                              tags$div(class="header", checked=NA,
                                      tags$head("Adjust the boundary conditions of:"),
                                      tags$p(textOutput(outputId = "sample"))
                              ),
                              
                              sliderInput( inputId = "metric",
                                           label = input$variable,
                                           min = min_value,
                                           max = max_value,
                                           value = values,
                                           sep ="",
                                           step = .1),
                             sliderInput("gradrate",paste(variablenamelist[1,3]),
                                         min = df2$gradrate[1],max = df2$gradrate[2],
                                         value = c(df2$gradrate[3],df2$gradrate[2]), #for the upper range
                                         sep ="",step = .01, ticks = FALSE),
                             sliderInput("ccrpi",paste(variablenamelist[2,3]),
                                         min = df2$ccrpi[1],max = df2$ccrpi[2],
                                         value = c(df2$ccrpi[3],df2$ccrpi[2]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("grade3",paste(variablenamelist[3,3]),
                                         min = df2$grade3[1],max = df2$grade3[2],
                                         value = c(df2$grade3[3],df2$grade3[2]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("grade8",paste(variablenamelist[4,3]),
                                         min = df2$grade8[1],max = df2$grade8[2],
                                         value = c(df2$grade8[3],df2$grade8[2]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("lbw",paste(variablenamelist[5,3]),
                                         min = df2$lbw[1],max = df2$lbw[2],
                                         value = c(df2$lbw[1],df2$lbw[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("childnohealth",paste(variablenamelist[6,3]),
                                         min = df2$childnohealth[1],max = df2$childnohealth[2],
                                         value = c(df2$childnohealth[1],df2$childnohealth[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("childpoverty",paste(variablenamelist[7,3]),
                                         min = df2$childpoverty[1],max = df2$childpoverty[2],
                                         value = c(df2$childpoverty[1],df2$childpoverty[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("povertyrate",paste(variablenamelist[8,3]),
                                         min = df2$povertyrate[1],max = df2$povertyrate[2],
                                         value = c(df2$povertyrate[1],df2$povertyrate[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("housingburden",paste(variablenamelist[9,3]),
                                         min = df2$housingburden[1],max = df2$housingburden[2],
                                         value = c(df2$housingburden[1],df2$housingburden[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("momsnohs",paste(variablenamelist[10,3]),
                                         min = df2$momsnohs[1],max = df2$momsnohs[2],
                                         value = c(df2$momsnohs[1],df2$momsnohs[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("collegerate",paste(variablenamelist[11,3]),
                                         min = df2$collegerate[1],max = df2$collegerate[2],
                                         value = c(df2$collegerate[3],df2$collegerate[2]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("adultsnoedu",paste(variablenamelist[12,3]),
                                         min = df2$adultsnoedu[1],max = df2$adultsnoedu[2],
                                         value = c(df2$adultsnoedu[1],df2$adultsnoedu[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("adultnohealth",paste(variablenamelist[1,3]),
                                         min = df2$adultnohealth[1],max = df2$adultnohealth[2],
                                         value = c(df2$adultnohealth[1],df2$adultnohealth[3]), sep ="",step = .01, ticks = FALSE),
                             sliderInput("unemployment",paste(variablenamelist[13,3]),
                                         min = df2$unemployment[1],max = df2$unemployment[2],
                                         value = c(df2$unemployment[1],df2$unemployment[3]), sep ="",step = .01, ticks = FALSE)
                              
        )
      )
})

# Update Slider ----
# update <- eventReactive(input$metric,{
#   overall_constraints[3, input$variable] <<- input$metric[1]
#   return(overall_constraints)
#   # overall_constraints[2, input$variable] <<- input$metric[2]
# })
myupdate <- observeEvent(c(input$gradrate,input$ccrpi),{
  print("running")
  #req(rv$run2 != 0)
  req(overall_constraints,input$gradrate)
  #overall_constraints[1, input$variable] <<- input$metric[1]
  #overall_constraints[2, input$variable] <<- input$metric[2]
  overall_constraints[1, "gradrate"] <- input$gradrate[1]
  overall_constraints[1, "ccrpi"] <- input$ccrpi[1]
  overall_constraints[1, "grade3"] <- input$grade3[1]
  overall_constraints[1, "grade8" ] <- input$grade8[1]
  overall_constraints[1, "lbw"] <- input$lbw[1]
  overall_constraints[1, "childnohealth"] <- input$childnohealth[1]
  overall_constraints[1, "childpoverty"] <- input$childpoverty[1]
  overall_constraints[1, "povertyrate"] <- input$povertyrate[1]
  overall_constraints[1, "housingburden"] <- input$housingburden[1]
  overall_constraints[1, "momsnohs" ] <- input$momsnohs[1]
  overall_constraints[1,  "collegerate" ] <- input$collegerate[1]
  overall_constraints[1, "adultsnoedu"]  <- input$adultsnoedu[1]
  overall_constraints[1, "adultnohealth"] <- input$adultnohealth[1]
  overall_constraints[1,  "unemployment"] <- input$unemployment[1]
  overall_constraints[2, "gradrate"] <- input$gradrate[2]
  overall_constraints[2, "ccrpi"] <- input$ccrpi[2]
  overall_constraints[2, "grade3"] <- input$grade3[2]
  overall_constraints[2, "grade8" ] <- input$grade8[2]
  overall_constraints[2, "lbw"] <- input$lbw[2]
  overall_constraints[2, "childnohealth"] <- input$childnohealth[2]
  overall_constraints[2, "childpoverty"] <- input$childpoverty[2]
  overall_constraints[2, "povertyrate"] <- input$povertyrate[2]
  overall_constraints[2, "housingburden"] <- input$housingburden[2]
  overall_constraints[2, "momsnohs" ] <- input$momsnohs[2]
  overall_constraints[2,  "collegerate" ] <- input$collegerate[2]
  overall_constraints[2, "adultsnoedu"]  <- input$adultsnoedu[2]
  overall_constraints[2, "adultnohealth"] <- input$adultnohealth[2]
  overall_constraints[2,  "unemployment"] <- input$unemployment[2]
  # assign("overall_constraints", overall_constraints, envir=globalenv())
  overall_constraints <<- overall_constraints
  saveRDS(overall_constraints,"Atemporaryfile.Rds")
  rv$run3 <- rv$run3 + 1
})

# LPSolver Calc CWBI ----
final <- reactiveValues()
getoriginalvalues <- eventReactive(rv$run3,{
  req(overall_constraints,original)
  final <- overall_constraints # updated
  final <- readRDS("Atemporaryfile.Rds")
  ## Mike FIX THIS ----
  mycoef <- pop.Coef(original) # prep step from coefficents.R
  minCWB_Z <- -1.969282 #prep step from coefficents.R
  maxCWB_Z <- 1.380706 #prep step from coefficents.R
  if(rv$run3 <= 1){
    final["df0_ave  ",input$variable] <- median(as.vector(final[1:2,input$variable]))
  } else {
    final["df0_ave",input$variable] <- overall_constraints[4,input$variable]}
  # 
  final2 <- final["df0_ave",] 
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  # CWBI <- median(input$gradrate)*(1+input$final/100)
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  final2 <- colMeans(final[1:2,])
  final3 <- c(CWBI,final2,use.names=TRUE)
  return(final3)
},ignoreNULL = FALSE)
getCWBI <- eventReactive(input$metric,{
  req(overall_constraints)
  req(original)
  # if(is.null(input$metric)==TRUE){final <- overall_constraints
  # }else{ updated = overall_constraints
  # final = updated # this used to be final <- overall_constraints
  # }
  # if(rv$run3 <= 1){
  #   final["df0_ave  ",input$variable] <- median(as.vector(final[1:2,input$variable]))
  # } else {
  #   final["df0_ave",input$variable] <- overall_constraints[4,input$variable]}
  final <- overall_constraints # updated
  if(exists("Atemporaryfile.Rds")){final <- readRDS("Atemporaryfile.Rds")}
  mycoef <- pop.Coef(original) # prep step from coefficents.R
  minCWB_Z <- min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
  maxCWB_Z <- max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R

  #***We are optimizing this CWBZ
  # final2 <- final["Mean",] # input$final2 is a placeholder for optimizer)
  final2 <- lptest(final) #lptest takes in overall_constraints or df2
  final2 <- final2[1:14]
  names(final2) = variables
  CWBZ <- rowSums(mycoef$A*t(final2) - mycoef$B)
  # CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  # CWBI <- median(input$gradrate)*(1+input$final/100)
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  CWBI <- abs(CWBI)
  names(CWBI) = "CWBI"
  final3 <- c(CWBI,final2,use.names=TRUE)
  return(final3)
  print("done")
  # if(is.null(CWBI)){CWBI <- as.vector(58.9)} # useful for debugging
  
},ignoreNULL = FALSE)

# Plotting -----
# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )
#output$sample <- renderText({input$gradrate})
#myexample = observeEvent(input$gradrate,{
  output$sample3 = renderText({paste(rv3$run3)})
#})


# THE SWITCH ----

#Google if there is a better way
#final <- reactive
switch <- eventReactive(c(rv$run3,input$calculate,input$gradrate),{
  
  #if(rv$run2 != 0)
  req(overall_constraints,rv$run2 != 0)
  updated <- overall_constraints
  updated <- read.csv("Atemporaryfile.csv")
  if(input$calculate==TRUE){
    value<-getCWBI()
  }else{value<-getoriginalvalues()}
  return(value)
},ignoreNULL = FALSE,ignoreInit = FALSE)
#
observe(switch())
# PLOTTING THE GAUGES ----
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
output$GaugePlot = renderAmCharts({
  START = 0
  value = round(df2[4, input$variable],.1)
  END = 100
  DIAL = overall_constraints[3, input$variable]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
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
  if(is.null(input$metric) == TRUE){
    amAngularGauge(x = START,
                   start = START, end = END,
                   main = input$variable, bands = bands)
  } else{
    amAngularGauge(x = round(median(as.vector(input$metric)),.1),
                   start = START, end = END,
                   main = input$variable, bands = bands)
  }
})
output$GaugePlot1 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "gradrate"],.1)
  END = 100
  DIAL = round(unname(unlist(final["gradrate"]))) # overall_constraints[3, "gradrate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  #browser()
  print(rv$run3)
  if(('gradrate' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Grad Rate", bands = bands)
})
output$GaugePlot2 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "ccrpi"],.1)
  END = 100
  DIAL = round(unname(unlist(final["ccrpi"]))) # overall_constraints[3, "ccrpi"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is ccrpi or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('ccrpi' == 'ccrpi'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "CCRPI", bands = bands)
})
output$GaugePlot3 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "grade3"],.1)
  END = 100
  DIAL = round(unname(unlist(final["grade3"]))) # overall_constraints[3, "grade3"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is grade3 or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('grade3' == 'grade3'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Grade 3 Test Scores", bands = bands)
})
output$GaugePlot4 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "grade8"],.1)
  END = 100
  DIAL = round(unname(unlist(final["grade8"]))) # overall_constraints[3, "grade3"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('grade8' == 'grade8'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "grade8", bands = bands)
})
output$GaugePlot5 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "lbw"],.1)
  END = 100
  DIAL = round(unname(unlist(final["lbw"]),.01)) # overall_constraints[3, "lbw"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('lbw' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Low Birth Weight", bands = bands)
})
output$GaugePlot6 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "childnohealth"],.1)
  END = 100
  DIAL = round(unname(unlist(final["childnohealth"]))) # overall_constraints[3, "childnohealth"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('childnohealth' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Childern No Health Ins", bands = bands)
})
output$GaugePlot7 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "childpoverty"],.1)
  END = 100
  DIAL = round(unname(unlist(final["childpoverty"]))) # overall_constraints[3, "childpoverty"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('childpoverty' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Child Poverty", bands = bands)
})
output$GaugePlot8 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "povertyrate"],.1)
  END = 100
  DIAL = round(unname(unlist(final["povertyrate"]))) # overall_constraints[3, "povertyrate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('povertyrate' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Family Poverty", bands = bands)
})
output$GaugePlot9 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "housingburden"],.1)
  END = 100
  DIAL = round(unname(unlist(final["housingburden"]))) # overall_constraints[3, "housingburden"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('housingburden' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Housing Burden", bands = bands)
})
output$GaugePlot10 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "momsnohs"],.1)
  END = 100
  DIAL = round(unname(unlist(final["momsnohs"]))) # overall_constraints[3, "momsnohs"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('momsnohs' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Moms No High School", bands = bands)
})
output$GaugePlot11 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "collegerate"],.1)
  END = 100
  DIAL = round(unname(unlist(final["collegerate"]))) # overall_constraints[3, "collegerate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('collegerate' == 'collegerate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "College Rate", bands = bands)
})
output$GaugePlot12 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "adultsnoedu"],.1)
  END = 100
  DIAL = round(unname(unlist(final["adultsnoedu"]))) # overall_constraints[3, "adultsnoedu"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('adultsnoedu' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Adults No HS", bands = bands)
})
output$GaugePlot13 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "adultnohealth"],.1)
  END = 100
  DIAL = round(unname(unlist(final["adultnohealth"]))) # overall_constraints[3, "adultnohealth"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('adultnohealth' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Adults no Health", bands = bands)
})
output$GaugePlot14 = renderAmCharts({
  final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "unemployment"],.1)
  END = 100
  DIAL = round(unname(unlist(final["unemployment"]))) # overall_constraints[3, "unemployment"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('unemployment' == 'gradrate'))
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
  amAngularGauge(x = DIAL,
                 start = START, end = END,
                 main = "Unemployment", bands = bands)
})
# Plotting the Map ----
output$mymap = renderLeaflet({
  counties <- shapefile("data/United Way Tracts (TIGER 2010).shp")
  #UWcensuszip <- read_excel("data/TRACT_ZIP_122014.xlsx")
  uwmapdata<-read_excel("Complete index table w trunct tract.xlsx")
  names(original) <- c('county','TRACT','gradrate','ccrpi',
                  'grade3','grade8','lbw','childnohealth',
                  'childpoverty','povertyrate','housingburden','momsnohs',
                  'collegerate','adultsnoedu','adultnohealth','unemployment')
  
  # merge two data frames by ID
  # dfzipmap <- merge(original,UWcensuszip,by="TRACT")
  original$trunctract<-uwmapdata$Tract
  original <- original[order(match(original$trunctract, counties$TRACTCE10)),]
  mycolor <- as.numeric(unlist(original[, input$variable]))
  if(length(input$mapcwbi)==1){if(input$mapcwbi == TRUE){
    df_complete <- df_complete[order(match(df_complete$weave_ct2010,original$TRACT))]
    mycolor<- as.numeric(df_complete$CWB_Index)
  }}
  #If value positively impacts CWBI then don't reverse else reverse the color scale.
  # if((input$variable == 'gradrate') || (input$variable == 'ccrpi') || (input$variable == 'grade3') || (input$variable == 'grade8') || (input$variable == 'collegerate')){
  #   reverse = FALSE}else{reverse=TRUE}
  reverse = FALSE
  #Add our color pallete to our map
  bins <- c(0, .10*max(mycolor), .20*max(mycolor), .30*max(mycolor), 
            .40*max(mycolor), .50*max(mycolor), .60*max(mycolor), .70*max(mycolor), Inf)
  
  if(input$mapcolor == "Quantile"){pal <- colorQuantile("RdYlGn", domain = mycolor, n=5)
  }else{
    pal <- colorBin(input$mapcolor, domain = mycolor, bins = bins,reverse = reverse)}
  
  # mycolor <- dff0$trunctract
  # mycolor <- as.numeric(paste(original$trunctract))
  if(input$calculate == TRUE){value = getCWBI() #allows the switch to control map
  mycolor <- as.numeric(value[input$variable])}
  if(length(input$mapcwbi)==1){if(input$mapcwbi == TRUE && input$calculate == TRUE){value = getCWBI()
  mycolor <- as.numeric(68.9)
  }}
  # browser()
  #Plot the map
  leaflet() %>%
    setView(lng = -84.386330, lat = 33.753746, zoom = 8) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = counties,
                fillColor = pal(mycolor),
                weight = 1, 
                smoothFactor = 0.5,
                color = "white",
                fillOpacity = 0.8)
})



# output$test = renderTable(append(input$metric))

"*******************************
          MAIN GRID
*******************************"
# The Actual Body or "Main Grid"----
output$MainGrid = renderUI({
      # Evaluating the Overall Page
      if (is.null(input$metric)==TRUE)
      {
        p("Welcome to United Way App", br(),
          "Please Select a variable to begin.  Any variable will do")
      } else {
        #tabsetPanel(tabPanel("Additional Content here",verbatimTextOutput('sample3')))
        tabsetPanel(
         tabPanel("Additional Content here",p("These is the debug page to show raw output for us"),verbatimTextOutput('sample')),
         tabPanel(
            "all_plots",p("KNOWN BUG: To Show the other gauges, select and change the variable to grade3, then you have to turn Optimize ON THEN OFF.  We are fixing it."),
            p("This is the first version we could show to figure out how Variabes affect child well being index. Before you make a selection the other besides the first on show what the child well being was."),
            p("TO START: Select and Change the variable to begin.  Then you can change the metric slider range.  The left most gauge shows what you have currently changed.  The optimization is based on the (slider) boundaries you set with the Metric Slider for each variable"),
            fluidRow( column(4,
                             box(width=12, amChartsOutput("GaugePlot",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot1",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot2",height="200")))),
            fluidRow( column(4,
                             box(width=12, amChartsOutput("GaugePlot3",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot4",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot5",height="200")))),
            fluidRow( column(4,
                             box(width=12, amChartsOutput("GaugePlot6",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot7",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot8",height="200")))),
            fluidRow( column(4,
                             box(width=12, amChartsOutput("GaugePlot9",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot10",height="200"))),
                      column(4,
                             box(width=12, amChartsOutput("GaugePlot11",height="200")))),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot12",height="200"))),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot13",height="200"))),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot14",height="200"))))#,
          ),
                    tabPanel("CWBI Gauge", p("For the first version and a few of the other versions, the gauge CWBI is in a seperate tab.  We could put them in the same tab but it might be too much info."),
                                             amChartsOutput('GaugeCWBI')),
        tabPanel("the map is here",fluidPage(leafletOutput("mymap")))
        )

      }
        
})


}

"*********************************************
                 RUNAPP
# *********************************************"
# Runapp ----
# options(shiny.error = NULL)
options(shiny.error = NULL)
options(shiny.reactlog=TRUE) 
options(shiny.sanitize.errors = FALSE)
# display.mode="showcase" #debug code
# options(shiny.reactlog=TRUE) #debug code
app <- shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body),
          server = server)
