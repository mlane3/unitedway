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
# library(flexdashboard)
library(leaflet)
library(rAmCharts)
# Data Processing Dependencies
library(dplyr)
library(readxl)
library(data.table)
library(raster)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(readxl)

# Sourcing Prior Scripts

# DATA CLEANING BEFORE SHINY ----
# # Original dataset
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
# Header ----
header = dashboardHeader(title = 'United Way App')

"*********************************************
                 SIDEBAR
*********************************************"
counties = unique(c("overall", original$county))

variables = names(overall_constraints)

#Side Bar ----
sidebar = dashboardSidebar(
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
                              switchInput(inputId = "calculate", value = FALSE)),
                    menuItemOutput('metric_slider')),

      # Eve Side Bar Menu ----
      sidebarMenu(
        menuItem(text = "Fixed Constraints",
                 title = "Example Button",
                 textInput( inputId = "plotbutton1",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton2",
                            label = "Enter Some Text: "),
                 
                 #Eve Added Wednesday night
                 textInput( inputId = "plotbutton3",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton4",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton5",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton6",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton7",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton8",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton9",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton10",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton11",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton12",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton13",
                            label = "Enter Some Text: "),
                 textInput( inputId = "plotbutton14",
                            label = "Enter Some Text: "),
                 #.... Add more ....
                 actionButton( inputId = "execute" ,
                               label = "Submit")
        )
      )
)



"*********************************************
                  BODY
*********************************************"

body = dashboardBody(uiOutput("MainGrid"))


"*********************************************
                 SERVER
*********************************************"
server = function(input, output)
{
  variablenamelist<-reactiveValues()
  # Eve Added  --------------------------------------------------------------
  user_text = observeEvent(input$execute,{
    variablenamelist <- data.table(
      variable = c( "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth",
                    "childpoverty", "povertyrate", "housingburden", "momsnohs", "collegerate",
                    "adultsnoedu", "adultnohealth", "unemployment" ),
      number = 1:14,
      title = c( "Graduation Rate", "College and Career Readiness Score", "%
childern exceed 3rd grade reading standard", "% childern exceed 8th grade math
standard", "Low Weight Births", "% children wo health insurance", "% childern
in poverty", "% families not financially stable", "% with housing cost
burden", "% of Moms with no high school", "% Adults in Post-Secondary
Education", "% Adults with no high school", "% adults wo health insurance",
                 "Unemployment Rate"),
      plotbutton = c(1,rep(0,13)))
    ##brower()

    #Eve Added on Saturday
    variablenamelist$plotbutton[1]<-input$plotbutton1
    variablenamelist$plotbutton[2]<-input$plotbutton2
    variablenamelist$plotbutton[3]<-input$plotbutton3
    variablenamelist$plotbutton[4]<-input$plotbutton4
    variablenamelist$plotbutton[5]<-input$plotbutton5
    variablenamelist$plotbutton[6]<-input$plotbutton6
    variablenamelist$plotbutton[7]<-input$plotbutton7
    variablenamelist$plotbutton[8]<-input$plotbutton8
    variablenamelist$plotbutton[9]<-input$plotbutton9
    variablenamelist$plotbutton[10]<-input$plotbutton10
    variablenamelist$plotbutton[11]<-input$plotbutton11
    variablenamelist$plotbutton[12]<-input$plotbutton12
    variablenamelist$plotbutton[13]<-input$plotbutton13
    variablenamelist$plotbutton[14]<-input$plotbutton14
    
    actionButton(inputId = "execute", label = "Submit")
    
    return (variablenamelist)
  })
  
# Select County  ----

variable_reactive = eventReactive(input$variable, 
{
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
  #  #debug code
    
  if (length(input$variable == 1) )
      sidebarMenu( menuItem( text = "Metric Slider",
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
# update <- eventReactive(input$metric,{
#   overall_constraints[3, input$variable] <<- input$metric[1]
#   return(overall_constraints)
#   # overall_constraints[2, input$variable] <<- input$metric[2]
# })

myupdate <- observeEvent(input$metric,{
  # overall_constraints[3, input$variable] <<- input$metric[1]
  overall_constraints[1, input$variable] <<- input$metric[1]
  overall_constraints[2, input$variable] <<- input$metric[2]
  update <- overall_constraints
  return(update)
})
# LPSolver Calc CWBI ----
getCWBI <- eventReactive(input$metric,{
  req(overall_constraints)
  req(original)

  # update <- myupdate()
  final <- overall_constraints # update # this used to be final <- overall_constraints
  mycoef <- pop.Coef(original) # prep step from coefficents.R
  minCWB_Z <- min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
  maxCWB_Z <- max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
  # final[1,input$variable] <- final$metric[1] # I wante to store metric changes to go to optimizer
  # final[2,input$variable] <- input$metric[2]  # I wante to store metric changes to go to optimizer
  
  # if(overall_constraints[3,input$variable] != input$metric[1] 
  #    && final["Mean",input$variable] != median(as.vector(input$metric))){
  #   final["Mean",input$variable] <- median(as.vector(input$metric)) #This is just a placeholder line for sending intial guess
  # } else {
  #   final["Mean",input$variable] <- overall_constraints[3,input$variable]}
  #***We are optimizing this CWBZ
  # final2 <- final["Mean",] # input$final2 is a placeholder for optimizer)
 #EVE add code
  x <- usertext()
  
   final2 <- lptest(final) #lptest takes in original_constrants or df2
  final2 <- final2[1:14]
  names(final2) = variables
  CWBZ <- rowSums(mycoef$A*t(final2) - mycoef$B)
  # CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  # CWBI <- median(input$gradrate)*(1+input$final/100)
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  CWBI <- abs(CWBI)
  names(CWBI) = "CWBI"

  final3 <- final3 <- c(CWBI,final2,use.names=TRUE)
  # #brower()
  return(final3)
  print("done")
  # if(is.null(CWBI)){CWBI <- as.vector(58.9)} # useful for debugging
  
})

# Plotting -----

# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )


# PLOTTING THE GAUGE
output$GaugeCWBI = renderAmCharts({

  final <- getCWBI() #(Load child well being)
  # if(is.null(final)){final <- as.vector(58.9)} # useful for debugging
  value = unname(unlist(final[1]))
  # AM Angular Gauge
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100), 
                     color = c("#ea3838", "#00CC00"),
                     stringsAsFactors = FALSE)
  amAngularGauge(x = value,
                 start = 0, end = 100,
                 main = "CWBI", bands = bands)}) 

output$sample = renderText({ input$metric })
output$GaugePlot = renderAmCharts({
  START = round(df2[1, input$variable],.1)
  value = round(df2[4, input$variable],.1)
  END = round(df2[2, input$variable],.1)
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
    amAngularGauge(x = median(as.vector(input$metric)), 
                   start = START, end = END,
                   main = input$variable, bands = bands) 
  }
})
output$GaugePlot1 = renderAmCharts({
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "gradrate"],.1)
  value = round(df2[4, "gradrate"],.1)
  END = round(df2[2, "gradrate"],.1)
  DIAL = round(unname(unlist(final["gradrate"]))) # overall_constraints[3, "gradrate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  # #brower()
  
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "ccrpi"],.1)
  value = round(df2[4, "ccrpi"],.1)
  END = round(df2[2, "ccrpi"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "grade3"],.1)
  value = round(df2[4, "grade3"],.1)
  END = round(df2[2, "grade3"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "grade8"],.1)
  value = round(df2[4, "grade8"],.1)
  END = round(df2[2, "grade8"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "lbw"],.1)
  value = round(df2[4, "lbw"],.1)
  END = round(df2[2, "lbw"],.1)
  DIAL = round(unname(unlist(final["lbw"]),.01)) # overall_constraints[3, "lbw"]
  # #brower()
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "childnohealth"],.1)
  value = round(df2[4, "childnohealth"],.1)
  END = round(df2[2, "childnohealth"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "childpoverty"],.1)
  value = round(df2[4, "childpoverty"],.1)
  END = round(df2[2, "childpoverty"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "povertyrate"],.1)
  value = round(df2[4, "povertyrate"],.1)
  END = round(df2[2, "povertyrate"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "housingburden"],.1)
  value = round(df2[4, "housingburden"],.1)
  END = round(df2[2, "housingburden"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "momsnohs"],.1)
  value = round(df2[4, "momsnohs"],.1)
  END = round(df2[2, "momsnohs"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "collegerate"],.1)
  value = round(df2[4, "collegerate"],.1)
  END = round(df2[2, "collegerate"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "adultsnoedu"],.1)
  value = round(df2[4, "adultsnoedu"],.1)
  END = round(df2[2, "adultsnoedu"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "adultnohealth"],.1)
  value = round(df2[4, "adultnohealth"],.1)
  END = round(df2[2, "adultnohealth"],.1)
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
  final = getCWBI() #(Load child well being)
  START = round(df2[1, "unemployment"],.1)
  value = round(df2[4, "unemployment"],.1)
  END = round(df2[2, "unemployment"],.1)
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
output$mymap = renderLeaflet({
  counties <- shapefile("data/United Way Tracts (TIGER 2010).shp")
  UWcensuszip <- read_excel("data/TRACT_ZIP_122014.xlsx")
  uwmapdata<-read_excel("Complete index table w trunct tract.xlsx")
  names(df0) <- c('county','TRACT','gradrate','ccrpi',
                  'grade3','grade8','lbw','childnohealth',
                  'childpoverty','povertyrate','housingburden','momsnohs',
                  'collegerate','adultsnoedu','adultnohealth','unemployment')
  
  # merge two data frames by ID
  # dfzipmap <- merge(original,UWcensuszip,by="TRACT")
  original$trunctract<-uwmapdata$Tract
  
  original <- original[order(match(original$trunctract, counties$TRACTCE10)),]
  mycolor <- as.numeric(unlist(original[, input$variable]))
  bins <- c(0, .10*max(mycolor), .20*max(mycolor), .30*max(mycolor), 
            .40*max(mycolor), .50*max(mycolor), .60*max(mycolor), .70*max(mycolor), Inf)
  # bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
  pal <- colorBin("RdYlBu", domain = mycolor, bins = bins)
  #add our color pallete to our map
  # mycolor <- dff0$trunctract
  # mycolor <- as.numeric(paste(original$trunctract))
  
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
      if (is.null(input$county))
      {
        p("Welcome to United Way App", br,
          "Please Select a county to begin")
      } else {
        tabsetPanel(tabPanel(
            "all_plots",
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
                    tabPanel("Additional Content here", amChartsOutput('GaugeCWBI')),
                    tabPanel("Gauge Plots Here", amChartsOutput('GaugePlot'),
                             amChartsOutput('GaugePlot2'),
                             amChartsOutput('GaugePlot3')
                             ),
          tabPanel("I'm A MAP!",leafletOutput("mymap"))
          
          )        
        # textOutput('sample')
        
      }
        
})


}

"*********************************************
                 RUNAPP
# *********************************************"
# Runapp ----
# # display.mode="showcase" #debug code
# # options(shiny.reactlog=TRUE) #debug code
app <- shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body),
          server = server)
