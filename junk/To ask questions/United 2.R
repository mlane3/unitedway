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
library(rAmCharts)

# Data Processing Dependencies
library(dplyr)
library(data.table)


# DATA CLEANING BEFORE SHINY ----
# Original dataset
source("lpsolver.R")
load("data to load in.RData", envir = parent.frame(), verbose = TRUE)
mynames = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultsnoedu','adultnohealth','unemployment')
pop.Coef <- function(){
  # library(data.table)
  #Calculate df0_average
  df0_ave = c(74.00129, 69.80657, 46.03773, 33.23892, 9.273624, 10.904, 24.06609,
              30.84306, 38.48023, 13.89575, 75.0582, 12.2767, 23.39984, 12.2241)
  names(df0_ave) = mynames[1:14]
  #the df0_average (weighted by track/row/county)
  #Calculate STDEV
  df0_sd = c(11.73959, 10.46115, 20.86766, 18.81275, 2.800196, 8.375753, 19.90409,
             20.26725, 10.86507, 11.83264, 10.64412, 9.722596, 13.92478, 6.665234)
  names(df0_sd) = mynames[1:14]
  # Define the Coefficent and Intercept B for each variable
  ChildA <- function(df0_sd){return(1/(3*7*df0_sd))} #Child Coefficents
  FamilyA <- function(df0_sd){ return(1/(3*3*df0_sd)) } #Family Coefficents
  ComA <- function(df0_sd){ return(1/(3*4*df0_sd)) } #Community Coefficents
  
  ChildB <- function(df0_ave,df0_sd){ return(df0_ave/(3*7*df0_sd)) } #Child Intercepts
  FamilyB <- function(df0_ave,df0_sd){ return(df0_ave/(3*3*df0_sd)) } #Family Intercepts
  ComB <- function(df0_ave,df0_sd){ return(df0_ave/(3*4*df0_sd)) } #Community Intercepts
  
  A <- list(
    ChildA(df0_sd["gradrate"]),ChildA(df0_sd["ccrpi"]),
    ChildA(df0_sd["grade3"]),ChildA(df0_sd["grade8"]),
    -1*ChildA(df0_sd["lbw"]),-1*ChildA(df0_sd["childnohealth"]),
    -1*ChildA(df0_sd["childpoverty"]),
    -1*FamilyA(df0_sd["povertyrate"]),
    -1*FamilyA(df0_sd["housingburden"]),
    -1*FamilyA(df0_sd["momsnohs"]),
    ComA(df0_sd["collegerate"]),
    -1*ComA(df0_sd["adultsnoedu"]),
    -1*ComA(df0_sd["adultnohealth"]),
    -1*ComA(df0_sd["unemployment"]))
  B <- list(
    ChildB(df0_ave["gradrate"],df0_sd["gradrate"]),
    ChildB(df0_ave["ccrpi"],df0_sd["ccrpi"]),
    ChildB(df0_ave["grade3"],df0_sd["grade3"]),
    ChildB(df0_ave["grade8"],df0_sd["grade8"]),
    -1*ChildB(df0_ave["lbw"],df0_sd["lbw"]),
    -1*ChildB(df0_ave["childnohealth"],df0_sd["childnohealth"]),
    -1*ChildB(df0_ave["childpoverty"],df0_sd["childpoverty"]),
    -1*FamilyB(df0_ave["povertyrate"],df0_sd["povertyrate"]),
    -1*FamilyB(df0_ave["housingburden"],df0_sd["housingburden"]),
    -1*FamilyB(df0_ave["momsnohs"],df0_sd["momsnohs"]),
    ComB(df0_ave["collegerate"],df0_sd["collegerate"]),
    -1*ComB(df0_ave["adultsnoedu"],df0_sd["adultsnoedu"]),
    -1*ComB(df0_ave["adultnohealth"],df0_sd["adultnohealth"]),
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"])
  )
  rownames(A)
  df0_coeff <- data.frame(name = mynames[3:16],
                          A = as.numeric(A),
                          B = as.numeric(B))
  rm(B,A,ChildA,ChildB,FamilyA,FamilyB,ComA,ComB)
  return(df0_coeff)
}

# Overall Constraints
overall_constraints <- df2

# Header ----
header = dashboardHeader(title = 'United Way App')

# Sidebar ----
counties = unique(c("overall", "Butts", "Cherokee", "Clayton", "Cobb",
"Coweta", "DeKalb", "Douglas", "Fayette", "Fulton", "Gwinnett", "Henry",
"Paulding", "Rockdale"))

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
                              choices = variables ),
                  switchInput(inputId = "calculate", value = FALSE)),
        menuItemOutput('metric_slider'))
      
      
)

# Body ----
body = dashboardBody(uiOutput("MainGrid"))


# Server ----
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
  final <- overall_constraints # this used to be test2 <- overall_constraints
  mycoef <- pop.Coef() # prep step from coefficents.R
  minCWB_Z <- -1.969282 #prep step from coefficents.R
  maxCWB_Z <- 1.380706 #prep step from coefficents.R
  if(overall_constraints[3,input$variable] != input$metric[1] 
     && final["Mean",input$variable] != median(as.vector(input$metric))){
    final[1,input$variable] <- input$metric[1] # I want to store metric changes to go to optimizer
    final[2,input$variable] <- input$metric[2]  # I want to store metric changes to go to optimizer
    final["Mean",input$variable] <- median(as.vector(input$metric)) #This is just a placeholder line for sending intial guess
  } else {
    final["Mean",input$variable] <- overall_constraints[3,input$variable]}
  #***We are optimizing this CWBZ
  final2 <- final["Mean",] # input$final2 is a placeholder for optimizer)
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  # CWBI <- median(input$gradrate)*(1+input$final/100)
  browser()
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  # browser()
  # if(is.null(CWBI)){CWBI <- as.vector(58.9)} # useful for debugging
})

# Rest of Server -----

# RENDER MENU
output$metric_slider = renderMenu( variable_reactive() )

# PLOTTING THE GAUGE
output$GaugeCWBI = renderAmCharts({
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100), 
                     color = c("#ea3838", "#00CC00"),
                     stringsAsFactors = FALSE)
  # browser()
  amAngularGauge(x = getCWBI(),
                 start = 0, end = 100,
                 main = "CWBI", bands = bands)}) 


output$sample = renderText({ input$metric })

output$GaugePlot = renderAmCharts({
    START = round(df2[1, input$variable],.1)
    value = round(df2[3, input$variable],.1)
    END = round(df2[2, input$variable],.1)
    
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
