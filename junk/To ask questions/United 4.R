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
counties = unique(c("overall", "Butts", "Cherokee", "Clayton", "Cobb",
                    "Coweta", "DeKalb", "Douglas", "Fayette", "Fulton", "Gwinnett", "Henry",
                    "Paulding", "Rockdale"))
pop.Coef <- function(){
  df0_ave = c(74.00129, 69.80657, 46.03773, 33.23892, 9.273624, 10.904, 24.06609,
              30.84306, 38.48023, 13.89575, 75.0582, 12.2767, 23.39984, 12.2241)
  names(df0_ave) = mynames[1:14]
  df0_sd = c(11.73959, 10.46115, 20.86766, 18.81275, 2.800196, 8.375753, 19.90409,
             20.26725, 10.86507, 11.83264, 10.64412, 9.722596, 13.92478, 6.665234)
  names(df0_sd) = mynames[1:14]
  ChildA <- function(df0_sd){return(1/(3*7*df0_sd))} #Child
  FamilyA <- function(df0_sd){ return(1/(3*3*df0_sd)) } #Family
  ComA <- function(df0_sd){ return(1/(3*4*df0_sd)) } #Community
  
  ChildB <- function(df0_ave,df0_sd){ return(df0_ave/(3*7*df0_sd)) } #Child 
  FamilyB <- function(df0_ave,df0_sd){ return(df0_ave/(3*3*df0_sd)) } #Family 
  ComB <- function(df0_ave,df0_sd){ return(df0_ave/(3*4*df0_sd)) } #Community 
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
    -1*ComB(df0_ave["unemployment"],df0_sd["unemployment"]))
  df0_coeff <- data.frame(name = mynames[1:14],
                          A = as.numeric(A),
                          B = as.numeric(B))
  rm(B,A,ChildA,ChildB,FamilyA,FamilyB,ComA,ComB)
  return(df0_coeff)
}
library(lpSolve)
library(lpSolveAPI)
lptest <- function(df2){
  Value = .689
  maxCWB_Z <- 1.380706
  minCWB_Z <- -1.969282
  mycoef <- pop.Coef() # prep step from coefficents.R
  ValueZ = (Value*(maxCWB_Z - minCWB_Z)) + minCWB_Z #inverse formula for normalization
  model <- make.lp(0, 15)
  set.objfn(model, c(mycoef$A[1],mycoef$A[2],mycoef$A[3],mycoef$A[4],
                     mycoef$A[5],mycoef$A[6],mycoef$A[7],mycoef$A[8],  
                     mycoef$A[9],mycoef$A[10],mycoef$A[11],mycoef$A[12],
                     mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))
  set.bounds(model,
             lower=c(df2$gradrate[1], df2$ccrpi[1], df2$grade3[1], df2$grade8[1], df2$lbw[1],
                     df2$childnohealth[1], df2$childpoverty[1], df2$povertyrate[1], df2$housingburden[1],
                     df2$momsnohs[1], df2$collegerate[1], df2$adultsnoedu[1], df2$adultnohealth[1], df2$unemployment[1],.90),
             upper=c(df2$gradrate[2], df2$ccrpi[2], df2$grade3[2], df2$grade8[2], df2$lbw[2],
                     df2$childnohealth[2], df2$childpoverty[2], df2$povertyrate[2], df2$housingburden[2],
                     df2$momsnohs[2], df2$collegerate[2], df2$adultsnoedu[2], df2$adultnohealth[2], df2$unemployment[2],1.0)) #***We are using for constraints
  # 1/n*coeff is how we will add it for multple contraints A*x = Y +B
  add.constraint(model, (c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
                           mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
                           mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
                           mycoef$A[13],mycoef$A[14],-sum(mycoef$B)))^2, "<=", -(0.00386-ValueZ))
  add.constraint(model, c(mycoef$A[1],mycoef$A[2], mycoef$A[3],mycoef$A[4],
                          mycoef$A[5],mycoef$A[6], mycoef$A[7], mycoef$A[8],
                          mycoef$A[9], mycoef$A[10],mycoef$A[11],mycoef$A[12],
                          mycoef$A[13],mycoef$A[14],0), ">=", (ValueZ+sum(mycoef$B))) 
  add.constraint(model, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), "=", 1)  
  # Compute the optimized model ----
  lp.control(model,sense='min', verbose='normal') #
  solve(model)
  return(get.variables(model))
}
# Header ----
header = dashboardHeader(title = 'United Way App')

# Sidebar ----
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
                          choices = variables, selected = variables[2]),
              switchInput(label = 'Start Optimization?',inputId = "calculate", value = FALSE)    ),
    menuItemOutput('metric_slider'))
)

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
getoriginalvalues <- eventReactive(input$metric,{
  req(overall_constraints)
  updated <- myupdate()
  overall_constraints <<- updated
  final <- overall_constraints# updated
  mycoef <- pop.Coef() # prep step from coefficents.R
  minCWB_Z <- -1.969282 #prep step from coefficents.R
  maxCWB_Z <- 1.380706 #prep step from coefficents.R
  if(is.null(input$metric)==FALSE){
    if(overall_constraints[4,input$variable] != input$metric[1] 
       && final["df0_ave",input$variable] != median(as.vector(input$metric))){
      final[1,input$variable] <- input$metric[1]
      final[2,input$variable] <- input$metric[2]
      final["df0_ave  ",input$variable] <- median(as.vector(input$metric))
    } else {
      final["df0_ave",input$variable] <- overall_constraints[4,input$variable]}
  }
  final2 <- final["df0_ave",] 
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  final2 <- colMeans(final[1:2,])
  final3 <- c(CWBI,final2,use.names=TRUE)
  return(final3)
},ignoreNULL = FALSE)
getCWBI <- eventReactive(input$metric,{
  req(overall_constraints)
  if(is.null(input$metric)==TRUE){final <- overall_constraints
  }else{
    update <- myupdate()
    final <- update
  }
  mycoef <- pop.Coef() # prep step from coefficents.R
  minCWB_Z <- -1.969282 #prep step from coefficents.R
  maxCWB_Z <- 1.380706 #prep step from coefficents.R
  final2 <- lptest(final) #lptest takes in original_constrants or df2
  final2 <- final2[1:14]
  names(final2) = variables
  CWBZ <- rowSums(mycoef$A*t(final2) - mycoef$B)
  CWBZ <- sum(mycoef$A*final2 - mycoef$B)
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  CWBI <- abs(CWBI)
  names(CWBI) = "CWBI"
  final3 <- c(CWBI,final2,use.names=TRUE)
  return(final3)
},ignoreNULL = FALSE)

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
output$GaugePlot1 = renderAmCharts({
  final = switch() #(Load child well being)
  START = round(df2[1, "ccrpi"],.1)
  value = round(df2[4, "ccrpi"],.1)
  END = round(df2[2, "ccrpi"],.1)
  DIAL = round(unname(unlist(final["ccrpi"]))) # overall_constraints[3, "ccrpi"]
  bands <- data.frame(start = c(START,value), end = c(value, END), 
                        color = c("#ea3838","#00CC00"),
                        stringsAsFactors = FALSE)
  amAngularGauge(x = DIAL,start = START, end = END,main = "ccrpi", bands = bands)
}) 
output$GaugePlot2 = renderAmCharts({
  final = switch() #(Load child well being)
  START = round(df2[1, "lbw"],.1)
  value = round(df2[4, "lbw"],.1)
  END = round(df2[2, "lbw"],.1)
  DIAL = round(unname(unlist(final["lbw"]),.01)) # overall_constraints[3, "lbw"]
  bands <- data.frame(start = c(START,value), end = c(value, END), 
                        color = c("#00CC00", "#ea3838"),
                        stringsAsFactors = FALSE)
  amAngularGauge(x = DIAL, 
                 start = START, end = END,
                 main = "lbw", bands = bands)
})
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
  main = paste0("Selected Variable:",input$variable)
  if(is.null(input$metric) == TRUE){
    amAngularGauge(x = START,
                   start = START,end = END,main = main, bands = bands) 
  } else{
    amAngularGauge(x = round(median(as.vector(input$metric)),.1), 
                   start = START, end = END,main = input$variable, bands = bands) 
  }
})
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
