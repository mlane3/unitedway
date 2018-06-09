"*********************************************************

            UNITED WAY FINAL APPLICATION
                Date: March 6th 2018

*********************************************************"
# Authors: Vincent, Brian, Dan, Eva, Betty, Toyin, Sally, and Michael.
# To reader, I have used the "----" notation.  These should help you go
# through the file quickly.  It is separeted like it is with Sifael's stuff

# NECESSARY PACKAGES ----
#this simple script installs packages
packages = c("foreach","stats","shiny","lpSolve","lpSolveAPI","shinydashboard","ggplot2","plotly","leaflet",
             "rAmCharts","dplyr","readxl","data.table","shinyWidgets","ggmap","rgdal","mapview")
lapply(packages, FUN = function(x){if(x %in% rownames(installed.packages())==FALSE){install.packages(x,dependencies = TRUE)}});
rm(packages)

# Shiny Dependencies
library(stats)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
# Plotting Dependencies
library(ggplot2)
#library(plotly)
library(ggmap)
library(rgdal)
library(htmltools)
library(mapview)
library(htmlwidgets)
library(RColorBrewer)
library(sp)
library(raster)
library(leaflet)
library(rAmCharts)
library(highcharter)
# Data Processing Dependencies
library(dplyr)
library(readxl)
library(data.table)
# Sourcing Prior Scripts
source('model/UWCWBI_final.R')
source('model/lpsolver.R')
source('optim_solver.R')
source('model/county_solver.R')
variablenamelist <- as.data.frame(data.table(
  variable = c( "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth",
                "childpoverty", "povertyrate", "housingburden", "momsnohs", "collegerate",
                "adultsnoedu", "adultnohealth", "unemployment" ),
  number = 1:14,
  title = c( 'S: HS Graduation Rate', 'S: HS College&Career Readiness', 'S: %
             Exceed 3rd Gr Reading Std', 'S: % Exceed 8th Gr Math Std', 'S: % Low Weight
             Births', 'S: % Children w/o Health Ins', 'S: % Children in Poverty', 'S: %
             Families Not Finan Stable', 'S: % Families w/Housing Burden', 'S: % Moms w/o
             HS Diploma', 'S: % Enrolled Post-Second Educ', 'S: % Adults w/o HS Diploma',
             'S: % Adults w/o Health Ins', 'S: Unemployment Rate'),
  plotbutton = c(0,rep(0,13)),
  starttitle = c( 'S: HS Graduation Rate', 'S: HS College&Career Readiness', 'S: %
                  Exceed 3rd Gr Reading Std', 'S: % Exceed 8th Gr Math Std', 'S: % Low Weight
                  Births', 'S: % Children w/o Health Ins', 'S: % Children in Poverty', 'S: %
                  Families Not Finan Stable', 'S: % Families w/Housing Burden', 'S: % Moms w/o
                  HS Diploma', 'S: % Enrolled Post-Second Educ', 'S: % Adults w/o HS Diploma',
                  'S: % Adults w/o Health Ins', 'S: Unemployment Rate'),
  resulttitle = c( 'R: HS  Graduation Rate', 'R: HSCollege&Career Readiness',
                   'R: % Exceed 3rd Gr Reading Std', 'R: % Exceed 8th Gr Math Std', 'R:  % Low
                   Weight Births', 'R: % Children w/o Health Ins', 'R: % Children in Poverty',
                   'R: % Families Not Finan Stable', 'R: % Families w/Housing Burden', 'R: % Moms
                   w/o HS Diploma', 'R: % Enrolled Post-Second Educ', 'R: % Adults w/o HS
                   Diploma', 'R: % Adults w/o Health Ins', 'R: Unemployment Rate'))
  )
row.names(variablenamelist) <- unlist(variablenamelist[,1])


# DATA CLEANING BEFORE SHINY ----
# # Original dataset
original <- read_xlsx("2016 Original Data.xlsx")
names(original) = c('county','weave_ct2010','gradrate','ccrpi',
                    'grade3','grade8','lbw','childnohealth',
                    'childpoverty','povertyrate','housingburden','momsnohs',
                    'collegerate','adultsnoedu','adultnohealth','unemployment')
# Overall Constraints
overall_constraints <- reactiveValues()
final <- reactiveValues()
overall_constraints <- df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1))
# county_constraints <- df1 = as.data.frame(read.csv("data/county constrants.csv"))

"*********************************************
                  HEADER
*********************************************"
# Header ----
header = dashboardHeader(title = 'United Way App')

"*********************************************
                 SIDEBAR
*********************************************"
counties <- unique(c("overall", original$county))
variables <- names(overall_constraints)
colors <- c("Quantile","RdGy",
"RdYlBu", "RdYlGn", "Spectral")

#Side Bar ----
sidebar = dashboardSidebar(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      sidebarMenu(  menuItem( text = "Multiple Plots",
                            tabName = "all_plots" )),
      # sidebarMenu( 
      #               menuItem( text = "County Selection",
      #                         icon = icon('th'),
      #                         selectInput(inputId = 'county',
      #                                     label = 'Select County:',
      #                                     choices = counties, selected = counties[1]))),
      # Eve Side Bar Menu ----
      sidebarMenu( 
        menuItem(text = "1)Enter Fixed Constraints",
                 title = "Example Button",
                 icon = icon('i-cursor'),
                 p("Enter in the the values for each indicator that you want to fix"),
                 actionButton( inputId = "execute" ,
                               label = "Submit"),
                 textInput( inputId = "plotbutton1",
                            label = paste0("Enter ",variablenamelist$starttitle[1],":")),
                 textInput( inputId = "plotbutton2",
                            label = paste0("Enter ",variablenamelist$starttitle[2],":")),
                 textInput( inputId = "plotbutton3",
                            label = paste0("Enter ",variablenamelist$starttitle[3],":")),
                 textInput( inputId = "plotbutton4",
                            label = paste0("Enter ",variablenamelist$starttitle[4],":")),
                 textInput( inputId = "plotbutton5",
                            label = paste0("Enter ",variablenamelist$starttitle[5],":")),
                 textInput( inputId = "plotbutton6",
                            label = paste0("Enter ",variablenamelist$starttitle[6],":")),
                 textInput( inputId = "plotbutton7",
                            label = paste0("Enter ",variablenamelist$starttitle[7],":")),
                 textInput( inputId = "plotbutton8",
                            label = paste0("Enter ",variablenamelist$starttitle[8],":")),
                 textInput( inputId = "plotbutton9",
                            label = paste0("Enter ",variablenamelist$starttitle[9],":")),
                 textInput( inputId = "plotbutton10",
                            label = paste0("Enter ",variablenamelist$starttitle[10],":")),
                 textInput( inputId = "plotbutton11",
                            label = paste0("Enter ",variablenamelist$starttitle[11],":")),
                 textInput( inputId = "plotbutton12",
                            label = paste0("Enter ",variablenamelist$starttitle[12],":")),
                 textInput( inputId = "plotbutton13",
                            label = paste0("Enter ",variablenamelist$starttitle[13],":")),
                 textInput( inputId = "plotbutton14",
                            label = paste0("Enter ",variablenamelist$starttitle[14],":"))
                 #.... Add more ....
        )
      ),
      sidebarMenu( 
        menuItem( text = "2)Optimization Selection",
                  icon = icon('tachometer'),
                  p("By default the algorithm figures out the optimium solution 
                  CWBI Goal of 68.9 or 10% improvement"),
                  p("Stop or Start an Optimization:"),
                  switchInput(label = 'Optimization',inputId = "calculate", value = FALSE),
                  p("Use or Don't Use a goal 68.9% CWBI:"),
                  switchInput(label = 'Unconstrained',inputId = "maxcwbi",value = FALSE)
        )),
      sidebarMenu(
        menuItem(text ="Map Controls:",
          icon = icon('map'),
          selectInput(inputId = 'variable',
                      label = 'Select a Indicator:',
                      choices = variables, selected = variables[1]),
          p("Selected:"),
          tags$div(class="header", checked=NA,
                   tags$p(textOutput(outputId = "sample2"))
          )
          # selectInput(inputId="mapcolor",label="Pick a map Color",
          #             choices = colors, selected = "RdYlGn" )
        )
      )
      
      #sidebarMenu(menuItemOutput('metric_slider'))

)
mapcolor <- "Quantile"
"*********************************************
                  BODY
*********************************************"

body = dashboardBody(
  uiOutput("MainGrid"))


"*********************************************
                 SERVER
*********************************************"
# Server ----
server = function(input, output){
  rv <- reactiveValues(run2 = 0,run3 = 0,run4 = 0,run5 = 0,run1 = 0,
                       variablenamelist = variablenamelist,
                       updated = overall_constraints['df0_ave',],
                       myfinal = df2['df0_ave',])
  # Eve: The Input Buttons --------------------------------------------------------------
  user_text = observeEvent(input$execute,{
    #Eve Added on Wednesday
    rv$variablenamelist$plotbutton[1]<-as.numeric(input$plotbutton1)
    rv$variablenamelist$plotbutton[2]<-as.numeric(input$plotbutton2)
    rv$variablenamelist$plotbutton[3]<-as.numeric(input$plotbutton3)
    rv$variablenamelist$plotbutton[4]<-as.numeric(input$plotbutton4)
    rv$variablenamelist$plotbutton[5]<-as.numeric(input$plotbutton5)
    rv$variablenamelist$plotbutton[6]<-as.numeric(input$plotbutton6)
    rv$variablenamelist$plotbutton[7]<-as.numeric(input$plotbutton7)
    rv$variablenamelist$plotbutton[8]<-as.numeric(input$plotbutton8)
    rv$variablenamelist$plotbutton[9]<-as.numeric(input$plotbutton9)
    rv$variablenamelist$plotbutton[10]<-as.numeric(input$plotbutton10)
    rv$variablenamelist$plotbutton[11]<-as.numeric(input$plotbutton11)
    rv$variablenamelist$plotbutton[12]<-as.numeric(input$plotbutton12)
    rv$variablenamelist$plotbutton[13]<-as.numeric(input$plotbutton13)
    rv$variablenamelist$plotbutton[14]<-as.numeric(input$plotbutton14)
    rv$variablenamelist$plotbutton[is.na(rv$variablenamelist$plotbutton)] <- 0.0
    rv$variablenamelist <<- rv$variablenamelist
    actionButton(inputId = "execute", label = "Submit")
    rv$run1 <- rv$run1 + 1
  })

# Select Variable for Map: ----
output$sample2 = output$sample = renderText({rv$variablenamelist[input$variable,3]})
    
#overall_constraints <- df2$ reactiveValues(overall_constraints)

# Reactive Sliders: ----

#variable_reactive = eventReactive(input$variable, 
#{
#  rv$run2 <- rv$run2 + 1
#  min_value = df2[1, input$variable]
#  max_value = df2[2, input$variable]
 # if(df2[3,input$variable] == overall_constraints[3,input$variable]){ #I am trying to intalize values here.
    # values <- c(min_value, df2[3, input$variable])
#    values <- c(df2[4, input$variable], max_value) #not sure about this change
 # } else {
    # min_value = overall_constraints[1, input$variable]
    # max_value = overall_constraints[2, input$variable]
  #  values <- c(overall_constraints[3, input$variable], max_value) #not sure about this change
#  }
    
 # if (length(input$variable == 1) )
#      sidebarMenu( menuItem( text = "Metric Slider",
#                              icon = icon('th'),
#                              tags$div(class="header", checked=NA,
#                                      tags$head("Adjust the boundary conditions of:"),
#                                      tags$p(textOutput(outputId = "sample"))
 #                             ),
                              
                              # sliderInput( inputId = "metric",
                              #              label = input$variable,
                              #              min = min_value,
                              #              max = max_value,
                              #              value = values,
                              #              sep ="",
                              #              step = .1),
#                             sliderInput("gradrate",paste(rv$variablenamelist[1,3]),
#                                         min = df2$gradrate[1],max = df2$gradrate[2],
#                                         value = c(df2$gradrate[1],df2$gradrate[2]), #for the upper range
#                                         sep ="",step = .01, ticks = FALSE),
#                             sliderInput("ccrpi",paste(rv$variablenamelist[2,3]),
#                                         min = df2$ccrpi[1],max = df2$ccrpi[2],
#                                         value = c(df2$ccrpi[1],df2$ccrpi[2]), sep ="",step = .01, ticks = FALSE),
#                             sliderInput("grade3",paste(rv$variablenamelist[3,3]),
#                                         min = df2$grade3[1],max = df2$grade3[2],
#                                         value = c(df2$grade3[1],df2$grade3[2]), sep ="",step = .01, ticks = FALSE),
#                             sliderInput("grade8",paste(rv$variablenamelist[4,3]),
#                                         min = df2$grade8[1],max = df2$grade8[2],
#                                         value = c(df2$grade8[1],df2$grade8[2]), sep ="",step = .01, ticks = FALSE),
#                             sliderInput("lbw",paste(rv$variablenamelist[5,3]),
#                                         min = df2$lbw[1],max = df2$lbw[2],
#                                         value = c(df2$lbw[1],df2$lbw[3]), sep ="",step = .01, ticks = FALSE),
 #                            sliderInput("childnohealth",paste(rv$variablenamelist[6,3]),
#                                         min = df2$childnohealth[1],max = df2$childnohealth[2],
 #                                        value = c(df2$childnohealth[1],df2$childnohealth[3]), sep ="",step = .01, ticks = FALSE),
 #                            sliderInput("childpoverty",paste(rv$variablenamelist[7,3]),
 #                                        min = df2$childpoverty[1],max = df2$childpoverty[2],
 #                                        value = c(df2$childpoverty[1],df2$childpoverty[3]), sep ="",step = .01, ticks = FALSE),
 #                            sliderInput("povertyrate",paste(rv$variablenamelist[8,3]),
 #                                        min = df2$povertyrate[1],max = df2$povertyrate[2],
 #                                        value = c(df2$povertyrate[1],df2$povertyrate[3]), sep ="",step = .01, ticks = FALSE),
  #                           sliderInput("housingburden",paste(rv$variablenamelist[9,3]),
  #                                       min = df2$housingburden[1],max = df2$housingburden[2],
#                             sliderInput("momsnohs",paste(rv$variablenamelist[10,3]),
 #                                        min = df2$momsnohs[1],max = df2$momsnohs[2],
#                                         value = c(df2$momsnohs[1],df2$momsnohs[3]), sep ="",step = .01, ticks = FALSE),
#                             sliderInput("collegerate",paste(rv$variablenamelist[11,3]),
#                                         min = df2$collegerate[1],max = df2$collegerate[2],
#                                         value = c(df2$collegerate[1],df2$collegerate[2]), sep ="",step = .01, ticks = FALSE),
 #                            sliderInput("adultsnoedu",paste(rv$variablenamelist[12,3]),
  #                                       min = df2$adultsnoedu[1],max = df2$adultsnoedu[2],
   #                                      value = c(df2$adultsnoedu[1],df2$adultsnoedu[3]), sep ="",step = .01, ticks = FALSE),
 #                            sliderInput("adultnohealth",paste(rv$variablenamelist[13,3]),
#                                         min = df2$adultnohealth[1],max = df2$adultnohealth[2],
#                                         value = c(df2$adultnohealth[1],df2$adultnohealth[3]), sep ="",step = .01, ticks = FALSE),
#                             sliderInput("unemployment",paste(rv$variablenamelist[14,3]),
#                                         min = df2$unemployment[1],max = df2$unemployment[2],
#                                         value = c(df2$unemployment[1],df2$unemployment[3]), sep ="",step = .01, ticks = FALSE)
 #                             
#        )
#      )
#})

# Eve: Update Slider ----
# update <- eventReactive(input$metric,{
#   overall_constraints[3, input$variable] <<- input$metric[1]
#   return(overall_constraints)
#   # overall_constraints[2, input$variable] <<- input$metric[2]
# })
# myupdate <- observeEvent(c(rv$run1,input$gradrate,input$ccrpi),{
#   print("running")
#   #req(rv$run2 != 0)
#   req(overall_constraints,input$gradrate)
#   #overall_constraints[1, input$variable] <<- input$metric[1]
#   #overall_constraints[2, input$variable] <<- input$metric[2]
#   rv$updated[1, "gradrate"] <- input$gradrate[1]
#   rv$updated[1, "ccrpi"] <- input$ccrpi[1]
#   rv$updated[1, "grade3"] <- input$grade3[1]
#   rv$updated[1, "grade8" ] <- input$grade8[1]
#   rv$updated[1, "lbw"] <- input$lbw[1]
#   rv$updated[1, "childnohealth"] <- input$childnohealth[1]
#   rv$updated[1, "childpoverty"] <- input$childpoverty[1]
#   rv$updated[1, "povertyrate"] <- input$povertyrate[1]
#   rv$updated[1, "housingburden"] <- input$housingburden[1]
#   rv$updated[1, "momsnohs" ] <- input$momsnohs[1]
#   rv$updated[1,  "collegerate" ] <- input$collegerate[1]
#   rv$updated[1, "adultsnoedu"]  <- input$adultsnoedu[1]
#   rv$updated[1, "adultnohealth"] <- input$adultnohealth[1]
#   rv$updated[1,  "unemployment"] <- input$unemployment[1]
#   rv$updated[2, "gradrate"] <- input$gradrate[2]
#   rv$updated[2, "ccrpi"] <- input$ccrpi[2]
#   rv$updated[2, "grade3"] <- input$grade3[2]
#   rv$updated[2, "grade8" ] <- input$grade8[2]
#   rv$updated[2, "lbw"] <- input$lbw[2]
#   rv$updated[2, "childnohealth"] <- input$childnohealth[2]
#   rv$updated[2, "childpoverty"] <- input$childpoverty[2]
#   rv$updated[2, "povertyrate"] <- input$povertyrate[2]
#   rv$updated[2, "housingburden"] <- input$housingburden[2]
#   rv$updated[2, "momsnohs" ] <- input$momsnohs[2]
#   rv$updated[2,  "collegerate" ] <- input$collegerate[2]
#   rv$updated[2, "adultsnoedu"]  <- input$adultsnoedu[2]
#   rv$updated[2, "adultnohealth"] <- input$adultnohealth[2]
#   rv$updated[2,  "unemployment"] <- input$unemployment[2]
#   # assign("rv$updated", rv$updated, envir=globalenv())
#   rv$updated <<- rv$updated
#   saveRDS(rv$updated,"Atemporaryfile.Rds")
# })

# Orginal CWBI ----
getoriginalvalues <- eventReactive(c(rv$run1,rv$variablenamelist$plotbutton),{
  req(overall_constraints,original)
  final <- overall_constraints # rv$updated
  #final <- readRDS("Atemporaryfile.Rds")
  mycoef <- pop.Coef(original) # prep step from coefficents.R
  minCWB_Z <- -1.969282 #prep step from coefficents.R
  maxCWB_Z <- 1.380706 #prep step from coefficents.R
  if(rv$run1 >= 1){
    for(i in 1:14){
      if(rv$variablenamelist$plotbutton[i] !=0){
        message = paste("Error",rv$variablenamelist$starttitle[i],"must be between",
                        df2["Min",i],"&",df2["Max",i],"OR zero (not a fixed constraint)")
        validate(need(rv$variablenamelist$plotbutton[i] > df2["Min",i],message))
        validate(need(rv$variablenamelist$plotbutton[i] < df2["Max",i],message))
        final["df0_ave",i] <- rv$variablenamelist$plotbutton[i]
      }else{
        final["df0_ave",i] <- df2["df0_ave",i]
        }}
  } else {
    final["df0_ave",] <- df2["df0_ave",]}
  
  final2 <- final["df0_ave",] 
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  CWBI <- as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3))
  final3 <- c(CWBI,final2,use.names=TRUE)
  return(final3)
},ignoreNULL = FALSE)
# Lpsolver FInal CWBI----
##This is the core of the optimizer, it used to be a linear problem
##It used to be a linear problem solver, but now its a gradient solver
getCWBI <- eventReactive(c(input$variable,rv$run1,input$maxcwbi),{
  req(overall_constraints,original)
  final <- overall_constraints
  
  final["df0_ave",] <- rv$updated
  variablenamelist2 <-rv$variablenamelist
  mycoef <- new.Coef() # pop.Coef(original) # prep step from coefficents.R
  minCWB_Z <- -1.969282 # min(df_index$CWB_Z) prep step from coefficents.R
  maxCWB_Z <- 1.380706 # max(df_index$CWB_Z) prep step from coefficents.R
  #***We are optimizing this CWBZ
  # final2 <- lp_solver(final,variablenamelist2) #lptest takes in overall_constraints or df2
  #Mike: need to improve Performance with the following 5 lines below
  final2 <- optim_solver(final,variablenamelist2)
  final2 <- final2$par[1:14]
  if(length(input$maxcwbi)==1){if(input$maxcwbi == TRUE){
    final2 <- lpmax(final,variablenamelist2)[1:14]
  }}
  names(final2) = variables
  # CWBI <- 100*(CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)
  CWBZ <- sum(mycoef$A*final2 - mycoef$B) #Calculate optimized value
  CWBI <- abs(as.numeric(round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3)))
  names(CWBI) = "CWBI"
  final3 <- c(CWBI,final2,use.names=TRUE)
  print("done")
  return(final3)
},ignoreNULL = FALSE)

# County Solver for Final CWBI ----
#output$metric_slider = renderMenu( variable_reactive() )
#myexample = observeEvent(input$gradrate,{
#output$3 = renderText({paste0(rv$run1,rv$run3)})
#})
# observeEvent(c(input$calculate, input$maxcwbi),{
#   if(input$calculate == TRUE && input$maxcwbi == TRUE){
#     rv$run3 <- 1
#     print(rv$run3)}
#   if(input$calculate == TRUE && input$maxcwbi == FALSE){
#     rv$run3 <- 0
#     print(rv$run3)}
#   if(input$calculate == FALSE && input$maxcwbi == TRUE){
#     rv$run3 <- 0}
#   if(input$calculate == FALSE && input$maxcwbi == FALSE){
#     rv$run3 <- 0}
#   if(rv$run3 ==1){
#     final <- county_solver(overall_constraints,rv$variablenamelist)
#     output$myImage <- renderImage({
#       pfad <- "Pictures/easteregg.jpg"
#       list(src = pfad,
#            contentType = 'image/png',
#            width = 200,
#            height = 150,
#            alt = "You found the Secret!!! Congradulations! :D")
#     }, deleteFile = F)
#     output$conclusion3 <- renderText("You found an easter Egg: The CWBI
#                                      Optimization by County. FOR MORE DETAILS:
#                                      go to the Welcome Tab to see the new details")
#      output$conclusion2 <- renderTable(final)
#      output$conclusion <- renderText({paste("Congratulations, you found an Easter Egg!!
#      a.k.a. Secret Feature mentioned on this welcome page. The Spatial
#      optimization by county is still in Beta (hence the table instead of a
#      plot).","This is an Easter Egg, so we can share the progress with
#      stakeholders, get feedback, and interest level.  It primarily has a few
#      instability issues, performance issues to work out, and there are still
#      bounds missing.","Conclusion:  You if you look at both the CWBI optimization
#      from the gauges and county ones below they both say that CCRPI, 8th grade
#      math, and low weight births. (We don’t mention this on other pages because
#      we want the user the draw their own conclusions.)  It is surprising because
#      the app was not designed to do a drill down, but it is giving a drill down
#      of the optimization due to accuracy alone.  The fact that results stay
#      consistent across the drill down --> this suggests the need to do stock and
#      flow models, spatial analytics (like correlation), spatial regression, so
#      we can figure out the relationships between variables and better understand
#      what we need to do to improve child well being. Bounds Issue: Primarily on
#      of the largest issues, is the lack of domain-expert upper and bounds across
#      time at the most granular level.  Specifically, we are waiting to hear back
#      from the DOE about max/min boundaries for CCRPI (at the school level in
#      greater metro Atlanta) as public data accessibility is a challenge.  If we
#      could find more accurate bounds for family poverty and adults without high
#      school diplomas Instability and Performance Issues: Due to the bounds issue
#      and need for more stackholder involvement, the optimizer currently not a
#      BUE global optimization.  The Conjugate Gradient optimizer relies on a
#      “parameter scaling factor” in order improve accuracy and speed.  Currently
#      the parameter scaling is based on simple spatial standard deviation by the
#      county distance, but this does not factor in population.  No one of the
#      team has much geospatial expertise so we did try to factor a distance
#      weight by county, but ARC could provide expertise calculating distance
#      weights and population weights. The Catch-22 (or Good News):  We should
#      mention that is actually the variation by county and indicator that plays
#      the big role in model accuracy. Also the  Some Instability allows the model
#      to Performance issue:  For the local optimization. We can fix the
#      county-level BUE problem very easily if we let optimizer run for 24 hours
#      (Basically do ensembling). However who is going to stare at a blank webpage
#      for 24 hours? Furthermore, we hope to get feedback from stakeholders, ARC,
#      UW about the spatial side of things.  To help communicate, we made this
#      Easter Egg. Future Ideas: Eventually once instability issue is solved or
#      with new data, we can ut this optimizer into either a time based
#      forecasting model (to predict future of indicators) or Pareto
#      multiobjective optimization.  These Pareto charts can answer what is the
#      cost of doing one optimization vs. another when there is more than one
#      goal! ",sep="\n")})
#   }
# 
# })

# The SWITCH ----
#This Switch Turns Optimizer on or off
switch <- eventReactive(c(rv$run1,input$variable,input$calculate),{
  req(overall_constraints)
  validate(need(input$variable !="","Error: Intial Guess got lost. Please select data to make sure it is avialable"))
  #validate(rv$run1)
  rv$updated <- overall_constraints["df0_ave",] #Mike: IS this still used?
  
  if(input$calculate==TRUE){
    rv$updated <- getoriginalvalues()[2:15]
    
    value<-getCWBI()
    for(i in 1:length(value)){if(value[i]>=100){value[i]<-99.9}
    }
    rv$variablenamelist$title <- rv$variablenamelist$resulttitle
  }else{value<-getoriginalvalues()
  rv$updated <- value[2:15]
  
  rv$variablenamelist$title <- rv$variablenamelist$starttitle
  }
  
  return(value)
},ignoreNULL = FALSE,ignoreInit = FALSE)

# Plotting -----
observe(switch())
# observeEvent(c(rv$run1,input$calculate),{
#   final <- as.data.frame(switch())
#   output$conclusion <- renderTable(final)
# })
# output$GaugePlot = output$GaugeCWBI = renderAmCharts({
#   final <- switch() #(Load child well being)
#   # if(is.null(final)){final <- as.vector(58.9)} # useful for debugging
#   value = unname(unlist(final[1]))
#   # AM Angular Gauge
#   # bands = data.frame(start = c(0,58.9), end = c(58.9, 100),
#   #                    color = c("#ea3838", "#00CC00"),
#   #                    stringsAsFactors = FALSE)
#   amSolidGauge(x = 59, color = c("#ea3838", "#00CC00"),type="semi")
#   amSolidGauge(x = 100, color = c("#ea3838", "#00CC00"),type="semi")
#   amSolidGauge(x = value,
#                  min = 0, max = 100,
#                  main = "CWBI", type = "semi",type="semi")
output$GaugeCWBI <- renderAmCharts({
  message = paste("You find the UW APP debug page. Server is asleep", br(),
  "Server needs user input. Please find & click the submit button or optimize button. If it does not display charts after 15 seconds, then please contact the app owner")
  validate(need(is.null(input$calculate)==F,message),
           need(is.null(input$execute)==F,message),
           need(is.null(input$variable)==F,message))
  rv$myfinal <- switch() #(Load child well being)
  # if(is.null(final)){final <- as.vector(58.9)} # useful for debugging
  value = 58.489
  value = round(unname(unlist(rv$myfinal[1])),1)
  # AM Angular Gauge
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100),
                     color = c("#ea3838", "#00CC00"),width=15,
                     stringsAsFactors = FALSE)
  #mainColor = "#FFFFFF"
  amAngularGauge(x = value, textsize = 12,
                 start = 0, end = 100,main = "CWBI",step = 25,
                 bands = bands,#theme="dark",mainColor = "#FFFFFF",
                 creditsPosition = "bottom-right")
  })
output$GaugePlot <- renderAmCharts({
  message = paste("You find the UW APP debug page. Server is asleep", br(),
                  "Server needs user input. Please find & click the submit button or optimize button. If it does not display charts after 15 seconds, then please contact the app owner")
  validate(need(is.null(input$calculate)==F,message),
           need(is.null(input$execute)==F,message),
           need(is.null(input$variable)==F,message))
  rv$myfinal <- switch() #(Load child well being)
  # if(is.null(final)){final <- as.vector(58.9)} # useful for debugging
  value = 58.489
  value = round(unname(unlist(rv$myfinal[1])),1)
  # AM Angular Gauge
  bands = data.frame(start = c(0,58.9), end = c(58.9, 100),
                     color = c("#ea3838", "#00CC00"),width=15,
                     stringsAsFactors = FALSE)
  #mainColor = "#FFFFFF"
  amAngularGauge(x = value, textsize = 12,
                 start = 0, end = 100,main = "CWBI",step = 25,
                 bands = bands,#theme="dark",mainColor = "#FFFFFF",
                 creditsPosition = "bottom-right")
})
output$GaugePlot1 = renderAmCharts({
  final = switch() #(Load child well being)
  #note START and END need to either be both even or both odd integers
  START = 43
  value = round(df2[4, "gradrate"],1) 
  END = 97
  DIAL = round(unname(unlist(rv$myfinal["gradrate"])),1) # overall_constraints[3, "gradrate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('gradrate' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[1], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot2 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 44
  value = round(df2[4, "ccrpi"],1)
  END = 94
  DIAL = round(unname(unlist(rv$myfinal["ccrpi"])),1) # overall_constraints[3, "ccrpi"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is ccrpi or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('ccrpi' == 'ccrpi'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[2], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot3 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 28
  value = round(df2[4, "grade3"],1)
  END = 90
  DIAL = round(unname(unlist(rv$myfinal["grade3"])),1) # overall_constraints[3, "grade3"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is grade3 or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('grade3' == 'grade3'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[3], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot4 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 3
  value = round(df2[4, "grade8"],1)
  END = 83
  DIAL = round(unname(unlist(rv$myfinal["grade8"])),1) # overall_constraints[3, "grade3"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('grade8' == 'grade8'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[4], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot5 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 2
  value = round(df2[4, "lbw"],1)
  END = 20
  DIAL = round(unname(unlist(rv$myfinal["lbw"]),.01)) # overall_constraints[3, "lbw"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('lbw' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[5], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot6 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "childnohealth"],1)
  END = 48
  DIAL = round(unname(unlist(rv$myfinal["childnohealth"])),1) # overall_constraints[3, "childnohealth"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('childnohealth' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[6], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot7 = renderAmCharts({
  #final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "childpoverty"],1)
  END = 42
  DIAL = round(unname(unlist(rv$myfinal["childpoverty"])),1) # overall_constraints[3, "childpoverty"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('childpoverty' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[7], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot8 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 1
  value = round(df2[4, "povertyrate"],1)
  END = 94
  DIAL = round(unname(unlist(rv$myfinal["povertyrate"])),1) # overall_constraints[3, "povertyrate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('povertyrate' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[3], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot9 = renderAmCharts({
  ##final = switch() #(Load child well being)
  START = 10
  value = round(df2[4, "housingburden"],1)
  END = 76
  DIAL = round(unname(unlist(rv$myfinal["housingburden"])),1) # overall_constraints[3, "housingburden"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('housingburden' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[9], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot10 = renderAmCharts({
  ###final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "momsnohs"],1)
  END = 80
  DIAL = round(unname(unlist(rv$myfinal["momsnohs"])),1) # overall_constraints[3, "momsnohs"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('momsnohs' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[10], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot11 = renderAmCharts({
  ###final = switch() #(Load child well being)
  START = 41
  value = round(df2[4, "collegerate"],1)
  END = 95
  DIAL = round(unname(unlist(rv$myfinal["collegerate"])),1) # overall_constraints[3, "collegerate"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('collegerate' == 'collegerate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[11], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot12 = renderAmCharts({
  ###final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "adultsnoedu"],1)
  END = 78
  DIAL = round(unname(unlist(rv$myfinal["adultsnoedu"])),1) # overall_constraints[3, "adultsnoedu"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('adultsnoedu' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[12], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot13 = renderAmCharts({
  ###final = switch() #(Load child well being)
  START = 0
  value = round(df2[4, "adultnohealth"],1)
  END = 92
  DIAL = round(unname(unlist(rv$myfinal["adultnohealth"])),1) # overall_constraints[3, "adultnohealth"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('adultnohealth' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[13], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
output$GaugePlot14 = renderAmCharts({
  ###final = switch() #(Load child well being)
  START = 1
  value = round(df2[4, "unemployment"],1)
  END = 14
  DIAL = round(unname(unlist(rv$myfinal["unemployment"])),1) # overall_constraints[3, "unemployment"]
  # AM Angular Gauge
  #PURU Comment: Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  if(('unemployment' == 'gradrate'))
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#ea3838","#00CC00"),width=15,
                        stringsAsFactors = FALSE)
  }
  else
  {
    bands <- data.frame(start = c(START,value), end = c(value, END),
                        color = c("#00CC00", "#ea3838"),width=15,
                        stringsAsFactors = FALSE)
  }
  amAngularGauge(x = DIAL,textsize = 12,
                 start = START, end = END,
                 main = rv$variablenamelist$title[14], bands = bands,step=(END-START)/2,
                 creditsPosition = "bottom-right")
})
# Plot the high charts ----
output$mychart4 <- renderHighchart({
  
  highchart(width = 400, height = 400) %>% 
    hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
    hc_title(text = "Child & Education",style = list(fontSize = "20px")) %>% 
    hc_tooltip(borderWidth = 0,backgroundColor = '#fff',shadow = FALSE,style = list(fontSize = '16px'),
               pointFormat = '{series.name}<br><span style="font-size:2em; color: color: black; font-weight: bold">{point.y}%</span>',
               positioner = JS("function (labelWidth, labelHeight) {return {x: 100 + labelWidth / 2,y: 180};}")) %>% 
    hc_pane(startAngle = 0,endAngle = 270,
            background = list(
              list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth =  0),
              list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0),
              list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
    hc_yAxis(min = 0,
             max = 100,
             tickLength= 100,
             lineWidth = 0,
             TickWidth = 10,
             minorTickInterval=12.5,
             minorTickWidth=5,
             tickColor= 'black',
             tickAmount=5,
             labels=list(x=-24,y=-5,style = list(fontSize = "90%",fontWeight= "bold", color= "black"))
             #tickPositions = list()
    ) %>% #
    hc_plotOptions(solidgauge = list(borderWidth = '15px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[1],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "108%",innerRadius = "100%",y = as.numeric(unname(unlist(rv$myfinal["gradrate"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[1],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "108%",innerRadius = "108%",y = as.numeric(unname(unlist(rv$updated["gradrate"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[2],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "90%",innerRadius = "82%",y = as.numeric(unname(unlist(rv$myfinal["ccrpi"])))))) %>%
    hc_add_series(name = rv$variablenamelist$title[2],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "90%",innerRadius = "90%",y = as.numeric(unname(unlist(rv$updated["ccrpi"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[3],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "72%",innerRadius = "64%",y = as.numeric(unname(unlist(rv$myfinal["grade3"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[3],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "72%",innerRadius = "72%",y = as.numeric(unname(unlist(rv$updated["grade3"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[4],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "54%",innerRadius = "46%",y = as.numeric(unname(unlist(rv$myfinal["grade8"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[4],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "54%",innerRadius = "54%",y = as.numeric(unname(unlist(rv$updated["grade8"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[11],borderColor = JS("Highcharts.Color('#7cb5ec').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#7cb5ec').setOpacity(0).get()"),radius = "36%",innerRadius = "28%",y = unname(as.numeric(unlist(rv$myfinal["collegerate"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[11],borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "36%",innerRadius = "36%",y = as.numeric(unname(unlist(rv$updated["collegerate"])))))) 
})
output$mychart3 <- renderHighchart({
  
  highchart(width = 400, height = 400) %>% 
    hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
    hc_title(text = "Health and Moms",style = list(fontSize = "20px")) %>% 
    hc_tooltip(borderWidth = 0,backgroundColor = '#fff',shadow = FALSE,style = list(fontSize = '16px'),
               pointFormat = '{series.name}<br><span style="font-size:2em; color: color: black; font-weight: bold">{point.y}%</span>',
               positioner = JS("function (labelWidth, labelHeight) {return {x: 100 + labelWidth / 2,y: 180};}")) %>% 
    hc_pane(startAngle = 0,endAngle = 270,
            background = list(
              list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth =  0),
              list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0),
              list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
    hc_yAxis(min = 0,
             max = 60,
             tickLength= 100,
             lineWidth = 0,
             TickWidth = 10,
             minorTickInterval=15,
             minorTickWidth=5,
             tickColor= 'black',
             tickAmount=3,
             labels=list(x=-24,y=-5,style = list(fontSize = "90%",fontWeight= "bold", color= "black"))
             #tickPositions = list()
    ) %>% #
    hc_plotOptions(solidgauge = list(borderWidth = '15px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[5],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "108%",innerRadius = "100%",y = as.numeric(unname(unlist(rv$myfinal["lbw"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[5],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "108%",innerRadius = "108%",y = as.numeric(unname(unlist(rv$updated["lbw"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[6],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "90%",innerRadius = "82%",y = as.numeric(unname(unlist(rv$myfinal["childnohealth"])))))) %>%
    hc_add_series(name = rv$variablenamelist$title[6],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[6]"),radius = "90%",innerRadius = "90%",y = as.numeric(unname(unlist(rv$updated["childnohealth"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[10],borderColor = JS("Highcharts.Color('#e4d354').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#e4d354').setOpacity(0).get()"),radius = "72%",innerRadius = "64%",y = as.numeric(unname(unlist(rv$myfinal["momsnohs"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[10],borderColor = JS("Highcharts.getOptions().colors[6]"),data = list(list(color = JS("Highcharts.getOptions().colors[6]"),radius = "72%",innerRadius = "72%",y = as.numeric(unname(unlist(rv$updated["momsnohs"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[13],borderColor = JS("Highcharts.Color('#7cb5ec').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#7cb5ec').setOpacity(0).get()"),radius = "54%",innerRadius = "46%",y = as.numeric(unname(unlist(rv$myfinal["adultnohealth"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[13],borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "54%",innerRadius = "54%",y = as.numeric(unname(unlist(rv$updated["adultnohealth"])))))) 
})
output$mychart2 <- renderHighchart({
  
  highchart(width = 400, height = 400) %>% 
    hc_chart(type = "solidgauge",backgroundColor = "#F0F0F0",marginTop = 50) %>% 
    hc_title(text = "Income and Family",style = list(fontSize = "20px")) %>% 
    hc_tooltip(borderWidth = 0,backgroundColor = '#fff',shadow = FALSE,style = list(fontSize = '16px'),
               pointFormat = '{series.name}<br><span style="font-size:2em; color: color: black; font-weight: bold">{point.y}%</span>',
               positioner = JS("function (labelWidth, labelHeight) {return {x: 100 + labelWidth / 2,y: 180};}")) %>% 
    hc_pane(startAngle = 0,endAngle = 270,
            background = list(
              list(outerRadius = '112%',innerRadius = '88%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth =  0),
              list(outerRadius = '87%',innerRadius = '63%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0),
              list(outerRadius = '62%',innerRadius =  '38%',backgroundColor = JS("Highcharts.Color('#F0F0F0').setOpacity(0.1).get()"),borderWidth = 0))) %>% 
    hc_yAxis(min = 0,
             max = 60,
             tickLength= 100,
             lineWidth = 0,
             TickWidth = 10,
             minorTickInterval=15,
             minorTickWidth=5,
             tickColor= 'black',
             tickAmount=3,
             labels=list(x=-24,y=-5,style = list(fontSize = "90%",fontWeight= "bold", color= "black"))
             #tickPositions = list()
    ) %>% #7cb5ec
    hc_plotOptions(solidgauge = list(borderWidth = '15px',dataLabels = list(enabled = FALSE),linecap = 'round',stickyTracking = FALSE)) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[7],borderColor = JS("Highcharts.Color('#ED561B').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#ED561B').setOpacity(0).get()"),radius = "108%",innerRadius = "100%",y = as.numeric(unname(unlist(rv$myfinal["childpoverty"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[7],borderColor = JS("Highcharts.getOptions().colors[8]"),data = list(list(color = JS("Highcharts.getOptions().colors[8]"),radius = "108%",innerRadius = "108%",y = as.numeric(unname(unlist(rv$updated["childpoverty"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[8],borderColor = JS("Highcharts.Color('#e4d354').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#e4d354').setOpacity(0).get()"),radius = "90%",innerRadius = "82%",y = as.numeric(unname(unlist(rv$myfinal["housingburden"])))))) %>%
    hc_add_series(name = rv$variablenamelist$title[8],borderColor = JS("Highcharts.getOptions().colors[6]"),data = list(list(color = JS("Highcharts.getOptions().colors[6]"),radius = "90%",innerRadius = "90%",y = as.numeric(unname(unlist(rv$updated["housingburden"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[9],borderColor = JS("Highcharts.Color('#e4d354').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#e4d354').setOpacity(0).get()"),radius = "72%",innerRadius = "64%",y = as.numeric(unname(unlist(rv$myfinal["povertyrate"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[9],borderColor = JS("Highcharts.getOptions().colors[6]"),data = list(list(color = JS("Highcharts.getOptions().colors[6]"),radius = "72%",innerRadius = "72%",y = as.numeric(unname(unlist(rv$updated["povertyrate"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[12],borderColor = JS("Highcharts.Color('#7cb5ec').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#7cb5ec').setOpacity(0).get()"),radius = "54%",innerRadius = "46%",y = as.numeric(unname(unlist(rv$myfinal["adultsnoedu"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[12],borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "54%",innerRadius = "54%",y = as.numeric(unname(unlist(rv$updated["adultsnoedu"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$starttitle[14],borderColor = JS("Highcharts.Color('#7cb5ec').setOpacity(0.5).get()"),data = list(list(color = JS("Highcharts.Color('#7cb5ec').setOpacity(0).get()"),radius = "36%",innerRadius = "28%",y = unname(as.numeric(unlist(rv$myfinal["unemployment"])))))) %>% 
    hc_add_series(name = rv$variablenamelist$title[14],borderColor = JS("Highcharts.getOptions().colors[0]"),data = list(list(color = JS("Highcharts.getOptions().colors[0]"),radius = "36%",innerRadius = "36%",y = as.numeric(unname(unlist(rv$updated["unemployment"])))))) 
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
  # dfzipmap <- raster::merge(original,UWcensuszip,by="TRACT")
  original$trunctract<-uwmapdata$Tract
  original <- original[order(match(original$TRACT, counties$GEOID10)),]
  legendcolor <- mycolor <- as.numeric(unlist(original[, input$variable]))
  df_complete <- df_complete[order(match(df_complete$weave_ct2010,original$TRACT))]
  if(length(input$maxcwbi)==1){if(input$maxcwbi == TRUE){
    mycolor<- as.numeric(df_complete$CWB_Index)*100
    legendcolor<- as.numeric(df_complete$CWB_Index)*100
  }}
  #If value positively impacts CWBI then don't reverse else reverse the color scale.
  # if((input$variable == 'gradrate') || (input$variable == 'ccrpi') || (input$variable == 'grade3') || (input$variable == 'grade8') || (input$variable == 'collegerate')){
  #   reverse = FALSE}else{reverse=TRUE}
  reverse = FALSE
  #Add our color pallete to our map
  bins <- c(0, .10*max(mycolor), .20*max(mycolor), .30*max(mycolor),
            .40*max(mycolor), .50*max(mycolor), .60*max(mycolor), .70*max(mycolor), Inf)
  
  if(mapcolor == "Quantile"){pal <- colorQuantile("RdYlGn", domain = mycolor, n=5)
  }else{
    pal <- colorBin(mapcolor, domain = mycolor, bins = bins,reverse = reverse)
  }
  
  # mycolor <- dff0$trunctract
  # mycolor <- as.numeric(paste(original$trunctract))
  labels<-paste("<p>", original$county,"<p>","CWBI ",
                round(as.numeric(df_complete$CWB_Index)*100,2),"<p>", input$variable,
                " ",round(as.numeric(unlist(original[, input$variable])), digits = 5),"<p>",
                sep="")
  if(input$maxcwbi!=TRUE){legendtitle <- rv$variablenamelist[input$variable,3]
  }else{legendtitle <- "CWBI"}
  pal2 <- colorNumeric(palette = "RdYlGn",domain = legendcolor)
  if(input$calculate == TRUE){value = getCWBI() #allows the switch to control map
  mycolor <- as.numeric(value[input$variable])}
  if(length(input$maxcwbi)==1){if(input$maxcwbi == TRUE && input$calculate == TRUE){
  value = getCWBI() #Mike: we can add more dyanmicism here
  mycolor <- as.numeric(68.9)}}
  #Plot the map
  leaflet() %>%
    setView(lng = -84.386330, lat = 33.753746, zoom = 8) %>%
    addProviderTiles(providers$Stamen.Toner) %>%
    addPolygons(data = counties,
                fillColor = pal(mycolor),
                weight = 1,
                smoothFactor = 0.5,
                color = "green",
                fillOpacity = 0.8,
                highlight= highlightOptions(weight = 5, color ="#666666", dashArray = "",
                                            fillOpacity = .7, bringToFront = TRUE ),
                label = lapply(labels, HTML)) %>%
    addLegend(position="bottomright", pal = pal2, values = legendcolor,
              title = legendtitle,
              labFormat = labelFormat(suffix = "%"),
              opacity = 1
    )

  })

"*******************************
          MAIN GRID
*******************************"
# The Actual Body or "Main Grid"----
output$MainGrid = renderUI({
      # Evaluating the Overall Page
      if (is.null(input$calculate)==TRUE||is.null(input$execute)==TRUE||
          is.null(input$variable)==TRUE)
      {
        p("Welcome to United App: Server has not finished waking up", br(),
          "If you are seeing this text it means server is starting up and ran a small bug.  Basically the server
          needs to be woken up with a user input. Please find the submit button or optimize button.  If you don't
          see a change in anything after 15 seconds or less please contact the app owner")
      } else {
      tabsetPanel(tabPanel("Additional Content here",verbatimTextOutput('sample3')))
        tabsetPanel(
         tabPanel("Welcome",h1("Welcome to the Child Well Being Optimizer"),
                  p("This optimizer is aimed at determining what factors can
                  be changed in order to improve Child Well- Being. to help
                  figure out what needs to be changed to improve child well
                  being.Through the manipulation of chosen factors you can see
                  how the selected indicators have an effect on the child
                  well-being index. The Optimizers uses gauge plots that allow
                  you to compare the original values and optimized values for
                  each child well-being indicator. Initially, gauges shows
                  average values of the orginal data.  Note this app can be easily updated to the 2016 data by simply uploading the right csv"),
                  h3("Directions:"),
                  p(strong("To Start:"),"Input what variables you would like
                  to fix, and then decide how you want to optimize. The
                  optimization is based on what you decide to fix. For
                  example, in 2016 % moms without high school births was 15.1%
                  while unemployment was 5.3.  By increasing moms without high
                  school to over 15.1, we see that families not fin stable,
                  housing burden, and unemployment have to imp.rove (decrease)
                  by over 1 to 4.  By manipulating or decreasing the
                  unemployment rate you can see the other indicators only have
                  to improve by less to reach the goal of 689. Next, see your
                  results in Optimizer tab. The S stands for the starting
                  average Atlanta values while R stands for the resulting
                  optimized average Atlanta values. Green indicates an
                  improvement in CWBI while red indicates a diminishment in
                  CWBI."), p(strong("Using the map:"),"The map allows you to
                  see the indicator values at the census track level. You can
                  use the map to compare your current values to the Atlanta
                  average values. ",strong("The color scale:"), "he color
                  scale is based on quartile analysis of the original data and
                  is similar to the color scale found on other CWBI maps.","We
                  hope this optimizer helps you to see Child Well Being from a
                  broader perspective.",strong("Secret Feature Hint:"),"If the
                  map displays only one color it is because the app is
                  allowing you to compare your original value to the Atlanta
                  average value. You can quickly",'turn the optimizer on/off',"to
                  see the old and new values. Changing the fix bound triggers
                  the optimizer to go through a full run. "),
                  h3("About Optimization:"),
                  p("Please note the optimizer may take up to 5 seconds to load"),
                  p("By default the algorithm figures out the optimium solution
                    CWBI Goal of 68.9% or 10% improvement from the current 58.9%.
                    Since relationship between 2 indicators is not well known,
                    It treats each indicator as an independent variable,
                    but relies on the original data. It uses a simple conjugate gradient spatial
                    optimization (see Beale, E.M.L. 1972 for details or google
                    Beale–Sorenson optimization or gradient descent optimization
                    for the broader optimization case).
                    Just like gradient descent of a single node neural network, it iterates through about 200-1000
                    solutions to child well being while descending through the solutions
                    by a gradient (or subtracting) until it find the globally optimized solution.
                    Then it searches for the nearest reasonable local optimized
                    solution. On average iteration step takes about 5.56 seconds.",p(),p()),
                  textOutput("conclusion")
                  ),
         tabPanel("Optimizer",strong("Welcome to the Child Well Being
         Optimizer."), p("This optimizer is to help figure out how indicators
         affect Child Well-Being index. To Start: Input what variables you
         want to fix. Then decide how you want to optimize. Then click
         optimize. S = Start and R = Result."), p("For example,  For example,
         input % Moms without High School to 15.1% while unemployment is %5.3
         in 2016. Input in both and turn on 'optimize', You shoudl get that
         child poverty has to be 20.1 and is the largest shift that indicators
         have to make to compensate for the increase % moms with no high
         school"),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot",height="200"),background='black')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot1",height="200"),background='red')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot2",height="200"),background='red'))),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot3",height="200"),background='red')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot4",height="200"),background='red')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot5",height="200"),background='red'))),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot6",height="200"),background='red')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot7",height="200"),background='red')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot8",height="200"),background='orange'))),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot9",height="200"),background='orange')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot10",height="200"),background='orange')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot11",height="200"),background='orange'))
                    ),
            fluidRow( column(4,
                           box(width=12, amChartsOutput("GaugePlot12",height="200"),background='blue')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot13",height="200"),background='blue')),
                    column(4,
                           box(width=12, amChartsOutput("GaugePlot14",height="200"),background='blue')))
                  ),
         tabPanel("Compare Optimization",strong("Welcome to the Child Well Being
                                     Optimizer."), p("All the directions are the same.  However, graphically
                                                     It might be a bit clear what is going on with child well being
                                                     As now you can see all the indicators near each other.
                                                     Hover over a bar to see what indicator is is.  Indicators
                                                     best grouped by county level correlations in the data. Similar to a Iphone
                                                     Health cart the gauges show the Results as Goals.  In fact optimization is about prescribing what the goal
                                                     for certain indicators should be rather then predict what they are.  It strongly suggests the need to find relationships between
                                                     highly spatially correlated indicators."),
                  fluidRow(column(6,box(width=12, amChartsOutput("GaugeCWBI",height="400"),background='black')),
                           column(6,box(width=12, highchartOutput("mychart2")))),
                  fluidRow(column(6,box(width=12, highchartOutput("mychart3"))),
                           column(6,box(width=12, highchartOutput("mychart4")))
                  )
        ),
         tabPanel("Map of Atlanta",
                  h3("A map of Atlanta Child Well Being"),
                  p("Please note this the map may take about 4 mins to load. It has to fetch a google or open street map."),
                  fluidPage(leafletOutput("mymap")),
                  conditionalPanel(condition = "input.maxcwbi == TRUE",
                                   imageOutput("myImage",height="100px"),
                                   textOutput("conclusion3"),
                                   tableOutput("conclusion2")))
        ) #This bracket goes to Tab set
        } #This bracket is for the if statement
    } #This bracket is the main grid
  ) #This paren is the main grid
} #This bracket is the server

"*********************************************

                 RUNAPP
# *********************************************"
# Runapp ----
# options(shiny.error = NULL)
options(shiny.error = recover)
# options(shiny.reactlog=TRUE) 
options(shiny.sanitize.errors = FALSE)
#display.mode="showcase" #debug code
app <- shinyApp(ui = dashboardPage(skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body),
          server = server)
