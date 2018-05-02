"**********************************************

    UNITED WAY MULTIPLE LOTS AND BUTTON IDEA
              APRIL 14th 2018
**********************************************"

# NECESSARY PACKAGES
library(shiny)
library(shinydashboard)
library(ggplot2)

variables <- mynames <- c('gradrate','ccrpi',
                                           'grade3','grade8','lbw','childnohealth',
                                           'childpoverty','povertyrate','housingburden','momsnohs',
                                           'collegerate','adultsnoedu','adultnohealth','unemployment')
counties = unique(c("overall", "Butts", "Cherokee", "Clayton", "Cobb",
                    "Coweta", "DeKalb", "Douglas", "Fayette", "Fulton", "Gwinnett", "Henry",
                    "Paulding", "Rockdale"))


variablenamelist <- data.table(variable = c( "gradrate", "ccrpi", "grade3", "grade8", "lbw", "childnohealth",
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

#Header ----
header = dashboardHeader(title = "United Way Plots")


#SideBar -----
sidebar = dashboardSidebar(
  
     # SIDEITEM
     sidebarMenu(
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
                             #.... Add more ....
                             actionButton( inputId = "execute" ,
                                           label = "Submit")
                             )
                  )
     )
)



#Body ----
body = dashboardBody(uiOutput("MainGrid"))

"*****************************
           SERVER
*****************************"
server = function(input, output)
{
text2<-reactiveValues()
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
    
    #A simpler example
    variablenamelist$plotbutton[1]<-input$plotbutton1
    variablenamelist$plotbutton[2]<-input$plotbutton2
    
    #column of buttons is similar to variablename$list
    #This is what is going in usertext or line 

    #columnofbuttons$buttons[1] <- text2
    write.csv(variablenamelist,"Atemporyfile.csv")
    output$some_text = renderText({variablenamelist$plotbutton})
    
  })
output$some_text = renderText({input$plotbutton1})
getCWBI <- eventReactive(input$plotbutton1,{
  x <- user_text()
  # Eva this is the rest of the code you ignore that I will do tonight ----
  #Rest of the code
  # final <- lptest(final)
  final <- as.numeric(input$plotbutton)
  return("final")
})
print(getCWBI)
output$MainGrid = renderUI(verbatimTextOutput("some_text"))
}


"*****************************
           SHINY APP
*****************************"
shinyApp( ui = dashboardPage( header = header,
                              sidebar = sidebar, 
                              body = body),
          server = server)
