"*************************************************
          UNITED WAY SHINY DASHBOARD
Date: February 28th 2018

*************************************************"

# NECESSARY PACKAGES
library(shiny)
library(shinydashboard)

"===========================================
HEADER
==========================================="
header = dashboardHeader(title = 'United Way App')


"===========================================
SIDEBAR
==========================================="
sidebar = dashboardSidebar(
  
  sidebarMenu( 
    menuItem( text = "Select County:",
              icon = icon("th"),
              selectInput(inputId = 'county',
                          label = "",
                          choices = c("Michael",
                                      "Purush",
                                      "Sifael"))
              
    )
    
    
  )
  
)

"===========================================
BODY
==========================================="
body = dashboardBody()


"===========================================
                SERVER
==========================================="
server = function(input, output){}


"===========================================
                RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'yellow',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)