"**********************************************

    UNITED WAY MULTIPLE LOTS AND BUTTON IDEA
              APRIL 14th 2018
**********************************************"

# NECESSARY PACKAGES
library(shiny)
library(shinydashboard)

library(ggplot2)

"*****************************
           HEADER
*****************************"
header = dashboardHeader(title = "United Way Plots")


"*****************************
           SIDEBAR
*****************************"
sidebar = dashboardSidebar(
  
     # SIDEITEM
     sidebarMenu(
       
                  menuItem( text = "Multiple Plots",
                            tabName = "all_plots" ),
                  
                  menuItem( text = "Button Example",
                            tabName = "example_button" )
       
       )
)



"*****************************
           BODY
*****************************"
body = dashboardBody(
  
      tabItems( 
        
                tabItem( tabName = "all_plots",
                         fluidRow( column(4,
                                          box(width=12, plotOutput("plot_1"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_2"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_3")))),
                         fluidRow( column(4,
                                          box(width=12, plotOutput("plot_4"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_5"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_6")))),
                         fluidRow( column(4,
                                          box(width=12, plotOutput("plot_7"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_8"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_9")))),
                         fluidRow( column(4,
                                          box(width=12, plotOutput("plot_10"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_11"))),
                                   column(4,
                                          box(width=12, plotOutput("plot_12"))))),
                
                tabItem( tabName = "example_button",
                         fluidRow(
                                   column( width = 12,
                                           box( width = 12,
                                                title = "Example Button",
                                                textInput( inputId = "text",
                                                           label = "Enter Some Text: "),
                                                actionButton( inputId = "execute" ,
                                                              label = "Submit")),
                                                verbatimTextOutput("some_text"))
                         )
                         )
        
        )

)


"*****************************
           SERVER
*****************************"
server = function(input, output)
{
  
  # GGPLOTS - MANUAL CONCEPT
  output$plot_1 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_2 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_3 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_4 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_5 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_6 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_7 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_8 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_9 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_10 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_11 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_12 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  output$plot_13 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point()  })
  output$plot_14 = renderPlot({ ggplot(mpg, aes(displ, hwy, colour = class)) + geom_point() })
  
  
  # BUTTON
  user_text = eventReactive(input$execute, {
    
    input$text
  })

  output$some_text = renderText({ user_text() })
  
  
}

"*****************************
           SHINY APP
*****************************"
shinyApp( ui = dashboardPage( header = header,
                              sidebar = sidebar, 
                              body = body),
          server = server)
