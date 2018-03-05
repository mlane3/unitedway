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
  sidebar = dashboardSidebar(
    sidebarMenu( 
      menuItem( text = "Select County:",
                icon = icon("th"),
                selectInput(inputId = 'county',
                            label = "",
                            choices = choices)
                
      ),
      menuItem( text = "Select Variable",
                icon = icon("th"),
                selectInput(inputId = 'county',
                            label = "",
                            choices = mynames)
      ),
      sliderInput("final",
                  "% Improvement (will be replaced later)",
                  min = -20,
                  max = 20,
                  value = -3,
                  sep ="",step = 1,ticks = FALSE)
    ),
    #To Sifael I tried to put this in it own drop down but now luck :/
    sidebarMenu(
      sliderInput("gradrate","Gradrate Range",
                  min = df2$gradrate[1],max = df2$gradrate[2],
                  value = c(df2$gradrate[3],df2$gradrate[2]), #for the upper range
                  sep ="",step = .01, ticks = FALSE)
              
    )
  )
)

"===========================================
BODY
==========================================="
body = dashboardBody(
  # source(coefficients) #to get pop.Coef
  fluidRow(
    plotlyOutput("gauge2")
  )
)

"===========================================
                SERVER
==========================================="
server = function(input, output){
  output$gauge2 <- renderPlotly({
    name = "Graduation Rate"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$gradrate)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$gradrate[4]-df2$gradrate[1])/(df2$gradrate[2]-df2$gradrate[1])
    scaled <- (df2$gradrate[2]-rawvalue)/(df2$gradrate[2]-df2$gradrate[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
        type = "pie",
        values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
        labels = c("-",paste(round(df2$gradrate[1])), paste(round(df2$gradrate[4])), paste(round(df2$gradrate[2]))),
        rotation = 108,
        direction = "clockwise",
        sort = FALSE,
        hole = 0.4,
        textinfo = "label",
        textposition = "outside",
        hoverinfo = "none",
        domain = list(x = c(0, 0.48), y = c(0, 1)),
        marker = list(colors = c('rgb(255, 255, 255)', 'rgb(255, 255, 255)', 'rgb(255, 255, 255)','rgb(255, 255, 255)')),
        showlegend = FALSE)
        base_plot <- add_trace(
          base_plot,
          type = "pie",
          values = c(50, scaled0*50, (1-scaled0)*50),
          labels = c(name, "Bad", "Good"),
          rotation = 90,
          direction = "clockwise",
          hole = 0.3,
          textinfo = "label",
          textposition = "inside",
          hoverinfo = "none",
          domain = list(x = c(0, 0.48), y = c(0, 1)),
          marker = list(colors = c('rgb(255, 255, 255)', 'rgb(223,189,139)', 'rgb(223,162,103)')),
          showlegend= FALSE
        )
        a <- list(
          showticklabels = FALSE,
          autotick = FALSE,
          showgrid = FALSE,
          zeroline = FALSE)
        
        b <- list(
          xref = 'paper',
          yref = 'paper',
          x = 0.23,
          y = 0.45,
          showarrow = FALSE,
          text = paste(rawvalue))
        
        base_chart <- layout(
          base_plot,
          shapes = list(
            list(
              type = 'path',
              line = list(width = .5),
              path =  paste('M 0.235 0.5 L' , x, y, 'L 0.245 0.5 Z'),
              xref = 'paper',
              yref = 'paper',
              fillcolor = 'rgba(44, 160, 101, 0.5)'
            )
          ),
          xaxis = a,
          yaxis = a,
          annotations = b
        )
      })
}


"===========================================
                RUNAPP
==========================================="
shinyApp( ui = dashboardPage( skin = 'yellow',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)