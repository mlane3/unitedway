"*************************************************
          UNITED WAY SHINY DASHBOARD
Date: February 28th 2018

*************************************************"
# Getting Started EXample --
library(ggplot2)
# NECESSARY PACKAGES ----------------------------------------------------
library(shiny)
library(flexdashboard)
source('model/UWCWBI_final.R')
source('model/coefficents.R')
library(readxl)
library(dplyr)
library(data.table)
library(plotly)
#Mike will to clean this

#setwd("~/R/unitedway")
df0 <- read_xlsx("2016 Original Data.xlsx")
names(df0) <- c('county','weave_ct2010','gradrate','ccrpi',
                'grade3','grade8','lbw','childnohealth',
                'childpoverty','povertyrate','housingburden','momsnohs',
                'collegerate','adultsnoedu','adultnohealth','unemployment')
minCWB_Z = min(df_index$CWB_Z) #-1.969282
maxCWB_Z = max(df_index$CWB_Z) #1.380706
df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 

# "===========================================
#                 HEADER -----
# ==========================================="
header = dashboardHeader(title = 'United Way App')

# "===========================================
#                 SIDEBAR
# ==========================================="
choices <- unique(c("overall",df0$county))
mynames <- names(df2)

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
                sep ="",step = .01, ticks = FALSE),
    sliderInput("ccrpi","College and Career Readiness",
                min = df2$ccrpi[1],max = df2$ccrpi[2],
                value = c(df2$ccrpi[3],df2$ccrpi[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("grade3","% Exceed 3rd Grade Reading",
                min = df2$grade3[1],max = df2$grade3[2],
                value = c(df2$grade3[3],df2$grade3[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("grade8","% Exceed 8th Grade Math",
                min = df2$grade8[1],max = df2$grade8[2],
                value = c(df2$grade8[3],df2$grade8[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("lbw","Low Weight Births",
                min = df2$lbw[1],
                max = df2$lbw[2],
                value = c(df2$lbw[1],df2$lbw[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("childnohealth","% Childern No Health Ins",
                min = df2$childnohealth[1],
                max = df2$childnohealth[2],
                value = c(df2$childnohealth[1],df2$childnohealth[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("childpoverty", "Child Poverty Rate",
                min = df2$childpoverty[1],
                max = df2$childpoverty[2],
                value = c(df2$childpoverty[1],df2$childpoverty[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("povertyrate",
                "Family Poverty Rate",
                min = df2$povertyrate[1],
                max = df2$povertyrate[2],
                value = c(df2$povertyrate[1],df2$povertyrate[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("housingburden",
                "% of Housing Burden",
                min = df2$housingburden[1],
                max = df2$housingburden[2],
                value = c(df2$housingburden[1],df2$housingburden[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("momsnohs",
                "% Moms with no Highschool Diploma",
                min = df2$momsnohs[1],
                max = df2$momsnohs[2],
                value = c(df2$momsnohs[1],df2$momsnohs[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("collegerate",
                "% Enrolled in Post Secondary Edu",
                min = df2$collegerate[1],
                max = df2$collegerate[2],
                value = c(df2$collegerate[3],df2$collegerate[2]), sep ="",step = .01, ticks = FALSE),
    sliderInput("adultsnoedu",
                "% Adults with no Highschool Diploma",
                min = df2$adultsnoedu[1],
                max = df2$adultsnoedu[2],
                value = c(df2$adultsnoedu[1],df2$adultsnoedu[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("adultnohealth",
                "% Adults wit no Health Ins",
                min = df2$adultnohealth[1],
                max = df2$adultnohealth[2],
                value = c(df2$adultnohealth[1],df2$adultnohealth[3]), sep ="",step = .01, ticks = FALSE),
    sliderInput("unemployment",
                "Unemployment Rate",
                min = df2$unemployment[1],
                max = df2$unemployment[2],
                value = c(df2$unemployment[1],df2$unemployment[3]), sep ="",step = .01, ticks = FALSE)
  )
)

# "===========================================
#                   BODY
# ==========================================="
body = dashboardBody(
  # source(coefficients) #to get pop.Coef
  fluidRow(gaugeOutput("gauge")),
  fluidRow(box(plotlyOutput("gauge2",height = 200)),
           box(plotlyOutput("gauge3",height = 200)),
           box(plotlyOutput("gauge4",height = 200)),
           box(plotlyOutput("gauge5",height = 200))),
  fluidRow(box(plotlyOutput("gauge6",height = 200)),
           box(plotlyOutput("gauge7",height = 200)),
           box(plotlyOutput("gauge8",height = 200)),
           box(plotlyOutput("gauge9",height = 200))),
  fluidRow(box(plotlyOutput("gauge10",height = 200)),
           box(plotlyOutput("gauge11",height = 200)),
           box(plotlyOutput("gauge12",height = 200)),
           box(plotlyOutput("gauge13",height = 200))
  ),
  fluidRow(box(plotlyOutput("gauge14",height = 200)),
          box(plotlyOutput("gauge15",height = 200)))
)

# "===========================================
# SERVER
# ==========================================="
#I am kind of stuck on this line.  How can I append the input in Rshiny??
# input <- append(input,df2) #How do I make reactive?

server = function(input, output){
  #Sifael I tried to make a reactive dashboard but I am really not sure how to hook this up
  # df3 <- reactive({
  #   df2 <- as.data.frame(read.csv("data/overall constrants.csv", skip = 2, row.names = 1)) 
  #   return(df2)
  # })
  # Actual Calculation and react step----
  # Sifael I still cannot get this to work right. 
  CWBI <- reactive({
    #From Mike to Sifael: this and the next line are the react steps I am stuck on
    #From Mike to Sifael: The second line replaces the values in df2 by the median of each slider
    df3 <- df2
    df3["Mean",] <- c(median(input$gradrate),median(input$ccrpi),median(input$grade3),
             median(input$grade8),median(input$lbw),median(input$childnohealth),
             median(input$childpoverty),median(input$povertyrate),median(input$housingburden),
             median(input$momsnohs),median(input$collegerate),median(input$adultsnoedu),
             median(input$adultnohealth),median(input$unemployment))
    myCoef <- pop.Coef(df0) #prep step from coefficents.R
    minCWB_Z = min(df_index$CWB_Z) # -1.969282 #prep step from coefficents.R
    maxCWB_Z = max(df_index$CWB_Z) # 1.380706 #prep step from coefficents.R
    #***We are optimizing this CwBZ
    df3["Mean",]
    Mean <- df3["Mean",]#*(1+input$final/100) #(1+input$final)/100 is a placeholder for optimizer) 
    print(CWBZ <- rowSums(myCoef$coefficients*Mean - myCoef$B)*100)
    CWBZ <- rowSums(myCoef$coefficients*Mean - myCoef$B)
    CWBI <- round((CWBZ - minCWB_Z)/(maxCWB_Z - minCWB_Z)*100,3)
    unname(CWBI)
    # CWBI <- median(input$gradrate)*(1+input$final/100)
  })
  output$gauge = renderGauge({
    #str(input$attribs)
    #str(input$lbw)
    #typeof(input$lbw)
    CWBI <- CWBI()
    gauge(CWBI, min = 0, max = 100,
          sectors = gaugeSectors(success = c(58.9, 100),danger = c(0, 58.9))
    )
  })
  # output$gauge2 = renderGauge({finalvalue = median(input$lbw)
  #   gauge(finalvalue, min = df2$lbw[1], max = df2$lbw[2],
  #         sectors = gaugeSectors(success = c(0, df2$lbw[4]),
  #                                danger = c(df2$lbw[4], df2$lbw[2])))})
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
  output$gauge3 <- renderPlotly({
    name = "College and Career Readiness"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$ccrpi)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$ccrpi[4]-df2$ccrpi[1])/(df2$ccrpi[2]-df2$ccrpi[1])
    scaled <- (df2$ccrpi[2]-rawvalue)/(df2$ccrpi[2]-df2$ccrpi[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$ccrpi[1])), paste(round(df2$ccrpi[4])), paste(round(df2$ccrpi[2]))),
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
  output$gauge4 <- renderPlotly({
    name = "% Exceed 3rd Grade Reading"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$grade3)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$grade3[4]-df2$grade3[1])/(df2$grade3[2]-df2$grade3[1])
    scaled <- (df2$grade3[2]-rawvalue)/(df2$grade3[2]-df2$grade3[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$grade3[1])), paste(round(df2$grade3[4])), paste(round(df2$grade3[2]))),
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
  output$gauge5 <- renderPlotly({
    name = "% Exceed 8th Grade Math"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$grade8)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$grade8[4]-df2$grade8[1])/(df2$grade8[2]-df2$grade8[1])
    scaled <- (df2$grade8[2]-rawvalue)/(df2$grade8[2]-df2$grade8[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$grade8[1])), paste(round(df2$grade8[4])), paste(round(df2$grade8[2]))),
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
  output$gauge6 <- renderPlotly({
    name = "Low Weight Births"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$lbw)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$lbw[4]-df2$lbw[1])/(df2$lbw[2]-df2$lbw[1])
    scaled <- (df2$lbw[2]-rawvalue)/(df2$lbw[2]-df2$lbw[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$lbw[1])), paste(round(df2$lbw[4])), paste(round(df2$lbw[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge7 <- renderPlotly({
    name = "Childern without Health Ins"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$childnohealth)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$childnohealth[4]-df2$childnohealth[1])/(df2$childnohealth[2]-df2$childnohealth[1])
    scaled <- (df2$childnohealth[2]-rawvalue)/(df2$childnohealth[2]-df2$childnohealth[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$childnohealth[1])), paste(round(df2$childnohealth[4])), paste(round(df2$childnohealth[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge8 <- renderPlotly({
    name = "Child Poverity Rate"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$childpoverty)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$childpoverty[4]-df2$childpoverty[1])/(df2$childpoverty[2]-df2$childpoverty[1])
    scaled <- (df2$childpoverty[2]-rawvalue)/(df2$childpoverty[2]-df2$childpoverty[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$childpoverty[1])), paste(round(df2$childpoverty[4])), paste(round(df2$childpoverty[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge9 <- renderPlotly({
    name = "povertyrate"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$povertyrate)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$povertyrate[4]-df2$povertyrate[1])/(df2$povertyrate[2]-df2$povertyrate[1])
    scaled <- (df2$povertyrate[2]-rawvalue)/(df2$povertyrate[2]-df2$povertyrate[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$povertyrate[1])), paste(round(df2$povertyrate[4])), paste(round(df2$povertyrate[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge10 <- renderPlotly({
    name = "housingburden"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$housingburden)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$housingburden[4]-df2$housingburden[1])/(df2$housingburden[2]-df2$housingburden[1])
    scaled <- (df2$housingburden[2]-rawvalue)/(df2$housingburden[2]-df2$housingburden[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$housingburden[1])), paste(round(df2$housingburden[4])), paste(round(df2$housingburden[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge11 <- renderPlotly({
    name = "momsnohs"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$momsnohs)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$housingburden[4]-df2$housingburden[1])/(df2$housingburden[2]-df2$housingburden[1])
    scaled <- (df2$housingburden[2]-rawvalue)/(df2$housingburden[2]-df2$housingburden[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$housingburden[1])), paste(round(df2$housingburden[4])), paste(round(df2$housingburden[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge12 <- renderPlotly({
    name = "collegerate"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$collegerate)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$collegerate[4]-df2$collegerate[1])/(df2$collegerate[2]-df2$collegerate[1])
    scaled <- (df2$collegerate[2]-rawvalue)/(df2$collegerate[2]-df2$collegerate[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$collegerate[1])), paste(round(df2$collegerate[4])), paste(round(df2$housingburden[2]))),
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
  output$gauge13 <- renderPlotly({
    name = "adultsnoedu"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$adultsnoedu)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$adultsnoedu[4]-df2$adultsnoedu[1])/(df2$adultsnoedu[2]-df2$adultsnoedu[1])
    scaled <- (df2$adultsnoedu[2]-rawvalue)/(df2$adultsnoedu[2]-df2$adultsnoedu[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$adultsnoedu[1])), paste(round(df2$adultsnoedu[4])), paste(round(df2$adultsnoedu[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge14 <- renderPlotly({
    name = "adultnohealth"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$adultnohealth)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$adultnohealth[4]-df2$adultnohealth[1])/(df2$adultnohealth[2]-df2$adultnohealth[1])
    scaled <- (df2$adultnohealth[2]-rawvalue)/(df2$adultnohealth[2]-df2$adultnohealth[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$adultnohealth[1])), paste(round(df2$adultnohealth[4])), paste(round(df2$adultnohealth[2]))),
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
      labels = c(name, "Good", "Bad"),
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
  output$gauge15 <- renderPlotly({
    name = "unemployment"
    h = 0.24
    k = 0.62
    r = 0.15
    rawvalue <- reactive({
      rawvalue <- median(input$unemployment)
      return(rawvalue)})
    rawvalue <- rawvalue()
    scaled0 <- (df2$unemployment[4]-df2$unemployment[1])/(df2$unemployment[2]-df2$unemployment[1])
    scaled <- (df2$unemployment[2]-rawvalue)/(df2$unemployment[2]-df2$unemployment[1])
    theta = scaled * 180
    theta = theta * pi / 180
    x = h + r*cos(theta)
    y = k + r*sin(theta)
    base_plot <- plot_ly(
      type = "pie",
      values = c(40, (1-scaled0)*30, scaled0*60, (1-scaled0)*30),
      labels = c("-",paste(round(df2$unemployment[1])), paste(round(df2$unemployment[4])), paste(round(df2$unemployment[2]))),
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
      labels = c(name, "Good", "Bad"),
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
shinyApp( ui = dashboardPage( skin = 'blue',
                              header = header,
                              sidebar = sidebar,
                              body = body) ,
          server = server)
