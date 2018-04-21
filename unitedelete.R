gaugeID = function(variable) {
  # AM Angular Gauge
  # bands = data.frame(start = c(START,value), end = c(value, END), 
  #                    color = c("#00CC00", "#ea3838"),
  #                    stringsAsFactors = FALSE)
  START = round(df2[1, variable],.1)
  value = round(df2[3, variable],.1)
  END = round(df2[2, variable],.1)
  #PURU_COMMENT_START
  # Check if the variable is gradrate or ccrpi or grade3 or grade8 or collegerate use RED to GREEN, if not SWAP color
  #PURU_COMMENT_END
  if((variable == 'gradrate') || (variable == 'ccrpi') || (variable == 'grade3') || (variable == 'grade8') || (variable == 'collegerate'))
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
  amAngularGauge(x = df2[4,variable], #median(as.vector(input$metric))
                 start = START, end = END,
                 main = variable, bands = bands)
}


myplots <- for(column in 1:14){
  
}


# output$GaugePlot = renderAmCharts({
#   
# }) 
