##********************
#Script to Create Buttons for the United Way project
##********************

library(shiny)

#Initialize variable 'i' to 1

i = 1

#For loop to create 14 buttons
for(i in 14) {
  
  tabItem( tabName = "plotbutton1",
           fluidRow(
             column( width = 12,
                     box( width = 12,
                          title = "Plot Button",
                          textInput( inputId = "text",
                                     label = "Enter Some Text: "),
                          actionButton( inputId = "execute" ,
                                        label = "Submit")),
                     verbatimTextOutput("some_text"))
           )
  )

  #increment the counter by 1 and print line when done
  i = i + 1
  
  print("Button Created") 
  
  }