setwd("C:/Users/chara/OneDrive/Desktop/DataScience")
library(shiny)
library(data.table)
library(randomForest)
library(caret)

#load the model
model2=readRDS("model2.rds")
model2
#user interface
ui = pageWithSidebar(
  
  # Page header
  headerPanel('Rice Classification'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3("Input parameters")),
    numericInput("Area", 
                 label = "Area of the rice", 
                 value = 1000),
    numericInput("MajorAxisLength", 
                 label = "Major Axis Length of the Rice", 
                 value = 92.5),
    numericInput("Extent", 
                 label = "Extent of the Rice", 
                 value = 0.7),
    numericInput("Roundness", 
                 label = "Roundness of the rice", 
                 value = 0.8),
    numericInput("AspectRation", 
                 label = "Aspect ration of the rice", 
                 value = 1.3),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(
      Name = c("Area of the rice",
               "Major Axis Length of the Rice",
               "Extent of the Rice",
               "Roundness of the rice",
               "Aspect ration of the rice"),
      Value = as.numeric(c(input$Area,
                             input$MajorAxisLength,
                             input$Extent,
                             input$Roundness,
                             input$AspectRation)),
      stringsAsFactors = FALSE)
    
    Class <- 0
    df = rbind(df, Class)
    input = transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    Output <- data.frame(Prediction=predict(model2,test), round(predict(model2,test,type="prob"), 2))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

#shinyapp

shinyApp(ui = ui, server = server)
