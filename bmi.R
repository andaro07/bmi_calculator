library(shiny)
library(shinythemes)
library(markdown)
library(data.table)
library(RCurl)
library(randomForest)

# Read data
weather <- read.csv(text = getURL("https://raw.githubusercontent.com/dataprofessor/data/master/weather-weka.csv") )

weather$outlook = as.factor(weather$outlook)
weather$play = as.factor(weather$play)

# Build model
model <- randomForest(play ~ ., data = weather, ntree = 500, mtry = 4, importance = TRUE)

####################################
# User Interface                   #
####################################
ui <- fluidPage(theme = shinytheme("cyborg"),
                navbarPage(div("BMI Calculator For Adults", style="color:#F09953"),
                           
                           tabPanel(div("Home", style = "color:lightblue"),
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      sliderInput("height", 
                                                  label = "Height (cm)", 
                                                  value = 175, 
                                                  min = 40, 
                                                  max = 250),
                                      sliderInput("weight", 
                                                  label = "Weight (kg)", 
                                                  value = 70, 
                                                  min = 20, 
                                                  max = 100),
                                      
                                      actionButton("submitbutton1", 
                                                   "Submit", 
                                                   class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output1')), # Status/Output Text Box
                                      verbatimTextOutput('contents1'),
                                      tableOutput('tabledata1') # Results table
                                    ) # mainPanel()
                                    
                           ), #tabPanel(), Home
                           
                           tabPanel(div("About", style="color:#00FFFF"), 
                                    titlePanel("About"), 
                                    div(includeMarkdown("bmi_about.md"), 
                                        align="justify")
                           ), #tabPanel(), About
                           
                           tabPanel(div('Play Golf?', style="color:#ff66ff"),
                                    
                                    # Input values
                                    sidebarPanel(
                                      HTML("<h3>Input parameters</h3>"),
                                      
                                      selectInput("outlook", label = "Outlook:", 
                                                  choices = list("Sunny" = "sunny", "Overcast" = "overcast", "Rainy" = "rainy"), 
                                                  selected = "Rainy"),
                                      sliderInput("temperature", "Temperature:",
                                                  min = 64, max = 86,
                                                  value = 70),
                                      sliderInput("humidity", "Humidity:",
                                                  min = 65, max = 96,
                                                  value = 90),
                                      selectInput("windy", label = "Windy:", 
                                                  choices = list("Yes" = "TRUE", "No" = "FALSE"), 
                                                  selected = "TRUE"),
                                      
                                      actionButton("submitbutton2", "Submit", class = "btn btn-primary")
                                    ),
                                    
                                    mainPanel(
                                      tags$label(h3('Status/Output')), # Status/Output Text Box
                                      verbatimTextOutput('contents2'),
                                      tableOutput('tabledata2') # Prediction results table
                                      
                                    ))
                           
                ) # navbarPage()
) # fluidPage()


####################################
# Server                           #
####################################
server <- function(input, output, session) {
  
  # Input Data
  datasetInput1 <- reactive({  
    
    bmi <- input$weight/( (input$height/100) * (input$height/100) )
    bmi <- data.frame(bmi)
    names(bmi) <- "BMI"
    print(bmi)
    
  })
  
  # Status/Output Text Box
  output$contents1 <- renderPrint({
    if (input$submitbutton1>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata1 <- renderTable({
    if (input$submitbutton1>0) { 
      isolate(datasetInput1()) 
    } 
  })
  
  
  datasetInput2 <- reactive({  
    
    # outlook,temperature,humidity,windy,play
    df <- data.frame(
      Name = c("outlook",
               "temperature",
               "humidity",
               "windy"),
      Value = as.character(c(input$outlook,
                             input$temperature,
                             input$humidity,
                             input$windy)),
      stringsAsFactors = FALSE)
    
    play <- "play"
    df <- rbind(df, play)
    input <- transpose(df)
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = FALSE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    test$outlook <- factor(test$outlook, levels = c("overcast", "rainy", "sunny"))
    
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents2 <- renderPrint({
    if (input$submitbutton2>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata2<- renderTable({
    if (input$submitbutton2>0) { 
      isolate(datasetInput2()) 
    } 
  })
  
}


####################################
# Create Shiny App                 #
####################################
shinyApp(ui = ui, server = server)
