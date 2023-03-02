library(shiny)

Iris <- read.csv("Iris.csv")
stockprice <- read.csv("stockprice.csv", stringsAsFactors = FALSE)
usedcars <- read.csv("usedcars.csv", stringsAsFactors = FALSE)


ui <- fluidPage(
  titlePanel("Uploading Files"),
  h2(textOutput("currentTime")),
  tabsetPanel(
    tabPanel("Viewing Files",
             sidebarPanel(
               selectInput(inputId = "dataset",
                           label = "Choose a dataset:",
                           choices = c("Iris", "stockprice", "usedcars")),
               numericInput(inputId = "obs",
                            label = "Number of observations to view:",
                            value = 10)
             ),
             mainPanel(
               verbatimTextOutput("summary"),
               tableOutput("view")
             )
    ),
    tabPanel("Uploading Files",
      fileInput("file1", "Choose CSV File",
                multiple = TRUE,
                accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
      tags$hr(),
      checkboxInput("header", "Header", TRUE),
      radioButtons("sep", "Separator",
                   choices = c(Comma = ",",
                               Semicolon = ";",
                               Tab = "\t"),
                   selected = ","),
      radioButtons("quote", "Quote",
                   choices = c(None = "",
                               "Double Quote" = '"',
                               "Single Quote" = "'"),
                   selected = '"'),
      tags$hr(),
      radioButtons("disp", "Display",
                   choices = c(Head = "head",
                               All = "all"),
                   selected = "head")
    ,
    mainPanel(
      tableOutput("contents")
    )
    ),
    tabPanel("Downloading File",
             selectInput("dataset", "Choose a dataset:",
                         choices = c("Iris", "stockprice", "usedcars")),
             downloadButton("downloadData", "Download")
    ,
    mainPanel(
      tableOutput("table")
    )
    )
  )
)


server <- function(input, output, session) {
  output$currentTime <- renderText({
    invalidateLater(1000, session)
    paste("The current time is", Sys.time())
  })
  
  
  
  datasetInput <- reactive({
    switch(input$dataset,
           "year" = year,
           "model" = model,
           "price" = price)
  })
  
  output$summary <- renderPrint({
    dataset <- datasetInput()
    summary(dataset)
  })
  
  output$view <- renderTable({
    head(datasetInput(), n = input$obs)
  })
  
  
  
  
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  
  datasetInput <- reactive({
    switch(input$dataset,
           "Iris" = Iris,
           "stockprice" = stockprice,
           "usedcars" = usedcars)
  })
  output$table <- renderTable({
    datasetInput()
  })
  output$downloadData <- downloadHandler(
    filename = function(){
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file){
      write.csv(datasetInput(), file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)
shiny::runApp()
