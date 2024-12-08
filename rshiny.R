library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)

ui <- dashboardPage(
  dashboardHeader(title = "Visualisation de Données"),
  dashboardSidebar(
    fileInput("file", "Sélectionnez un fichier CSV"),
    tags$hr(),
    h4("Choisissez les variables"),
    selectInput("xVar", "Variable X", ""),
    selectInput("yVar", "Variable Y", ""),
    tags$hr(),
    checkboxInput("logScale", "Échelle logarithmique", value = FALSE)
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Graphique",
        width = 12, height = 400,
        plotlyOutput("scatterPlot")
      )
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    validate(need(input$file, "Veuillez charger un fichier CSV"))
    read.csv(input$file$datapath, header = TRUE)
  })
  
  observe({
    req(data())
    updateSelectInput(session, "xVar", choices = names(data()), selected = names(data())[1])
    updateSelectInput(session, "yVar", choices = names(data()), selected = names(data())[2])
  })
  
  output$scatterPlot <- renderPlotly({
    req(data())
    
    p <- plot_ly(data(), x = ~get(input$xVar), y = ~get(input$yVar), type = "scatter", mode = "markers") %>%
      layout(
        xaxis = list(title = input$xVar),
        yaxis = list(title = input$yVar),
        title = "Scatter Plot"
      )
    
    if (input$logScale) {
      p <- p %>% layout(yaxis = list(type = "log"))
    }
    
    p
  })
  
}

shinyApp(ui, server)

