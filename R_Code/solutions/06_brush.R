library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggbeeswarm)

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "My Shiny App"
  ),
  dashboardSidebar(
    sliderInput("dotsize", "Dot Size", min=0.1, max=5, value=1, step=0.1),
    downloadButton(
      outputId  = 'save_violinplot',
      label     = 'Save plot',
      icon      = shiny::icon('download')
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS('04_includeCSS.css')
    ),
    fluidRow(
      box(
        width       = 12, 
        status      = 'success', 
        title       = 'Plot',
        plotOutput(
          "plot",
          brush = "plot_brush"
        )
      ),
    ),
    fluidRow(
      box(
        width       = 12, 
        status      = 'primary', 
        title       = 'Data',
        dataTableOutput(outputId='table_selected')
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  violinplot <- reactive({
    data(iris)
    
    ggplot(iris, aes(x=Species, y=Petal.Length)) + 
      geom_violin(aes(fill=Species)) + 
      geom_point(size=input$dotsize) + 
      geom_boxplot(width=0.1) +
      theme(aspect.ratio=3/4)
  })

  selected  <- reactive({
    brushedPoints(iris, input$plot_brush)
  })
  
  output$plot <- renderPlot({
    violinplot()
  })
  
  output$save_violinplot <- downloadHandler(
    filename    = 'plot.pdf',
    content     = function(file) {
      ggsave(file, plot=violinplot(), width=297, height=210, unit='mm')
    })

  output$table_selected <- renderDataTable(
    selected()
  )
}

# Run the application
shinyApp(ui = ui, server = server)