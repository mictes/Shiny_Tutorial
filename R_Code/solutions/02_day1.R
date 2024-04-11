library(shiny)
library(tidyverse)
library(ggbeeswarm)

# Define UI
ui <- fluidPage(
  titlePanel("My Shiny App"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("dotsize", "Dot Size", min=0.1, max=5, value=1, step=0.1),
      downloadButton(
        outputId  = 'save_violinplot',
        label     = 'Save plot',
        icon      = shiny::icon('download')
      )
    ),
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  violinplot <- reactive({
    data(iris)
    
    ggplot(iris, aes(x=Species, y=Petal.Length)) + 
      geom_violin(aes(fill=Species)) + 
      geom_quasirandom(size=input$dotsize) + 
      geom_boxplot(width=0.1)
  })
  
  output$plot <- renderPlot({
    violinplot()
  })
  
  output$save_violinplot <- downloadHandler(
    filename    = 'plot.pdf',
    content     = function(file) {
      ggsave(file, plot=violinplot(), width=297, height=210, unit='mm')
    })
}

# Run the application
shinyApp(ui = ui, server = server)