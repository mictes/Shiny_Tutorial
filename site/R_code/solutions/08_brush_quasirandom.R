library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggbeeswarm)

data(iris)

# Filters and returns rows from a dataset dset 
# where the x and y coordinates fall within the boundaries defined by a brush object, 
# based on data extracted from a specified layer
find_dots <- function(dset, plot, layer, brush) {
  tmp <- layer_data(plot, layer)
  idx <- tmp$x >= brush$xmin & tmp$x <= brush$xmax & tmp$y >= brush$ymin & tmp$y <= brush$ymax
  return(dset[idx, ])
}

# Define UI
ui <- dashboardPage(
  dashboardHeader(
    title = "My Shiny App"
  ),
  dashboardSidebar(
    sliderInput("dotsize", label = "Dot Size", min = 0.1, max = 5, value = 1, step = 0.1),
    downloadButton(
      outputId  = 'save_violinplot',
      label     = 'Save plot',
      icon      = shiny::icon('download')
    )
  ),
  dashboardBody(
    tags$head(
      includeCSS('05_includeCSS.css')
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
    ggplot(iris, aes(x=Species, y=Petal.Length)) + 
      geom_violin(aes(fill=Species)) + 
      geom_quasirandom(size=input$dotsize) + 
      geom_boxplot(width=0.1) +
      theme(aspect.ratio=3/4)
  })

  selected  <- reactive({
    req(input$plot_brush)
    find_dots(iris, violinplot(), 2, input$plot_brush)
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