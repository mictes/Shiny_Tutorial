# Michael Teske
# 2024-04-04
# v. 0.1.5

# - - - - - - - - - - - - - - - 
# Setup
# - - - - - - - - - - - - - - -

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(ggbeeswarm)
library(ggpubr)
library(DT)

options(bitmapType='cairo')


# - - - - - - - - - - - - - - -
# Data
# - - - - - - - - - - - - - - -

data(iris)


# - - - - - - - - - - - - - - -
# Functions
# - - - - - - - - - - - - - - -

# Filters and returns rows from a dataset dset 
# where the x and y coordinates fall within the boundaries defined by a brush object, 
# based on data extracted from a specified layer
find_dots <- function(dset, plot, layer, brush)
{
  tmp <- layer_data(plot, layer)
  idx <- tmp$x >= brush$xmin & tmp$x <= brush$xmax & tmp$y >= brush$ymin & tmp$y <= brush$ymax
  return(dset[idx, ])
}

# Create a list of comparisons (between two neighbouring species)
pairwise_comp <- function(conditions) {
  conditions.clean  <- unique(as.character(conditions))
  comparisons <- combn(conditions.clean, 2, simplify=F)
  return(comparisons)
}


# - - - - - - - - - - - - - - -
# Shiny
# - - - - - - - - - - - - - - -

sidebar <- dashboardSidebar(
  selectizeInput(
    inputId   = 'species',
    label     = 'Species',
    choices   = levels(iris$Species),
    selected  = levels(iris$Species),
    multiple  = T,
    options   = list(
      plugins   = list('remove_button', 'drag_drop')
    )
  ),
  sliderInput(
    inputId   = 'size',
    label     = 'Dot size',
    min       = 0.5,
    max       = 5,
    step      = 0.5,
    value     = 1
  ),
  checkboxInput(
    inputId   = 'statistics',
    label     = 'Wilcoxon test',
    value     = T
  ),
  downloadButton(
    outputId  = 'save_violinplot',
    label     = 'Save plot',
    icon      = shiny::icon('download')
  ),
  actionButton(
    inputId   = 'help',
    label     = 'Help',
    icon      = shiny::icon('question-circle')
  ),
  div(
    id        = 'credits',
    HTML('
      <a href="https://github.com/mictes", target="_blank">
        <i class="fa-brands fa-github"></i> mictes, 2024
      </a>
    ')
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel='shortcut icon', href='facivon.png'),
    includeCSS('https://cdn.jsdelivr.net/npm/intro.js@7.2.0/minified/introjs.min.css'),
    # It is also possible to add CSS directly, although it is usually more convenient to use external files (same for JavaScript)
    tags$style("
      aside .btn {
        margin: 12pt;
        color: #000000 !important;
      }
      #credits {
        margin: 12pt;
      }
      #credits a {
        color: #777;
      }
    ")
  ),
  fluidRow(
    box(
      width       = 12, 
      status      = 'success', 
      title       = 'Violinplot',
      plotOutput(
        outputId  = 'violinplot',
        dblclick  = 'plot_dblclick',
        brush     = brushOpts(
          id          = 'plot_brush',
          resetOnNew  = T
        )
      )
    )
  ),
  fluidRow(
    box(
      width       = 12, 
      status      = 'primary', 
      title       = 'Selected dots',
      dataTableOutput(outputId='table_selected')
    )
  ),
  includeScript('https://cdn.jsdelivr.net/npm/intro.js@7.2.0/intro.min.js'),
  tags$script("
    // create an array with objects describing each step in the guided tour 
    var Steps = [
      {
        element:  '#species-label',
        intro:    'Select the iris species that you would like to include in the plot. You can drag and drop the items to display the species in the desired order.',
        position: 'right'
      },
      {
        element:  '#size-label',
        intro:    'You can adjust the size of the dots in the plot.',
        position: 'right'
      },
      {
        element:  '#statistics',
        intro:    'Show significances (Wilcoxon-Mann-Whitney-Test).',
        position: 'right'
      },
      {
        element:  '#save_violinplot',
        intro:    'Download the plot as PDF file.',
        position: 'right'
      },
      {
        element:  '#violinplot',
        intro:    'Click and draw an area to select dots. The data of the selected dots is displayed in a table below and can be downloaded as a CSV file. Double-click on the area to zoom into the selection, double-click again to zoom out.'
      // position: 'top'
      }
    ];
              
    // initialize an introjs instance          
    var intro = introJs();
  
    // load data
    intro.setOptions({steps: Steps});

    // start intro onclick on help button
    document.querySelector('#help').addEventListener('click', function(e) {
      intro.start();
    });
  ")
)

ui <- dashboardPage(
  skin    = 'blue',
  dashboardHeader(title = 'Shiny Tutorial'),
  sidebar,
  body
)

server <- function(input, output, session) {
  # Reactively filter the iris dataset based on the selected species.
  data_subset <- reactive({
    req(input$species)
    iris_subset         <- filter(iris, Species %in% input$species)
    iris_subset$Species <- factor(iris_subset$Species, levels=input$species)
    iris_subset         <- iris_subset[order(iris_subset$Species), ]
  })

  # Replace brushedPoints()
  # Reactively identify the dots selected in the plot using the brush.
  # 2 corresponds to the layer of the geom_quasirandom function
  selected  <- reactive({
    req(input$plot_brush)
    find_dots(data_subset(), violinplot(), 2, input$plot_brush)
  })

  # Initialize zoom
  zoom  <- reactiveValues(x = NULL, y = NULL)

  # Monitor double-clicks on the plot
  # When a double-click on the plot is detected and a brush is present, set zoom to the coordinates of the brush
  observeEvent(input$plot_dblclick, {
    if (!is.null(input$plot_brush)) {
      zoom$x <- c(input$plot_brush$xmin, input$plot_brush$xmax)
      zoom$y <- c(input$plot_brush$ymin, input$plot_brush$ymax)
    } else {
      zoom$x <- NULL
      zoom$y <- NULL
    }
  })

  # Reactively generates the violin plot based on the selected species, dot size, plot zoom and statistics 
  violinplot  <- reactive({
    pl  <- ggplot(data_subset(), aes(x=Species, y=Petal.Length)) + 
      geom_violin(aes(fill=Species)) + 
      geom_quasirandom(size=input$size) + 
      geom_boxplot(width=0.1) +
      coord_cartesian(xlim=zoom$x, ylim=zoom$y, expand=T) +   # When zoom is NULL, the coordinates of the plot are unchanged (no zoom/zoom-out)
      theme(aspect.ratio=3/4)

    if(input$statistics == T && !is.null(data_subset())) {
      pl  <- pl +
        stat_compare_means(
          comparisons = pairwise_comp( unique(data_subset()$Species)),
          method      = 'wilcox.test',
          label       = 'p.signif'
        )
    }

    pl
  })

  output$violinplot <- renderPlot({
    violinplot()
  })

  output$save_violinplot <- downloadHandler(
    filename    = 'plot.pdf',
    content     = function(file) {
      ggsave(file, plot=violinplot(), width=297, height=210, unit='mm')
    }
  )

  output$table_selected <- renderDataTable(
    selected(),
    extensions  = 'Buttons',
    options     = list(
      pageLength  = 10,
      scrollX     = T,
      pagingType  = 'simple_numbers',
      dom         = 'Bfrtip', 
      buttons     = list(
        list(extend = 'csv', filename = 'selected_dots')
      ) 
    ),
    rownames = F,
    server   = F # necessary to download whole table
  )
}

shinyApp(
  ui      = ui, 
  server  = server
)
