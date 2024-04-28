library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      # input here
      # check: https://shiny.posit.co/r/gallery/widgets/widget-gallery/
    ),
    
    mainPanel(
     # output here
     # check: https://github.com/rstudio/cheatsheets/blob/main/shiny.pdf
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$result <- renderText({
    num1 <- 2
    num2 <- 1
    
    result <- switch(input$operation,
           "add"      = num1 + num2,
           "subtract" = num1 - num2,
           "multiply" = num1 * num2,
           "divide"   = num1 / num2
    )

    paste("Result:", result)
  })
}

# Run the application
shinyApp(ui = ui, server = server)