library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num1", label = "Enter first number:", value = 0),
      numericInput("num2", label = "Enter second number:", value = 0),
      radioButtons("operation", label = "Choose operation:", choices = list(
        "Add"      = "add",
        "Subtract" = "subtract",
        "Multiply" = "multiply",
        "Divide"   = "divide"
      ))
    ),
    
    mainPanel(
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$result <- renderText({
    if (input$operation == "divide") {
      req(input$num2 != 0)
    }
    
    result <- switch(input$operation,
                     "add"      = input$num1 + input$num2,
                     "subtract" = input$num1 - input$num2,
                     "multiply" = input$num1 * input$num2,
                     "divide"   = input$num1 / input$num2
    )
    
    paste("Result:", result)
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)