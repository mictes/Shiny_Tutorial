library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Calculator"),
  
  sidebarLayout(
    sidebarPanel(
      numericInput("num1", "Enter first number:", value = 0),
      numericInput("num2", "Enter second number:", value = 0),
      radioButtons("operation", "Choose operation:",
                   choices = list("Add" = "add",
                                  "Subtract" = "subtract",
                                  "Multiply" = "multiply",
                                  "Divide" = "divide"))
    ),
    
    mainPanel(
      textOutput("result")
    )
  )
)

# Define server logic
server <- function(input, output) {

  result <- reactive({
    # Handling division by zero with req()
    if (input$operation == "divide") {
      req(input$num2 != 0)
    }

    switch(input$operation,
           "add"      = input$num1 + input$num2,
           "subtract" = input$num1 - input$num2,
           "multiply" = input$num1 * input$num2,
           "divide"   = input$num1 / input$num2
    )
  })

  print(input$num1)
  # print(result())
  
  output$result <- renderText({
    paste("Result:", result())
  })

}

# Run the application
shinyApp(ui = ui, server = server)