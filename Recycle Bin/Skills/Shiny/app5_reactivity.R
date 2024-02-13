# reactivity

library(shiny)

ui <- fluidPage(
  numericInput("count", label = "Number of values", value = 100)
)

server <- function(input, output, session) {
  input$count <- 10  
}

shinyApp(ui, server)

