# outputs practice

library(shiny)

ui <- fluidPage(
  textOutput("text"),
  verbatimTextOutput("code"),
  verbatimTextOutput("print"),
  tableOutput("static"),
  dataTableOutput("dynamic"),
  plotOutput("plot", width = "700px", height = "300px")
)

server <- function(input, output, session) {
  output$text <- renderText("Hello friend!") #only need curly braces when multiples lines
  output$code <- renderPrint(summary(1:10))
  output$print <- renderPrint("hello!")
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
  output$plot <- renderPlot(plot(1:5), res = 96, alt = "scatterplot")
}

shinyApp(ui, server)
