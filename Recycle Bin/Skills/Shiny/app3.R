# more practice
# https://mastering-shiny.org/basic-ui.html
# more on sliders, including how to add looping animations
# https://shiny.posit.co/r/articles/build/sliders/


library(shiny)

ui <- fluidPage(
  textInput("name", "What's your name?"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Number two", value = 50, min = 0, max = 100),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100)
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
