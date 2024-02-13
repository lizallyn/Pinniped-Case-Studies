# This is mostly input formatting options examples
# more practice
# https://mastering-shiny.org/basic-ui.html
# more on sliders, including how to add looping animations
# https://shiny.posit.co/r/articles/build/sliders/


library(shiny)

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")
ui <- fluidPage(
  textInput("name", "What's your name?", placeholder = "your name"),
  passwordInput("password", "What's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  numericInput("num", "Number one", value = 0, min = 0, max = 100),
  sliderInput("num2", "Date Attempt", value = as.Date("2020-02-02"), 
              min = as.Date("2020-01-02"), max = as.Date("2020-03-02"), timeFormat = "%Y-%m-%d"),
  sliderInput("num3", "Animation Slider", value = 20, min = 0, max = 100, step = 5,
              animate = animationOptions(interval = 500, loop = T)),
  sliderInput("rng", "Range", value = c(10, 20), min = 0, max = 100),
  dateInput("dob", "When were you born?"),
  dateRangeInput("holiday", "When do you want to go on vacation next?"),
  selectInput("state", "What's your favourite state?", state.name),
  radioButtons("animal", "What's your favourite animal?", animals),
  radioButtons("rb", "Choose one:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry", "happy", "sad")
  ),
  "Should I shutdown?",
  checkboxInput("shutdown", "Yes"),
  selectInput(
    "state", "What's your favourite state?", state.name,
    multiple = TRUE),
  checkboxGroupInput("animal", "What animals do you like?", animals),
  actionButton("drink", "Drink me!", icon = icon("cocktail")),
  fluidRow(
    actionButton("click", "Click me!", class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
  ),
  fluidRow(
    actionButton("eat", "Eat me!", class = "btn-block")
  )
)

server <- function(input, output, session) {
}

shinyApp(ui, server)
