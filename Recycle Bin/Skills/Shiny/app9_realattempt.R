# trying a first shiny app with real model
# MAy 2024

library(shiny)
library(ggplot2)

# source("Functions/Prep_data_for_Salmon_functions.R")
# source("Functions/Prep_data_for_Harvest_functions.R")
# 
# source("Functions/makeArray.R")
# source("Functions/createHarvestPlan.R")
# 
# source("Functions/Shiny_set_pars.R")
# 
# source("Functions/salmonSpeciesUpdate.R")
# source("Functions/decideForagingDestination.R")
# source("Functions/collusion.R")
# source("Functions/rungeKutta.R")
# source("Functions/getHarvested.R")
# source("Functions/learnX.R")
# source("Functions/learnY.R")


source("Functions/assembleTheLegos_shiny.R")
source("Functions/prepForPlots.R")

ui <- fluidPage(
  titlePanel("Pinniped Behavioral Responses and Learning in the Gauntlet"),
  "This plot shows the underlying salmon presence at the Gauntlet over a 365 day period.",
  plotOutput("salmon_species_plot"),
  "Manipulations Start Below:",
  fluidRow(
    column(6, numericInput("seals", "Number of Seals", value = 15, min = 1, max = 100)),
    column(6, numericInput("seals2copy", "Number of Seals to Copy", value = 1, min = 0, max = 100))
  ),
  fluidRow(
    column(12, plotOutput("salmon_eaten_plot"))
  ),
  "Foraging Learning Parameters",
  fluidRow(
    column(3, sliderInput("w", "w", value = 0.1, min = 0, max = 1, step = 0.05)),
    column(3, sliderInput("x_intercept", "Intercept of x --> P_x", 
                          value = 0.03, min = 0, max = 1.2, step = 0.01)),
    column(3, sliderInput("step", "Speed of Learning", 
                          value = 0.25, min = 0, max = 1, step = 0.05)),
    column(3, sliderInput("decay", "Speed of Forgetting", 
                          value = 0.05, min = 0, max = 1, step = 0.01))
  ),
  "Consumption Parameters",
  fluidRow(
    column(6, sliderInput("Cmax", "Maximum consumption", value = 1, min = 0, max = 10, step = 0.5)),
    column(6, sliderInput("alpha", "Search and Capture Rate",
                          value = 0.1, min = 0, max = 1, step = 0.01))
  ),
  fluidRow(
    column(12, plotOutput("X"))
  )
)

server <- function(input, output, session){
  loop_results <- reactive({
    assembleTheLegos_shiny(num_seals_input = input$seals, 
                           seals_copy_input = input$seals2copy,
                           w_input = input$w,
                           x_intercept_input = input$x_intercept,
                           step_input = input$step,
                           decay_input = input$decay,
                           cmax_input = input$Cmax,
                           alpha_input = input$alpha)
  })

  output$num_seals_plot <- renderPlot({
    loop_results()[["Seals_G"]]
  })
  output$salmon_species_plot <- renderPlot({
    loop_results()[["Salmon_G"]]
  })
  output$prob_gauntlet_plot <- renderPlot({
    loop_results()[["Prob_G"]]
  })
  output$eaten_per_seal_plot <- renderPlot({
    loop_results()[['Seals_Eaten']]
  })
  output$salmon_eaten_plot <- renderPlot({
    loop_results()[['Salmon_Eaten']]
  })
  output$social_plot <- renderPlot({
    loop_results()[['Social']]
  })
  
}

shinyApp(ui, server)
