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
  "This plot shows the underlying salmon presence at the Gauntlet over a 365 day period.",
  plotOutput("salmon_species_plot"),
  "Manipulations Start Below:",
  fluidRow(
    column(6, numericInput("seals", "Number of Seals", value = 15, min = 1, max = 100))
  ),
  fluidRow(
    column(12, plotOutput("salmon_eaten_plot"))
  ),
  "Social Manipulations",
  fluidRow(
    column(6, numericInput("seals2copy", "Number of Seals to Copy", value = 0, min = 0, max = 100))
  ),
  fluidRow(
    column(12, plotOutput("social_plot"))
  )
)

server <- function(input, output, session){
  loop_results <- reactive({
    assembleTheLegos_shiny(num_seals_input = input$seals, 
                           seals_copy_input = input$seals2copy)
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
