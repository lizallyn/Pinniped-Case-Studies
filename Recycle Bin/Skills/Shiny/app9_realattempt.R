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
  numericInput("seals", "Number of Seals", value = 25, min = 1, max = 100),
  plotOutput("num_seals_plot"),
  plotOutput("salmon_species_plot"),
)

server <- function(input, output, session){
  loop_results <- reactive({
    assembleTheLegos_shiny(num_seals_input = input$seals)
  })

  output$num_seals_plot <- renderPlot({
    loop_results()[[1]]
  })
  output$salmon_species_plot <- renderPlot({
    loop_results()[[2]]
  })
  
}

shinyApp(ui, server)
