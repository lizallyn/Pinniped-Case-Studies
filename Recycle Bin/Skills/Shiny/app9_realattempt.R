# trying a first shiny app with real model
# MAy 2024

library(shiny)

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

ui <- fluidPage(
  numericInput("seals", "Number of Seals", value = 25, min = 1, max = 100),
  plotOutput("num_seals_plot"),
  numericInput("day_of_year", "Day of the Year", value = 200, min = 1, max = 365),
  tableOutput("salmon_species_table")
)

server <- function(input, output, session){
  num_seals <- reactive({
    input$seals
  })

  output$num_seals_plot <- renderPlot({
    assembleTheLegos_shiny(num_seals_input = num_seals())
  })
  
  
  day_of_year <- reactive({
    Daily_fish[input$day_of_year,]
  })
  output$salmon_species_table <- renderTable({
    day_of_year()
  })
  output$salmon_species_plot <- renderPlot({
    
  })
}

shinyApp(ui, server)
