library(shiny)
# remove(list=ls())
library(ggplot2)
library(reshape2)
library(devtools)
library(choroplethrZip)
library(stringr)
library(gsubfn)
library(sf)
library(spdep)
library(data.table)
library(dplyr)
library(tidyr)

source("Tier Trend Dashboard.r")
source("Location Dashboard.r")
source("barChart.r")
source("Literature Review.r")




# Main app UI
ui <- fluidPage(
  # Other UI components of your main app
  tabsetPanel(
    tabPanel("Literature Review", literatureReview_ui("LitReviewModule1")),
    tabPanel("Resistance by Location", location_ui("LocationModule1")),
    tabPanel("Resistance by Tier", tierTrend_ui("TierModule1")),
    tabPanel("Antibiotic Resistance Analysis", Resistanceui("antibioticModule1"))
    # Add more tabs for additional modules if needed
  )
)

# Main app server
server <- function(input, output, session) {
  Resistanceserver("antibioticModule1")
  location_server("LocationModule1")
  tierTrend_server("TierModule1")
  literatureReviewServer("LitReviewModule1")
  # Call server functions for additional modules if needed
}

# Run the app
shinyApp(ui, server)

