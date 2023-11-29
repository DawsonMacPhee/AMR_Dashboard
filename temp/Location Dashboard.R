# Library Loading
remove(list=ls())
library(shiny)
library(ggplot2)
library(reshape2)
library(devtools)
library(choroplethrZip)
library(stringr)
library(gsubfn)

expandZip3Level = function(zip, new_value) {
  if (identical(new_value, numeric(0))) new_value = 0
  region = c()
  value = c()
  
  new_region = as.numeric(paste(zip, "00", sep=""))
  for (i in 0:99) {
    if (as.character(new_region + i) %in% valid_zips) {
      region = append(region, new_region + i)
      value = append(value, new_value)
    }
  }
  
  list(zip_region=region, zip_value=value)
}

filterOnResistance = function(data_mode, filtered_rows) {
  if (data_mode == "resistance") {
    output = filtered_rows[filtered_rows$ResistanceLevel == "R",]
  } else {
    output = filtered_rows
  }
    
  output
}

# Data Loading
resistance_rate_antibiotic = readr::read_rds("./zip_data_antibiotic_resistance_rate.Rds")
resistance_rate_microbe = readr::read_rds("./zip_data_microbe_resistance_rate.Rds")
test_rate_antibiotic = readr::read_rds("./zip_data_antibiotic_test_rate.Rds")
test_rate_microbe = readr::read_rds("./zip_data_microbe_test_rate.Rds")

data("df_pop_zip")
valid_zips = df_pop_zip$region

# Setting up R-Shiny Window
ui <- fluidPage(
  titlePanel(title=h4("Location Analysis", align="center")),
  sidebarPanel(h3("Resistance Rate Heatmap"),
               radioButtons("data_mode", "Mode of Operation:", c("Resistance Rate" = "resistance", "Test Rate" = "test")),
               selectInput(
                 "zip",
                 "Zipcode Area:",
                 c("All", paste(sort(unique(resistance_rate_antibiotic$Zip)), "00", sep="")),
                 multiple = TRUE,
                 selected = "All"
               ),
               selectInput(
                 "antibiotic",
                 "Antibiotic:",
                 c("All", "Tier 1", "Tier 2", "Tier 3", unique(resistance_rate_antibiotic$Antibiotic)),
               ),
               selectInput(
                 "microbe",
                 "Microbe:",
                 c("All", unique(resistance_rate_microbe$Microbe)),
               ),
               tags$head(
                 tags$style(type="text/css", "form { height: 600px; }"),
               )
  ),
  mainPanel(plotOutput("heatmap"),
            plotOutput("bars")
  )
)

server <- function(input,output) {
  #~~~~~~~~~~~~~~~~~Define Reactive Vars~~~~~~~~~~~~~~~~~
  heatmapData = reactive({
    # Set mode filter
    if (input$data_mode == "resistance") {
      data_antibiotic = resistance_rate_antibiotic
      data_microbe = resistance_rate_microbe
    } else {
      data_antibiotic = test_rate_antibiotic
      data_microbe = test_rate_microbe
    }
    
    # Set zip filter
    if ("All" %in% input$zip) {
      raw_zips = unique(data_antibiotic$Zip)
    } else {
      raw_zips = str_sub(input$zip, 1, -3)
    }
    
    region = c()
    value = c()
    
    if (grepl("Tier", input$antibiotic, fixed = TRUE) & input$microbe == "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_antibiotic[
            data_antibiotic$Zip == zip & 
            data_antibiotic$Tier == gsub("Tier ", "", input$antibiotic),
          ]
        )
        new_value = as.numeric(unique(filtered_rows$PercentTier))
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    } else if (grepl("Tier", input$antibiotic, fixed = TRUE) & input$microbe != "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_antibiotic[
            data_antibiotic$Zip == zip & 
            data_antibiotic$Tier == gsub("Tier ", "", input$antibiotic),
          ]
        )
        new_tier_value = as.numeric(unique(filtered_rows$PercentTier))
        
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_microbe[
            data_microbe$Zip == zip & 
            data_microbe$Microbe == input$microbe,
          ]
        )
        new_microbe_value = as.numeric(filtered_rows$PercentMicrobe)
        
        if (identical(new_tier_value, numeric(0))) new_tier_value = 0
        if (identical(new_microbe_value, numeric(0))) new_microbe_value = 0
        new_value = new_tier_value * new_microbe_value
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    } else if (input$antibiotic == "All" & input$microbe == "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_antibiotic[
            data_antibiotic$Zip == zip,
          ]
        )
        new_value = mean(as.numeric(unique(filtered_rows$PercentTier)))
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    } else if (input$antibiotic != "All" & input$microbe == "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_antibiotic[
            data_antibiotic$Zip == zip & 
            data_antibiotic$Antibiotic == input$antibiotic,
          ]
        )
        new_value = as.numeric(filtered_rows$PercentAntibiotic)
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    } else if (input$antibiotic == "All" & input$microbe != "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_microbe[
            data_microbe$Zip == zip & 
            data_microbe$Microbe == input$microbe,
          ]
        )
        new_value = as.numeric(filtered_rows$PercentMicrobe)
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    } else if (input$antibiotic != "All" & input$microbe != "All") {
      for (zip in raw_zips) {
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_antibiotic[
            data_antibiotic$Zip == zip & 
            data_antibiotic$Antibiotic == input$antibiotic,
          ]
        )
        new_antibiotic_value = as.numeric(filtered_rows$PercentAntibiotic)
        
        filtered_rows = filterOnResistance(
          input$data_mode,
          data_microbe[
            data_microbe$Zip == zip & 
            data_microbe$Microbe == input$microbe,
          ]
        )
        new_microbe_value = as.numeric(filtered_rows$PercentMicrobe)
        
        if (identical(new_antibiotic_value, numeric(0))) new_antibiotic_value = 0
        if (identical(new_microbe_value, numeric(0))) new_microbe_value = 0
        new_value = new_antibiotic_value * new_microbe_value
        
        list[zip_region, zip_value] = expandZip3Level(zip, new_value)
        region = c(region, zip_region)
        value = c(value, zip_value)
      }
    }
    
    df = data.frame(region=as.character(region), value=value)
    df
  })

  #~~~~~~~~~~~~~~~~~Define Plots~~~~~~~~~~~~~~~~~
  output$heatmap = renderPlot({
    if ("All" %in% input$zip) {
      zip_choropleth(heatmapData(), 
                     state_zoom = "new york", 
                     legend     = "Percentage of Resistant Tests")
    } else {
      df = heatmapData()
      zip_choropleth(df,
                     state_zoom = "new york", 
                     zip_zoom = unique(df$region),
                     legend     = "Percentage of Resistant Tests")
    }
  }, height = 600, width = 800)
}

shinyApp(ui, server)