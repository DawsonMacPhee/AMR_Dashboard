#~~~~~~~~~~~~~~~~~Library Loading~~~~~~~~~~~~~~~~~
remove(list=ls())
library(shiny)
library(ggplot2)
library(reshape2)
library(devtools)
library(choroplethrZip)
library(stringr)
library(gsubfn)
library(sf)
library(spdep)

#~~~~~~~~~~~~~~~~~Defining Functions~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~Data Loading~~~~~~~~~~~~~~~~~
resistance_rate_antibiotic = readRDS("./data/dashboard_data/zip_data_antibiotic_resistance_rate.Rds")
resistance_rate_microbe = readRDS("./data/dashboard_data/zip_data_microbe_resistance_rate.Rds")
test_rate_antibiotic = readRDS("./data/dashboard_data/zip_data_antibiotic_test_rate.Rds")
test_rate_microbe = readRDS("./data/dashboard_data/zip_data_microbe_test_rate.Rds")

spatial_autocorrelation = readRDS("./data/dashboard_data/spatial_autocorrelation_data.Rds")
spatial_weights = readRDS("./data/dashboard_data/spatial_autocorrelation_weights.Rds")

data("df_pop_zip")
valid_zips = df_pop_zip$region

#~~~~~~~~~~~~~~~~~Setting up R-Shiny UI~~~~~~~~~~~~~~~~~
ui <- fluidPage(
  tags$head(
    tags$style(type="text/css", "#heatmap-container { max-width: 1250px; background-color: #FFEAE0; margin: auto; padding: 20px 20px 0px 20px; border-radius: 10px; overflow: auto; }"),
    tags$style(type="text/css", "#heatmap-sidebar { height: 525px; max-width: 350px; }"),
    tags$style(type="text/css", "#heatmap-analysis { max-width: 1000px; background-color: #FFEAE0; margin: auto; padding: 0px 20px 15px 20px; border-radius: 10px; margin-top: 15px; }"),
    tags$style(type="text/css", "#autocorrelation-container { max-width: 1250px; background-color: #E0F5FF; margin: auto; padding: 20px 20px 0px 20px; border-radius: 10px; overflow: auto; }"),
    tags$style(type="text/css", "#autocorrelation-sidebar { height: 375px; max-width: 350px; }"),
    tags$style(type="text/css", "#autocorrelation-output { height: 375px; overflow: hidden; margin-left: 35px; }"),
    tags$style(type="text/css", "#autocorrelation-analysis { max-width: 1000px; background-color: #E0F5FF; margin: auto; margin-bottom: 40px; padding: 0px 20px 15px 20px; border-radius: 10px; margin-top: 15px; }"),
  ),
  titlePanel(title=h1("Are there Trends in Resistance Rates by Location?", align="center")),
  hr(),
  fluidRow(
    id="heatmap-container",
    column(4,
           id="heatmap-sidebar",
           class="well",
           h3("Resistance Rate Heatmap"),
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
    ),
    column(8, plotOutput("heatmap")),
  ),
  fluidRow(
    id="heatmap-analysis",
    h3("Data Analysis"),
    p("The todo section here should describe what the heatmap is doing and why it's important.")
  ),
  hr(),
  fluidRow(
    id="autocorrelation-container",
    column(4,
           id="autocorrelation-sidebar",
           class="well",
           h3("Spatial Autocorrelation"),
           selectInput(
             "autocorrelation_microbe",
             "Microbe:",
             unique(spatial_autocorrelation$Microbe),
           ),
    ),
    column(8,
           id = "autocorrelation-output",
           fluidRow(
             column(6,
                    h4("Moran's I"),
                    htmlOutput("moran"),
                    plotOutput("moranPlot"),
             ),
             column(6,
                    h4("Geary's C"),
                    htmlOutput("geary"),
                    plotOutput("gearyPlot"),
             ),
           ),
    ),
  ),
  fluidRow(
           id="autocorrelation-analysis",
           h3("Conclusions"),
           p("The todo section here should describe what conclusions we can make about the analysis and how these algorithms work.")
  ),
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

  moranAnalysis = reactive({
    test = moran.test(as.numeric(spatial_autocorrelation$ResistanceRate[spatial_autocorrelation$Microbe == input$autocorrelation_microbe]), spatial_weights, alternative="greater", zero.policy=TRUE)
    
    sprintf("I Statistic: %f<br>Hypothesis Test P-Value: %f<br><br>", test$estimate[1], test$p.value)
  })
  
  moranSimulation = reactive({
    moran.mc(as.numeric(spatial_autocorrelation$ResistanceRate[spatial_autocorrelation$Microbe == input$autocorrelation_microbe]), spatial_weights, nsim = 999, alternative = "greater", zero.policy=TRUE)
  })
  
  gearyAnalysis = reactive({
    test = geary.test(as.numeric(spatial_autocorrelation$ResistanceRate[spatial_autocorrelation$Microbe == input$autocorrelation_microbe]), spatial_weights, alternative="greater", zero.policy=TRUE)
    
    sprintf("C Statistic: %f<br>Hypothesis Test P-Value: %f<br><br>", test$estimate[1], test$p.value)
  })
  
  gearySimulation = reactive({
    geary.mc(as.numeric(spatial_autocorrelation$ResistanceRate[spatial_autocorrelation$Microbe == input$autocorrelation_microbe]), spatial_weights, nsim = 999, alternative = "greater", zero.policy=TRUE)
  })
  
  #~~~~~~~~~~~~~~~~~Define Outputs~~~~~~~~~~~~~~~~~
  output$heatmap = renderPlot({
    if ("All" %in% input$zip) {
      zip_choropleth(heatmapData(), 
                     state_zoom = "new york", 
                     legend     = "Percentage of Resistant Tests") + 
        theme(
          plot.background = element_rect(fill="#FFEAE0", color="#FFEAE0"),
          text = element_text(size=16),
          panel.border = element_blank()
        )
    } else {
      df = heatmapData()
      zip_choropleth(df,
                     state_zoom = "new york", 
                     zip_zoom = unique(df$region),
                     legend     = "Percentage of Resistant Tests") + 
        theme(
          plot.background = element_rect(fill="#FFEAE0", color="#FFEAE0"),
          text = element_text(size=16),
          panel.border = element_blank()
        )
    }
  }, height = 477, width = 800, bg="#FFEAE0")
  
  output$moran = renderUI({ HTML(moranAnalysis()) })
  
  output$moranPlot = renderPlot({ 
    plot(moranSimulation(), xlab="Moran's I")
  }, height = 275, width = 375)
  
  output$geary = renderUI({ HTML(gearyAnalysis()) })
  
  output$gearyPlot = renderPlot({ 
    plot(gearySimulation(), xlab="Geary's C")
  }, height = 275, width = 375)
}

shinyApp(ui, server)