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
location_ui <- function(id) {
ns <- NS(id)
fluidPage(
  # CSS Definitions
  tags$head(
    tags$style(type="text/css", "#heatmap-block { background-color: #a9c5d1; padding: 20px; border-radius: 5px; }"),
    tags$style(type="text/css", "#heatmap-container { max-width: 1170px; overflow: auto; background-color: #bfe0c3; margin: auto; border-radius: 5px; }"),
    tags$style(type="text/css", "#heatmap-content { display: flex; justify-content: center; width: 1170px; padding: 20px 20px 0px 20px; }"),
    tags$style(type="text/css", "#heatmap-sidebar { height: 525px; max-width: 350px; }"),
    tags$style(type="text/css", "#heatmap-output { width: 800px; }"),
    tags$style(type="text/css", "#heatmap-analysis { max-width: 1500px; margin: auto; margin-top: 15px; padding: 0px 20px 15px 20px; }"),
    tags$style(type="text/css", "#autocorrelation-block { background-color: #bfe0c3; padding: 20px; border-radius: 5px; margin-bottom: 40px; }"),
    tags$style(type="text/css", "#autocorrelation-container { max-width: 1124px; overflow: auto; background-color: #a9c5d1; margin: auto; border-radius: 5px; }"),
    tags$style(type="text/css", "#autocorrelation-content { display: flex; justify-content: center; width: 1124px; padding: 20px 10px 0px 20px; }"),
    tags$style(type="text/css", "#autocorrelation-sidebar { height: 375px; max-width: 350px; }"),
    tags$style(type="text/css", "#autocorrelation-output { display: flex; height: 375px; margin-left: 10px; }"),
    tags$style(type="text/css", ".autocorrelation-display { overflow: hidden; padding-left: 10px; padding-right: 10px; width: 395px; }"),
    tags$style(type="text/css", "#autocorrelation-analysis { max-width: 1500px; margin: auto; margin-top: 15px; padding: 0px 20px 15px 20px; }"),
  ),
  titlePanel(title=h1("Are there Trends in Resistance Rates by Location?", align="center")),
  p("
    By looking at resistance rates by location, we hope to gain a better understanding of how AMR is spreading from location to 
    location. Antibiotic tiers aim to reduce AMR, which should lead to less spread. Showing little to no spread by location, could 
    support the idea that the antibiotic tiers are doing their job.
  "),
  hr(),
  
  # Heatmap UI
  div(id="heatmap-block",
      
    # Heatmap
    div(id="heatmap-container",
      div(id="heatmap-content",
        div(id="heatmap-sidebar", class="well",
          h3("Resistance Rate Heatmap"),
          radioButtons(ns("data_mode"), "Mode of Operation:", c("Resistance Rate" = "resistance", "Test Rate" = "test"), selected = "resistance"),
          selectInput(
            ns("zip"),
            "Zipcode Area:",
            c("All", paste(sort(unique(resistance_rate_antibiotic$Zip)), "00", sep="")),
            multiple = TRUE,
            selected = "All"
          ),
          selectInput(
            ns("antibiotic"),
            "Antibiotic:",
            c("All", "Tier 1", "Tier 2", "Tier 3", unique(resistance_rate_antibiotic$Antibiotic)),
          ),
          selectInput(
            ns("microbe"),
            "Microbe:",
            c("All", unique(resistance_rate_microbe$Microbe)),
          ),
        ),
        div(id="heatmap-output", plotOutput(ns("heatmap"))),
      ),
    ),
  
    # Heatmap Writeup
    fluidRow(id="heatmap-analysis",
      h3("The Resistance Rate Heatmap"),
      HTML("
        <p>
           This exploration tool is a great way to take a peak at how AMR is affecting the state of New York. Seen on the map above, 
           you're able to examine the resistance and test rates as well as apply filters to take a closer look into whatever zip codes, 
           antibiotics, or microbes you're interested in. By toggling through our modes of operations, you get two different maps for 
           each setting.
         </p>
         <p style='text-align: center'>
           <br><b>Resistance Rate = # of resistant test results / # of tests in the zipcode per the selected filters<br><br>
           Test Rate = # of tests in the zipcode per the selected filters / # of tests in the state</b><br><br>
         </p>
         <p>
           The Resistance Rate is all about discovering what zip codes have infections with the highest resistance to antimicrobials. 
           The Test Rate is all about discovering what zip codes are testing for AMR the most. The higher the test rate, the more tests 
           being conducted, and the more you can trust the resistance rate presented (100% resistance in only 5 tests doesn't really 
           inspire confidence). Be careful though -- note the scale of the heat map changes from setting to setting! Also, if you want 
           to see a specific zip code, you're going to want to remove the 'All' selection in the input by selecting it and pressing delete.
        </p>
      ")
    ),
  ),
  
  # Autocorrelation UI
  div(id="autocorrelation-block",
    # Autocorrelation
    div(id="autocorrelation-container",
      div(id="autocorrelation-content",
        div(id="autocorrelation-sidebar", class="well",
          h3("Spatial Autocorrelation"),
          selectInput(
            ns("autocorrelation_microbe"),
            "Microbe:",
            unique(spatial_autocorrelation$Microbe),
          ),
        ),
        div(id = "autocorrelation-output",
          div(class="autocorrelation-display",
            h4("Moran's I"),
            htmlOutput("moran"),
            plotOutput(ns("moranPlot")),
          ),
          div(class="autocorrelation-display",
            h4("Geary's C"),
            htmlOutput("geary"),
            plotOutput(ns("gearyPlot")),
          ),
        ),
      ),
    ),
  
    # Autocorrelation Writeup
    fluidRow(id="autocorrelation-analysis",
      h3("Spatial Autocorrelation"),
      HTML("
         <p>
           The heat map on its own is a cool way to view and explore the data, but it doesn't really give us a definitive answer to whether AMR is 
           spreading or not. For this, we're using two spatial auto correlation methods -- Moran's I and Geary's C. Moran's I 
           is better for linear data, while Geary's C is better for non-linear data. By implementing both we will improve our results and analysis. To 
           calculate these values, we take the resistance rate of each zip code, and compare it to each of its neighboring zip codes. Then, 
           we generate weight values that represents the relationship between a zip code and each of its neighboring areas. Those weight values are 
           used in two different formulas to calculate I and C values that represent the entire state.
         </p>
         <div style='display: flex; justify-content: center; gap: 20px; padding: 20px;'>"
      ),
      tags$img(
        src = "./images/MoranFormula.png",
        alt = "Moran's I Formula",
        height = 75
      ),
      tags$img(
        src = "./images/GearyFormula.png",
        alt = "Geary's C Formula",
        height = 75
      ),
      HTML("
         </div>
         <p>
           These algorithms are all about the comparison of correlated data and random data. See the number lines below to understand what the I and C 
           values actually mean. They are very similar at their core, but shifted and inverted compared to eachother. The more extreme the I and C values get, 
           the greater the likelihood of a positive or negative correlation actually existing in the data.
         </p>
         <div style='display: flex; flex-direction: column; align-items: center; gap: 20px; padding: 20px;'>"
      ),
      tags$img(
        src = "./images/MoranLine.jpg",
        alt = "Moran's I Meaning",
        height = 150,
        width = 450
      ),
      tags$img(
        src = "./images/GearyLine.jpg",
        alt = "Geary's C Meaning",
        height = 150,
        width = 450
      ),
      HTML("
         </div>
         <p>
           Above, you're able to run these spatial auto correlation algorithms for the top 3 microbes with the most tests in the state of New York. 
           Given your new-found knowledge in this matter, you will probably notice that none of these I or C values suggest any significant 
           evidence of AMR spreading from location to location. To help you visualize this, we conducted both a Hypothesis Test and a Monte 
           Carlo Simulation. For our Hypothesis Test, we tested a null hypothesis of 'There is no correlation from zip code to zip code in terms 
           of resistance rate'. You can see the resulting p values are quite large and don't allow us to reject this hypothesis at any 
           reasonable level of accuracy. To reject this, we'd need a p value of at least 0.05 or less. Being unable to reject this null hypothesis 
           suggests that it is correct, and that there is no discernible correlation from zip code to zip code in terms of resistance rate. For our 
           Monte Carlo simulations, we generated 1000 randomly distributed collections of resistance rates and used them to calculate I and C 
           values. We plotted the density of the random I and C values, which you can see in the curve on the plot. Finally, we added a bar to the 
           plot showing our I and C value, which for all microbes is located right in the middle of the density of these random distributions. 
           For this reason, it's safe to say that our I and C values show a random distribution of resistance rates by location.
        </p>
        <p>
          According to the analysis we have conducted here, both Morans I and Geary's C values suggest that there is no linear or non-linear correlation 
          in resistance rate by location. This tells us that given the data we have access to, there may be a correlation between the antibiotic tiers 
          effectively reducing the resistance rate, and the lack of AMR spreading from zip code to zip code. Key word -- given the data we have 
          access to. Different vets are testing AMR to varying degrees with labs that have various standards and tolerance levels. Additionally, with only 
          49 distinct zip code areas in the data, we don't have a very large area to analyze. The presence of spatial correlation would have told us more 
          than the lack of correlation does (it's much easier to show something definitively exists than show it doesn't). Instead, we think more locations 
          and data points in those locations would help provide us with better context before we can confirm there is truly no correlation.
        </p>
      ")
    ),
  ),
)
}

location_server <- function(id) {
moduleServer (id, function(input,output, session) {
  #~~~~~~~~~~~~~~~~~Define Reactive Vars~~~~~~~~~~~~~~~~~
  heatmapData = reactive({
    # Set mode filter
    if(!is.null(input$data_mode)) {
      if (input$data_mode == "resistance") {
        data_antibiotic = resistance_rate_antibiotic
        data_microbe = resistance_rate_microbe
      } else {
        data_antibiotic = test_rate_antibiotic
        data_microbe = test_rate_microbe
      }
    } else {
      print(paste("Data mode: fuck", input$data_mode))
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
          plot.background = element_rect(fill="#bfe0c3", color="#bfe0c3"),
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
          plot.background = element_rect(fill="#bfe0c3", color="#bfe0c3"),
          text = element_text(size=16),
          panel.border = element_blank()
        )
    }
  }, height = 477, width = 800, bg="#bfe0c3")
  
  output$moran = renderUI({ HTML(moranAnalysis()) })
  
  output$moranPlot = renderPlot({ 
    plot(moranSimulation(), xlab="Moran's I")
  }, height = 275, width = 375)
  
  output$geary = renderUI({ HTML(gearyAnalysis()) })
  
  output$gearyPlot = renderPlot({ 
    plot(gearySimulation(), xlab="Geary's C")
  }, height = 275, width = 375)
})
}

shinyApp(ui = location_ui, server = location_server)