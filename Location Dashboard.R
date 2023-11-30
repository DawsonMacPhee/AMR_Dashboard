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
  # CSS Definitions
  tags$head(
    tags$style(type="text/css", "#heatmap-block { background-color: #E0F5FF; padding: 20px; border-radius: 5px; }"),
    tags$style(type="text/css", "#heatmap-container { max-width: 1170px; overflow: auto; background-color: #FFEAE0; margin: auto; border-radius: 15px; }"),
    tags$style(type="text/css", "#heatmap-content { display: flex; justify-content: center; width: 1170px; padding: 20px 20px 0px 20px; }"),
    tags$style(type="text/css", "#heatmap-sidebar { height: 525px; max-width: 350px; }"),
    tags$style(type="text/css", "#heatmap-output { width: 800px; }"),
    tags$style(type="text/css", "#heatmap-analysis { max-width: 1500px; margin: auto; margin-top: 15px; padding: 0px 20px 15px 20px; }"),
    tags$style(type="text/css", "#autocorrelation-block { background-color: #FFEAE0; padding: 20px; border-radius: 5px; margin-bottom: 40px; }"),
    tags$style(type="text/css", "#autocorrelation-container { max-width: 1124px; overflow: auto; background-color: #E0F5FF; margin: auto; border-radius: 15px; }"),
    tags$style(type="text/css", "#autocorrelation-content { display: flex; justify-content: center; width: 1124px; padding: 20px 10px 0px 20px; }"),
    tags$style(type="text/css", "#autocorrelation-sidebar { height: 375px; max-width: 350px; }"),
    tags$style(type="text/css", "#autocorrelation-output { display: flex; height: 375px; margin-left: 10px; }"),
    tags$style(type="text/css", ".autocorrelation-display { overflow: hidden; padding-left: 10px; padding-right: 10px; width: 395px; }"),
    tags$style(type="text/css", "#autocorrelation-analysis { max-width: 1500px; margin: auto; margin-top: 15px; padding: 0px 20px 15px 20px; }"),
  ),
  titlePanel(title=h1("Are there Trends in Resistance Rates by Location?", align="center")),
  hr(),
  
  # Heatmap UI
  div(id="heatmap-block",
      
    # Heatmap
    div(id="heatmap-container",
      div(id="heatmap-content",
        div(id="heatmap-sidebar", class="well",
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
        div(id="heatmap-output", plotOutput("heatmap")),
      ),
    ),
  
    # Heatmap Writeup
    fluidRow(id="heatmap-analysis",
      h3("The Resistance Rate Heatmap"),
      HTML("
        <p>
          This exploration tool is a great way to take a peak at how AMR is affecting the state of New York. Seen on the map above, 
          you're able to examine the resistance and test rates as well as apply filters to take a closer look into whatever zipcodes, 
          antibiotics, or microbes you're interested in. By toggling through our modes of operations, you get two different maps for 
          each setting.
        </p>
        <p style='text-align: center'>
          <br><b>Resistance Rate = # of resistant test results / # of tests in the zipcode per the selected filters<br><br>
          Test Rate = # of tests in the zipcode per the selected filters / # of tests in the state</b><br><br>
        </p>
        <p>
          The Resistance Rate is all about discovering what zipcodes have infections with the highest resistance to antimicrobials. 
          The Test Rate is all about discovering what zipcodes are testing for AMR the most. The higher the test rate, the more tests 
          being conducted, and the more you can trust the resistance rate presented (100% resistance in only 5 tests doesn't really 
          inspire confidence). Be careful though -- note the scale of the heatmap changes from setting to setting! Also, if you want 
          to see a specific zipcode, you're going to want to remove the 'All' selection in the input by selecting it and pressing delete.
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
            "autocorrelation_microbe",
            "Microbe:",
            unique(spatial_autocorrelation$Microbe),
          ),
        ),
        div(id = "autocorrelation-output",
          div(class="autocorrelation-display",
            h4("Moran's I"),
            htmlOutput("moran"),
            plotOutput("moranPlot"),
          ),
          div(class="autocorrelation-display",
            h4("Geary's C"),
            htmlOutput("geary"),
            plotOutput("gearyPlot"),
          ),
        ),
      ),
    ),
  
    # Autocorrelation Writeup
    fluidRow(id="autocorrelation-analysis",
      h3("Spatial Autocorrelation"),
      HTML("
        <p>
          The heatmap on its own is a cool way to view and explore the data, but it doesn't really give us a definitive answer as to 
          whether AMR is spreading or not. For this, we're using two spatial autocorrelation methods -- Moran's I and Geary's C. Moran's I 
          is better for linear data, while Gearcy's C is better for non-linear. By implementing both we will improve our results and analysis. To 
          calculate these values, we take the resistance rate of each zipcode, and compare it to each of its neighbouring zipcodes. Then, 
          we generate weight values that represents the relationship between a zipcode and each of its neighbouring areas. Those weight values are 
          used in two different formulas to calculate an I and C value that represents the entire state.
        </p>
        <!--IMAGE OF FORMULAS-->
        <p>
          These algorithms are all about the comparison of correlated data and random data. See the number lines below to understand what the I and C 
          values actually mean. The are very similar at their core, but shifted and inverted compared to eachother. The more extreme the I and C values get, 
          the greater the likelihood of a positive or negative correlation actually existing in thge data.
        </p>
        <!--IMAGE OF NUMBER LINES-->
        <p>
          Above, you're able to run these spatial autocorrelation algorithms for the top 3 microbes with the most tests in the state of New York. 
          Given your new found knowledge in this matter, you will probably notice that none of these I or C values suggest any significant 
          evidence of AMR spreading from location to location. To help you visualize this, we conducted both a Hypothesis Test and a Monte 
          Carlo Simulation. For our Hypothesis Test, tested a null hypothesis of 'There is no correlation from zipcode to zipcode in terms 
          of resistance rate'. You can see the resulting p values are quite large and don't allow us to reject this hypothesis at any 
          reasonable level of accuracy. To reject this, we'd need a p value of at least 0.05 or less. Being unable to reject this null hypothesis 
          suggests that it is correct, and that there is no discernible correlation from zipcode to zipcode in terms of resistance rate. For our 
          Monte Carlo simulations, we generated 1000 randomly distributed collections of resistance rates and used them to calculate I and C 
          values. We plotted the density of the random I and C values, which you can see in the curve on the plot. Finally, we added a bar to the 
          plot showing our I and C value, which for all microbes is located right in the middle of the density of these random distributions. 
          For this reason, it's safe to say that our I and C values show a random distribution of resistance rates by location.
        </p>
        <p>
          CONCLUSION
        </p>
      ")
    ),
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