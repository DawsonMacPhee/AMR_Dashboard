

# INITIAL DATA SETUP
full_data <- readRDS("./data/dashboard_data/full_data.rds")
aggregated_data <- readRDS("./data/dashboard_data/aggregated_data.rds")

microbes_choices <- unique(full_data[org_standard != ""]$org_standard)
antimicrobial_choices <- c("TIER 1", "TIER 2", "TIER 3", "ALL TIERS")

tier1 <- c("W1", "W2", "E1", "E2", "R1", "R2", "I1", "I2", "S1", "F1", "H1", "H2", "H3",
           "H4", "V1", "B1", "B3", "M1", "M2")
tier2 <- c("Q1", "W3", "W4", "W5", "R3", "Y1", "U1", "O1", "O2", "O3", "P1", "A1", "D1",
           "D2", "D3", "D4", "H5", "J1", "L1", "Z1", "Z2", "Z3", "Z4", "X1", "C1", "B2",
           "N1", "N2", "N3", "N4", "N5", "N6")
tier3 <- c("T1", "T2", "G1", "K1")

# HELPER FUNCTIONS
getSelectedTiers <- function(input) {
  selected_tiers <- c()
  if("TIER 1" %in% input$antimicrobial) selected_tiers <- c(selected_tiers, tier1)
  else if("TIER 2" %in% input$antimicrobial) selected_tiers <- c(selected_tiers, tier2)
  else if("TIER 3" %in% input$antimicrobial) selected_tiers <- c(selected_tiers, tier3)
  else if("ALL TIERS" %in% input$antimicrobial) selected_tiers <- c(tier1, tier2, tier3)
  return(selected_tiers)
}

createSummaryData <- function(aggregated_data, selected_tiers) {
  aggregated_data %>%
    filter(pseud_drug %in% selected_tiers) %>%
    group_by(tier, year_month) %>%
    summarize(
      resistant_counts = sum(count[resistance_category == "R"]),
      total_counts = sum(count),
      proportion_resistant = resistant_counts / total_counts,
      .groups = 'drop'
    ) %>%
    ungroup() %>%
    mutate(year_month = as.Date(year_month),
           months_since_start = 12 * (as.numeric(format(year_month, "%Y")) - as.numeric(format(min(year_month), "%Y"))) +
             (as.numeric(format(year_month, "%m")) - as.numeric(format(min(year_month), "%m"))))
}

# R SHINY UI SETUP
tierTrend_ui <- function(id) {
ns <- NS(id)
fluidPage(
  titlePanel("Analysis of Resistance Trends Over Time"),
  fluidRow(
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("antimicrobial"), "Select Antomicrobial(s)", choices = antimicrobial_choices, multiple = FALSE),
        selectInput(ns("microbes"), "Select Tested Microbe(s)", choices = c("ALL", microbes_choices), multiple = TRUE),
      ),
      mainPanel(
        plotOutput(ns("ratioScatterPlot")),
      )
    )
  ),
  fluidRow(
    column(12,
           h4("Plot Of Resistance Trends Over Time"),
           p("This plot of resistance trends over time gives users an 
             opportunity to unserstand the trends in the percentage of 
             resistant/non-resistant cases over time across the different 
             tiers, and for different microbes. You can select either all tiers, 
             tier 1, tier 2, or tier 3 and then select which specific microbes you would 
             like to see the resistance rate data against on a month by month 
             basis"),
           p("The ratio is simply # of 'R' cases over the # of 'S', 'I', and 'R'
             cases"),
           p("This plot can be insightful if the user wishes to understand the 
             resistance of specific microbes against the different tiers, which
             may help create more informed decisions regarding antimicrobaisls 
             to use, so that higher tier antimicrobials are not being used when
             lower tier ones will suffice."),
           hr()
    )
  ),
  fluidRow(
    plotOutput(ns("residualPlot")),
    verbatimTextOutput("residualValue"),
    
  ),
  fluidRow(
    column(12,
           h4("Residual Plot/Deviance Test for goodness of fit"),
           p("Since the poission model is best for data that is 'equi-dispersed', 
             meaning that the data's variance is approximately equal to its mean.
             One common way to test a dataset's goodness-of-fit for poisson
             regression is to use a Residual plot and test."),
           p("The residual test result (the ratio of the residual deviance, which 
           is the difference observed and predicted values, to the 
           degrees of freedom for residuals) must be <1 in order for the data to
           qualify for poission regression generally, and ultimately only tier 3 
             data met the criteria for a poisson regression"),
           hr()
    )
  ),
  fluidRow(
    verbatimTextOutput("modelSummary")
  ),
  fluidRow(
    column(12,
           h4("Poission Regression Explanation"),
           p("Seeing as the data we are trying to model is countable data (i.e. 
             the number of resistant test cases), and that in at least one 
             subset the data is equi-dispersed, poission regression stands out
             as a method to consider when trying to understand where AMR is
             trending over time. With the dependent variable as the count data,
             and time as the independent variable, we can create a model that can
             possibly confirm if there are the changes in AMR over time."),
           p("In this case, although the data meets the criteria for linear regression,
             it fails to reach significance and thus fails to disprove the null hypothesis 
             that AMR does not have a significant relationship with time")
    )
  ),
)
}

# SERVER FUNCTION
tierTrend_server <- function(id){
moduleServer(id, function(input, output, session) {
  output$ratioScatterPlot <- renderPlot({
    selected_antimicrobials <- getSelectedTiers(input)
    dataSubset <- full_data[pseud_drug %in% selected_antimicrobials]
    if (!("ALL" %in% input$microbes)) {
      dataSubset <- dataSubset[org_standard %in% input$microbes]
    }
    dataSubset[, date := as.Date(paste(order_year, order_month, "01", sep = "-"), format = "%Y-%m-%d")]
    dataSubset[, ratio := sum(resistance_category == "R") / .N, by = date]
    ggplot(dataSubset, aes(x = date, y = ratio)) +
      geom_point() +
      geom_line() +
      ylim(0, 1) +
      labs(
        title = "Resistance Ratio Over Time",
        x = "Time",
        y = "Resistance Ratio"
      ) +
      theme_minimal()
  })
  
  output$residualPlot <- renderPlot({
    selected_tiers <- getSelectedTiers(input)
    
    summary_data <- createSummaryData(aggregated_data, selected_tiers)
    
    poisson_model_with_offset <- glm(resistant_counts ~ months_since_start + offset(log(total_counts)), 
                                     family = "poisson", data = summary_data)
    
    summary_data$residuals <- residuals(poisson_model_with_offset, type = "response")
    ggplot(summary_data, aes(x = months_since_start, y = residuals)) +
      geom_hline(yintercept = 0, linetype = "dashed") +
      geom_point() +
      labs(title = "Residuals from Poisson Model",
           x = "Months Since Start", y = "Residuals") +
      theme_minimal()
  })
  
  output$modelSummary <- renderPrint({
    selected_tiers <- getSelectedTiers(input)
    
    summary_data <- createSummaryData(aggregated_data, selected_tiers)
    
    poisson_model_with_offset <- glm(resistant_counts ~ months_since_start + offset(log(total_counts)), 
                                     family = "poisson", data = summary_data)
    summary(poisson_model_with_offset)
  })
  
  output$residualValue <- renderPrint({
    selected_tiers <- getSelectedTiers(input)
    summary_data <- createSummaryData(aggregated_data, selected_tiers)
    poisson_model_with_offset <- glm(resistant_counts ~ months_since_start + offset(log(total_counts)), 
                                     family = "poisson", data = summary_data)
    
    # Calculate the deviance ratio
    residual_deviance <- poisson_model_with_offset$deviance
    df_residual <- poisson_model_with_offset$df.residual
    deviance_ratio <- residual_deviance / df_residual
    
    # Print the deviance ratio
    cat("Deviance Ratio:", deviance_ratio)
  })
})
}

# LAUNCHING SHINY APP
shinyApp(ui = tierTrend_ui, server = tierTrend_server)
