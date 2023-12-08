

# Checking the version of Shiny
shiny_version <- packageVersion("shiny")
print(shiny_version)

# Load the dataset
data <- read.csv("data/raw_data/data.csv", stringsAsFactors = FALSE)

# Data cleaning and preparation
antibiotic_columns <- grep("R1|H1|W1|N5|N3|I2|E1|O1|N4|N6|N2|B1|Z1|W2|Z2|F1|M2|T1|K1|D1|S1|D3|E2|B2|L1|O2|W4|H3|H4|X1|T2|Z4|U1|Z3|W5|J1|O3|V1|I1|G1|A1|R3|FUCIDIC ACID|B3|D4|H2|CEFOPERAZONE|N1|C1|R2|P1|D2|W3|Q1|H5|M1|Y1", names(data), value = TRUE)
relevant_data <- data %>%
  select(org_standard, all_of(antibiotic_columns))

# Reshape the data to a long format for easier analysis
long_data <- relevant_data %>%
  pivot_longer(cols = -org_standard, names_to = "antibiotic", values_to = "result") %>%
    filter(result != "")

# Calculate the resistance rate for each microbe
resistance_summary <- long_data %>%
  group_by(org_standard, antibiotic) %>%
  summarise(
    resistance_count = sum(result == "R", na.rm = TRUE),
    tested_count = sum(result %in% c("R", "S", "I")),
    resistance_rate = resistance_count / tested_count,
    .groups = 'drop'
  ) %>%
  filter(tested_count > 0)

# Define the user interface for the Shiny app
Resistanceui <- function(id){
  ns <- NS(id)
  fluidPage(
    titlePanel("Antibiotic Resistance Analysis"),
    sidebarLayout(
      sidebarPanel(
        selectInput(ns("microbe_select"), "Choose a Microbe:", choices = unique(resistance_summary$org_standard))
      ),
      mainPanel(
        plotOutput(ns("resistancePlot"))
      )
    ),
    fluidRow(
      h3("Resistance Rate Analysis"),
      p("The Resistance rate by antibiotics gives us insights into which antibiotics are likely to be effective against a particular microbe. This allows us to choose
      the best treatment quickly reducing the need for trial-and-error. This approach saves time and resources for the vets. You can choose different Microbes from the drop-down
      menu to view resistance rates for various microbes against numerous antibiotics that it was tested against."),
      p("Certain antibiotics, such as H2 and I1 (in the case of e-coli), show a higher resistance rate compared to others, indicating that the microbe is more resistant to these antibiotics.
      Other antibiotics, such as B3 and E2 (against e-coli), show lower resistance rates, which could suggest that they are more effective against the microbe or that resistance has not yet developed or is less prevalent."),
      p("The resistance rates vary significantly across the antibiotics, which implies a heterogeneous response by the microbe to different antibiotics. This could be due to various factors like the mechanism of action of the 
      antibiotic, the genetic makeup of the microbe, or prior exposure and evolution of resistance.")
    )
  )
}

# Define the server logic for the Shiny app
Resistanceserver <- function(id){
  moduleServer (id, function(input, output, session) {
    output$resistancePlot <- renderPlot({
      selected_microbe <- resistance_summary %>%
        filter(org_standard == input$microbe_select)
      
      ggplot(selected_microbe, aes(x = antibiotic, y = resistance_rate, fill = resistance_rate)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(title = paste("Resistance Rates for", input$microbe_select),
            x = "Antibiotic",
            y = "Resistance Rate") +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })
  })
}

# Run the Shiny app
shinyApp(ui = Resistanceui, server = Resistanceserver)
