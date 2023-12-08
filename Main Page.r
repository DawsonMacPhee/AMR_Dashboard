# IMPORTANT - Run this on first launch if you don't have the choroplethrZip library installed
#install_github('arilamstein/choroplethrZip@v1.5.0')

library(shiny)
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
library(gridExtra)

source("Tier Trend Dashboard.r")
source("Location Dashboard.r")
source("Resistance Dashboard.r")
source("Literature Review.r")

resistance_rate_antibiotic = readRDS("./data/dashboard_data/zip_data_antibiotic_resistance_rate.Rds")


# Main app UI
ui <- fluidPage(
  
    navbarPage(title = "Antimicrobial Resistance Analysis",
        tabPanel("Home",
            fluidRow(
                h1("Welcome to Our Dashboard!"),
                p("Antimicrobial Resistance (AMR) is a major issue plaguing the medical world right now. Each year there are around 2.8 million AMR related infections in North America, and about 1.27 million worldwide related deaths. This issue is all related to the
                 interactions between microbes like bacteria or fungi and the antimicrobials we are using to fend off the infections they cause. Resistances to traditional antibiotics are growing and the infections become harder and harder to treat."),
                p("One method of fighting AMR is through the use of Antibiotic tiers. This is a system designed to reduce the spread of AMR by introducing a tier system applied to Antibiotics. The idea is that the higher tiers are reserved for the worst infections and
                 are kept potent and safe from AMR as a result."),
                p("In the dashboard you are about to explore, we plan to examine a dataset which was collected in New York State, USA between the years of 2019-2022. This dataset includes AMR testing results from Dogs and Cats, as collected by Veterinarians in the state.
                 We plan on using this data to examine the effectiveness at the antibiotic tier system at combating AMR. This examination leads us into multiple sub questions related to Antibiotic Tier effectiveness. We encourage you to play around
                 with our different sections and read our conclusions, inviting you to make your own assessment of the data and inform your knowledge of this incredibly important topic."),
                p("NOTE: If you are interested in reading our sources and exploring the content more, please reference our Literature Review section for a full list of sources and references.")
            ),
            br(),
            plotOutput("allResistancePlots", width = "100%")
        ),

        
        tabPanel("Resistance by Location", location_ui("LocationModule1"), id="locationTab"),
        tabPanel("Resistance by Tier", tierTrend_ui("TierModule1"), id="tierTab"),
        tabPanel("Resistance by Antibiotic", Resistanceui("antibioticModule1"), id="resistanceTab"),
        tabPanel("Literature Review", literatureReview_ui("LitReviewModule1"), id="litReview")
    )   
  
)

# Main app server
server <- function(input, output, session) {

    create_pie_chart <- function(tier) {
        tier_resistance_data = resistance_rate_antibiotic[resistance_rate_antibiotic$Tier == tier,]
        total_count_in_tier = sum(as.numeric(tier_resistance_data$AntibioticFrequencyCount))
        percents = c(
          sum(as.numeric(tier_resistance_data$AntibioticFrequencyCount[tier_resistance_data$ResistanceLevel == "R"])) / total_count_in_tier,
          sum(as.numeric(tier_resistance_data$AntibioticFrequencyCount[tier_resistance_data$ResistanceLevel == "I"])) / total_count_in_tier,
          sum(as.numeric(tier_resistance_data$AntibioticFrequencyCount[tier_resistance_data$ResistanceLevel == "S"])) / total_count_in_tier
        )
        tier_resistance_data = data.frame(Percents = percents, ResistanceLevel = c("R", "I", "S"))

        ggplot(tier_resistance_data, aes(x = "", y = Percents, fill = ResistanceLevel)) +
        geom_bar(width = 1, stat = "identity") +
        coord_polar("y", start = 0) +
        theme_void() +
        scale_fill_manual(values = c("R" = "red", "I" = "blue", "S" = "green")) +
        labs(title = paste("Resistance Levels for Tier", tier), fill = "Resistance Level")
    }

    output$allResistancePlots <- renderPlot({
        # Create individual pie charts
        pie_chart_tier_1 <- create_pie_chart(1)
        pie_chart_tier_2 <- create_pie_chart(2)
        pie_chart_tier_3 <- create_pie_chart(3)
        
        # Arrange the pie charts side by side
        grid.arrange(pie_chart_tier_1, pie_chart_tier_2, pie_chart_tier_3, ncol = 3)
    })

    output$app1_content <- renderUI({
        if (input$navbar == "resistanceTab") {
            app1UI("antibioticModule1")
        }
    })
    Resistanceserver("antibioticModule1")

    # Logic for App 2
    output$app2_content <- renderUI({
        if (input$navbar == "locationTab") {
            app2UI("LocationModule1")
        }
    })
    location_server("LocationModule1")

    output$app3_content <- renderUI({
        if (input$navbar == "tierTab") {
            app3UI("TierModule1")
        }
    })
    tierTrend_server("TierModule1")

    output$app4_content <- renderUI({
        if (inpurt$navbar == "litReview"){
            app4UI("LitReviewModule1")
        }
    })

}

# Run the app
shinyApp(ui, server)

