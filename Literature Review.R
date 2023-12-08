library(shiny)

# Define UI for Literature Review Page
literatureReview_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    # CSS
    tags$head(
      tags$style(type="text/css", "#literature-review { background-color: #a9c5d1; padding: 20px; border-radius: 5px; margin-bottom: 40px; }"),
      tags$style(type="text/css", "#review-container { max-width: 1124px; margin: auto; border-radius: 5px; padding: 20px; background-color: #bfe0c3; }"),
      tags$style(type="text/css", "#review-content { font-size: 16px; line-height: 1.6; color: #333; }"),
    ),
    
    # Main Header
    titlePanel(title=h1("Literature Review: Understanding Antimicrobial Resistance And Why It Is A Concern", align="center")),
    
    hr(),
    
    # Main Content
    div(id="literature-review",
        div(id="review-container",
            div(id="review-content",
                h3("What is Antimicrobial Resistance?"),
                p("Antimicrobial resistance (AMR) is an increasingly pressing issue in both human and animal
                  medicine. This literature review aims to introduce the reader to the basics of amt and why it is
                  important to understand.
                  Antimicrobial resistance occurs when microbes such as bacteria, viruses, and fungi develop
                  resistance to the effects of antibiotics, and if antimicrobial resistance increases, then the health
                  systems dealing with resistant microbes become less effective at curing subjects, unless they
                  use higher potency antibiotics for which less microbes have developed resistance. This situation
                  resistance often arises from the overuse/misuse of antimicrobials in healthcare and agriculture,
                  and making changes to reduce this overuse/mise is very important to reduce the likelihood of
                  microbes developing more resistances (Laxminarayan et al., 2013)"),
                p("Microbes like bacteria and fungi are developing resistances to traditional antibiotics. This 
                means that infections are becoming harder to treat as our antimicrobials become less effective. There are
                2.8million AMR infections each year and about 1.27million related deaths (Dadgostar, 2019)."),
                h3("Why is Antimicrobial Resistance Concerning?"),
                p("in addition to human infections, AMR limits treatment options for animal infections, which can
                  have downstream effects on humans by increasing resistance in livestock due in part to raises
                  concerns about food safety (Thanner, Drissner, & Walsh, 2016).
                  Additionally, there is also data pointing towards interspecies susceptibility between humans and
                  animals (such as companion animals like cats and dogs) which suggests that the AMR problem
                  needs to be addressed at various levels, not just the human level
                  ((Thanner, Drissner, & Walsh, 2016). In addition to the health implications,
                  AMR puts stress on healthcare systems and economies since treating resistant infections
                  overall requires more costly care.It increases healthcare expenses and economic pressures,
                  and the negative effects of these pressures ripples out into various other parts of the economy
                  areas like agriculture, food quality, animal health care, and more (O'Neill, 2016)."),
                h3("Conclusion"),
                p("In conclusion, rising AMR is not in the best interests of anyone in either animal or human health.
                  There are many different ways AMR affects us and many different proposed solutions, and in
                  our dashboard, we are aiming to provide a solution that will prove useful for doctors and lay persons
                  who wish to take a data driven approach when identifying which antimicrobials to use."),
                hr(),
                p("References:"),
                p("Laxminarayan, R., Duse, A., Wattal, C., et al. (2013). Antibiotic resistance—the need for global 
                  solutions. The Lancet Infectious Diseases, 13(12), 1057-1098. https://doi.org/10.1016/S1473-3099(13)70318-9",
                  br(), br(),
                  "Thanner, S., Drissner, D., & Walsh, F. (2016). Antimicrobial Resistance in Agriculture. 
                  mBio, 7(2). American Society for Microbiology. https://doi.org/10.1128/mbio.02227-15",
                  br(), br(),
                  "O'Neill, J. (2016). Tackling drug-resistant infections globally: final report and recommendations. 
                  Government of the United Kingdom. https://apo.org.au/node/63983",
                  br(), br(),
                  "Minor, S. B., Rafferty, K. D., Rutkowski, J., Allison, S., & Herrera, V. (2019). 1071. 
                  Implementation and impact of an antimicrobial tier structure along with prospective audit and feedback 
                  at a large health system: Collaborations for care transformation. Open Forum Infectious Diseases, 
                  6(Supplement_2). https://doi.org/10.1093/ofid/ofz360.935",
                  br(), br(),
                  "Dadgostar, P. (2019). Antimicrobial Resistance: Implications and Costs. 
                  Infection and Drug Resistance, 12, 3903-3910. https://doi.org/10.2147/IDR.S234610")
            )
        )
    )
  )
}

# Include server function because UI will not run without
literatureReviewServer <- function(input, output, session) {}

shinyApp(ui = literatureReview_ui, server = literatureReviewServer)