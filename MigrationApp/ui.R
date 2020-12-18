#
# User Interface for Refugee Migration Dashboard
# 

library(shiny)
library(plotly)
library(shinydashboard)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    
    # Application title
    titlePanel(h1("Refugee Migration", align = "center")),
    
    tabsetPanel(
        
        tabPanel( "Profiler",
                  fluid = TRUE,
                  sidebarLayout(
                      sidebarPanel(
                          
                          sliderInput("years",
                                      "Timeperiod:",
                                      min = 1975,
                                      max = 2016,
                                      sep = "",
                                      value = c(2000,2016)
                                      ),
                          
                          hr(),
                          
                          checkboxInput("countryyes", "View an Individual Country?"),
                          
                          conditionalPanel(condition='input.countryyes',
                                           selectInput("country", 
                                                       "Select Country:", 
                                                       choices=NULL)
                                           )
                          ),
                      
                      mainPanel(
                          fluidRow(
                              valueBoxOutput(width=10, "totalnumber"),
                              ),
                          hr(),
                          br(),
                          fluidRow(
                              splitLayout(cellWidths = c("50%", "50%"), plotlyOutput("yearPlot"), plotlyOutput("destPlot"))
                          )
                          )
                      )
        ),
        
        tabPanel( "Movement",
                  fluid = TRUE,
                  titlePanel(h3("Since 2000, 186 Million Refugees Have Found Asylum", align = "center")),
                  fluidRow(
                      br(),
                      splitLayout(cellWidths = c("50%", "50%"), plotOutput("originMap"), plotOutput("residenceMap")
                  ),
                  br(),
                  br(),
                  fluidRow(
                      column(2),
                      column(8,
                              p("These charts do not indicate that refugees have overwhelmingly fled to European countries in recent years, signaling that the influx of asylum seekers entering European countries may not actually be the preeminent crisis worldwide. A wholistic discussion of refugees must place significant focus on countries like Pakistan, which hosts the majority of refugees from Afganistan, or Lebanon which has a significantly higher concentration of refugees per capita then any other country."),
                             p("It is important to note that this visualization is based on the raw number of refugees leaving and living in each country, and is not scaled based on the population of each country. For instance, the chart above shows that Germany hosts significantly more refugees than Lebanon, which is true, but Lebanon hosts 208 refugees per 1,000 inhabitants compared to Germany which hosts only 3 refugees per 1,000 inhabitants. \n\n"),
                             p("The official definition of ‘refugee’, as defined by the 1951 Refugee Convention, is as follows:"),
                             p("The term ‘refugee’ shall apply to any person who … owing to well-founded fear of being persecuted for reasons of race, religion, nationality, membership of a particular social group or political opinion, is outside the country of his nationality and is unable or, owing to such fear, is unwilling to avail himself of the protection of that country; or who, not having a nationality and being outside the country of his former habitual residence, is unable or, owing to such fear, is unwilling to return to it.” (UNHCR, 1951 Refugee Convention)",
                               style = "font-family: 'times'"),
                             p("The visualizations in on this website refer only to people who have qualified as refugees and now live under the protection of the United Nations High Commissioner for Refugees (UNHCR) system. The definition of refugee however is relatively narrow and excludes millions of displaced people worldwide. Since a refugee is defined as someone outside the country of their nationality, the definition excludes an estimated 41.3 million internally displaced persons worldwide, who have been forced to flee their homes but who remain within their country’s borders. It also excludes as estimated 3.5 million asylum seekers, who have fled their home countries but have yet to be accepted for refugee status. The 1951 Refugee Convention also explicitly excludes Palestinians. 5.5 million displaced Palestinians instead fall under the jurisdiction of the United Nations Relief and Works Agency (UNRWA), an organization that does not seek to resettle displaced Palestinians, but rather provide them with social services where they are. All said, the 20.5 million refugees represented in the visualizations above account for less than 30% of all the displaced people worldwide.")
                             )
                      )
                  )
        )
        
    )
))
