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
                  sidebarLayout(
                      sidebarPanel(
                          sliderInput("years2",
                                      "Timeperiod:",
                                      min = 1975,
                                      max = 2016,
                                      sep = "",
                                      value = c(2000,2016)
                          ),
                      ),
                      mainPanel(
                          # plotOutput("originMap"),
                          # plotOutput("residenceMap")
                      )
                  )
        )
        
    )
))
