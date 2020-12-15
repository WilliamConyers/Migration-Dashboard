#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

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
                          selectInput("Country",
                                      "Select Country:",
                                      choices = NULL),
                          ),
                      mainPanel(
                          plotlyOutput("yearPlot")
                          )
                      )
        ),
        
        tabPanel( "Second Tab",
                  fluid = TRUE,
                  sidebarLayout(
                      sidebarPanel(
                          sliderInput("bins",
                                      "NUMBER of bins:",
                                      min = 1,
                                      max = 50,
                                      value = 30)
                      ),
                      mainPanel(
                          plotOutput("distPlot")
                      )
                  )
        )
        
    )
))
