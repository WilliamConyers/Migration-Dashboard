#
# Server for Refugee Migration Dashboard
# 

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output,session) {
    
    #read in data from local file
    path <- "refugees.csv"
    data <- read_csv(path)[,2:5] 
    
    #populate country will all the possible country choices
    observeEvent(input$countryyes,{
        if(input$countryyes) {
            mychoices <- unique(c(data$Residence, data$Origin))
            updateSelectInput(session, "country", choices=c("All Countries", mychoices))
        }
    })
    
    #get current country from selectInput
    country <- reactive({
        input$country
    })
    
    #plot of yearly refugee totals
    output$yearPlot <- renderPlotly({
        
        #transform data set to get refugee totals my year, in the specified data range
        #Different charts for if an individual country is specified
        if (input$countryyes & input$country != "All Countries") {
            currentcountry <- country()
            data1 <- data %>%
                filter(Origin==currentcountry) %>%
                group_by(Year) %>%
                summarise(YearTotal = sum(Refugees, na.rm = T)) %>%
                filter(Year >= input$years[1],
                       Year <= input$years[2])
        } else {
            data1 <- data %>%
                group_by(Year) %>%
                summarise(YearTotal = sum(Refugees, na.rm = T)) %>%
                filter(Year >= input$years[1],
                       Year <= input$years[2])
        }
        
        #Make plot in plotly
        plot_ly(data=data1,
                type="bar",
                # line = list(width=2, color="firebrick"),
                marker = list(color = "firebrick"),
                opacity = .7,
                x = ~Year,
                y = ~YearTotal,
                text=paste0('Year: ', data1$Year, '<br>',
                            'Total Refugees: ', formatC(data1$YearTotal, format="d", big.mark=","), '<br>'),
                hoverinfo = 'text',
                hoverlabel = list(bgcolor="white", bordercolor="firebrick")
                ) %>%
            layout(title=list(text="Yearly Refugee Counts"),
                   yaxis=list(title="Total Refugees"),
                   xaxis=list(title="Year")
                   )
    })
    

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })

})
