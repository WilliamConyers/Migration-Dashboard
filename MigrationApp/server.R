#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    #read in data from local file
    path <- "refugees.csv"
    data <- read_csv(path)[,2:5] 
    
    output$yearPlot <- renderPlotly({
        
        #transform data set to get refugee totals my year, in the specified data range
        data1 <- data %>%
            group_by(Year) %>%
            summarise(YearTotal = sum(Refugees, na.rm = T)) %>%
            filter(Year >= input$years[1],
                   Year <= input$years[2])
        
        #Make plot of yearly totals
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
