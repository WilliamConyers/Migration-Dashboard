#
# Server for Refugee Migration Dashboard
# 

library(shiny)
library(plotly)
library(tidyverse)
library(maps)
library(mapproj)
library(shinydashboard)

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
                marker = list(color = "firebrick"),
                opacity = .7,
                x = ~Year,
                y = ~YearTotal,
                text=paste0('Year: ', data1$Year, '<br>',
                            'Total Refugees: ', formatC(data1$YearTotal, format="d", big.mark=","), '<br>'),
                hoverinfo = 'text',
                hoverlabel = list(bgcolor="white", bordercolor="firebrick")
                ) %>%
            layout(title=list(text="Yearly Refugee Counts", xref="paper"),
                   yaxis=list(title="Total Refugees"),
                   xaxis=list(title="Year")
                   )
    })
    
    
    #plot of destination countries
    output$destPlot <- renderPlotly({
        
        if (input$countryyes & input$country != "All Countries") {
            currentcountry <- country()
            data2 <- data %>%
                filter(Origin==currentcountry) %>%
                filter(Year >= input$years[1],
                       Year <= input$years[2]) %>%
                group_by(Residence) %>%
                summarise(destTotals = sum(Refugees)) %>%
                arrange(-destTotals)
        } else {
            data2 <- data %>%
                filter(Year >= input$years[1],
                       Year <= input$years[2]) %>%
                group_by(Residence) %>%
                summarise(destTotals = sum(Refugees)) %>%
                arrange(-destTotals)
        }
        
        #make top 10 and then other
        data3 <- data2 %>% top_n(10)
        othervalue = sum(data2$destTotals, na.rm=TRUE) - sum(data3$destTotals, na.rm=TRUE)
        data4 <- add_row(data3, Residence="Other",destTotals=othervalue)
        
        #Make plot in plotly
        plot_ly(data=data4,
                type="pie",
                values = ~destTotals,
                labels = ~Residence) %>%
            layout(title=list(text="Where Refugees Went", xref = "paper"),
                   legend = list(orientation = "h",   # show entries horizontally
                                 xanchor = "center",  # use center of legend as anchor
                                 x = 0.5))             # put legend in center of x-axis)
    })
    
    output$totalnumber <- renderValueBox({
        startyear = input$years[1]
        endyear = input$years[2]
        data5 <- data %>%
            filter(Year >= startyear,
                   Year <= endyear)
        if (input$countryyes & input$country != "All Countries") {
            currentcountry <- country()
            data6 <- data5 %>%
                filter(Origin == currentcountry)
            numref = sum(data6$Refugees, na.rm = TRUE)
            region = currentcountry
        } else {
            numref = sum(data5$Refugees, na.rm = TRUE)
            region = "All Countries"
        }
        valueBox(
            value = formatC(numref, format="d", big.mark=","),
            subtitle = paste0("Number of Refugees From ", region, " between ", startyear, " and ", endyear, "."),
            color = "yellow"
        )
    })


    
    # 
    # 
    # 
    # 
    # 
    # #read in data for world map
    # world <- map_data("world")
    # 
    # #originMap is a world chloroplast map showing number of refugees coming from each country.
    # output$originMap <- renderPlot({
    # 
    #     #make data set for Origin map
    #     dataOrigin = data %>%
    #         group_by(Origin, Year) %>%
    #         summarise(Generated = sum(Refugees, na.rm=TRUE)) %>%
    #         filter(Year >= input$years2[1],
    #                Year <= input$years2[2])
    #     mapOrigin <- left_join(world,dataOrigin,by=c("region"="Origin"))
    # 
    #     #make map
    #     ggplot(mapOrigin, aes(x=long, y=lat, group=group, fill=Generated)) +
    #         geom_polygon(color = "white", size=0.2) +
    #         theme_void() +
    #         scale_fill_gradient(low="white",
    #                             high="firebrick",
    #                             trans = "log") +
    #         labs(title="Refugees Produced by Country",
    #              subtitle = "",
    #              fill="Number of \nRefugees") +
    #         theme(plot.title = element_text(hjust = 0.5, size = 15),
    #               legend.position = c(0.15, 0.35))
    # 
    # })
    # 
    # #residenceMap is a world chloroplast map showing number of refugees accepted by each country.
    # output$residenceMap <- renderPlot({
    # 
    #     #make data set for Origin map
    #     dataResidence = data %>%
    #         group_by(Residence, Year) %>%
    #         summarise(Accepted = sum(Refugees, na.rm=TRUE)) %>%
    #         filter(Year >= input$years2[1],
    #                Year <= input$years2[2])
    #     mapResidence <- left_join(world,dataResidence,by=c("region"="Residence"))
    # 
    #     #make map
    #     ggplot(mapResidence, aes(x=long, y=lat, group=group, fill=Accepted)) +
    #         geom_polygon(color = "white", size=0.2) +
    #         theme_void() +
    #         scale_fill_gradient(low="white",
    #                             high="blue",
    #                             trans = "log") +
    #         labs(title="Refugees Accepted by Country",
    #              subtitle = "",
    #              fill="Number of \nRefugees") +
    #         theme(plot.title = element_text(hjust = 0.5, size = 15),
    #               legend.position = c(0.15, 0.35))
    # 
    # })

})
