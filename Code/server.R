#install.packages("shinythemes")
#install.packages("RColorBrewer")
#install.packages("sf")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("hrbrthemes")
#install.packages("viridis")
#install.packages("ggiraph")
#devtools::install_github("hrbrmstr/streamgraph")   #installation for streamgraph
                                                    #(Choose 'All' when asked for option while installing this package)

#Loading all the required libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(ggplot2)
library(plotly)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(streamgraph)
library(devtools)
library(ggiraph)

# Read the shape file library and convert it into spatial data objects using sf library.

shp <- st_read("TM_WORLD_BORDERS_SIMPL-0.3.shp")
world_spdf <- as(shp, "Spatial")

#import and load the Happiness data table and mental health merged dataset
myData <- read.csv('HappinessData_Vizualization.csv')
facData <- read.csv('HappinessData_Factors.csv')
barData <- read.csv('Graph.csv')

#remane the colname as Country in facdata
colnames(facData)[1] <- "Country"

#data for click functionality on map
test <- read.csv("test.csv")

shinyServer(function(input, output, session) {
  #plot the Tab 1 map with happiness rank using Leaflet
  
  output$mainMap <- renderLeaflet({
    #filter the dataset according to the input year
    myData <- myData[myData$Year == input$year, ]
    
    #merge data for choropleth
    world_spdf@data = data.frame(world_spdf@data, myData[match(world_spdf@data[, "NAME"],
                                                               myData[, "Country"]), ])
    
    #create bins for colorbin for choropleth and create the palette
    mybins <- c(0, 4, 5, 6, 7, Inf)
    mypalette <-
      colorBin(
        palette = "RdYlGn",
        domain = world_spdf$Happiness.Score,
        na.color = "transparent",
        bins = mybins
      )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ",
      world_spdf$NAME,
      "<br/>",
      "Happiness Rank: ",
      world_spdf$Happiness.Rank,
      "<br/>",
      "Happiness Score: ",
      round(world_spdf$Happiness.Score, 2),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Tab 1 Map using leaflet
    leaflet(world_spdf) %>%
      addProviderTiles(providers$Wikimedia) %>%
      setView(lat = 10,
              lng = 0 ,
              zoom = 1.5) %>%
      addPolygons(
        fillColor = ~ mypalette(Happiness.Score),
        stroke = TRUE,
        opacity = 1,
        fillOpacity = 0.9,
        color = "grey",
        dashArray = "15",
        weight = 0.3,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          opacity = 0.9,
          direction = "auto"
        )
      ) %>% #plot the legend
      addLegend(
        pal = mypalette,
        values =  ~ Happiness.Score,
        opacity = 0.9,
        title = "Happiness Score",
        position = "bottomleft"
      ) %>%
      addControl(html = "<img src= 'rating.png'/>", position = "bottomright") #insert the smiley image
  })
  
  #observe function for map click zooming
  observe({
    click <- input$mainMap_shape_click
    proxy <- leafletProxy("mainMap")
    if (is.null(click))
      return()
    proxy %>% setView(lng = click$lng,
                      lat = click$lat,
                      zoom = 3)
  })
  
  #plot the top 10 countries using lollipop plot using plotly
  output$Topranks <- renderPlotly({
    #filter data for top 10
    myData <- myData[myData$Year == input$year, ]
    myDataT = arrange(myData, Happiness.Rank)
    myDataT = myDataT[1:10, ]
    
    #text for lollipop chart heading
    topText <- reactive({
      paste("Country rankings for the year ", input$year)
    })
    # Return the formula text for printing as a caption for the first plot
    output$Top <- renderText({
      topText()
    })
    
    
    
    plot1 <- myDataT %>%
      arrange(-Happiness.Rank) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      mutate(Country = factor(Country, levels = Country)) %>%   # This trick update the factor levels
      ggplot(aes(
        x = Country,
        y = Happiness.Rank,
        text = paste("Country: ", Country,
                     "<br>Happiness Rank: ", Happiness.Rank)
      )) +
      geom_segment(aes(xend = Country, yend = 1), color = "forestgreen") +
      geom_point(
        size = 3,
        color = "forestgreen",
        fill = alpha("orange", 0.5),
        alpha = 1,
        shape = 21,
        stroke = 1
      ) +
      coord_flip() +
      theme_bw() +
      xlab("")
    
    ggplotly(plot1, tooltip = "text")
  })
  
  #plot the bottom 10 countries using lollipop plot using plotly
  output$Bottomranks <- renderPlotly({
    #filter data for bottom 10
    myData <- myData[myData$Year == input$year, ]
    myDataB = arrange(myData, desc(Happiness.Rank))
    myDataB = myDataB[1:10, ]
    
    max(myDataB$Happiness.Rank, na.rm = TRUE)
    
    #lollipop plot
    plot2 <- myDataB %>%
      arrange(Happiness.Rank) %>%    # First sort by val. This sort the dataframe but NOT the factor levels
      mutate(Country = factor(Country, levels = Country)) %>%   # This trick update the factor levels
      ggplot(aes(
        x = Country,
        y = Happiness.Rank,
        text = paste("Country: ", Country,
                     "<br>Happiness Rank: ", Happiness.Rank)
      )) +
      geom_segment(aes(xend = Country, yend = (
        max(myDataB$Happiness.Rank, na.rm = TRUE)
      ) - 9),
      color = "#DC143C") +
      geom_point(
        size = 3,
        color = "#DC143C",
        fill = alpha("orange", 0.5),
        alpha = 1,
        shape = 21,
        stroke = 1
      ) +
      coord_flip() +
      theme_bw() +
      xlab("")
    
    ggplotly(plot2, tooltip = "text") #tooltip
    
  })
  
  #Tab 2 map for factors using leaflet
  output$factorMap <- renderLeaflet({
    #filter data for 2019 year
    myDataF <- myData[myData$Year == 2019, ]
    
    #merge data for choropleth
    world_spdf@data = data.frame(world_spdf@data, myDataF[match(world_spdf@data[, "NAME"],
                                                                myDataF[, "Country"]), ])
    #reactive text for map heading
    factorText <- reactive({
      paste("Distribution of the ",
            input$factors,
            " around the globe in 2019")
    })
    # Return the formula text for printing
    output$caption1 <- renderText({
      factorText()
    })
    
    #create bins according to the happiness factors
    if (input$factors == "Economy..GDP.per.Capita." ||
        input$factors == "Family.Support") {
      mybins <- c(0, 0.50, 1, 1.5, Inf)
    }
    
    else if (input$factors == "Health.Life.Expectancy") {
      mybins <- c(0, 0.25, 0.50, 0.75, 1, Inf)
    }
    
    else if (input$factors == "Freedom") {
      mybins <- c(0, 0.2, 0.4, 0.6, Inf)
    }
    
    else if (input$factors == "Absence.of.Corruption" ||
             input$factors == "Generosity") {
      mybins <- c(0, 0.1, 0.2, 0.3, 0.4, Inf)
    }
    
    #create palette for choropleth
    mypalette <-
      colorBin(
        palette = "YlGn",
        domain = as.numeric(world_spdf[[input$factors]]),
        na.color = "transparent",
        bins = mybins
      )
    
    # Prepare the text for tooltips:
    mytext <- paste(
      "Country: ",
      world_spdf$NAME,
      "<br/>",
      "Happiness Rank: ",
      world_spdf$Happiness.Rank,
      "<br/>",
      input$factors,
      ": " ,
      as.numeric(world_spdf[[input$factors]]),
      sep = ""
    ) %>%
      lapply(htmltools::HTML)
    
    # Tab 2 Map
    leaflet(world_spdf) %>%
      addProviderTiles(providers$Wikimedia)  %>%
      setView(lat = 10,
              lng = 0 ,
              zoom = 1.5) %>%
      addPolygons(
        fillColor = ~ mypalette(as.numeric(world_spdf[[input$factors]])),
        stroke = TRUE,
        opacity = 1,
        fillOpacity = 0.9,
        color = "grey",
        dashArray = "15",
        weight = 0.3,
        highlight = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = mytext,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "13px",
          opacity = 0.9,
          direction = "auto"
        ),
        layerId = ~ NAME
      ) %>% #plot the legend
      addLegend(
        pal = mypalette,
        values =  ~ as.numeric(world_spdf[[input$factors]]),
        opacity = 0.9,
        title = input$factors,
        position = "bottomleft"
      )
    
  })
  #observe function for map click and select the country clicked
  observe({
    click = input$factorMap_shape_click
    sub = facData[facData$Country == input$factorMap_shape_click$id, c("Factors", "Values", "Country")]
    Factors = sub$Factors
    Values = sub$Values
    Country = sub$Country
    
    if (is.null(click))
      return()
    else
      
      #polar plot for the selected country
      output$polar <- renderPlot ({
        ggplot(data = sub,
               aes(x = Factors, fill = Factors, y = Values),
               width = 1) +
          geom_col_interactive(aes(
            x = Factors,
            fill = Factors,
            y = Values,
            tooltip = Values
          ),
          width = 1) +
          # geom_hline(yintercept = seq(0, 1, by = .1),
          #            color = "white",
          #            size = .5) +
          geom_vline(
            xintercept = seq(.5, 8.5, by = 1),
            color = "white",
            size = .5
          ) +
          scale_fill_brewer(
            palette = "Set1",
            labels = c(
              "Economy",
              "Freedom",
              "Family Support",
              "Absence of Corruption",
              "Generosity",
              "Health"
            )
          ) +
          coord_polar() +
          theme_classic() +
          theme(
            axis.ticks.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.x = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank()
          ) +
          xlab("Subjective Wellbeing Factors") +
          ylab("") +
          scale_y_discrete(limits = c("0", "2", "4", "6")) + #define fixed limits
          ggtitle("Relative Overview of the Happiness Factors", Country) #title for the plot
        
      })
    
  })
  
  #observe function for zooming on map click
  observe({
    click <- input$factorMap_shape_click
    proxy <- leafletProxy("factorMap")
    if (is.null(click))
      return()
    proxy %>% setView(lng = click$lng,
                      lat = click$lat,
                      zoom = 3)
  })
  
  #plot for streamgraph
  output$streamgraph <- renderStreamgraph ({
    #filter data by the country selected
    facData <- facData[(facData$Country == input$country) ,]
    #Text for happiness rank
    output$streamText <- renderUI({
      a <- facData %>% filter(Year == 2019) %>%
        filter(Country == input$country) %>%
        select(Happiness.Rank)
      
      HTML(paste("Happiness Rank for 2019: ", unique(a$Happiness.Rank)))
      
    })
    # Stream graph with a legend
    streamgraph(
      facData,
      key = "Factors",
      value = "Values",
      date = "Year",
      interactive = TRUE
    ) %>%
      sg_axis_x("Year") %>%
      sg_legend(show = TRUE, label = "Factors: ")
    
    
  })
  
  #plot for Tab 3 diverging barpolot using plotly
  output$mental <- renderPlotly ({
    barData <- barData[(barData$seek_help == "Yes"),]
    
    #prepare data for plotting diverging barplot
    barData <- barData %>%
      mutate(Value = ifelse(
        Type == " Employer Support for Mental Health (% YES)",
        Value,-1 * Value
      ))
    breaks_values <- pretty(barData$Value)
    
    #plot the diverging barplot
    barData %>%
      mutate(Country = reorder(Country, Value)) %>%
      ggplot(aes(x = Country, y = Value, fill = Type)) +
      geom_hline(yintercept = 0) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_y_continuous(breaks = breaks_values,
                         labels = abs(breaks_values)) +
      theme_minimal() +
      scale_fill_manual(values = c("mediumseagreen", "darkgreen")) +
      #scale_fill_manual(values = c("#bf812d", "#35978f"))+
      xlab("") +
      ylab("Happiness Rank                                Employer Support for Mental Health (% YES)")
    
    
  })
  
})