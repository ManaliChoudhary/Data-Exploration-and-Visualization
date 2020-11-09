#install.packages("shinythemes")
#install.packages("tidyverse")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("shinycssloaders")

#Loading all the required libraries
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(leaflet)
library(RColorBrewer)
library(sf)
library(tidyverse)
library(ggplot2)
library(plotly)
library(streamgraph)
library(devtools)
library(ggiraph)

#dataset for country list for dropdown in Tab 2
df <- read.csv('Country.csv')
country <- df$Country


shinyUI(fluidPage(
  theme = shinytheme("united"),
  
  navbarPage(
    "In the Pursuit of Happiness",
    
    #Home Tab
    tabPanel(
      "Home",
      #Home tab with image
      img(
        src = 'home.jpg',
        align = "centre",
        height = "100%",
        width = "100%"
      )
    ),
    #Tab 1
    tabPanel(
      "Global State of Happiness",
      h1("Do you think some nations are happier than others?"),
      h4 (HTML("Well, the ","<b>","World Happiness Report","</b>"," proposes so!
         You can trust this data acquired from the World Happiness Report annual publications of the ",
               "<b>","United Nation (UN)'s Sustainable Development Solutions Network","</b>")),
      sidebarPanel(
        #slider input for year
        sliderInput("year",
                    "Slide through the Years:" ,
                    2015,
                    2019,
                    2015),
        h3(textOutput("Top")),
        tabsetPanel(
          #tab for ranks
          tabPanel("Happiest 10",
                   plotlyOutput(outputId = "Topranks",
                                height = 520),),
          tabPanel(
            "Saddest 10",
            plotlyOutput(outputId = "Bottomranks",
                         height = 520)
          )
        ),
        width = 4,
        
      ),
      
      mainPanel(
        #main panel for map in Tab 1
        h4(
          "Hover on the map to know more and click to focus!" ,
          style = "color : #FF4500",
          align = "right"
        ),
        leafletOutput(outputId = "mainMap",
                      height = "700") %>% withSpinner(color = "#FF4500"),
      ),
      
    ),
    #Tab 2
    tabPanel(
      "Happiness Factors",
      h1("Can Money buy Happiness?"),
      h4(
        " Lets find out with the help of these Subjective Wellbeing Factors contributing to the Happiness Score.."
      ),
      sidebarPanel(
        selectInput(
          #dropdown for choosing factors
          "factors",
          "Choose a Happiness Factor: ",
          c(
            "Economy (GDP / Capita)" = "Economy..GDP.per.Capita.",
            "Family Support" = "Family.Support",
            "Health (Life Expectancy)" = "Health.Life.Expectancy",
            "Freedom" = "Freedom",
            "Absence of Corruption" = "Absence.of.Corruption",
            "Generosity" = "Generosity"
          )
        ),
        h4(
          "Click on the map to know more about that country for the year 2019!" ,
          style = "color : #FF4500"
        ),
        plotOutput(#plot the polar area plot
          outputId = "polar",
          height = "600"),
        
        
        width = 4
      ),
      
      mainPanel(
        h2(textOutput("caption1")),
        leafletOutput(#map for factors in Tab 2
          outputId = "factorMap",
          
          height = "700") %>% withSpinner(color = "#FF4500"),
        h4("Scroll down to see more", style = "color : #FF4500", align = "left"),
        br(),
        br(),
        
        #panel for the streamgraph and the dropdown of countries for input
        
        h2(
          "Check how your country has performed by the Subjective Wellbeing Factors since 2015"
        ),
        selectInput("country", "Select a Country", country),
        h4(htmlOutput("streamText")),
        h4(
          "Hover on the colored area to know more",
          style = "color : #FF4500",
          align = "right"
        ),
        streamgraphOutput(outputId = "streamgraph")
      ),
    ),
    #Tab 3
    tabPanel(
      "Mental Health Wellbeing",
      h1("Is Happiness a State of Mind?"),
      
      #narrative for the diverging barplot
      
      h4(
        HTML(
          "Surprisingly, there is a relation between the Happiest countries in the world and how seriously they take
            Mental Health Wellbeing for their citizens. According to a Survey on Mental Health Illness at Workplace (2014-15),
            the only countries responding positively for" ,
          "<b>",
          "<em>Whether Mental Health Support is provided by the Employers in their country</em>",
          "</b>",
          " ,were the countries majorly among the Happiest 20 in the world."
        )
      ),
      br(),
      h3(
        HTML(
          "So, What is your ",
          "<b>",
          "<em>Definition for Happiness?</em>",
          "</b>"
        )
      ),
      br(),
      h4(
        HTML(
          "Hover on the left side bars to know the Happiness Ranks of the countries                                    Hover on the right side bars to know the level of Employer support in that country"
        ),
        style = "white-space: pre-wrap ; color : #FF4500"
      ),
      mainPanel(plotlyOutput(
        #plot output for the diverging graph
        outputId = "mental",
        width = "1500",
        height = "500"
        
      ))
      
    )
  )
))
