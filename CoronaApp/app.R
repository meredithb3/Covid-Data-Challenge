library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)
library(ggthemes)
library(shinydashboard)

covidData <- read.csv("coronacounties.csv")
povertyData <- read.csv("PovertyEstimates.csv")
populationData <- usmap::countypop
populationData$fips <- as.integer(populationData$fips)

data <- merge(covidData, povertyData, by.x = 'fips', by.y = 'FIPStxt')

state_cases <- data %>%
    filter(date == "2020-04-15") %>%
    group_by(state) %>%
    summarise(total_cases = sum(cases))

county_cases <- data %>%
    filter(date == "2020-04-15")

county_cases <- merge(x = populationData, y = county_cases, by = "fips", all = TRUE)

county_cases$log_cases <- log(county_cases$cases)
county_cases$cases[is.na(county_cases$cases)] <- 0
county_cases$log_cases[is.na(county_cases$log_cases)] <- 0

county_cases <- county_cases %>%
    mutate(casesPC = cases/pop_2015)
county_cases$log_casesPC <- log(county_cases$casesPC)
county_cases$casesPC[is.na(county_cases$casesPC)] <- 0
county_cases$log_casesPC[is.na(county_cases$log_casesPC)] <- 0


# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
                
    dashboardHeader(title = "COVID-19 Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction and Purpose", tabName = "intro", icon = icon("home")),
            menuItem("The Data", tabName = "data", icon = icon("table")),
            menuItem("Exploring the Data", tabName = "eda", icon = icon("search")),
            menuItem("Creating Our Model", tabName = "model", icon = icon("user-md")),
            menuItem("Model Analysis", tabName = "analysis", icon = icon("chart-line")),
            menuItem("Conclusion", tabName = "conclusion", icon = icon("poll-h"))
        )
    ),
    
    #body of pages
    dashboardBody(
        tabItems(
            
            #intro tab contents
            tabItem(tabName = "intro",
                    fluidRow(
                        box(title = "Background Information", status = "primary", solidHeader = TRUE,
                            
                            )
                    )
            ),
            
            #data tab contents
            tabItem(tabName = "data",
                    fluidRow(
                        box(title = "box1", status = "primary", solidHeader = TRUE,
                            
                            )
                    )
            ),
            
            #eda tab contents
            tabItem(tabName = "eda",
                    fluidRow(
                        box(title = "edabox", status = "primary", solidHeader = TRUE,
                            
                        )
                    )
            ),
            
            #model tab contents
            tabItem(tabName = "model",
                    fluidRow(
                        box(title = "box1model", status = "primary", solidHeader = TRUE,
                            
                        )
                    )
            ),
            
            #analysis tab contents
            tabItem(tabName = "analysis",
                    fluidRow(
                        box(title = "box1analysis", status = "primary", solidHeader = TRUE,
                            
                        )
                    )
            ),
            
            #conclusion tab contents
            tabItem(tabName = "conclusion",
                    fluidRow(
                        box(title = "box1conclusion", status = "primary", solidHeader = TRUE,
                            
                        )
                    )
            )
            
        )
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
