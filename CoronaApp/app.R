library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)
library(ggthemes)

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
ui <- fluidPage(

)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
