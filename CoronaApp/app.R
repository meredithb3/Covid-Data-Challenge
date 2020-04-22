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
demoData1 <- read.csv("stco-mr2010-1.csv")
demoData2 <- read.csv("stco-mr2010_mt_wy.csv")
demoData <- rbind(demoData1, demoData2)

white_people_by_county <- demoData %>% 
    filter(IMPRACE == 1, ORIGIN == 1) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_white = sum(RESPOP))

black_people_by_county <- demoData %>% 
    filter(IMPRACE == 2, ORIGIN == 1) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_black = sum(RESPOP))

asian_people_by_county <- demoData %>% 
    filter(IMPRACE == 4, ORIGIN == 1) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_asian = sum(RESPOP))

hispanic_people_by_county <- demoData %>% 
    filter(ORIGIN == 2) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_hispanic = sum(RESPOP))

other_by_county <- demoData %>% 
    filter(ORIGIN == 1, !(IMPRACE %in% c(1,2,4))) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_other = sum(RESPOP))

non_white_people_by_county <- demoData %>% 
    filter(IMPRACE !=1 | ORIGIN != 1) %>% 
    group_by(STATE, COUNTY) %>% 
    summarise(num_nonwhite = sum(RESPOP))

demographicsData <- full_join(full_join( full_join(white_people_by_county,
                                                   black_people_by_county),
                                         full_join(hispanic_people_by_county,
                                                   asian_people_by_county)),
                              full_join(other_by_county,
                                        non_white_people_by_county))

create_fips <- function(ste, cnty){
    if (cnty < 10){
        c <- paste("00",as.character(cnty),sep="")
    }
    else if (cnty<100){
        c <- paste("0",as.character(cnty),sep="")
    }
    else{
        c <- as.character(cnty)
    }
    
    if (ste<10){
        s <- paste("0",as.character(ste),sep="")
    }
    else{
        s <- as.character(ste)
    }
    return(as.integer(paste(s,c,sep="")[1]))
}

demographicsData['fips'] <- mapply(create_fips, demographicsData$STATE, 
                                   demographicsData$COUNTY)

demoDfPopulation <- demographicsData['num_white'] + demographicsData['num_nonwhite'] 

demographicsData['perc_white'] <- demographicsData$num_white/demoDfPopulation
demographicsData['perc_black'] <- demographicsData$num_black/demoDfPopulation
demographicsData['perc_hispanic'] <- demographicsData$num_hispanic/demoDfPopulation
demographicsData['perc_nonwhite'] <- demographicsData$num_nonwhite/demoDfPopulation

data <- merge(covidData, povertyData, by.x = 'fips', by.y = 'FIPStxt')
data <- merge(data, populationData, by.x = 'fips', by.y = 'fips')
data <- merge(data, demographicsData, by.x = 'fips', by.y = 'fips')

state_cases <- data %>%
    filter(date == "2020-04-15") %>%
    group_by(state) %>%
    summarise(total_deaths = sum(deaths))

state_capita_deaths <- data %>%
    filter(date == "2020-04-15") %>%
    group_by(state) %>%
    summarise(deaths_per_capita = sum(deaths)/sum(pop_2015))

county_deaths <- data %>%
    group_by(fips) %>%
    mutate(total_deaths = sum(deaths)) %>%
    mutate(deathsPC = total_deaths/pop_2015) %>%
    mutate(logDeathsPC = log(deathsPC)) %>%
    filter(date == "2020-04-15")

plot_poverty_data <- merge(populationData, povertyData, by.x = 'fips', by.y = 'FIPStxt')


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
                    box(title = "Introduction", status = "primary", solidHeader = TRUE,
                       htmlOutput(outputId = "introText")
                        ),
                    box(title = "Purpose", status = "primary", solidHeader = TRUE,
                        htmlOutput(outputId = "purposeText")
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
                        box(title = "Deaths per Capita Map", status = "primary", solidHeader = TRUE, height = 520,
                            plotOutput("countyDPC"), 
                            "Looking at our plot of deaths per capita by county, we see that there are some 
                            counties (namely in New York, Louisiana, and Michigan) that have relatively much
                            larger deaths per capita than most others."),
                        box(title = "Deaths per Capita Histogram", status = "primary", solidHeader = TRUE, height = 520,
                            plotOutput("histCDPC"),
                            "The histogram of deaths per capita by county confirms this as it has extreme right skew.
                            We should analyze the log of this variable to see if it has more even spread.", br()),
                        box(title = "Log Deaths per Capita Map", status = "warning", solidHeader = TRUE, height = 530,
                            plotOutput("countyLDPC"),
                            "Looking at this new plot of the log of deaths per capita by county, we see much more even 
                            spread of values. We should note that counties in black have zero deaths and thus we will 
                            only analyze counties with deaths as we cannot draw conclusions for counties that have not
                            experienced COVID-19 outbreaks."),
                        box(title = "Log Deaths per Capita Histogram", status = "warning", solidHeader = TRUE, height = 530,
                            plotOutput("histLDPC"),
                            "The histogram of the log of deaths per capita by county appears relatively nornal and symmetric.
                            This is in agreement with the more even spread of counties affected in the map to the right and 
                            will likely be good for modeling through our analysis portion.")
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

    output$introText <- renderText({
        txt <- "Arguably, the largest issue the world has had to face as a collective in modern times 
        is the issue of the novel coronavirus (COVID-19) running rampant throughout countries and leaving
        trails of bodies in its wake. Schools and workplaces have been shut down and people have been 
        restricted to their homes by government mandates throughout the United States. Economies have plummetted 
        and we are seeing the largest levels of unemployment that we have seen in ages. For our data analysis, 
        we decidedto perform a regression analysis on the death rate of COVID-19 on United States counties using 
        several characteristics as predictors."
        paste(txt)
    })
    output$purposeText <- renderText({
        txt <- "In the United States we know there is a large disparity of wealth and resources throughout 
        its citizens. As is with any major issue, certain communitites will likely be affected by a greater 
        degree than others. Due to issues like systemic racism, the opression of minorities, and a lack 
        of healthcare options availible to America's lower income brackets, it is very possible that these 
        communities will be ravaged by the impacts of COVID-19 significantly more than others with greater
        priviliges and resources. Using our analysis, we will hopefully be able to analyze which characteristics of a 
        community (we will treat counties as the 'community' level in question) lead to a higher death rate and thus 
        unequal impact."
    })
    output$countyDPC <- renderPlot({
        county_deaths_map <- plot_usmap("counties", data = county_deaths, values = "deathsPC", color = "black", size=0.05)
        
        county_deaths_map <- county_deaths_map + 
            scale_fill_gradient(low = "white", high = "limegreen", name = "Deaths per Capita") +
            labs(title = "Deaths per Capita by County on April 15, 2020") + 
            theme(legend.position = "right")
        
        county_deaths_map
    })
    output$histCDPC <- renderPlot({
        county_deaths %>%
            ggplot(mapping = aes(x = deathsPC)) +
            geom_histogram(binwidth = 0.0005) + 
            labs(title = "Histogram of Number of Deaths per Capita by County", x = "Number of Deaths/Capita",
                 y = "# Counties with X Deaths/Capita")
    })
    output$countyLDPC <- renderPlot({
        county_deaths_map <- plot_usmap("counties", data = county_deaths, values = "logDeathsPC", color = "black", size = 0.05)
        
        county_deaths_map <- county_deaths_map + 
            scale_fill_gradient(low = "white", high = "limegreen", name = "log(Deaths per Capita)") +
            labs(title = "Log Deaths per Capita by County on April 15, 2020") + 
            theme(legend.position = "right")
        
        county_deaths_map
    })
    output$histLDPC <- renderPlot({
        county_deaths %>%
            ggplot(mapping = aes(x = logDeathsPC)) +
            geom_histogram(binwidth = 0.38) + 
            labs(title = "Histogram of Number of log of Deaths per Capita by County", x = "log(Deaths/Capita)",
                 y = "# Counties with log(Deaths/Capita)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
