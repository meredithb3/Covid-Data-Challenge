library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(usmap)
library(ggthemes)
library(shinydashboard)
library(skimr)
library(broom)
library(knitr) 
library(rms)
library(stats)
library(kableExtra)
library(DT)

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
demographicsData['perc_asian'] <- demographicsData$num_asian/demoDfPopulation
demographicsData['perc_other'] <- demographicsData$num_other/demoDfPopulation

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

county_deaths <- county_deaths %>%
    filter(logDeathsPC != -Inf)

county_deaths$POVALL_2018 <- as.numeric(gsub(",", "", county_deaths$POVALL_2018))
county_deaths$POV017_2018 <- as.numeric(gsub(",", "", county_deaths$POV017_2018))
county_deaths$POV517_2018 <- as.numeric(gsub(",", "", county_deaths$POV517_2018))
county_deaths$MEDHHINC_2018 <- as.numeric(gsub(",", "", county_deaths$MEDHHINC_2018))

county_deaths <- county_deaths %>%
    filter(!is.na(Rural.urban_Continuum_Code_2003))
county_deaths <- county_deaths %>%
    filter(!is.na(Urban_Influence_Code_2003))

county_deaths$Rural.urban_Continuum_Code_2013 <- as.factor(county_deaths$Rural.urban_Continuum_Code_2013)

county_deaths <- county_deaths %>%
    mutate(logCases = log(cases),
           log_pop_2015 = log(pop_2015))

county_deaths$predicted = predict.lm(final)
county_deaths$resid = residuals(final)

final_output <- augment(final) %>%
    mutate(obs_num = row_number())

augmented_model <- augment(final)

leverage_threshold <- 2*(5+1)/(nrow(augmented_model))

high_lev <- augmented_model %>%
    filter(.hat > leverage_threshold)

augmented_model <- augmented_model %>%
    mutate(obs_num = 1:nrow(augmented_model))

full_interaction <- lm(logDeathsPC ~ logCases + 
                           Rural.urban_Continuum_Code_2013 + PCTPOVALL_2018 +
                           log_pop_2015 + perc_white + perc_black + 
                           logCases*PCTPOVALL_2018 + logCases*perc_white +
                           logCases*perc_black +
                           PCTPOVALL_2018*perc_white + PCTPOVALL_2018*perc_black +
                           PCTPOVALL_2018*log_pop_2015 + log_pop_2015*perc_white + 
                           log_pop_2015*perc_black,
                       data = county_deaths)
interaction_model <- step(full_interaction, direction = "backward", trace = FALSE)



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = "purple",
                
    dashboardHeader(title = "COVID-19 Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction and Purpose", tabName = "intro", icon = icon("home")),
            menuItem("The Data", tabName = "data", icon = icon("table")),
            menuItem("Exploring the Data", tabName = "eda", icon = icon("search")),
            menuItem("Creating Our Model", tabName = "model", icon = icon("user-md")),
            menuItem("Assumptions", tabName = "assumptions", icon = icon("question")),
            menuItem("Model Analysis", tabName = "analysis", icon = icon("chart-line")),
            menuItem("Model Interpretation", tabName = "interp", icon = icon("lightbulb")),
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
                        box(title = "Our Dataets & Sources", status = "primary", solidHeader = TRUE, width = 1000,
                            box(title = "COVID-19 Data", status = "danger",
                                "Our COVID data by county comes from the New York Times COVID dataset available on github.", br(), br(),
                                dataTableOutput("covid"),
                                "The COVID dataset contained each county's data for fips code, number of cases, number of deaths, and state all by date since 
                                January 21st. We combined this in order to create a dataset that had the total number of deaths and cases (not shown here) on 
                                April 15th, the last day in which we downloaded the data before finalizing our analysis."),
                            box(title = "Poverty Data", status = "success", 
                                "Our poverty statistics by county data comes from the website census.gov under the poverty information section.", br(), br(),
                                dataTableOutput("poverty"),
                                "The poverty dataset contained a lot of variables but we determined the most useful for us would be county, fips code, 
                                percent of its citizens in poverty, percent of its citizens with health insurance, and its 'rural urban continuum code' which 
                                essentially describes how rural or urban (densely population) a county is."
                                ),
                            box(title = "Demographics Data", status = "warning", 
                                "The demograhpics by county data comes from the webiste census.gov under the race data sections.", br(), br(),
                                dataTableOutput("demog")
                                )
                            
                            )
                    )
            ),
            
            #eda tab contents
            tabItem(tabName = "eda",
                    fluidRow(
                        box(title = "Deaths per Capita Map", status = "primary", solidHeader = TRUE, height = 600,
                            plotOutput("countyDPC"), 
                            "Looking at our plot of deaths per capita by county, we see that there are some 
                            counties (namely in New York, Louisiana, and Michigan) that have relatively much
                            larger deaths per capita than most others."),
                        box(title = "Deaths per Capita Histogram", status = "primary", solidHeader = TRUE, height = 600,
                            plotOutput("histCDPC"),
                            "The histogram of deaths per capita by county confirms this as it has extreme right skew.
                            We should analyze the log of this variable to see if it has more even spread.", br()),
                        box(title = "Log Deaths per Capita Map", status = "warning", solidHeader = TRUE, height = 600,
                            plotOutput("countyLDPC"),
                            "Looking at this new plot of the log of deaths per capita by county, we see much more even 
                            spread of values. We should note that counties in black have zero deaths and thus we will 
                            only analyze counties with deaths as we cannot draw conclusions for counties that have not
                            experienced COVID-19 outbreaks."),
                        box(title = "Log Deaths per Capita Histogram", status = "warning", solidHeader = TRUE, height = 600,
                            plotOutput("histLDPC"),
                            "The histogram of the log of deaths per capita by county appears relatively nornal and symmetric.
                            This is in agreement with the more even spread of counties affected in the map to the right and 
                            will likely be good for modeling through our analysis portion.")
                    )
            ),
            
            #model tab contents
            tabItem(tabName = "model",
                    fluidRow(
                            column(width = 5, 
                                   box(title = "Variables for the Model", status = "primary", solidHeader = TRUE,
                            "The first thing we do to create our model is establish the variable that we 
                            would like to be our response as well as its corresponding predictors. We previously
                            decided that the best response variable to use would be the log of deaths per capita 
                            in a county as it had the most symmetric and normal distribution and would be a good 
                            indicator of how well a county is dealing with the impact of COVID-19. The predictor models 
                            we have decided to test in our full model are the number of cases, the rural-urban continuum code
                            (it measures the level of density for a county), the percent of the county that is in poverty, the percent
                            that has medical insurance, its population, and the percent of a county that is white, black, asian, or 'other.'"
                            ),
                            
                            box(title = "Selection Methods", status = "primary", solidHeader = TRUE,
                            "In order to select a model that best fits the data without including too many 
                            extraneous variables, we used the backwards selection method and used BIC as a criteria. 
                            This will maximize the efficiency of our model. The final model after selection is shown to
                            the right. It should also be noted that we used the log of population and the log of cases. We
                            did this because upon the univariate analysis of these variables we saw severe skewness; we corrected
                            this skewness to account for our linear model by taking the log of these variables which resulted in their
                            normal distributions."
                            )
                        ),
                            
                            box(title = "Model Output", status = "danger", solidHeader = TRUE,
                                htmlOutput("finalModel"))
                    )

            ),
            
            
            #assumptions tab contents
            tabItem(tabName = "assumptions",
                    fluidRow(
                        box(title = "Assumption 1: Linearity", status = "success", solidHeader = TRUE,
                            "We can check the linearity assumption by graphing the response variable against all the predictor variables within our model. ", br(), br(),
                            selectInput("predictor","Predictor Variable:",
                                        choices = list("log(Cases)", "Rural-Urban Continuum Code", 
                                                    "Percent in Poverty", "log(Population)", "Percent White",
                                                    "Percent Black")),
                            plotOutput("pVr"),
                            textOutput("pVrTxt")
                        ),
                        
                        box(title = "Assumption 2: Constant Variance", status = "success", solidHeader = TRUE,
                            "Next, we can check the constant variance assumption by looking at the plot of the residuals of each predictor variable and the response variable. For this assumption to be satisfied, the regression variance must be the around the same for each predictor variable (randomly scattered points around y=0 line in residual plot).", br(), br(),
                            selectInput("cvIn","Variable to be Plotted vs. Residuals:",
                                        choices = list("Predicted","log(Cases)", "Rural-Urban Continuum Code", 
                                                       "Percent in Poverty", "log(Population)", "Percent White",
                                                       "Percent Black")),
                            plotOutput("constVar"),
                            "We see that there does not seem to be any correlation between the residuals and the log(Deaths/Capita) or in any of the graphs plotting our residuals against our predictor variables. Therefore, we assume that our model fits the constant variance assumption, and we will proceed to check normality of our model."
                        )),
                    
                    fluidRow(
                        box(title = "Assumption 3: Normality", status = "success", solidHeader = TRUE,
                            "Next, we can check the normality assumption by plotting the histogram of the residuals and the normal QQ plot of the residuals to check for any discrepancies and departures from normality.", br(), br(),
                            selectInput("normality","Plot Preference:",
                                        choices = list("Histogram of Residuals", "Normal QQ Plot")),
                            plotOutput("normalityPlot"),
                            textOutput("normalityTxt")
                            ),
                        box(title = "Assumption 4: Independence", status = "success", solidHeader = TRUE,
                            "We do have a few concerns about the independence of our observations, mainly we believe that there may be some correlation between neighboring counties. Since the virus must be transmitted from person to person under our current knowledge, we believe that neighboring counties, which are divided not by a physical border, but just by a cartographic line, may have a highly correlated number of deaths per capita, as well as similar characteristics or features. However, we will proceed with caution in our analysis. ")
                        
                    )
            ),
            
            tabItem(tabName = "analysis",
                    fluidRow(
                        box(title = "Model Diagnostics", status = "warning", solidHeader = TRUE, width = 1000,
                            "In this part we will analyze diagnostics such as multicollinearity and possible influential points in our data.", br(), br(),
                            
                            box(title = "Variance Inflation Factors", width = 1000, status = "success",
                                verbatimTextOutput("multicol"),
                            "We do not see any issue with multicollinearity in our model given that all of the 
                            variance inflation factors are below the general threshold of 10."),
                            
                            box(title = "Influential Points", width = 1000, status = "success",
                                plotOutput("highLevPlot"),
                                "We see many points above the leverage threshold of 2 times the average leverage
                                so we shall analyze the cooks distance of the points to make sure none are truly
                                influential.",
                                tableOutput("cooksD"),
                                "After filtering the high leverage values for those with a Cooks Distance of greater
                                than 1, we do not see any values left in our data table and thus do not have any truly influential points in 
                                our data."
                                )
                            
                            ),
                        
                        box(title = "Interaction Effects", status = "danger", solidHeader = TRUE, width = 1000,
                            htmlOutput("intModel"),
                            "After including all possible interaction effects in a model and then using the same method
                            of backward selection with BIC as a criterion, we obtain the above model and see that there 
                            appear to be significant interactions with the following: log(cases) with percent in poverty, percent black,
                            and percent white. percent in poverty with percent black and log(population). log(population) with percent black.
                            We will go into more detail with exactly what these interactions mean in our model in the 'Interpretation' tab.", br(), br(),
                            )
                        
            
                        )
            ),
            
            
            #interp tab contents
            tabItem(tabName = "interp",
                    fluidRow(
                        box(width = 1000, title = "Coefficient & General Interpretation", status = "info", solidHeader = TRUE,
                            "We again see that the number of cases has a high correlation with the number of deaths per capita in a county as we would expect. Some of the Rural Urban Continuum codes still contain strong evidence for a correlation with the log deaths per capita, as noted by the p-value. We now see that the log population of a county now has strong evidence for a correlation with the log deaths per capita. Additionally, all of the interactions seem to be have fairly strong evidence of being correlated with the log number of deaths per capita. Additionally, we see that the interaction between the percentage of individuals in poverty and the percentage of black individuals in a county is retained in our final model, which was an interaction that we had discussed wanting to explore further. we see that as a county increases it's percentage of black residents and it's percentage of residents in poverty, the deaths per capita in the county increase on average.", br(), br(),
                            
                            box(width = 1000, title = "Regular Model Coefficients", status = "danger",
                                'We expect that as the log number of cases increases by 1, we expect the number of deaths per capita in the county to increase by 3.7028 on average. Additionally, we expect that as the log population increases by 1, we expect the the number of deaths per capita to increase by 0.29226 on average. Next, we expect that as the percentage of white inhabitants in a county increases by 1 percent, the deaths per capita in the county will increase by .0509 on average. Meanwhile, as the percentage of black inhabitants in a county increases by 1 percent, the deaths per capita in the county will increase by .7247 on average. If the percentage of individuals in poverty in a county increases by 1 percent in a county, we expect the deaths per capita in the county to increase by .0089 on average. In the following paragraph, we will provide interpretation for the coefficients corresponding to the rural urban continuum codes generated by the model.', br(), br(),
                                'A key of all of the rural urban continuum codes can be found at https://seer.cancer.gov/seerstat/variables/countyattribs/ruralurban.html. In general, as the code number increases, a county is smaller and less urban.', br(), br(),
                                'The baseline rural urban continuum code for our analysis is a code 1. If a county has a rural urban continuum code of 2, we expect there to be 1.0143721  deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 3, we expect there to be 0.8300360 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 4, we expect there to be 0.8628820 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 5, we expect there to be 0.9337068 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 6, we expect there to be 1.0256837 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 7, we expect there to be 1.1207587	 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 8, we expect there to be 1.3932181 deaths per capita more than counties with a baseline code on average. If a county has a rural urban continuum code of 9, we expect there to be 1.6894608 deaths per capita more than counties with a baseline code on average. This trend seems to indicate that as a county becomes less urban, the average deaths per capita seems to increase.'
                            ),
                            box(width = 1000, title = "Interaction Coefficients", status = "danger",)
                        ),
                        
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
    output$finalModel <- renderText({
        final <- lm(logDeathsPC ~ logCases + Rural.urban_Continuum_Code_2013 + 
                        PCTPOVALL_2018 + log_pop_2015 + perc_white + perc_black, data = county_deaths)
        
        
        tidy(final, format = "markdown", exponentiate = TRUE) %>%
            kable("html",digits = 7) %>%
            kable_styling()
    })
    output$pVr <- renderPlot({
        if(input$predictor == "log(Cases)") {
            ggplot(data = county_deaths, aes(x = logCases, y = logDeathsPC)) +
                geom_point(color = "black") +
                labs(title = "Log Deaths/Capita vs Log Cases", y = "Log Deaths/Capita", x = "Log Cases")
        }
        else
        if(input$predictor == "Rural-Urban Continuum Code") {
            ggplot(data = county_deaths, aes(x = Rural.urban_Continuum_Code_2013, y = logDeathsPC)) +
                geom_point(color = "black") +
                labs(title = "Log Deaths/Capita vs Continuum Code", y = "Log Deaths/Capita", x = "Continuum Code")
        }
        else
        if(input$predictor == "Percent in Poverty") {
            ggplot(data = county_deaths, aes(x = PCTPOVALL_2018, y = logDeathsPC)) +
                geom_point(color = "black") +
                labs(title = "Log Deaths/Capita vs Percent in Poverty", y = "Log Deaths/Capita", x = "Percent in Poverty")
        }
        else
        if(input$predictor == "log(Population)"){
        ggplot(data = county_deaths, aes(x = log_pop_2015, y = logDeathsPC)) +
            geom_point(color = "black") +
            labs(title = "Log Deaths/Capita vs Log Population (2015)", y = "Log Deaths/Capita", 
                 x = "Log Population (2015)")
        }
        else
        if(input$predictor == "Percent White"){
            ggplot(data = county_deaths, aes(x = perc_white, y = logDeathsPC)) +
                geom_point(color = "black") +
                labs(title = "Log Deaths/Capita vs Percent White", y = "Log Deaths/Capita", 
                     x = "Percent White")
        }
        else
        if(input$predictor == "Percent Black"){
            ggplot(data = county_deaths, aes(x = perc_black, y = logDeathsPC)) +
                geom_point(color = "black") +
                labs(title = "Log Deaths/Capita vs Percent Black", y = "Log Deaths/Capita", 
                     x = "Percent Black")
        }
    })
    output$pVrTxt <- renderText({
        if(input$predictor == "log(Cases)") {
            "From the graph above, we can see that there is a slightly positive association between log deaths per capita and the log number of cases. We would expect this, as more cases would likely contribute to more deaths per capita. There are no obvious concerns with linearity here, although the data is sort of spread out in a slight backwards fan. "
        }
        else
            if(input$predictor == "Rural-Urban Continuum Code") {
                "From the boxplots above, there appears to be a slight positive linear assoication in log deaths per capita amongst the different rural-urban continuum codes. Since this is a categorical variable, it becomes a bit harder for us to distinguish any glaring concerns with linearity."
            }
        else
            if(input$predictor == "Percent in Poverty") {
                "From the graph above, we can see that there is a very slightly positive association between log deaths per capita and the percent poverty in 2018. We would expect this, as a higher percent of people in poverty would likely contribute to more deaths per capita. There are no obvious concerns with linearity here, although the linear association is very weak."
            }
        else
            if(input$predictor == "log(Population)"){
               "From the graph above, we can see that there is a negative linear association between log deaths per capita and the log of population in 2015. This is interesting because it implies that an increase in log population correlates with a decrease in log deaths per capita. There are no obvious concerns with linearity here. "
            }
        else
            if(input$predictor == "Percent White"){
                "Both the above graphs of specific percents of the population appear to violate the linearity assumption. This makes sense, as we saw in their distributions in the Exploratory Data Analysis that their initial distributions were skewed and not fixed by a transformation. Thus, we can proceed with this analysis with caution."
            }
        else
            if(input$predictor == "Percent Black"){
                "Both the above graphs of specific percents of the population appear to violate the linearity assumption. This makes sense, as we saw in their distributions in the Exploratory Data Analysis that their initial distributions were skewed and not fixed by a transformation. Thus, we can proceed with this analysis with caution."
            }
    })
    
    output$constVar <- renderPlot({
    if(input$cvIn == "Predicted"){
        ggplot(data = county_deaths, mapping = aes(x = predicted, y = resid)) + 
            geom_point() + 
            geom_hline(yintercept = 0, color = "red") + 
            labs(title = "Residuals vs. Predicted Log Deaths/Capita",
                 x = "Log(Price)",
                 y = "Residual")
    }
    else
    if(input$cvIn == "log(Cases)"){
        ggplot(data = county_deaths, mapping = aes(x = logCases, y = resid)) + 
            geom_point() + 
            geom_hline(yintercept = 0, color = "red") + 
            labs(title = "Residuals vs. Log Cases",
                 x = "Log(Cases)",
                 y = "Residuals")
    }
    else
        if(input$cvIn == "Percent in Poverty"){
            ggplot(data = county_deaths, mapping = aes(x = PCTPOVALL_2018, y = resid)) + 
                geom_point() + 
                geom_hline(yintercept = 0, color = "red") + 
                labs(title = "Residuals vs. Percent in Poverty",
                     x = "Percent in Poverty",
                     y = "Residuals")
        }
    else
        if(input$cvIn == "log(Population)"){
            ggplot(data = county_deaths, mapping = aes(x = log_pop_2015, y = resid)) + 
                geom_point() + 
                geom_hline(yintercept = 0, color = "red") + 
                labs(title = "Residuals vs. Log(Population)",
                     x = "Log(Population)",
                     y = "Residuals")
        }
    else
        if(input$cvIn == "Percent White"){
            ggplot(data = county_deaths, mapping = aes(x = perc_white, y = resid)) + 
                geom_point() + 
                geom_hline(yintercept = 0, color = "red") + 
                labs(title = "Residuals vs. Percentage of White Residents",
                     x = "Percentage of White Residents",
                     y = "Residuals")
        }
    else
        if(input$cvIn == "Percent Black"){
            ggplot(data = county_deaths, mapping = aes(x = perc_black, y = resid)) + 
                geom_point() + 
                geom_hline(yintercept = 0, color = "red") + 
                labs(title = "Residuals vs. Percentage of Black Residents",
                     x = "Percentage of Black Residents",
                     y = "Residuals")
        }
    else
        if(input$cvIn == "Rural-Urban Continuum Code"){
            ggplot(data = county_deaths, mapping = aes(x = Rural.urban_Continuum_Code_2013, y = resid)) + 
                geom_boxplot() + 
                geom_hline(yintercept=0,color="red") +
                labs(x = "Female", 
                     y="Residuals")
        }
        
    })
    
    output$normalityPlot <- renderPlot({
        if(input$normality == "Histogram of Residuals"){
            ggplot(data = county_deaths, mapping = aes(x = resid)) +   
                geom_histogram(color = "Black", fill = "Light Blue", binwidth = 0.05) + 
                labs(title = "Distribution of Residuals",
                     x = "Residuals",
                     y = "Frequency")
        }
            
        else
            if(input$normality == "Normal QQ Plot"){
                ggplot(data = county_deaths, mapping = aes(sample = resid)) +  
                    stat_qq() + 
                    stat_qq_line() +
                    labs(title = "Normal QQ Plot of Residuals") 
            }
    })
    
    output$normalityTxt <- renderText({
        if(input$normality == "Histogram of Residuals"){
            "The distribution of residuals from our model appears to be mostly normal, however there is a slight left-skewness Therefore, we would like to investigate the normality of the model a bit further by looking at a QQ plot."
        }
        
        else
            if(input$normality == "Normal QQ Plot"){
                "From the two graphs above, we can see that the normality assumption is mostly satisfied. While there does appear to be some deviation from the normal QQ plot towards the edges of the graphs, we believe that this is enough to satisfy the normality for this design, so we will process with our testing."
            }
    })
    
    output$multicol <- renderPrint({
        vif(final)
    })
    
    output$highLevPlot <- renderPlot({
        ggplot(data = augmented_model, aes(x = obs_num,y = .hat)) + 
            geom_point(alpha = 0.7) + 
            geom_hline(yintercept = leverage_threshold,color = "red")+
            labs(x = "Observation Number",y = "Leverage",title = "Leverage") +
            geom_text(aes(label=ifelse(.hat > leverage_threshold, as.character(obs_num), "")), nudge_x = 4)
    })
    
    output$cooksD <- renderTable({
        high_lev %>%
            filter(.cooksd > 1)
    })
    
    output$intModel <- renderText({
        tidy(interaction_model, format = "markdown") %>%
        kable("html",digits = 4) %>%
            kable_styling()
    })
    
    output$covid <- renderDataTable({
        county_deaths %>%
            mutate(county = county.x) %>%
            select(county, state, fips, total_deaths, date) %>%
            datatable(., options = list(dom = "ftp", pageLength = 5))
    })
    
    output$poverty <- renderDataTable({
        data %>%
            mutate(RU_code = Rural.urban_Continuum_Code_2003) %>%
            mutate(county = county.x) %>%
            select(county, fips, PCTPOVALL_2018, MEDHHINC_2018, RU_code) %>%
            datatable(., width = "100%", options = list(dom = "ftp", pageLength = 5))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
