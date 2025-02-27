install.packages(c("shiny", "dplyr", "plotly"))
library(shiny)
library(dplyr)
library(plotly)
library(shiny)
if (!requireNamespace("shinyjs", quietly = TRUE)) {
  install.packages("shinyjs")
}

# Load shinyjs
library(shinyjs)
library(tidyverse)
url <- "https://opendata-geohive.hub.arcgis.com/datasets/d8eb52d56273413b84b0187a4e9117be_0.csv?outSR=%7B%22latestWkid%22%3A3857%2C%22wkid%22%3A102100%7D"
df <- read_csv(url)
selected_vars <- c("Date", "ConfirmedCovidCases", "TotalConfirmedCovidCases",
                   "ConfirmedCovidDeaths", "TotalCovidDeaths", "StatisticsProfileDate",
                   "HospitalisedCovidCases", "RequiringICUCovidCases",
                   "HealthcareWorkersCovidCases", "Male", "Female", "SevenDayAvg_Cases")

covid_data <- df[selected_vars]
covid_data_selected <- covid_data[-c(1, 2), ]
covid_data_selected$Date <- as.Date(covid_data_selected$Date)
covid_data_selected$StatisticsProfileDate <- as.Date(covid_data_selected$StatisticsProfileDate)
covid_data_selected$StatisticsProfileDate <- covid_data_selected$Date
covid_data_selected$ConfirmedCovidDeaths[is.na(covid_data_selected$ConfirmedCovidDeaths)] <- 0
covid_data_selected[is.na(covid_data_selected)] <- 0
covid_data_selected$DailyHospitalisedCases <- c(0, diff(covid_data_selected$HospitalisedCovidCases))
covid_data_selected$DailyHealthcareWorkersCases <- c(0, diff(covid_data_selected$HealthcareWorkersCovidCases))
covid_data_selected$DailyICUCases <- c(0, diff(covid_data_selected$RequiringICUCovidCases))
covid_data_selected$DailyMaleCases <- c(0, diff(covid_data_selected$Male))
covid_data_selected$DailyFemaleCases <- c(0, diff(covid_data_selected$Female))




shinyUI(fluidPage(
  useShinyjs(),
  
  titlePanel("COVID-19 Time Series Plot"),

  # Define the first tab
  tabsetPanel(
    tabPanel("Time Series Plot",
             sidebarLayout(
    
               sidebarPanel(

                 style = "background: url('https://upload.wikimedia.org/wikipedia/commons/thumb/8/82/SARS-CoV-2_without_background.png/600px-SARS-CoV-2_without_background.png') no-repeat center center fixed; background-size: cover;",
                 # User input for date range
                 dateRangeInput("dateRange", "Select Date Range:",
                                start = min(covid_data_selected$Date),
                                end = max(covid_data_selected$Date),
                                min = min(covid_data_selected$Date),
                                max = max(covid_data_selected$Date),
                                separator = " - "),
                 
                 selectInput("singleVar", "Select a Single Variable:",
                             choices = names(covid_data_selected)[2:ncol(covid_data_selected)]),
                 
                 # Checkbox to add extra variables
                 checkboxGroupInput("extraVars", "Add Extra Variables:",
                                    choices = names(covid_data_selected)[2:ncol(covid_data_selected)],
                                    selected = character(0)),
                 
                
                 
                 # Warning for duplicate variables
                 textOutput("warningText"),
                 actionButton("resetButton", "Reset and Enable Checkboxes")
               ),
               
               mainPanel(
                 # Time series plot
                 plotOutput("timeSeriesPlot",width="700px",height = "600px"),
                 
                 # Display variable information
                 h4(textOutput("plotTitle"))
               )
             )
    ),
  
 
    # Define the second tab (you can customize this tab as needed)
    tabPanel("X-Y Scatterplot",
             sidebarLayout(
               
               sidebarPanel(
                 style = "background: url('https://images.unsplash.com/photo-1605289982774-9a6fef564df8?crop=entropy&cs=tinysrgb&fit=max&fm=jpg&ixid=MnwxfDB8MXxyYW5kb218MHx8Y292aWQtMTl8fHx8fHwxNzAyNTgzNTM5&ixlib=rb-4.0.3&q=80&utm_campaign=api-credit&utm_medium=referral&utm_source=unsplash_source&w=1080') no-repeat center center fixed; background-size: cover;",
                 selectInput("var_type_x", "Select X Variable Type", choices = c("Daily", "Cumulative")),
                 selectInput("var_x", "Select X Variable", choices = NULL),
                 selectInput("var_type_y", "Select Y Variable Type", choices = c("Daily", "Cumulative")),
                 selectInput("var_y", "Select Y Variable", choices = NULL)
               ),
               mainPanel(
                
                 plotOutput("scatterplot", width="700px",height = "700px"),
                 h4(textOutput("plotTitleScatter"))
               )
             )
             
    )
  )
)
  )

