install.packages(c("shiny", "dplyr", "plotly"))
library(shiny)
library(dplyr)
library(plotly)
library(shiny)
library(tidyverse)
install.packages("shinyjs")
library(shinyjs)

# Add this line at the beginning of your UI
shinyjs::useShinyjs()


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
# Define server logic
daily_vars <- c("ConfirmedCovidCases", "DailyHospitalisedCases", "DailyICUCases",
                "DailyHealthcareWorkersCases", "DailyMaleCases", "DailyFemaleCases", "SevenDayAvg_Cases")
cumulative_vars <- c("TotalConfirmedCovidCases", "TotalCovidDeaths", "HospitalisedCovidCases",
                     "RequiringICUCovidCases", "HealthcareWorkersCovidCases", "Male", "Female")
default_var_daily_x <- daily_vars[1]
default_var_daily_y <- daily_vars[2]
default_var_x <- cumulative_vars[1]
default_var_y <- cumulative_vars[1]
shinyServer(function(input, output, session) {
  
  # Reactive function for filtered data based on date range
  filtered_data <- reactive({
    start_date <- input$dateRange[1]
    end_date <- input$dateRange[2]
    
    subset_data <- covid_data_selected %>% filter(Date >= start_date, Date <= end_date)
    print(length(input$extraVars))
    if (length(input$extraVars) > 0) {
      subset_data <- subset_data %>%
        select(Date, input$singleVar ,
                input$extraVars )
    } else {
      subset_data <- subset_data %>%
        select(Date,input$singleVar)
    }

    print(subset_data)
    return(subset_data)
  })
  
  # Render the time series plot
  output$timeSeriesPlot <- renderPlot({
    plot_data <- filtered_data()


    # Check if any two selected variables are the same

    if (!is.null(input$extraVars) && length(input$extraVars) > 1 && any(duplicated(input$extraVars))) {
      warning("Please select distinct variables for plotting.")
      shinyjs::alert("Please select distinct variables for plotting.")
      return(NULL)
    }
   
    
    
  
    
    
    # Plot the time series
    plot_data_long <- tidyr::gather(plot_data, key = "Variable", value = "Value", -Date)
    variable_colors <- rainbow(length(unique(plot_data_long$Variable)))
 
    ggplot(plot_data_long, aes(x = Date, y = Value, color = Variable)) +
      geom_line(size = 1) +
      theme_minimal() +
      labs(title = paste("Number of Variables:", ncol(plot_data) - 1,
                         "\nVariables:", paste(colnames(plot_data)[-1], collapse = ", ")),
           x = "Date", y = "Count") +
      scale_color_manual(values = setNames(variable_colors, unique(plot_data_long$Variable)))
  })
  
  # Reactive function for warning about duplicate variables
  output$warningText <- renderText({
    if (length(input$extraVars) > 1 && any(duplicated(input$extraVars))) {
      return("Warning: Please select distinct variables for plotting.")
      shinyjs::alert("Please select distinct variables for plotting.")
    } else {
      return(NULL)
    }
  })
  
  observe({
    if (!is.null(input$extraVars) && input$singleVar %in% input$extraVars) {
      shinyjs::alert("Warning: Please choose different variables for single and additional variables.")
    }
  })
  # Reactive function for title display
  output$plotTitle <- renderText({
    vars_selected <- c(input$singleVar, input$extraVars)
    paste("Time Series Plot -", length(vars_selected), "Variables:", paste(vars_selected, collapse = ", "))
  })
  
  observe({
    if (length(input$extraVars) == 2) {
      shinyjs::disable("extraVars")
    } else {
      shinyjs::enable("extraVars")
    }
  })
  
  observeEvent(input$resetButton, {
    shinyjs::enable("extraVars")
    updateCheckboxGroupInput(session, "extraVars", selected = character(0))
  })

  observe({
   
    
    if (is.null(input$var_x)) {
      updateSelectInput(session, "var_x", selected =  default_var_daily_x, choices = daily_vars)
    }
    
    if (is.null(input$var_y)) {
      updateSelectInput(session, "var_y", selected =  default_var_daily_y, choices = cumulative_vars)
    }
  })
  
  observe({
    if (input$var_type_x == "Daily") {
      updateSelectInput(session, "var_x", choices = daily_vars,selected =  default_var_daily_x, )
    } else {
      updateSelectInput(session, "var_x", choices = cumulative_vars, selected =  default_var_x,)
    }
  })
  
  observe({
    if (input$var_type_y == "Daily") {
      updateSelectInput(session, "var_y", choices = daily_vars,selected =default_var_daily_y )
    } else {
      updateSelectInput(session, "var_y", choices = cumulative_vars, default_var_y)
    }
  })
  
  observe({
    # Warning for choosing different variable types
    if (!is.null(input$var_x) && !is.null(input$var_y)) {
      if ((input$var_type_x == "Daily" && input$var_type_y == "Cumulative") ||
          (input$var_type_x == "Cumulative" && input$var_type_y == "Daily")) {
        shinyjs::alert("Warning: Please choose the same variable type for X and Y axes.")
        return(NULL)
      }
    }
    
    # Warning for choosing the same variable for both axes
    if (input$var_x == input$var_y && input$var_x!="" && input$var_y!="") {
      print(input$var_x)
      print(input$var_y)
      shinyjs::alert("Warning: Please choose different variables for X and Y axes.")
      return(NULL)
    }
  })
 

  
  output$selected_vars <- renderText({
    paste("Selected X Variable:", input$var_x, "\nSelected Y Variable:", input$var_y)
  })
  
  output$scatterplot <- renderPlot({
    ggplot(covid_data_selected, aes_string(x = input$var_x, y = input$var_y, size =input$var_y , fill = input$var_y,color =input$var_y )) +
      geom_point() +
      labs(title = paste("Scatterplot of", input$var_x, "vs", input$var_y),
           x = input$var_x, y = input$var_y)
  })
  output$plotTitleScatter <- renderText({
    vars_selected <- c(input$var_y, input$var_x)
    paste("Scatterplot of - Variables", paste(vars_selected, collapse = ", "))
  })
})