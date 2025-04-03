#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DataExplorer)   # 
library(shinythemes)    # Color themes for aesthetics
library(shinyjs)        # Allows for JavaScript functions in R
library(shinydashboard) # Gives tools for more comprehensive dashboards
library(bslib)


# Load dataset from file
dataset <- read.csv("C:\\Users\\domin\\Downloads\\Practicum_Cleaned_Data.csv")

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Data Visualization App"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Frequency Bar Chart", tabName = "bar_chart_tab", icon = icon("chart-bar")),
      menuItem("Boxplot Comparisons", tabName = "boxplot_tab", icon = icon("box"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Bar Chart Tab
      tabItem(tabName = "bar_chart_tab",
              fluidRow(
                box(title = "Select Variable for Frequency", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("primary_var", "Select a Variable:", choices = colnames(dataset)[-1])
                ),
                box(title = "Interactive Frequency Bar Chart", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("bar_chart"))
              )
      ),
      
      # Boxplot Tab
      tabItem(tabName = "boxplot_tab",
              fluidRow(
                box(title = "Select Variables for Boxplot", width = 12, status = "primary", solidHeader = TRUE,
                    selectInput("categorical_var", "Select a Categorical Variable:", 
                                choices = colnames(dataset)),
                    selectInput("continuous_var", "Select a Continuous Variable:", 
                                choices = colnames(dataset))
                ),
                box(title = "Boxplot: Continuous vs. Categorical", width = 12, status = "primary", solidHeader = TRUE,
                    plotlyOutput("boxplot"),
                    uiOutput("warning_message")
                    )
              )
      )
    )
  )
)

# Server
server <- function(input, output) {
  output$bar_chart <- renderPlotly({
    req(input$primary_var)
    
    # Build frequency plot
    p <- ggplot(dataset, aes_string(x = input$primary_var)) +
      geom_bar(fill = "steelblue") +
      labs(title = paste("Frequency of", input$primary_var), x = input$primary_var, y = "Count") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  
  # Boxplot for Categorical vs. Continuous
  output$boxplot <- renderPlotly({
    req(input$categorical_var, input$continuous_var)
    
    # Remove rows with NA values in the selected variables for a clean plot
    clean_data <- dataset %>%
      filter(!is.na(.data[[input$categorical_var]]), !is.na(.data[[input$continuous_var]]))
    
    # Check if there's enough data after filtering
    if (nrow(clean_data) > 0) {
      p <- ggplot(clean_data, aes_string(x = input$categorical_var, y = input$continuous_var)) +
        geom_boxplot(fill = "lightblue", color = "black") +
        labs(title = paste("Boxplot of", input$continuous_var, "by", input$categorical_var),
             x = input$categorical_var, y = input$continuous_var) +
        theme_minimal()
      
      return(ggplotly(p))  # Return the plot as interactive plotly graph
    } else {
      # Return a message if there is not enough data
      return(NULL)
    }
  })
  
  # Display a warning message if the selected variables don't make sense for plotting
  output$warning_message <- renderUI({
    req(input$categorical_var, input$continuous_var)
    
    # Check for missing data in the selected variables
    if (sum(is.na(dataset[[input$categorical_var]])) > 0 || sum(is.na(dataset[[input$continuous_var]])) > 0) {
      return(
        div(style = "color: red;", 
            "Warning: Some data is missing from the selected variables. Missing data has been removed from the plot.")
      )
    }
  })
}


# Run the app
shinyApp(ui, server)



