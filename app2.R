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


# Example dataset (assumed to already exist as 'dataset')
data_vars <- names(dataset)

ui <- dashboardPage(
  dashboardHeader(title = "Data Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Exploration", tabName = "exploration_tab", icon = icon("search")),
      menuItem("Analysis", tabName = "analysis_tab", icon = icon("chart-line"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "exploration_tab",
              
              # Top row with dropdown menu
              fluidRow(
                column(width = 4,
                       selectInput("primary_var", "Variable #1:", choices = c("", data_vars), selected = "")
                ),
                column(width = 4,
                       selectInput("secondary_var", "Variable #2:", choices = c("", data_vars), selected = "")
                ),
                column(width = 2,
                       checkboxInput("stratify", "Stratify?", value = FALSE),
                       checkboxInput("show_percent", "Show Percentages?", value = FALSE)
                ),
                column(width = 2,
                       conditionalPanel(
                         condition = "input.stratify == true",
                         selectInput("stratify_var", "Stratify By:", choices = data_vars)
                       )
                )
              ),
              
              # Output or other content below
              fluidRow(
                box(title = textOutput("output_title"), width = 12, solidHeader = TRUE, status = "info",
                    verbatimTextOutput("selected_info"),
                    conditionalPanel(
                      condition = "input.primary_var != ''",
                      plotlyOutput("bar_chart")
                    )
                )
              )
      ),
      
      # Analysis Tab (new)
      tabItem(tabName = "analysis_tab",
              
              # Analysis section content
              fluidRow(
                box(title = "Analysis Section", width = 12, solidHeader = TRUE, status = "info",
                    p("This is where you would implement your analysis features."),
                    p("You could add options for more complex visualizations, models, etc.")
                )
              )
      )        
    )
  )
)

#########################################################################################
#SERVER
#########################################################################################

server <- function(input, output, session) {

##############################################
# EXPLORATION
##############################################

  
  # Dynamically update the output box title
  output$output_title <- renderText({
    if (input$primary_var != "" && input$secondary_var == "" && !input$stratify) {
      return("Frequencies")
    } else if (input$primary_var != "" && input$secondary_var != "" && !input$stratify) {
      return("Comparison")
    } else if (input$primary_var != "" && input$secondary_var != "" && input$stratify) {
      return("Stratified Comparison")
    } else if (input$primary_var != "" && input$secondary_var == "" && input$stratify) {
      return("Stratified Frequency")
    } else {
      return("Variable Selection")
    }
  })
  
  # Summary Information
  output$selected_info <- renderPrint({
    cat("Primary variable:", input$primary_var, "\n")
    cat("Secondary variable:", ifelse(input$secondary_var == "", "None selected", input$secondary_var), "\n")
    cat("Stratify:", ifelse(input$stratify, "Yes", "No")) 
    cat("    Pecentages?", ifelse(input$show_percent, "Yes", "No"))
  })
  
  
  #######################################################################################
  #FREQUENCIES AND BARCHARTS
  #######################################################################################
  
  # This section focuses on the multiple levels of changes that can occur for the 
  # frequency section. This will occur only when there is only one variable selected.
  # Additionally, there will be functionality to stratify the view and switch
  # the reference to percentages rather than counts
  
  # Bar chart output for primary variable + Addition of Stratification Var.
  output$bar_chart <- renderPlotly({
    req(input$primary_var)
    
    # Set a new dataframe for this, to make it easier to code
    data <- dataset
    
    # If Statement for if the user stratified the frequency.
    if (input$stratify && input$stratify_var != "") {
      # And if the calculate percentages was added
      if (input$show_percent) {
        
        df <- data %>%
          group_by(across(all_of(c(input$primary_var, input$stratify_var)))) %>%
          summarise(n = n(), .groups = "drop") %>%
          group_by(across(all_of(input$primary_var))) %>%
          mutate(percent = n / sum(n) * 100)
        
        p <- ggplot(df, aes_string(x = input$primary_var, y = "percent", fill = input$stratify_var)) +
          geom_bar(stat = "identity", position = "dodge") +
          theme_minimal() +
          labs(x = input$primary_var, y = "Percentage", fill = input$stratify_var)
        
      } else {
        # Raw counts
        p <- ggplot(data, aes_string(x = input$primary_var, fill = input$stratify_var)) +
          geom_bar(position = "dodge") +
          theme_minimal() +
          labs(x = input$primary_var, y = "Count", fill = input$stratify_var)
      }
      
    } else {
      # Not stratified
      if (input$show_percent) {
        # Percentages without stratification
        df <- data %>%
          group_by(across(all_of(input$primary_var))) %>%
          summarise(n = n(), .groups = "drop") %>%
          mutate(percent = n / sum(n) * 100)
        
        p <- ggplot(df, aes_string(x = input$primary_var, y = "percent")) +
          geom_bar(stat = "identity", fill = "#0073C2FF") +
          theme_minimal() +
          labs(x = input$primary_var, y = "Percentage")
        
      } else {
        # Raw counts
        p <- ggplot(data, aes_string(x = input$primary_var)) +
          geom_bar(fill = "#0073C2FF") +
          theme_minimal() +
          labs(x = input$primary_var, y = "Count")
      }
    }
     
    ggplotly(p)
  })
  
  #######################################################################################
  #BOX PLOTS 
  #######################################################################################
}

shinyApp(ui, server)




