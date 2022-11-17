#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(tidyverse)
library(kcmarine)
library(ggvis)
source(here("src", "utility_functions.R"))

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Monitoring Portal - Marine QC"),

    # Row of buttons
    fluidRow(
        # BUTTON - refresh downloaded discrete data
        column(2, 
               actionButton("refresh_data", "Download data")), 
        # TEXT - display date of discrete data file
        column(4, 
               textOutput("date_data"))
    ),
    
    # Tab selections: single site, Central Basin, Whidbey Basin
)

# Define server logic
server <- function(input, output) {
    # Define discrete data path
    data_fpath <- here("data", "discrete_data.csv")
    
    # Define initial data
    initial_data <- load_discrete()
    discrete_data <- reactiveValues(data = initial_data)
    
    # Define initial data date
    old_date <- ifelse(file.exists(data_fpath), 
                       substr(file.info(data_fpath)$mtime, 1, 10), 
                       "")
    file_date <- reactiveVal(old_date)
    
    # Update data and date text on button push - refresh_data
    observeEvent(input$refresh_data, {
        update_discrete()
        temp <- data.frame(load_discrete())
        discrete_data$data <- temp
        
        new_date <- Sys.Date()
        file_date(new_date)
    })
    
    # Render data date text
    output$date_data <- renderText(
        paste("Data last updated:", 
              file_date())
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
