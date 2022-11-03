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
source(here("src", "utility_functions.R"))

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Monitoring Portal - Marine QC"),

    fluidRow(
        # BUTTON - refresh downloaded discrete data
        column(2, 
               actionButton("refresh_data", "Download data")), 
        # TEXT - display date of discrete data file
        column(4, 
               textOutput("date_data"))
    )
    
    
)

# Define server logic
server <- function(input, output) {
    # Set initial text: date of last data download
    output$date_data <- renderText(
        paste("Data last updated:", 
              substr(file.info(here("data", 
                                    "discrete_data.csv"))$mtime, 
                     1, 10)))
    # Update text on button push: date of last data download
    observeEvent(input$refresh_data,
                 {update_discrete()
                     output$date_data <- renderText(
                         paste("Data last updated:", 
                               Sys.Date()))})
}

# Run the application 
shinyApp(ui = ui, server = server)
