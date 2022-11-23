#### SETUP ####
library(shiny)
library(here)
library(tidyverse)
library(kcmarine)
library(ggvis)
source(here("src", "utility_functions.R"))

# Load in data - this happens before the app opens and may take a sec
# TO DO: cache data as an .rda instead
# Define discrete data path
data_fpath <- here("data", "discrete_data.csv")

# Define initial data and data date
if (!file.exists(data_fpath)) {
    update_discrete()
}
initial_data <- load_discrete()
old_date <- substr(file.info(data_fpath)$mtime, 1, 10)

# Determine initial date, stations for tab 1 (single site) plot
initial_date_1 <- initial_data$CollectDate[1]
initial_station_1 <- initial_data$Locator[1]
initial_stations <- sort(unique(initial_data$Locator))
initial_dates <- sort(unique(initial_data %>% 
                                 filter(Locator == initial_station_1) %>% 
                                 pull(CollectDate)), 
                      decreasing = TRUE)

#### UI ####
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
    
    # Checkboxes
    fluidRow(
        column(3, 
               checkboxInput("log", 
                             "Nutrients & TSS on log scale", 
                             value = FALSE)),
        column(3, 
               checkboxInput("include_bad", 
                             "Include bad data (shown as squares)", 
                             value = FALSE))
    ), 
    
    tags$hr(),
    
    # Tab selections: single site, Central Basin, Whidbey Basin
    tabsetPanel(
        tabPanel("Single site", 
                 fluidRow(
                     column(2, 
                            selectInput("sites_1", 
                                        label = "Site:", 
                                        choices = initial_stations, 
                                        selected = initial_station_1)), 
                     column(2, 
                            selectInput("dates_1", 
                                        label = "Date:", 
                                        choices = initial_dates, 
                                        selected = initial_date_1))
                 ), 
                 # Test plot
                 plotOutput("test_plot")
        ), 
        tabPanel("Central Basin", 
        ), 
        tabPanel("Whidbey Basin", 
        )
    )
)

#### SERVER ####
# Define server logic
server <- function(input, output, session) {

    discrete_data <- reactiveValues(data = initial_data)
    file_date <- reactiveVal(old_date)
    
    # Set scale setting
    scale_setting <- reactive({
        ifelse(input$log, "log", "linear")
    })
    
    # Update data and more on button push - refresh_data
    observeEvent(input$refresh_data, {
        update_discrete()
        discrete_data$data <- load_discrete()
        
        # "Last updated" date text
        new_date <- Sys.Date()
        file_date(new_date)
        
        # Station list tab 1 - keep previous selection
        updateSelectInput(session, 
                          "sites_1", 
                          choices = sort(unique(discrete_data$data$Locator)), 
                          selected = input$sites)
        
        # Date list tab 1 - automatically selects most recent
        updateSelectInput(session, 
                          "dates_1", 
                          choices = sort(
                              unique(
                                  discrete_data$data %>% 
                                      filter(Locator == input$sites) %>% 
                                      pull(CollectDate)), 
                              decreasing = TRUE))
    })
    
    # Update tab 1 dates list if station selection changes; select most recent
    observeEvent(input$sites_1, {
        updateSelectInput(session, 
                          "dates_1", 
                          choices = sort(
                              unique(
                                  discrete_data$data %>% 
                                      filter(Locator == input$sites_1) %>% 
                                      pull(CollectDate)), 
                              decreasing = TRUE))
    })

    # Render data date text
    output$date_data <- renderText(
        paste("Data last updated:", 
              file_date())
    )
    
    # Render simple plot
    output$test_plot <- renderPlot({
        ggplot(data = discrete_data$data %>% 
                   filter(ParmId == 14, 
                          Locator == input$sites_1, 
                          !is.na(Value), 
                          !is.na(Depth), 
                          !is.na(CollectDate)), 
               aes(x = Depth, y = Value, 
                   color = CollectDate == input$dates_1)) + 
            labs() + 
            geom_point() + 
            coord_flip() + 
            scale_x_reverse()
    })
}

#### RUN ####
# Run the application 
shinyApp(ui = ui, server = server)
