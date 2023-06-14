#### SETUP ####
library(shiny)
library(here)
library(tidyverse)
library(kcmarine)
library(plotly)
library(lubridate)
library(htmlwidgets)
source(here("src", "utility_functions.R"))

# Suppress warnings (just for cleanliness)
options(warn = -1) 

## Load in data
# Define initial data and data date
if (!file.exists(data_fpath_rda)) {
    update_discrete()
}
initial_data <- process_discrete(load_discrete())
old_date <- substr(file.info(data_fpath_rda)$mtime, 1, 10)

# Determine initial date, stations for tab 1 (single site) plot
initial_date_1 <- initial_data$CollectDate[1]
initial_station_1 <- initial_data$Locator[1]
initial_stations <- sort(unique(initial_data$Locator))
initial_dates_1 <- sort(unique(initial_data %>% 
                                 filter(Locator == initial_station_1) %>% 
                                 pull(CollectDate)), 
                      decreasing = TRUE)

# Determine initial date range for tab 2 (Central Basin) plot
initial_dates_2 <- unique(initial_data %>% 
                            filter(Locator %in% locators_cb) %>% 
                            arrange(CollectDate, decreasing = FALSE) %>% 
                            mutate(WeekDate = rev(factor(WeekDate))) %>% 
                            pull(WeekDate))
initial_date_2 <- initial_dates_2[1]

# Determine initial date range for tab 3 (Whidbey Basin) plot
initial_dates_3 <- unique(initial_data %>% 
                              filter(Locator %in% locators_wb) %>% 
                              arrange(CollectDate, decreasing = FALSE) %>% 
                              mutate(WeekDate = rev(factor(WeekDate))) %>% 
                              pull(WeekDate))
initial_date_3 <- initial_dates_3[1]

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
                                        choices = initial_dates_1, 
                                        selected = initial_date_1))
                 ), 
                 # Plots!
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_T")), 
                     column(4, plotlyOutput("plot_oss_density"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_DOfield")), 
                     column(4, plotlyOutput("plot_oss_DO"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_Sfield")), 
                     column(4, plotlyOutput("plot_oss_S"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_pHfield")), 
                     column(4, plotlyOutput("plot_oss_pH"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_P")), 
                     column(4, plotlyOutput("plot_oss_TotalP"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_NH3")), 
                     column(4, plotlyOutput("plot_oss_NNN"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_TotalN")), 
                     column(4, plotlyOutput("plot_oss_TSS"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_PAR")), 
                     column(4, plotlyOutput("plot_oss_sPAR"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_light")), 
                     column(4, plotlyOutput("plot_oss_chl"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_pheo")), 
                     column(4, plotlyOutput("plot_oss_chlfield"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_Si")), 
                     column(4, plotlyOutput("plot_oss_TOC"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_DOC")), 
                     column(4, plotlyOutput("plot_oss_fecal"))
                 ), 
                 fluidRow(
                     column(4, plotlyOutput("plot_oss_entero"))
                 )
        ), 
        tabPanel("Central Basin", 
                 fluidRow(
                     column(2, 
                            selectInput("parm_2", 
                                        label = "Parameter:", 
                                        choices = parms, 
                                        selected = parms[1])), 
                     column(2, 
                            selectInput("dates_2", 
                                        label = "Date:", 
                                        choices = initial_dates_2, 
                                        selected = initial_date_2))
                 ), 
                 fluidRow(
                     column(8, plotlyOutput("plot_cb", 
                                            height = "1600px"))
                 )
        ), 
        tabPanel("Whidbey Basin", 
                 fluidRow(
                     column(2, 
                            selectInput("parm_3", 
                                        label = "Parameter:", 
                                        choices = parms, 
                                        selected = parms[1])), 
                     column(2, 
                            selectInput("dates_3", 
                                        label = "Date:", 
                                        choices = initial_dates_3, 
                                        selected = initial_date_3))
                 ), 
                 fluidRow(
                     column(8, plotlyOutput("plot_wb", 
                                            height = "800px"))
                 ) 
        )
    )
)

#### SERVER ####
# Define server logic
server <- function(input, output, session) {

    discrete_data <- reactiveValues(data = initial_data)
    file_date <- reactiveVal(old_date)
    
    # Update data and more on button push - refresh_data
    observeEvent(input$refresh_data, {
        update_discrete()
        discrete_data$data <- process_discrete(load_discrete())
        
        # "Last updated" date text
        new_date <- Sys.Date()
        file_date(new_date)
        
        # Station list tab 1 - keep previous selection
        updateSelectInput(session, 
                          "sites_1", 
                          choices = sort(unique(discrete_data$data$Locator)), 
                          selected = input$sites_1)
        
        # Date list tab 1 - automatically selects most recent
        updateSelectInput(session, 
                          "dates_1", 
                          choices = sort(
                              unique(
                                  discrete_data$data %>% 
                                      filter(Locator == input$sites_1) %>% 
                                      pull(CollectDate)), 
                              decreasing = TRUE))
        
        # Date list tab 2 - automatically selects most recent
        updateSelectInput(session, 
                          "dates_2", 
                          choices = unique(
                            discrete_data$data %>% 
                              filter(Locator %in% locators_cb) %>% 
                              arrange(CollectDate, decreasing = FALSE) %>% 
                              mutate(WeekDate = rev(factor(WeekDate))) %>% 
                              pull(WeekDate)
                          ))
        
        # Date list tab 3 - automatically selects most recent
        updateSelectInput(session, 
                          "dates_3", 
                          choices = unique(
                            discrete_data$data %>% 
                              filter(Locator %in% locators_wb) %>% 
                              arrange(CollectDate, decreasing = FALSE) %>% 
                              mutate(WeekDate = rev(factor(WeekDate))) %>% 
                              pull(WeekDate)
                          ))
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
    
    # Render offshore single site plots
    source(here("src", "make_plots_oss.R"), local = TRUE)
    
    # Render offshore Central Basin plots
    source(here("src", "make_plots_cb.R"), local = TRUE)
    
    # Render offshore Whidbey Basin plots
    source(here("src", "make_plots_wb.R"), local = TRUE)
}

#### RUN ####
# Run the application 
shinyApp(ui = ui, server = server)
