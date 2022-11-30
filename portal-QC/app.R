#### SETUP ####
library(shiny)
library(here)
library(tidyverse)
library(kcmarine)
library(plotly)
library(lubridate)
library(htmlwidgets)
source(here("src", "utility_functions.R"))

## Load in data
# Define discrete data path
data_fpath <- here("data", "discrete_data.csv")

# Define initial data and data date
if (!file.exists(data_fpath)) {
    update_discrete()
}
initial_data <- process_discrete(load_discrete())
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
                 plotlyOutput("test_plot")
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
    
    # Set shape for bad data
    shape_setting <- reactive({
        ifelse(input$include_bad, 15, NA)
    })
    
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
    output$test_plot <- renderPlotly({
        p <- ggplot(data = discrete_data$data %>% 
                        filter(Parameter == "Nitrite + Nitrate Nitrogen", 
                               Locator == input$sites_1, 
                               !is.na(Depth)) %>% 
                        mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                                 NonDetect == TRUE ~ "ND", 
                                                 TRUE ~ "Regular")), 
               aes(x = Depth, y = Value, 
                   color = CollectDate == input$dates_1, 
                   shape = Shape, 
                   customdata = URL, 
                   text = paste0(Value, "; ", CollectDate))) + 
            theme_bw() + 
            theme(legend.position = "none") +
            labs(x = "Depth (m)", 
                 y = "Nitrate + nitrite N (mg/L)") + 
            geom_point() + 
            coord_flip() + 
            {if (input$log) scale_y_continuous(trans = "log")} + 
            scale_x_reverse() + 
            scale_color_manual(values = c("TRUE" = "red", 
                                          "FALSE" = "black")) + 
            scale_shape_manual(values = c("Bad" = shape_setting(), 
                                          "ND" = 6, 
                                          "Regular" = 16))
        pp <- ggplotly(p, tooltip = c("text"))
        onRender(
            pp, "
            function(el) {
            el.on('plotly_click', function(d) {
            var url = d.points[0].customdata;
            //url
            window.open(url);
            }); 
            }"
        )
    })
}

#### RUN ####
# Run the application 
shinyApp(ui = ui, server = server)
