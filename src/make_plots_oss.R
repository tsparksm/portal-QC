output$plot_oss_T <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Temperature", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Temperature (\u00B0C)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_density <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Density", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Density (kg/m<sup>3</sup>)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_DOfield <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Dissolved Oxygen, Field", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "DO, Field (mg/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_DO <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Dissolved Oxygen", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "DO (mg/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_Sfield <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Salinity, Field", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Salinity, Field (PSU)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_S <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Salinity", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Salinity (PSU)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_pHfield <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "pH, Field", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "pH, Field") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_pH <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "pH", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "pH") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_P <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Orthophosphate Phosphorus", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Orthophosphate P (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_TotalP <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Total Phosphorus", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Total P (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_NH3 <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Ammonia Nitrogen", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Ammonia N (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_NNN <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Nitrite + Nitrate Nitrogen", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Nitrate + nitrite N (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
    if (nrow(p$data) > 0) {
      p <- p + 
        geom_point(aes(x = Depth, y = Value, 
                       color = CollectDate == input$dates_1, 
                       shape = Shape, 
                       customdata = URL, 
                       text = paste0(Value, "; ", CollectDate))) + 
        scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                      "FALSE" = alpha("black", 0.2))) + 
        scale_shape_manual(values = c("Bad" = 15, 
                                      "ND" = 6, 
                                      "Regular" = 16))
    }
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

output$plot_oss_TotalN <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Total Nitrogen", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Total N (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_TSS <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Total Suspended Solids", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "TSS (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} +  
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_PAR <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Light Intensity (PAR)", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "PAR (\u00B5mol/sm<sup>2</sup>)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_sPAR <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Surface Light Intensity (PAR)", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Surface PAR (\u00B5mol/sm<sup>2</sup>)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_light <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Light Transmissivity", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Light Transmissivity (%)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_chl <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Chlorophyll a", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Chlorophyll <i>a</i> (\u00B5g/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_pheo <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Pheophytin a", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Pheophytin <i>a</i> (\u00B5g/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_chlfield <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Chlorophyll, Field", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Chlorophyll <i>a</i>, Field (\u00B5g/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_Si <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Silica", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Silica (mg/L)") + 
    coord_flip() + 
    {if (input$log) scale_y_continuous(trans = "log", 
                                       labels = scales::number_format(accuracy = 0.001))} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_TOC <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Total Organic Carbon", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "TOC (mg/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_DOC <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Dissolved Organic Carbon", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "DOC (mg/L)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_fecal <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Fecal Coliform", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Fecal Coliform (CFU/100 mL)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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

output$plot_oss_entero <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                filter(Parameter == "Enterococcus", 
                       Locator == input$sites_1, 
                       !is.na(Depth)) %>% 
                mutate(Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none") +
    labs(x = "Depth (m)", 
         y = "Enterococcus (CFU/100 mL)") + 
    coord_flip() + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = CollectDate == input$dates_1, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      scale_color_manual(values = c("TRUE" = alpha("red", 0.9), 
                                    "FALSE" = alpha("black", 0.2))) + 
      scale_shape_manual(values = c("Bad" = 15, 
                                    "ND" = 6, 
                                    "Regular" = 16))
  }
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