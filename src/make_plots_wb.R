output$plot_wb <- renderPlotly({
  p <- ggplot(data = discrete_data$data %>% 
                mutate(Locator = factor(Locator, 
                                        levels = locators_wb), 
                       Shape = case_when(QualityID == 4 ~ "Bad", 
                                         NonDetect == TRUE ~ "ND", 
                                         TRUE ~ "Regular")) %>% 
                filter(Parameter == input$parm_3, 
                       Locator %in% locators_wb, 
                       !is.na(Depth)) %>% 
                {if (input$include_bad) . else filter(., QualityID != 4)}) + 
    theme_bw() + 
    theme(legend.position = "none", 
          panel.spacing.y = unit(0, "lines"), 
          panel.spacing.x = unit(0, "lines")) + 
    labs(x = "Depth (m)", 
         y = parm_units$Label[parm_units$Parameter == input$parm_3]) + 
    coord_flip() + 
    {if (input$log & (input$parm_3 %in% parms_log)) scale_y_continuous(trans = "log")} + 
    scale_x_reverse()
  if (nrow(p$data) > 0) {
    p <- p + 
      geom_point(aes(x = Depth, y = Value, 
                     color = WeekDate == input$dates_3, 
                     shape = Shape, 
                     customdata = URL, 
                     text = paste0(Value, "; ", CollectDate))) + 
      facet_wrap(~ Locator, ncol = 3, scales = "free_y") + 
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