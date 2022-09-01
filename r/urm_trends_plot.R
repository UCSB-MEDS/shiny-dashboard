urm_trends_plot <- function(df, color, prog_input){
  # df = data frame used to create age_program_groups (i.e. urms_trends())
  # color = program color used to fill bar plots (i.e. mesm_color)
  # prog_input = input selected (i.e. input$urm_trends)
  
  urm_trends_gg <- ggplot(data = df,
                          aes(x = ay_year,
                              y = percent,
                              text = paste0("URM ", "(", percent, "%", ")", "\n",
                                            "Sample size: ", size))) +
    geom_bar(stat = "identity",
             fill = color) +
    theme_minimal() +
    theme(legend.position = "none",
          panel.grid.minor = element_blank()) +
    scale_x_continuous(breaks = seq(max(df$ay_year),
                                    min(df$ay_year))) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(title = paste0("Underrepresented Minority Trends", " (", prog_input, ")"),
         y = NULL,
         x = NULL) 
  
  plotly::ggplotly(urm_trends_gg, tooltip = "text") %>%
    config(modeBarButtonsToRemove = list("pan", 
                                         "select", 
                                         "lasso2d", 
                                         "autoScale2d", 
                                         "hoverClosestCartesian", 
                                         "hoverCompareCartesian"))
  
} # EO urm trends plot function