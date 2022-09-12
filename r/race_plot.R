#' RACE PLOTLY
#'
#' @param df data frame used to create category_ipeds
#' @param prog_input input id 
#'
#' @return interactive race plot depending on degree program selected by input 
#'
#' @examples race_plot(df = ipeds, prog_input = input$race)
#' 
race_plot <- function(df, prog_input){

  ## DATA WRANGLING ##
  # 2017-curr_year
  # reactive
  category_ipeds_stats <- reactive({
    if (prog_input == "All Programs") {
      df %>% 
        group_by(category_ipeds) %>% 
        summarize(count = n()) %>% 
        # total number of enrolled students in the past 5 years
        # MEDS(57) + MESM(508) + PHD(62)
        mutate(size = 627) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      df %>% 
        group_by(objective1,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == prog_input)
      
    } # EO else statement
    
  }) # EO reactive category_ipeds_stats df
  
  
  ## PLOTTING ##
  # ggplot
  ipeds_gg <- ggplot(data = category_ipeds_stats(),
                     aes(x = category_ipeds,
                         y = percent,
                         fill = category_ipeds,
                         text = paste0(category_ipeds, " (", percent, "%", ")", "\n",
                                       "Sample size: ", size))
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none",
      plot.subtitle = element_text(face = "italic")
    ) +
    scale_x_discrete(limits = rev(levels(category_ipeds_stats()$category_ipeds)),
                     labels = function(x)
                       str_wrap(x, width = 35)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#003660", # ucsb navy
        "Asian" = "#047c91", # ucsb aqua
        "Black or African American" = "#dcd6cc", # ucsb clay
        "Hispanic or Latino" = "#6d7d33", # ucsb moss
        "Native Hawaiian or Other Pacific Islander" = "#9cbebe", # ucsb mist
        "White" = "#dce1e5", # ucsb light grey
        "Two or more races" = "#79a540", # bren leaf green
        "Unknown race and ethnicity" = "#09847a" # ucsb sea green
      )
    ) +
    labs(
      title = paste0("IPEDS Categories and Distribution", "\n",
                     "(", prog_input, ")"),
      x = NULL,
      y = NULL
    )
  
  # plotly
  p <- plotly::ggplotly(ipeds_gg, tooltip = "text") %>% 
    config(
      modeBarButtonsToRemove = list(
        "pan",
        "select",
        "lasso2d",
        "autoScale2d",
        "hoverClosestCartesian",
        "hoverCompareCartesian"
      )
    )
  
  return(p)
  
  
} # EO race plot function