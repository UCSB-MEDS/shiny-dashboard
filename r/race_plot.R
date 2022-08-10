race_plot <- function(df, year_str, prog_input){
  # df = data frame to be to create age_program_groups (i.e. bren_apps)
  # color = program color to be used to fill bar plots (i.e. mesm_color)
  # year_str = chr string of years data comes from (i.e. "2016-2021" or "2021")
  # prog_input = input selected (i.e. input$age_prog)
  
  ## DATA WRANGLING ##
  # 2016-2021
  category_ipeds <- bren_apps %>% 
    select(ay_year,
           objective1,
           background,
           category,
           hispanic_latino) %>% 
    # replace NULL string with NA
    naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
    mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
    # assign race using ipeds definition
    mutate(category_ipeds = case_when(
      str_detect(category, ";") == TRUE ~ "Two or more races",
      str_detect(category, "American Indian / Alaska Native") == TRUE & hispanic_latino == FALSE ~ "American Indian or Alaska Native",
      str_detect(category, "Asian / Asian American") == TRUE & hispanic_latino == FALSE ~ "Asian",
      str_detect(category, "African American / Black") == TRUE & hispanic_latino == FALSE ~ "Black or African American",
      str_detect(category, "Native Hawaiian / other Pacific Islander") == TRUE & hispanic_latino == FALSE ~ "Native Hawaiian or Other Pacific Islander",
      str_detect(category, "White / Caucasian") == TRUE & hispanic_latino %in% c(FALSE, NA) ~ "White",
      hispanic_latino == TRUE ~ "Hispanic or Latino",
      is.na(category) == TRUE ~ "Unknown race and ethnicity"
    )) %>% 
    mutate(category_ipeds = factor(category_ipeds, levels = c("American Indian or Alaska Native",
                                                              "Asian",
                                                              "Black or African American",
                                                              "Hispanic or Latino",
                                                              "Native Hawaiian or Other Pacific Islander",
                                                              "White",
                                                              "Two or more races",
                                                              "Unknown race and ethnicity"))) %>% 
    group_by(objective1,
             category_ipeds) %>% 
    summarize(count = n())
  
  # left join with tot number of students and calculate percentages
  # reactive
  category_ipeds_stats <- reactive({
    left_join(category_ipeds, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 == prog_input)
  }) # EO reactive category_ipeds_stats df
  
  
  ## PLOTTING ##
  # ggplot
  ipeds_gg <- ggplot(data = category_ipeds_stats(),
                          aes(x = category_ipeds,
                              y = count,
                              fill = category_ipeds,
                              text = paste0("Category: ", category_ipeds, "\n",
                                            "Percent: ", percent, "%", "\n",
                                            "Sample Size: ", tot))
  ) +
    geom_bar(stat = "identity") +
    coord_flip() +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "none"
    ) +
    scale_x_discrete(limits = rev(levels(category_ipeds_stats()$category_ipeds))) +
    scale_fill_manual(
      values = c(
        "American Indian or Alaska Native" = "#09847a",
        "Asian" = "#09847a",
        "Black or African American" = "#09847a",
        "Hispanic or Latino" = "#09847a",
        "Native Hawaiian or Other Pacific Islander" = "#09847a",
        "White" = "#09847a",
        "Two or more races" = "#09847a",
        "Unknown race and ethnicity" = "#09847a"
      )
    ) +
    labs(
      title = paste0("IPEDS Categories and Distribution of ", prog_input, " program",
                     "\n",
                     "(", year_str, ")"),
      x = NULL,
      y = NULL
    )
  
  # plotly
  plotly::ggplotly(ipeds_gg, tooltip = "text") %>% 
    layout(title = list(font = list(size = 17))) %>% 
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

  
} # EO age plot function