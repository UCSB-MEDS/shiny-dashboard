race_plot <- function(df, color, year_str, prog_input){
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
                          aes(x = count,
                              y = reorder(category_ipeds, count),
                              text = paste0("Category: ", category_ipeds, "\n",
                                            "Percent: ", percent, "%", "\n",
                                            "Sample Size: ", tot))
  ) +
    geom_bar(stat = "identity",
             fill = color) +
    theme_minimal() +
    theme(
      panel.grid.minor = element_blank()
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