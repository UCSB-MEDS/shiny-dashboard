age_plot <- function(df, color, year_str, prog_input){
  # df = data frame to be to create age_program_groups (i.e. bren_apps)
  # color = program color to be used to fill bar plots (i.e. mesm_color)
  # year_str = chr string of years data comes from (i.e. "2016-2021" or "2021")
  # prog_input = input selected (i.e. input$age_prog)
  
  ## DATA WRANGLING ##
  # 2016-2021
  age_program_groups <- df %>% 
    select(c("ay_year",
             "application_id",
             "objective1",
             "dob")) %>%
    # calculate age
    mutate(dob_year = year(dob)) %>%
    mutate(age = ay_year - dob_year) %>% 
    filter(age > 18) %>% # remove outliers incorrectly submitted dobs
    mutate(age_group = case_when(age >= 20 & age <= 22 ~ "20-22",
                                 age >= 23 & age <= 24 ~ "23-24",
                                 age >= 25 & age <= 29 ~ "25-29",
                                 age >= 30 & age <= 34 ~ "30-34",
                                 age >= 35 & age <= 39 ~ "35-39",
                                 age >= 40 & age <= 49 ~ "40-49",
                                 age >= 50 & age <= 64 ~ "50+",
                                 age >= 65 ~ "50+")) %>% 
    group_by(objective1, age_group) %>% 
    summarize(age_group_counts = n())
  
  # left join with tot number of students and calculate percentages
  # reactive
  age_stats <- reactive({
    left_join(age_program_groups, tot_5yr, by = "objective1") %>% 
      mutate(age_percent = round((age_group_counts / tot) * 100, 1)) %>% 
      filter(objective1 == prog_input)
  }) # EO reactive age_stats df
  
  
  ## PLOTTING ##
  age_ggplot <- ggplot(data = age_stats(),
                       aes(
                         x = age_group,
                         y = age_percent,
                         text = paste0(
                           "Age group: ",
                           age_group,
                           "\n",
                           "Percent: ",
                           age_percent,
                           "%",
                           "\n",
                           "Sample size: ",
                           tot
                         )
                       )) +
    geom_bar(stat = "identity",
             fill = color) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          plot.subtitle = element_text(face = "italic")) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
    labs(
      title = paste0(
        "Age of graduate students at start of ",
        prog_input,
        " program (",
        year_str,
        ")"
      ),
      x = NULL,
      y = NULL
    )
  
  plotly::ggplotly(age_ggplot, tooltip = "text") %>%
    layout(title = list(font = list(size = 16))) %>%
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