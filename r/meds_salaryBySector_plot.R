#' meds_salaryBySector_plot
#'
#' @param data MUST be 'meds_placement' df
#'
#' @return
#' @export
#'
#' @examples
#' ## SC NOTE 2023-02-06: use this fxn to create static plot for MEDS salary by sector since only 1 year of data (no need for radioButtons)
meds_salaryBySector_plot <- function(input, data) {
  
  # wrangle data/create reactive df for salary by sector plot ----
  salary_sector <- reactive({
    
    radioButton_yearInput <- input$meds_salarySector_year
    placement_size <- meds_placement_size
    response_num <- sum(meds_placement_size$responses)
    
    if (radioButton_yearInput == "All Years") {
      data %>% 
        select(class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us, sector_type) %>%
        # assign private, public, and other
        # mutate(sector_type = case_when(
        #   employer_sector %in% c("Consulting", "Corporate") ~ "Private",
        #   employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
        #   employer_sector %in% c("Foreign Government", "Other") ~ "Other",
        #   TRUE ~ employer_sector
        # )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>%
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        group_by(sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>%
        mutate(responses = response_num)
      
    }  # END if statement
    
    else {
      
      data %>% 
        select(class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us) %>%
        # assign private, public, and other
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        # filter for year
        filter(class_year == radioButton_yearInput) %>%
        group_by(class_year, sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median", "Low", "High"),
                     names_to = "range", values_to = "values") %>%
        left_join(placement_size, by = "class_year")
      
    } # END else statement
    
  }) 
  
  
  #-------
    # salary_sector <- data %>% 
    # select(class_year, employer_sector, employment_type, compensation_frequency, estimated_annual_compensation_us, sector_type) %>%
    # # assign private, public, and other - SC NOTE 2023-02-16: MOVE TO CLEANING SCRIPT 
    # # mutate(sector_type = case_when(
    # #   employer_sector %in% c("Consulting", "Corporate") ~ "Private",
    # #   employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
    # #   employer_sector %in% c("Foreign Government", "Other") ~ "Other",
    # #   TRUE ~ employer_sector
    # # )) %>% 
    # mutate(sector_type = factor(sector_type, levels = c("Private", "Public", "Non-Profit", "Other"))) %>%
    # # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
    # # (41 obs removed)
    # # only 1 NA
    # filter(employment_type == "Full-Time Job") %>% 
    # # remove $0 compensation (5 tot)
    # filter(estimated_annual_compensation_us != 0) %>% 
    # # remove stipend compensation_frequency
    # filter(compensation_frequency != "Stipend") %>%
    # # filter for year
    # filter(class_year %in% c("2022")) %>%
    # group_by(class_year, sector_type) %>% 
    # summarize(Median = median(estimated_annual_compensation_us),
    #           Low = min(estimated_annual_compensation_us),
    #           High = max(estimated_annual_compensation_us)) %>% 
    # pivot_longer(cols = c("Median", "Low", "High"),
    #              names_to = "range", values_to = "values") %>%
    # left_join(meds_placement_size, by = "class_year")
  
  # render plotly ----
  plotly::renderPlotly({
    
    placement_size <- meds_placement_size
    response_num <- sum(placement_size$responses)
    # create ggplot
    comp_sector <- ggplot(data = salary_sector(), aes(x = sector_type, y = values,fill = reorder(range, values),
                                                      text = paste0(sector_type, "\n", range, ": ", "$", values, "\n", "Number of respondents: ", response_num))) +
      geom_bar(stat = "identity", position = "dodge") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(), breaks = seq(0, 100000, 20000)) +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 25)) +
      scale_fill_manual(values = c("High" = "#003660", "Median" = "#047c91", "Low" = "#dcd6cc")) + # ucsb navy, ucsb aqua, ucsb clay
      labs(title = "MEDS Alumni Salary Compensation by Sector",
           x = NULL, y = "Dollars ($)", fill = NULL)
    
    # convert to plotly
    plotly::ggplotly(comp_sector, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h", y = -0.25, x = 0.2)) %>%
      config(modeBarButtonsToRemove = list("pan", "select", "lasso2d", "autoScale2d", "hoverClosestCartesian", "hoverCompareCartesian"))
    
  }) 
  
}

