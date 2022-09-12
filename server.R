# server instructions
server <- function(input, output, session){

  # WELCOME / LANDING PAGE -----
  
  
  # CAREER DB ----
  
  ## SO career valuebox stats ----
  ## * placement status stat ----
  
  ## DATA WRANGLING ##
  # status df for valuebox stat
  status_stat <- mesm_status %>% 
    select(mesm_class_year,
           member_status) %>% 
    # assign placement status label
    mutate(status = case_when(
      member_status %in% c("FT Career",
                           "FT Temporary Career",
                           "PT Temporary Career",
                           "FT Career-Sponsored",
                           "PT Career",
                           "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off",
                           "Searching") ~ "Searching or Time Off",
      member_status %in% c("FT New Business",
                           "FT Eco-E") ~ "New Business",
      member_status %in% c("Internship/Fellowship",
                           "Continuing Internship",
                           "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
      TRUE ~ member_status
    )) %>% 
    # assign placed vs not placed
    mutate(placed = case_when(
      status == "Searching or Time Off" ~ "Not Placed",
      TRUE ~ "Placed"
    )) %>% 
    # calculate totals
    group_by(placed) %>% 
    summarize(count = n()) %>% 
    # calculate percentage
    # used tot mesm_responses (79+74+82)
    mutate(percent = round((count / 235) * 100)) %>% 
    filter(placed == "Placed")
  
  ## VALUEBOX ##
  output$placement_stat <- renderValueBox({
    
    shinydashboard::valueBox(
      "of graduates were employed 6 months after graduation",
      value = paste0(status_stat$percent, "%"),
      icon = icon("house"),
      color = "green"
    ) # EO valueBox
  }) # EO MESM valueBox prog size
  
  ## * bren network stat ----
  ## DATA WRANGLING ##
  mesm_brenNet <- mesm_placement %>% 
    group_by(job_source) %>% 
    summarize(count = n()) %>% 
    # calculate percentages
    # total responses (196)
    mutate(percent = round((count / 196) * 100)) %>% 
    filter(job_source == "Bren School Network")
  
  ## VALUEBOX ##
  output$brenNet_stat <- renderValueBox({
    
    valueBox(
      "of graduates found their jobs through the Bren School Network",
      value = paste0(mesm_brenNet$percent, "%"),
      icon = icon("briefcase"),
      color = "blue"
    ) # EO valueBox
  }) # EO MESM bren network stat
  
  
  ## * MESM satisfaction initial placement stat ----
  ## DATA WRANGLING ##
  mesm_satisfaction_stat <- mesm_placement %>% 
    group_by(placement_satisfaction) %>% 
    summarize(count = n()) %>% 
    # calculate percentages
    # total responses (196)
    mutate(percent = round((count / 196) * 100)) %>% 
    filter(placement_satisfaction %in% c("Satisfied", "Very Satisfied"))
  
  # isolate just satisfied percent
  satisfied_num <- mesm_satisfaction_stat %>% filter(placement_satisfaction == "Satisfied") %>% 
    select(percent)
  # isolate just very satisfied percent
  verySatisfied_num <- mesm_satisfaction_stat %>% filter(placement_satisfaction == "Very Satisfied") %>% 
    select(percent)
  # add percentages together for valueBox
  total_satisfied <- satisfied_num$percent + verySatisfied_num$percent
  
  ## VALUEBOX ##
  output$mesm_satisfied_stat <- renderValueBox({
    
    valueBox("of graduates ranked being “satisfied” or “very satisfied” with their initial job placement",
             value = paste0(total_satisfied, "%"),
             icon = icon("heart"),
             color = "light-blue"
    ) # EO valueBox overall satisfaction stat
  }) # EO MESM satisfaction initial placement stat
  
  
  
  
  ## SO career placements table  ----
  ## DATA WRANGLING ##
  employer <- mesm_placement %>% 
    select(c(employer_account_name,
             employer_sector)) %>%
    mutate(employer_account_name = case_when(
      employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
      employer_account_name == "Clean, Renewable and Environmental Opportunities (CREO)CREO" ~ "Clean, Renewable and Environmental Opportunities (CREO)",
      employer_account_name == "Environmental Incentives. LLC" ~ "Environmental Incentives, LLC",
      TRUE ~ employer_account_name
    )) %>% 
    mutate(employer_account_name = str_replace_all(employer_account_name, "Formerly", "formerly")) %>% 
    group_by(employer_account_name,
             employer_sector) %>% 
    summarize(freq = n())
  
  ## TABLE ##
  output$career_employ_sector_tbl <- DT::renderDataTable({
    DT::datatable(
      employer,
      colnames = c("Employer", "Sector", "# of alumni"),
      # caption = htmltools::tags$caption(style = "caption-side: top; text-align: left",
      #                                   htmltools::em("Bren MESM Alumni Employers and Sectors since 2019")),
      class = "cell-border stripe",
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom = 'Bftipr'
      ) # EO options
      
    ) # EO datatable
  }) # EO renderDataTable
  
  ## SO mesm location ----
  ## DATA WRANGLING ##
  # ca / out of state / international
  us_names <- c("USA", "US", "Usa")
  ca_names <- c("Ca", "CALIFORNIA")
  
  placement_location <- mesm_placement %>% 
    select(c(
      employer_account_name,
      work_location_city,
      mesm_class_year,
      work_location_state,
      work_location_country
    )) %>% 
    # standardize state abbreviation values
    mutate(work_location_state = case_when(
      work_location_state %in% ca_names ~ "CA",
      work_location_state == "Maryland" ~ "MD",
      work_location_state == "Washington" ~ "WA",
      work_location_state == "District of Columbia" ~ "DC",
      work_location_state == "N/A" ~ NA_character_, 
      work_location_state == "michoacan" ~ "Michoacan",
      # specifically assign correct work location state
      work_location_city == "Washington DC" ~ "DC",
      work_location_city == "Oxnard" ~ "CA",
      work_location_city == "Santa Cruz" ~ "CA",
      work_location_city == "Fort Collins" ~ "CO",
      work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
      TRUE ~ work_location_state
    )) %>% 
    # standardize united states values
    mutate(work_location_country = case_when(
      work_location_country %in% us_names ~ "United States",
      # specificallly assign correct country values
      work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
      work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
      TRUE ~ work_location_country
    )) %>% 
    # assign ca / out of state / international
    mutate(location = case_when(
      work_location_state == "CA" ~ "Domestic (California)",
      work_location_state != "CA" & work_location_country == "United States" ~ "Domestic (Out of State)",
      work_location_country != "United States" ~ "International"
    )) %>% 
    group_by(mesm_class_year,
             location) %>%
    summarize(location_count = n())
  
  # calculating percentages
  placement_location_stats <- placement_location %>% 
    left_join(placement_size, by = "mesm_class_year") %>%
    mutate(percent = round((location_count / mesm_responses) * 100, 1))
  
  ## PLOTTING ##
  output$mesm_location <- plotly::renderPlotly({
    # ggplot
    location_gg <- ggplot(data = placement_location_stats,
                          aes(x = mesm_class_year,
                              y = percent,
                              fill = reorder(location, percent),
                              text = paste0(location, " (", percent, "%", ")", 
                                            "\n",
                                            "Number of respondents: ", mesm_responses)
                              )) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(placement_location_stats$mesm_class_year),
                                      max(placement_location_stats$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Where MESM alumni are working 6 months after graduating",
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      scale_fill_manual(values = c("Domestic (California)" = "#9cbebe",
                                   "Domestic (Out of State)" = "#003660",
                                   "International" = "#dcd6cc"))
    
    # plotly
    plotly::ggplotly(location_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           y = -0.1),
             title = list(font = list(size = 15.5))) %>% 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
    
  }) # EO mesm location
  
  ## SO mesm map ----
  ## DATA WRANGLING ##
  # domestic map polygons
  mesm_domestic <- mesm_map %>% 
    filter(work_location_country == "United States") %>% 
    select(-c(lat, long)) %>% 
    left_join(us_state_geoms, by = c("work_location_state" = "state_abbrev")) %>% 
    select(-c(fips, work_location_state)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326)
  
  mesm_domestic_stats <- mesm_domestic %>% 
    group_by(state) %>% 
    summarize(count = n()) %>% 
    mutate(state = paste0(state, " (", count, ")"))
  
  ## PLOTTING MAP ##
  output$car_alumniMap <- tmap::renderTmap({ 
    
    tmap_mode("view")
    
    tm_shape(mesm_domestic_stats) +
      tm_tiles(leaflet::providers$CartoDB.PositronNoLabels) +
      tm_polygons(
        col = "count",
        style = "jenks",
        n = 4,
        palette = "YlGn",
        popup.vars = c("Number of alumni: " = "count"),
        legend.show = FALSE
      ) +
      tm_view(set.view = c(-117, 37, 3)) # long, lat, zoom
    
  }) # EO alumni map
  
  
  ## SO international placements table ----
  ## DATA WRANGLING ##
  international_tbl <- mesm_placement %>% 
    filter(!work_location_country %in% c("US", "Usa", "USA", "United States"),
           !is.na(work_location_country)) %>% 
    mutate(employer_account_name = case_when(
      employer_account_name == "The R?hui Forum and Resource Center" ~ "Rāhui Forum and Resource Center",
      TRUE ~ employer_account_name
    )) %>% 
    group_by(employer_account_name,
             employer_sector,
             work_location_country) %>% 
    summarize(count = n())
  
  ## TABLE ##
  output$international_place <- DT::renderDataTable({
    DT::datatable(
      international_tbl,
      colnames = c("Employer", "Sector", "Location", "# of alumni"),
      # caption = htmltools::tags$caption(style = "caption-side: top; text-align: left",
      #                                   htmltools::em("Bren MESM Alumni Employers and Sectors since 2019")),
      class = "cell-border stripe",
      rownames = FALSE,
      options = list(
        pageLength = 9,
        dom = 'Btipr'
      ) # EO options
    ) # EO datatable
  
  }) # EO renderDataTable
  
  ## SO job source ----
  ## DATA WRANGLING ##
  mesm_source <- mesm_placement %>% 
    select(c(mesm_class_year,
             job_source)) %>% 
    group_by(mesm_class_year, 
             job_source) %>% 
    summarize(count = n()) %>% 
    # 2 NAs 2019; 3 NAs 2021
    drop_na() %>% 
    left_join(placement_size, by = "mesm_class_year") %>% 
    mutate(percent = round((count / mesm_responses) * 100, 1))
  
  ## PLOTTING ##
  output$mesm_job_source <- plotly::renderPlotly({
    
    # ggplot
    source_gg <- ggplot(data = mesm_source,
                        aes(x = mesm_class_year,
                            y = percent,
                            fill = reorder(job_source, percent),
                            text = paste0(job_source, " (", percent, "%", ")",
                                          "\n",
                                          "Number of respondents: ", mesm_responses))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(mesm_source$mesm_class_year),
                                      max(mesm_source$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Job Sources MESM alumni are using to secure jobs",
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      scale_fill_manual(values = c("Bren School Network" = "#003660", # ucsb navy
                                   "Company website" = "#047c91", # ucsb aqua
                                   "Internet posting" = "#9cbebe", # ucsb mist
                                   "Other" = "#6d7d33", # ucsb moss
                                   "Personal/Professional Contact" = "#79a540") # bren leaf green
      )
    # plotly
    plotly::ggplotly(source_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           y = -0.1)) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
  }) # EO job source
  
  
  
  ## SO placement status ----
  ## DATA WRANGLING ##
  status <- mesm_status %>% 
    select(mesm_class_year,
           member_status) %>% 
    mutate(status = case_when(
      member_status %in% c("FT Career",
                           "FT Temporary Career",
                           "PT Temporary Career",
                           "FT Career-Sponsored",
                           "PT Career",
                           "FT Career-Sponsored") ~ "Career",
      member_status %in% c("Time Off",
                           "Searching") ~ "Searching or Time Off",
      member_status %in% c("FT New Business",
                           "FT Eco-E") ~ "Eco-Entrepreneurship/New Business",
      member_status %in% c("Internship/Fellowship",
                           "Continuing Internship",
                           "Short-term/Project") ~ "Internship, Fellowship, or Short-term Project",
      TRUE ~ member_status
    )) %>% 
    group_by(mesm_class_year, status) %>% 
    summarize(count = n()) %>% 
    left_join(status_size, by = "mesm_class_year") %>% 
    mutate(percent = round((count / mesm_responses) * 100, 1))
  
  ## PLOTTING ##
  # ggplot
  output$mesm_placement_status <- plotly::renderPlotly({
    status_gg <- ggplot(data = status,
                        aes(x = mesm_class_year,
                            y = percent,
                            fill = reorder(status, percent),
                            text = paste0(status, " (", percent, "%", ")",
                                          "\n",
                                          "Number of respondents: ", mesm_responses))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(status$mesm_class_year),
                                      max(status$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "MESM Alumni Placement Status 6 months after graduation",
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      scale_fill_manual(values = c("Advanced Degree/Another Degree" = "#003660", # ucsb navy
                                   "Career" = "#047c91", # ucsb aqua
                                   "Internship, Fellowship, or Short-term Project" = "#9cbebe", # ucsb mist
                                   "Eco-Entrepreneurship/New Business" = "#6d7d33", # ucsb moss
                                   "Searching or Time Off" = "#79a540") # bren leaf green
      )
    
    #plotly
    plotly::ggplotly(status_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           y = -0.1),
             title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
  }) # EO placement status 
  

  
  
  ## SO sector trends ----
  ## DATA WRANGLING ##
  sector <- mesm_placement %>% 
    select(c(mesm_class_year,
             employer_sector)) %>% 
    mutate(sector_type = case_when(
      employer_sector %in% c("Consulting", "Corporate") ~ "Private",
      employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
      employer_sector %in% c("Foreign Government", "Other") ~ "Other",
      TRUE ~ employer_sector
    )) %>% 
    mutate(sector_type = factor(sector_type, levels = c("Private",
                                                        "Public",
                                                        "Non-Profit",
                                                        "Other"))) %>%
    group_by(mesm_class_year, sector_type) %>% 
    summarize(count = n())
  
  sector_time <- sector %>% 
    left_join(placement_size, by = "mesm_class_year") %>% 
    mutate(percent = round((count / mesm_responses) * 100, 1))
  
  ## PLOTTING ##
  # ggplot
  output$sector_trends <- plotly::renderPlotly({
    
    sector_time_gg <- ggplot(data = sector_time,
                             aes(x = mesm_class_year,
                                 y = percent,
                                 fill = sector_type,
                                 text = paste0(sector_type, " (", percent, "%", ")", "\n",
                                               "Number of respondents: ", mesm_responses))) +
      geom_bar(stat = "identity",
               position = "dodge") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "MESM Alumni Placement by Sector",
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      scale_fill_manual(values = c("Private" = "#003660", # ucsb navy
                                   "Public" = "#047c91", # ucsb aqua
                                   "Non-Profit" = "#dcd6cc", # uscb clay
                                   "Other" = "#9cbebe") # ucsb mist
      )
    
    # plotly
    plotly::ggplotly(sector_time_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           y = -0.1,
                           x = 0.1)) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO sector time plotly

 
  
  ## SO placement sector satisfaction ----
  ## DATA WRANGLING ##
  # total number of alumni in each sector (2019-2021)
  sector_totals <- mesm_placement %>% 
    group_by(employer_sector) %>% 
    summarize(sector_count = n())
  
  sector_satisfaction <- reactive({
    mesm_placement %>% 
      select(c(employer_sector,
               placement_satisfaction)) %>%  
      group_by(employer_sector,
               placement_satisfaction) %>% 
      summarize(count = n()) %>% 
      # consulting (3), corporate (2), local gov (1)
      drop_na() %>% 
      left_join(sector_totals, by = "employer_sector") %>% 
      # calculate percent by satisfaction count / total # of alumni working in that sector
      mutate(percent = round((count / sector_count) * 100, 1)) %>% 
      mutate(placement_satisfaction = factor(placement_satisfaction, levels = c("Very Satisfied",
                                                                                "Satisfied",
                                                                                "Somewhat Satisfied",
                                                                                "Unsatisfied"),
                                             labels = c("Very Satisfied",
                                                        "Satisfied",
                                                        "Somewhat Satisfied",
                                                        "Unsatisfied"))) %>% 
      # change other to Eco-E/New Business
      mutate(employer_sector = case_when(
        employer_sector == "Other" ~ "Eco-Entrepreneurship/New Business",
        TRUE ~ employer_sector
      )) %>% 
      # reactive filter
      filter(employer_sector %in% input$sector_types)
      
  }) # EO reactive sector_satisfaction()
  
  ## PLOTTING ##
  output$sector_satisfaction <- plotly::renderPlotly({
    
    sector_satisfaction_gg <- ggplot(data = sector_satisfaction(),
                                     aes(x = placement_satisfaction,
                                         y = percent,
                                         fill = reorder(placement_satisfaction, percent),
                                         text = paste0(placement_satisfaction, " (", percent, "%", ")",
                                                       "\n",
                                                       "Number of respondents: ", sector_count)))+
      geom_bar(position = "dodge",
               stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(sector_satisfaction()$placement_satisfaction))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank(),
            legend.position = "none") +
      labs(title = paste0("MESM Placement Satisfaction in ", input$sector_types, "\n",
                          "(Over 3 Years)"),
           x = NULL,
           y = "Percent of Respondents",
           fill = NULL) +
      # color brewer 4-class PuBu
      scale_fill_manual(values = c("Very Satisfied" = "#0570b0", 
                                   "Satisfied" = "#74a9cf", 
                                   "Somewhat Satisfied" = "#bdc9e1", 
                                   "Unsatisfied" = "#f1eef6"))
    
    plotly::ggplotly(sector_satisfaction_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15))) %>% 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO placement satisfaction
  
  
  
  ## SO salary ----
  ## DATA WRANGLING ##
  salary <- reactive({
    
    if (input$compensation_year == "All Years") {
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>%
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>% 
        # 3 year Median
        mutate(Median = median(estimated_annual_compensation_us)) %>% 
        mutate(Low = min(estimated_annual_compensation_us)) %>% 
        mutate(High = max(estimated_annual_compensation_us)) %>% 
        select(-estimated_annual_compensation_us) %>% 
        pivot_longer(cols = c(Low,
                              High,
                              Median),
                     names_to = "range",
                     values_to = "values") %>% 
        mutate(mesm_responses = 196)
      
    } # EO if statement
    
    else {
      # chose 2019, 2020, 2021
      mesm_placement %>% 
        select(mesm_class_year,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>% 
        # filter for year 
        filter(mesm_class_year %in% input$compensation_year) %>% 
        # 3 year Median
        mutate(Median = median(estimated_annual_compensation_us)) %>% 
        mutate(Low = min(estimated_annual_compensation_us)) %>% 
        mutate(High = max(estimated_annual_compensation_us)) %>% 
        select(-estimated_annual_compensation_us) %>% 
        pivot_longer(cols = c(Low,
                              High,
                              Median),
                     names_to = "range",
                     values_to = "values") %>% 
        left_join(placement_size, by = "mesm_class_year")
      
    } # EO else statement
    
  }) # EO salary reactive
  
  ## PLOTTING ##
  output$compensation <- renderPlotly({
    # ggplot
    salary_gg <- ggplot(data = salary(),
                        aes(x = reorder(range, values),
                            y = values,
                            fill = range,
                            text = paste0(range, ": ", "$", round(values, 2), "\n",
                                          "Number of respondents: ", mesm_responses)
                        )) +
      geom_bar(stat = "identity",
               position = "dodge") +
      theme_minimal() +
      theme(legend.position = "none") +
      scale_y_continuous(labels = scales::dollar_format(),
                         breaks = seq(0, 100000, 25000)) +
      scale_fill_manual(
        values = c(
          "Low" = "#dcd6cc",
          "Median" = "#047c91",
          "High" = "#003660"
        )) +
      labs(title = paste0("MESM Alumni Low, Median, and High Salary Compensation", 
                          "\n", "(", input$compensation_year, ")"), 
           x = NULL,
           y = "Dollars ($)",
           fill = NULL)
    # plotly
    plotly::ggplotly(salary_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
    
  }) # EO salary plot
  
  
  
  ## SO salary specialization ----
  ## DATA WRANGLING ##
  salary_special <- reactive({
    if (input$compSpecialization_year == "All Years") {
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year,
               mesm_program_enrollment_specializations,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) %>% 
        mutate(mesm_responses = 196)
      
    } # EO if statement
    
    else {
      # chose 2019, 2020, 2021
      mesm_placement %>% 
        select(mesm_class_year,
               mesm_program_enrollment_specializations,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        # filter for year
        filter(mesm_class_year == input$compSpecialization_year) %>% 
        mutate(mesm_program_enrollment_specializations = str_split(mesm_program_enrollment_specializations, "; ")) %>% 
        unnest(mesm_program_enrollment_specializations) %>% 
        group_by(mesm_class_year,
                 mesm_program_enrollment_specializations) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values") %>% 
        mutate(range = factor(range, levels = c("High", "Median", "Low"))) %>%
        left_join(placement_size, by = "mesm_class_year")
    } # EO else statement
    
  }) # EO salary_special reactive 
  
  ## PLOTTING ##
  output$comp_specialization <- plotly::renderPlotly({
    
    salary_special_gg <- ggplot(data = salary_special(),
                                  aes(x = mesm_program_enrollment_specializations,
                                      y = values,
                                      fill = reorder(range, values),
                                      text = paste0(mesm_program_enrollment_specializations, "\n",
                                                    range, ": ", "$", values, "\n",
                                                    "Number of respondents: ", mesm_responses)
                                  )) +
        geom_bar(stat = "identity",
                 position = "dodge") +
        coord_flip() +
        theme_minimal() +
        scale_y_continuous(labels = scales::dollar_format(),
                           breaks = seq(0, 100000, 25000)) +
        scale_x_discrete(
          labels = function(x)
            str_wrap(x, width = 25)
        ) +
        scale_fill_manual(
          values = c(
            "High" = "#003660", # ucsb navy
            "Median" = "#047c91", # ucsb aqua
            "Low" = "#dcd6cc" # uscb clay
          )) +
        labs(title = paste0("Salary Compensation by MESM Specialization"),
             x = NULL,
             y = "Dollars ($)",
             fill = NULL)
      
      plotly::ggplotly(salary_special_gg, tooltip = "text") %>% 
        layout(title = list(font = list(size = 16)),
               legend = list(orientation = "h",
                             y = -0.25,
                             x = 0.2)) %>%
        config(modeBarButtonsToRemove = list("pan", 
                                             "select",
                                             "lasso2d",
                                             "autoScale2d",
                                             "hoverClosestCartesian",
                                             "hoverCompareCartesian"))
      
  }) # EO salary specialization plot
  
  
  
  ## SO salary sector ----
  ## DATA WRANGLING ##
  salary_sector <- reactive({
    if (input$compSector_year == "All Years") {
      mesm_placement %>% 
        select(mesm_class_year,
               employer_sector,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # assign private, public, and other
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private",
                                                            "Public",
                                                            "Non-Profit",
                                                            "Other"))) %>%
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
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values") %>%
        mutate(mesm_responses = 196)
      
    }  # EO if statement
    
    else {
      mesm_placement %>% 
        select(mesm_class_year,
               employer_sector,
               employment_type,
               compensation_frequency,
               estimated_annual_compensation_us) %>%
        # assign private, public, and other
        mutate(sector_type = case_when(
          employer_sector %in% c("Consulting", "Corporate") ~ "Private",
          employer_sector %in% c("Federal Government", "Local Government", "State Government", "Research/Education") ~ "Public",
          employer_sector %in% c("Foreign Government", "Other") ~ "Other",
          TRUE ~ employer_sector
        )) %>% 
        mutate(sector_type = factor(sector_type, levels = c("Private",
                                                            "Public",
                                                            "Non-Profit",
                                                            "Other"))) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # remove stipend compensation_frequency
        filter(compensation_frequency != "Stipend") %>%
        # filter for year
        filter(mesm_class_year == input$compSector_year) %>%
        group_by(mesm_class_year,
                 sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values") %>%
        left_join(placement_size, by = "mesm_class_year")
      
    } # EO else statement
    
  }) # EO salary_sector reactive
  
  ## PLOTTING ##
  output$comp_sector <- plotly::renderPlotly({
    comp_sector <- ggplot(data = salary_sector(),
                          aes(x = sector_type,
                              y = values,
                              fill = reorder(range, values),
                              text = paste0(sector_type, "\n",
                                            range, ": ", "$", values, "\n",
                                            "Number of respondents: ", mesm_responses)
                          )) +
      geom_bar(stat = "identity",
               position = "dodge") +
      coord_flip() +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(),
                         breaks = seq(0, 100000, 25000)) +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 25)
      ) +
      scale_fill_manual(
        values = c("High" = "#003660", # ucsb navy
                   "Median" = "#047c91", # ucsb aqua
                   "Low" = "#dcd6cc" # uscb clay
        ) 
      ) +
      labs(title = paste0("MESM Alumni Salary Compensation by Sector"),
           x = NULL,
           y = "Dollars ($)",
           fill = NULL)
    
    plotly::ggplotly(comp_sector, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)),
             legend = list(orientation = "h",
                           y = -0.25,
                           x = 0.2)
             ) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
    
  }) # EO compensation sector plotly
  
  
  
  
  # DEMOGRAPHICS DB ----
  ## SO program sizes valueBox ----
  # program size df
  program_size_curr_year <- program_size %>% filter(ay_year == curr_year)
  
  meds_size <- program_size_curr_year %>% filter(objective1 == "MEDS")
  mesm_size <- program_size_curr_year %>% filter(objective1 == "MESM")
  phd_size <- program_size_curr_year %>% filter(objective1 == "PhD")
  
  # MEDS curr size valueBox output
  output$meds_curr_size <- renderValueBox({
    shinydashboard::valueBox(
      "MEDS students in 2022 cohort",
      value = meds_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "light-blue"
    )
  }) # EO MEDS valueBox curr size
  
  # MESM curr size valueBox output
  output$mesm_curr_size <- renderValueBox({
    valueBox(
      "MESM students in 2022 cohort",
      value = mesm_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "blue"
    )
    
  })# EO MESM valueBox curr size
  
  # PhD curr size valueBox output
  output$phd_curr_size <- renderValueBox({
    valueBox(
      "PhD students in 2022 cohort",
      value = phd_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "green"
    )
  }) # EO PhD valueBox curr size
  
  
  
  ## SO 2017-curr_year admit stats ----
  ## DATA WRANGLING ##
  # reactive stacked df 2017 - curr_year
  admissions_stacked_all <- reactive({
    admissions %>% 
      select(c(ay_year,
               objective1,
               Enrolled,
               Applied,
               Admitted)) %>% 
      filter(objective1 == input$admit_stats_all) %>% 
      pivot_longer(cols = c(Enrolled,
                            Applied,
                            Admitted),
                   names_to = "admin_tots",
                   values_to = "counts") %>% 
      mutate(admin_tots = factor(admin_tots, levels = c("Applied",
                                                        "Admitted",
                                                        "Enrolled")))
    
  }) # EO reactive stacked df 2017 - curr_year
  
  # avg acceptance rate
  admissions_rate <- reactive({
    admissions %>% 
      group_by(objective1) %>%
      summarize(mean = round(mean(admit_rate_pct), 1)) %>% 
      filter(objective1 == input$admit_stats_all)
  }) 
  
  output$admit_stats_all <- renderPlotly({
    
    ## PLOTTING ##
    # 2017 - curr_year admissions stacked
    admissions_all_plot <- ggplot(data = admissions_stacked_all(),
                                  aes(x = ay_year,
                                      y = counts,
                                      fill = reorder(admin_tots, counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Applied"),
               stat = "identity", 
               aes(text = paste0("Applied: ", counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Admitted"),
               stat = "identity",
               width = 0.75,
               aes(text = paste0("Admitted: ", counts))) +
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Enrolled"),
               stat = "identity",
               width = 0.6,
               aes(text = paste0("Enrolled: ", counts))) +
      scale_x_continuous(breaks = seq(min(admissions_stacked_all()$ay_year),
                                      max(admissions_stacked_all()$ay_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_fill_manual(
        values = c(
          "Applied" = "#dcd6cc",
          "Admitted" = "#9cbebe",
          "Enrolled" = "#003660"
        )
      ) +
      labs(title = paste0(input$admit_stats_all, " Admissions", "\n",
                          "<i>",
                          "<sup>",
                          "Average acceptance rate: ", admissions_rate()$mean, "%",
                          "</i>",
                          "</sup>"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    # plotly 2017 - curr_year admissions 
    plotly::ggplotly(admissions_all_plot, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO 2017-curr_year admit stats
  
  
  
  ## SO overall diversity demographics ----
  # reactive diversity df 2017 - curr_year
  diversity_overall <- reactive({
    diversity_stats %>%  
      filter(objective1 == input$diversity_stats_all)
    
  }) # EO reactive diversity df 2017 - curr_year
  
  ## PLOTTING ##
  output$overall_diversity <- renderPlotly({
    overall_demo <- ggplot(data = diversity_overall(),
                      aes(x = demographic,
                          y = percent,
                          fill = demographic,
                          text = paste0(demographic, " (", percent, "%", ")", "\n",
                                        "Sample size: ", size)
                          )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(diversity_overall()$demographic))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(
        values = c(
          "California Resident" = "#047c91",
          "Nonresident" = "#047c91",
          "International" = "#047c91",
          "Female" = "#9cbebe",
          "Age 25+" = "#9cbebe",
          "Military" = "#9cbebe",
          "First generation" = "#9cbebe",
          "Undocumented" = "#9cbebe",
          "URM" = "#09847a",
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
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      labs(title = paste0(input$diversity_stats_all, " Overall Diversity Demographics"),
           x = NULL,
           y = NULL)
    
    # plotly 2017 - curr_year
    plotly::ggplotly(overall_demo, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO renderPlotly 2017 - curr_year overall diversity demo
  
  ## SO sex ----
  ## DATA WRANGLING ##
  sex_program_time <- enrolled %>% 
    select(c("ay_year",
             "application_id",
             "gender",
             "objective1")) %>% 
    group_by(ay_year,
             objective1,
             gender) %>% 
    summarize(count = n())
  
  sex_stats_time <- left_join(sex_program_time,
                              program_size,
                              by = c("ay_year", 
                                     "objective1")) %>% 
    mutate(percent = round((count / size) * 100)) %>% 
    mutate(gender = factor(gender, levels = c("F", "M", "U"),
                           labels = c("Female", "Male", "Undeclared")))
  
  ## PLOTTING ##
  output$sex_all <- renderPlotly({
    
    # group bar chart over time 
    sex_all <- ggplot(data = sex_stats_time,
                         aes(x = ay_year,
                             y = percent,
                             fill = reorder(gender, percent),
                             text = paste0(gender, " (", percent, "%", ")", "\n",
                                           "Sample size: ", size))
                      ) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(sex_stats_time$ay_year),
                                      max(sex_stats_time$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Sex diversity and distribution trends by degree program",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("Female" = "#9cbebe",
                                   "Male" = "#003660",
                                   "Undeclared" = "#dcd6cc")) +
      facet_wrap(~objective1, ncol = 1)
    
    plotly::ggplotly(sex_all, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
    
  }) # EO sex renderPlotly
  
  
  ## SO age ----
  output$age_all <- renderPlotly({
    # empty vars
    color <- NULL
    year_str <- NULL
    
    if (input$age_prog == "MESM") {
      color <- mesm_color
      year_str <- "2017-2022"
    } # EO if MESM age plot

    else if (input$age_prog == "MEDS") {
      color <- meds_color
      year_str <- "2021-2022"
    } # EO else if MEDS age plot

    else if (input$age_prog == "PhD") {
      color <- phd_color
      year_str <- "2017-2022"
    } # EO else if PhD age plot
    
    # age plot function
    age_plot(
      df = enrolled,
      color = color,
      year_str = year_str,
      prog_input = input$age_prog
    )
  }) # EO age plot

  
  
  ## SO residency ----
  ## DATA WRANGLING ##
  
  # 2017-curr_year
  residency_stats <- enrolled %>% 
    select(c("ay_year",
             "application_id",
             "objective1",
             "citizenship_country",
             "residency_country",
             "birth_country",
             "california_resident",
             "ca_high_school",
             "visa")) %>% 
    mutate(california_resident = unlist(california_resident)) %>% 
    # residency status
    mutate(residency = case_when(
      # ca resident
      california_resident == TRUE & visa %in% c(NA,
                                                "DACA/AB540",
                                                "None: DACA Recipient",
                                                "Permanent Resident",
                                                "Undocumented Status") ~ "ca resident",
      # non ca resident
      california_resident == FALSE & visa %in% c(NA,
                                                 "Permanent Resident",
                                                 "Undocumented Status") ~ "non resident",
      # international
      visa %in% c("F-1 Student",
                  "J-1",
                  "Family of H,H1,H2,H3") ~ "international"
    )) %>% 
    group_by(ay_year,
             objective1,
             residency) %>% 
    summarize(count = n()) %>%
    left_join(program_size,
              by = c("ay_year",
                     "objective1")) %>% 
    mutate(percent = round((count / size) * 100)) %>% 
    mutate(residency = factor(residency, levels = c("ca resident",
                                                    "non resident",
                                                    "international"),
                              labels = c("CA Resident",
                                         "Nonresident",
                                         "International"))) 
  # EO residency data wrangling
  
  ## PLOTTING ##
  output$residency_all <- renderPlotly({
    residency_all <- ggplot(data = residency_stats,
                            aes(x = ay_year,
                                y = percent, 
                                fill = reorder(residency, percent),
                                text = paste0(residency, " (", percent, "%", ")", "\n",
                                              "Sample size: ", size)
                                )) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(residency_stats$ay_year),
                                      max(residency_stats$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Residency distribution trends by degree program",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("CA Resident" = "#9cbebe",
                                   "Nonresident" = "#003660",
                                   "International" = "#dcd6cc"),) +
      facet_wrap(~objective1, ncol = 1)
    
    plotly::ggplotly(residency_all, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
  }) # EO residency plotly
  
  
  
  
  ## SO international table ----
  ## DATA WRANGLING ##
  ug_intl <- enrolled %>% 
    select(ug1_location,
           ug1_name) %>%
    filter(str_detect(ug1_location, "US - ") == FALSE) %>% 
    mutate(ug1_location = case_when(
      ug1_location %in% c("Korea, Republic Of (South)", "Korea-Republic Of (South)") ~ "South Korea",
      ug1_location %in% c("China, Peoples Republic", "China, P.R.") ~ "China",
      ug1_location == "District Of Columbia" ~ "District of Columbia",
      TRUE ~ ug1_location
    )) %>% 
    mutate(ug1_name = case_when(
      ug1_name == "BANGALORE UNIV" ~ "Bangalore University",
      ug1_name == "BEIJING FORESTRY UNIVERSITY" ~ "Beijing Forestry University",
      ug1_name  == "BEIJING U TECH" ~ "Beijing University of Technology",
      ug1_name == "BEIJING UNIV - BUSINESS & TECH" ~ "Beijing Technology and Business University",
      ug1_name == "BOGAZICI UNIV" ~ "Boğaziçi University",
      ug1_name == "ECOLE DE HAUTES ETUDES COMMRCL" ~ "École des Hautes Études Commerciales (HEC Montréal)",
      ug1_name == "ESPIRITU SANTO UNIVERSITY" ~ "Universidad de Especialidades Espíritu Santo",
      ug1_name == "INDIAN INST OF TECH (IIT) - KHARAGP" ~ "Indian Institute of Technology Kharagpur",
      ug1_name == "INDIAN INSTITUTE OF TECHNOLOGY" ~ "Indian Institute of Technology",
      ug1_name == "INDIAN SCH MINES" ~ "Indian Institute of Technology (Indian School of Mines)",
      ug1_name == "LAHORE UNIV MGMT SCIENCES" ~ "Lahore University of Management Sciences",
      ug1_name == "MCGILL UNIVERSITY" ~ "McGill University",
      ug1_name == "NANJING U INFO SCI & TECH" ~ "Nanjing University of Information Science and Technology",
      ug1_name == "NANKAI UNIVERSITY" ~ "Nankai University",
      ug1_name == "PONTIFICIA UNIV CATOLICA DEL ECUADOR" ~ "Pontifical Catholic University of Ecuador",
      ug1_name == "PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE" ~ "Pontifical Catholic University of Chile",
      ug1_name == "QUEENS UNIVERSITY" ~ "Queen's University",
      ug1_name == "RAJASTHAN TECH UNIV" ~ "Rajasthan Technical University",
      ug1_name == "SARDAR VALLABH NAT'L INST OF TECH" ~ "National Institute of Technology Surat",
      ug1_name == "SEOUL NATIONAL UNIVERSITY" ~ "Seoul National University",
      ug1_name == "TECNOLGICO DE MONTERREY" ~ "Tecnológico de Monterrey",
      ug1_name == "TEL AVIV UNIVERSITY" ~ "Tel Aviv University",
      ug1_name %in% c("TSINGHUA UNIV", "TSINGHUA UNIVERSITY") ~ "Tsinghua University",
      ug1_name == "UNIV AUTONOMA DE BAJA CALIFORNIA" ~ "Autonomous University of Baja California",
      ug1_name == "UNIV DE SAO PAULO" ~ "University of São Paulo",
      ug1_name == "University of Sao Paulo - Sao Carlos" ~ "University of São Paulo at São Carlos School of Engineering",
      ug1_name == "UNIV DEL PACIFICO" ~ "Universidad del Pacífico",
      ug1_name == "UNIV DEL SALVADOR" ~ "Universidad de El Salvador",
      ug1_name == "UNIV LAS PALMAS DE GRAN CANARIA" ~ "Universidad de Las Palmas de Gran Canaria",
      ug1_name == "UNIV NACIONAL AUTONOMA DE HONDURAS" ~ "Universidad Nacional Autónoma de Honduras",
      ug1_name == "UNIV NACL DE LA PLATA" ~ "Universidad Nacional de La Plata",
      ug1_name == "UNIV OF NEW SOUTH WALES" ~ "University of New South Wales",
      ug1_name == "UNIV OF PANAMA" ~ "University of Panama",
      ug1_name == "UNIV OF SINGAPORE" ~ "National University of Singapore",
      ug1_name == "UNIV OF TORONTO" ~ "University Of Toronto",
      ug1_name == "UNIVE FEDERAL DO MARANHAO" ~ "Federal University of Maranhão",
      ug1_name == "universidad autónoma de nayarit" ~ "Universidad Autonoma de Nayarit",
      ug1_name == "UNIVERSITY OF BELIZE" ~ "University of Belize",
      ug1_name == "UNIVERSITY OF HONG KONG" ~ "University of Hong Kong",
      ug1_name == "WUHAN UNIVERSITY" ~ "Wuhan University",
      ug1_name == "UNIV FEDERAL DE SANTA MARIA" ~ "Federal University of Santa Maria",
      ug1_name == "AMERICAN UNIVERSITY OF PARIS" ~ "American University of Paris",
      ug1_name == "ITM UNIV- GURGAON" ~ "NorthCap University",
      ug1_name == "U MUMBAI" ~ "University of Mumbai",
      ug1_name == "VISVESVARAYA NATL INST TECHLGY" ~ "Visvesvaraya National Institute of Technology",
      ug1_name == "UNIV IBEROAMERICANA" ~ "Universidad Iberoamericana Ciudad de México",
      ug1_name == "UNIV PHILIPPINES" ~ "University of the Philippines",
      ug1_name == "UNIVERSITY OF SAINT ANDREWS" ~ "University of St Andrews",
      ug1_name == "MAKERERE UNIV" ~ "Makerere University",
      TRUE ~ ug1_name
    )) %>% 
    group_by(ug1_location,
             ug1_name) %>% 
    summarize(count = n())
  
  ## TABLE ##
  output$intl_unis <- DT::renderDataTable({
    DT::datatable(
      ug_intl,
      caption = "International Universities Bren Alumni Attended",
      colnames = c("Country", "University", "# of alumni"),
      class = "cell-border stripe",
      rownames = FALSE,
      options = list(
        pageLength = 12,
        dom = 'Bftipr'
      ) # EO options
      
    ) # EO datatable
  }) # EO renderDataTable
  
  
  
  ## SO origins map ----
  output$origins_map <- tmap::renderTmap({
    tmap_mode("view")
    
    tm_shape(ug_geoms) +
      tm_fill(
        col = "total",
        title = "Number of students",
        palette = "YlGn",
        style = "jenks",
        n = 6,
        popup.vars = c("Total students: " = "total")
      ) +
      tm_view(set.view = c(-10, 32, 1)) # long, lat, zoom
  }) # EO origins map using tmap
  
  
  
  
  ## OBSERVE EVENTS ----
  observeEvent(input$race_tabsetPanel, {

    if (input$race_tabsetPanel == "Race / Category (IPEDS)") {
      shinyjs::show(id = "background_box")
      
      shinydashboardPlus::updateBox(id = "race_box",
                                    action = "update",
                                    options = list(width = 6))
    } # EO if statement

    else {
      shinyjs::hide(id = "background_box")

      shinydashboardPlus::updateBox(id = "race_box",
                                    action = "update",
                                    options = list(width = 12))
    } # EO else statement
  }) # EO OE
  
  
  ## SO race / category ----
  
  ## DATA WRANGLING ##
  # 2017-curr_year
  # reactive
  category_ipeds_stats <- reactive({
    if (input$race == "All Programs") {
      ipeds %>% 
        group_by(category_ipeds) %>% 
        summarize(count = n()) %>% 
        # total number of enrolled students in the past 5 years
        # MEDS(57) + MESM(508) + PHD(62)
        mutate(size = 627) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      ipeds %>% 
        group_by(objective1,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$race)
      
    } # EO else statement
  }) # EO reactive category_ipeds_stats df

  ## PLOTTING ##
  output$race_pltly <- plotly::renderPlotly({
    
    # NOTE(HD): could not plotly_click and source to work from a script / function
    # NOTE(CONT): so had to pull race plot function out of script
    # # race plot function
    # race_plot(
    #   df = ipeds,
    #   prog_input = input$race
    # ) # EO race plot function
    
    
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
                       "(", input$race, ")"),
        x = NULL,
        y = NULL
      )
    
    # plotly
    plotly::ggplotly(ipeds_gg, 
                     source = "race_plot",
                     tooltip = "text") %>% 
      config(
        modeBarButtonsToRemove = list(
          "pan",
          "select",
          "lasso2d",
          "autoScale2d",
          "hoverClosestCartesian",
          "hoverCompareCartesian"
        )
      ) %>% 
      # had to add event_register to register source "race_plot"
      event_register("plotly_click")
  }) # EO race plotly

  
  
  ## SO background distribution ----
  output$background_pltly <- renderPlotly({

    # reactive element based on plotly click in race plot
    background_click <- reactive({
      event_data("plotly_click", source = "race_plot")
    }) # EO background_click reactive

    ## KEY-VALUE PAIRS FOR `background_click` REACTIVE ELEMENT AND RACE VALUE ##
    # "American Indian or Alaska Native", = 8
    # "Asian", = 7
    # "Black or African American", = 6
    # "Native Hawaiian or Other Pacific Islander", = 5
    # "White", = 4
    # "Hispanic or Latino", = 3
    # "Two or more races", = 2
    # "Unknown race and ethnicity" = 1

    print(paste0("Y INFO: ", background_click()$y))

    # validate plotly click
    validate(need(!is.null(background_click()), "Click a bar in the IPEDS Categories and Distribution plot to see the background distribution of the selected racial category. If nothing appears after clicking, try refreshing the page."))

    # empty vars
    race_num <- NULL
    race_str <- NULL
    color <- NULL

    if (background_click()$y == 8) {
      race_num <- 8
      race_str <- "American Indian or Alaska Native"
      color <- "#003660" # ucsb navy
    } # EO if American Indian or Alaska Native

    else if (background_click()$y == 7) {
      race_num <- 7
      race_str <- "Asian"
      color <- "#047c91" # ucsb aqua
    } # EO else if Asian

    else if (background_click()$y == 6) {
      race_num <- 6
      race_str <- "Black or African American"
      color <- "#dcd6cc" # ucsb clay
    } # EO else if Black or African American

    else if (background_click()$y == 5) {
      race_num <- 5
      race_str <- "Native Hawaiian or Other Pacific Islander"
      color <- "#9cbebe" # ucsb mist
    } # EO else if Hispanic or Latino

    else if (background_click()$y == 4) {
      race_num <- 4
      race_str <- "White"
      color <- "#dce1e5" # ucsb light grey

    } # EO else if Native Hawaiian or Other Pacific Islander

    else if (background_click()$y == 3) {
      race_num <- 3
      race_str <- "Hispanic or Latino"
      color <- "#6d7d33" # ucsb moss
    } # EO else if White

    else if (background_click()$y == 2) {
      race_num <- 2
      race_str <- "Two or more races"
      color <- "#79a540" # bren leaf green
    } # EO else if Two or more races

    else if (background_click()$y == 1) {
      race_num <- 1
      race_str <- "Unknown race and ethnicity"
      color <- "#09847a" # ucsb sea green
    } # EO else if Unknown race and ethnicity


    # background distribution function
    background_distribution(
      prog_input = input$race,
      race_num = race_num,
      df = ipeds,
      race_str = race_str,
      color = color
    ) # EO background_distribution()

  }) # background_pltly renderPlotly
  
  
  ## SO race / category trends ----
  ## DATA WRANGLING ##
  category_ipeds_stats_time <- reactive({
    if (input$race_trends == "All Programs") {
      ipeds %>% 
        group_by(ay_year,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(total_students_yr, by = "ay_year") %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      ipeds %>% 
        group_by(ay_year,
                 objective1,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        left_join(program_size, by = c("ay_year", "objective1")) %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$race_trends)

    } # EO else statement
  }) # EO category_ipeds_stats_time reactive df
  
  ## PLOTTING ##
  output$race_trends_pltly <- plotly::renderPlotly({
    race_trends_gg <- ggplot(data = category_ipeds_stats_time(),
                             aes(x = ay_year,
                                 y = percent,
                                 fill = reorder(category_ipeds, percent),
                                 text = paste0(category_ipeds, " (", percent, "%", ")", "\n",
                                               "Sample size: ", size
                                 ))) +
      geom_bar(stat = "identity",
               position = "dodge") +
      scale_x_continuous(breaks = seq(max(category_ipeds_stats_time()$ay_year),
                                      min(category_ipeds_stats_time()$ay_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(values = c(
        "American Indian or Alaska Native" = "#003660", # ucsb navy
        "Asian" = "#047c91", # ucsb aqua
        "Black or African American" = "#dcd6cc", # ucsb clay
        "Hispanic or Latino" = "#6d7d33", # ucsb moss
        "Native Hawaiian or Other Pacific Islander" = "#9cbebe", # ucsb mist
        "White" = "#dce1e5", # ucsb light grey
        "Two or more races" = "#79a540", # bren leaf green
        "Unknown race and ethnicity" = "#09847a" # ucsb sea green
      )) +
      labs(title = paste0("IPEDS Race / Category Trends", " (", input$race_trends, ")"),
           y = NULL,
           x = NULL,
           fill = NULL)
    
    plotly::ggplotly(race_trends_gg, tooltip = "text") %>% 
      layout(legend = list(orientation = "h",
                           y = -0.1)) %>% 
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
    
  }) # EO render plotly 
  
  
  
  ## SO URM trends ----
  
  ## PLOTTING ##
  output$urm_trends_pltly <- plotly::renderPlotly({
    # empty vars
    color <- NULL
    
    if (input$urm_trends == "All Programs") {
      color <- all_programs_color
    } # EO if All Programs urm trend plot
    
    else if (input$urm_trends == "MESM") {
      color <- mesm_color
    } # EO else if MESM urm trend plot
    
    else if (input$urm_trends == "MEDS") {
      color <- meds_color
    } # EO else if MEDS urm trend plot
    
    else if (input$urm_trends == "PhD") {
      color <- phd_color
    } # EO else if PhD urm trend plot
    
    # urm trends plot function
    urm_trends_plot(
      df = enrolled,
      color = color,
      prog_input = input$urm_trends
    )
    
  }) # EO urm trends plotly
  
} # EO server