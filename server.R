# server instructions
server <- function(input, output, session){

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
      "of graduates learned of their jobs through the Bren School Network",
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
    filter(placement_satisfaction == "Very Satisfied")
  
  ## VALUEBOX ##
  output$mesm_satisfied_stat <- renderValueBox({
    
    valueBox("of graduates ranked being “very satisfied” with their initial job placement",
             value = paste0(mesm_satisfaction_stat$percent, "%"),
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
        pageLength = 10,
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
                                            "Number of respondents: ", mesm_responses, "\n",
                                            "Class size: ", program_size)
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
    mutate(percent = round((count / program_size) * 100, 1))
  
  ## PLOTTING ##
  output$mesm_job_source <- plotly::renderPlotly({
    
    # ggplot
    source_gg <- ggplot(data = mesm_source,
                        aes(x = mesm_class_year,
                            y = percent,
                            fill = reorder(job_source, percent),
                            text = paste0(job_source, " (", percent, "%", ")",
                                          "\n",
                                          "Number of respondents: ", mesm_responses, "\n",
                                          "Class size: ", program_size))) +
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
                                          "Number of respondents: ", mesm_responses, "\n",
                                          "Class size: ", program_size))) +
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
                                               "Number of respondents: ", mesm_responses, "\n",
                                               "Class size: ", program_size))) +
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
        employer_sector == "Other" ~ "Eco-E/New Business",
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
      labs(title = paste0("MESM Placement Satisfaction in ", input$sector_types),
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
  
  
  
  ## SO compensation ----
  ## DATA WRANGLING ##
  salary <- reactive({
    
    if (input$compensation_year == "All Years") {
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year,
               employment_type,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>%
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
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
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
        mutate(mesm_responses = 196)
      
    } # EO else statement
    
  })
  
  ## PLOTTING ##
  output$compensation <- renderPlotly({
    # ggplot
    salary_gg <- ggplot(data = salary(),
                        aes(x = reorder(range, values),
                            y = values,
                            text = paste0(range, ": ", "$", round(values, 2), "\n",
                                          "Number of respondents: ", mesm_responses)
                        )) +
      geom_bar(stat = "identity",
               position = "dodge",
               fill = "#09847a") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format(),
                         breaks = seq(0, 100000, 25000)) +
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
    
  }) # EO compensation plot
  
  
  
  ## SO compensation specialization ----
  ## DATA WRANGLING ##
  salary_special <- reactive({
    if (input$compSpecialization_year == "All Years") {
      # chose All Years
      mesm_placement %>% 
        select(mesm_class_year,
               mesm_program_enrollment_specializations,
               employment_type,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
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
        mutate(range = factor(range, levels = c("High", "Median", "Low")))
      
    } # EO if statement
    
    else {
      # chose 2019, 2020, 2021
      mesm_placement %>% 
        select(mesm_class_year,
               mesm_program_enrollment_specializations,
               employment_type,
               estimated_annual_compensation_us) %>%
        # did not include Internship, Part-Time Job, Self-Employed/Freelance (e.g. Eco-E)
        # (41 obs removed)
        # only 1 NA
        filter(employment_type == "Full-Time Job") %>% 
        # remove $0 compensation (5 tot)
        filter(estimated_annual_compensation_us != 0) %>% 
        # filter for year
        filter(mesm_class_year == input$compSpecialization_year) %>% 
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
        mutate(range = factor(range, levels = c("High", "Median", "Low")))
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
                                              "Number of respondents: ", 196)
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
  
  
  ## SO compensation sector ----
  ## DATA WRANGLING ##
  salary_sector <- reactive({
    if (input$compSector_year == "All Years") {
      mesm_placement %>% 
        select(mesm_class_year,
               employer_sector,
               employment_type,
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
        group_by(sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values")
      
    }  # EO if statement
    
    else {
      mesm_placement %>% 
        select(mesm_class_year,
               employer_sector,
               employment_type,
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
        # filter for year
        filter(mesm_class_year == input$compSector_year) %>% 
        group_by(sector_type) %>% 
        summarize(Median = median(estimated_annual_compensation_us),
                  Low = min(estimated_annual_compensation_us),
                  High = max(estimated_annual_compensation_us)) %>% 
        pivot_longer(cols = c("Median",
                              "Low",
                              "High"),
                     names_to = "range",
                     values_to = "values")
      
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
                                            "Number of respondents: ", 196)
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
      layout(title = list(font = list(size = 16))) %>%
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
  program_size_21 <- program_size %>% filter(ay_year == 2021)
  
  meds_size <- program_size_21 %>% filter(objective1 == "MEDS")
  mesm_size <- program_size_21 %>% filter(objective1 == "MESM")
  PhD_size <- program_size_21 %>% filter(objective1 == "PhD")
  
  # MEDS valueBox output
  output$meds_curr_size <- renderValueBox({
    shinydashboard::valueBox(
      "MEDS students in 2021 cohort",
      value = meds_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "light-blue"
    )
  }) # EO MEDS valueBox prog size
  
  # MESM valueBox output
  output$mesm_curr_size <- renderValueBox({
    valueBox(
      "MESM students in 2021 cohort",
      value = mesm_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "blue"
    )
    
  })# EO MESM valueBox prog size
  
  # PhD valueBox output
  output$PhD_curr_size <- renderValueBox({
    valueBox(
      "PhD students in 2021 cohort",
      value = PhD_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "green"
    )
  }) # EO PhD valueBox prog size
  
  
  
  ## SO 2016-2021 admit stats ----
  ## DATA WRANGLING ##
  # reactive stacked df 2016 - 2021
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
    
  }) # EO reactive stacked df 2016 - 2021
  
  # avg acceptance rate
  admissions_rate <- reactive({
    admissions %>% 
      group_by(objective1) %>%
      summarize(mean = round(mean(admit_rate_pct), 1)) %>% 
      filter(objective1 == input$admit_stats_all)
  }) 
  
  output$admit_stats_all <- renderPlotly({
    
    ## PLOTTING ##
    # 2016- 2021 admissions stacked
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
    
    # plotly 2016 - 2021 admissions 
    plotly::ggplotly(admissions_all_plot, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO 2016-2021 admit stats
  
  
  
  ## SO overall diversity demographics ----
  # reactive diversity df 2021
  diversity_overall <- reactive({
    diversity_stats %>%  
      filter(objective1 == input$diversity_stats_all)
  }) # EO reactive diversity df 2021
  
  ## PLOTTING ##
  output$overall_diversity <- renderPlotly({
    overall_demo <- ggplot(data = diversity_overall(),
                      aes(x = demographic,
                          y = percent,
                          fill = demographic,
                          text = paste0(demographic, " (", percent, "%", ")", "\n",
                                        "Number of students: ", count, "\n",
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
    
    # plotly 2021
    plotly::ggplotly(overall_demo, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO 2021 diversity demo
  
  ## SO gender ----
  ## DATA WRANGLING ##
  gender_program_time <- enrolled %>% 
    select(c("ay_year",
             "application_id",
             "gender",
             "objective1")) %>% 
    group_by(ay_year,
             objective1,
             gender) %>% 
    summarize(gender_count = n())
  
  gender_cohort_tot_all <- enrolled %>% 
    select(c(
      "ay_year",
      "application_id",
      "gender",
      "objective1")) %>%
    group_by(objective1,
             ay_year) %>%
    summarize(cohort_tot = n())
  
  gender_stats_time <- left_join(gender_program_time, 
                                 gender_cohort_tot_all, 
                                 by = c("ay_year",
                                        "objective1")) %>% 
    mutate(gender_percent = round((gender_count / cohort_tot) * 100)) %>% 
    mutate(gender = factor(gender, levels = c("F", "M", "U"),
                           labels = c("Female", "Male", "Undeclared")))
  
  ## PLOTTING ##
  output$gender_all <- renderPlotly({
    
    # group bar chart over time 
    # think about adding text on MEDS plot to emphasize inaugural year? Not lack of data?
    gender_all <- ggplot(data = gender_stats_time,
                         aes(x = ay_year,
                             y = gender_percent,
                             fill = reorder(gender, gender_percent),
                             text = paste0(gender, " (", gender_percent, "%", ")", "\n",
                                           "Sample size: ", cohort_tot))
                         ) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(gender_stats_time$ay_year),
                                      max(gender_stats_time$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Gender diversity and distribution trends by degree program",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("Female" = "#9cbebe",
                                   "Male" = "#003660",
                                   "Undeclared" = "#dcd6cc")) +
      facet_wrap(~objective1, ncol = 1)
    
    plotly::ggplotly(gender_all, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
    
  }) # EO gender plotly
  
  
  ## SO age ----
  # render plotly
  output$age_all <- renderPlotly({
    # empty vars
    color <- NULL
    year_str <- NULL
    
    if (input$age_prog == "MESM") {
      color <- mesm_color
      year_str <- "2016-2021"
    } # EO if MESM age plot

    else if (input$age_prog == "MEDS") {
      color <- meds_color
      year_str <- "2021"
    } # EO else if MEDS age plot

    else if (input$age_prog == "PhD") {
      color <- phd_color
      year_str <- "2016-2021"
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
  
  # 2016-2021
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
                                                "Permanent Resident",
                                                "Permanent Residency Pending (Work Permit)",
                                                "Undocumented Status") ~ "ca resident",
      # non ca resident
      california_resident == FALSE & visa %in% c(NA,
                                                 "Permanent Resident",
                                                 "Permanent Residency Pending (Work Permit)",
                                                 "Undocumented Status") ~ "non resident",
      # international
      visa %in% c("F-1 Student",
                  "J-1",
                  "Family of H,H1,H2,H3") ~ "international"
    )) %>% 
    group_by(ay_year,
             objective1,
             residency) %>% 
    summarize(residency_count = n()) %>%
    left_join(program_size,
              by = c("ay_year",
                     "objective1")) %>% 
    mutate(percent = round((residency_count / size) * 100)) %>% 
    mutate(residency = factor(residency, levels = c("ca resident",
                                                    "non resident",
                                                    "international"),
                              labels = c("CA Resident",
                                         "Nonresident",
                                         "International")))
  
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
      ug1_location %in% c("Korea, Republic Of (South)", "Korea-Republic Of (South)") ~ "Republic of Korea",
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
  
  
  
  ## SO race / category ----
  ## PLOTTING ##
  
  output$race_pltly <- plotly::renderPlotly({
    # empty vars
    color <- NULL
    
    if (input$race == "All Programs") {
      color <- all_programs_color
    } # EO if all programs
    
    else if (input$race == "MESM") {
      color <- mesm_color
    } # EO else if mesm
    
    else if (input$race == "MEDS") {
      color <- meds_color
    } # EO else if meds
    
    else if (input$race == "PhD") {
      color <- phd_color
    } # EO else if PhD 
    
    # race plot function
    race_plot(
      df = enrolled,
      prog_input = input$race,
      color = color
    ) # EO race plot function
    
  }) # EO race plotly
  
  
  ## SO race / category trends ----
  ## DATA WRANGLING ##
  category_ipeds_stats_time <- reactive({
    if (input$race_trends == "All Programs") {
      enrolled %>% 
        select(ay_year,
               background,
               category,
               hispanic_latino) %>% 
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
        # assign demographic using ipeds definition
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
        group_by(ay_year,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        mutate(category_ipeds = factor(category_ipeds, levels = c(
          "American Indian or Alaska Native",
          "Asian",
          "Black or African American",
          "Hispanic or Latino",
          "Native Hawaiian or Other Pacific Islander",
          "White",
          "Two or more races",
          "Unknown race and ethnicity"
        ))) %>% 
        left_join(total_students_yr, by = "ay_year") %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      enrolled %>% 
        select(ay_year,
               objective1,
               background,
               category,
               hispanic_latino) %>% 
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
        # assign demographic using ipeds definition
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
        group_by(ay_year,
                 objective1,
                 category_ipeds) %>% 
        summarize(count = n()) %>% 
        mutate(category_ipeds = factor(category_ipeds, levels = c(
          "American Indian or Alaska Native",
          "Asian",
          "Black or African American",
          "Hispanic or Latino",
          "Native Hawaiian or Other Pacific Islander",
          "White",
          "Two or more races",
          "Unknown race and ethnicity"
        ))) %>% 
        left_join(program_size, by = c("ay_year", "objective1")) %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$race_trends)

    } # EO else statement
  }) # EO category_ipeds_stats_time reactive
  
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
      labs(title = paste0("IPEDS Categories Over Time", " (", input$race_trends, ")"),
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
  
  
  
  
  ## SO race / category trends 2 ----
  ## DATA WRANGLING #
  category_ipeds_time_2 <- reactive({
    enrolled %>% 
      select(ay_year,
             objective1,
             background,
             category,
             hispanic_latino) %>% 
      # replace NULL string with NA
      naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
      mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
      # assign demographic using ipeds definition
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
      group_by(ay_year,
               objective1,
               category_ipeds) %>% 
      summarize(count = n()) %>% 
      mutate(category_ipeds = factor(category_ipeds, levels = c(
        "American Indian or Alaska Native",
        "Asian",
        "Black or African American",
        "Hispanic or Latino",
        "Native Hawaiian or Other Pacific Islander",
        "White",
        "Two or more races",
        "Unknown race and ethnicity"
      ))) %>% 
      left_join( program_size, by = c("ay_year", "objective1")) %>%
      mutate(percent = round((count / size) * 100, 1)) %>% 
      filter(category_ipeds == input$race_trends_2)
    
  }) # EO category ipeds time 2 reactive
  
  ## PLOTTING ##
  output$race_trends_2_pltly <- plotly::renderPlotly({
    race_trends_2 <- ggplot(data = category_ipeds_time_2(),
                            aes(x = ay_year,
                                y = percent,
                                fill = objective1,
                                text = paste0(category_ipeds, " (", percent, "%", ")", "\n",
                                              "Sample size: ", size)
                            )) +
      geom_bar(stat = "identity") +
      scale_x_continuous(breaks = seq(max(category_ipeds_time_2()$ay_year),
                                      min(category_ipeds_time_2()$ay_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      scale_fill_manual(
        values = c(
          "MESM" = mesm_color,
          "MEDS" = meds_color,
          "PhD" = phd_color
        )
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      labs(
        title = paste0(input$race_trends_2, " IPEDS Category Trend"),
        x = NULL,
        y = NULL,
        fill = NULL
      ) +
      facet_wrap(~objective1, ncol = 1)
    
    
    plotly::ggplotly(race_trends_2, tooltip = "text")
    
  }) # EO race trends plotly 2
  
  
  
  ## SO URM trends ----
  ## DATA WRANGLING ##
  # urm vars
  category_urms <- c("African American / Black",
                     "American Indian / Alaska Native")
  
  visa_urms <- c("Permanent Residency Pending (Work Permit)",
                 "Permanent Resident")
  
  urm_trends <- reactive({
    if (input$urm_trends == "All Programs") {
      enrolled %>% 
        select("ay_year",
               "application_id",
               "objective1",
               "citizenship_country",
               "residency_country",
               "birth_country",
               "visa",
               "background",
               "category",
               "hispanic_latino") %>% 
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        mutate(urm_status = case_when(
          # us citizens are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>% 
        group_by(ay_year,
                 urm_status) %>% 
        summarize(count = n()) %>% 
        filter(urm_status == "Y") %>% 
        left_join(total_students_yr, by = "ay_year") %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      enrolled %>% 
        select("ay_year",
               "application_id",
               "objective1",
               "citizenship_country",
               "residency_country",
               "birth_country",
               "visa",
               "background",
               "category",
               "hispanic_latino") %>% 
        # replace NULL string with NA
        naniar::replace_with_na(replace = list(hispanic_latino = "NULL")) %>%
        mutate(hispanic_latino = unlist(hispanic_latino)) %>% 
        rowwise() %>% # Note(HD): look into this, but forces to go through every row?
        mutate(urm_status = case_when(
          # us citizens are hispanic/latino
          hispanic_latino == TRUE & citizenship_country == "US" ~ "Y",
          # permanent residents are hispanic/latino
          hispanic_latino == TRUE & visa %in% visa_urms ~ "Y",
          # us citizens identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & citizenship_country == "US" ~ "Y",
          # permanent residents identify as urms
          TRUE %in% str_detect(string = category, pattern = category_urms) == TRUE
          & visa %in% visa_urms ~ "Y",
          # everything else
          TRUE ~ "N")) %>% 
        group_by(ay_year,
                 urm_status,
                 objective1) %>% 
        summarize(count = n()) %>% 
        filter(urm_status == "Y") %>% 
        left_join(program_size, by = c("ay_year", "objective1")) %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$urm_trends)
      
    } # EO else statement
    
  }) # EO urm trends reactive 
  
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
      df = urm_trends(),
      color = color,
      prog_input = input$urm_trends
    )
    
  }) # EO urm trends plotly
  
  
  
  ## SO ethnicity / background ----
  
  ## * american indian or alaska native ethnicity ----
  ## DATA WRANGLING ##
  ## PLOTTING ##
  
  
  ## * asian ethnicity ----
  ## DATA WRANGLING ##
  # reactive
  asian_background_stats <- reactive({
    
    if (input$asian_eth == "All Programs") {
      # breakdown of asian ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Asian") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      # breakdown of asian ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Asian") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$asian_eth)
      
    } # EO else statement

  }) # EO asian ethnicity reactive
  
  
  ## PLOTTING ##
  output$asian_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = asian_background_stats(),
                       aes(x = background,
                           y = percent,
                           text = paste0(background, " (", percent, "%", ")", "\n",
                                         "Sample size: ", size
                           ))) +
      geom_bar(stat = "identity",
               fill = "#047c91") +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Ethnicities / Backgrounds of Asian Category", "\n",
                          "(", input$asian_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15))) %>% 
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
    
  }) # EO asian ethnicity
  
  
  
  ## * black ethnicity ----
  ## DATA WRANGLING ##
  
  # reactive
  black_background_stats <- reactive({
    # breakdown of black ethnicity
    if(input$black_eth == "All Programs") {
      ipeds %>% 
        filter(category_ipeds == "Black or African American") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>%
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
    # breakdown of black ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Black or African American") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>%
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$black_eth)
  
    } # EO else statement
    
  }) # EO black ethnicity reactive
  


  
  ## PLOTTING ##
  output$black_eth_pltly <- plotly::renderPlotly({
    
    # validate 
    validate(
      need(nrow(black_background_stats()) > 0,
           paste0("There are no data on Black or African American racial category for ", input$black_eth, " degree program."))
    ) # EO validate
    
    eth_gg <- ggplot(data = black_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0(background, " (", percent, "%", ")", "\n",
                                       "Sample size: ", size
                         ))) +
      geom_bar(stat = "identity",
               fill = "#dcd6cc") +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Ethnicities / Backgrounds of Black or ", "\n",
                          "African American Category (", input$black_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15)))%>% 
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
    
  }) # EO black ethnicity
  
  
  
  ## * hispanic / latino ethnicity ----
  ## DATA WRANGLING ##
  
  # reactive
  hisp_lat_background_stats <- reactive({
    if (input$hisp_lat_eth == "All Programs") {
      # breakdown of hispanic / latino ethnicity 
      ipeds %>% 
        filter(category_ipeds == "Hispanic or Latino") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
    } # EO if statement 
    
    else {
      # breakdown of hispanic / latino ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Hispanic or Latino") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$hisp_lat_eth)
    } # EO else statement

  }) # EO hispanic / latino ethnicity reactive
  
  ## PLOTTING ##
  output$hisp_lat_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = hisp_lat_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0(background, " (", percent, "%", ")", "\n",
                                       "Sample size: ", size
                         ))) +
      geom_bar(stat = "identity",
               fill = "#6d7d33") +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of Hispanic or Latino", "\n",
                          "Category (", input$hisp_lat_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15)))%>% 
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
    
  }) # EO hispanic / latino ethnicity
  
  
  
  ## * native hawaiian or other pacific islander ethnicity ----
  ## DATA WRANGLING ##
  ## PLOTTING ##
  
  
  ## * white ethnicity ----
  ## DATA WRANGLING ##
  
  # reactive
  white_background_stats <- reactive({
    if (input$white_eth == "All Programs") {
      # breakdown of white ethnicity
      ipeds %>% 
        filter(category_ipeds == "White") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
       
    } # EO if statement
    
    else {
      # breakdown of white ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "White") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$white_eth)
    } # EO else statement

  }) # EO white ethnicity reactive
  
   
  ## PLOTTING ##
  output$white_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = white_background_stats(),
                           aes(x = background,
                               y = percent,
                               text = paste0(background, " (", percent, "%", ")", "\n",
                                             "Sample Size: ", size
                               ))) +
      geom_bar(stat = "identity",
               fill = "#dce1e5") +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Ethnicity / Backgrounds of White Category", "\n",
                          "(", input$white_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15)))%>% 
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
    
  }) # EO white ethnicity
  
  
  ## * two or more races ethnicity ----
  ## DATA WRANGLING ##
  
  # reactive
  two_more_eth_background_stats <- reactive({
    # breakdown of two or more races ethnicity 
    if (input$two_more_eth == "All Programs") {
      ipeds %>% 
        filter(category_ipeds == "Two or more races") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      # breakdown of two or more races ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Two or more races") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$two_more_eth)
      
    } # EO else statement

  }) # EO two or more race ethnicity reactive 
  
  
  ## PLOTTING ##
  output$two_more_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = two_more_eth_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0(background, " (", percent, "%", ")", "\n",
                                       "Sample size: ", size
                         ))) +
      geom_bar(stat = "identity",
               fill = "#79a540") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of Two or More Races", "\n", 
                          "Category (", input$two_more_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15)))%>% 
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
    
  }) # EO two or more races ethnicity
  
  
  
  ## * unknown race or ethnicity ----
  ## DATA WRANGLING ##

  # reactive
  unk_eth_background_stats <- reactive({
    if (input$unk_eth == "All Programs") {
      # breakdown of unknown race or ethnicity
      ipeds %>% 
        filter(category_ipeds == "Unknown race and ethnicity") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(background) %>% 
        summarize(count = n()) %>% 
        mutate(size = 604) %>% 
        mutate(percent = round((count / size) * 100, 1))
      
    } # EO if statement
    
    else {
      # breakdown of unknown race or ethnicity by program
      ipeds %>% 
        filter(category_ipeds == "Unknown race and ethnicity") %>% 
        mutate(background = case_when(
          is.na(background) == TRUE ~ "Unknown race and ethnicity",
          TRUE ~ background
        )) %>%
        mutate(background = str_split(background, "; ")) %>% 
        unnest(background) %>% 
        group_by(objective1,
                 background) %>% 
        summarize(count = n()) %>% 
        left_join(tot_5yr, by = "objective1") %>% 
        mutate(percent = round((count / size) * 100, 1)) %>% 
        filter(objective1 == input$unk_eth)
      
    } # EO else statement

  }) # EO unknown race or ethnicity reactive
  
  
  ## PLOTTING ##
  output$unk_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = unk_eth_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0(background, " (", percent, "%", ")", "\n",
                                       "Sample size: ", size
                         ))) +
      geom_bar(stat = "identity",
               fill = "#09847a") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of Unknown Race and", "\n",
                          "Ethnicity Category (", input$unk_eth, ")"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 15)))%>% 
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
    
  }) # EO unknown race or ethnicity
  

  
} # EO server