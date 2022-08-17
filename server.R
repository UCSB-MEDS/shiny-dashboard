# server instructions
server <- function(input, output, session){

  # 2021 DB ----

  
  
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
    mutate(percent = round((count / 235) * 100, 1)) %>% 
    filter(placed == "Placed")
  
  # MEDS valueBox output
  output$placement_stat <- renderValueBox({
    
    shinydashboard::valueBox(
      "MESM Alumni placed in a career or similar position",
      value = paste0(status_stat$percent, "%"),
      icon = icon("home"),
      color = "green"
    ) # EO valueBox
  }) # EO MEDS valueBox prog size
  
  
  
  ## SO career placements table  ----
  ## DATA WRANGLING ##
  employer <- mesm_placement %>% 
    select(c(employer_account_name,
             employer_sector)) %>%
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
        pageLength = 9,
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
      work_location_state == "CA" ~ "California",
      work_location_state != "CA" & work_location_country == "United States" ~ "Out of State",
      work_location_country != "United States" ~ "International"
    )) %>% 
    group_by(mesm_class_year,
             location) %>%
    summarize(location_count = n())
  
  # calculating percentages
  # QUESTION: Do we want to calculate based on number of responses received 
  # OR the size of the cohort?
  placement_location_stats <- placement_location %>% 
    left_join(placement_size, by = "mesm_class_year") %>%
    mutate(percent = round((location_count / program_size) * 100, 1))
  
  ## PLOTTING ##
  output$mesm_location <- plotly::renderPlotly({
    # ggplot
    location_gg <- ggplot(data = placement_location_stats,
                          aes(x = mesm_class_year,
                              y = percent,
                              fill = reorder(location, percent),
                              text = paste0("Location: ", location, "\n",
                                            "Percent: ", percent, "%"))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(placement_location_stats$mesm_class_year),
                                      max(placement_location_stats$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1),
                         breaks = seq(0, 50, 10)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Where MESM alumni are working 6 months after graduating",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("California" = "#9cbebe",
                                   "Out of State" = "#003660",
                                   "International" = "#dcd6cc"))
    
    # plotly
    plotly::ggplotly(location_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h",
                           x = 0.1),
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
    summarize(count = n())
  
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
                            text = paste0("Job Source: ", job_source, "\n",
                                          "Percent: ", percent, "%"))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(mesm_source$mesm_class_year),
                                      max(mesm_source$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Job Sources MESM alumni are using to secure jobs",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("Bren School Network" = "#003660", # ucsb navy
                                   "Company website" = "#047c91", # ucsb aqua
                                   "Internet posting" = "#9cbebe", # ucsb mist
                                   "Other" = "#6d7d33", # ucsb moss
                                   "Personal/Professional Contact" = "#79a540") # bren leaf green
      )
    # plotly
    plotly::ggplotly(source_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h")) %>%
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
                           "FT Eco-E") ~ "New Business",
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
                            text = paste0("Placement Status: ", status, "\n",
                                          "Percent: ", percent, "%", "\n",
                                          "Sample size: ", mesm_responses, "\n",
                                          "Cohort size: ", program_size))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(status$mesm_class_year),
                                      max(status$mesm_class_year))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "MESM alumni Placement Status 6 months after graduation",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("Advanced Degree/Another Degree" = "#003660", # ucsb navy
                                   "Career" = "#047c91", # ucsb aqua
                                   "Internship, Fellowship, or Short-term Project" = "#9cbebe", # ucsb mist
                                   "New Business" = "#6d7d33", # ucsb moss
                                   "Searching or Time Off" = "#79a540") # bren leaf green
      )
    
    #plotly
    plotly::ggplotly(status_gg, tooltip = "text") %>%
      layout(legend = list(orientation = "h"),
             title = list(font = list(size = 16))) %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select",
                                           "lasso2d",
                                           "autoScale2d",
                                           "hoverClosestCartesian",
                                           "hoverCompareCartesian"))
  }) # EO placement status 
  
  
  ## SO placement sector ----
  ## DATA WRANGLING ##
  sector_simple <- reactive({
    
    validate(
      need(input$sector_tree_check != "",
           "Please select at least one year.")
    ) # EO validate
    
    mesm_placement %>% 
      select(c(mesm_class_year,
               employer_sector)) %>% 
      mutate(sector_simple = case_when(
        employer_sector %in% c("Consulting", "Corporate") ~ "Private",
        employer_sector %in% c("Federal Government", "Local Government", "State Government") ~ "Government",
        employer_sector %in% c("Non-Profit", "Research/Education") ~ "Nonprofit / Public",
        employer_sector == "Foreign Government" ~ "International",
        employer_sector == "Other" ~ "Other"
      )) %>% 
      mutate(sector_simple = factor(sector_simple, levels = c("Government",
                                                              "Private",
                                                              "Nonprofit / Public",
                                                              "International",
                                                              "Other"))) %>% 
      filter(mesm_class_year %in% input$sector_tree_check) %>% 
      group_by(employer_sector,
               sector_simple) %>% 
      summarize(count = n())
  }) # EO reactive placement sector df

  ## PLOTTING TREE MAP ##
  output$sector_tree <- d3treeR::renderD3tree2({
    # treemap 
    sector_tree <- treemap::treemap(dtf = sector_simple(),
                                    index = c("sector_simple", 
                                              "employer_sector"),
                                    vSize = "count",
                                    type = "index"
                                    )
    
    # interactive treemap
    # rootname = title of map
    d3treeR::d3tree2(sector_tree, rootname = "2021 MESM Placement Sectors")
    
    
  }) # EO placement sector
  
  
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
      filter(employer_sector %in% input$sector_types)
      
  }) # EO reactive sector_satisfaction()
  
  ## PLOTTING ##
  output$sector_satisfaction <- plotly::renderPlotly({
    
    sector_satisfaction_gg <- ggplot(data = sector_satisfaction(),
                                     aes(x = placement_satisfaction,
                                         y = percent,
                                         fill = reorder(placement_satisfaction, percent),
                                         text = paste0("Placement Satisfaction: ", placement_satisfaction,
                                                       "\n",
                                                       "Percent: ", percent, "%", "\n",
                                                       "Sample size: ", sector_count)))+
      geom_bar(position = "dodge",
               stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(sector_satisfaction()$placement_satisfaction))) +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "MESM Placement Satisfaction by Sector",
           x = NULL,
           y = NULL,
           fill = NULL) +
      scale_fill_manual(values = c("Very Satisfied" = "#003660", # ucsb navy
                                   "Satisfied" = "#047c91", # ucsb aqua
                                   "Somewhat Satisfied" = "#dcd6cc", # uscb clay
                                   "Unsatisfied" = "#9cbebe") # ucsb mist
      )
    
    plotly::ggplotly(sector_satisfaction_gg, tooltip = "text") %>% 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO placement satisfaction
  
  
  
  ## SO avg compensation ----
  ## DATA WRANGLING ##
  salary <- mesm_placement %>% 
    select(mesm_class_year,
           employment_type,
           estimated_annual_compensation_us) %>%
    # did not include Internship or Part-Time Job (~30 obs dropped)
    # only 1 NA
    filter(employment_type %in% c("Full-Time Job", "Self-Employed/Freelance (e.g. Eco-E)")) %>% 
    # remove $0 compensation (5 tot)
    filter(estimated_annual_compensation_us != 0) %>% 
    # 3 year average
    mutate(mean = mean(estimated_annual_compensation_us)) %>%
    filter(estimated_annual_compensation_us == min(estimated_annual_compensation_us) | estimated_annual_compensation_us == max(estimated_annual_compensation_us)) %>% 
    pivot_longer(cols = c(estimated_annual_compensation_us,
                          mean),
                 names_to = "range",
                 values_to = "values") %>% 
    # assign range labels 
    mutate(range = case_when(
      values == 4000.00 ~ "Low",
      values == 109000.00 ~ "High",
      employment_type == "Full-Time Job" ~ "Average",
      TRUE ~ "none"
    )) %>% 
    filter(range != "none")
  
  ## PLOTTING ##
  output$compensation <- renderPlotly({
    salary_gg <- ggplot(data = salary,
                        aes(x = reorder(range, values),
                            y = values,
                            fill = reorder(range, values),
                            text = paste0(range, ": ", "$", round(values, 2))
                        )) +
      geom_bar(stat = "identity",
               position = "dodge") +
      theme_minimal() +
      scale_y_continuous(labels = scales::dollar_format()) +
      scale_fill_manual(values = c("High" = "#9cbebe",
                                   "Average" = "#003660",
                                   "Low" = "#dcd6cc")) +
      labs(title = "MESM Alumni Low, High, and Average Salary Compensation (2019-2021)", 
           x = NULL,
           y = "Dollars ($)",
           fill = NULL)
    
    plotly::ggplotly(salary_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 13))) %>% 
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO compensation plot
  
  
  # DEMOGRAPHICS DB ----
  ## SO program sizes valueBox ----
  # program size df
  program_size_21 <- program_size %>% filter(ay_year == 2021)
  
  meds_size <- program_size_21 %>% filter(objective1 == "MEDS")
  mesm_size <- program_size_21 %>% filter(objective1 == "MESM")
  phd_size <- program_size_21 %>% filter(objective1 == "PHD")
  
  # MEDS valueBox output
  output$meds_curr_size <- renderValueBox({
    shinydashboard::valueBox(
      "MEDS students in 2021 cohort",
      value = meds_size$program_size,
      icon = icon("users", lib = "font-awesome"),
      color = "light-blue"
    )
  }) # EO MEDS valueBox prog size
  
  # MESM valueBox output
  output$mesm_curr_size <- renderValueBox({
    valueBox(
      "MESM students in 2021 cohort",
      value = mesm_size$program_size,
      icon = icon("users", lib = "font-awesome"),
      color = "blue"
    )
    
  })# EO MESM valueBox prog size
  
  # PHD valueBox output
  output$phd_curr_size <- renderValueBox({
    valueBox(
      "PhD students in 2021 cohort",
      value = phd_size$program_size,
      icon = icon("users", lib = "font-awesome"),
      color = "green"
    )
  }) # EO PHD valueBox prog size
  
  
  
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
  
  
  
  ## SO 2021 diversity demographics overall ----
  # reactive diversity df 2021
  diversity_all_21 <- reactive({
    diversity_stats %>% 
      filter(ay_year == 2021) %>% 
      filter(objective1 == input$diversity_stats_all)
  }) # EO reactive diversity df 2021
  
  ## PLOTTING ##
  output$diversity_2021 <- renderPlotly({
    demo_21 <- ggplot(data = diversity_all_21(),
                      aes(x = demographic,
                          y = percent,
                          fill = demographic,
                          text = paste0(demographic, "\n", 
                                        "Percentage: ", percent, "%", "\n",
                                        "Sample size: ", program_size)
                          )) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(diversity_all_21()$demographic))) +
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
      labs(title = paste0("2021 ", input$diversity_stats_all, " Diversity Demographics"),
           x = NULL,
           y = NULL)
    
    # plotly 2021
    plotly::ggplotly(demo_21, tooltip = "text") %>%
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
                             text = paste0("Gender: ", gender, "\n",
                                           "Percent: ", gender_percent, "%"))) +
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

    else if (input$age_prog == "PHD") {
      color <- phd_color
      year_str <- "2016-2021"
    } # EO else if PHD age plot
    
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
    mutate(percent = round((residency_count / program_size) * 100)) %>% 
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
                                text = paste0("Residency: ", residency, "\n",
                                              "Percent: ", percent, "%"))) +
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
      ) 
  }) # EO origins map using tmap
  
  
  
  ## SO race / category ----
  ## PLOTTING
  
  output$race_pltly <- plotly::renderPlotly({
    
    # race plot function
    race_plot(
      df = enrolled,
      prog_input = input$race
    ) # EO race plot function
    
  }) # EO race plotly
  
  ## SO ethnicity / background ----
  
  ## * american indian or alaska native ethnicity ----
  ## DATA WRANGLING
  # breakdown of american indian or alaska native ethnicities by program
  amIn_alNat_background <- ipeds %>% 
    filter(category_ipeds == "American Indian or Alaska Native") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  amIn_alNat_background_stats <- reactive({
    
    left_join(amIn_alNat_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$amIn_alNat_eth)
    
  }) # EO rdf
  
  
  ## PLOTTING
  output$amIn_alNat_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = amIn_alNat_background_stats(),
                       aes(x = background,
                           y = count,
                           text = paste0("Program: ", objective1, "\n",
                                         "Background: ", background, "\n",
                                         "Percent: ", percent, "%", "\n",
                                         "Sample size: ", tot
                           ))) +
      geom_bar(stat = "identity",
               fill = "#003660") +
      coord_flip() +
      scale_x_discrete(
        labels = function(x)
          str_wrap(x, width = 35)
      ) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of American Indian or Alaska Native Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
    
  }) # EO american indian or alaska native ethnicity
  
  
  
  ## * asian ethnicity ----
  ## DATA WRANGLING
  # breakdown of asian ethnicities by program
  asian_background <- ipeds %>% 
    filter(category_ipeds == "Asian") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  asian_background_stats <- reactive({
    left_join(asian_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$white_eth)
  }) 
  
  
  ## PLOTTING
  output$asian_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = asian_background_stats(),
                       aes(x = background,
                           y = percent,
                           text = paste0("Program: ", objective1, "\n",
                                         "Background: ", background, "\n",
                                         "Percent: ", percent, "%", "\n",
                                         "Sample size: ", tot
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
      labs(title = paste0("Backgrounds of Asian Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
  ## DATA WRANGLING
  # breakdown of white ethnicities by program
  black_background <- ipeds %>% 
    filter(category_ipeds == "Black or African American") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  black_background_stats <- reactive({
    left_join(black_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$black_eth)
  }) 
  
  
  ## PLOTTING
  output$black_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = black_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0("Program: ", objective1, "\n",
                                       "Background: ", background, "\n",
                                       "Percent: ", percent, "%", "\n",
                                       "Sample size: ", tot
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
      labs(title = paste0("Backgrounds of Black Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
  ## DATA WRANGLING
  # breakdown of white ethnicities by program
  hisp_lat_background <- ipeds %>% 
    filter(category_ipeds == "Hispanic or Latino") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  hisp_lat_background_stats <- reactive({
    left_join(hisp_lat_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$hisp_lat_eth)
  }) 
  
  
  ## PLOTTING
  output$hisp_lat_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = hisp_lat_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0("Program: ", objective1, "\n",
                                       "Background: ", background, "\n",
                                       "Percent: ", percent, "%", "\n",
                                       "Sample size: ", tot
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
      labs(title = paste0("Backgrounds of Hispanic or Latino Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
  ## DATA WRANGLING
  # breakdown of native hawaiian or other pacific islander by program
  natHi_pi_eth_background <- ipeds %>% 
    filter(category_ipeds == "Native Hawaiian or Other Pacific Islander") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  natHi_pi_eth_background_stats <- reactive({
    left_join(natHi_pi_eth_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$natHi_pi_eth)
  }) 
  
  
  ## PLOTTING
  output$natHi_pi_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = natHi_pi_eth_background_stats(),
                     aes(x = background,
                         y = count,
                         text = paste0("Program: ", objective1, "\n",
                                       "Background: ", background, "\n",
                                       "Percent: ", percent, "%", "\n",
                                       "Sample size: ", tot
                         ))) +
      geom_bar(stat = "identity",
               fill = "#9cbebe") +
      coord_flip() +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of Native Hawaiian or Other Pacific Islander", "\n",
                          "Ethnicity Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
    
  }) # EO native hawaiian or other pacific islander ethnicity
  
  
  
  ## * white ethnicity ----
  ## DATA WRANGLING
  # breakdown of white ethnicities by program
  white_background <- ipeds %>% 
    filter(category_ipeds == "White") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  white_background_stats <- reactive({
    left_join(white_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$white_eth)
  }) 
  
  
  ## PLOTTING
  output$white_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = white_background_stats(),
                           aes(x = background,
                               y = percent,
                               text = paste0("Program: ", objective1, "\n",
                                             "Background: ", background, "\n",
                                             "Percent: ", percent, "%", "\n",
                                             "Sample size: ", tot
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
      labs(title = paste0("Backgrounds of White Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
  ## DATA WRANGLING
  # breakdown of two or more races ethnicities by program
  two_more_eth_background <- ipeds %>% 
    filter(category_ipeds == "Two or more races") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  two_more_eth_background_stats <- reactive({
    left_join(two_more_eth_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$two_more_eth)
  }) 
  
  
  ## PLOTTING
  output$two_more_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = two_more_eth_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0("Program: ", objective1, "\n",
                                       "Background: ", background, "\n",
                                       "Percent: ", percent, "%", "\n",
                                       "Sample size: ", tot
                         ))) +
      geom_bar(stat = "identity",
               fill = "#79a540") +
      coord_flip() +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1, scale = 1)) +
      theme_minimal() +
      theme(
        legend.position = "none"
      ) +
      labs(title = paste0("Backgrounds of Two or More Races Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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
  ## DATA WRANGLING
  # breakdown of unknown race or ethnicity by program
  unk_eth_background <- ipeds %>% 
    filter(category_ipeds == "Unknown race and ethnicity") %>% 
    mutate(background = case_when(
      is.na(background) == TRUE ~ "Unknown race and ethnicity",
      TRUE ~ background
    )) %>%
    mutate(background = str_split(background, "; ")) %>% 
    unnest(background) %>% 
    group_by(objective1,
             background) %>% 
    summarize(count = n())
  
  # reactive
  unk_eth_background_stats <- reactive({
    left_join(unk_eth_background, tot_5yr, by = "objective1") %>% 
      mutate(percent = round((count / tot) * 100, 1)) %>% 
      filter(objective1 %in% input$unk_eth)
  }) 
  
  
  ## PLOTTING
  output$unk_eth_pltly <- plotly::renderPlotly({
    eth_gg <- ggplot(data = unk_eth_background_stats(),
                     aes(x = background,
                         y = percent,
                         text = paste0("Program: ", objective1, "\n",
                                       "Background: ", background, "\n",
                                       "Percent: ", percent, "%", "\n",
                                       "Sample size: ", tot
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
                          "Ethnicity Category"),
           x = NULL,
           y = NULL,
           fill = NULL)
    
    plotly::ggplotly(eth_gg, tooltip = "text") %>% 
      layout(title = list(font = list(size = 16)))%>% 
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