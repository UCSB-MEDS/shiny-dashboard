# server instructions
server <- function(input, output, session){

  # 2021 DB ----
  ## SO program sizes valueBox ----
  # program size df
  program_size <- bren_apps %>% 
    select(c(ay_year,
             objective1)) %>% 
    filter(ay_year == 2021) %>% 
    group_by(objective1) %>% 
    summarize(count = n())
  
  meds_size <- program_size %>% filter(objective1 == "MEDS")
  mesm_size <- program_size %>% filter(objective1 == "MESM")
  phd_size <- program_size %>% filter(objective1 == "PHD")
  
  # MEDS valueBox output
  output$meds_curr_size <- renderValueBox({
    shinydashboard::valueBox(
      "MEDS students in 2021 cohort",
      value = meds_size$count,
      icon = icon("users", lib = "font-awesome"),
      color = "light-blue"
    )
  })
  # MESM valueBox output
  output$mesm_curr_size <- renderValueBox({
      valueBox(
        "MESM students in 2021 cohort",
        value = mesm_size$count,
        icon = icon("users", lib = "font-awesome"),
        color = "blue"
      )
             
  })
  # PHD valueBox output
  output$phd_curr_size <- renderValueBox({
    valueBox(
      "PhD students in 2021 cohort",
      value = phd_size$count,
      icon = icon("users", lib = "font-awesome"),
      color = "green"
      )
  })
  
  ## SO 2021 admit stats  ----
  output$admit_2021 <- renderPlotly({
    ## DATA WRANGLING ##
    # stacked df 2021
    input$admit_stats_all
    
    admissions_stacked <- admissions %>% 
      select(c(ay_year,
               objective1,
               Take,
               Applied,
               Admitted)) %>% 
      filter(ay_year == 2021) %>% 
      pivot_longer(cols = c(Take,
                            Applied,
                            Admitted),
                   names_to = "admin_tots",
                   values_to = "counts") %>% 
      mutate(admin_tots = factor(admin_tots, levels = c("Applied",
                                                        "Admitted",
                                                        "Take")))
    ## PLOTTING ##
    # ggplot 
    admissions_stacked_plot <- ggplot(data = admissions_stacked,
                                      aes(x = ay_year,
                                          y = counts,
                                          fill = reorder(admin_tots, counts))) +
      geom_bar(data = admissions_stacked %>% filter(admin_tots == "Applied"),
               stat = "identity", 
               aes(text = paste0("Applied: ", counts))) +
      geom_bar(data = admissions_stacked %>% filter(admin_tots == "Admitted"),
               stat = "identity",
               width = 0.75,
               aes(text = paste0("Admitted: ", counts))) +
      geom_bar(data = admissions_stacked %>% filter(admin_tots == "Take"),
               stat = "identity",
               width = 0.6,
               aes(text = paste0("Take: ", counts))) +
      scale_x_continuous(breaks = seq(min(admissions_stacked$ay_year),
                                      max(admissions_stacked$ay_year))) +
      theme_minimal() +
      scale_fill_manual(
        values = c(
          "Applied" = "#dcd6cc",
          "Admitted" = "#9cbebe",
          "Take" = "#003660"
        )
      ) +
      labs(title = "2021 Admissions by Degree Program",
           x = NULL,
           y = NULL,
           fill = NULL) +
      facet_wrap(~objective1, nrow = 1)
    
    # plotly
    plotly::ggplotly(admissions_stacked_plot, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  }) # EO 2021 admit stats 

  
  ## SO 2019-2021 admit stats ----
  ## DATA WRANGLING ##
  # reactive stacked df 2019 - 2021
  admissions_stacked_all <- reactive({
    admissions %>% 
      select(c(ay_year,
               objective1,
               Take,
               Applied,
               Admitted)) %>% 
      filter(objective1 == input$admit_stats_all) %>% 
      pivot_longer(cols = c(Take,
                            Applied,
                            Admitted),
                   names_to = "admin_tots",
                   values_to = "counts") %>% 
      mutate(admin_tots = factor(admin_tots, levels = c("Applied",
                                                        "Admitted",
                                                        "Take")))
  }) # EO reactive stacked df 2019 - 2021
  
  output$admit_stats_all <- renderPlotly({
    
    ## PLOTTING ##
    # 2019 - 2021 admissions stacked
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
      geom_bar(data = admissions_stacked_all() %>% filter(admin_tots == "Take"),
               stat = "identity",
               width = 0.6,
               aes(text = paste0("Take: ", counts))) +
      scale_x_continuous(breaks = seq(min(admissions_stacked_all()$ay_year),
                                      max(admissions_stacked_all()$ay_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_fill_manual(
        values = c(
          "Applied" = "#dcd6cc",
          "Admitted" = "#9cbebe",
          "Take" = "#003660"
        )
      ) +
      labs(title = paste0(input$admit_stats_all, " Admissions"),
           x = NULL,
           y = NULL,
           fill = NULL) 
    
    # plotly 2019 - 2021 admissions 
    plotly::ggplotly(admissions_all_plot, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    

  }) # EO 2019-2021 admit stats
  
  ## SO 2021 diversity demographics overall ----
  # reactive diversity df 2021
  diversity_all_21 <- reactive({
    diversity %>% 
      filter(ay_year == 2021) %>% 
      filter(objective1 == input$diversity_stats_all)
  }) # EO reactive diversity df 2021
  
  ## PLOTTING ##
  output$diversity_2021 <- renderPlotly({
    demo_21 <- ggplot(data = diversity_all_21(),
                      aes(x = demographic,
                          y = count,
                          fill = demographic,
                          text = paste0(demographic, "\n", "Count: ", count))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_x_discrete(limits = rev(levels(diversity_all_21()$demographic))) +
      scale_y_continuous(breaks = seq(0, 70, 10)) +
      scale_fill_manual(
        values = c(
          "California Resident" = "#047c91",
          "Nonresident" = "#047c91",
          "International" = "#047c91",
          "Female" = "#9cbebe",
          "Age 25+" = "#9cbebe",
          "Military" = "#9cbebe",
          "Undocumented" = "#09847a",
          "African American or Black" = "#09847a",
          "American Indian or Alaska Native" = "#09847a",
          "Asian or Asian American" = "#09847a",
          "Hispanic or Latinx" = "#09847a",
          "Native Hawaiian / other Pacific Islander" = "#09847a",
          "White" = "#09847a",
          "Two or more races" = "#09847a",
          "Unknown race and ethnicity" = "#09847a"
        )
      ) +
      theme_minimal() +
      theme(legend.position = "none",
            panel.grid.minor = element_blank()) +
      labs(title = paste0("2021 ", input$diversity_stats_all, " Demographics"),
           x = NULL,
           y = "Number of students")
    
    # plotly 2021
    plotly::ggplotly(demo_21, tooltip = "text") %>%
      config(modeBarButtonsToRemove = list("pan", 
                                           "select", 
                                           "lasso2d", 
                                           "autoScale2d", 
                                           "hoverClosestCartesian", 
                                           "hoverCompareCartesian"))
    
  })
  
  ## SO career placements table  ----
  ## DATA WRANGLING ##
  employer <- mesmP %>% 
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
  
  
  # CAREER DB ----
  
  ## SO alumni map ----
  ## DATA WRANGLING ##
  # ca / out of state / international
  us_names <- c("USA", "US", "Usa")
  ca_names <- c("Ca", "CALIFORNIA")
  
  mesmP_map <- mesmP %>% 
    select(c(
      employer_account_name,
      work_location_city,
      mesm_class_year,
      work_location_state,
      work_location_country
    )) %>% 
    # standardize state values
    mutate(work_location_state = case_when(
      work_location_state %in% ca_names ~ "CA",
      work_location_state == "Maryland" ~ "MD",
      work_location_state == "Washington" ~ "WA",
      work_location_state == "District of Columbia" ~ "DC",
      # Note(HD): Don't need this because we replace with Seoul below
      #work_location_state == "N/A" ~ NA_character_, 
      work_location_state == "michoacan" ~ "Michoacan",
      # reassign NA values to correct state values
      work_location_city == "Washington DC" ~ "DC",
      work_location_city == "Oxnard" ~ "CA",
      work_location_city == "Santa Cruz" ~ "CA",
      work_location_city == "Fort Collins" ~ "CO",
      work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "AZ",
      work_location_city == "Amsterdam" ~ "North Holland",
      work_location_city == "Seoul" ~ "Seoul",
      TRUE ~ work_location_state
    )) %>% 
    # standardize united states values
    mutate(work_location_country = case_when(
      work_location_country %in% us_names ~ "United States",
      # reassign NA values to correct country values
      work_location_city == "Remote" & employer_account_name == "Fred Phillips Consulting" ~ "United States",
      work_location_city == "Fort Collins" & employer_account_name == "CGRS, Inc." ~ "United States",
      employer_account_name == "Cruz Foam" ~ "United States",
      employer_account_name == "United Water Conservation District" ~ "United States",
      TRUE ~ work_location_country
    )) %>% 
    # add latitude
    mutate(lat = case_when(
      work_location_state == "Ontario" ~ 51.2538,
      work_location_state == "Galapagos" ~ -0.9538,
      work_location_state == "Tahiti" ~ -17.6509,
      work_location_state == "Michoacan" ~ 19.5665,
      work_location_state == "North Holland" ~ 52.5206,
      work_location_state == "Seoul" ~ 37.532600,
      TRUE ~ NA_real_
    )) %>% 
    # add longitude
    mutate(long = case_when(
      work_location_state == "Ontario" ~ -85.3232,
      work_location_state == "Galapagos" ~ -90.9656,
      work_location_state == "Tahiti" ~ -149.4260,
      work_location_state == "Michoacan" ~ -101.7068,
      work_location_state == "North Holland" ~ 4.7885,
      work_location_state == "Seoul" ~ 127.024612,
      TRUE ~ NA_real_
    ))
  
  
  # domestic map polygons
  mesmP_domestic <- mesmP_map %>% 
    filter(work_location_country == "United States") %>% 
    select(-c(lat, long)) %>% 
    left_join(df_state_geometries_us, by = c("work_location_state" = "state_abbrev")) %>% 
    select(-c(fips, state)) %>% 
    st_as_sf() %>% 
    st_transform(crs = 4326)
  
  # domestic map polygons
  mesmP_domestic_stats <- reactive({
    
    mesmP_domestic %>%
      group_by(work_location_state) %>% 
      summarize(count = n())
  })
  
  ## PLOTTING MAP ##
  output$car_alumniMap <- renderLeaflet({ 
    
    bins <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 140)
    pal <- colorBin(palette = c("#e5f5e0", "#a1d99b", "#31a354"),
                    domain = mesmP_domestic_stats()$count, 
                    bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      mesmP_domestic_stats()$work_location_state, mesmP_domestic_stats()$count
    ) %>% lapply(htmltools::HTML)
    
    leaflet(mesmP_domestic_stats()) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(count),
        weight = 2,
        opacity = 1,
        color = "#003660",
        fillOpacity = 0.9,  
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      setView(lat = 37, lng = -118, zoom = 2.5)
  }) # EO alumni map
  
  
  ## SO job source ----
  output$curr_mesm_source <- renderPlot({
    
    mesmP_source <- mesmP %>% 
      select(c(mesm_class_year,
               job_source)) %>% 
      group_by(mesm_class_year, 
               job_source) %>% 
      summarize(count = n()) %>% 
      # 2 NAs 2019; 3 NAs 2021
      drop_na()
    
    # 2021
    ggplot(data = mesmP_source %>% filter(mesm_class_year == 2021),
           aes(x = reorder(job_source, count),
               y = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "MESM Job Source (2021)",
           # Note(HD): percentage includes 3 NAs ??
           subtitle = "Personal/ Professional network + Bren Network = 58%",
           x = NULL, 
           y = "Number of students") +
      theme_minimal() +
      theme(plot.subtitle = element_text(size = 9,
                                         face = "italic")) +
      geom_text(aes(label = count),
                fontface = "bold",
                size = 4,
                hjust = -0.21)
  }) # EO job source
  
  ## SO placement status ----
  output$curr_mesm_status <- renderPlot({
    mesm_status <- mesmS %>% 
      group_by(mesm_class_year,
               member_status) %>% 
      summarize(count = n())
    
    # 2021
    # color time off & searching
    # subtitle of students who got a job 6 months after graduating 
    ggplot(data = mesm_status %>% filter(mesm_class_year == 2021),
           aes(x = reorder(member_status, count),
               y = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "MESM Placement Overview (2021)",
           subtitle = "",
           x = NULL,
           y = "Number of students") +
      geom_text(aes(label = count),
                hjust = -0.22,
                size = 4) 
  }) # EO placement status 
  
  
  ## SO placement sector ----
  ## DATA WRANGLING ##
  sector_simple <- reactive({
    
    validate(
      need(input$sector_tree_check != "",
           "Please select at least one year.")
    ) # EO validate
    
    mesmP %>% 
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
  
  
  ## SO placement satisfaction ----
  ## DATA WRANGLING ##
  mesmP_satisfy <- reactive({
    
    mesmP %>%
      select(c(mesm_class_year,
             placement_satisfaction)) %>% 
      group_by(mesm_class_year,
             placement_satisfaction) %>%
      summarize(satisfy_count = n()) %>%
      filter(mesm_class_year == input$satisfy_all)
      
  })
  
  ## PLOTTING ##
  output$satisfaction <- renderPlot({
    # 2021
    # 3 NAs
    ggplot(data = mesmP_satisfy() %>% drop_na,
           aes(x = reorder(placement_satisfaction, satisfy_count),
               y = satisfy_count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste0("MESM ", input$satisfy_all, " satisfaction at job placement"),
           x = NULL,
           y = "Number of students") +
      theme_minimal() +
      geom_text(aes(label = satisfy_count),
                hjust = -0.3)
    
  }) # EO placement satisfaction
  
  
  
  ## SO placement avg compensation ----
  ## DATA WRANGLING ##
  mesm_salary <- mesmP %>% 
    select(c(mesm_class_year,
             estimated_annual_compensation_us)) %>% 
    drop_na() %>% 
    group_by(mesm_class_year) %>% 
    summarize(avg_salary = mean(estimated_annual_compensation_us))
  
  ## PLOTTING ##
  output$compensation <- renderPlotly({
    avg_salary <- ggplot(data = mesm_salary,
                         aes(x = mesm_class_year,
                             y = avg_salary)) +
      geom_line() +
      geom_point(aes(text = paste0("$", round(avg_salary)))) +
      scale_x_continuous(breaks = seq(min(mesm_salary$mesm_class_year),
                                      max(mesm_salary$mesm_class_year))) +
      theme_minimal() +
      labs(title = "MESM Average compensation",
           x = NULL,
           y = "US Dollars ($)")
    
    plotly::ggplotly(avg_salary, tooltip = "text")
    
  }) # EO compensation plot
  
  
  # DEMOGRAPHICS DB ----
  
  ## SO gender ----
  ## DATA WRANGLING ##
  gender_program_time <- bren_apps %>% 
    select(c("ay_year",
             "application_id",
             "gender",
             "objective1")) %>% 
    group_by(ay_year,
             objective1,
             gender) %>% 
    summarize(gender_count = n())
  
  gender_cohort_tot_all <- bren_apps %>% 
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
  # empty vars
  color <- NULL
  year_str <- NULL
  
  # render plotly
  output$age_all <- renderPlotly({
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
      df = bren_apps,
      color = color,
      year_str = year_str,
      prog_input = input$age_prog
    )
  }) # EO age plot

  
  
  ## SO CA res/ non res/ international ----
  ## DATA WRANGLING ##
  # 2021
  origin_program <- bren_apps %>% 
    select(c("ay_year",
             "application_id",
             "objective1",
             "citizenship_country",
             "residency_country",
             "birth_country",
             "california_resident",
             "ca_high_school",
             "visa")) %>% 
    # filter for just current yr
    filter(ay_year == 2021) %>% 
    mutate(california_resident = unlist(california_resident)) %>% 
    # residency status
    mutate(residency = case_when(
      # ca residency
      california_resident == TRUE & visa %in% c(NA,
                                                "DACA/AB540",
                                                "Permanent Resident",
                                                "Permanent Residency Pending (Work Permit)",
                                                "Undocumented Status") ~ "ca resident",
      # non ca resident
      california_resident == FALSE & visa %in% c(NA,
                                                 "Permanent Resident",
                                                 "Permanent Residency Pending (Work Permit)",
                                                 "Undocumented Status") ~ "non ca resident",
      # international
      visa %in% c("F-1 Student",
                  "J-1",
                  "Family of H,H1,H2,H3") ~ "international"
    )) %>% 
    group_by(ay_year, objective1, residency) %>% 
    summarize(residency_count = n())
  
  # 2016-2021
  origin_program_all <- bren_apps %>% 
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
      # ca residency
      california_resident == TRUE & visa %in% c(NA,
                                                "DACA/AB540",
                                                "Permanent Resident",
                                                "Permanent Residency Pending (Work Permit)",
                                                "Undocumented Status") ~ "ca resident",
      # non ca resident
      california_resident == FALSE & visa %in% c(NA,
                                                 "Permanent Resident",
                                                 "Permanent Residency Pending (Work Permit)",
                                                 "Undocumented Status") ~ "non ca resident",
      # international
      visa %in% c("F-1 Student",
                  "J-1",
                  "Family of H,H1,H2,H3") ~ "international"
    )) %>% 
    group_by(ay_year, objective1, residency) %>% 
    summarize(residency_count = n())
  
  residency_stats_all <- left_join(origin_program_all,
                                   prog_cohort_tot_all, 
                                   by = c("ay_year",
                                          "objective1")) %>% 
    mutate(residency_percent = round((residency_count / cohort_tot) * 100)) %>% 
    mutate(residency = factor(residency, levels = c("ca resident",
                                                    "non ca resident",
                                                    "international"),
                              labels = c("CA Resident",
                                         "Nonresident",
                                         "International")))
  
  ## PLOTTING ##
  output$residency_all <- renderPlotly({
    residency_all <- ggplot(data = residency_stats_all,
                            aes(x = ay_year,
                                y = residency_percent,
                                fill = reorder(residency, residency_percent),
                                text = paste0("Residency: ", residency, "\n",
                                              "Percent: ", residency_percent, "%"))) +
      geom_bar(position = "dodge",
               stat = "identity") +
      scale_x_continuous(breaks = seq(min(residency_stats_all$ay_year),
                                      max(residency_stats_all$ay_year))) +
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
  ## DATA WRANGLING ##
  origins_df <- origins_df %>%
      filter(ay_year == 2021) %>%
      group_by(objective1,
               ug1_location) %>%
      summarize(count = n())

  # Note(HD): Need to figure out how to make this reactive w/out breaking
  # don't forget to add () to df
  # don't forget to add input$origins_map_check and %in%
  # origins_rdf <- reactive({
  #   origins_df %>% 
  #     filter(ay_year %in% input$origins_map_check) %>%
  #     group_by(objective1,
  #              ug1_location) %>%
  #     summarize(count = n())
  # }) 
  
  
  ## PLOTTING ##
  output$origins_map <- leaflet::renderLeaflet({
    
    bins <- c(0, 1, 5, 10, 15, 20, 25, 260)
    pal <- colorBin(palette = c("#e5f5e0", "#a1d99b", "#31a354"),
                    domain = origins_df$count, 
                    bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%g",
      origins_df$ug1_location, origins_df$count
    ) %>% lapply(htmltools::HTML)
    
    # create map
    leaflet(origins_df) %>% 
      addTiles() %>% 
      addPolygons(
        fillColor = ~pal(count),
        weight = 2,
        opacity = 1,
        color = "#003660",
        fillOpacity = 0.9,  
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>% 
      setView(lat = 37, lng = -20, zoom = 1.5)
    
  }) # EO origins map
  
  ## SO race & ethnicity ----
  ## DATA WRANGLING
  # race and ethnicity test
  race_ethnicity <- bren_apps %>% 
    select(ay_year,
           objective1,
           background,
           category,
           hispanic_latino) %>%
    group_by(ay_year,
             objective1,
             background,
             category) %>% 
    summarize(count = n()) %>% 
    mutate(background = case_when(is.na(background) == TRUE ~ "Unknown race and ethnicity",
                                  TRUE ~ background),
           category = case_when(is.na(category) == TRUE ~ "Unknown race and ethnicity",
                                TRUE ~ category)) %>% 
    filter(ay_year == 2021)

  
  ## PLOTTING ##
  output$race_ethn_mesm <- d3treeR::renderD3tree2({
    race_eth_tree <- treemap::treemap(dtf = race_ethnicity %>% filter(objective1 == "MESM"),
                                      index = c("category", "background"),
                                      vSize = "count",
                                      type = "index"
                                      )
    
    # interactive treemap
    d3treeR::d3tree2(race_eth_tree, rootname = "2021 MESM Race and Ethnicity")
    
  })
  
  
} # EO server