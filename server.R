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
    # app id 57928 submitted 2015-03-26
    # app id 163517 submitted 2021-07-26
    ay_year_df <- apps_clean %>%
      # convert UNIX timestamp to datetime
      mutate(submitted_date = as.numeric(unlist(submitted_date)),
             submitted_date = anytime::anydate(submitted_date)) %>%
      # add year col
      mutate(ay_year = case_when(
        # 2016 apps: 2015-09-01 to 2016-03-31
        year(submitted_date) == 2015 & month(submitted_date) >= 3 ~ 2016,
        year(submitted_date) == 2016 & month(submitted_date) <= 3 ~ 2016,
        # 2017 apps: 2016-09-01 to 2017-03-31
        year(submitted_date) == 2016 & month(submitted_date) >= 9 ~ 2017,
        year(submitted_date) == 2017 & month(submitted_date) <= 3 ~ 2017,
        # 2018 apps: 2017-09-01 to 2018-03-31
        year(submitted_date) == 2017 & month(submitted_date) >= 9 ~ 2018,
        year(submitted_date) == 2018 & month(submitted_date) <= 3 ~ 2018,
        # 2019 apps: 2018-09-01 to 2019-03-31
        year(submitted_date) == 2018 & month(submitted_date) >= 9 ~ 2019,
        year(submitted_date) == 2019 & month(submitted_date) <= 3 ~ 2019,
        # 2020 apps: 2019-09-01 to 2020-03-31
        year(submitted_date) == 2019 & month(submitted_date) >= 9 ~ 2020,
        year(submitted_date) == 2020 & month(submitted_date) <= 3 ~ 2020,
        # 2021 apps: 2020-09-01 to 2021-03-31
        year(submitted_date) == 2020 & month(submitted_date) >= 9 ~ 2021,
        year(submitted_date) == 2021 & month(submitted_date) <= 7 ~ 2021)) 
    
    # total number of applicants per yr by program
    apps_tot <- ay_year_df %>% 
      group_by(ay_year,
               objective1) %>% 
      summarize(Applied = n())
    
    # total number of admitted applicants per yr by program
    admit_tot <- ay_year_df %>% 
      mutate(decision = case_when(
        decision %in% c("Provisionally Admitted","Unconditionally Admitted") ~ "Admitted",
        TRUE ~ decision
      )) %>% 
      group_by(ay_year,
               objective1,
               decision) %>% 
      summarize(Admitted = n())
    
    # total number of YES SIR submissions per year by program
    sir_yes_tot <- ay_year_df %>% 
      group_by(ay_year,
               objective1,
               sir) %>% 
      summarize(Take = n())
    
    # full df
    admissions <- left_join(sir_yes_tot,
                            apps_tot,
                            by = c("ay_year", "objective1")) %>% 
      left_join(admit_tot, by = c("ay_year", "objective1")) %>% 
      filter(decision == "Admitted",
             sir == "Yes") %>% 
      mutate(admit_rate = (Admitted / Applied),
             take_rate = (Take / Admitted))
    
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
  # app id 57928 submitted 2015-03-26
  # app id 163517 submitted 2021-07-26
  ay_year_df <- apps_clean %>%
    # convert UNIX timestamp to datetime
    mutate(submitted_date = as.numeric(unlist(submitted_date)),
           submitted_date = anytime::anydate(submitted_date)) %>%
    # add year col
    mutate(ay_year = case_when(
      # 2016 apps: 2015-09-01 to 2016-03-31
      year(submitted_date) == 2015 & month(submitted_date) >= 3 ~ 2016,
      year(submitted_date) == 2016 & month(submitted_date) <= 3 ~ 2016,
      # 2017 apps: 2016-09-01 to 2017-03-31
      year(submitted_date) == 2016 & month(submitted_date) >= 9 ~ 2017,
      year(submitted_date) == 2017 & month(submitted_date) <= 3 ~ 2017,
      # 2018 apps: 2017-09-01 to 2018-03-31
      year(submitted_date) == 2017 & month(submitted_date) >= 9 ~ 2018,
      year(submitted_date) == 2018 & month(submitted_date) <= 3 ~ 2018,
      # 2019 apps: 2018-09-01 to 2019-03-31
      year(submitted_date) == 2018 & month(submitted_date) >= 9 ~ 2019,
      year(submitted_date) == 2019 & month(submitted_date) <= 3 ~ 2019,
      # 2020 apps: 2019-09-01 to 2020-03-31
      year(submitted_date) == 2019 & month(submitted_date) >= 9 ~ 2020,
      year(submitted_date) == 2020 & month(submitted_date) <= 3 ~ 2020,
      # 2021 apps: 2020-09-01 to 2021-03-31
      year(submitted_date) == 2020 & month(submitted_date) >= 9 ~ 2021,
      year(submitted_date) == 2021 & month(submitted_date) <= 7 ~ 2021)) 
  
  # total number of applicants per yr by program
  apps_tot <- ay_year_df %>% 
    group_by(ay_year,
             objective1) %>% 
    summarize(Applied = n())
  
  # total number of admitted applicants per yr by program
  admit_tot <- ay_year_df %>% 
    mutate(decision = case_when(
      decision %in% c("Provisionally Admitted","Unconditionally Admitted") ~ "Admitted",
      TRUE ~ decision
    )) %>% 
    group_by(ay_year,
             objective1,
             decision) %>% 
    summarize(Admitted = n())
  
  # total number of YES SIR submissions per year by program
  sir_yes_tot <- ay_year_df %>% 
    group_by(ay_year,
             objective1,
             sir) %>% 
    summarize(Take = n())
  
  # full df
  admissions <- left_join(sir_yes_tot,
                          apps_tot,
                          by = c("ay_year", "objective1")) %>% 
    left_join(admit_tot, by = c("ay_year", "objective1")) %>% 
    filter(decision == "Admitted",
           sir == "Yes") %>% 
    mutate(admit_rate = (Admitted / Applied),
           take_rate = (Take / Admitted))
  
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
    select(c(mesm_class_year,
             employer_account_name,
             employer_sector)) %>% 
    filter(mesm_class_year == 2021) %>% 
    select(-mesm_class_year) %>% 
    group_by(employer_account_name,
             employer_sector) %>% 
    summarize(freq = n())
  ## TABLE ##
  output$careerP_tbl_21 <- DT::renderDataTable({
    DT::datatable(
      employer,
      colnames = c("Employer", "Sector", "# of alumni"),
      # caption = htmltools::tags$caption(style = "caption-side: top; text-align: left",
      #                                   htmltools::em("Employers of 2021 MESM graduates")),
      class = "cell-border stripe",
      rownames = FALSE,
      options = list(
        pageLength = 8,
        dom = 'Bftipr'
      ) # EO options
      
    ) # EO datatable
  }) # EO renderDataTable
  
  
  # CAREER HOME DB ----
  
  ## SO sector map ----
  output$car_sectorMap <- renderLeaflet({
    
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
    
    # df international placements
    mesmP_international <- mesmP_map %>% 
      filter(work_location_country != "United States") %>% 
      st_as_sf(coords = c("long", "lat"),
               crs = 4326)
    
    # get state geometries using tigris
    df_state_geometries_us <- tigris::states(year = 2018) %>%
      select(GEOID, STUSPS, NAME, geometry) %>% 
      rename(fips = GEOID,
             state_abbrev = STUSPS,
             state = NAME) %>% 
      rmapshaper::ms_simplify(keep = 0.005, keep_shapes = TRUE)
    
    # join with mesmP_map
    mesmP_domestic <- mesmP_map %>% 
      filter(work_location_country == "United States") %>% 
      select(-c(lat, long)) %>% 
      left_join(df_state_geometries_us, by = c("work_location_state" = "state_abbrev")) %>% 
      select(-c(fips, state)) %>% 
      st_as_sf() %>% 
      st_transform(crs = 4326) %>% 
      # Note(HD): create center point from multipolygon 
      st_centroid()
    
    # rbind dfs
    mesmP_map_all <- rbind(mesmP_international, mesmP_domestic) 
    
    mesmP_map_stats <- mesmP_map_all %>% 
      group_by(work_location_state) %>% 
      summarize(count = n())
    
    # full map
    # 2019 - 2021
    popup <- paste0("Location: ", mesmP_map_stats$work_location_state, 
                    "<br>", 
                    "Count: ", mesmP_map_stats$count)
    
    leaflet(mesmP_map_stats) %>% 
      addTiles() %>% 
      addCircleMarkers(radius = ~count,
                       popup = popup)
  }) # EO sector map
  
  # SO CURR CAREER DB ----
  
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
  

  
} # EO server