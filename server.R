# server instructions
server <- function(input, output, session){
  
  # OE creating homepage for career and demo ----
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "CAREER"){
      updateTabItems(session, "sidebarID", selected = "hidden_career")
    }
  }) # EO OE
  
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "DEMO"){
      updateTabItems(session, "sidebarID", selected = "hidden_demo")
    }
  }) # EO OE
  
  
  
  # 2021 DB ----
  ## SO military stat ----
  # military 2021 df
  military_status <- bren_apps %>% 
    select(c("ay_year",
             "objective1",
             "military_service")) %>% 
    filter(ay_year == 2021) %>% 
    mutate(military_service = as.numeric(unlist(military_service)),
           military_status = case_when(military_service > 1 ~ "Y",
                                       TRUE ~ "N")) %>% 
    group_by(military_status) %>% 
    summarize(military_status_count = n()) %>% 
    filter(military_status == "Y") %>% 
    mutate(percent = (military_status_count / 119) * 100) # Note(HD): can't get percentage?
  # military output
  output$military_stat <- renderValueBox({
    valueBox("Students who have served in military",
             value = round((military_status$military_status_count / 119) * 100),
             icon = icon("flag"))
    
  })
  
  ## SO undocumented stat ----
  undocumented <- bren_apps %>% 
    select(c("ay_year",
             "application_id",
             "objective1",
             "visa")) %>% 
    # undocumented status
    mutate(undocumented_status = case_when(
      visa %in% c("DACA/AB540",
                  "Undocumented Status") ~ "Y",
      TRUE ~ "N")) %>% 
    group_by(ay_year, objective1, undocumented_status) %>% 
    summarize(undocumented_count = n()) %>% 
    filter(undocumented_status == "Y")
  # undocumented output
  output$undocumented_stat <- renderInfoBox({
    infoBox(title = paste0("Undocumented ", "\n", "students"), # Note(HD): paste doesn't seem to work
            value = undocumented$undocumented_count, 
            icon = icon("user"), 
            fill = TRUE)
  })
  
  ## SO urm stat ----
  # urm vars
  category_urms <- c("African American / Black",
                     "American Indian / Alaska Native")
  visa_urms <- c("Permanent Residency Pending (Work Permit)",
                 "Permanent Resident")
  # urm wrangling
  urm <- bren_apps %>% 
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
    filter(urm_status == "Y",
           ay_year == 2021) %>% 
    group_by(urm_status) %>% 
    summarize(count = n())
  # urm output
  output$urm_stat <- renderInfoBox({
    infoBox("URM Students", 
            value = urm$count, 
            icon = icon("user"))
  })
  
  ## SO program size curr ----
  output$program_size_curr <- renderPlot({
    # current year
    program_size_curr <- bren_apps %>% 
      select(c("ay_year",
               "application_id",
               "objective1")) %>%
      filter(ay_year == 2021) %>% 
      group_by(objective1) %>%
      summarize(prog_count = n())
    #plot
    ggplot(data = program_size_curr,
           aes(x = reorder(objective1, prog_count),
               y = prog_count,
               fill = objective1)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = prog_count,
                    color = objective1),
                vjust = 1.3,
                size = 4,
                fontface = "bold") +
      theme_minimal() +
      labs(title = NULL,
           x = NULL,
           y = "Number of students") +
      scale_fill_manual(values = c("MEDS" = "#047C91",
                                   "MESM" = "#6D7D33",
                                   "PHD" = "#005AA3")) +
      scale_color_manual(values = c("MEDS" = "#FFFFFF",
                                    "MESM" = "#FFFFFF",
                                    "PHD" = "#FFFFFF")) +
      theme(legend.position = "none")
  })
  
  # SO MESM admit stats
  output$mesm_admit_stats <- renderPlot({
    ay_year_df <- apps_clean %>%
      # convert UNIX timestamp to datetime
      mutate(submitted_date = as.numeric(unlist(submitted_date)),
             submitted_date = anytime::anydate(submitted_date)) %>%
      # add year col
      mutate(ay_year = case_when(
        # 2016 apps: 2015-09-01 to 2016-03-31
        lubridate::year(submitted_date) == 2015 & lubridate:: month(submitted_date) >= 3 ~ 2016,
        lubridate::year(submitted_date) == 2016 &  lubridate::month(submitted_date) <= 3 ~ 2016,
        # 2017 apps: 2016-09-01 to 2017-03-31
        lubridate:: year(submitted_date) == 2016 &  lubridate::month(submitted_date) >= 9 ~ 2017,
        lubridate::year(submitted_date) == 2017 &  lubridate::month(submitted_date) <= 3 ~ 2017,
        # 2018 apps: 2017-09-01 to 2018-03-31
        lubridate::year(submitted_date) == 2017 & lubridate:: month(submitted_date) >= 9 ~ 2018,
        lubridate::year(submitted_date) == 2018 &  lubridate::month(submitted_date) <= 3 ~ 2018,
        # 2019 apps: 2018-09-01 to 2019-03-31
        lubridate::year(submitted_date) == 2018 & lubridate:: month(submitted_date) >= 9 ~ 2019,
        lubridate::year(submitted_date) == 2019 & lubridate:: month(submitted_date) <= 3 ~ 2019,
        # 2020 apps: 2019-09-01 to 2020-03-31
        lubridate::year(submitted_date) == 2019 &  lubridate::month(submitted_date) >= 9 ~ 2020,
        lubridate::year(submitted_date) == 2020 &  lubridate::month(submitted_date) <= 3 ~ 2020,
        # 2021 apps: 2020-09-01 to 2021-03-31
        lubridate::year(submitted_date) == 2020 &  lubridate::month(submitted_date) >= 9 ~ 2021,
        lubridate::year(submitted_date) == 2021 &  lubridate::month(submitted_date) <= 7 ~ 2021)) 
    
    # total number of applicants per yr by program
    apps_tot <- ay_year_df %>% 
      group_by(ay_year,
               objective1) %>% 
      summarize(total_app_count = n())
    
    # total number of YES SIR submissions per year by program
    sir_yes_tot <- ay_year_df %>% 
      group_by(ay_year,
               objective1,
               sir) %>% 
      summarize(sir_tot_count = n())
    
    # total number of admitted applicants per yr by program
    admit_tot <- ay_year_df %>% 
      mutate(decision = case_when(
        decision %in% c("Provisionally Admitted","Unconditionally Admitted") ~ "Admitted",
        TRUE ~ decision
      )) %>% 
      group_by(ay_year,
               objective1,
               decision) %>% 
      summarize(decision_count = n())
    
    # full df
    admissions <- left_join(sir_yes_tot,
                            apps_tot,
                            by = c("ay_year", "objective1")) %>% 
      left_join(admit_tot, by = c("ay_year", "objective1")) %>% 
      filter(decision == "Admitted",
             sir == "Yes") %>% 
      mutate(admit_rate = (decision_count / total_app_count),
             take_rate = (sir_tot_count / decision_count))
    
    admissions_stacked <- admissions %>% 
      select(c(ay_year,
               objective1,
               sir_tot_count,
               total_app_count,
               decision_count)) %>% 
      pivot_longer(cols = c(sir_tot_count,
                            total_app_count,
                            decision_count),
                   names_to = "admin_tots",
                   values_to = "counts") %>% 
      mutate(admin_tots = factor(admin_tots, levels = c("total_app_count",
                                                        "decision_count",
                                                        "sir_tot_count")))
    
    # admissions stacked
    ggplot(data = admissions_stacked %>% filter(objective1 == "MESM"),
           aes(x = ay_year,
               y = counts,
               fill = admin_tots)) +
      geom_bar(stat = "identity",
               position = "dodge") +
      coord_flip() +
      scale_x_continuous(breaks = seq(min(admissions_stacked$ay_year),
                                      max(admissions_stacked$ay_year))) +
      theme_minimal() +
      theme(panel.grid.minor = element_blank()) +
      scale_fill_manual(
        values = c("decision_count" = "#9cbebe",
                   "sir_tot_count" = "#003660",
                   "total_app_count" = "#dcd6cc"),
        labels = c(
          "decision_count" = "Admitted students",
          "sir_tot_count" = "Current students",
          "total_app_count" = "Applicants"
        )
      ) +
      labs(x = NULL,
           y = NULL,
           fill = NULL)
  })

  
} # EO server