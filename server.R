####  server  #### 

server <- function(input, output, session){

#..................career tabPanel (career_db)...................
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # MESM placement percentage valueBox ----
  output$placement_stat <- employmentStatus_stat_valueBox(input)
  
  # MESM bren network stat valueBox ----
  output$brenNet_stat <- brenNet_stat_valueBox(input)
  
  # MESM satisfaction initial placement stat ----
  output$mesm_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input)

  ##~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM table  ----
  ##~~~~~~~~~~~~~~~~~~~~
  
  # MESM initial employers & sectors table ----
  output$career_employ_sector_tbl <- initialEmployers_table(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM geography tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # MESM domestic placement map ----
  output$car_alumniMap <- domesticPlacement_map(input)
  
  # MESM international placement table ----
  output$international_place <- internationalPlacement_table(input)
    
  # MESM geographic comparison plot ----
  output$mesm_location <- geographicComparison_plot(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM data viz tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # MESM placement status plot ----
  output$mesm_placement_status <- placementStatus_plot(input)
  
  # MESM job source plot ----
  output$mesm_job_source <- jobSource_plot(input)
    
  # MESM sector trends plot ----
  output$sector_trends <- sectorTrends_plot(input)
    
  # MESM sector satisfaction plot ----
  output$sector_satisfaction <- sectorSatisfaction_plot(input)
  
  # MESM salary plot ----
  output$compensation <- salary_plot(input)
  
  # MESM salary by sector plot ----
  output$comp_sector <- salaryBySector_plot(input)
    
  # MESM salary by specialization plot ----
  output$comp_specialization <- salarySpecialization_plot(input)
  
  
 
  
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
      "MEDS students in incoming 2022 cohort",
      value = meds_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "light-blue"
    )
  }) # EO MEDS valueBox curr size
  
  # MESM curr size valueBox output
  output$mesm_curr_size <- renderValueBox({
    valueBox(
      "MESM students in incoming 2022 cohort",
      value = mesm_size$size,
      icon = icon("users", lib = "font-awesome"),
      color = "blue"
    )
    
  })# EO MESM valueBox curr size
  
  # PhD curr size valueBox output
  output$phd_curr_size <- renderValueBox({
    valueBox(
      "PhD students in incoming 2022 cohort",
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
  
} # END server