####  server  #### 

server <- function(input, output, session){
  
#................demographics tabPanel (demo_db).................
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ program size valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$meds_curr_size <- programSize_valueBox(input, program_acronym = "MEDS", color = "light-blue")
  output$mesm_curr_size <- programSize_valueBox(input, program_acronym = "MESM", color = "blue") 
  output$phd_curr_size <- programSize_valueBox(input, program_acronym = "PhD", color = "green")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ demographics tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  output$overall_diversity <- diversityDemographics_plot(input)
  output$admit_stats_all <- admissions_plot(input)
  output$sex_all <- sex_plot(input)
  output$age_all <- age_plot(input)
  output$residency_all <- residency_plot(input)
  output$intl_unis <- internationalUniversities_table(input)
  
  ##~~~~~~~~~~~~~~~~~
  ##  ~ map box  ----
  ##~~~~~~~~~~~~~~~~~
  
  output$origins_map <- origins_map(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ race / category tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$urm_trends_pltly <- urmTrends_plot(input)
  output$race_trends_pltly <- ipedsTrends_plot(input)
  output$race_pltly <- ipedsCategories_plot(input)

  
  # IPEDS backgrounds
  
#..................career tabPanel (career_db)...................
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$placement_stat <- employmentStatus_stat_valueBox(input)
  output$brenNet_stat <- brenNet_stat_valueBox(input)
  output$mesm_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input)

  ##~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM table  ----
  ##~~~~~~~~~~~~~~~~~~~~

  output$career_employ_sector_tbl <- initialEmployers_table(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM geography tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$car_alumniMap <- domesticPlacement_map(input)
  output$international_place <- internationalPlacement_table(input)
  output$mesm_location <- geographicComparison_plot(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ MESM data viz tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$mesm_placement_status <- placementStatus_plot(input)
  output$mesm_job_source <- jobSource_plot(input)
  output$mesm_sector_trends <- sectorTrends_plot(input)
  output$mesm_sector_satisfaction <- sectorSatisfaction_plot(input)
  output$mesm_salary <- salary_plot(input)
  output$mesm_salary_by_sector <- salaryBySector_plot(input)
  output$mesm_salary_by_specialization <- salarySpecialization_plot(input)
  
  
  
  
  

  
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
  

  
} # END server





