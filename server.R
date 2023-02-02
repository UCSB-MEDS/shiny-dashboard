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
  output$background_pltly <- ipedsBackground_plot(input)
  
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
  
} # END server





