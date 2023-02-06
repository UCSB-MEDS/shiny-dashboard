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
  
  ##~~~~~~~~~~~~~~~~~~~~
  ##  ~ valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~
  
  # MESM valueboxes ----
  output$placement_stat <- employmentStatus_stat_valueBox(input)
  output$mesm_brenNet_stat <- brenNet_stat_valueBox(input, data = mesm_placement)
  output$mesm_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input, data = mesm_placement)
  
  # MEDS valueBoxes ----
  # output$meds_placement_stat (WAITING FOR STATUS DATA)
  output$meds_brenNet_stat <- brenNet_stat_valueBox(input, data = meds_placement)
  output$meds_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input, data = meds_placement)
  
  ##~~~~~~~~~~~~~~~
  ##  ~ table  ----
  ##~~~~~~~~~~~~~~~
  
  # MESM table ----
  output$mesm_career_employ_sector_tbl <- initialEmployers_table(input, data = mesm_placement)
  
  # MEDS table ----
  output$meds_career_employ_sector_tbl <- initialEmployers_table(input, data = meds_placement)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ geography tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # MESM geography tabBox ----
  output$mesm_domesticPlacement_map <- domesticPlacement_map(input, data = mesm_placement)
  output$mesm_internationalPlacement_tbl <- internationalPlacement_table(input, data = mesm_placement)
  output$mesm_geogComparison_plot <- geographicComparison_plot(input, data = mesm_placement, program_acronym = "MESM")
  
  # MEDS geography tabBox ----
  output$meds_domesticPlacement_map <- domesticPlacement_map(input, data = meds_placement)
  # NO INTERNATIONAL PLACEMENT YET FOR MEDS (ADD WHEN APPROPRIATE)
  output$meds_geogComparison_plot <- geographicComparison_plot(input, data = meds_placement, program_acronym = "MEDS")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ data viz tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # MESM data viz tabBox ----
  output$mesm_placement_status <- placementStatus_plot(input) # NEED STATUS DATA
  output$mesm_job_source <- jobSource_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_sector_trends <- sectorTrends_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_sector_satisfaction <- sectorSatisfaction_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_salary <- salary_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_salary_by_sector <- salaryBySector_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_salary_by_specialization <- salarySpecialization_plot(input)
  
  # MEDS data viz tabBox ----
  # output$meds_placement_status (WAITING FOR STATUS DATA)
  output$meds_job_source <- jobSource_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_sector_trends <- sectorTrends_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_sector_satisfaction <- sectorSatisfaction_plot(input, data = meds_placement, program_acronym = "MEDS") # NEED TO FIGURE OUT WHY LOCAL GOVERMENT PLOT LOOKS WEIRD
  output$meds_salary <- meds_salary_plot(data = meds_placement) # FXN ONLY FOR 2022
  output$meds_salary_by_sector <- meds_salaryBySector_plot(data = meds_placement) # FXN ONLY FOR 2022
  
} # END server





