
server <- function(input, output, session){
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                        Career tabPanel (career_page)                     ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ text elements  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  output$meds_about_career_data_text <- career_data_text(program_acronym = "MEDS")
  output$mesm_about_career_data_text <- career_data_text(program_acronym = "MESM")
  
  ##~~~~~~~~~~~~~~~~~~~~
  ##  ~ valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~

  # MESM valueboxes ----
  output$placement_stat <- employmentStatus_stat_valueBox(input, data = mesm_status, program_acronym = "MESM")
  output$mesm_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input, data = mesm_placement)
  output$mesm_brenNet_stat <- brenNet_stat_valueBox(input, data = mesm_placement)

  # MEDS valueBoxes ----
  output$meds_placement_stat <- employmentStatus_stat_valueBox(input, data = meds_status, program_acronym = "MEDS")
  output$meds_satisfied_stat <- initPlacementSatisfaction_stat_valueBox(input, data = meds_placement)
  output$meds_brenNet_stat <- brenNet_stat_valueBox(input, data = meds_placement)

  ##~~~~~~~~~~~~~~~
  ##  ~ table  ----
  ##~~~~~~~~~~~~~~~

  # MESM table ----
  output$mesm_career_employ_sector_tbl <- initialEmployers_table(input, data = mesm_placement)
  output$mesm_title_tbl <- initialTitle_table(input, data = mesm_placement)

  # MEDS table ----
  output$meds_career_employ_sector_tbl <- initialEmployers_table(input, data = meds_placement)
  output$meds_title_tbl <- initialTitle_table(input, data = meds_placement)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ geography tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~

  # MESM geography tabBox ----
  output$mesm_domesticPlacement_map <- domesticPlacement_map(input, data = mesm_dom_placement_data)
  output$mesm_internationalPlacement_tbl <- internationalPlacement_table(input, data = mesm_placement)
  output$mesm_geogComparison_plot <- geographicComparison_plot(input, data = mesm_placement, program_acronym = "MESM")

  # MEDS geography tabBox ----
  output$meds_domesticPlacement_map <- domesticPlacement_map(input, data = meds_dom_placement_data)
  output$meds_internationalPlacement_tbl <- internationalPlacement_table(input, data = meds_placement)
  output$meds_geogComparison_plot <- geographicComparison_plot(input, data = meds_placement, program_acronym = "MEDS")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ data viz tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~

  # MEDS data viz tabBox ----
  output$meds_salary_plot <- salary_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_salary_by_sector_plot <- salaryBySector_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_placement_status_plot <- placementStatus_plot(input, data = meds_status, program_acronym = "MEDS")
  output$meds_sector_trends_plot <- sectorTrends_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_sector_satisfaction_plot <- sectorSatisfaction_plot(input, data = meds_placement, program_acronym = "MEDS")
  output$meds_job_source_plot <- jobSource_plot(input, data = meds_placement, program_acronym = "MEDS")
  
  # MESM data viz tabBox ----
  output$mesm_salary_plot <- salary_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_salary_by_sector_plot <- salaryBySector_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_salary_by_specialization_plot <- salarySpecialization_plot(input, data = mesm_placement)
  output$mesm_placement_status_plot <- placementStatus_plot(input, data = mesm_status, program_acronym = "MESM")
  output$mesm_sector_trends_plot <- sectorTrends_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_sector_satisfaction_plot <- sectorSatisfaction_plot(input, data = mesm_placement, program_acronym = "MESM")
  output$mesm_job_source_plot <- jobSource_plot(input, data = mesm_placement, program_acronym = "MESM")

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                  Demographics tabPanel (demographics_page)               ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ text elements  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  output$demographic_programs_text <- demographics_programs_text()
  output$student_origin_text <- student_origin_text()
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ program size valueBoxes  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$meds_curr_size_stat <- programSize_valueBox(input, program_acronym = "MEDS", color = "light-blue")
  output$mesm_curr_size_stat <- programSize_valueBox(input, program_acronym = "MESM", color = "blue")
  output$phd_curr_size_stat <- programSize_valueBox(input, program_acronym = "PhD", color = "green")
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ demographics tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$overall_diversity_plot <- diversityDemographics_plot(input, curr_year = curr_admission_year)
  output$admit_stats_all_plot <- admissions_plot(input)
  output$sex_all_plot <- sex_plot(input)
  output$age_all_plot <- age_plot(input, curr_year = curr_admission_year)
  output$residency_all_plot <- residency_plot(input)
  output$intl_unis_tbl <- internationalUniversities_table(input)
  
  ##~~~~~~~~~~~~~~~~~
  ##  ~ map box  ----
  ##~~~~~~~~~~~~~~~~~
  
  output$origins_map <- origins_map(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ race / category tabBox  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  output$urm_trends_plot <- urmTrends_plot(input)
  output$race_trends_plot <- ipedsTrends_plot(input)
  output$race_plot <- ipedsCategories_plot(input)
  output$background_plot <- ipedsBackground_plot(input)
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                        About tabPanel (about_page)                       ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  ##~~~~~~~~~~~~~~~~~~~~~~~
  ##  ~ text elements  ----
  ##~~~~~~~~~~~~~~~~~~~~~~~
  
  output$footer_date_text <- footer_date_text()
  
} # END server
