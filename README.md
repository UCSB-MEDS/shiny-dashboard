# R Shiny [Bren Student Data Exporer](https://shinyapps.bren.ucsb.edu/student-data-explorer/)

## Purpose
The [Data Explorer](https://shinyapps.bren.ucsb.edu/student-data-explorer/) is an interactive dashboard that showcases the Bren School admissions and career outcomes data of current students and recent alumni. It was developed with the intention of supporting:

* Prospective students in their decision-making as they explore the different degree programs at the Bren School
* Bren departments and staff with their reporting requirements
* All Bren communities and stakeholders from the past, present, and future by upholding data transparency and data integrity principles through an accessible application


## What is here?
    .
    ├── r                              # fxns for building inputs & outputs
    │   ├── admissions_plot.R                  
    │   ├── age_plot .R    
    │   ├── brenNet_stat_valuebox.R                  
    │   ├── diversityDemographics_plot.R                  
    │   ├── domesticPlacement_map.R                  
    │   ├── employmentStatus_stat_valueBox.R                  
    │   ├── geographicComparison_plot.R                  
    │   ├── initialEmployers_table.R 
    │   ├── initPlacementSatisfaction_stat_valueBox.R 
    │   ├── internationalPlacement_table.R 
    │   ├── internationalUniversities_table.R 
    │   ├── ipedsBackgrounds_plot.R 
    │   ├── ipedsCategories_plot.R 
    │   ├── ipedsTrends_plot.R 
    │   ├── jobSource_plot.R 
    │   ├── meds_salary_plot.R 
    │   ├── meds_salaryBySector_plot.R 
    │   ├── origins_map.R 
    │   ├── placementStatus_plot.R 
    │   ├── program_radioButtons.R 
    │   ├── programSize_valueBox.R 
    │   ├── race_plot.R 
    │   ├── residency_plot.R 
    │   ├── salary_plot.R 
    │   ├── salaryBySector_plot.R 
    │   ├── salarySpecialization_plot.R 
    │   ├── sectorSatisfaction_plot.R 
    │   ├── sectorTrends_plot.R 
    │   ├── sectorType_radioButtons.R 
    │   ├── sex_plot.R 
    │   ├── urmTrends_plot.R 
    │   └── year_radioButtons.R             
    ├── text                           # markdown and html files that contain text to be included in app
    │   ├── demo_about.md                  # introduces student demographic page
    │   ├── footer.html                    # landing page footer
    │   ├── ipeds_definition.md            # ipeds definition, used on demographics page
    │   ├── ipeds_text.md                  # Race and Background Reporting Defintions (green box on demographics page)
    │   ├── meds_career_about.md           # Welcome tab text on MEDS career page
    │   ├── meds_career_data_info.md       # About the data tab text on MEDS career page
    │   ├── meds_internationalPlacement.md # note about no MEDS international placements yet (takes place of table)
    │   ├── mesm_career_about.md           # Welcome tab text on MESM career page
    │   ├── mesm_career_data_info.md       # About the data tab text on MESM career page
    │   ├── race_ethnicity_text.md         # not currently used anywhere in app (ipeds_text.md used instead in green box)
    │   ├── sector_definitions.md          # sector definitions used as plot caption
    │   ├── undergrad_map_caption.md       # caption for undergrad origin map
    │   ├── urm_definition.md              # urm definition 
    │   ├── welcome_data_text.md           # landing page info about the data used in the app and who created it 
    │   └── welcome_what_text.md           # landing page info introducing the app
    ├── www                           # special directory in shiny for images and logos
    │   ├── logos                        # hex png files
    │   ├── images                       # jpg or png images used in app (e.g. banner image)
    │   ├── .DS_Store                    
    │   └── styles.css                   # CSS styles applied to app `tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"))`
    ├── global.R                      # data frames, variables, functions, and libraries that are used repeatedly in app
    ├── server.R                      # code needed to build visuals and overall app
    ├── ui.R                          # layout and appearance of app
    ├── shiny-dashboard.Rproj
    ├── .DS_store                     
    ├── .gitignore                    # files to ignore when committing project  
    └── README.md

## Contributors
This dashboard was first completed in September 2022 and was presented to the Bren Staff on 13 September 2022. This dashboard will be updated anually by Bren Staff and a MEDS Fellow. 

* [Halina Do-Linh](https://github.com/hdolinh) (Bren R Shiny Developer Fellow 2022)
* [Jamie Montgomery](https://github.com/jamiecmontgomery) (MEDS Program Coordinator)
* [Sam Csik](https://github.com/samanthacsik) (NCEAS Data Training Coordinator)
* [Kristi Birney](https://bren.ucsb.edu/people/kristi-birney) (Bren Career Team)
* [Kristine Duarte](https://bren.ucsb.edu/people/kristine-duarte) (Bren Student Affairs Team)

## Application Updates
* **February 2023, updates by [Sam Csik](https://github.com/samanthacsik):** refactored code base, added career data for MEDS and MESM graduating classes of 2022
* **July 2024, updates by [Sam Csik](https://github.com/samanthacsik), [Jamie Montgomery](https://github.com/jamiecmontgomery), & [Kat Le](https://github.com/katleyq):** added career data for MEDS and MESM graduating classes of 2023, added admissions data for the 2023 entering classes, refactored code for maps (`{tmap}` > `{leaflet}` + removed data wrangling from server to improve loading speeds) 
