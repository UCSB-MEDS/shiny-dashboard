# R Shiny [Bren Student Data Exporer](https://shinyapps.bren.ucsb.edu/dashboard/)

## Purpose
The [Data Explorer](https://shinyapps.bren.ucsb.edu/dashboard/) is an interactive dashboard that showcases the Bren School admissions and career outcomes data of current students and recent alumni. It was developed with the intention of supporting:

* Prospective students in their decision-making as they explore the different degree programs at the Bren School
* Bren departments and staff with their reporting requirements
* All Bren communities and stakeholders from the past, present, and future by upholding data transparency and data integrity principles through an accessible application


## What is here?
    .
    ├── r                              # scripts to create visuals in app
    │   ├── age_plot.R                   # wrangle data and create age plot
    │   ├── background-distribution.R    # wrangle data and create background / ethnicity distribution plot
    │   ├── race_plot.R                  # wrangle data and create race / category plot
    │   └── urm_trends_plot.R            # wrangle data and create URM trends plot
    ├── rsconnect/shinyapps.io/hdolinh # created after running `deployApp()` 
    ├── text                           # markdown and html files that contain text to be included in app
    │   ├── career_about.md              # text introduces career page
    │   ├── career_data_info.md          # text with important info about career data
    │   ├── demo_about.md                # text introduces student demographic page
    │   ├── footer.html                  # text with icons on the bottom of the welcome page
    │   ├── ipeds_text.md                # text on the student demographics page that explains race / ethnicity definitions
    │   ├── welcome_data_text.html       # text with important information about the data in used in the app and who created it on welcome page
    │   └── welcome_what_text.md         # text overview of the whole app on welcome page
    ├── www                           # special directory in shiny for images and logos
    │   ├── logos                        # hex png files
    │   ├── images                       # jpg or png images used in app (this directory is in .gitignore and will not appear in github repo)
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

* Halina Do-Linh (Bren R Shiny Developer Fellow 2022)
* Jamie Montgomery (MEDS Program Coordinator)
* Sam Csik (NCEAS Data Training Coordinator)
* Kristi Birney (Bren Career Team)
* Kristine Duarte (Bren Student Affairs Team)

