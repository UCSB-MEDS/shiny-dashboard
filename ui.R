# shiny dashboard has three main components:
# header, sidebar, body
ui <- dashboardPage(
  # HEADER ----
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  
  # SIDEBAR ----
  dashboardSidebar(
    sidebarMenu(
      id = "sidebarID",
      
      # welcome ----
      menuItem(
        tabName = "welcome", # Note(HD): previously curr_yr
        text = "Welcome",
        icon = icon("star", lib = "glyphicon"),
        badgeColor = "green"
      ), # EO curr yr menuItem
      
      # career ----
      menuItem(
        tabName = "career_db",
        text = "MESM Career Placement",
        icon = icon("road", lib = "glyphicon")
      ), # EO career menuItem
      
      # student demographics ----
      menuItem(
        tabName = "demo_db",
        text = "Student Demographics",
        icon = icon("user", lib = "glyphicon")
      )#, # EO student demographics menuItem
      
    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  
  # BODY ----
  dashboardBody(
    # call styles sheet
    tags$head(
      tags$link(rel = "stylesheet",
                type = "text/css",
                href = "styles.css")
    ), # EO tags$head
    
    # Note(HD): initially added to use hidden()
    useShinyjs(),
    
    
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      
      # tabs welcome ----
      tabItem(
        tabName = "welcome",
        h2("Welcome to the Bren Dashboard!"),
        
        # * intro text ----
        fluidRow(
          box(title = "Get to know our students",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining the dashboard, 
              why it's important, and what a user can do with it")
        ) # EO FR first row
        ), # EO curr yr tabItem
      
      
      # tabs career ----
      tabItem(
        tabName = "career_db",
        fluidRow(
          
          column(4,
                 # * intro box ----
                 fluidRow(
                   box(
                     width = 12,
                     title = "Explore Bren Alumni Career Outcomes",
                     solidHeader = TRUE,
                     status = "success",
                     includeMarkdown("text/career_about.md")
                   ) # EO intro box
                 ), # EO FR
    
                 # * valueBox stats ----
                 valueBox(paste0(44.3, "%"), 
                          "MESM Alumni are Very Satisfied with job placement",
                          icon = icon("heart"),
                          width = NULL
                 ), # EO valueBox overall satisfaction stat
                 # placement stat
                 valueBoxOutput(outputId = "placement_stat",
                                width = NULL),
                 valueBox(paste0(20.4, "%"),
                          "MESM Alumni are in Consulting",
                          icon = icon("briefcase"),
                          color = "blue",
                          width = NULL
                 ) # EO valueBox stay connected stat
          ), # EO column 1
          
          column(8,
                 # * info box ----
                 box(
                   title = "Bren MESM Alumni Employers and Sectors since 2019",
                   width = 12,
                   solidHeader = TRUE,
                   status = "navy",
                   DT::dataTableOutput(outputId = "career_employ_sector_tbl") %>%
                     withSpinner(color = "#003660", type = 1)
                 ) # EO employers and sectors box
          ) # EO column 2
          
        ), # EO FR first row
        
        fluidRow(
          box(id = "info_data_career",
              width = 12,
              span(
                tags$div(class = "lrg-bold",
                         includeMarkdown("text/career_data_info.md"))
              ),
              background = "green"),
          # remove title from box
          tags$head(tags$style('#info_data_career .box-header{ display: none}'))
          
        ), # EO FR second row
        
        fluidRow(
          # * career maps ----
          tabBox(width = 6,
                 tabPanel("Initial Placement Location",
                          tmap::tmapOutput(outputId = "car_alumniMap") %>%
                            withSpinner(color = "#003660", type = 1)#,
                          # tags$p(class = "italic_sector",
                          #        "Click a state to see the number of alumni at each location.")
                          ), # EO tabPanel leaflet map
                 tabPanel("Initial Placement",
                          plotly::plotlyOutput(outputId = "mesm_location") %>%
                            withSpinner(color = "#003660", type = 1)
                 ), # EO tabPanel bar plot location of MESM alumni
                 tabPanel("Initial International Placements",
                          DT::dataTableOutput(outputId = "international_place") %>%
                            withSpinner(color = "#003660", type = 1)
                          ) # EO tabPanel international placements
          ), # EO tabBox employers map / info
          
          # * career plots ----
          tabBox(width = 6,
                 tabPanel("Placement Status",
                          plotly::plotlyOutput(outputId = "mesm_placement_status") %>%
                            withSpinner(color = "#003660", type = 1)
                 ), # EO tabPanel placement status in box 2
                 tabPanel("Job Source",
                          plotly::plotlyOutput(outputId = "mesm_job_source") %>%
                            withSpinner(color = "#003660", type = 1)
                 ), # EO tabPanel placement source in box 2
                 tabPanel("Sector Trends",
                          plotly::plotlyOutput(outputId = "sector_trends") %>% 
                            withSpinner(color = "#003660", type = 1),
                          tags$p(class = "italic_sector",
                                 "Private includes Consulting and Corporate. 
                                 Public includes Federal Government, Local Government, 
                                 State Government, and Research/Education. Other includes 
                                 Foreign Government, Eco-E, and New Business. Non-Profit includes Non-Profit.")
                 ), # EO tabPanel sector over time
                 tabPanel("Sector Satisfaction",
                          plotly::plotlyOutput(outputId = "sector_satisfaction") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "sector_types",
                                       label = NULL,
                                       choices = c("Consulting", "Corporate", "Eco-E/New Business",
                                                   "Federal Government", "Foreign Government", 
                                                   "Local Government", "Non-Profit", 
                                                   "Research/Education", "State Government"),
                                       selected = "Consulting",
                                       inline = TRUE)
                 ), # EO tabPanel placement sector satisfaction in box 2
                 tabPanel("Salary",
                          plotlyOutput(outputId = "compensation"),
                          tags$p(class = "italic_sector",
                                 "Data includes Full-Time Career positions only."),
                          radioButtons(inputId = "compensation_year",
                                       label = NULL,
                                       choices = c(2019, 2020, 2021, "All Years"),
                                       selected = "All Years",
                                       inline = TRUE)
                 ), # EO tabPanel compensation in box 2
                 tabPanel("Salary by Specialization",
                          plotlyOutput(outputId = "comp_specialization"),
                          tags$p(class = "italic_sector",
                                 "Data includes Full-Time Career positions only."),
                          radioButtons(inputId = "compSpecialization_year",
                                       label = NULL,
                                       choices = c(2019, 2020, 2021, "All Years"),
                                       selected = "All Years",
                                       inline = TRUE)
                 ), # EO tabPanel compensation specialization in box 2
                 tabPanel("Salary by Sector",
                          plotlyOutput(outputId = "comp_sector"),
                          tags$p(class = "italic_sector",
                                 "Data includes Full-Time Career positions only."),
                          radioButtons(inputId = "compSector_year",
                                       label = NULL,
                                       choices = c(2019, 2020, 2021, "All Years"),
                                       selected = "All Years",
                                       inline = TRUE)
                 ) # EO tabPanel compensation sector in box 2
          ) # EO tabBox career second plot
        
        ), # EO FR second row
      
        ), # EO tabItem career_db
      

      # tabs demographics ----
      tabItem(
        tabName = "demo_db",
        
        fluidRow(
          column(5,
                 ## * intro box ----
                 box(width = 12,
                     title = "Learn more about students at Bren!",
                     includeMarkdown("text/demo_about.md"),
                     solidHeader = TRUE,
                     status = "success"
                 ), # EO intro box
                 
                 box(width = 12,
                     ## * undergrad map ----
                     title = "Where are students coming from?",
                     solidHeader = TRUE,
                     status = "navy",
                     tmap::tmapOutput(outputId = "origins_map") %>%
                       withSpinner(color = "#003660", type = 1),
                     tags$p(class = "italic_sector",
                            "Students come from over 200+ U.S. universities across 
                            41 states, and 66 international universities 
                            from 24 countries. View table of international
                            universities on the International tab to the right.")
                 ) # EO map box 
          ), # EO column 1 in FR 1
          
          column(7,
                 fluidRow(
                   ## * valueBoxes ----
                   valueBoxOutput(outputId = "meds_curr_size",
                                  width = 4), 
                   valueBoxOutput(outputId = "mesm_curr_size",
                                  width = 4),
                   valueBoxOutput(outputId = "PhD_curr_size",
                                  width = 4)
                 ), # EO FR 1 in column 2
                 fluidRow(
                   tabBox(width = 12,
                          ## * overall diversity demographics ----
                          tabPanel("Diversity Demographics",
                                   plotly::plotlyOutput(outputId = "overall_diversity") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "diversity_stats_all",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE), 
                                   tags$p(class = "italic_sector",
                                          "*URM or Underrepresented Minority is
                                        defined as a U.S. citizen who identifies
                                        as Black/African American, Hispanic/Latino,
                                        or American Indian, which is consistent 
                                        with the UC definition of URM.")
                          ), # EO 2021 demographics tabPanel
                          ## * admissions ----
                          tabPanel("Admissions",
                                   plotly::plotlyOutput(outputId = "admit_stats_all") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "admit_stats_all",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE)
                          ), # EO tabPanel previous admissions
                          ## * gender ----
                          tabPanel("Gender",
                                   plotly::plotlyOutput(outputId = "gender_all") %>%
                                     withSpinner(color = "#003660", type = 1)
                          ), # EO gender tabPanel
                          ## * age ----
                          tabPanel("Age",
                                   plotly::plotlyOutput(outputId = "age_all") %>%
                                     withSpinner(color = "#003660", type = 1),
                                   radioButtons(inputId = "age_prog",
                                                label = NULL,
                                                choices = c("MEDS", "MESM", "PhD"),
                                                selected = "MESM",
                                                inline = TRUE)
                          ), # EO age tabPanel
                          ## * residency ----
                          tabPanel("Residency",
                                   plotly::plotlyOutput(outputId = "residency_all") %>%
                                     withSpinner(color = "#003660", type = 1)
                          ), # EO residency tabPanel
                          ## * international unis tbl ----
                          tabPanel("International",
                                   #tags$p("International Universities Bren Alumni Attended"),
                                   DT::dataTableOutput(outputId = "intl_unis"))
                   ) # EO demographics tabBox
                 ) # EO FR 2 in column 2
          ) # EO column 2 in FR 1
          
        ), # EO FR first row
          
        
        fluidRow(
          ## * definitions ----
          box(id = "ipeds_def",
              title = "Integrated Postsecondary Education Data System (IPEDS) Definitions for Race and Ethnicity Categories",
              width = 12,
              span(
                tags$div(class = "lrg-bold",
                         includeMarkdown("text/ipeds_text.md"))
              ), # EO span tag
              background = "green"
          ), # EO info box
          # remove title from box
          tags$head(tags$style('#ipeds_def .box-header{ display: none}'))
        ), # EO FR fourth row
        
        ## * race & ethnicity plots ----
        fluidRow(
          # race / category
          tabBox(width = 6,
                 tabPanel("Race / Category",
                          plotly::plotlyOutput(outputId = "race_pltly") %>% 
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "race",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                 ), # EO race / category distribution tabPanel
                 tabPanel("Race / Category Trends Option 1",
                          plotly::plotlyOutput(outputId = "race_trends_pltly") %>% 
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "race_trends",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                 ), # EO race / category over time tabpanel
                 tabPanel("Race / Category Trends Option 2",
                          plotly::plotlyOutput(outputId = "race_trends_2_pltly") %>% 
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "race_trends_2",
                                       label = NULL,
                                       choices = c("Asian", 
                                                   "Black or African American", 
                                                   "Hispanic or Latino",
                                                   "White",
                                                   "Two or more races",
                                                   "Unknown race and ethnicity"),
                                       selected = "Asian",
                                       inline = TRUE)
                 ), # EO race / category over time tabpanel 2
                 tabPanel("URM Trends",
                          plotly::plotlyOutput(outputId = "urm_trends_pltly") %>% 
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "urm_trends",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                 ) # EO urm distribution tabpanel
          ), # EO tabBox race / category

          # ethnicity / background
          tabBox(width = 6,
                 tabPanel("Asian",
                          plotly::plotlyOutput(outputId = "asian_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "asian_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ), # EO asian eth
                 tabPanel("Black or African American",
                          plotly::plotlyOutput(outputId = "black_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "black_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ), # EO black or african american eth
                 tabPanel("Hispanic or Latino",
                          plotly::plotlyOutput(outputId = "hisp_lat_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "hisp_lat_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ), # EO hispanic or latino eth
                 tabPanel("White",
                          plotly::plotlyOutput(outputId = "white_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "white_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ), # EO white eth
                 tabPanel("Two or more races",
                          plotly::plotlyOutput(outputId = "two_more_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "two_more_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ), # EO two or more eth
                 tabPanel("Unknown race or ethnicity",
                          plotly::plotlyOutput(outputId = "unk_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "unk_eth",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PhD", "All Programs"),
                                       selected = "All Programs",
                                       inline = TRUE)
                          ) # EO unknown eth
          ) # EO ethnicity tabBox
        ) # EO FR fifth row
      ) # EO demo home tabItem
      
      ) # EO tabItems
      
    ) # EO dashboardBody

  ) # EO dashboardPage
