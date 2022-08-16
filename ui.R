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
                     title = "Explore jobs Bren graduates are securing!",
                     solidHeader = TRUE,
                     status = "success",
                     includeMarkdown("text/career_about.md")
                   ) # EO intro box
                 ),
    
                 # * valueBox stats ----
                 valueBox(paste0("TBD", "%"), 
                          "NEW STAT HERE",
                          icon = icon("star"),
                          width = NULL
                 ), # EO valueBox 88% stat
                 valueBox(paste0("TBD"),
                          "NEW STAT HERE",
                          icon = icon("dollar-sign"),
                          color = "green",
                          width = NULL
                 ), # EO valueBox starting salary stat
                 valueBox(paste0("TBD"),
                          "NEW STAT HERE",
                          icon = icon("globe"),
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
          # * career map ----
          box(width = 6, 
              # solidHeader = TRUE,
              # status = "primary",
              title = "Bren Alumni Solve Environmental Problems Nationwide",
              leaflet::leafletOutput(outputId = "car_alumniMap") %>%
                withSpinner(color = "#003660", type = 1)
          ), # EO box career map
          
          # * career second plot ----
          tabBox(width = 6,
                 tabPanel("Sector",
                          d3treeR::d3tree2Output(outputId = "sector_tree") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "sector_tree_check",
                                             label = NULL,
                                             choices = c(2019, 2020, 2021),
                                             selected = 2021,
                                             inline = TRUE)
                 ), # EO tabPanel 1 in box 2 
                 tabPanel("Satisfaction",
                          plotOutput(outputId = "satisfaction") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "satisfy_all",
                                       label = NULL,
                                       choices = c("2019", "2020", "2021"),
                                       selected = "2021",
                                       inline = TRUE)
                 ), # EO tabPanel placement satisfaction in box 2
                 tabPanel("Average Compensation",
                          plotlyOutput(outputId = "compensation")
                 ), # EO tabPanel avg comp in box 2
                 tabPanel("Source",
                          plotOutput(outputId = "curr_mesm_source") %>%
                            withSpinner(color = "#003660", type = 1)
                 ), # EO tabPanel placement source in box 2
                 tabPanel("Status",
                          plotOutput(outputId = "curr_mesm_status") %>%
                            withSpinner(color = "#003660", type = 1)
                 ) # EO tabPanel placement status in box 2
          ) # EO tabBox career second plot
        
        ), # EO FR second row
      
        ), # EO tabItem career_db
      

      # tabs demographics ----
      tabItem(
        tabName = "demo_db",
        
        fluidRow(
          ## * intro box ----
          box(width = 12,
              title = "Learn more about students at Bren!",
              "Some introductory information",
              solidHeader = TRUE,
              status = "success"
              ) # EO intro box
        ), # EO FR first row
        
        fluidRow(
          column(4,
                 ## * valueBoxes ----
                 valueBoxOutput(outputId = "meds_curr_size",
                                width = NULL), 
                 valueBoxOutput(outputId = "mesm_curr_size",
                                width = NULL),
                 valueBoxOutput(outputId = "phd_curr_size",
                                width = NULL)
          ), # EO column 1 valueBoxes
          
          column(8,
                 tabBox(width = 12,
                        tabPanel("2021 Diversity Demographics",
                                 plotly::plotlyOutput(outputId = "diversity_2021") %>%
                                   withSpinner(color = "#003660", type = 1),
                                 radioButtons(inputId = "diversity_stats_all",
                                              label = NULL,
                                              choices = c("MEDS", "MESM", "PHD"),
                                              selected = "MESM",
                                              inline = TRUE)
                        ), # EO 2021 demographics tabPanel
                        tabPanel("Admissions",
                                 plotly::plotlyOutput(outputId = "admit_stats_all") %>%
                                   withSpinner(color = "#003660", type = 1),
                                 radioButtons(inputId = "admit_stats_all",
                                              label = NULL,
                                              choices = c("MEDS", "MESM", "PHD"),
                                              selected = "MESM",
                                              inline = TRUE)
                        ), # EO tabPanel previous admissions
                        tabPanel("Gender",
                                 plotly::plotlyOutput(outputId = "gender_all") %>%
                                   withSpinner(color = "#003660", type = 1)
                        ), # EO gender tabPanel
                        tabPanel("Age",
                                 plotly::plotlyOutput(outputId = "age_all") %>%
                                   withSpinner(color = "#003660", type = 1),
                                 radioButtons(inputId = "age_prog",
                                              label = NULL,
                                              choices = c("MEDS", "MESM", "PHD"),
                                              selected = "MESM",
                                              inline = TRUE)
                        ), # EO age tabPanel
                        tabPanel("Residency",
                                 plotly::plotlyOutput(outputId = "residency_all") %>%
                                   withSpinner(color = "#003660", type = 1)
                        ) # EO residency tabPanel
                 ) # EO demographics tabBox
                 
                 ) # EO column 2 tabbox
        ), # EO FR second row
        
        ## * plots ----
        fluidRow(
          box(width = 12,
              title = "Where students are coming from based on undergraduate location",
              solidHeader = TRUE,
              status = "navy",
              tmap::tmapOutput(outputId = "origins_map") %>%
                withSpinner(color = "#003660", type = 1)
              ) # EO map box  
        ), # EO FR third row
        
        fluidRow(
          ## * definitions ----
          box(title = "IPEDS and UC Demographics Definitions",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining important information and definitions 
              so a user can interpret these visuals accurately."
          ) # EO info box
        ), # EO FR fourth row
        
        ## * race & ethnicity plots ----
        fluidRow(
          # race / category
          box(
            width = 6,
            title = "Race / Category",
            plotly::plotlyOutput(outputId = "race_pltly") %>% 
              withSpinner(color = "#003660", type = 1),
            checkboxGroupInput(inputId = "race",
                         label = NULL,
                         choices = c("MEDS", "MESM", "PHD"),
                         selected = c("MEDS", "MESM", "PHD"),
                         inline = TRUE)
          ), # EO race plotly box
          # ethnicity / background
          tabBox(width = 6,
                 # tabPanel("American Indian or Alaska Native",
                 #          plotly::plotlyOutput(outputId = "amIn_alNat_eth_pltly") %>%
                 #            withSpinner(color = "#003660", type = 1),
                 #          checkboxGroupInput(inputId = "amIn_alNat_eth",
                 #                             label = NULL,
                 #                             choices = c("MEDS", "MESM", "PHD"),
                 #                             selected = c("MEDS", "MESM", "PHD"),
                 #                             inline = TRUE)
                 #          ), # EO american indian or alaksa native eth
                 tabPanel("Asian",
                          plotly::plotlyOutput(outputId = "asian_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "asian_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ), # EO asian eth
                 tabPanel("Black or African American",
                          plotly::plotlyOutput(outputId = "black_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "black_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ), # EO black or african american eth
                 tabPanel("Hispanic or Latino Ethnicities",
                          plotly::plotlyOutput(outputId = "hisp_lat_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "hisp_lat_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ), # EO hispanic or latino eth
                 # tabPanel("Native Hawaiian or Other Pacific Islander",
                 #          plotly::plotlyOutput(outputId = "natHi_pi_eth_pltly") %>%
                 #            withSpinner(color = "#003660", type = 1),
                 #          checkboxGroupInput(inputId = "natHi_pi_eth",
                 #                             label = NULL,
                 #                             choices = c("MEDS", "MESM", "PHD"),
                 #                             selected = c("MEDS", "MESM", "PHD"),
                 #                             inline = TRUE)
                 #          ), # EO native hawaiian or other pacific islander eth
                 tabPanel("White",
                          plotly::plotlyOutput(outputId = "white_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "white_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ), # EO white eth
                 tabPanel("Two or more races",
                          plotly::plotlyOutput(outputId = "two_more_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "two_more_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ), # EO two or more eth
                 tabPanel("Unknown race or ethnicity",
                          plotly::plotlyOutput(outputId = "unk_eth_pltly") %>%
                            withSpinner(color = "#003660", type = 1),
                          checkboxGroupInput(inputId = "unk_eth",
                                             label = NULL,
                                             choices = c("MEDS", "MESM", "PHD"),
                                             selected = c("MEDS", "MESM", "PHD"),
                                             inline = TRUE)
                          ) # EO unknown eth
          ) # EO ethnicity tabBox
        ) # EO FR fifth row
      ) # EO demo home tabItem
      
      ) # EO tabItems
      
    ) # EO dashboardBody

  ) # EO dashboardPage
