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
      
      # curr yr ----
      menuItem(
        tabName = "curr_yr",
        text = "2021 Data",
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
      
      # tabs curr yr ----
      tabItem(
        tabName = "curr_yr",
        h2("2021 Dashboard Content"),
        
        # * intro text ----
        fluidRow(
          box(title = "Get to know our students",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining the dashboard, 
              why it's important, and what a user can do with it")
        ), # EO FR first row
        
        # * valueBoxes ----
        fluidRow(
          valueBoxOutput(outputId = "meds_curr_size"), 
          valueBoxOutput(outputId = "mesm_curr_size"),
          valueBoxOutput(outputId = "phd_curr_size")
        ), # EO FR second row
        # * admissions plots ----
        fluidRow(
          tabBox(width = 6,
                 tabPanel("2021 Admissions",
                          plotly::plotlyOutput(outputId = "admit_2021") %>%
                            withSpinner(color = "#003660", type = 1)
                          ), # EO tab panel curr admissions
                 tabPanel("Previous Admissions",
                          plotly::plotlyOutput(outputId = "admit_stats_all") %>%
                            withSpinner(color = "#003660", type = 1),
                          radioButtons(inputId = "admit_stats_all",
                                       label = NULL,
                                       choices = c("MEDS", "MESM", "PHD"),
                                       selected = "MESM",
                                       inline = TRUE)
                 ), # EO tabPanel previous admissions
          ) # EO tabBox
          # * demo and car plots ----
          ), # EO FR third row
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
        h2("Student Demographics Home Content"),
        
        fluidRow(
          ## * valueBoxes ----
          valueBox(2, "Undocumented students in 2020 programs", icon = icon("users")),
          valueBox(3, "Military students in 2021 programs", icon = icon("flag")),
          valueBox(11, "First generation students in 2021 programs", icon = icon("users"))
        ), # EO FR first row
        
        ## * plots ----
        fluidRow(
          box(width = 6,
              title = "Where students are coming from",
              leaflet::leafletOutput(outputId = "origins_map") %>%
                withSpinner(color = "#003660", type = 1),
              checkboxGroupInput(inputId = "origins_map_check",
                                 label = "Input is not active yet (only 2021 data)",
                                 choices = c(2016, 2017, 2018, 2019, 2020, 2021),
                                 selected = 2021,
                                 inline = TRUE)
              ), # EO map box  
          tabBox(width = 6,
                 tabPanel("2021 Diversity Demographics",
                                 plotly::plotlyOutput(outputId = "diversity_2021") %>%
                                   withSpinner(color = "#003660", type = 1),
                                 radioButtons(inputId = "diversity_stats_all",
                                              label = NULL,
                                              choices = c("MEDS", "MESM", "PHD"),
                                              selected = "MESM",
                                              inline = TRUE)
                        ), # EO 2021 demographics tabPanel
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
                 ), # EO demographics over time tabBox
        ), # EO FR second row
        
        fluidRow(
          ## * definitions ----
          box(title = "IPEDS and UC Demographics Definitions",
              width = 12,
              solidHeader = TRUE,
              status = "navy",
              "Here is some text explaining important information and definitions 
              so a user can interpret these visuals accurately."
          ) # EO info box
        ), # EO FR third row
        
        # race & ethnicity plots
        fluidRow(
          # race / category
          box(
            width = 6,
            title = "Race / Category",
            plotly::plotlyOutput(outputId = "race_pltly") %>% 
              withSpinner(color = "#003660", type = 1),
            radioButtons(inputId = "race",
                         label = NULL,
                         choices = c("MEDS", "MESM", "PHD"),
                         selected = "MESM",
                         inline = TRUE)
          ), # EO race plotly box
          # ethnicity / background
          box(
            width = 6,
            title = "Ethnicity / Background",
            plotly::plotlyOutput(outputId = "ethnicity_pltly") #%>% 
              #withSpinner(color = "#003660", type = 1)
          ) # EO ethnicity plotly box
        ) # EO FR fourth row
      ) # EO demo home tabItem
      
      ) # EO tabItems
      
    ) # EO dashboardBody

  ) # EO dashboardPage
