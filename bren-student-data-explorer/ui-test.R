#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------------------- HEADER-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

header <- dashboardHeader(
  
  title = span(tags$a(img(src = "logos/bren_hex.png"), href = "https://bren.ucsb.edu/", target = "_blank"), "Bren Student Data Explorer") 
  
) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##--------------------------------- SIDEBAR-------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

sidebar <- dashboardSidebar(
  
  sidebarMenu(id = "sidebarID",
              
              menuItem(tabName = "career_db", text = "Career Outcomes", icon = icon("road", lib = "glyphicon")),
              menuItem(tabName = "demo_db", text = "Demographics", icon = icon("user", lib = "glyphicon")),
              menuItem(tabName = "about", text = "About this Dashboard", icon = icon("star", lib = "glyphicon"), badgeColor = "green") 
              
  ) # END sidebarMenu
  
) # END dashboardSidebar

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##----------------------------------- BODY--------------------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

body <- dashboardBody(
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##            body header (add stylesheet, google analytics, etc.)          ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    includeHTML("www/google-analytics.html")
  ), # END header
  
  useShinyjs(), # call shinyjs; Note(HD): initially added to use hidden() ----
  
  tabItems(
    
    # START career tabItem ----
    tabItem(tabName = "career_db",
            
            tabsetPanel(id = "career_tabpanels",
                        
                        tabPanel(title = "MEDS Intial Career Placements"),
                        tabPanel(title = "MESM Initial Career Placements")
                        
                        ) # END career tabsetpanel
            
            ) # END career tabItem
    
  ) # END tabItems
  
) # END dashboardBody

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                                                                            --
##---------------------- COMBINE ALL INTO DASHBOARDPAGE-------------------------
##                                                                            --
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

dashboardPage(header, sidebar, body)
