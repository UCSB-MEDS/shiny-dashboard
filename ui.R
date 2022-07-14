# shiny dashboard has three main components:
# header, sidebar, body
dashboardPage(
  # HEADER ----
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  
  # SIDEBAR ----
  dashboardSidebar(
    title = "Menu",
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      
      ## Current yr ----
      menuItem(id = "current_yr",
               tabName = "current_yr",
               text = "2021 Dashboard",
               icon = icon("dashboard")
      ), # EO current_yr menuItem
      
      ## Career db ----
      menuItem(id = "career_db",
               tabName = "career_db",
               text = "Career Outcomes",
               icon = icon("th"),
               expandedName = "CAREER",
               ### sub current yr ----
               menuSubItem(text = "Currrent Year",
                           tabName = "career_curryr"),
               
               ### sub 5 yr ----
               menuSubItem(text = "5 Year Trends",
                           tabName = "career_5yr")
      ), # EO career_db menuItem
      
      ## Student demo ----
      menuItem(id = "student_demo",
               tabName = "student_demo",
               text = "Student Demographics",
               icon = icon("circle"),
               ### sub current yr ----
               menuSubItem(text = "Currrent Year",
                           tabName = "demo_curryr"),
               
               ### sub 5 yr ----
               menuSubItem(text = "5 Year Trends",
                           tabName = "demo_5yr")
      ), # EO student_demo menuItem
      
      shinyjs::hidden(menuItem(id = "hidden_career", 
                               tabName = "hidden_career",
                               text = "test"))
    ) # EO tabs sidebarMenu
  ), # EO dashboardSideBar
  
  # BODY ----
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      tabItem(tabName = "current_yr",
              h2("Current year dashboard content")
      ), # EO current_yr tabItem
      tabItem(tabName = "hidden_career",
              h2("Career dashboard content")
      ), # EO career_db tabItem
      tabItem(tabName = "career_curryr",
              h2("Career current year dashboard content")
      ), # EO career_curryr tabItem
      tabItem(tabName = "career_5yr",
              h2("Career 5 year dashboard content")
      ), # EO career_5yr tabItem
      tabItem(tabName = "demo_curryr",
              h2("Student Demographics current year dashboard content")
      ), # EO demo_curryr tabItem
      tabItem(tabName = "demo_5yr",
              h2("Student Demographics 5 year dashboard content")
      ) # EO demo_5yr tabItem
    ) # EO tabItems
  ) # EO dashboardBody
) # EO dashboardPage
