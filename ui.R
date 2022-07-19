# shiny dashboard has three main components:
# header, sidebar, body
dashboardPage(
  # HEADER ----
  dashboardHeader(
    title = "Bren Dashboard"
  ), # EO dashboardHeader
  
  # SIDEBAR ----
  dashboardSidebar(
    id = "tabs",
    title = "Menu",
    sidebarMenu(
      # curr yr ----
      menuItem(
        tabName = "curr_yr",
        text = "2021 Data",
        icon = icon("dashboard")
      ), # EO curr yr menuItem
      
      # career ----
      menuItem(
        tabName = "career_home",
        text = "Career Outcomes 101",
        icon = icon("th"),
        
        # curr career ----
        menuSubItem(
          tabName = "curr_career",
          text = "Current Career Data"
        ), # EO curr_career menuSubItem
        
        # 5 yr career ----
        menuSubItem(
          tabName = "career_5yr",
          text = "5 Year Career Data"
        ) # EO 5 yr career
      ) # EO career menuItem
      
    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  # BODY ----
  dashboardBody(
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      tabItem(
        tabName = "curr_yr",
        h2("2021 Dashboard Content")
      ), # EO curr yr tabItem
      
      tabItem(
        tabName = "career_home",
        h2("Career Outcomes Home Content")
      ), # EO career home tabItem
      
      tabItem(
        tabName = "curr_career",
        "Current Career Content"
      ), # EO curr career tabItem
      
      tabItem(
        tabName = "career_5yr",
        "5 yr Career Content"
      ) # EO 5 yr career tabItem
    ) # EO tabItems
    
  ) # EO dashboardBody
) # EO dashboardPage
