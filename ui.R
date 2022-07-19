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
      id = "sidebarID",
      # curr yr ----
      menuItem(
        tabName = "curr_yr",
        text = "2021 Data",
        icon = icon("dashboard")
      ), # EO curr yr menuItem
      
      # career ----
      menuItem(
        id = "career_id",
        expandedName = "CAREER",
        tabName = "career_home",
        text = "Career Outcomes 101",
        icon = icon("th"),
        
        # sub curr career ----
        menuSubItem(
          tabName = "curr_career",
          text = "Current Career Data"
        ), # EO sub curr career menuSubItem
        
        # sub 5 yr career ----
        menuSubItem(
          tabName = "career_5yr",
          text = "5 Year Career Data"
        ) # EO sub 5 yr career
      ), # EO career menuItem
      
      hidden(menuItem("hidden_career", 
                      tabName = "hidden_career")
             ) # EO hidden_career menuItem
      
    ) # EO sidebarMenu
  ), # EO dashboardSidebar
  
  # BODY ----
  dashboardBody(
    useShinyjs(),
    tabItems(
      # Note(HD): tabName must match tabName in menuItem
      tabItem(
        tabName = "curr_yr",
        h2("2021 Dashboard Content")
      ), # EO curr yr tabItem
      
      tabItem(
        tabName = "hidden_career",
        h2("Career Outcomes Home Content")
      ), # EO career home tabItem
      
      tabItem(
        tabName = "curr_career",
        "Current Career Content"
      ), # EO sub curr career tabItem
      
      tabItem(
        tabName = "career_5yr",
        "5 yr Career Content"
      ) # EO sub 5 yr career tabItem
    ) # EO tabItems
    
  ) # EO dashboardBody
) # EO dashboardPage
