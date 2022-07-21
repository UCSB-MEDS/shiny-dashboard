# server instructions
server <- function(input, output, session){
  
  # OE creating homepage for career and demo ----
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "CAREER"){
      updateTabItems(session, "sidebarID", selected = "hidden_career")
    }
  }) # EO OE
  
  observeEvent(input$sidebarItemExpanded, {
    if(input$sidebarItemExpanded == "DEMO"){
      updateTabItems(session, "sidebarID", selected = "hidden_demo")
    }
  }) # EO OE
  
  
  
  # 2021 DB ----
  ## SO military stat ----
  ### military 2021 df
  military_status <- bren_apps %>% 
    select(c("ay_year",
             "objective1",
             "military_service")) %>% 
    filter(ay_year == 2021) %>% 
    mutate(military_service = as.numeric(unlist(military_service)),
           military_status = case_when(military_service > 1 ~ "Y",
                                       TRUE ~ "N")) %>% 
    group_by(military_status) %>% 
    summarize(military_status_count = n()) %>% 
    filter(military_status == "Y")
    
  
  output$military_stat <- renderValueBox({
    valueBox("Students who have served in military",
             value = round((military_status$military_status_count / 119) * 100),
             icon = icon("credit-card"))
    
  })
  
} # EO server