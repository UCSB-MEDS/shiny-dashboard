internationalUniversities_table <- function(input) {
  
  # wrangle data ----
  ug_intl <- enrolled %>% 
    select(ug1_location,
           ug1_name) %>%
    filter(str_detect(ug1_location, "US - ") == FALSE) %>% 
    mutate(ug1_location = case_when(
      ug1_location %in% c("Korea, Republic Of (South)", "Korea-Republic Of (South)") ~ "South Korea",
      ug1_location %in% c("China, Peoples Republic", "China, P.R.") ~ "China",
      ug1_location == "District Of Columbia" ~ "District of Columbia",
      TRUE ~ ug1_location
    )) %>% 
    mutate(ug1_name = case_when(
      ug1_name == "BANGALORE UNIV" ~ "Bangalore University",
      ug1_name == "BEIJING FORESTRY UNIVERSITY" ~ "Beijing Forestry University",
      ug1_name  == "BEIJING U TECH" ~ "Beijing University of Technology",
      ug1_name == "BEIJING UNIV - BUSINESS & TECH" ~ "Beijing Technology and Business University",
      ug1_name == "BOGAZICI UNIV" ~ "Boğaziçi University",
      ug1_name == "ECOLE DE HAUTES ETUDES COMMRCL" ~ "École des Hautes Études Commerciales (HEC Montréal)",
      ug1_name == "ESPIRITU SANTO UNIVERSITY" ~ "Universidad de Especialidades Espíritu Santo",
      ug1_name == "INDIAN INST OF TECH (IIT) - KHARAGP" ~ "Indian Institute of Technology Kharagpur",
      ug1_name == "INDIAN INSTITUTE OF TECHNOLOGY" ~ "Indian Institute of Technology",
      ug1_name == "INDIAN SCH MINES" ~ "Indian Institute of Technology (Indian School of Mines)",
      ug1_name == "LAHORE UNIV MGMT SCIENCES" ~ "Lahore University of Management Sciences",
      ug1_name == "MCGILL UNIVERSITY" ~ "McGill University",
      ug1_name == "NANJING U INFO SCI & TECH" ~ "Nanjing University of Information Science and Technology",
      ug1_name == "NANKAI UNIVERSITY" ~ "Nankai University",
      ug1_name == "PONTIFICIA UNIV CATOLICA DEL ECUADOR" ~ "Pontifical Catholic University of Ecuador",
      ug1_name == "PONTIFICIA UNIVERSIDAD CATOLICA DE CHILE" ~ "Pontifical Catholic University of Chile",
      ug1_name == "QUEENS UNIVERSITY" ~ "Queen's University",
      ug1_name == "RAJASTHAN TECH UNIV" ~ "Rajasthan Technical University",
      ug1_name == "SARDAR VALLABH NAT'L INST OF TECH" ~ "National Institute of Technology Surat",
      ug1_name == "SEOUL NATIONAL UNIVERSITY" ~ "Seoul National University",
      ug1_name == "TECNOLGICO DE MONTERREY" ~ "Tecnológico de Monterrey",
      ug1_name == "TEL AVIV UNIVERSITY" ~ "Tel Aviv University",
      ug1_name %in% c("TSINGHUA UNIV", "TSINGHUA UNIVERSITY") ~ "Tsinghua University",
      ug1_name == "UNIV AUTONOMA DE BAJA CALIFORNIA" ~ "Autonomous University of Baja California",
      ug1_name == "UNIV DE SAO PAULO" ~ "University of São Paulo",
      ug1_name == "University of Sao Paulo - Sao Carlos" ~ "University of São Paulo at São Carlos School of Engineering",
      ug1_name == "UNIV DEL PACIFICO" ~ "Universidad del Pacífico",
      ug1_name == "UNIV DEL SALVADOR" ~ "Universidad de El Salvador",
      ug1_name == "UNIV LAS PALMAS DE GRAN CANARIA" ~ "Universidad de Las Palmas de Gran Canaria",
      ug1_name == "UNIV NACIONAL AUTONOMA DE HONDURAS" ~ "Universidad Nacional Autónoma de Honduras",
      ug1_name == "UNIV NACL DE LA PLATA" ~ "Universidad Nacional de La Plata",
      ug1_name == "UNIV OF NEW SOUTH WALES" ~ "University of New South Wales",
      ug1_name == "UNIV OF PANAMA" ~ "University of Panama",
      ug1_name == "UNIV OF SINGAPORE" ~ "National University of Singapore",
      ug1_name == "UNIV OF TORONTO" ~ "University Of Toronto",
      ug1_name == "UNIVE FEDERAL DO MARANHAO" ~ "Federal University of Maranhão",
      ug1_name == "universidad autónoma de nayarit" ~ "Universidad Autonoma de Nayarit",
      ug1_name == "UNIVERSITY OF BELIZE" ~ "University of Belize",
      ug1_name == "UNIVERSITY OF HONG KONG" ~ "University of Hong Kong",
      ug1_name == "WUHAN UNIVERSITY" ~ "Wuhan University",
      ug1_name == "UNIV FEDERAL DE SANTA MARIA" ~ "Federal University of Santa Maria",
      ug1_name == "AMERICAN UNIVERSITY OF PARIS" ~ "American University of Paris",
      ug1_name == "ITM UNIV- GURGAON" ~ "NorthCap University",
      ug1_name == "U MUMBAI" ~ "University of Mumbai",
      ug1_name == "VISVESVARAYA NATL INST TECHLGY" ~ "Visvesvaraya National Institute of Technology",
      ug1_name == "UNIV IBEROAMERICANA" ~ "Universidad Iberoamericana Ciudad de México",
      ug1_name == "UNIV PHILIPPINES" ~ "University of the Philippines",
      ug1_name == "UNIVERSITY OF SAINT ANDREWS" ~ "University of St Andrews",
      ug1_name == "MAKERERE UNIV" ~ "Makerere University",
      TRUE ~ ug1_name
    )) %>% 
    group_by(ug1_location,
             ug1_name) %>% 
    summarize(count = n())
  
  # render DT datatable ----
  DT::renderDataTable({
    
    DT::datatable(data = ug_intl, colnames = c("Country", "University", "# of alumni"),
                  caption = "International Universities Attended by Bren Alumni",
                  class = "cell-border stripe", rownames = FALSE,
                  options = list(pageLength = 12, dom = 'Bftipr')) 
    
  }) 
  
}




