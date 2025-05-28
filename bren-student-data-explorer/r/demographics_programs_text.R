#' Generates text for the "About Bren's Programs & Demographics Data" box, including an updated range of years for which data are available
#'
#' @returns
#' @export
#'
#' @examples
demographics_programs_text <- function(){
  
  renderUI({
    HTML(paste0(
      'The Bren School offers three graduate programs:', "<br><br>",
      
      '<a href="https://bren.ucsb.edu/masters-programs/master-environmental-data-science" target="_blank">',
      '<img class="meds_hex" src="logos/bren_meds_hex.png" alt="MEDS hex in blue-green"> Master of Environmental Data Science (MEDS)</a>, a one-year professional master’s program focused on using data science to advance solutions to environmental problems.', "<br><br>",
      
      '<a href="https://bren.ucsb.edu/masters-programs/master-environmental-science-and-management" target="_blank">',
      '<img class="mesm_hex" src="logos/bren_mesm_hex.png" alt="MESM hex in dark-blue"> Master of Environmental Science & Management (MESM)</a>, a two-year professional master’s program focused on developing solutions to critical environmental problems while preparing students for careers in environmental science and management.', "<br><br>",
      
      '<a href="https://bren.ucsb.edu/phd-environmental-science-and-management" target="_blank">',
      '<img class="phd_hex" src="logos/bren_phd_hex.png" alt="PhD hex in light-green"> PhD in Environmental Science and Management</a>, a doctoral program designed to develop the broad knowledge, analytical powers, technical skills, and innovative thinking required to be a leader in the field.', "<br><br>",
      
      paste0("The information presented here is based on admissions data, provided by UCSB’s Graduate Division, for the incoming Bren School cohorts of ",
             curr_admission_year - 4, "-", curr_admission_year,
             ". Data are collected at the time of admission and enrollment.")
    ))
  })
  
}