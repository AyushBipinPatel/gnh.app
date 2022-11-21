#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_landing_page_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' landing_page Server Functions
#'
#' @noRd 
mod_landing_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_landing_page_ui("landing_page_1")
    
## To be copied in the server
# mod_landing_page_server("landing_page_1")
