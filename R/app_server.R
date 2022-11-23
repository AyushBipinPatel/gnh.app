#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic


  # landing page server logic -----------------------------------------------

  mod_landing_page_server("landing_page_1")


  # National primary measures server logic ----------------------------------

  mod_primary_measures_national_server("primary_measures_national_1")


  # National Sufficiency in Indicators --------------------------------------

  mod_sufficiency_indicators_national_server("sufficiency_indicators_national_1")


  # National contribution to GNH --------------------------------------------

  mod_contribution_indicators_national_server("contribution_indicators_national_1")



}
