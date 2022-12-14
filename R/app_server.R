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


  # District report server logic --------------------------------------------

  mod_district_report_server("district_report_1")


  # District comparison primary measures ------------------------------------

  mod_d_comp_primary_measures_server("d_comp_primary_measures_1")


  # District comparison sufficiency in indicators ---------------------------

  mod_d_comp_sufficiency_indicators_server("d_comp_sufficiency_indicators_1")


  # District comparison contribution  of indicators -------------------------

  mod_d_comp_contribution_indicators_server("d_comp_contribution_indicators_1")





}
