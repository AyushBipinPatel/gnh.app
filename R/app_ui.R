#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      shiny::navbarPage(
        title = "Bhutan National Happiness Index",
        theme = bslib::bs_theme(version = 4,
                                bootswatch = "flatly",
                                fg = "#000000",bg = "#ffffff",
                                base_font = bslib::font_google("Source Sans Pro",local = TRUE),
                                heading_font = bslib::font_google("Open Sans",local = TRUE)),
        shiny::tabPanel(title = "About GNH",
                        mod_landing_page_ui("landing_page_1")
                        ),
        shiny::navbarMenu(title = "National Level Results",

                          shiny::tabPanel(title = "GNH - Primary Measures",
                                          mod_primary_measures_national_ui("primary_measures_national_1")
                                          ),
                          shiny::tabPanel(title = "Sufficiency in Indicators",
                                          mod_sufficiency_indicators_national_ui("sufficiency_indicators_national_1")
                          ),
                          shiny::tabPanel(title = "Contribution to happiness",
                                          mod_contribution_indicators_national_ui("contribution_indicators_national_1")
                          )

                          ),
        shiny::tabPanel(title = "District Level Report",
                        mod_district_report_ui("district_report_1")
                        ),
        shiny::navbarMenu(title = "District Comparisons",
                          shiny::tabPanel(title = "Primary Measure - District Comparison",
                                          mod_d_comp_primary_measures_ui("d_comp_primary_measures_1")
                                          ),
                          shiny::tabPanel(title = "Sufficiency in Indicators - District Comparison"),
                          shiny::tabPanel(title = "Contribution of Indicators - District Comparison"),

                          )

        )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "gnh.app"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
