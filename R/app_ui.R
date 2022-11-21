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
                                bootswatch = "simplex",
                                fg = "#000000",bg = "#ffffff",
                                base_font = bslib::font_google("Source Sans Pro",local = TRUE),
                                heading_font = bslib::font_google("Open Sans",local = TRUE)),
        shiny::tabPanel(title = "About GNH",
                        add_landing_page_info()
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
