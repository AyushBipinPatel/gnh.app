#' primary_measures_national UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_primary_measures_national_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidPage(
      shiny::tags$div(
        shiny::tags$h1("Primary Measures of the GNH Index"),
        shiny::tags$p("Add description of GNH"),
        shiny::tags$p("Add description of Headcount of happy people"),
        shiny::tags$p("Add description of sufficiency of not yet happy people"),
      ),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(

          shinyWidgets::pickerInput(
            inputId = ns("primary_measure"),
            label = "Choose a Measure",
            choices = c(
              "Gross National Happiness Index (range 0 to 1)" = "GNH (suf)",
              "Headcount ratio of Happy people (%)" = "Headcount ratio (suf)",
              "Average sufficiency of Not-Yet-Happy (%)" = "Intensity (suf) among Not-Yet-Happy"
            ),
            selected = "GNH (suf)",
            multiple = F,
            options = list(
              `live-search` = TRUE)
          ),

          shiny::actionButton(inputId = ns("pm_submit"),
                              label = "Apply Changes"),
          width = 2

        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(
            shiny::tabPanel(title = "Chart",
                            highcharter::highchartOutput(ns("bar"), width = "100%",
                                                         height = "800px")
                            ),
            shiny::tabPanel(title = "Table",
                            DT::DTOutput(ns("table"),height = "800px")
                            )
          ),
          width = 10
        )
      )
    )

  )
}

#' primary_measures_national Server Functions
#'
#' @noRd
mod_primary_measures_national_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_primary_measure <- shiny::eventReactive(input$pm_submit,{
      input$primary_measure
    }, ignoreNULL = F)

    output$bar <- highcharter::renderHighchart({

      col_chart_title <- switch (

        sel_primary_measure(),
        "GNH (suf)" = "Gross National Happiness Index (range 0 to 1)" ,
        "Headcount ratio (suf)" = "Headcount ratio of Happy people (%)" ,
        "Intensity (suf) among Not-Yet-Happy" = "Average sufficiency of Not-Yet-Happy" ,

      )


      hch_simple_column_chart(gnh_data_mod_primary_measures|>
                                dplyr::filter(measure_lab.x == sel_primary_measure()),
                              x_axis = "area_lab",
                              y_axis = "b",
                              title = paste0(col_chart_title),
                              flname = paste0(col_chart_title), # same as the chart title as it would make sense to save a chart by its title
                              tooltip = paste("Measure : ",col_chart_title,"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
                              xtitle = NULL,
                              ytitle = col_chart_title)


    })

    output$table <- DT::renderDT({
      DT::datatable(
        gnh_data_mod_primary_measures|>
          dplyr::select(b,measure_lab.x,share_val,area_lab)|>
          tidyr::pivot_wider(names_from = measure_lab.x,values_from = b)|>
          dplyr::relocate(area_lab,dplyr::everything()),
        colnames = c("Area", "Population Share (%)",
                     "Headcount ratio of Happy people (%)",
                     "Average sufficiency of Not-Yet-Happy (%)",
                     "Gross National Happiness Index (range 0 to 1)"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste("Primary Measures of GNH"))
        )
      )
    })

  })
}

## To be copied in the UI
# mod_primary_measures_national_ui("primary_measures_national_1")

## To be copied in the server
# mod_primary_measures_national_server("primary_measures_national_1")
