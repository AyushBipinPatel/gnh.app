#' d_comp_primary_measures UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_d_comp_primary_measures_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::fluidPage(
      shiny::tags$h1("Sub-national level comparison of primary measures of GNH"),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shinyWidgets::pickerInput(
            inputId = ns("dc_pri_measure"),
            label = "Choose a measure",
            choices = c(
              "Gross National Happiness Index (range 0 to 1)" = "GNH (suf)",
              "Headcount ratio of Happy people (%)" = "Headcount ratio (suf)",
              "Average sufficiency of Not-Yet-Happy (%)" = "Intensity (suf)"
            ),
            options = list(
              `live-search` = TRUE)
           ),
          shiny::actionButton(inputId = ns("dc_submit"),"Apply Changes"),
          width = 2
          ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(

            shiny::tabPanel(title = "Map",
                            highcharter::highchartOutput(ns("map"), width = "100%",
                                                         height = "800px")
                            ),
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

#' d_comp_primary_measures Server Functions
#'
#' @noRd
mod_d_comp_primary_measures_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_dc_pri_measure <- shiny::eventReactive(input$dc_submit,{
      input$dc_pri_measure
    },ignoreNULL = F)

    output$map <- highcharter::renderHighchart({
      map_chart_title <- switch (

        sel_dc_pri_measure(),
        "GNH (suf)" = "Gross National Happiness Index (range 0 to 1)" ,
        "Headcount ratio (suf)" = "Headcount ratio of Happy people (%)" ,
        "Intensity (suf)" = "Average sufficiency of Not-Yet-Happy" ,

      )


      hch_map(
        gnh_data_mod_primay_measures_district_overview|>
          dplyr::filter(measure_lab == sel_dc_pri_measure()),
        map_chart_title
      )
    })

    output$bar <- highcharter::renderHighchart({
      col_chart_title <- switch (

        sel_dc_pri_measure(),
        "GNH (suf)" = "Gross National Happiness Index (range 0 to 1)" ,
        "Headcount ratio (suf)" = "Headcount ratio of Happy people (%)" ,
        "Intensity (suf)" = "Average sufficiency of Not-Yet-Happy" ,

      )

      hch_simple_column_chart(gnh_data_mod_primay_measures_district_overview|>
                                dplyr::filter(measure_lab == sel_dc_pri_measure())|>
                                dplyr::arrange(dplyr::desc(b)),
                              x_axis = "region_lab",
                              y_axis = "b",
                              title = paste0(sel_dc_pri_measure()," - ",col_chart_title),
                              flname = paste0(sel_dc_pri_measure()," - ",col_chart_title), # same as the chart title as it would make sense to save a chart by its title
                              tooltip = paste("Measure : ",col_chart_title,"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
                              xtitle = NULL,
                              ytitle = col_chart_title)


    })

    output$table <- DT::renderDT({
      DT::datatable(
        gnh_data_mod_primay_measures_district_overview|>
          dplyr::select(b,measure_lab,share_val,region_lab)|>
          tidyr::pivot_wider(names_from = measure_lab,values_from = b)|>
          dplyr::relocate(region_lab,dplyr::everything()),
        colnames = c("Dzongkhags", "Population Share (%)",
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
          htmltools::em(paste(sel_dc_pri_measure(),"-Primary Measures of GNH"))
        )
      )
    })

  })
}

## To be copied in the UI
# mod_d_comp_primary_measures_ui("d_comp_primary_measures_1")

## To be copied in the server
# mod_d_comp_primary_measures_server("d_comp_primary_measures_1")
