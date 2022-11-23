#' sufficiency_indicators_national UI Function
#'
#' @description A module to show the uncensored and censored headcount ratios of sufficiency in indicators.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_sufficiency_indicators_national_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidPage(
      shiny::tags$div(
        shiny::tags$h1("Suficiency in Indicators"),
        shiny::tags$p("Breif description on the uncensored and censored sufficiency in indicators.")
      ),
      shiny::sidebarLayout(
        sidebarPanel =  shiny::sidebarPanel(
          shinyWidgets::pickerInput(inputId = ns("si_measure"),
                                    label = "Choose a Measure",
                                    choices = c(
                                      "Censored headcount ratio (%)" = "Censored headcount ratio (suf)",
                                      "Uncensored headcount ratio (%)" = "Uncensored headcount ratio (suf)"
                                    ),
                                    selected = "Censored headcount ratio (suf)",
                                    multiple = F,
                                    options = list(
                                      `live-search` = TRUE)
                                    ),
          shinyWidgets::pickerInput(inputId = ns("si_area"),
                                    label = "Choose an Area",
                                    choices = c(
                                      "National",
                                      "Urban",
                                      "Rural"
                                    ),
                                    selected = "National",
                                    multiple = F,
                                    options = list(
                                      `live-search` = TRUE)
                                    ),
          shiny::actionButton(inputId = ns("si_submit"),
                              label = "Apply Changes"),
          shiny::tags$hr(),
          shiny::tags$small("By default the indicators are arranged as per domains in the chart. Toggle the below switch to rearrange the chart in decreasing order."),
          shiny::tags$br(),
          shinyWidgets::materialSwitch(
            inputId = ns("arrange"),
            label = "Toggle to change order",
            value = TRUE,
            status = "primary"
          ),
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
          )
        )
      )
    )

  )
}

#' sufficiency_indicators_national Server Functions
#'
#' @noRd
mod_sufficiency_indicators_national_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_si_measures <- shiny::eventReactive(input$si_submit,{
      input$si_measure
    },ignoreNULL = F)

    sel_si_area <- shiny::eventReactive(input$si_submit,{
      input$si_area
    },ignoreNULL = F)

    output$bar <- highcharter::renderHighchart({
      chart_measure_title <- switch(
        sel_si_measures(),
        "Censored headcount ratio (suf)" = "Censored headcount ratio (%)" ,
        "Uncensored headcount ratio (suf)" = "Uncensored headcount ratio (%)"
      )

      hch_simple_column_chart(
       if(isTRUE(input$arrange)){
         gnh_data_mod_sufficiency_in_indicators|>
           dplyr::filter(measure_lab == sel_si_measures() &
                           area_lab == sel_si_area())
       }else{
         gnh_data_mod_sufficiency_in_indicators|>
           dplyr::filter(measure_lab == sel_si_measures() &
                           area_lab == sel_si_area())|>
           dplyr::arrange(dplyr::desc(b))
       },
        x_axis = "ind_lab",
        y_axis = "b",
        colors = "ind_col",
        title = paste0(chart_measure_title," for GNH indicators"),
        flname = paste0(chart_measure_title," for GNH indicators"), # same as the chart title as it would make sense to save a chart by its title
        tooltip = paste("Measure : ",chart_measure_title,"<br>Measure Value : {point.y}%<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
        xtitle = NULL,
        ytitle = chart_measure_title
      )
    })

    output$table <- DT::renderDT({
      DT::datatable(
        gnh_data_mod_sufficiency_in_indicators|>
          dplyr::select(b,ind_lab,area_lab,measure_lab) |>
          dplyr:: mutate(area_measure = paste(area_lab,measure_lab))|>
          dplyr::select(-c(area_lab,measure_lab)) |>
          tidyr::pivot_wider(names_from = area_measure,values_from = b),
        colnames = c("Indicator", "Rural Censored headcount ratio (%)",
                     "Urban Censored headcount ratio (%)",
                     "Rural Uncensored headcount ratio (%)",
                     "Urban Uncensored headcount rati0 (%)",
                     "National Censored headcount ratio (%)",
                     "National Uncensored headcount ratio (%)"),
        filter = list(position = 'top', clear = FALSE),

        options = list(
          pageLength = 33,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste("Sufficiency in Indicators of GNH"))
        )

      )
    })



  })
}

## To be copied in the UI
# mod_sufficiency_indicators_national_ui("sufficiency_indicators_national_1")

## To be copied in the server
# mod_sufficiency_indicators_national_server("sufficiency_indicators_national_1")
