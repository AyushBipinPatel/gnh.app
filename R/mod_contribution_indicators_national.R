#' contribution_indicators_national UI Function
#'
#' @description A module to show the contribution of indicators to GNH at national level
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_contribution_indicators_national_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidPage(
      shiny::tags$div(
        shiny::tags$h1("Contribution of Indicators to GNH"),
        shiny::tags$p("Breif description on the contribution of indicatros to GNH.")
      ),
      shiny::sidebarLayout(
        sidebarPanel =  shiny::sidebarPanel(
          shinyWidgets::pickerInput(inputId = ns("ci_measure"),
                                    label = "Choose a Measure",
                                    choices = c(
                                      "Absolute contribution to happiness (%)" = "Absolute contribution (suf. adj)",
                                      "Relative contribution to happiness (%)" = "Relative contribution (suf. adj)"
                                    ),
                                    selected = "Absolute contribution (suf. adj)",
                                    multiple = F,
                                    options = list(
                                      `live-search` = TRUE)
          ),
          shinyWidgets::pickerInput(inputId = ns("ci_area"),
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
          shiny::actionButton(inputId = ns("ci_submit"),
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

#' contribution_indicators_national Server Functions
#'
#' @noRd
mod_contribution_indicators_national_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_ci_measures <- shiny::eventReactive(input$ci_submit,{
      input$ci_measure
    },ignoreNULL = F)

    sel_ci_area <- shiny::eventReactive(input$ci_submit,{
      input$ci_area
    },ignoreNULL = F)

    output$bar <- highcharter::renderHighchart({
      chart_measure_title <- switch(
        sel_ci_measures(),
          "Absolute contribution (suf. adj)" = "Absolute contribution to happiness (%)",
          "Relative contribution (suf. adj)" = "Relative contribution to happiness (%)"
      )

      if(sel_ci_measures() == "Absolute contribution (suf. adj)"){

        hch_simple_column_chart(
          if(isTRUE(input$arrange)){
            gnh_data_mod_contribution_indicators_national|>
              dplyr::filter(measure_lab == sel_ci_measures() &
                              area_lab == sel_ci_area())
          }else{
            gnh_data_mod_contribution_indicators_national|>
              dplyr::filter(measure_lab == sel_ci_measures() &
                              area_lab == sel_ci_area())|>
              dplyr::arrange(dplyr::desc(b))
          },
          x_axis = "ind_lab",
          y_axis = "b",
          colors = "ind_col",
          title = paste0(chart_measure_title,"to happiness at", sel_ci_area()," level"),
          flname = paste0(chart_measure_title,"to happiness", sel_ci_area()," level"), # same as the chart title as it would make sense to save a chart by its title
          tooltip = paste("Measure : ",chart_measure_title,"<br>Measure Value : {point.y}%<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
          xtitle = NULL,
          ytitle = chart_measure_title
        )

      }else{

        hch_simple_pie_chart(
          if(isTRUE(input$arrange)){
            gnh_data_mod_contribution_indicators_national|>
              dplyr::filter(measure_lab == sel_ci_measures() &
                              area_lab == sel_ci_area())
          }else{
            gnh_data_mod_contribution_indicators_national|>
              dplyr::filter(measure_lab == sel_ci_measures() &
                              area_lab == sel_ci_area())|>
              dplyr::arrange(dplyr::desc(b))
          },
          x_axis = "ind_lab",
          y_axis = "b",
          colors = "ind_col",
          title = paste0(chart_measure_title,"to happiness at", sel_ci_area()," level"),
          flname = paste0(chart_measure_title,"to happiness", sel_ci_area()," level"), # same as the chart title as it would make sense to save a chart by its title
          tooltip = paste("Measure : ",chart_measure_title,"<br>Measure Value : {point.y}%<br>Survey : {point.survey} <br>Survey Year : {point.year}"),
          xtitle = NULL,
          ytitle = chart_measure_title
        )

      }
    })

    output$table <- DT::renderDT({
      DT::datatable(
        gnh_data_mod_contribution_indicators_national|>
          dplyr::select(b,ind_lab,area_lab,measure_lab) |>
          dplyr:: mutate(area_measure = paste(area_lab,measure_lab))|>
          dplyr::select(-c(area_lab,measure_lab)) |>
          tidyr::pivot_wider(names_from = area_measure,values_from = b),
        colnames = c("Indicator", "Absolute contribution (%) - Rural",
                     "Absolute contribution (%) - Urban",
                     "Relative contribution (%) - Rural",
                     "Relative contribution (%) - Urban",
                     "Absolute contribution (%) - National",
                     "Relative contribution (%) - National"),
        filter = list(position = 'top', clear = FALSE),

        options = list(
          pageLength = 33,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste("Contribution of Indicators to GNH"))
        )

      )
    })


  })
}

## To be copied in the UI
# mod_contribution_indicators_national_ui("contribution_indicators_national_1")

## To be copied in the server
# mod_contribution_indicators_national_server("contribution_indicators_national_1")c
