#' district_report UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_district_report_ui <- function(id){
  ns <- NS(id)
  tagList(

    shiny::fluidPage(

      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(

          shinyWidgets::pickerInput(
            inputId = ns("opts_dist"),
            label = "Choose a district",
            choices = gnh_data_mod_primay_measures_district_overview|>
              dplyr::pull(region_lab) |> unique(),
            options = list(
              `live-search` = TRUE)
              ),
          shinyWidgets::actionBttn(inputId = ns("dr_submit"),
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

          shiny::tags$h1(
            shiny::textOutput(outputId = ns("heading1"))
          ),
          shiny::tags$hr(),
          shiny::tags$h2("Primary Measures of GNH"),
          shiny::fluidRow(
            shiny::tags$div(shinyWidgets::statiCard(value = shiny::textOutput(ns("gnh")),
                                                    subtitle = "GNH")),
            shiny::HTML('&nbsp&nbsp&nbsp'),
            shiny::tags$div(shinyWidgets::statiCard(value = shiny::textOutput(ns("hh")),
                                                    subtitle = "Headcount ratio of Happy")),
            shiny::HTML('&nbsp&nbsp&nbsp'),
            shiny::tags$div(shinyWidgets::statiCard(value = shiny::textOutput(ns("aus")),
                                                    subtitle = "Average Sufficiency of Not-Yet-Happy"))
          ),
          shiny::tags$hr(),
          shiny::tags$h2("Sufficiency in Indicators"),
            shiny::tabsetPanel(
              shiny::tabPanel(title = "Censored Headcount ratio",
                              highcharter::highchartOutput(ns("bar_c"), width = "100%",
                                                           height = "800px")
                              ),
              shiny::tabPanel(title = "Uncensored Headcount ratio",
                              highcharter::highchartOutput(ns("bar_u"), width = "100%",
                                                           height = "800px")
              ),
              shiny::tabPanel(title = "Table",
                              DT::DTOutput(ns("table_cu"),height = "800px")
                              )
            ),

          shiny::tags$hr(),
          shiny::tags$h2("Contribution to indicators to GNH"),
          shiny::tabsetPanel(
            shiny::tabPanel(title = "Absolute Contribution",
                            highcharter::highchartOutput(ns("bar_a"), width = "100%",
                                                         height = "800px")
            ),
            shiny::tabPanel(title = "Relative Contribution",
                            highcharter::highchartOutput(ns("bar_r"), width = "100%",
                                                         height = "800px")
            ),
            shiny::tabPanel(title = "Table",
                            DT::DTOutput(ns("table_ar"),height = "800px")
            )
          ),

          width = 10
      )
    )

    )

  )
}

#' district_report Server Functions
#'
#' @noRd
mod_district_report_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_dist <- shiny::eventReactive(input$dr_submit,{
      input$opts_dist
    },ignoreNULL = F)


# primary measures --------------------------------------------------------


    output$gnh <- shiny::renderText({
      gnh_data_mod_primay_measures_district_overview|>
        dplyr::filter(region_lab == sel_dist() & measure_lab == "GNH (suf)") |>
        dplyr::pull(b)
    })

    output$hh <- shiny::renderText({
      gnh_data_mod_primay_measures_district_overview|>
        dplyr::filter(region_lab == sel_dist() & measure_lab == "Headcount ratio (suf)") |>
        dplyr::pull(b)
    })

    output$aus <- shiny::renderText({
      gnh_data_mod_primay_measures_district_overview|>
        dplyr::filter(region_lab == sel_dist() & measure_lab == "Intensity (suf)") |>
        dplyr::pull(b)
    })



# sufficiency in indicators -----------------------------------------------

    output$bar_c <- highcharter::renderHighchart({


      hch_simple_column_chart(if(isTRUE(input$arrange)){
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Censored headcount ratio (suf)")
      }else{
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Censored headcount ratio (suf)")|>
          dplyr::arrange(dplyr::desc(b))
      },
                              x_axis = "ind_lab",
                              y_axis = "b",
                              colors = "ind_col",
                              title = paste0(sel_dist(),"Censored headcount ratio (%)"),
                              flname = paste0(sel_dist(),"Censored headcount ratio (%)"), # same as the chart title as it would make sense to save a chart by its title
                              tooltip = paste("Measure : Censored headcount ratio","<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
                              xtitle = NULL,
                              ytitle = "Censored Headcount ratio (%)")


    })

    output$bar_u <- highcharter::renderHighchart({


      hch_simple_column_chart(if(isTRUE(input$arrange)){
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Uncensored headcount ratio")
      }else{
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Uncensored headcount ratio")|>
          dplyr::arrange(dplyr::desc(b))
      },
                              x_axis = "ind_lab",
                              y_axis = "b",
                              colors = "ind_col",
                              title = paste0(sel_dist(),"Uncensored headcount ratio (%)"),
                              flname = paste0(sel_dist(),"Uncensored headcount ratio (%)"), # same as the chart title as it would make sense to save a chart by its title
                              tooltip = paste("Measure : UNcensored headcount ratio","<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
                              xtitle = NULL,
                              ytitle = "Uncensored Headcount ratio (%)")


    })

    output$table_cu <-  DT::renderDataTable({
      DT::datatable(
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist())|>
          dplyr::select(b,ind_lab,measure_lab) |>
          tidyr::pivot_wider(names_from = measure_lab,values_from = b),
        colnames = c("Indicator", "Censored headcount ratio (%)",
                     "Uncensored headcount ratio (%)"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          pageLength = 33,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste(sel_dist(),"- Sufficiency in Indicators of GNH"))
        )
      )

    })



# Contribution of Indicators ----------------------------------------------

    output$bar_a <- highcharter::renderHighchart({


      hch_simple_column_chart(if(isTRUE(input$arrange)){
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Absolute contribution (suf. adj)")
      }else{
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Absolute contribution (suf. adj)")|>
          dplyr::arrange(dplyr::desc(b))
      },
      x_axis = "ind_lab",
      y_axis = "b",
      colors = "ind_col",
      title = paste0(sel_dist(),"Absolute contribution to GNH (%)"),
      flname = paste0(sel_dist()," Absolute contribution to GNH (%)"), # same as the chart title as it would make sense to save a chart by its title
      tooltip = paste("Measure : Absolute contribution to GNH","<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
      xtitle = NULL,
      ytitle = "Absolute contribution to GNH (%)")


    })

    output$bar_r <- highcharter::renderHighchart({


      hch_simple_pie_chart(if(isTRUE(input$arrange)){
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Relative contribution (suf. adj)")
      }else{
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist() &
                          measure_lab == "Relative contribution (suf. adj)")|>
          dplyr::arrange(dplyr::desc(b))
      },
      x_axis = "ind_lab",
      y_axis = "b",
      colors = "ind_col",
      title = paste0(sel_dist()," Relative contribution (%) to GNH"),
      flname = paste0(sel_dist()," Relative contribution (%) to GNH"), # same as the chart title as it would make sense to save a chart by its title
      tooltip = paste("Measure : Relative contribution (%)","<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
      xtitle = NULL,
      ytitle = "Relative contribution (%) (%)")


    })

    output$table_ar <-  DT::renderDataTable({
      DT::datatable(
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(region_lab == sel_dist())|>
          dplyr::select(b,ind_lab,measure_lab) |>
          tidyr::pivot_wider(names_from = measure_lab,values_from = b),
        colnames = c("Indicator","Absolute contribution (%)", "Relative Contribution (%)"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          pageLength = 33,
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste(sel_dist(),"- Contribution of Indicators to GNH"))
        )
      )

    })



  })
}

## To be copied in the UI
# mod_district_report_ui("district_report_1")

## To be copied in the server
# mod_district_report_server("district_report_1")
