#' d_comp_sufficiency_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_d_comp_sufficiency_indicators_ui <- function(id){
  ns <- NS(id)
  tagList(


    shiny::fluidPage(
      shiny::tags$h1("Sub-national level comparison of sufficiency in indicators"),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shinyWidgets::pickerInput(
            inputId = ns("dc_si"),
            label = "Choose a measure",
            choices =c(
              "Censored headcount ratio (%)" = "Censored headcount ratio (suf)",
              "Uncensored headcount ratio (%)" = "Uncensored headcount ratio"
            ),
            options = list(
              `live-search` = TRUE)
          ),
          shinyWidgets::pickerInput(
            inputId = ns("dc_si_ind"),
            label = "Choose a indicator",
            choices =gnh_data|>dplyr::pull(ind_lab)|>unique()|>na.omit(),
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

#' d_comp_sufficiency_indicators Server Functions
#'
#' @noRd
mod_d_comp_sufficiency_indicators_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_dc_si <- shiny::eventReactive(input$dc_submit,{
      input$dc_si
    },ignoreNULL = F)

    sel_dc_si_ind <- shiny::eventReactive(input$dc_submit,{
      input$dc_si_ind
    },ignoreNULL = F)

    output$map <- highcharter::renderHighchart({
      map_chart_title <- switch (

        sel_dc_si(),
        "Censored headcount ratio (suf)" = "Censored headcount ratio (%)" ,
        "Uncensored headcount ratio" = "Uncensored headcount ratio (%)"

      )


      hch_map(
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::filter(measure_lab == sel_dc_si() & ind_lab == sel_dc_si_ind()),
        paste0(map_chart_title," for ", sel_dc_si_ind())
      )
    })

    output$bar <- highcharter::renderHighchart({
      col_chart_title <- switch (

        sel_dc_si(),
        "Censored headcount ratio (suf)" = "Censored headcount ratio (%)" ,
        "Uncensored headcount ratio" = "Uncensored headcount ratio (%)",

      )

      hch_simple_column_chart(gnh_data_mod_sufficiency_in_indicators_district_overview|>
                                dplyr::filter(measure_lab == sel_dc_si() & ind_lab == sel_dc_si_ind())|>
                                dplyr::arrange(dplyr::desc(b)),
                              x_axis = "region_lab",
                              y_axis = "b",
                              title = paste0(col_chart_title, " for ", sel_dc_si_ind()),
                              flname = paste0(col_chart_title, " for ", sel_dc_si_ind()), # same as the chart title as it would make sense to save a chart by its title
                              tooltip = paste("Indicator: ", sel_dc_si_ind(),"<br>Measure : ",col_chart_title,"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
                              xtitle = NULL,
                              ytitle = col_chart_title)


    })

    output$table <- DT::renderDT({
      DT::datatable(
        gnh_data_mod_sufficiency_in_indicators_district_overview|>
          dplyr::select(b,measure_lab,share_val,region_lab,ind_lab)|>
          tidyr::pivot_wider(names_from = measure_lab,values_from = b)|>
          dplyr::relocate(region_lab,ind_lab,dplyr::everything()),
        colnames = c("Dzongkhags","Indicator" ,"Population Share (%)",
                     "Censored headcount ratio (%)",
                     "Uncensored headcount ratio (%)"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste(sel_dc_si(),"-Sufficiency in indicators"))
        )
      )
    })


  })
}

## To be copied in the UI
# mod_d_comp_sufficiency_indicators_ui("d_comp_sufficiency_indicators_1")

## To be copied in the server
# mod_d_comp_sufficiency_indicators_server("d_comp_sufficiency_indicators_1")
