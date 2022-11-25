#' d_comp_contribution_indicators UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_d_comp_contribution_indicators_ui <- function(id){
  ns <- NS(id)
  tagList(



    shiny::fluidPage(
      shiny::tags$h1("Sub-national level comparison of contribution of indicators"),
      shiny::sidebarLayout(
        sidebarPanel = shiny::sidebarPanel(
          shinyWidgets::pickerInput(
            inputId = ns("dc_ci"),
            label = "Choose a measure",
            choices =c(
              "Absolute contribution (%)" = "Absolute contribution (suf. adj)",
              "Relative contribution ratio (%)" = "Relative contribution (suf. adj)"
            ),
            options = list(
              `live-search` = TRUE)
          ),
          shiny::conditionalPanel(condition = "input.mod_d_comp_contr == 'Map'",
                                  ns = ns,
                                  shiny::tags$hr(),
                                  shinyWidgets::pickerInput(
                                    inputId = ns("dc_ci_ind"),
                                    label = "Choose a indicator",
                                    choices =gnh_data|>dplyr::pull(ind_lab)|>unique()|>na.omit(),
                                    options = list(
                                      `live-search` = TRUE)
                                  )
                                  ),
          shiny::actionButton(inputId = ns("dc_submit"),"Apply Changes"),
          width = 2
        ),
        mainPanel = shiny::mainPanel(
          shiny::tabsetPanel(id = ns("mod_d_comp_contr"),

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

#' d_comp_contribution_indicators Server Functions
#'
#' @noRd
mod_d_comp_contribution_indicators_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    sel_dc_ci <- shiny::eventReactive(input$dc_submit,{
      input$dc_ci
    },ignoreNULL = F)

    sel_dc_ci_ind <- shiny::eventReactive(input$dc_submit,{
      input$dc_ci_ind
    },ignoreNULL = F)

    output$map <- highcharter::renderHighchart({
      map_chart_title <- switch (

        sel_dc_ci(),
        "Absolute contribution (suf. adj)" = "Absolute contribution (%)",
          "Relative contribution (suf. adj)" = "Relative contribution ratio (%)"

      )


      hch_map(
        gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::filter(measure_lab == sel_dc_ci() & ind_lab == sel_dc_ci_ind()),
        paste0(map_chart_title," for ", sel_dc_ci_ind())
      )
    })

    output$bar <- highcharter::renderHighchart({
      col_chart_title <- switch (

        sel_dc_ci(),
        "Absolute contribution (suf. adj)" = "Absolute contribution (%)",
        "Relative contribution (suf. adj)" = "Relative contribution ratio (%)"

      )

      highcharter::hchart(object = gnh_data_mod_contribution_indicators_district_overview|>
                            dplyr::filter(measure_lab == sel_dc_ci())|>
                            dplyr::mutate(
                              ind_lab = forcats::fct_relevel(ind_lab,
                                                             c(
                                                               "Life satisfaction",
                                                               "Positive emotions",
                                                               "Negative emotions",
                                                               "Spirituality",
                                                               "Self-reported health status",
                                                               "Healthy days",
                                                               "Disability",
                                                               "Mental health",
                                                               "Work",
                                                               "Sleep",
                                                               "Schooling",
                                                               "Literacy",
                                                               "Values",
                                                               "Knowledge",
                                                               "Artisan skills",
                                                               "Speak native language",
                                                               "Cultural participation",
                                                               "Driglam Namzha",
                                                               "Government performance",
                                                               "Fundamental rights",
                                                               "Services",
                                                               "Political participation",
                                                               "Donations (time & money)",
                                                               "Community relationship",
                                                               "Family",
                                                               "Safety",
                                                               "Ecological issues",
                                                               "Resp. towards environment",
                                                               "Wildlife damage (rural)",
                                                               "Urbanisation issues",
                                                               "HH per capita income",
                                                               "Housing",
                                                               "Assets"
                                                             )
                              )
                            )|>
                            dplyr::arrange(dplyr::desc(b)),
                          type = "column",
                          highcharter::hcaes(x = "region_lab",
                                             y = "b",
                                             group = "ind_lab")) %>%
        highcharter::hc_plotOptions(
          column = list(stacking = stack,
                        borderWidth = 0.01),
          series = list(
            pointPadding = 0,
            groupPadding = 0.025
          )
        ) %>%
        highcharter::hc_chart(
          backgroundColor = "#ffffff"
        ) %>%
        highcharter::hc_exporting(
          enabled = TRUE,
          buttons = list(
            contextButton = list(
              menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG",
                            "downloadJPEG", "downloadSVG","separator", "downloadCSV")
            )
          ),
          filename = paste0(col_chart_title, " of indicators across dzongkhag")
        ) %>%
        highcharter::hc_title(
          text = paste0(col_chart_title, " of indicators across dzongkhag"),
          margin = 20,
          align = "left",
          style = list(color = "#000000", useHTML = TRUE)
        )  %>%
        highcharter::hc_chart(zoomType = "x") %>%
        highcharter::hc_xAxis(title = list(text = NULL),
                              scrollbar = list(enabled = T),
                               labels = list(
                                rotation = -90,
                                step = 1,
                                style = list(
                                  fontSize = 14,
                                  fontWeight = "bold",
                                  color = "#000000"
                                )
                              )) %>%
        highcharter::hc_yAxis(title = list(text = col_chart_title,
                                           style = list(
                                             fontSize = 16,
                                             fontWeight = "bold",
                                             color = "#000000"
                                           )),
                              labels = list(
                                style = list(
                                  fontSize = 14,
                                  fontWeight = "bold",
                                  color = "#000000"
                                )
                              )) %>%
        highcharter::hc_tooltip(
          pointFormat = paste("Indicator: {point.ind_lab}","<br>Measure : ",col_chart_title,"<br>Measure Value : {point.y}<br>Survey : {point.survey} <br>Survey Year : {point.year} <br>Population Share : {point.share_val}%"),
          style = list(
            fontSize = 16
          ),
          headerFormat = '<span style="font-size: 18px"><strong>{point.key}</strong></span><br/>') %>%
        highcharter::hc_colors(
          c(
            "#046c9a","#5797ba", "#91c4dc", "#cdf2ff", "#7cbf94",
            "#a6dbb7","#d0f7dc", "#51a472", "#8c8466", "#c1b896",
            "#d0e199","#c2d980", "#eaf2cb", "#ddeab2", "#c49cd3",
            "#481183","#8957aa", "#fbe6ff", "#fcc76b", "#fed38a",
            "#ffdea9","#ffeac8", "#7c1419", "#ab5c51", "#d79e92",
            "#ffe2db","#e96d4e", "#fabda3", "#ffe3d5", "#f39776",
            "#f6aead","#e8797d", "#d43d51"
          )
        )
        # ) %>%
        # highcharter::hc_caption(text = cap_charts)


    })

    output$table <- DT::renderDT({
      DT::datatable(
       gnh_data_mod_contribution_indicators_district_overview|>
          dplyr::select(b,measure_lab,share_val,region_lab,ind_lab)|>
          tidyr::pivot_wider(names_from = measure_lab,values_from = b)|>
          dplyr::relocate(region_lab,ind_lab,dplyr::everything()),
        colnames = c("Dzongkhags","Indicator" ,"Population Share (%)",
                     "Absolute contribution (%)",
                     "Relative contribution (%)"),
        filter = list(position = 'top', clear = FALSE),
        options = list(
          columnDefs = list(list(className = 'dt-center', targets = "_all")),
          autoWidth = TRUE
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: center; color: #000000;',
          htmltools::em(paste(sel_dc_ci()," of indicators"))
        )
      )
    })

  })
}

## To be copied in the UI
# mod_d_comp_contribution_indicators_ui("d_comp_contribution_indicators_1")

## To be copied in the server
# mod_d_comp_contribution_indicators_server("d_comp_contribution_indicators_1")
