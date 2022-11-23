#' hch_map
#'
#' @param passed_data The data frame with values to be represented on the map
#' @param catch_sel_measure The name of the measure to be presented
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


hch_map <- function(passed_data,catch_sel_measure){



  # create a data class for the hc_* function

  ### create data class

  dta_class <- dplyr::tibble(
    name = passed_data %>%
      dplyr::mutate(
        discrete_grps = ggplot2::cut_interval(x = b,n = 7)
      ) %>%
      dplyr::pull(discrete_grps) %>%
      levels(),
    from = c(1:7),
    to= c(2:(7+1))
  ) %>%
    highcharter::list_parse()


  # The map
  highcharter::highchart() %>%
    highcharter::hc_add_series_map(
      map = list(
        type = btn_poly_json[[1]],
        features = btn_poly_json[[4]]
      ),
      df = passed_data %>%
        dplyr::mutate(
          discrete_grps = ggplot2::cut_interval(x = b,n = 7),
          discrete_grps = as.numeric(discrete_grps)
        ) ,
      value = "discrete_grps",
      name= NULL,
      joinBy =  c("region_shp","region_lab"),
      nullColor = "#DDDDDD",
      borderColor = "#FFFFFF"
    ) %>%
    highcharter::hc_chart(
      backgroundColor = "#ffffff"
    ) %>%
    highcharter::hc_colorAxis(
      dataClassColor = "category",
      dataClasses = dta_class
    ) %>%
    highcharter::hc_tooltip(
      pointFormat = paste("Region: <b>{point.region_lab}</b><br>","Population Share of {point.region_lab}: {point.share_val:.2f}%<br>",catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}"),
      headerFormat = paste('<span style="font-size: 18px"><b>',catch_sel_measure,'</b></span><br>'),
      style = list(
        fontSize = 16
      )
    ) %>%
    highcharter::hc_colors(
      colors = c("#b0d9cb",
                 "#abcd72",
                 "#e2d200",
                 "#fdd262",
                 "#f18b00",
                 "#cb1724",
                 "#5b1a18")

    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG",
                        "downloadJPEG", "downloadSVG","separator", "downloadCSV")
        )
      ),
      filename = paste(catch_sel_measure,": {point.b}<br>Survey : {point.survey}<br>Survey Year: {point.year}")
    ) %>%
    highcharter::hc_mapNavigation(
      enabled = TRUE,
      enableMouseWheelZoom = TRUE,
      enableDoubleClickZoom = TRUE
    ) %>%
    #highcharter::hc_caption(text = cap_charts) %>%
    highcharter::hc_title(
      text = paste("Bhutan:",catch_sel_measure,sep = " "),
      margin = 20,
      align = "left",
      style = list(color = "#000000", useHTML = TRUE)
    )

}
