#' hch_simple_column_chart
#'
#' @param data_passed A dataframe that has required columns to create the column chart
#' @param x_axis Name of the column in the dataframe as string for x axis
#' @param y_axis Name of the column in the dataframe as string for y axis
#' @param title A string for title of the chart
#' @param flname A string to save the chart
#' @param tooltip Text to be shown by tooltiop as a string
#' @param xtitle x-axis title as a string
#' @param ytitle y-axis title as a string
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd

hch_simple_column_chart <- function(data_passed,x_axis,y_axis,title,flname,tooltip,xtitle,ytitle){

  highcharter::hchart(data_passed,
                      "column",
                      highcharter::hcaes(x = .data[[x_axis]], y = .data[[y_axis]]),
                      color = "#181f86"
  ) %>%
    highcharter::hc_chart(
      backgroundColor = "#ffffff"
    ) %>%
    highcharter::hc_plotOptions(
      series = list(
        pointPadding = 0,
        groupPadding = 0.05
      )
    ) %>%
    highcharter::hc_exporting(
      enabled = TRUE,
      buttons = list(
        contextButton = list(
          menuItems = c("viewFullscreen", "printChart", "separator", "downloadPNG",
                        "downloadJPEG", "downloadSVG","separator", "downloadCSV")
        )
      ),
      filename = flname
    ) %>%
    highcharter::hc_title(
      text = title,
      margin = 20,
      align = "left",
      style = list(color = "#000000", useHTML = TRUE)
    )  %>%
    highcharter::hc_chart(zoomType = "x") %>%
    highcharter::hc_xAxis(title = list(text = xtitle),
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
    highcharter::hc_yAxis(title = list(text = ytitle,
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
      pointFormat = tooltip,
      style = list(
        fontSize = 16
      ),
      headerFormat = '<span style="font-size: 18px"><strong>{point.key}</strong></span><br/>'
    )%>%
    highcharter::hc_caption(text = NULL #cap_charts
                            )



}
