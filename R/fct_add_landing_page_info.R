#' add_landing_page_info
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


add_landing_page_info <- function() {
  shiny::tags$div(
    shiny::tags$h1("About Gross National Happiness Index"),
    shiny::tags$div(
      shiny::tags$p(
        "The development concept of Gross National Happiness (GNH) was first articulated by the Fourth King of Bhutan, His Majesty Jigme Singye Wangchuck in the mid-1970s. Questioning the then prevailing assumption that material wealth alone could  not deliver happiness and well-being, he re-defined Bhutan's developmental pathway and centralise happiness and wellbeing as the ultimate end goal of any developmental strategy. Building on this legacy, the current King has further translated GNH into policy directions to the nation's overarching, long-term strategies and five-year plans."
      ),
      shiny::tags$p(
        "GNH is conceptualised through the nine domains including psychological wellbeing, health, time use and balance, education, cultural diversity and resilience, good governance, community vitality, ecological diversity and resilience, and living standards. Together, these domains attempt to paint a multi-coloured picture of happiness and wellbeing in Bhutan."
      ),
      shiny::div(
        shiny::tags$img("Get an SVG or high res image for the mandala picture of the nine domains")
      ),
      shiny::tags$p(
        "This domain based GNH framework also provides the structure for the 33 indicators of GNH to track the country's overall progress. The Gross National Happiness Survey is carried out every five years to capture responses across these indicators from a sample of the population. Using the Alkire Foster methodology of the multidimensional poverty measure, Bhutan developed the GNH Index to provide positive policy incentives and help improve conditions of happiness and wellbeing."
      ),
      shiny::tags$p(
        "GNH Index is computed through two steps, the identification step where in happy persons are identified by distinguishing from the not-yet-happy individuals through the application of 'sufficiency thresholds' and the aggregation step, by which data on the happy persons are aggregated to create the Index. Essentially, the GNH measurement categorises persons into four groups; unhappy, narrowly happy, extensively happy, and deeply happy. The analysis explores the happiness people enjoy already, then focuses on how policies can increase happiness and sufficiency among the unhappy and narrowly happy people."
      ),
      shiny::tags$p(
        "Each of the domains are equally weighted and within domains, indicators also have relatively equal weighting. However, those indicators that have exibited higher statistical reliability and objectiveness have been given slightly higher weighting."
      ),
      shiny::tags$strong(shiny::tags$p("The GNH index includes nine domains.")),
      shiny::tags$ul(
        shiny::tags$li("Psychological wellbeing"),
        shiny::tags$li("Health"),
        shiny::tags$li("Education"),
        shiny::tags$li("Time use"),
        shiny::tags$li("Cultural diversity and resilience"),
        shiny::tags$li("Good governance"),
        shiny::tags$li("Community vitality"),
        shiny::tags$li("Ecological diversity and resilience"),
        shiny::tags$li("Living standards"),
      )
      )
    )
}
