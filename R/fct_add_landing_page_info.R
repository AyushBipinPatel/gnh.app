#' add_landing_page_info
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd


add_landing_page_info <- function() {
  shiny::tags$div(
    shiny::tags$h1("Bhutan's Gross National Happiness Index"),
    shiny::tags$div(
      shiny::tags$p(
        "The phrase ‘gross national happiness’ was first coined by the 4th King of Bhutan, King Jigme Singye Wangchuck, in 1972 when he declared, “Gross National Happiness is more important than Gross Domestic Product.” The concept implies that sustainable development should take a holistic approach towards notions of progress and give equal importance to non-economic aspects of wellbeing."
      ),
      shiny::tags$p(
        "Since then the idea of Gross National Happiness (GNH) has influenced Bhutan’s economic and social policy, and also captured the imagination of others far beyond its borders. In creating the Gross National Happiness Index, Bhutan sought to create a measurement tool that would be useful for policymaking and create policy incentives for the government, NGOs and businesses of Bhutan to increase GNH."
      ),
      shiny::tags$p(
        "The GNH Index includes both traditional areas of socio-economic concern such as living standards, health and education and less traditional aspects of culture and psychological wellbeing. It is a holistic reflection of the general wellbeing of the Bhutanese population rather than a subjective psychological ranking of ‘happiness’ alone."
      )
    )
  )
}
