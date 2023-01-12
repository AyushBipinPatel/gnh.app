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
        "The phrase ‘gross national happiness’ was first coined by the 4th King of Bhutan, King Jigme Singye Wangchuck, in 1972 when he declared, “Gross National Happiness is more important than Gross Domestic Product.” The concept implies that sustainable development should take a holistic approach towards notions of progress and give equal importance to non-economic aspects of wellbeing. The king was awarded the Blue Planet Prize in the year 2022. This prize is awarded as a recognition of scientific research and its applicaitons that have helped to provide solution for global environmental problems."
      ),
      shiny::tags$p(
        "Since then the idea of Gross National Happiness (GNH) has influenced Bhutan’s economic and social policy, and also captured the imagination of others far beyond its borders. In creating the Gross National Happiness Index, Bhutan sought to create a measurement tool that would be useful for policymaking and create policy incentives for the government, NGOs and businesses of Bhutan to increase GNH."
      ),
      shiny::tags$p(
        "The GNH Index includes both traditional areas of socio-economic concern such as living standards, health and education and less traditional aspects of culture and psychological wellbeing. It is a holistic reflection of the general wellbeing of the Bhutanese population rather than a subjective psychological ranking of ‘happiness’ alone."
      ),
      shiny::tags$h2("Construciton of the GNH Index"),
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
      ),
      shiny::tags$p(
        "There are 33 indicators in the 9 domains above and the Index seeks to measure the nation’s wellbeing directly by starting with each person’s achievements in each indicator. The GNH index is based on the Alkire-Foster method of multidimensional measurement, which has been adapted for this purpose. It identifies four groups of people – unhappy, narrowly happy, extensively happy, and deeply happy. The analysis explores the happiness people enjoy already, then focuses on how policies can increase happiness and sufficiency among the unhappy and narrowly happy people."
      )
    )
  )
}
