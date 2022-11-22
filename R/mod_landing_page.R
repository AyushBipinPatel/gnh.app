#' landing_page UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_landing_page_ui <- function(id){
  ns <- NS(id)
  tagList(

    add_landing_page_info(),
    shiny::fluidRow(
      networkD3::sankeyNetworkOutput(outputId = ns("snake_domain_indicator"),
                                     width = "100%",
                                     height = "700px" )
    )

  )
}

#' landing_page Server Functions
#'
#' @noRd
mod_landing_page_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


    output$snake_domain_indicator <- networkD3::renderSankeyNetwork({
      networkD3::sankeyNetwork(Links = gnh_links, Nodes = gnh_nodes, Source = "source",
                    Target = "target", Value = "value", NodeID = "name",
                    fontSize = 15, nodeWidth = 50,LinkGroup = "ln_grp",sinksRight = F)
    })

  })
}

## To be copied in the UI
# mod_landing_page_ui("landing_page_1")

## To be copied in the server
# mod_landing_page_server("landing_page_1")
