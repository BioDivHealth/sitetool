#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # App text -------------------------------------------------------------------

  output$about <- renderUI({
    file_path <- app_sys("app/www/about.md")
    includeMarkdown(file_path)
  })
  # Get landcover data ---------------------------------------------------------
  mapData <- mod_step1_server("step1_1")

  # Get input sites ------------------------------------------------------------
  sites <- mod_step2_server("step2_1", mapData$shape, mapData$lc_raster, updatedSites)

  # Plot landcover data --------------------------------------------------------
  updatedSites <- mod_step3_server("step3_1", sites, mapData$lc_raster, mapData$product)

}
