#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  output$about <- renderUI({
    file_path <- app_sys("app/www/about.Rmd")
    html_content <- rmarkdown::render(
      input = file_path,
      output_format = rmarkdown::html_fragment(),
      quiet = TRUE
    )

    # Read the rendered HTML and convert to HTML for Shiny
    htmltools::HTML(paste(readLines(html_content), collapse = "\n"))
  })
  # Get landcover data ---------------------------------------------------------
  mapData <- mod_step1_server("step1_1")

  # Get input sites ------------------------------------------------------------
  sites <- mod_step2_server("step2_1", mapData$shape, mapData$lc_raster, updatedSites)

  # Plot landcover data --------------------------------------------------------
  updatedSites <- mod_step3_server("step3_1", sites, mapData$lc_raster, mapData$product)

}
