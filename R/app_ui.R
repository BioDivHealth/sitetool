#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
      bslib::page_navbar(
        title = tags$div(
          style = "display: flex; align-items: center; flex-wrap: nowrap; min-width: 300px;",
          tags$img(
            src = "www/SiteTool_HexLogo.png",
            height = "60px",
            style = "margin-right: 15px;"
          ),
          tags$span(
            "Site Selection Tool",
            style = "font-weight: 600; font-size: 1.5rem; white-space: nowrap;"
          )
        ),

        theme = bslib::bs_theme(preset = 'pulse'),
        fillable = FALSE,
        bslib::nav_panel(
          title = "Instructions",
          uiOutput('about'),
          tags$style(type = "text/css", ".container-fluid {padding-left:20px}")
        ),
        bslib::nav_panel("Tool",
                         mod_step1_ui("step1_1"),
                         mod_step2_ui("step2_1"),
                         mod_step3_ui("step3_1")
        )
      )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext='png'),
    tags$link(rel = "icon", type = "image/png", href = "SiteTool_HexLogo.png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "Site Selection Tool"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
