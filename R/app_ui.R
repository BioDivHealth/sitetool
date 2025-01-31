#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(

      title =  span("Site Selection Landcover Analyzer",
                    style={'padding-left:15px'}),

      theme = bslib::bs_theme(preset='pulse'),

      fillable = FALSE,

      bslib::nav_panel("Home",
                mod_step1_ui("step1_1"),

                mod_step2_ui("step2_1"),

                mod_step3_ui("step3_1")
      ),

      bslib::nav_panel(title = 'About',
                uiOutput('about'),
                tags$style(type = "text/css", ".container-fluid {padding-left:20px}")
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
    #favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ssanalyzer"
    ),
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
