#' step2 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_step2_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      full_screen = TRUE,
        bslib::layout_sidebar(
          sidebar = bslib::sidebar(
            ## Site Type ##
            title = 'Step 2. Generate a list of potential sites.',
            width = 350,
            selectInput(ns('selection_type'), h6('Please select a sampling procedure:'), choices = c("none", "random", "populated places" = "village")),

            # Params for random type
            conditionalPanel(
              ns=NS(id),
              condition = "input.selection_type == 'random'",
              fluidRow(
                column(4, tags$label("Number of sites:")),
                column(8, numericInput(ns('num_sites_random'), label = NULL, value = 100, min = 1, max = 1000000, step = 1, width = "100%"))
              ),
              fluidRow(
                column(4, tags$label("Min. distance between sites (m):")),
                column(8, numericInput(ns('dist_site'), label = NULL, value = 0, min = 0, step = 1, width = "100%"))
              ),
              fluidRow(
                column(4, tags$label("Min. distance from city (km):")),
                column(8, numericInput(ns('dist_city'),  label = NULL, value = 0, min = 0, max = 10, step = 0.1, width = "100%"))
              ),
              fluidRow(
                column(4, tags$label("Min. distance from main road (m):")),
                column(8, numericInput(ns('dist_road'), label = NULL, value = 0, min = 0, max = 2000, step = 1, width = "100%"))
              ),
              helpText(tags$small("Enter exact minimum thresholds for city (km) and road (m) filters."))
            ),

            # Params for village type
            conditionalPanel(
              ns=NS(id),
              condition = "input.selection_type == 'village'",
              fluidRow(
                # --- Selection mode row ---
                column(
                  12,
                  radioButtons(
                    ns("site_selection_mode"),
                    label = NULL,
                    choices = c("All Sites" = "all", "Random Subset" = "subset"),
                    selected = "all",
                    inline = TRUE
                  )
                ),

                # --- Number of sites row ---
                column(
                  4,
                  tags$label("Number of sites:")
                ),
                column(
                  8,
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'subset'", ns("site_selection_mode")),
                    numericInput(
                      ns("num_sites_village"),
                      label = NULL,
                      value = 100,
                      min = 1,
                      max = 1000000,
                      step = 1,
                      width = "100%"
                    )
                  ),
                  conditionalPanel(
                    condition = sprintf("input['%s'] == 'all'", ns("site_selection_mode")),
                    textOutput(ns("siteCount"))
                  )
                )
              )
              ),


            ## Input Sites ##

            radioButtons(ns('input_sites'), h6("Please input any selected sites:"),
                         choices = c("None" = "none",
                                     "Select on map" = "add_points_mode",
                                     "Upload a CSV" = "csv"

                         )),
            conditionalPanel(
              ns = NS(id),
              condition = "input.input_sites == 'csv'",
              fileInput(ns("csvfile"),
                        label = h6("Upload a CSV of potential sites:"),
                        accept = c(".csv"))
            ),
            uiOutput(ns("goStep2_ui")),
            uiOutput(ns("saveFile_ui"))
          ),
        mod_core_mapping_ui(ns("core_mapping_2")),
        div(
          style = "display: flex; align-items: center; gap: 15px;",

          shinyWidgets::switchInput(
            inputId = ns("toggle_raster"),
            label = "Show Raster",
            onLabel = "On",
            offLabel = "Off",
            value = TRUE,
            labelWidth = "100px"
          ),

          actionButton(ns("clear_generated"), "Clear Generated Points", icon = icon("eraser")),
          actionButton(ns("clear_selected"), "Clear Selected Points", icon = icon("eraser"))
        )
      )
    )
  )
}

#' step2 Server Functions
#'
#' @noRd
mod_step2_server <- function(id, shape, lc_raster, updatedSites) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mapvals <- reactiveValues(sites=NULL, bbox=NULL, raster=NULL, sf=NULL, draw=FALSE, add_points=FALSE)

    mod_core_mapping_server("core_mapping_2", mapvals)

    # add inputs
    observe({
      mapvals$raster <- lc_raster()
      mapvals$sf <- shape()
    })

    observeEvent(updatedSites(), {
      req(updatedSites())

      site_list <- updatedSites() %>%
        dplyr::select(site, site_id, input_site, longitude, latitude) %>%
        dplyr::distinct() %>%
        sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

      current_sites <- isolate(mapvals$sites)

      if (is.null(current_sites) || !identical(site_list, current_sites)) {
        mapvals$sites <- site_list
      }
    }, ignoreNULL = TRUE)

    observeEvent(input$input_sites, {
      if(input$input_sites == 'add_points_mode'){
        mapvals$add_points = TRUE
      } else{
        mapvals$add_points = FALSE
      }
    })

    # Reset sites when selection type or bbox changes
    observeEvent(mapvals$bbox, {
      mapvals$sites <- NULL
    })

    is_positive_integer <- function(x) {
      !is.null(x) && !is.na(x) && is.finite(x) && x >= 1 && x <= 1000000 && x == floor(x)
    }

    is_non_negative_number <- function(x, max = Inf) {
      !is.null(x) && !is.na(x) && is.finite(x) && x >= 0 && x <= max
    }

    has_sites <- reactive({
      !is.null(mapvals$sites) && nrow(mapvals$sites) > 0
    })

    step2_inputs_valid <- reactive({
      req(input$selection_type)

      if (is.null(mapvals$sf)) {
        return(FALSE)
      }

      if (input$selection_type == 'random') {
        return(
          is_positive_integer(input$num_sites_random) &&
            is_non_negative_number(input$dist_site) &&
            is_non_negative_number(input$dist_city, max = 10) &&
            is_non_negative_number(input$dist_road, max = 2000)
        )
      }

      if (input$selection_type == 'village') {
        req(input$site_selection_mode)
        if (input$site_selection_mode == 'subset') {
          return(is_positive_integer(input$num_sites_village))
        }
        return(TRUE)
      }

      if (input$selection_type == 'none') {
        return(has_sites())
      }

      FALSE
    })

    output$goStep2_ui <- renderUI({
      if (isTRUE(step2_inputs_valid())) {
        actionButton(ns("goStep2"), "Go")
      } else {
        tags$button(
          type = "button",
          class = "btn btn-default action-button",
          style = "width: 100%;",
          disabled = "disabled",
          "Go"
        )
      }
    })

    output$saveFile_ui <- renderUI({
      if (isTRUE(has_sites())) {
        downloadButton(ns("saveFile"), "Save List of Sites")
      } else {
        tags$button(
          type = "button",
          class = "btn btn-default",
          style = "width: 100%;",
          disabled = "disabled",
          shiny::icon("download"),
          " Save List of Sites"
        )
      }
    })

    observeEvent(input$clear_generated, {
      mapvals$sites <- mapvals$sites%>%
        dplyr::filter(input_site == TRUE)
    })

    observeEvent(input$clear_selected, {
      mapvals$sites <- mapvals$sites%>%
        dplyr::filter(input_site == FALSE)
    })

    observeEvent(input$toggle_raster, {
        if(input$toggle_raster){
          mapvals$raster = lc_raster()
        }
      else{
        mapvals$raster = NULL
      }
    })

    # Output number of sites (including input sites)
    output$siteCount <- renderText({
      req(mapvals$sites)
      nrow(mapvals$sites)
    })

    output$selectSites <- DT::renderDT({
      req(mapvals$sites)
      mapvals$sites %>%
        dplyr::filter(input_site == TRUE) %>%
        sf::st_drop_geometry()
    })

    # Handle uploaded CSV file
    observeEvent(input$csvfile, {
      req(input$csvfile)
      tryCatch({
        uploaded_data <- read.csv(input$csvfile$datapath)

        if(any(!c('site','longitude','latitude') %in% colnames(uploaded_data))) stop("incorrect columns")

        # Convert to sf with geometry, add input_site TRUE and site_id
        id <- uploaded_data %>%
          dplyr::mutate(input_site = TRUE,
                        site_id = paste0('input_', dplyr::row_number())) %>%
          sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
          dplyr::select(site, site_id, input_site)

        if(!is.null(mapvals$sites)){
          mapvals$sites <- rbind(id, mapvals$sites)
        } else {
          mapvals$sites <- id
        }

      }, error = function(e) {
        print(e)
        showNotification("Upload file is of incorrect type. Please upload a CSV with columns labeled site, longitude, and latitude.", type = "error")
        return(NULL)
      })
    })

    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("site_list.csv")
      },
      content = function(file){
        req(mapvals$sites)
        outfile <- mapvals$sites %>%
          dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                        latitude = sf::st_coordinates(.)[,2]) %>%
          sf::st_drop_geometry()

        write.csv(outfile, file, row.names = FALSE)
      }
    )

    # Generate sites on goStep2 click
    observeEvent(input$goStep2, {
      req(mapvals$sf)

      sites_filter <- NULL
      if(input$selection_type == 'village'){
          sites_filter <- get_village_points(mapvals$sf, in_app = TRUE)
          if (is.null(sites_filter)) {
            return(NULL)
          }
          if(input$site_selection_mode == 'subset'){
            req(input$num_sites_village)

            n_requested <- as.numeric(input$num_sites_village)
            n_available <- nrow(sites_filter)
            if (is.na(n_requested) || n_requested <= 0) {
              showNotification("Please enter a valid positive number of sites.", type = "error")
              return(NULL)
            }

            if (n_requested > n_available) {
              showNotification(
                sprintf(
                  "Requested %d sites, but only %d available. Selecting all sites instead.",
                  n_requested, n_available
                ),
                type = "warning"
              )
            } else {
              sites_filter <- sites_filter[sample(seq_len(n_available), n_requested), , drop = FALSE]
              showNotification(
                sprintf("Randomly selected %d of %d available sites.", n_requested, n_available),
                type = "message"
              )
            }
          }
      } else if(input$selection_type == 'random'){
        req(input$num_sites_random)
        sites_filter <- get_random_points(mapvals$sf,
                                          as.numeric(input$num_sites_random),
                                          as.numeric(input$dist_site),
                                          as.numeric(input$dist_road),
                                          as.numeric(input$dist_city) * 1000,
                                          in_app = TRUE)
      } else if (input$selection_type == 'none') {
        if (!isTRUE(has_sites())) {
          showNotification("Please provide selected sites or choose a sampling procedure.", type = "error")
          return(NULL)
        }
        mapvals$sites <- mapvals$sites %>%
          dplyr::mutate(site = as.character(site))
        return(NULL)
      }

      if (is.null(sites_filter)) {
        return(NULL)
      }

      sites_filter <- sites_filter %>%
        dplyr::mutate(site = as.character(site))  # coerce to character

      if (!is.null(mapvals$sites) && nrow(mapvals$sites) > 0) {
        # Also coerce mapvals$sites columns to same type if needed
        mapvals$sites <- mapvals$sites %>%
          dplyr::mutate(site = as.character(site))

        combined_sites <- dplyr::bind_rows(mapvals$sites %>% dplyr::filter(input_site == TRUE),
                                           sites_filter)
      } else {
        combined_sites <- sites_filter
      }


      mapvals$sites <- combined_sites
    })

    # Return reactive sites for upstream use if needed
    return(reactive(mapvals$sites))
  })

}

## To be copied in the UI
# mod_step2_ui("step2_1")

## To be copied in the server
# mod_step2_server("step2_1")
