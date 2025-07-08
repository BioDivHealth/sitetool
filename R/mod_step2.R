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
      bslib::navset_card_tab(
        sidebar = bslib::sidebar(

          ## Site Type ##
          title = 'Step 2. Generate a list of potential sites.',
          width = 350,
          selectInput(ns('selection_type'), h6('Please select a sampling procedure:'), choices = c("random", "village")),

          # Params for random type
          conditionalPanel(
            ns=NS(id),
            condition = "input.selection_type == 'random'",
            fluidRow(
              column(4, tags$label("Number of sites:")),
              column(8, textInput(ns('num_sites'), label = NULL, value = 100, placeholder = "Enter number of sites"))
            ),
            fluidRow(
              column(4, tags$label("Distance from city (km)")),
              column(8, sliderInput(ns('dist_city'),  label = NULL, min = 0, max = 10, value = 0))
            ),
            fluidRow(
              column(4, tags$label("Distance from main road (m)")),
              column(8, sliderInput(ns('dist_road'), label = NULL, min = 0, max = 2000, value = 0))
            ),
          ),

          # Params for village type
          conditionalPanel(
            ns=NS(id),
            condition = "input.selection_type == 'village'",
            fluidRow(
              column(4, tags$label('Number of sites:')),
              column(8, textOutput(ns("siteCount")))
            )
            # fluidRow(
            #   column(4, tags$label("Max city population")),
            #   column(8, sliderInput(ns('city_pop'),
            #                         label = NULL,
            #                         min = 0,
            #                         max = 1000000,
            #                         step = 100,
            #                         value = 10000)
            #   )
            # )
          ),

          ## Input Sites ##

          radioButtons(ns('input_sites'), h6("Please input any selected sites:"),
                       choices = c("None" = "none",
                                   "Upload a CSV" = "csv",
                                    "Select on map" = "add_points_mode"
                       )),
          conditionalPanel(
            ns = NS(id),
            condition = "input.input_sites == 'csv'",
            fileInput(ns("csvfile"),
                      label = h6("Upload a CSV of potential sites:"),
                      accept = c(".csv"))
          ),
          actionButton(ns("goStep2"), "Go"),
          downloadButton(ns("saveFile"), "Save List of Sites")
        ),
        bslib::nav_panel(
          title = 'Map',
          mod_core_mapping_ui(ns("core_mapping_2"))
        ),
        bslib::nav_panel(
          title='Selected Sites',
          DT::dataTableOutput(ns("selectSites"))
        )
      )
      )
    )
}

#' step2 Server Functions
#'
#' @noRd
mod_step2_server <- function(id, shape, lc_raster) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mapvals <- reactiveValues(sites=NULL, bbox=NULL, raster=NULL, sf=NULL, draw=FALSE, add_points=FALSE)

    mod_core_mapping_server("core_mapping_2", mapvals)

    # add inputs
    observe({
      mapvals$raster <- lc_raster()
      mapvals$sf <- shape()
    })

    observeEvent(input$input_sites == 'add_points_mode', {
      mapvals$add_points = TRUE
    })

    # Reset sites when selection type or bbox changes
    observeEvent(list(input$selection_type, mapvals$bbox), {
      mapvals$sites <- NULL
    })

    # Validate num_sites input
    observeEvent(input$num_sites, {
      req(input$num_sites)
      validate_text_input(input$num_sites, session, "num_sites", 1000000)
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
      } else if(input$selection_type == 'random'){
        req(input$num_sites)
        sites_filter <- get_random_points(mapvals$sf,
                                          as.numeric(input$num_sites),
                                          as.numeric(input$dist_road),
                                          as.numeric(input$dist_city),
                                          in_app = TRUE)
      }

      # Add uploaded input sites if any
      if(!is.null(mapvals$sites)){
        input_sites_df <- mapvals$sites %>%
          dplyr::filter(input_site == TRUE)
      } else {
        input_sites_df <- NULL
      }

      if(!is.null(input_sites_df) && nrow(input_sites_df) > 0){
        sites_filter <- rbind(input_sites_df, sites_filter)
      }

      mapvals$sites <- sites_filter
    })

    # Return reactive sites for upstream use if needed
    return(reactive(mapvals$sites))
  })

}

## To be copied in the UI
# mod_step2_ui("step2_1")

## To be copied in the server
# mod_step2_server("step2_1")
