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
          selectInput(ns('selection_type'), h6('Please select a type of site:'), choices = c("random", "village")),

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
          radioButtons(ns('input_sites'), h6('Input Sites'),
                       choices = c("None" = "none",
                                   "Upload a CSV" = "csv"
                                   # "Select on map" = "map"
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
mod_step2_server <- function(id, shape, lc_raster){

  moduleServer(id, function(input, output, session){
    ns <- session$ns
    sites <- reactiveVal(NULL)
    input_sites <- reactiveVal(NULL)
    mapvals <- reactiveValues(sites=NULL, bbox=NULL, raster=NULL, sf=NULL, draw=FALSE)

    mod_core_mapping_server("core_mapping_2", mapvals)

    observe({
      mapvals$raster = lc_raster()
      mapvals$sf = shape()
    })

    observeEvent(list(input$selection_type, mapvals$bbox), {
      mapvals$sites = NULL
    })

    # Error checking user input
    observeEvent(input$num_sites, {
      req(input$num_sites)
      validate_text_input(input$num_sites, session, "num_sites", 1000000)
    })

    # # Output number of potential sites (including input sites)
    output$siteCount <- renderText({
      nrow(sites())
    })

    output$selectSites <- DT::renderDT({
      input_sites()
    })

    # Check uploaded data ------------------------------------------------------

    observeEvent(input$csvfile, {
      req(input$csvfile)
      tryCatch({
        uploaded_data = read.csv(input$csvfile$datapath)

        # Check if the uploaded data is a dataframe
        if (!is.data.frame(uploaded_data)) {
          stop("wrong file")
        }

        # Check for correct columns
        if (any(!c('site', 'longitude', 'latitude') %in% colnames(uploaded_data))){
          stop("incorrect columns")
        }

        input_sites(uploaded_data)
      },  error = function(e) {
        showNotification("Upload file is of incorrect type. Please upload a CSV with columns labeled site, longitude, and latitude.", type = "error")
        return(NULL)
      })
    })

    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("site_list.csv")
      },
      content = function(file){
        outfile = sites()%>%
          dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          sf::st_drop_geometry()

        write.csv(outfile, file, row.names = F)
      }
    )

    # Step 2: Get points  ------------------------------------------------------
    observeEvent(input$goStep2, {
      req(mapvals$sf)

      # Download sites within the bounding box
      if(input$selection_type == 'village'){
        sites_filter = get_village_points(mapvals$sf,
                                          #as.numeric(input$city_pop)*1e3,
                                          in_app=T)
      }

      if(input$selection_type == 'random'){
        req(input$num_sites)


        sites_filter = get_random_points(mapvals$sf,
                                   as.numeric(input$num_sites),
                                   as.numeric(input$dist_road),
                                   as.numeric(input$dist_city),
                                   in_app=T)
      }

      if(!is.null(input_sites())){
        id = input_sites()
        id$input_site= TRUE
        id$site_id = paste0('input_',1:nrow(id))
        id = sf::st_as_sf(id,
                          coords = c("longitude", "latitude"),
                          crs = "epsg:4326")%>%
          dplyr::select(c(site, site_id, input_site))

        sites_filter = rbind(id, sites_filter)
      }

      # store and return sites
      mapvals$sites = sites_filter
      sites(sites_filter)
    })
    return(reactive(sites()))
  })
}

## To be copied in the UI
# mod_step2_ui("step2_1")

## To be copied in the server
# mod_step2_server("step2_1")
