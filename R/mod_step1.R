#' step1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_step1_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::layout_sidebar(
        class="p-3 border border-top-0 rounded-bottom",
        sidebar = bslib::sidebar(
          title = "Step 1. Select a region of interest and landcover data source.",
          width = 350,

          ## Input Type ##
          radioButtons(ns('map_draw'), h6('Please select a region of interest using:'),
                       choices = c("Box tool on map" = 'map',
                                   "Bounding box coordinates" = 'bbox',
                                   "Upload a shapefile" = 'sf'
                       )
          ),


          conditionalPanel(
            ns=NS(id),
            condition = "input.map_draw == 'bbox'",
            textInput(ns('bbox_coords'),  h6("Enter coordinates separated by commas (xmin, ymin, xmax, ymax):"))
          ),

          conditionalPanel(
            ns=NS(id),
            condition = "input.map_draw == 'map'",
            h6('Bounding box coordinates:'),
            textOutput(ns("boundingBoxCoords"))
          ),

          conditionalPanel(
            ns=NS(id),
            condition = "input.map_draw == 'sf'",
            fileInput(ns("shapefile"), h6("Upload a GEOJSON shapefile, with a single boundary."),
                      accept=c('.geojson'), multiple=F)
          ),

          ## Input Type ##
          radioButtons(ns('upload'), h6('Please select a raster data source:'),
                       choices = c("Default" = FALSE, "Upload a file" = TRUE)),

          conditionalPanel(
            ns=NS(id),
            condition = "input.upload == 'TRUE'",
            fileInput(ns("rastfile"), h6("Upload a GeoTIFF covering your region of interest:"), accept = c("tif", ".tiff")),
            selectInput(ns('product'),
                        'Landcover Product:',
                        choices = c('Copernicus Global Land Cover', 'ESA WorldCover',  'Dynamic World', 'Climate/NDVI/DEM' = 'Climate/NDVI'))
          ),

          conditionalPanel(
            ns=NS(id),
            condition = "input.upload == 'FALSE'",
            selectInput(ns('product'),
                        'Landcover Product:',
                        choices = c('Copernicus Global Land Cover'))
          ),


          actionButton(ns("goStep1"), "Go"),
          downloadButton(ns("saveFile"), "Save Raster")
        ),
        mod_core_mapping_ui(ns("core_mapping_1"))
      )
    )
  )
}

#' step1 Server Functions
#'
#' @noRd
mod_step1_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    # Reactive values  ---------------------------------------------------------
    sites <- reactiveVal(NULL)
    input_sites <- reactiveVal(NULL)
    mapvals <- reactiveValues(sf = NULL, draw=FALSE, raster=NULL)

    # Base Map
    mod_core_mapping_server("core_mapping_1", mapvals)

    # Clear map items with change of input
    observeEvent(input$map_draw, {
      mapvals$raster = NULL
      mapvals$draw = ifelse(input$map_draw == 'map', TRUE, FALSE)
    })

    observeEvent(mapvals$sf, {
      mapvals$raster = NULL
    })

    # Get ROI --------- --------------------------------------------------------
    observeEvent(input$bbox_coords, {
      # get coords from output
      if (!(mapvals$draw)) {
        if (!is.null(input$bbox_coords) && nchar(input$bbox_coords) > 0) {
          bbox <- as.numeric(strsplit(input$bbox_coords, ",")[[1]])

          # Ensure bbox has exactly 4 values: xmin, ymin, xmax, ymax
          if(!anyNA(bbox)) {
            if(length(bbox) == 4) {
              mapvals$sf = sf::st_as_sfc(sf::st_bbox(c(xmin=bbox[1],
                                                       ymin=bbox[2],
                                                       xmax=bbox[3],
                                                       ymax=bbox[4]),
                                                     crs=sf::st_crs(4326)))%>%
                sf::st_as_sf()
            }
          }
        }
      }
      else(mapvals$sf = NULL)
    })

    observeEvent(input$shapefile, {
      # Check if either shapefile or raster file is provided
      if (is.null(input$shapefile) && is.null(input$rastfile$datapath)) {
        showNotification("Please ensure the shapefile was correctly uploaded.", type = "error")
        return()
      }

      shape = sf::read_sf(input$shapefile$datapath)

      # Check if the object is a valid sf object
      if (!("sf" %in% class(shape))) {
        showNotification("The uploaded file is not a valid shapefile.", type = "error")
        return()
      }

      # Check if the object is a valid sf object
      if (!sf::st_is_valid(shape)) {
        showNotification("The geometry of the shape is not valid, please try again.", type = "error")
        return()
      }

      # Check if the geometry type is polygon
      if (!("POLYGON" %in% sf::st_geometry_type(shape))) {
        showNotification("Please ensure a single polygon is uploaded.", type = "error")
        return()
      }

      mapvals$sf = shape
    })

    output$boundingBoxCoords <- renderText({
      if(!is.null(mapvals$sf)){
        paste(sf::st_bbox(mapvals$sf), collapse=', ')
      }
    })

    observeEvent(input$goStep1, {
      req(mapvals$sf)

      # Add buffer for edge sites
      bbox_sf = mapvals$sf%>%sf::st_buffer(2)

      # Load raster:
      if (input$upload) {
        withProgress(message = 'Uploading raster', value = 0, {
          tryCatch({
            # Check if file is uploaded
            if (is.null(input$rastfile) || is.null(input$rastfile$datapath)) {
              stop("No file uploaded. Please upload a valid raster file.")
            }

            # Try to load the raster
            tryCatch({
              r <- terra::rast(input$rastfile$datapath)

            }, error = function(e) {
              showNotification("Not a suppported file format.", type = "error")
              return(NULL)
            })

            # Check if CRS is assigned
            if (is.na(terra::crs(r))) {
              stop("Please ensure the raster has an assigned coordinate reference system (CRS).")
            }

            # Reproject raster if necessary
            if (!terra::same.crs(r, "+proj=longlat")) {
              incProgress(1/2, detail = paste("Reprojecting raster"))
              r <- terra::project(r, 'EPSG:4326')
            }

            # Check if bounding box overlaps raster
            if (!(terra::relate(terra::ext(bbox_sf), terra::ext(r), relation = 'within'))) {
              stop("The uploaded raster and the bounding box do not overlap. Please ensure they cover the same region.")
            }

            # Crop raster to bounding box
            r <- terra::crop(r, bbox_sf)

            # Assign levels for raster categories if applicable
            if (input$product != 'Climate/NDVI') {
              tryCatch({
                levels(r) <- raster_cats %>%
                  subset(product == input$product) %>%
                  dplyr::select(c(value, subcover))
              }, error = function(e) {
                showNotification("Failed to assign levels to the raster. Please check the category data.", type = "error")
              })
            }

            # If everything is successful:
            mapvals$raster = r
            showNotification("Raster successfully uploaded.", type = "message")

          }, error = function(e) {
            showNotification(e$message, type = "error")
            return(NULL)
          })
        })

      } else {
        withProgress(message = 'Downloading landcover data', value = 0, {
          r <- get_landuse(bbox_sf, inapp=T)

          tryCatch({
            levels(r) <- raster_cats %>%
              subset(product == input$product) %>%
              dplyr::select(c(value, subcover))

            mapvals$raster = r
          }, error = function(e) {
            showNotification("Failed to download data, please check your internet connection or select a smaller area.", type = "error")
          })
        })
      }

    })

    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("landcover_raster.tif")
      },
      content = function(file){
        terra::writeRaster(mapvals$raster, file, overwrite=T)
      }
    )

    # Return Items
    list(
      shape = reactive(mapvals$sf),
      lc_raster = reactive(mapvals$raster),
      product = reactive(input$product)
    )
  })
}

## To be copied in the UI
# mod_step1_ui("step1_1")

## To be copied in the server
# mod_step1_server("step1_1")
