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
      full_screen = TRUE,
      bslib::layout_sidebar(
        class="p-3 border border-top-0 rounded-bottom",
        sidebar = bslib::sidebar(
          title = "Step 1. Select a region of interest and raster data source.",
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
          selectInput(ns('product'),
                      h6('Please select a raster data source:'),
                      choices = c('SRTM Elevation', 'ESA WorldCover', 'Human Footprint 2009', 'Upload a file')
          ),

          conditionalPanel(
            ns=NS(id),
            condition = "input.product == 'Upload a file'",
            fileInput(ns("rastfile"), h6("Upload a GeoTIFF covering your region of interest:"))
           # textInput(ns('rastName'), h6('Please add a name for your uploaded file:'))
          ),

          actionButton(ns("goStep1"), "Add Raster"),
         # downloadButton(ns("saveFile"), "Save Raster"),
          actionButton(ns('clearRast'), "Clear All Rasters")
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

    observeEvent(input$clearRast, {
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
      if (!all(sf::st_is_valid(shape))) {
        showNotification("The geometry of the shape is not valid, please try again.", type = "error")
        return()
      }

      # Check if the geometry type is polygon
      if (!any(sf::st_geometry_type(shape) %in% c("POLYGON", "MULTIPOLYGON"))){
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
      bbox_sf <- mapvals$sf %>% sf::st_buffer(2)

      # Load raster:
      if (input$product == 'Upload a file') {
        withProgress(message = 'Uploading raster', value = 0, {
          tryCatch({

            # ---- 1. Check upload ----
            if (is.null(input$rastfile) || is.null(input$rastfile$datapath)) {
              stop("No file uploaded. Please upload a valid raster file.")
            }

            max_size_mb <- 100  # lower for shinyapps.io free tier
            file_size_mb <- file.info(input$rastfile$datapath)$size / (1024^2)
            if (file_size_mb > max_size_mb) {
              showNotification(
                paste0("File too large (", round(file_size_mb, 1), " MB). ",
                       "Please upload a file smaller than ", max_size_mb, " MB."),
                type = "error", duration = NULL
              )
              return(NULL)
            }

            # ---- 2. Load raster (lazy pointer, not full read) ----
            r <- tryCatch({
              terra::rast(input$rastfile$datapath)
            }, error = function(e) {
              showNotification("Not a supported file format.", type = "error")
              return(NULL)
            })

            if (is.null(r)) return(NULL)

            # ---- 3. CRS checks ----
            if (is.na(terra::crs(r))) {
              stop("Please ensure the raster has an assigned coordinate reference system (CRS).")
            }

            # ---- 4. Reproject on disk ----
            if (!terra::same.crs(r, "+proj=longlat")) {
              incProgress(1/2, detail = "Reprojecting raster to EPSG:4326")
              r <- terra::project(r, "EPSG:4326",
                                  filename = tempfile(fileext = ".tif"),
                                  overwrite = TRUE)
            }

            # ---- 5. Check bounding box overlap ----
            if (!(terra::relate(terra::ext(bbox_sf), terra::ext(r), relation = "within"))) {
              stop("The uploaded raster and the bounding box do not overlap. Please ensure they cover the same region.")
            }

            # ---- 6. Crop on disk ----
            r <- terra::crop(r, bbox_sf,
                             filename = tempfile(fileext = ".tif"),
                             overwrite = TRUE)

            # ---- 7. Downsample if too big ----
            max_cells <- 4e6  # adjust as needed
            if (terra::ncell(r) > max_cells) {
              fact <- ceiling(sqrt(terra::ncell(r) / max_cells))
              r <- terra::aggregate(r, fact = fact,
                                    filename = tempfile(fileext = ".tif"),
                                    overwrite = TRUE)
              showNotification("Raster was downsampled to reduce memory use.", type = "warning")
            }

            # ---- 8. Handle color table safely ----
            coltab <- tryCatch(terra::coltab(r)[[1]], error = function(e) NULL)
            if (!is.null(coltab)) {
              coltab <- coltab[!(coltab$red == 0 & coltab$green == 0 & coltab$blue == 0), ]
              levs <- data.frame(
                value = coltab$value,
                cover = if ("label" %in% names(coltab)) coltab$label else as.character(coltab$value)
              )
              levels(r) <- levs
            }

            # ---- 9. Store raster in reactive vals ----
            if (is.null(mapvals$raster)) {
              mapvals$raster <- list()
            }
            mapvals$raster[[input$rastfile$name]] <- r

            showNotification("Raster successfully uploaded.", type = "message")

            # ---- 10. Cleanup ----
            gc()

          }, error = function(e) {
            showNotification(e$message, type = "error")
            return(NULL)
          })
        }) # <- closes withProgress for upload
      } else {
        withProgress(message = 'Downloading raster data', value = 0, {
          if(input$product == 'ESA WorldCover'){
            r <- get_worldcover(bbox_sf, inapp=T, tile_limit = 4)

          }
          if (input$product == 'SRTM Elevation') {
            r <- download_elevation(bbox_sf, inapp = TRUE)
          }
          if (input$product == 'Human Footprint 2009') {
            r <- download_footprint(bbox_sf, inapp = TRUE)
          }

          if (is.null(r)) {
            showNotification("Failed to download data, please check your internet connection or select a smaller area.",
                             type = "error")
          }

          if (is.null(mapvals$raster)) {
            mapvals$raster <- list()
          }

          # use input$product as the name
          mapvals$raster[[input$product]] <- r
        }) # <- closes withProgress for download
      }

    }) # <- closes observeEvent


    output$saveFile <- downloadHandler(
      filename = function() {
        paste0(names(mapvals$raster)[input$layer], ".tif")
      },
      content = function(file) {
        writeRaster(mapvals$raster[[input$layer]],
                    filename = file,
                    filetype = "GTiff",
                    overwrite = TRUE)
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
