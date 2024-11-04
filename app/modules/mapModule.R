# Returns a list of sites based user input parameters

# UI ---------------------------------------------------------------------------
mapModuleUI <-  function(id){
     ns <- NS(id)
     card(
       layout_sidebar(
         class="p-3 border border-top-0 rounded-bottom",
         sidebar = sidebar(
           title = "Step 1. Select a region of interest and input landcover data.",
           width = 350,
           
           ## Input Type ## 
           radioButtons(ns('map_draw'), h6('Please select a region of interest using:'),
                        choices = c("Box tool on map" = TRUE, "Bounding box coordinates" = FALSE)),
           
           
           conditionalPanel(
             ns=NS(id),
             condition = "input.map_draw == 'FALSE'",
             textInput(ns('bbox_coords'),  h6("Enter coordinates separated by commas (xmin, ymin, xmax, ymax):"))
           ),
           
           conditionalPanel(
             ns=NS(id),
             condition = "input.map_draw == 'TRUE'",
             h6('Bounding box coordinates:'), 
             textOutput(ns("boundingBoxCoords")),
           ),
           
           
           checkboxInput(ns('upload'), 'Upload landcover data?', value = FALSE),
           
           conditionalPanel(
             ns=NS(id),
             condition = "input.upload",
             fileInput(ns("rastfile"), h6("Upload a GeoTIFF covering your region of interest:"), accept = c("tif", ".tiff")),
           ),
           
           conditionalPanel(
             ns=NS(id),
             condition = "input.upload",
             selectInput(ns('product'),
                         'Landcover Product:',
                         choices = c('Copernicus Global Land Cover', 'ESA WorldCover',  'Dynamic World', 'NDVI'))
           ),
           
           conditionalPanel(
             ns=NS(id),
             condition = "input.upload == 'FALSE'",
             selectInput(ns('product'),
                         'Landcover Product:',
                         choices = c('Copernicus Global Land Cover'))
           ),

           
           actionButton(ns("goStep1"), "Go")
         ),
         core_mapping_module_ui(ns('lcMap')) 
        )
      )
}


# Server  ----------------------------------------------------------------------
mapModuleServer <- function(id){
  
  moduleServer(id, function(input, output, session) {
    
    # Reactive values  ---------------------------------------------------------
    sites <- reactiveVal(NULL)
    input_sites <- reactiveVal(NULL)
    mapvals <- reactiveValues(bbox=NULL, draw=FALSE, raster=NULL)
    
    # Base Map
    core_mapping_module_server("lcMap", mapvals)
    
    # Clear map items with change of input
    observeEvent(input$map_draw, {
      mapvals$bbox = NULL
      mapvals$raster = NULL
      mapvals$draw = input$map_draw
    })
    
    observeEvent(mapvals$bbox, {
      mapvals$raster = NULL
    })
    
    # Get ROI --------- --------------------------------------------------------
    observeEvent(input$bbox_coords, {
      # get coords from output
      if(input$map_draw == 'FALSE') {
        if (!is.null(input$bbox_coords) && nchar(input$bbox_coords) > 0) {
          bbox <- strsplit(input$bbox_coords, ",")[[1]]
          
          # Ensure bbox has exactly 4 values: xmin, ymin, xmax, ymax
          if (length(bbox) == 4) {
            mapvals$bbox = as.numeric(bbox)
          }
        }
      }
      else(mapvals$bbox = NULL)
    })

    # Output text for bounding box and site counbt  ----------------------------

    output$boundingBoxCoords <- renderText({
      paste(mapvals$bbox, collapse=', ')
    })
     
     observeEvent(input$goStep1, {
       bbox = mapvals$bbox
       bbox_sf = st_as_sfc(st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]), 
                                   crs=st_crs(4326)))%>%
         st_as_sf()%>%
         st_buffer(2)
       
       
       # Load raster:
       if (input$upload) {
         withProgress(message = 'Uploading raster', value = 0, {
           r <- rast(input$rastfile$datapath)
           
           # Check that sites are within uploaded raster:
           if (!(relate(ext(bbox_sf), ext(r), relation = 'within'))) {
             showNotification("The uploaded raster and the bounding box do not overlap. Please ensure they cover the same region.", type = "error")
             return(NULL)
           }
           
           r <- terra::crop(r, bbox_sf)
           
           # Store raster for use
           if (input$product != 'NDVI') {
             levels(r) <- raster_cats %>% subset(product == input$product)
           }
         })

       } else {
         withProgress(message = 'Downloading landcover data', value = 0, {
           r <- cov_landuse(bbox_sf)
           levels(r) <- raster_cats %>% subset(product == 'Copernicus Global Land Cover')
         })
       }
      mapvals$raster = r
    })
     
     # Return Items
    list(
      bbox = reactive(mapvals$bbox), 
      lc_raster = reactive(mapvals$raster),
      product = reactive(input$product)
    )
  })
}

