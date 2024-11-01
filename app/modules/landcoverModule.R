# Input: list of sites with lat, long
# Output: landcover parameters for each site


# UI ---------------------------------------------------------------------------
landcoverInput <-  function(id){
  ns <- NS(id)
  
  navset_card_tab(
    sidebar = sidebar(
      
      title = 'Step 2. Load and analyze landcover data.',
      
      width = 350, 

      checkboxInput(ns('upload'), 'Upload landcover data?', value = FALSE),
      
      conditionalPanel(
        ns=NS(id),
        condition = "input.upload",
        fileInput(ns("rastfile"), h6("Upload a GeoTIFF covering your region of interest:"), accept = c("tif", ".tiff")),
      ),
      
      selectInput(ns('product'),
                  'Landcover Product:',
                  choices = c('Copernicus Global Land Cover')),

      numericInput(ns("radius"),
                   "Enter distance from each site to analyze landcover data (meters):",
                   value = 1000),

      actionButton(ns("goStep2"), "Go"),
      
      downloadButton(ns("saveFile"), "Save File")
    ),
    nav_panel(
      title='Map',
      core_mapping_module_ui(ns('lcMap'))
    ),
    nav_panel(
      title='Dataset',
      DT::dataTableOutput(ns("lcData"))
     )

    )
}
# 
# # Server  ----------------------------------------------------------------------
landcoverOutput <- function(id, sites){
  stopifnot(is.reactive(sites))

  moduleServer(id, function(input, output, session) {
    mapvals <- reactiveValues(bbox=NULL, draw=FALSE, sites=NULL, raster=NULL)
    df <- reactiveVal(NULL)
    
    core_mapping_module_server("lcMap", mapvals)
    
    observe({mapvals$sites = sites()})
    # Reactivity for raster type -----------------------------------------------

    # Step 2: Caculate landcover values ----------------------------------------

    observeEvent(input$goStep2, {
      req(sites())

      # Load raster:
      if (input$upload) {
        r <- rast(input$rastfile$datapath)

        # Check that sites are within uploaded raster:
        if (!(relate(ext(sites()), ext(r), relation = 'within'))) {
          showNotification("The uploaded raster and the bounding box do not overlap. Please ensure they cover the same region.", type = "error")
          return(NULL)
        }

        # Store raster for use
        if (input$product != 'NDVI') {
          levels(r) <- raster_cats %>% subset(product == input$product)
        }
      } else {
        withProgress(message = 'Downloading landcover data', value = 0, {
          r <- cov_landuse(st_buffer(sites(), dist=0.1))
          levels(r) <- raster_cats %>% subset(product == 'Copernicus Global Land Cover')
        })
        mapvals$raster = r
      }
      
      # Immediately render the plot once the raster is available
      # output$landcoverMap <- renderPlot({
      #   req(r)  # Ensure raster is available
      #   tryCatch({
      #     # need to also catch this Warning: [plot] unknown categories in raster values
      #     cropped_raster <- crop(r, ext(sites()))
      #     plot(cropped_raster)  # Plot the cropped raster
      # 
      #     # Add label for sites
      #     plot(st_geometry(sites()), col = "black", pch = 8, cex = 3, add = TRUE)
      # 
      #   }, error = function(e) {
      #     showNotification("Error plotting raster: incorrect type or data mismatch.", type = "error")
      #     return(NULL)
      #   })
      # })

      # Proceed with calculating landcover values and saving the dataframe
      withProgress(message = "Calculating landcover values", value = 0, {
        if (input$product == 'NDVI') {
          out_df <- createNDVIDataFrame(sites(), raster = r, type = input$product, dist = input$radius, progress = TRUE)
        } else {
          out_df <- createLCDataFrame(sites(), raster = r, dist = input$radius, progress = TRUE)
        }

        sites_xy <- sites() %>%
          mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          st_drop_geometry()

        # Add x and y of sites to dataframe
        out_df <- out_df %>%
          left_join(sites_xy)
        
        # Add product being used
        out_df$product = input$product

      })

      # Render the data table after processing is complete
      output$lcData <- DT::renderDT({
        out_df%>%
          pivot_wider(names_from = c(cover, measure), values_from = value)
      })

      df(out_df)
    })


    # Save File: Full dataset  ---------------------------------------------------
    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("landcover_analyzer_export.csv")
      },
      content = function(file){
        write.csv(df(), file, row.names = FALSE)
      }
    )

  return(reactive(df()))
  })
}