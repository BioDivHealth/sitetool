#' core_mapping UI Function
#'
#' @description Creates the app map.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_core_mapping_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
        .leaflet-bottom.leaflet-left {
          bottom: 40px !important; /* shift legends up by 30px */
        }
      "))
    ),
    leaflet::leafletOutput(ns("map"), height = 500),
    tags$div(
      style = "display: flex; gap: 10px;",
      selectInput(
        ns("bmap"), "Background map:",
        choices = c(
          "ESRI Topo"   = "Esri.WorldTopoMap",
          "Open Topo"   = "OpenTopoMap",
          "ESRI Imagery"= "Esri.WorldImagery",
          "ESRI Nat Geo"= "Esri.NatGeoWorldMap"
        ),
        selected = "Esri.WorldTopoMap"
      )
    )
  )
}


#' core_mapping Server Functions
#'
#' @noRd
mod_core_mapping_server <- function(id, common){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Render initial Leaflet map
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet.extras::addDrawToolbar(
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = TRUE,
          markerOptions = FALSE,
          circleMarkerOptions = FALSE,
          singleFeature = TRUE,
          polygonOptions = FALSE
        )
    })

    # Proxy for updating the map
    proxy <- leaflet::leafletProxy("map")

    # Observe changes to base map input (bmap), shape file (sf), raster, and sites

    observe({
      req(input$bmap)
      proxy %>%
        leaflet::clearTiles() %>%
        #leaflet::clearControls() %>%
        leaflet::addProviderTiles(input$bmap)

      # Clear existing drawings to redraw
      if (!is.null(common$draw) && common$draw) {
        proxy %>% reset_map(common$draw)
      }

      # Draw shape file bbox if available
      if (!is.null(common$sf)) {
        proxy %>% draw_sf(common$sf, common$draw)
      }

      # Add raster if available
      if (!is.null(common$raster)) {
        proxy %>%
          add_raster_stack(common$raster)
      } else {
        # have to rebuild map because layers toggle is a pain :)))))
        output$map <- leaflet::renderLeaflet({
          leaflet::leaflet() %>%
            leaflet::setView(lng = 0, lat = 0, zoom = 2) %>%
            leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
            leaflet.extras::addDrawToolbar(
              polylineOptions = FALSE,
              circleOptions = FALSE,
              rectangleOptions = TRUE,
              markerOptions = FALSE,
              circleMarkerOptions = FALSE,
              singleFeature = TRUE,
              polygonOptions = FALSE
            )
        })

        # Proxy for updating the map
        proxy <- leaflet::leafletProxy("map")
        }
      # Add or update site markers
      if (!is.null(common$sites) && nrow(common$sites) > 0) {
        proxy %>% map_points(common$sites)
      }
    })

    # Listen for bounding box drawn by user
    observeEvent(input$map_draw_new_feature, {
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
      colnames(xy) <- c("longitude", "latitude")

      # Normalize longitude if outside -180 to 180 range
      xy[,1] <- ((xy[,1] + 180) %% 360) - 180

      bbox <- c(min(xy[,1]), min(xy[,2]), max(xy[,1]), max(xy[,2]))

      if (check_bbox(bbox)) {
        bbox_sf <- sf::st_as_sfc(
          sf::st_bbox(c(xmin=bbox[1], ymin=bbox[2], xmax=bbox[3], ymax=bbox[4]), crs=sf::st_crs(4326))
        ) %>% sf::st_as_sf()
        bbox_sf <- fix_geometry(bbox_sf)

        if (!is.null(bbox_sf)) {
          common$sf <- bbox_sf
        } else {
          proxy %>% reset_map(common$draw)
          showNotification("Error with selected area.")
        }
      }
    })


    # Add new site on map click
    observeEvent(input$map_click, {
      req(common$add_points)
      click <- input$map_click
      req(click)

      # Create a new site data.frame with coordinates
      new_site <- data.frame(
        site = paste0("SelectedPoint_Lat_",round(click$lat, digits=4), "_Lon_", round(click$lng, digits=4)),
        site_id = paste0("select_", as.integer(Sys.time()), "_Lat_",round(click$lat, digits=4), "_Lon_", round(click$lng, digits=4)),
        input_site = TRUE,
        longitude = click$lng,
        latitude = click$lat,
        stringsAsFactors = FALSE
      )

      # Convert to sf point geometry
      new_site_sf <- sf::st_as_sf(new_site, coords = c("longitude", "latitude"), crs = 4326)

      # Check if inside bbox
      if (!is.null(common$sf)) {
        inside <- any(sf::st_within(new_site_sf, common$sf, sparse = FALSE))

        if (!inside) {
          showNotification("Clicked point is outside of the region of interest.", type = "warning")
          return(NULL) # skip adding point
        }
      }

      # Combine with existing sites
      if (is.null(common$sites) || nrow(common$sites) == 0) {
        common$sites <- new_site_sf
      } else {
        common$sites <- rbind(common$sites, new_site_sf)
      }
    })

    # Toggle site selection when marker clicked
    observeEvent(input$map_marker_click, {
      req(common$add_points)
      click_id <- input$map_marker_click$id
      req(click_id)

      isolate({
        sites_df <- common$sites
        idx <- which(sites_df$site_id == click_id)
        if (length(idx) == 1) {
          sites_df$input_site[idx] <- !sites_df$input_site[idx]
          common$sites <- sites_df
        }
      })
    })

  })
}



## To be copied in the UI
# mod_core_mapping_ui("core_mapping_1")

## To be copied in the server
# mod_core_mapping_server("core_mapping_1")
