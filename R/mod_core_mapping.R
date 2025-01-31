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
    leaflet::leafletOutput(ns("map"), height = 500),
    tags$div(
      style = "display: flex; gap: 10px;",
      selectInput(ns("bmap"), "Background map:",
                  choices = c("ESRI Topo" = "Esri.WorldTopoMap",
                              "Open Topo" = "OpenTopoMap",
                              "ESRI Imagery" = "Esri.WorldImagery",
                              "ESRI Nat Geo" = "Esri.NatGeoWorldMap"),
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
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>%
        leaflet::setView(0, 0, zoom = 2) %>%
        leaflet::addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet.extras::addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                                       markerOptions = FALSE,
                                       circleMarkerOptions = FALSE, singleFeature = TRUE, polygonOptions = FALSE)
    })

    # create map proxy to make further changes to existing map
    map <- leaflet::leafletProxy("map")

    # Capture coordinates of polygons

    observeEvent(input$map_draw_new_feature, {
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
      colnames(xy) <- c("longitude", "latitude")

      #convert any longitude drawn outside of the original map
      xy[,1] <- ((xy[,1] + 180) %% 360) - 180

      bbox = c(min(xy[,1]), min(xy[,2]), max(xy[,1]), max(xy[,2]))

      if(check_bbox(bbox)){
        bbox_sf = sf::st_as_sfc(sf::st_bbox(c(xmin=bbox[1],
                                                ymin=bbox[2],
                                                xmax=bbox[3],
                                                ymax=bbox[4]),
                                              crs=sf::st_crs(4326)))%>%
          sf::st_as_sf()
        bbox_sf = fix_geometry(bbox_sf)
        if(!is.null(bbox_sf)){
          common$sf = bbox_sf
        }
        else{
          map%>%reset_map(common$draw)
          showNotification('Error with selected area.')
        }
      }
    })

    observe({

      map%>%
        leaflet::addProviderTiles(input$bmap)%>%
        reset_map(common$draw)

      if(!is.null(common$sf)){
        map%>%
          draw_sf(common$sf, common$draw)
      }
      if(!is.null(common$sites)){
        map%>%
          map_points(common$sites)
      }
      if(!is.null(common$raster)){
        map%>%
          add_raster(common$raster)
      }

    })

  })
}

## To be copied in the UI
# mod_core_mapping_ui("core_mapping_1")

## To be copied in the server
# mod_core_mapping_server("core_mapping_1")
