# Helper Mapping Functions ----------------------------------------------------

reset_map <- function(map, draw, bbox){
  if(draw == 'TRUE'){
    map%>%
      clearShapes() %>%
      clearMarkers()%>%
      clearControls()%>%
      leaflet.extras::addDrawToolbar(rectangleOptions = TRUE, 
                                     polylineOptions = FALSE, 
                                     circleOptions = FALSE,
                                     markerOptions = FALSE, 
                                     circleMarkerOptions = FALSE, 
                                     singleFeature = TRUE, 
                                     polygonOptions = FALSE)
  }
  else{
    map%>%
      leaflet.extras::removeDrawToolbar(clearFeatures=TRUE)%>%
      clearShapes()%>%
      clearMarkers()%>%
      clearControls()
  }
  
}

map_points <- function(map, sites) {
  mn_lng = mean(st_coordinates(sites)[, 1])
  mn_lat = mean(st_coordinates(sites)[, 2])
  
  if (!is.null(sites) && nrow(sites) > 0) {
    map <- map %>%
      clearMarkers()%>%
      clearControls()%>%
      setView(lng = mn_lng, lat = mn_lat, zoom = 8)%>%
      addCircleMarkers(data = sites, 
                       lng = ~st_coordinates(sites)[, 1], 
                       lat = ~st_coordinates(sites)[, 2], 
                       popup = ~paste0("Site: ", site,
                                       "<br>LAT: ", st_coordinates(geometry)[,2],
                                       "<br>LNG: ", st_coordinates(geometry)[,1]),
                       color = ~ifelse(input_site, "red", "blue"),  # Change color if needed
                       radius = 5,
                       fillOpacity = 0.8)%>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Input Sites", "Additional Sites")
      )
  }
}

draw_sf <- function(map, sf_obj) {
  map <- map %>%
    reset_map(draw=F)%>%
    #fitBounds(lng1 = bbox["xmin"], lat1 = bbox["ymin"], lng2 = bbox["xmax"], lat2 = bbox["ymax"]) %>%
    addPolygons(data = sf_obj, color = "#5365FF", fillOpacity = 0.2)
  
}

add_raster <- function(map, r){
 # pal <- colorFactor(topo.colors(nlevels(r)), levels(r))
  
  map <- map %>%
    clearImages() %>%  # Clear any previous raster
    addRasterImage(r, opacity = 0.8) %>%
    addRasterLegend(r, opacity = 0.8)
}


# Map UI -----------------------------------------------------------------------
core_mapping_module_ui <- function(id) {
  ns <- shiny::NS(id)
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

# Map Server -------------------------------------------------------------------
core_mapping_module_server <- function(id, common) {
    
  moduleServer(id, function(input, output, session) {

    # create map
    output$map <- renderLeaflet({
      leaflet() %>%
        setView(0, 0, zoom = 2) %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet.extras::addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                                       markerOptions = FALSE, 
                                       circleMarkerOptions = FALSE, singleFeature = TRUE, polygonOptions = FALSE)
    })
    
    # create map proxy to make further changes to existing map
    map <- leafletProxy("map")
    
    # Capture coordinates of polygons

    observeEvent(input$map_draw_new_feature, {
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
      colnames(xy) <- c("longitude", "latitude")
      
      #convert any longitude drawn outside of the original map
      xy[,1] <- ((xy[,1] + 180) %% 360) - 180

      common$bbox = c(min(xy[,1]), min(xy[,2]), max(xy[,1]), max(xy[,2]))
    })
    
      observe({
        map%>%
          addProviderTiles(input$bmap)%>%
          reset_map(common$draw)
  
         if(!is.null(common$bbox)){
           map%>%
             addRectangles(
               lng1 = common$bbox[1], lat1 = common$bbox[2],
               lng2 = common$bbox[3], lat2 = common$bbox[4],
               color = "blue", weight = 2, fillOpacity = 0.2
             )
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



draw_bbox <- function(map, bbox) {
  map <- map %>%
      reset_map(draw=F)%>%
      addRectangles(
        lng1 = bbox[1], lat1 = bbox[2],
        lng2 = bbox[3], lat2 = bbox[4],
        color = "#5365FF", fillOpacity = 0.2
      )
      
}




