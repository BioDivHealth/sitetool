core_mapping_module_ui <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    leaflet::leafletOutput(ns("map"), height = 700),
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

core_mapping_module_server <- function(id, common, main_input, COMPONENT_MODULES) {
  moduleServer(id, function(input, output, session) {
    # create map
    output$map <- renderLeaflet(
      leaflet() %>%
        setView(0, 0, zoom = 2) %>%
        addProviderTiles("Esri.WorldTopoMap") %>%
        leaflet.extras::addDrawToolbar(polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = TRUE,
                       markerOptions = FALSE, circleMarkerOptions = FALSE, singleFeature = TRUE, polygonOptions = FALSE)
    )

    # create map proxy to make further changes to existing map
    map <- leafletProxy("map")

    # change provider tile option
    observe({
      map %>% addProviderTiles(input$bmap)
    })

    # Capture coordinates of polygons
    gargoyle::init("change_poly")
    observe({
      coords <- unlist(input$map_draw_new_feature$geometry$coordinates)
      xy <- matrix(c(coords[c(TRUE,FALSE)], coords[c(FALSE,TRUE)]), ncol=2)
      colnames(xy) <- c("longitude", "latitude")
      #convert any longitudes drawn outside of the original map
      xy[,1] <- ((xy[,1] + 180) %% 360) - 180
      common$poly = xy
      
      # Calculate bounding box
      if (nrow(common$poly) >= 3) {
        x_coords <- common$poly[, 1]
        y_coords <- common$poly[, 2]
        
        # Bounding box values
        common$xmin <- min(x_coords)
        common$xmax <- max(x_coords)
        common$ymin <- min(y_coords)
        common$ymax <- max(y_coords)
      }
      
      gargoyle::trigger("change_poly")
    }) %>% bindEvent(input$map_draw_new_feature)

    component <- reactive({
      main_input$tabs
    })

    module <- reactive({
      if (component() == "intro") "intro"
      else main_input[[glue("{component()}Sel")]]
    })

    observe({
      req(module())
      current_mod <- module()
      gargoyle::on(current_mod, {
        map_fx <- COMPONENT_MODULES[[component()]][[module()]]$map_function
        if (!is.null(map_fx)) {
          do.call(map_fx, list(map, common = common))
        }
      })
    })

    return(map)
})
}


select_query_module_map <- function(map, common, sites) {
  
  if (!is.null(sites) && nrow(sites) > 0) {
    map <- map %>%
      clearMarkers()%>%
      clearControls()%>%
      addCircleMarkers(data = sites, 
                       lng = ~st_coordinates(sites)[, 1], 
                       lat = ~st_coordinates(sites)[, 2], 
                       popup = ~paste0("Site:", site,
                                       "<br>LAT: ", st_coordinates(geometry)[,2],
                                       "<br>LNG: ", st_coordinates(geometry)[,1]),
                       color = ~ifelse(added, "red", "blue"),  # Change color if needed
                       radius = 5,
                       fillOpacity = 0.8)%>%
      addLegend(
        position = "bottomright",
        colors = c("red", "blue"),
        labels = c("Input Sites", "Additional Sites")
    )
  }
}
