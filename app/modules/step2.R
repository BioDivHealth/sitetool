# Input: list of sites with lat, long
# Output: landcover parameters for each site


# UI ---------------------------------------------------------------------------
sitesUI <-  function(id){
  ns <- NS(id)
  
    layout_sidebar(
       sidebar = sidebar(
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
          ),
          fluidRow(
            column(4, tags$label("Max city population (thousands)")),
            column(8, sliderInput(ns('city_pop'), label = NULL, min = 0, max = 1000, value = 1000))
          )
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
      nav_panel(
        title = 'Site Map',
        core_mapping_module_ui(ns("siteMap"))
      )
    )
}
# 
# # Server  ----------------------------------------------------------------------
sitesServer <- function(id, bbox, lc_raster){
  stopifnot(is.reactive(bbox))
  stopifnot(is.reactive(lc_raster))

  moduleServer(id, function(input, output, session) {
    
    # Reactive values  ---------------------------------------------------------
    sites <- reactiveVal(NULL)
    input_sites <- reactiveVal(NULL)
    mapvals <- reactiveValues(sites=NULL, bbox=NULL, raster=NULL, draw=FALSE)
    
    core_mapping_module_server("siteMap", mapvals)
    
    observe({mapvals$raster = lc_raster()
            mapvals$bbox = bbox()})
    
    observeEvent(list(input$selection_type, mapvals$bbox), {
      mapvals$sites = NULL
    })
    
    # # Output number of potential sites (including input sites)
    output$siteCount <- renderText({
      nrow(sites())
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
          mutate(longitude = sf::st_coordinates(.)[,1],
               latitude = sf::st_coordinates(.)[,2]) %>%
          st_drop_geometry()
        
        write.csv(outfile, file, row.names = F)
      }
    )

    # Step 2: Caculate landcover values ----------------------------------------
    observeEvent(input$goStep2, {
      req(mapvals$bbox)
      
      # Download sites within the bounding box
      if(input$selection_type == 'village'){
        sites_filter = get_village_points(mapvals$bbox, as.numeric(input$city_pop)*1e3)
      }
      
      if(input$selection_type == 'random'){
        points = get_random_points(mapvals$bbox, 
                                   as.numeric(input$num_sites), 
                                   as.numeric(input$dist_road),
                                   as.numeric(input$dist_city))
        
        # turn into dataframe
        sites_filter = data.frame(points)%>%
          st_as_sf()%>%
          dplyr::mutate(site = 1:length(points),
                        site_id = paste0('random_', 1:length(points)),
                        input_site = FALSE)
        
      }
      
      if(!is.null(input_sites())){
        id = input_sites()
        id$input_site= TRUE
        id$site_id = paste0('input_',1:nrow(id))
        id = st_as_sf(id, coords = c("longitude", "latitude"),
                      crs = "epsg:4326")%>%
          select(c(site, site_id, input_site))
        
        sites_filter = rbind(id, sites_filter)
      }
      
      # store and return sites
      mapvals$sites = sites_filter
      sites(sites_filter)
    })
    return(reactive(sites())) 
  })
  


}