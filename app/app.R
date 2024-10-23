## SHINY LANDCOVER APP ##

# Packages  -------------------------------------------------------------------

if (!require("Require")) {
  install.packages("Require")
  require("Require")
}

Require(c("shiny", "shinyWidgets", "ggplot2", "bslib", "leaflet", "dplyr", "tidyr",
          "terra", "ggiraph", "sf", "osmdata", "gargoyle", "gt", "ggdist"))


# Functions and modules --------------------------------------------------------

source('helpers/CalculateLandCover.R')
source('helpers/core_mapping.R')
source('helpers/report_stats.R')
source('helpers/get_landuse_data.R')


# Global vars and options  ----------------------------------------------------

# Increase max upload size 
options(shiny.maxRequestSize=2000*1024^2)

# Labels for land cover categories
raster_cats = read.csv('data/categories.csv')

# Define UI  -----------------------------------------------------------

ui <- page_navbar(
  
    title =  span("Site Selection Landcover Analyzer", 
                  style={'padding-left:15px'}),

    theme = bs_theme(preset='pulse'),
    fillable = FALSE,
    
    nav_panel("Home",
      
      card(
        layout_sidebar(
          class="p-3 border border-top-0 rounded-bottom",
            sidebar = sidebar(
              title = "Step 1. Generate a list of potential sites.",
              h6('Please select a region of interest using the box tool on the map.'),
              
  
              selectInput("selection_type", "Type of site:", choices = c("random", "village")),
              
              # Conditionally show the numeric input only when 'random' is selected
              conditionalPanel(
                condition = "input.selection_type == 'random'",
                numericInput("num_sites", "Number of random sites:", value = 100, min = 20)
              ),
  
              fileInput("csvfile", 
                        label = tagList("Upload a CSV of potential sites ", tags$em("(optional):")),
                        accept = c(".csv")),
              actionButton("goStep1", "Go"),
              
              h6('Bounding Box Coordinates (xmin, ymin, xmax, ymax):'),
              textOutput("boundingBoxCoords"),
              
              h6('Number of Potential Sites:'),
              textOutput("siteCount")
            ),
            core_mapping_module_ui("siteMap")
      )),
      
      navset_card_tab(
        sidebar = sidebar(
          title = 'Step 2. Load and analyze landcover data.',
          checkboxInput('upload', 'Upload your own data?', value = FALSE),
          selectInput('product', 
                      'Landcover Product:', 
                      choices = c('Copernicus Global Land Cover')),
          conditionalPanel(
            condition = "input.upload",
         #   h6('Upload a GeoTIFF covering the above area:'),
            fileInput("rastfile", "Upload a GeoTIFF covering the above area:", accept = c("tif", ".tiff"))   
          ),

          numericInput("radius", 
                       "Enter radius from each site center to analyze landcover data (meters):", 
                       value = 1000),
          actionButton("goStep2", "Go"),
          downloadButton("saveFile", "Save File")
        ),
        nav_panel(
          title = 'Landcover Map',
          plotOutput("landcoverMap")
        ),
        nav_panel(
          title='Dataset', 
             DT::dataTableOutput("landcoverData")  # Adjust the column width as needed
        )
      ),
      
        navset_card_tab(
          sidebar = sidebar(
          title = 'Step 3. Filter and compare sites.',
          fileInput("landcsv", 
                    label = tagList("Upload previously analyzed landcover data", tags$em("(optional):")),
                    accept = c(".csv")),
          h6('Select a measure to compare'),
          selectInput("measure", "Measurement", "")
         ),
          nav_panel(title='Comparison Plot',
                   # tags$style(type = "text/css", ".container-fluid {padding-left:5px};  padding-right:15px ;margin-right:auto"),
                    uiOutput("compPlot", inline=TRUE)
          )
          # nav_panel(title='Selected Data', 
          #           column(8, DT::dataTableOutput("outData")),  # Display the data table
          #           column(4, 
          #                  downloadButton("outFile", "Save File"),  # Save button
          #                  actionButton("clearButton", "Clear Selections")  # Clear button
          #           )
          #   )
        )
      ),
  nav_panel(title = 'About', 
            uiOutput('about'), 
            tags$style(type = "text/css", ".container-fluid {padding-left:20px}")
            #class="p-3 border border-top-0 rounded-bottom"
            )
)


# Define Server ----------------------------------------------------------------

server <- function(session, input, output) {
  
  # Reactive values ------------------------------------------------------------
  sites <- reactiveVal(NULL)
  input_sites <- reactiveVal(NULL)
  raster <- reactiveVal(NULL)
  bounds <- reactiveVal(NULL)
  df <- reactiveValues(long=NULL, wide=NULL, filter=NULL)
  mapvals <- reactiveValues(poly = NULL)
  selected_points <- reactiveVal(list())

  # Base map and text ----------------------------------------------------------
  map = core_mapping_module_server("siteMap", mapvals)
  output$about <- renderUI({includeMarkdown("about.md")})
  
  # Reactivity for raster type -------------------------------------------------
  observe({
    if (input$upload) {
      # If the checkbox is checked, update the selectInput choices
      updateSelectInput(session, 'product', 
                        choices = c('Copernicus Global Land Cover', 'ESA WorldCover',  'Dynamic World', 'NDVI'))
    } else {
      # Default choices if checkbox is not checked
      updateSelectInput(session, 'product', 
                        choices = c('Copernicus Global Land Cover'))
    }
  })
  
  # Check uploaded data --------------------------------------------------------
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
  
  # Step 1: Get points from map  -----------------------------------------------
  observeEvent(input$goStep1, {
      req(mapvals$poly)
        
      # Create bounding box from user input
      search_area <- c(mapvals$xmin, mapvals$ymin, mapvals$xmax, mapvals$ymax)
      bounds(search_area)
      
      # Output bounding box coordinates
      output$boundingBoxCoords <- renderText({
        paste0("(", mapvals$xmin, ", ", mapvals$ymin, ", ", 
              mapvals$xmax, ", ", mapvals$ymax, ")")
      })
      
      # Download sites within the bounding box
      if(input$selection_type == 'village'){
        sites_data <- opq(bbox = search_area) %>%
          add_osm_feature(key = "place", value = c("city", "suburb", "village", "town", "hamlet")) %>%
          osmdata_sf()
        
        # Extract village points and clean the data
        sites_filter <- sites_data$osm_points %>%
          select(osm_id, name, geometry) %>%
          rename(site_id = osm_id, site = name) %>%
          mutate(input_site = FALSE)%>%
          filter(!is.na(site))
      }
      
      if(input$selection_type == 'random'){
        sites_filter = data.frame(
          site = as.character(1:input$num_sites),
          site_id = paste0('random_', 1:input$num_sites),
          input_site = FALSE,
          lat = runif(input$num_sites, min = mapvals$ymin, max = mapvals$ymax),
          lon = runif(input$num_sites, min = mapvals$xmin, max = mapvals$xmax)
        )%>%
        st_as_sf(coords=c('lon', 'lat'), crs=4326)  
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
      
      # store sites for later use
      sites(sites_filter)
      
      # Output number of potential sites (including input sites)
      output$siteCount <- renderText({
          nrow(sites())
      })
      
      # update map with selected sites
      map_points(map, mapvals, sites())
  })
  
 
   # # Check uploaded raster -----------------------------------------------------
   # observeEvent(c(input$rastfile, input$product), {
   #   req(input$rastfile)
   #   req(sites())
   # 
   #   # Load raster:
   #   if(input$product == 'Default'){
   #     r = cov_landuse(sites())
   #     levels(r) = raster_cats%>%subset(product == 'Copernicus Global Land Cover')
   #   }
   #   else{
   #     r = rast(input$rastfile$datapath)
   # 
   #     # Check that sites are within uploaded raster:
   #     if (!(relate(ext(sites()), ext(r), relation='within'))) {
   #       showNotification("The uploaded raster and the bounding box do not overlap. Please ensure they cover the same region.", type="error")
   #       return(NULL)
   #     }
   #     # store raster for use
   #     if(input$product != 'NDVI'){
   #       levels(r) = raster_cats%>%subset(product == input$product)
   #     }
   #   }
   # 
   #   output$landcoverMap <- renderPlot({
   #       tryCatch({
   #         cropped_raster <- crop(r, ext(sites()))
   #         plot(cropped_raster) # Attempt to plot the cropped raster
   # 
   #         # add label for sites
   #         plot(st_geometry(sites()), col="magenta", pch=8, cex=3, add=TRUE)
   # 
   #         # Store raster if works
   #         raster(r)
   # 
   #       }, error = function(e) {
   #         showNotification("Incorrect raster type selected.", type = "error")
   #         return(NULL)
   #       })
   #   })
   # 
   # })


 
  # Step 2: Caculate landcover values ------------------------------------------
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
        r <- cov_landuse(sites())
        levels(r) <- raster_cats %>% subset(product == 'Copernicus Global Land Cover')
      })
    }
    
    # Immediately render the plot once the raster is available
    output$landcoverMap <- renderPlot({
      req(r)  # Ensure raster is available
      tryCatch({
        # need to also catch this Warning: [plot] unknown categories in raster values
        cropped_raster <- crop(r, ext(sites()))
        plot(cropped_raster)  # Plot the cropped raster
        
        # Add label for sites
        plot(st_geometry(sites()), col = "black", pch = 8, cex = 3, add = TRUE)
        
      }, error = function(e) {
        showNotification("Error plotting raster: incorrect type or data mismatch.", type = "error")
        return(NULL)
      })
    })
    
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
      
      # Save as long format
      df$long <- out_df
      
      # Convert to wide format for export
      out_df <- out_df %>%
        pivot_wider(names_from = c(cover, measure), values_from = value)
      
      df$wide <- out_df
    })
    
    # Render the data table after processing is complete
    output$landcoverData <- DT::renderDT({
      df$wide
    })
  })
  
  
  # Save File: Full dataset  ------------------------------------------
  output$saveFile <- downloadHandler(
    filename = function() {
      paste0("landcover_analyzer_export.csv")
    },
    content = function(file){
      write.csv(df$wide, file, row.names = FALSE)
    }
  )
  
  observe({
    updateSelectInput(session, "measure", choices=unique(df$long$measure), select='proportion')
  })
  
  observe({
    req(df$long)
    
    df$filter = df$long%>%
       subset(measure == input$measure)#%>%
      # filter(
      #   (cover == "builtup" & between(value, input$BuiltUpSelect[1], input$BuiltUpSelect[2])) |
      #     (cover == "treecover" & between(value, input$TreeSelect[1], input$TreeSelect[2])) |
      #     (cover == "cropland" & between(value, input$CropSelect[1], input$CropSelect[2])) |
      #     (cover == "grassland" & between(value, input$GrassSelect[1], input$GrassSelect[2])) |
      #     (cover == "shrubland" & between(value, input$ShrubSelect[1], input$ShrubSelect[2]))
      # )
  })
  

  # Step 3: Compare sites  -----------------------------------------------------
  observeEvent(list(input$landcsv, input$measure), {
    
    # Check for uploaded CSV or use output from step 2
    if(!is.null(input$landcsv)){
      # TODO ERROR HANDLING
      indata = read.csv(input$landcsv$datapath)
      df$wide = indata
      
      df$long = indata%>%
        pivot_longer(cols=-c(site, site_id, input_site, latitude, longitude), 
                                      names_to='temp', 
                                      values_to='value')%>%
        separate(temp, into=c('cover', 'measure'), sep='_', extra='merge')
    }
    else{req(df$long)}
    
    # Render the plots and HTML
    output$compPlot <- renderUI({
      
      # Organize dataframe
      
      d = df$filter%>% # add in colors
        left_join(raster_cats%>%subset(product == input$product)%>%select(-c(subcover, value))%>%group_by(cover)%>%slice(1))%>%
        mutate(group = ifelse(input_site==TRUE, 'Input Sites', 'All Sites'),
               group = factor(group, c('Input Sites', 'All Sites')),
               point_size = ifelse(group == "Input Sites", 3, 1),  # Size for each group
               point_alpha = ifelse(group == "Input Sites", 0.8, 0.3)  # Alpha for each group
        )

      # Get the unique cover levels
      cats <- unique(d$cover)
      
      # Create a list of plots and corresponding HTML
      plot_list <- purrr::map(cats, function(cat) {
        
        # Filter data for the current cover level
        cover_data <- d %>% filter(cover == cat)
        
        # Generate the interactive plot
        plot <- generate_plot(cover_data, cat, input$measure)
        
        # Generate the corresponding HTML text
        html_text <- generate_text(cover_data, cat)
        
        # Combine the plot and HTML into one display
        tagList(
             renderGirafe({plot}),
              HTML(html_text),
              hr() # Horizontal line to separate each plot-text block
        )
      })
      
      # Return the list of plots and text blocks
      do.call(tagList, plot_list)
    })
  
  })
  
  # Observe selected points from the plot
  observeEvent(input$compPlot_selected, {
    print(input$compPlot_selected)
    selected_sites <- input$compPlot_selected

    current_selection <- selected_points()
    selected_points(append(current_selection, list(selected_sites)))
  })

  # display selected points in a table
  output$outData <- DT::renderDT({
    selected_df <- df$wide%>%
      subset(site_id %in% unlist(selected_points()))
    
    DT::datatable(selected_df)
  })
  
  # Clear the selected points
  observeEvent(input$clearButton, {
    selected_points(c())  # Reset to an empty vector

  })

  output$outFile <- downloadHandler(
    filename = function() {
      paste0("selected_sites.csv")
    },
    content = function(file){
      selected_df <- df$wide%>%
        subset(site_id %in% unlist(selected_points()))
      
      write.csv(selected_df, file, row.names = FALSE)
    }
  )
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)