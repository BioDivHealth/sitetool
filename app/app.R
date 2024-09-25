## SHINY LANDCOVER APP ##

# Packages  -------------------------------------------------------------------
if (!require("Require")) {
  install.packages("Require")
  require("Require")
}

Require(c("shiny", "ggplot2", "bslib", "leaflet", "dplyr", "tidyr", "shinyWidgets", "ggiraph", 
          "sf", "osmdata", "gargoyle", "gt"))

# Functions and modules --------------------------------------------------------
source('helpers/CalculateLandCover.R')
source('helpers/core_mapping.R')


# Global vars and options  ----------------------------------------------------

options(shiny.maxRequestSize=50*1024^2)

plot_vars = c('area_mn',  'treecover_percent', 'grassland_percent', 
              'cropland_percent', 'builtup_percent', 'treecover_patch_area', 
              'builtup_patch_area', 'cropland_patch_area')

plot_labs = c('Fragmentation (mean patch area)', 'Proportion treecover', 'Proportion grasssland',
              'Proportion cropland', 'Proportion built-up', 'Treecover mean patch area', 'Built-up mean patch area',
              'Cropland mean patch area')

palette <- scales::col_bin(palette=c('red','yellow','green'), bins=c(0.05,.1,1))

ynames <- c("Tree Cover (%)", "Shrubland (%)", "Grassland (%)", "Cropland (%)")

names(plot_vars) = plot_labs

raster_cats = read.csv('helpers/worldcover_cats.csv')

# Define UI  -----------------------------------------------------------

ui <- page_sidebar(
  
    title =  "Site Selection Landcover Analyzer", 
    class = 'bslib-page-dashboard',

    theme = bs_theme(preset='pulse'),
  
    fillable = FALSE,
    sidebar = sidebar(h4('Instructions'),
                      uiOutput("instructions")
                      ),
    card(
      layout_sidebar(
          sidebar = sidebar(
            title = "Step 1. Generate map of potential sites.",
            h6('Please select a region of interest using the box tool on map.'),

            selectInput("selection_type", "Type of site:", choices = c("random", "village")),
            
            # Conditionally show the numeric input only when 'random' is selected
            conditionalPanel(
              condition = "input.selection_type == 'random'",
              numericInput("num_sites", "Number of random sites:", value = 100, min = 20)
            ),

            fileInput("csvfile", 
                      label = tagList("Upload CSV of potential sites ", tags$em("(optional):")),
                      accept = c(".csv")),
            actionButton("goStep1", "Go"),
            
            h6('Bounding Box Coordinates (xmin, ymin, xmax, ymax):'),
            textOutput("boundingBoxCoords"),
            
            h6('Number of Potential Sites:'),
            textOutput("siteCount")
          ),
          core_mapping_module_ui("siteMap")
    )),
    
    layout_sidebar(
      sidebar = sidebar(
        title = 'Step 2. Load and analyze landcover data.',
        h6('Upload a GeoTIFF covering the above area.'),
        fileInput("rastfile", "Choose a GeoTIFF File:",
                  accept = c("tif", ".tiff")),
        numericInput("radius", "Enter radius from site center to analyze landcover (meters):", value = 2000),
        actionButton("goStep2", "Go")
      ),
      plotOutput("landcoverMap")
    ),
    
      navset_card_tab(
        sidebar = sidebar(
        title = 'Step 3. Filter and compare sites.',
        fileInput("landcsv", "Upload previously analyzed landcover data (optional):",
                  accept = c(".csv")),
        # Create a fluid row to place X and Y axis inputs side by side
        fluidRow(
          column(6, selectInput("x_lc", "X-axis landcover", "")),  # X-axis label choice
          column(6, selectInput("x_m", "X-axis measurement", ""))  # X-axis measurement choice
        ),
        fluidRow(
          column(6, selectInput("y_lc", "Y-axis landcover", "")), 

          column(6, selectInput("y_m", "Y-axis measurement", ""))   # Y-axis measurement choice
        ),
        # pickerInput(inputId = "x",
        #             label = "X-Axis",
        #             choices = plot_vars,
        #             select = plot_vars[1]),
        # pickerInput(inputId = "y",
        #             label = "Y-Axis",
        #             choices = plot_vars,
        #             select = plot_vars[2]),
        # sliderInput(inputId = "BuiltUpSelect", 
        #             label = "Proportion Built-up", 
        #             min = 0,
        #             step = 0.05,
        #             max = 1,
        #             value = c(0,1)), 
        # sliderInput(inputId = "TreeSelect", 
        #             label = "Proportion Treecover", 
        #             min = 0,
        #             step = 0.05,
        #             max = 1,
        #             value = c(0,1)), 
        # sliderInput(inputId = "CropSelect", 
        #             label = "Proportion Cropland", 
        #             min = 0,
        #             step=0.05,
        #             max = 1,
        #             value = c(0,1)), 
        # sliderInput(inputId = "GrassSelect", 
        #             label = "Proportion Grassland", 
        #             min = 0,
        #             max = 1,
        #             step=0.05,
        #             value = c(0,1)),
        # sliderInput(inputId = "ShrubSelect", 
        #             label = "Proportion Shrubland", 
        #             min = 0,
        #             max = 1,
        #             step=0.05,
        #             value = c(0,1)),
        # pickerInput(inputId = "maptype", 
        #             label = "Map Type", 
        #             choices = c('street', 'satellite'), 
        #             select = "satellite"),
        checkboxInput('log_scale','Log Scale?'),
        actionButton("goStep3", "Go")
      ),
        nav_panel(title='Scatterplot',
                  fillable=T,
                  girafeOutput("gradientPlot")),
        nav_panel(title='Comparison Plot and Table',
                    #  width = 1/2,
                     # heights_equal = "row",
                      plotOutput('compPlot'),
                      gt_output("statsTable")
                  ),
        nav_panel(title='Data', DT::DTOutput("landcoverData"), downloadButton("savefile", "Save File"))
    )
    

)

# Define Server -----------------------------------------------------------

server <- function(session, input, output) {

  sites <- reactiveVal(NULL)
  raster <- reactiveVal(NULL)
  bounds <- reactiveVal(NULL)
  df <- reactiveVal(NULL)
  filter_data <- reactiveVal(NULL)
  
  # Common reactive values (shared across components)
  common <- reactiveValues(poly = NULL)
  
  # Example structure of main_input and COMPONENT_MODULES
  main_input <- reactiveValues(tabs = "intro") 
  COMPONENT_MODULES <- list(
    intro = list(intro = list(map_function = NULL))
    # Add other components and map functions here
  )
  
  gargoyle::init("intro")
  
  map = core_mapping_module_server("siteMap", common, main_input, COMPONENT_MODULES)
  
  output$instructions <- renderUI({includeMarkdown("instructions.md")})
  
  
  #### STEP 1 SERVER ####
  observeEvent(input$goStep1, {
      req(common$poly)
        
        # Create bounding box from user input
      search_area <- c(common$xmin, common$ymin, common$xmax, common$ymax)
      bounds(search_area)
      
      # Output bounding box coordinates
      output$boundingBoxCoords <- renderText({
        paste0("(", common$xmin, ", ", common$ymin, ", ", 
              common$xmax, ", ", common$ymax, ")")
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
          mutate(added = FALSE)%>%
          filter(!is.na(site))
      }
      
      if(input$selection_type == 'random'){
        sites_filter = data.frame(
          site = as.character(1:input$num_sites),
          site_id = paste0('random_', 1:input$num_sites),
          added = FALSE,
          lat = runif(input$num_sites, min = common$ymin, max = common$ymax),
          lon = runif(input$num_sites, min = common$xmin, max = common$xmax)
        )%>%
        st_as_sf(coords=c('lon', 'lat'), crs=4326)  
      }
  
      if(!is.null(input$csvfile)){
        candidate = read.csv(input$csvfile$datapath)
        candidate$added= TRUE
        candidate$site_id = paste0('input_',1:3)
        candidate = st_as_sf(candidate, coords = c("longitude", "latitude"),
                 crs = "epsg:4326")%>%
          select(c(site, site_id, added))
  
        sites_filter = rbind(candidate, sites_filter)
      }
  
      # store sites for later use
      sites(sites_filter)
      
      output$siteCount <- renderText({
          nrow(sites())
      })
      
      # update map with selected sites
      select_query_module_map(map, common, sites())

  })

  #### STEP 2 SERVER ####
  observeEvent(input$goStep2, {
    req(input$rastfile)
    req(sites())

    # load landcover
    r = rast(input$rastfile$datapath)
    
    #r = crop(r, ext(bounds(), xy=TRUE))

    # store raster for use
    levels(r) = raster_cats
    raster(r)
    
    #c = terra::cats(r)[[1]]%>%rename(lc_value = value)
    #raster_cats(c)
    
    output$landcoverMap <- renderPlot({
      plot(r)
      plot(st_geometry(sites()), col="blue", pch=8, add=T)
    })

    withProgress(message = "Calculating landcover values", value=0, {
      out_df = createLCDataFrame(sites(), raster=raster(), dist=input$radius, progress=T)
      
      df(out_df)
    })
    
    
  })
  
  observe({
    updateSelectInput(session, "x_lc", choices=unique(df()$cover))
    updateSelectInput(session, "y_lc", choices=unique(df()$cover))
    updateSelectInput(session, "x_m", choices=unique(df()$measure))
    updateSelectInput(session, "y_m", choices=unique(df()$measure))
  })

  #### STEP 3 SERVER ####
  observeEvent(input$goStep3, {
    if(!is.null(input$landcsv)){
      indata = read.csv(input$landcsv$datapath)
      df(indata)
    }
    else{req(df())}

    # filter_data = df()%>%
    #   filter(between(builtup_percent, input$BuiltUpSelect[1], input$BuiltUpSelect[2]))%>%
    #   filter(between(treecover_percent, input$TreeSelect[1], input$TreeSelect[2]))%>%
    #   filter(between(cropland_percent, input$CropSelect[1], input$CropSelect[2]))%>%
    #   filter(between(grassland_percent, input$GrassSelect[1], input$GrassSelect[2]))%>%
    #   filter(between(shrubland_percent, input$ShrubSelect[1], input$ShrubSelect[2]))
    # 
    # filter_data(filter_data)
    
    output$landcoverData <- DT::renderDT({
      df()
    })
    
    filter_data = df()%>%
      filter((cover == input$x_lc & measure == input$x_m) |
             (cover == input$y_lc & measure == input$y_m)) %>%
      pivot_wider(names_from = cover, values_from = value)
    
    output$gradientPlot<- renderGirafe({
      p1 <- filter_data%>%
        ggplot(aes_string(input$x_lc, input$y_lc))+
        geom_point_interactive(aes(tooltip = site, data_id=site_id, color=added))+
        scale_color_manual(values=c('black','red'),
                           labels = c('Additional Sites', 'Input Sites'))+
        labs(x=paste(input$x_lc, input$x_m), 
             y=paste(input$y_lc, input$y_m), 
             color=NULL)+
        theme_classic()

      p2 <- p1+
        coord_trans(x = 'log10', y = 'log10')+
        theme_classic()

      if(input$log_scale){
        girafe(
          ggobj = p2,
          options = list(
            opts_hover(css = "fill:yellow;cursor:pointer;"),
            opts_selection(type = "single")))
      }
      else{
        girafe(
          ggobj = p1,
          options = list(
            opts_hover(css = "fill:yellow;cursor:pointer;"),
            opts_selection(type = "single")))
      }
      })
    
    output$compPlot <- renderPlot({
      
      filter_data%>%
        pivot_longer(c(treecover_percent, shrubland_percent, grassland_percent, cropland_percent), names_to='parameter', values_to='value')%>%
        mutate(group = ifelse(added==TRUE, 'Input Sites', 'All Sites'),
               group = factor(group, c('Input Sites', 'All Sites')))%>%
        ggplot()+
        geom_violin(aes(value*100, parameter, fill=group), draw_quantiles = c(0.25, 0.5, 0.75))+
        scale_fill_manual(values=c('red','blue'), drop=FALSE)+
        guides(fill = guide_legend(reverse = TRUE))+
        scale_y_discrete(labels=rev(ynames))+
        labs(x='Percent', y='', fill=NULL)+
        theme_minimal()+
        theme(axis.text.y = element_text(face="bold", colour = "black", size=12),
              axis.title.x = element_text(face="bold", colour = "black", size=12),
              legend.position = 'bottom')
    
    })
    
    
    output$statsTable <- render_gt(
      expr = filter_data%>%
        pivot_longer(c(treecover_percent, shrubland_percent, grassland_percent, cropland_percent), 
                     names_to = 'parameter', values_to = 'value') %>%
        group_by(parameter) %>%
        summarize(
          t_statistic = t.test(value ~ added, alternative=c("two.sided"))$statistic,
          t_test_p = t.test(value ~ added, alternative=c("two.sided"))$p.value)%>%
        arrange(rev(parameter))%>%
        mutate(parameter = ynames)%>%
          gt()%>%
          data_color(columns=t_test_p, fn=palette)
        )
    
    })
  
  output$savefile <- downloadHandler(
    filename = function() {
      paste0("landcover_analyzer_export.csv")
    },
    content = function(file){
      write.csv(filter_data(), file, row.names = FALSE)
    }
  )
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)