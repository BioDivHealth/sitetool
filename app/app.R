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
        numericInput("radius", "Enter radius from site center to analyze landcover (meters):", value = 1000),
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
        h6('X-Axis'),
        selectInput("x_lc", "X Landcover Category", ""),  # X-axis label choice
        selectInput("x_m", "X Measurement", ""),  # X-axis measurement choice
        
        h6('Y-Axis'),
        selectInput("y_lc", "Y Landcover Category", ""),  # X-axis label choice
        selectInput("y_m", "Y Measurement", ""),  # X-axis measurement choice
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
        checkboxInput('log_scale','Log Scale?')
       # actionButton("goStep3", "Go")
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
  
  ## Reactive Values ##
  sites <- reactiveVal(NULL)
  raster <- reactiveVal(NULL)
  bounds <- reactiveVal(NULL)
  df <- reactiveVal(NULL)
  filter_data <- reactiveVal(NULL)
  mapvals <- reactiveValues(poly = NULL)
  
  ## Base Map ##
  map = core_mapping_module_server("siteMap", mapvals)
  
  ## Output Text ##
  output$instructions <- renderUI({includeMarkdown("instructions.md")})
  
  #### STEP 1 SERVER ####
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
  
      if(!is.null(input$csvfile)){
        candidate = read.csv(input$csvfile$datapath)
        candidate$input_site= TRUE
        candidate$site_id = paste0('input_',1:3)
        candidate = st_as_sf(candidate, coords = c("longitude", "latitude"),
                 crs = "epsg:4326")%>%
          select(c(site, site_id, input_site))
  
        sites_filter = rbind(candidate, sites_filter)
      }
  
      # store sites for later use
      sites(sites_filter)
      
      output$siteCount <- renderText({
          nrow(sites())
      })
      
      # update map with selected sites
      map_points(map, mapvals, sites())

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
    
    output$landcoverMap <- renderPlot({
      plot(r)
      plot(st_geometry(sites()), col="white", pch=8, add=T)
    })

    withProgress(message = "Calculating landcover values", value=0, {
      out_df = createLCDataFrame(sites(), raster=raster(), dist=input$radius, progress=T)
      
      df(out_df)
    })
  })
  
  observe({
    updateSelectInput(session, "x_lc", choices=unique(df()$cover), select=unique(df()$cover[1]))
    updateSelectInput(session, "y_lc", choices=unique(df()$cover), select=unique(df()$cover[2]))
    updateSelectInput(session, "x_m", choices=unique(df()$measure), select='proportion')
    updateSelectInput(session, "y_m", choices=unique(df()$measure), select='proportion')
  })

  #### STEP 3 SERVER ####
  observeEvent(list(input$landcsv, input$x_lc, input$y_lc, input$x_m, input$y_m), {
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
      ungroup()%>%
      filter((cover == input$x_lc & measure == input$x_m) |
             (cover == input$y_lc & measure == input$y_m)) %>%
      mutate(cover_measure = paste0(cover, measure)) %>%  # Create a new column
      select(-c(measure, cover))%>%
      pivot_wider(names_from = cover_measure, values_from = value)
    
    str(filter_data)
    paste0(input$x_lc, input$x_m)

    output$gradientPlot<- renderGirafe({
      p1 <- filter_data%>%
        ggplot(aes(x =!!sym(paste0(input$x_lc, input$x_m)), 
                   y = !!sym(paste0(input$y_lc, input$y_m))))+
        geom_point_interactive(aes(tooltip = site, data_id=site_id, color=input_site))+
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
      
      df()%>%
        subset(measure == 'proportion')%>%
        subset(cover %in% c('treecover', 'shrubland', 'grassland', 'cropland', 'builtup'))%>%
        mutate(group = ifelse(input_site==TRUE, 'Input Sites', 'All Sites'),
               group = factor(group, c('Input Sites', 'All Sites')))%>%
        ggplot()+
        geom_violin(aes(value*100, cover, fill=group), draw_quantiles = c(0.25, 0.5, 0.75))+
        scale_fill_manual(values=c('red','blue'), drop=FALSE)+
        guides(fill = guide_legend(reverse = TRUE))+
        # scale_y_discrete(labels=rev(ynames))+
        labs(x='Percent', y='', fill=NULL)+
        theme_minimal()+
        theme(axis.text.y = element_text(face="bold", colour = "black", size=12),
              axis.title.x = element_text(face="bold", colour = "black", size=12),
              legend.position = 'bottom')
      
    
    })
    
    
    # output$statsTable <- render_gt(
    #   expr = df()%>%
    #     pivot_longer(c(treecover_percent, shrubland_percent, grassland_percent, cropland_percent), 
    #                  names_to = 'parameter', values_to = 'value') %>%
    #     group_by(parameter) %>%
    #     summarize(
    #       t_statistic = t.test(value ~ input_site, alternative=c("two.sided"))$statistic,
    #       t_test_p = t.test(value ~ input_site, alternative=c("two.sided"))$p.value)%>%
    #     arrange(rev(parameter))%>%
    #     mutate(parameter = ynames)%>%
    #       gt()%>%
    #       data_color(columns=t_test_p, fn=palette)
    #     )
    
    })
  
  output$savefile <- downloadHandler(
    filename = function() {
      paste0("landcover_analyzer_export.csv")
    },
    content = function(file){
      write.csv(df(), file, row.names = FALSE)
    }
  )
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)