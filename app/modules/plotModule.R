

# UI ---------------------------------------------------------------------------
plotUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    sidebar = sidebar(
      title = 'Step 3. Analyze landcover data and compare sites.',
      width = 350,
      fileInput(ns("incsv"), 
                label = tagList("Upload previously analyzed landcover data", tags$em("(optional):")),
                accept = c(".csv")),
    #  h6('Select a measure to compare'),
     # 
      numericInput("radius", 
                   "Enter distance from each site center to analyze landcover data (meters):", 
                   value = 1000),
    #selectInput(ns("measure"), "Measurement", ""),
      actionButton(ns("goStep3"), "Go"),
      downloadButton(ns("saveFile"), "Save File")
    ),
    nav_panel(
      title = 'Comparison Plot',
      uiOutput(ns("compPlot"), inline = TRUE)
    ),
    nav_panel(
      title='Dataset',
      DT::dataTableOutput(ns("lcData"))
    )
  )
}


# Module Server
plotServer <- function(id, sites = NULL, lc_data = NULL, product) {
  moduleServer(id, function(input, output, session) {

    df <- reactiveVal(NULL)
    
    observeEvent(input$incsv, {
      sites(read.csv(input$incsv$datapath))
    })
  
    observeEvent(input$goStep3, {
      req(sites())
      req(lc_data())

      withProgress(message = "Calculating landcover values", value = 0, {
        if (product() == 'NDVI') {
          out_df <- createNDVIDataFrame(sites(), r = lc_data(), type = product(), dist = 1000, progress = TRUE)
        } else {
          out_df <- createLCDataFrame(sites(), r = lc_data(), dist = 1000, progress = TRUE)
        }
        str(out_df)
        sites_xy <- sites() %>%
          mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          st_drop_geometry()
        
        str(sites_xy)
        # Add x and y of sites to dataframe
        out_df <- out_df %>%
          left_join(sites_xy, by=c('site_id'))
        str(out_df)
        # Add product being used
        out_df$product = product()
        
      })
      # Render the data table after processing is complete
      output$lcData <- DT::renderDT({
        out_df%>%
          pivot_wider(names_from = c(cover, measure), values_from = value)
      })
      str(out_df)
      df(out_df)
    })
    
    # observe({
    #   req(df())
    #   updateSelectInput(session, "measure", choices = unique(df()$measure), selected = 'proportion')
    # })
    # 
    output$compPlot <- renderUI({
      req(df())
      print('IM TRYING TO MAKE A PLOT')
      # Organize dataframe for plotting
      d <- df() %>%
        left_join(raster_cats %>% 
                    select(-c(subcover, value)) %>%
                    group_by(cover) %>%
                    slice(1),
                  by = c("cover", "product")) %>%
        mutate(
          group = ifelse(input_site == TRUE, 'Input Sites', 'All Sites'),
          group = factor(group, levels = c('Input Sites', 'All Sites')),
          point_size = ifelse(group == "Input Sites", 3, 1),
          point_alpha = ifelse(group == "Input Sites", 0.8, 0.3)
        )
      
      # Generate plots for each unique cover type
      purrr::map(unique(d$cover), function(cat) {
        cover_data <- d %>% filter(cover == cat)
        
        tagList(
          renderGirafe({ generate_plot(cover_data, cat, 'proportion') }),
          HTML(generate_text(cover_data, cat)),
          hr()
        )
      }) %>% tagList()
    })
    
    # Save File: Full dataset  -------------------------------------------------
    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("landcover_analyzer_export.csv")
      },
      content = function(file){
        write.csv(df(), file, row.names = FALSE)
      }
    )
    
  })
}


# extras

# Plot everything  -----------------------------------------------------------


# observe({
#   
#   df = df()%>%
#      subset(measure == input$measure)#%>%
#     # filter(
#     #   (cover == "builtup" & between(value, input$BuiltUpSelect[1], input$BuiltUpSelect[2])) |
#     #     (cover == "treecover" & between(value, input$TreeSelect[1], input$TreeSelect[2])) |
#     #     (cover == "cropland" & between(value, input$CropSelect[1], input$CropSelect[2])) |
#     #     (cover == "grassland" & between(value, input$GrassSelect[1], input$GrassSelect[2])) |
#     #     (cover == "shrubland" & between(value, input$ShrubSelect[1], input$ShrubSelect[2]))
#     # )
# })
# 
# 