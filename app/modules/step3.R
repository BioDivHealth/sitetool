

# UI ---------------------------------------------------------------------------
plotUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    sidebar = sidebar(
      title = 'Step 3. Analyze landcover data and compare sites.',
      width = 350,
   #   fileInput(ns("incsv"), 
   #              label = tagList("Upload a list of sites.", tags$em("(optional):")),
   #             accept = c(".csv")),
    
      numericInput(ns("radius"), 
                   h6("Enter distance from each site center to analyze landcover data (meters):"), 
                   value = 1000),
     
      actionButton(ns("goStep3"), "Go"),
      selectInput(ns("measure"), h6("Select a landcover measure to compare"), ""),
      downloadButton(ns("saveFile"), "Save Dataset")
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
          out_df <- createNDVIDataFrame(sites(), r = lc_data(), type = product(), dist = input$radius, progress = TRUE)
        } else {
          # alter to more general cover name
          r = lc_data()
          levels(r) = raster_cats %>%
            subset(product == product())%>%
            select(c('value', 'cover'))
          out_df <- createLCDataFrame(sites(), r = r, dist = input$radius, progress = TRUE)
        }

        sites_xy <- sites() %>%
          mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          st_drop_geometry()
        

        # Add x and y of sites to dataframe
        out_df <- out_df%>%
          left_join(sites_xy, by=c('site_id'))

        # Add product being used
        out_df$product = product()
        
        # Rearrange datafame
        out_df <- out_df%>%
          select(c(site, site_id, input_site, longitude, latitude, product, cover, measure, value))
        
      })
      # Render the data table after processing is complete
      output$lcData <- DT::renderDT({
          out_df
      })

      df(out_df)
    })
    
    observe({
      req(df())
      updateSelectInput(session, "measure", choices = unique(df()$measure), selected = 'proportion')
    })
    # 
    output$compPlot <- renderUI({
      req(df())

      # Organize dataframe for plotting
      d <- df() %>%
        subset(measure == input$measure)%>%
        left_join(raster_cats %>% 
                    select(-c(subcover, value)) %>%
                    subset(product = product())%>%
                    group_by(cover) %>%
                    slice(1),
                  by = c("cover")) %>%
        mutate(
          group = ifelse(input_site == TRUE, 'Input Sites', 'All Sites'),
          group = factor(group, levels = c('Input Sites', 'All Sites')),
          point_size = ifelse(group == "Input Sites", 1.5, 1),
          point_alpha = ifelse(group == "Input Sites", 0.8, 0.3)
        )

      

      # Generate plots for each unique cover type
      purrr::map(unique(d$cover), function(cat) {
        cover_data <- d %>% filter(cover == cat)
        
        tagList(
          renderGirafe({ generate_plot(cover_data, cat, input$measure) }),
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
