

# UI ---------------------------------------------------------------------------
plotUI <- function(id) {
  ns <- NS(id)
  navset_card_tab(
    sidebar = sidebar(
      title = 'Step 3. Filter and compare sites.',
      width = 350,
      fileInput(ns("incsv"), 
                label = tagList("Upload previously analyzed landcover data", tags$em("(optional):")),
                accept = c(".csv")),
      h6('Select a measure to compare'),
      selectInput(ns("measure"), "Measurement", "")
    ),
    nav_panel(
      title = 'Comparison Plot',
      uiOutput(ns("compPlot"), inline = TRUE)
    )
  )
}


# Module Server
plotServer <- function(id, indf = NULL) {
  moduleServer(id, function(input, output, session) {

    df <- reactiveVal(NULL)
    
    observeEvent(input$incsv, {
      df(read.csv(input$incsv$datapath))
    })
    
    observe({df(indf())})
    
    observe({
      req(df())
      updateSelectInput(session, "measure", choices = unique(df()$measure), selected = 'proportion')
    })
    
    output$compPlot <- renderUI({
      req(df())
      
      # Organize dataframe for plotting
      d <- df() %>%
        left_join(raster_cats %>% 
                    filter(product == 'Copernicus Global Land Cover') %>% 
                    select(-c(subcover, value)) %>%
                    group_by(cover) %>%
                    slice(1),
                  by = "cover") %>%
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
          renderGirafe({ generate_plot(cover_data, cat, input$measure) }),
          HTML(generate_text(cover_data, cat)),
          hr()
        )
      }) %>% tagList()
    })
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