## SHINY LANDCOVER APP ##
source('global.R')



# Define UI  -----------------------------------------------------------
ui <- page_navbar(
  
    title =  span("Site Selection Landcover Analyzer", 
                  style={'padding-left:15px'}),

    theme = bs_theme(preset='pulse'),
    
    fillable = FALSE,
    
     nav_panel("Home",
        mapModuleUI('raster'),
      
        sitesUI('getSites'),
        
        plotUI('cplot')
    ),
      
    nav_panel(title = 'About', 
              uiOutput('about'), 
              tags$style(type = "text/css", ".container-fluid {padding-left:20px}")
              )
)

# Define Server ----------------------------------------------------------------
server <- function(session, input, output) {

  # App text -------------------------------------------------------------------
  
  output$about <- renderUI({includeMarkdown("about.md")}) 
  
  # Get landcover data ---------------------------------------------------------
  
  mapData <- mapModuleServer("raster")
  
  # Get input sites ------------------------------------------------------------
  
  sites <- sitesServer("getSites", mapData$bbox, mapData$lc_raster)
  
  # Plot landcover data --------------------------------------------------------
  
  plotServer("cplot", sites, mapData$lc_raster, mapData$product)

  
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
