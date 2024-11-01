## SHINY LANDCOVER APP ##
source('global.R')


# Define UI  -----------------------------------------------------------
ui <- page_navbar(
  
    title =  span("Site Selection Landcover Analyzer", 
                  style={'padding-left:15px'}),

    theme = bs_theme(preset='pulse'),
    
    
    fillable = FALSE,
    
     nav_panel("Home",
        mapModuleUI('mapInputs'),
      
        landcoverInput('lc'),
        
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
  
  # Get input sites ------------------------------------------------------------
  
  sites <- mapModuleServer("mapInputs")
  
  # Get landcover data ---------------------------------------------------------

  df <- landcoverOutput("lc", sites)
  
  
  # Plot landcover data --------------------------------------------------------
  
  plotServer("cplot", df)

  
}

# Run the application ----------------------------------------------------------
shinyApp(ui = ui, server = server)
