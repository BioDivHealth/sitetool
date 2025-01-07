#' step3 UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_step3_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::navset_card_tab(
      sidebar = bslib::sidebar(
        title = 'Step 3. Analyze landcover data and compare sites.',

        width = 350,

        numericInput(ns("radius"),
                     h6("Enter distance from each site center to analyze landcover data (meters):"),
                     min = 1,
                     value = 1000,
                     max = 100000),

        actionButton(ns("goStep3"), "Go"),
        selectInput(ns("measure"), h6("Select a landcover measure to compare"), ""),
        downloadButton(ns("saveFile"), "Save Dataset")
      ),
      bslib::nav_panel(
        title = 'Comparison Plot',
        uiOutput(ns("compPlot"), inline = TRUE)
      ),
      bslib::nav_panel(
        title='Dataset',
        DT::dataTableOutput(ns("lcData"))
      )
    )
  )
}

#' step3 Server Functions
#'
#' @noRd
mod_step3_server <- function(id, sites = NULL, lc_data = NULL, product){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    df <- reactiveVal(NULL)

    observeEvent(input$goStep3, {
      req(sites())
      req(lc_data())

      withProgress(message = "Calculating landcover values", value = 0, {
        if (product() == 'Climate/NDVI') {
          out_df <- createNDVIDataFrame(sites(), raster = lc_data(), dist = input$radius, progress = TRUE)

        } else {
          r = lc_data()
          levels(r) = raster_cats %>%
            subset(product == product())%>%
            dplyr::select(c('value', 'cover'))
          out_df <- createLCDataFrame(sites(), r = r, dist = input$radius, progress = TRUE)
        }

        sites_xy <- sites() %>%
          dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          sf::st_drop_geometry()


        # Add x and y of sites to dataframe
        out_df <- out_df%>%
          dplyr::left_join(sites_xy)

        # Add product being used
        out_df$product = product()

        # Rearrange datafame
        out_df <- out_df%>%
          dplyr::select(c(site, site_id, input_site, longitude, latitude, product, cover, measure, value))

      })
      # Render the data table after processing is complete
      output$lcData <- DT::renderDT({
        out_df
      })

      df(out_df)
    })

    observe({
      req(df())
      if(product() == 'Climate/NDVI'){
        updateSelectInput(session, "measure", choices = unique(df()$cover))
      }
      else{
        updateSelectInput(session, "measure", choices = unique(df()$measure), selected = 'proportion')
      }
    })
    #
    output$compPlot <- renderUI({
      req(df())

      # Organize dataframe for plotting
      d <- df() %>%
        dplyr::mutate(
          group = ifelse(input_site == TRUE, 'Input Sites', 'All Sites'),
          group = factor(group, levels = c('Input Sites', 'All Sites')),
          point_size = ifelse(group == "Input Sites", 1.5, 1),
          point_alpha = ifelse(group == "Input Sites", 0.8, 0.3)
        )

      if (product() == 'Climate/NDVI') {
        d = d%>%
          dplyr::left_join(
            raster_cats%>%
              dplyr::filter(product == product())%>%
              dplyr::select(c(cover, color)) %>%
              dplyr::rename(measure = cover))%>%
          dplyr::mutate(measure = ifelse(measure == 'sd', 'standard deviation', measure))

        purrr::map(unique(d$measure), function(cat) {
          measure_data <- d %>% dplyr::filter(measure == cat)
          tagList(
            ggiraph::renderGirafe({ generate_plot(measure_data, cat, input$cover) }),
            HTML(generate_text(measure_data, cat)),
            hr()
          )
        }) %>% tagList()

      } else {

        d <- d %>%
          dplyr::filter(measure == input$measure) %>%
          dplyr::left_join(
            raster_cats %>%
              dplyr::select(-c(subcover, value)) %>%
              dplyr::filter(product == product()) %>%
              dplyr::group_by(cover) %>%
              dplyr::slice(1),
            by = c("cover")
          )

        # Generate plots for each unique cover
        purrr::map(unique(d$cover), function(cat) {
          cover_data <- d %>%
            dplyr::filter(cover == cat)

          tagList(
            ggiraph::renderGirafe({ generate_plot(cover_data, cat, input$measure) }),
            HTML(generate_text(cover_data, cat)),
            hr()
          )
        }) %>% tagList()
      }
    })
    # Save File: Full dataset  -------------------------------------------------
    output$saveFile <- downloadHandler(
      filename = function() {
        paste0("landcover_analyzer_export.csv")
      },
      content = function(file){

        utils::write.csv(df(), file, row.names = FALSE)
      }
    )

  })
}

## To be copied in the UI
# mod_step3_ui("step3_1")

## To be copied in the server
# mod_step3_server("step3_1")
