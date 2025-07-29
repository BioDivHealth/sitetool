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
        ggiraph::girafeOutput(ns("compPlot"))
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

    observeEvent(input$compPlot_selected, {
      update <- df()%>%
        dplyr::mutate(input_site = ifelse(site_id == input$compPlot_selected, !input_site, input_site))

      df(update)
    })

    observeEvent(input$goStep3, {
      req(sites())
      req(lc_data())

      withProgress(message = "Calculating landcover values", value = 0, {

        out_df <- siteRasterStats(sites(), r = lc_data(), dist = input$radius, progress = TRUE)

        sites_xy <- sites() %>%
          dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          sf::st_drop_geometry()


        # Add x and y of sites to dataframe
        out_df <- out_df%>%
          dplyr::left_join(sites_xy, by=c("site", "site_id", "input_site"))

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
      updateSelectInput(session, "measure", choices = unique(df()$measure))
    })

    output$compPlot <- ggiraph::renderGirafe({
      req(df())

      data = df() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(measure == input$measure)%>%
        dplyr::mutate(site_type = factor(
          ifelse(input_site, "Selected Sites", "Generated Sites"),
          levels = c("Selected Sites", "Generated Sites")
        ))

      p <-
        data%>%
        ggplot2::ggplot() +
        ggplot2::geom_boxplot(
          data = data,
          ggplot2::aes(x = site_type,
                       y = value),
          width = 0.2,
          outlier.shape = NA
        ) +
        ggiraph::geom_point_interactive(
          data = data,
          ggplot2::aes(x = site_type,
                       y = value,
                       #       color = ifelse(is.null(selected_site_id) | site_id != selected_site_id, group, "highlight"),
                       tooltip = site,
                       data_id = site_id),
          # size = data$point_size,
          #alpha = data$point_alpha),
          position = ggplot2::position_jitter(seed = 1, width = .3)
        ) +
        ggplot2::scale_fill_identity() +
        ggplot2::facet_wrap(~ cover, ncol = 1, scales = "free") +
        ggplot2::scale_x_discrete(limits = c('Selected Sites', 'Generated Sites')) +
        #   scale_color_manual(values = c("highlight" = "yellow", "Input Sites" = "red", "All Sites" = "black")) +
        ggplot2::coord_flip() +
        ggplot2::labs(x = '', y = '') +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = 'none',
                       plot.title = ggplot2::element_text(face = "bold", hjust=0.5))


      ggiraph::girafe(
        ggobj = p,
        options = list(
          ggiraph::opts_hover(css = "fill-opacity:1;fill:yellow;cursor:pointer;"),
          ggiraph::opts_selection(type = "single", only_shiny=FALSE)
        )
      )
    })

    #
    # output$compPlot <- render({
    #   req(df())
    #
    #   # Organize dataframe for plotting
    #   d <- df() %>%
    #     dplyr::mutate(
    #       group = ifelse(input_site == TRUE, 'Input Sites', 'All Sites'),
    #       group = factor(group, levels = c('Input Sites', 'All Sites')),
    #       point_size = ifelse(group == "Input Sites", 1.5, 1),
    #       point_alpha = ifelse(group == "Input Sites", 0.8, 0.3)
    #     )
    #
    #   if (product() == 'Climate/NDVI') {
    #     d = d%>%
    #       dplyr::left_join(
    #         raster_cats%>%
    #           dplyr::filter(product == product())%>%
    #           dplyr::select(c(cover, color)) %>%
    #           dplyr::rename(measure = cover),
    #         by = c("cover"))%>%
    #       dplyr::mutate(measure = ifelse(measure == 'sd', 'standard deviation', measure))
    #
    #     tagList(
    #       lapply(unique(d$measure), function(cat) {
    #         measure_data <- d[d$measure == cat, ]  # Equivalent to dplyr::filter(measure == cat)
    #
    #         tagList(
    #           ggiraph::renderGirafe({ generate_plot(measure_data, cat, input$cover) }),
    #           HTML(generate_text(measure_data, cat)),
    #           hr()
    #         )
    #       })
    #     )
    #
    #   } else {
    #
    #     d <- d %>%
    #       dplyr::filter(measure == input$measure) %>%
    #       dplyr::left_join(
    #         raster_cats %>%
    #           dplyr::select(-c(subcover, value)) %>%
    #           dplyr::filter(product == product()) %>%
    #           dplyr::group_by(cover) %>%
    #           dplyr::slice(1),
    #         by = c("cover")
    #       )
    #
    #     # Generate plots for each unique cover
    #     tagList(
    #       lapply(unique(d$cover), function(cat) {
    #         cover_data <- d %>%
    #           dplyr::filter(cover == cat)
    #
    #         tagList(
    #           ggiraph::renderGirafe({ generate_plot(cover_data, cat, input$measure) }),
    #           HTML(generate_text(cover_data, cat)),
    #           hr()
    #         )
    #       })
    #     )
    #   }
    # })
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
