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
      full_screen = TRUE,
      sidebar = bslib::sidebar(
        title = 'Step 3. Analyze landcover data and compare sites.',

        width = 350,

        numericInput(ns("radius"),
                     h6("Enter distance from each site center to analyze raster data (meters):"),
                     min = 1,
                     value = 1000,
                     max = 100000),

        actionButton(ns("goStep3"), "Go"),

        #selectInput(ns("measure"), h6("Select a landcover measure to compare"), "")
      ),
      bslib::nav_panel(
        title = 'Summary Plot',
        tagList(
          tags$div(
            style = "color: red; text-align: center; margin-bootm: 10px; font-style: italic;",
            "Click on a point in the plot to switch between a selected and generated site."),
          div(
            style = "height:800px; overflow-y:scroll;",
            ggiraph::girafeOutput(ns("compPlot"))
          )
        )
      ),
      bslib::nav_panel(
        title='Statistical Comparison',
        uiOutput(ns('stats'), inline = TRUE)
      ),
      bslib::nav_panel(
        title='Selected Sites',
        tagList(
          div(
            style = "text-align:right; margin-bottom:10px;",
            downloadButton(ns("saveSelected"), "Save Dataset")
          ),
          DT::dataTableOutput(ns("selectedTable"))
        )
      ),
      bslib::nav_panel(
        title='Full Dataset',
        tagList(
          div(
            style = "text-align:right; margin-bottom:10px;",
            downloadButton(ns("saveFile"), "Save Dataset")
          ),
          DT::dataTableOutput(ns("fullTable"))
        )
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
    summary <- reactiveVal(NULL)

    # reset df if sites are reset
    observe({
      if(is.null(sites())){
        df(NULL)
      }
      if(is.null(lc_data())){
        df(NULL)
      }
    })

    # reactivity for selecting points
    observeEvent(input$compPlot_selected, {
      update <- df()%>%
        dplyr::mutate(input_site = ifelse(site_id == input$compPlot_selected, !input_site, input_site))

      df(update)
    })

    observeEvent(input$goStep3, {
      req(sites())
      req(lc_data())

      withProgress(message = "Calculating landcover values", value = 0, {

        out_df <- lapply(names(lc_data()), function(rname) {

          r <- lc_data()[[rname]]

          df <- siteRasterStats(
            in_df = sites(),
            raster = r,
            dist = input$radius,
            progress = TRUE
          )


          # Add column 'product' with the raster name
          df$product <- rname

          if(!is.null(terra::coltab(r))){
            df$type = 'categorical'
          }
          else{
            df$type = 'numeric'
            df$cover = df$product
          }

          return(df)
        }) %>% dplyr::bind_rows()  # combine into single data.frame

        sites_xy <- sites() %>%
          dplyr::mutate(longitude = sf::st_coordinates(.)[,1],
                 latitude = sf::st_coordinates(.)[,2]) %>%
          sf::st_drop_geometry()

        # Add x and y of sites to dataframe
        out_df <- out_df%>%
          dplyr::left_join(sites_xy, by=c("site", "site_id", "input_site"))

        # Rearrange datafame
        out_df <- out_df%>%
          dplyr::select(c(site, site_id, input_site, longitude, latitude, product, cover, measure, value, type))

      })

      df(out_df)

      sum_df <- lapply(names(lc_data()), function(rname) {
        r <- lc_data()[[rname]]

        summarizeRaster(r, rname)
      }) %>% dplyr::bind_rows()
      summary(sum_df)
    })

    output$fullTable <- DT::renderDT({
      req(df())
      df()
    })

    output$selectedTable <- DT::renderDT({
      req(df())
      display = df()%>%
        dplyr::filter(input_site == TRUE)

      display
    })


    # observe({
    #   req(df())
    #   updateSelectInput(session, "measure", choices = unique(df()$measure))
    # })

    output$compPlot <- ggiraph::renderGirafe({
      req(df())

      df_plot = df() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(measure %in% c('mean', 'proportion'))%>%
        dplyr::mutate(site_type = factor(
          dplyr::if_else(input_site, "Selected Sites", "Generated Sites"),
          levels = c("Selected Sites", "Generated Sites")
        ))

      p <- ggplot2::ggplot(df_plot, ggplot2::aes(x = value, y = site_type, color = site_type, fill = site_type)) +
        ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.4) +
        ggiraph::geom_point_interactive(
          ggplot2::aes(
            tooltip = paste0(
              "Site: ", site,
              "\nID: ", site_id,
              "\nValue: ", round(value, 2)
            ),
            data_id = site_id,        # needed for hover interactivity
            fill = site_type,         # fill controls point color
            color = site_type
          ),
          position = ggplot2::position_jitter(height = 0.2, width = 0),
          alpha = 0.7,
          size = 2
        ) +
        ggplot2::scale_color_manual(
          values = c("Selected Sites" = "tomato", "Generated Sites" = "blue")
        ) +
        ggplot2::scale_fill_manual(
          values = c("Selected Sites" = "tomato", "Generated Sites" = "blue")
        ) +
        ggplot2::facet_wrap(~cover, ncol = 1, scales = "free_x") +
        ggplot2::theme_minimal(base_size = 14) +
        ggplot2::theme(
          strip.text.y = ggplot2::element_text(angle = 0, hjust = 0),
          axis.text.y = ggplot2::element_text(hjust = 1),
          legend.position = "none"
        ) +
        ggplot2::labs(
          x = "Value",
          y = ""
        )

      ggiraph::girafe(
        ggobj = p,
        width_svg = 8,
        height_svg = max(6, length(unique(df_plot$cover)) * 2),
        options = list(
          ggiraph::opts_hover(css = "fill-opacity:1;fill:yellow;cursor:pointer;r:5px;"),
          ggiraph::opts_selection(type = "single", only_shiny = FALSE)
        )
      )

    })


    output$stats <- renderUI({
      req(df())

      d <- df() %>%
        dplyr::filter(measure %in% c('mean', 'proportion'))

      # Generate plots for each unique cover
      tagList(
        lapply(unique(d$cover), function(cat) {
          cover_data <- d %>%
            dplyr::filter(cover == cat)
          selected_measure = cover_data$measure[1]

          tagList(
            HTML(generate_text(cover_data, cat, selected_measure)),
            hr()
          )
        })
      )
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


    # Save File: Selected  -------------------------------------------------
    output$saveSelected <- downloadHandler(
      filename = function() {
        paste0("selected_sites.csv")
      },
      content = function(file){
        out = df()%>%
          dplyr::filter(input_site == TRUE)
        utils::write.csv(out, file, row.names = FALSE)
      }
    )

    return(df)
  })
}

## To be copied in the UI
# mod_step3_ui("step3_1")

## To be copied in the server
# mod_step3_server("step3_1")
