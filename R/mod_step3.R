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
                     h6("Enter distance from each site center to analyze raster data (meters):"),
                     min = 1,
                     value = 1000,
                     max = 100000),

        actionButton(ns("goStep3"), "Go")
        #selectInput(ns("measure"), h6("Select a landcover measure to compare"), "")
      ),
      bslib::nav_panel(
        title = 'Summary Plot',
        tagList(
          tags$div(
            style = "color: red; text-align: center; margin-bootm: 10px; font-style: italic;",
            "Click on a point in the plot to switch between a selected and generated site."),
          ggiraph::girafeOutput(ns("compPlot"), height = "800px")
        )
      ),
      bslib::nav_panel(
        title='Statistical Comparison',
        uiOutput(ns('stats'), inline = TRUE)
      ),
      bslib::nav_panel(
        title='Selected Sites',
        tagList(
          DT::dataTableOutput(ns("selectedTable")),
          downloadButton(ns("saveSelected"), "Save Dataset")
        )
      ),
      bslib::nav_panel(
        title='Full Dataset',
        tagList(
          DT::dataTableOutput(ns("fullTable")),
          downloadButton(ns("saveFile"), "Save Dataset")
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

        out_df <- siteRasterStats(sites(), raster = lc_data(), dist = input$radius, progress = TRUE)

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

      df(out_df)

      # get summary values
      out = summarizeRaster(lc_data())

      summary(out)
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
      if(terra::is.factor(lc_data())){
        selected_measure = 'proportion'
      }
      else{
        selected_measure = 'mean'
      }

      data = df() %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::filter(measure == selected_measure)%>%
        dplyr::mutate(site_type = factor(
          dplyr::if_else(input_site, "Selected Sites", "Generated Sites"),
          levels = c("Selected Sites", "Generated Sites")
        ))

      # Count how many unique 'cover' classes you have
      data$cover <- factor(data$cover)
      cover_levels <- levels(data$cover)
      data$x_base <- as.numeric(data$cover)

      cover_lines <- summary() %>%
        dplyr::mutate(
          x_base = match(cover, cover_levels),
          x_start = x_base - 0.25,
          x_end = x_base + 0.25
        )

      data_boxplot <- data %>% dplyr::mutate(geom_type = "boxplot")
      data_points <- data %>% dplyr::mutate(geom_type = "points")

      data_combined <- dplyr::bind_rows(data_boxplot, data_points)

      # Now add your x_plot offsets, jitter etc on data_combined
      data_combined <- data_combined %>%
        dplyr::mutate(
          x_plot = dplyr::case_when(
            site_type == "Selected Sites" & geom_type == "boxplot" ~ x_base + 0.25,
            site_type == "Selected Sites" & geom_type == "points"  ~ x_base + 0.15,  # offset further right
            site_type == "Generated Sites" & geom_type == "boxplot" ~ x_base - 0.15,
            site_type == "Generated Sites" & geom_type == "points"  ~ x_base - 0.25,  # offset further left
            TRUE ~ x_base
          ),
          x_plot_jitter = ifelse(
            geom_type == "points",
            x_plot + runif(nrow(data_combined), min = -0.05, max = 0.05),
            x_plot
          ),
          y_plot_jitter = ifelse(
            geom_type == "points",
            value + runif(nrow(data_combined), min = -0.05, max = 0.05),
            value
          )
        )

      # Number of facets for sizing
      n_covers <- length(cover_levels)

      text_size <- if(n_covers == 1) 8 else 14

      # Plot
      p <- ggplot2::ggplot(data_combined) +
        # Boxplots (aligned with site_type positions)
        ggplot2::geom_boxplot(
          data = data_combined %>% dplyr::filter(geom_type == "boxplot"),
          ggplot2::aes(
            x = x_plot,
            y = value,
            fill = site_type,
            group = interaction(site_type, cover)
          ),
          width = 0.15
        ) +
        # Interactive points (on same x as boxplots)
        ggiraph::geom_point_interactive(
          data = data_combined %>% dplyr::filter(geom_type == "points")%>%
            dplyr::distinct(site_id, site_type, cover, .keep_all = TRUE),
          ggplot2::aes(
            x = x_plot_jitter,
            y = y_plot_jitter,
            color = site_type,
            tooltip = site,
            data_id = site_id
          ),
          size = 3,
          shape = 21,
          fill = "white",
          alpha = 0.8
        ) +
        ggplot2::geom_segment(
          data = cover_lines,
          ggplot2::aes(x = x_start, xend = x_end, y = value, yend = value,
                       linetype= "Mean of Sampling Area"),
          color = "black",
          linewidth = 1
        ) +
        # Custom x-axis with labels in center
        ggplot2::scale_x_continuous(
          breaks = base::seq_along(cover_levels),
          labels = cover_levels
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = NULL, y = stringr::str_to_title(selected_measure)) +
        ggplot2::scale_linetype_manual(
          name = NULL,
          values = c("Mean of Sampling Area" = "dashed")
        ) +
        ggplot2::scale_fill_manual(values = c("Selected Sites" = "tomato", "Generated Sites" = "blue")) +
        ggplot2::scale_color_manual(values = c("Selected Sites" = "tomato", "Generated Sites" = "blue")) +
        ggplot2::theme(
          axis.text = ggplot2::element_text(size = text_size),
          axis.title = ggplot2::element_text(size = text_size),
          legend.position = "top",
          legend.text = ggplot2::element_text(size = text_size),
          legend.title = ggplot2::element_blank(),
          #panel.spacing = grid::unit(10, "lines"),
          #strip.text = ggplot2::element_blank(),
        #  strip.background = ggplot2::element_blank(),
         # plot.margin = grid::unit(c(0.5, 0.5, 0.5, 0.5), "cm"),  # top, right, bottom, left

        )

      # Interactive output
      ggiraph::girafe(
        ggobj = p,
        width_svg = 6,
        height_svg = n_covers * 1.5,
        options = list(
          ggiraph::opts_hover(css = "fill-opacity:1;fill:orange;cursor:pointer;"),
          ggiraph::opts_selection(type = "single", only_shiny = FALSE)
        )
      )
    })


    output$stats <- renderUI({
      req(df())
      if(terra::is.factor(lc_data())){
        selected_measure = 'proportion'
      }
      else{
        selected_measure = 'mean'
      }

      d <- df() %>%
        dplyr::filter(measure == selected_measure)

      # Generate plots for each unique cover
      tagList(
        lapply(unique(d$cover), function(cat) {
          cover_data <- d %>%
            dplyr::filter(cover == cat)

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
