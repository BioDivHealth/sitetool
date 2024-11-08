
# Function to perform Mann-Whitney U test and generate summary
generate_text <- function(data, cat) {
    # Split data into input sites and the rest
    input_points_cat <- data %>% filter(input_site == TRUE)
    rest_data_cat <- data %>% filter(input_site == FALSE)
    
    # Check if there are enough points in both groups
    if (!(nrow(input_points_cat) > 0 && nrow(rest_data_cat)) > 0) {
      return(NULL)
    }
    # Perform Mann-Whitney U test
    mw_test <- wilcox.test(input_points_cat$value, rest_data_cat$value, exact=FALSE)
    
    # Get range and means
    input_range <- format(range(input_points_cat$value, na.rm = TRUE), digits=2)
    rest_range <- format(range(rest_data_cat$value, na.rm = TRUE), digits=2)
    input_mean <- format(mean(input_points_cat$value, na.rm = TRUE), digits=2)
    input_sd <- format(sd(input_points_cat$value, na.rm = TRUE), digits=2)
    rest_mean <- format(mean(rest_data_cat$value, na.rm = TRUE), digits=2)
    rest_sd <- format(sd(rest_data_cat$value, na.rm = TRUE), digits=2)
    
    # determine color and direction
    bias_result <- ifelse(mw_test$p.value < 0.05, "do", "do not")
    p_value_color <- ifelse(bias_result == "do", "red", "green")

    # Create formatted text with colored numbers
    report_text <- paste0(
      "The input points for <b>", cat, " proportion </b>", 
      "<b><span style='color:", p_value_color, "'>", bias_result,  "</span></b>",
      " show sampling bias relative to the rest of the dataset (Mann-Whitney Test: p-value = ",
      "<span style='color:", p_value_color, "'>", format(mw_test$p.value, digits=2), "</span>.",
      "). The range for the input sites is <span style='color:blue'>", 
      paste(input_range, collapse = '-'), "</span>",
      " and the range for the rest of the dataset is <span style='color:blue'>", 
      paste(rest_range, collapse = '-'), "</span>.",
      " The input sites have a mean (+/- SD) of <span style='color:green'>", 
      input_mean, "</span> +/- <span style='color:blue'>", 
      input_sd, "</span>",
      " while the value for the rest of the sites is <span style='color:green'>", 
      rest_mean, "</span> +/- <span style='color:blue'>", 
      rest_sd, "</span>."
    )

  return(report_text)
}


# Function to generate interactive plots for each cover
generate_plot <- function(data, cat, measure) {
  
  # Create the ggplot object
  p <- ggplot() +
    ggdist::stat_halfeye(
      data = data%>% filter(input_site == FALSE),
      aes(x = group, y = value, fill = color),
      adjust = .7,
      width = 5,
      .width = c(0.5, 0.8),
      justification = -.1,
      point_colour = NA
    ) +
    geom_boxplot(
      data = data,
      aes(x = group, y = value),
      width = 0.7,
      outlier.shape = NA
    ) +
    geom_point_interactive(
      data = data,
      aes(x = group, y = value, 
 #         color = ifelse(is.null(selected_site_id) | site_id != selected_site_id, group, "highlight"), 
          tooltip = site, 
          data_id = site_id),
      size = data$point_size,
      alpha = data$point_alpha,
      position = position_jitter(seed = 1, width = .3)
    ) +
    scale_fill_identity() +
    scale_x_discrete(limits = c('Input Sites', 'All Sites')) +
 #   scale_color_manual(values = c("highlight" = "yellow", "Input Sites" = "red", "All Sites" = "black")) +
    coord_flip() +
    labs(x = '', y = '', 
         title = paste(stringr::str_to_title(cat), measure)) +
    theme_minimal() +
    theme(legend.position = 'none', 
          plot.title = element_text(face = "bold", hjust=0.5))
          # plot.margin = margin(t = 0,  # Top margin
          #                      r = 0,  # Right margin
          #                      b = 0,  # Bottom margin
          #                      l = -5)) # Left margin 
          # 
  
  # # Interactive portion using girafe
  girafe(
    ggobj = p,
    options = list(
   #   width_svg = 4,
    #  height_svg = 2,
    #  opts_sizing(rescale = FALSE, width=0.1),
      opts_hover(css = "fill-opacity:1;fill:yellow;cursor:pointer;"),
      opts_selection(type = "single")
    )
  )
}
