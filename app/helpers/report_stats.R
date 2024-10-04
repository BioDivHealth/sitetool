
# Function to perform Mann-Whitney U test and generate summary
generate_wilcox_report <- function(data) {
  data = data%>%subset(measure == 'proportion')
  
  # Create a list to store the results
  results_list <- list()
  
  # Get the unique categories in the dataset
  categories <- unique(data$cover)
  
  # Loop through each category
  for (cat in categories) {
    # Filter data for the specific category

    data_cat <- data %>% filter(cover == cat)
    
    # Split data into input sites and the rest
    input_points_cat <- data_cat %>% filter(input_site == TRUE)
    rest_data_cat <- data_cat %>% filter(input_site == FALSE)
    
    # Check if there are enough points in both groups
    if (nrow(input_points_cat) > 0 && nrow(rest_data_cat) > 0) {
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
        " show sampling bias relative to the rest of the dataset (Mann-Whitney Test: p-value =",
        "<span style='color:", p_value_color, "'>", format(mw_test$p.value, digits=2), "</span>.",
        " The range for the input sites is <span style='color:blue'>", 
        paste(input_range, collapse = '-'), "</span>",
        " and the range for the rest of the dataset is <span style='color:blue'>", 
        paste(rest_range, collapse = '-'), "</span>.",
        " The input sites have a mean (+/- SD) of <span style='color:green'>", 
        input_mean, "</span> +/- <span style='color:green'>", 
        input_sd, "</span>",
        " while the value for the rest of the sites is <span style='color:green'>", 
        rest_mean, "</span> +/- <span style='color:green'>", 
        rest_sd, "</span>."
      )
      # Append the report text to the results list
      results_list[[cat]] <- report_text
    }
  }
  
  return(results_list)
}


# Function to generate interactive plots for each cover
generate_plot <- function(data, measure) {
  # Create the ggplot object
  p <- ggplot() +
    ggdist::stat_halfeye(
      data = data,
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
      aes(x = group, y = value, color = group, tooltip = site, data_id = site_id),
      size = data$point_size,
      alpha = data$point_alpha,
      position = position_jitter(seed = 1, width = .3)
    ) +
    scale_fill_identity() +
    scale_x_discrete(limits = c('Input Sites', 'All Sites')) +
    scale_color_manual(values = c("Input Sites" = "red", "All Sites" = "black")) +
    coord_flip() +
    labs(x = '', y = stringr::str_to_title(measure)) +
    theme_minimal() +
    theme(legend.position = 'none', strip.text = element_text(face = "bold"))
  
  # Interactive portion using girafe
  girafe(
    ggobj = p,
    width_svg = 6,
    height_svg = 10,
    options = list(
      opts_hover(css = "fill-opacity:1;fill:yellow;cursor:pointer;"),
      opts_selection(type = "single")
    )
  )
}
