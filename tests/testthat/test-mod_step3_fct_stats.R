test_generate_plot <- function() {
  # Create mock data
  mock_data <- data.frame(
    input_site = c(FALSE, TRUE, FALSE, FALSE, TRUE),
    group = c("Input Sites", "All Sites", "Input Sites", "All Sites", "Input Sites"),
    value = c(10, 15, 12, 20, 8),
    color = c("red", "blue", "red", "blue", "red"),
    site = c("Site1", "Site2", "Site3", "Site4", "Site5"),
    site_id = 1:5,
    point_size = c(2, 2, 2, 2, 2),
    point_alpha = c(0.5, 0.5, 0.5, 0.5, 0.5)
  )

  base_plot <- generate_ggplot(mock_data, cat = "landcover", measure = "comparison")

    test_that("generate_plot returns a girafe object", {
      interactive_plot = generate_plot(mock_data, cat = "landcover", measure = "comparison")
      expect_s3_class(interactive_plot, "girafe")
    })

  test_that("generate_plot correctly filters data", {
    filtered_data <- mock_data %>% dplyr::filter(input_site == FALSE)
    expect_equal(nrow(filtered_data), 3, info = "Filtered data should exclude input_site = TRUE.")
  })

  test_that("generate_plot includes boxplot layer", {
    expect_true(any(sapply(base_plot$layers, function(layer) {
      inherits(layer$geom, "GeomBoxplot")
    })), info = "ggplot object should include boxplot.")
  })

  test_that("generate_plot includes slab interval layer", {
    expect_true(any(sapply(base_plot$layers, function(layer) {
      inherits(layer$geom, "GeomSlabinterval")
    })), info = "ggplot object should include .")
  })

  test_that("generate_plot sets the correct plot title", {
    expect_equal(
      base_plot$labels$title,
      "Landcover Comparison",
      info = "Plot title should match the specified category and measure."
    )
  })

  test_that("generate_plot fills histogram correctly", {
    expect_true("fill" %in% names(base_plot$layers[[1]]$mapping))
  })

  test_that("generate_plot colors histogram correctly", {
    expect_equal(
      sort(unique(ggplot2::ggplot_build(base_plot)$data[[1]]$fill)),
      sort(unique(mock_data$color))
    )
  })
}

test_generate_plot()

