# Packages  -------------------------------------------------------------------
if (!require("Require")) {
  install.packages("Require")
  require("Require")
}

Require(c("shiny", "shinyWidgets", "ggplot2", "bslib", "leaflet", "dplyr", "tidyr",
          "terra", "ggiraph", "sf", "osmdata", "gargoyle", "gt", "ggdist"))

# Functions and modules --------------------------------------------------------
source('helpers/CalculateLandCover.R')
source('helpers/report_stats.R')
source('helpers/get_landuse_data.R')
source('helpers/get_points.R')
source('modules/mapModule.R')
source('modules/landcoverModule.R')
source('modules/plotModule.R')
source('modules/core_mapping.R')

# Global vars and options  ----------------------------------------------------

# Increase max upload size for large raster data
options(shiny.maxRequestSize=2000*1024^2)

# Labels for land cover categories
raster_cats = read.csv('data/categories.csv')

# Shapefiles for cropping and getting points
land_boundaries = st_read('data/ne_50m_land/ne_50m_land.shp', quiet=T)
lake_boundaries = st_read('data/ne_50m_lakes/ne_50m_lakes.shp', quiet=T)