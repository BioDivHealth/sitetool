# Site Selection Analyzer

### Overview
A tool to produce a list of potential field sites, and assess the characteristics of your potential field sites relative to other possible field sites in the same area. 

### Features:
- Generate a list of potential field sites in a selected area. 
- Analyze landcover characteristics using the ESA WorldCover data. 
- Filter and compare sites based on landcover parameters.
- Perform statistical comparisons to assess your potential sites

### Setup and Installation
To run this Shiny app, please ensure you have an up-to-date installation of R. The following code will install the required packages:

```
if (!require("Require")) {
  install.packages("Require")
  require("Require")
}

Require(c("shiny", "ggplot2", "bslib", "leaflet", "dplyr", "tidyr", "shinyWidgets", "ggiraph", 
          "sf", "osmdata", "gargoyle", "gt"))
```

### Running the App
To run the app, use the following command in R:
`shiny::runApp()`

### Limitations/Future Additions
- Currently only supports one type of landcover data, plan to expand to other input data sources
- Can only select a small geographic area when using the village option
