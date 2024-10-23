# Site Selection Landcover Analyzer

### Overview
A tool to produce a list of potential field sites and assess the landcover characteristics of the sites. If you have already selected potential sites, this tool allows you to assess how your sites fall within the landcover distribution of the wider area. 

#### Features:
- Generate a list of potential field sites in a selected area. 
- Analyze landcover characteristics for a list of sites.
- Filter and compare sites based on landcover parameters.
- Perform statistical comparisons to between potential sites. 

The app is available in a browser version at: https://nimirz.shinyapps.io/ss-analyzer/. However, the app will run best when run on your local computer by following the below instructions.  

### Setup and Installation
To run this Shiny app locally, please ensure you have an up-to-date installation of R. The following code will install the required packages:

```
if (!require("Require")) {
  install.packages("Require")
  require("Require")
}

Require(c("shiny", "shinyWidgets", "ggplot2", "bslib", "leaflet", "dplyr", "tidyr", "terra", "ggiraph", 
           "sf", "osmdata",  "ggdist"))
```

Next, please clone the repository to your local drive by executing the following command:
```
git clone https://github.com/BioDivHealth/ss-analyzer/
```

### Running the App
In R, navigate to the folder where you cloned the repository, and within the `app` folder use the the following command:
`shiny::runApp()`

If running on RStudio, open the `app/app.R` file and press **"Run App"** in the upper right corner of your IDE. 

The app should open in an external browser window. 

### Using the App
There are 3 main steps to using the app:
1. Select Sites
2. Analyze Landcover Data
3. Visualize Results

### Saving Results
After completing step 2, you can download a list of potential sites with the analyzed landcover characteristics by clicking the **Save File** button. This will download a csv named *landcover_analyzer_export.csv*. To skip the analysis step when returning to the app, you can upload this file in step 3. The plots in step 3 can be downloaded by clicking the icon in the upper right corner of the plot.   
