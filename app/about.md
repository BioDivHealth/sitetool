### Overview



The aim of this tool is to help researchers select and assess potential field sites for studies on ecology, public health, anthropology, and more, in a quantitative way. Often, researchers aim to have field sites that exist along a gradient of different land use types and characteristics. This tool allows researchers to analyze the landcover characteristics of a list of sites, and see where potential sites fall along a gradient of other potential sites.  

There are two main ways to use this app:
1. Generate a list of potential sites within a bounding box and analyze the landcover characteristics for each site based on an uploaded raster.
2. Check if previously selected field sites are biased by comparing landcover characteristics to other potential sites in the same area.

By using this tool, researchers may uncover previously unconsidered field sites as well as ensure that their chosen sites exist along a landcover gradient relevant to their question of interest.

#### Input Raster Sources

You can use the default landcover raster (Copernicus Land Cover 100m) orupload a landcover raster for your region of interest in *GeoTIFF* format. The app currently can take three types of land-use/landcover data:
1. [ESA WorldCover Viewer](https://viewer.esa-worldcover.org/worldcover/) 
2. [Dynamic World](https://dynamicworld.app/)
3. [Copernicus Land Cover](https://lcviewer.vito.be/)

Please visit the above sites to download a raster for your region of interest. If your region covers multiple tiles, please merge tiles into one file prior to uploading. Please ensure the raster CRS is WGS 84, and the values for any landcover type are in one layer following the numeric codes for the above products. 

The app can also support rasters with NDVI values, or any similar measurement where the input raster only has one numeric layer.  

It is recommended to use the 10m resolution products due to the scale of the analysis.  

#### Instructions
##### Step 1
1. Select the box icon located in the upper left-hand corner of the map. Use this to select your potential study site area.
2. Select either **random** sites or **village** sites. Village sites will find all of the towns/villages/cities in the selected area using OpenStreetMap, and random sites will randomly place points on the map equal to the number of random sites inputted. (Note: if you select a very large area for **village** sites, the request may time out.)
3. If you already have potential sites, please upload a list of the sites in CSV format. The CSV should contain a column labeled **site**, which has the site name, and columns with **latitude** and **longitude** in decimal degrees.
4. Press **go**. The bounding box coordinates of your selected area will be printed, along with the number of potential sites (including your input sites).


##### Step 2
1. If you are uploading your own raster data, please head to one of the websites to download a landcover raster for your area. If your area encompasses multiple tiles, please merge before uploading. You may use the bounding box coordinates from the previous step to select the same region of interest, making sure to add an appropriate sized buffer around the area for the land cover analysis. Select the product type for your raster and upload. If the raster uploads correctly, an plot of your raster will display. (Note: the current upload size limit is 2GB, please ensure files are smaller than this.)
2. Input the distance from the center of each potential point you would like the landcover analyzed from in meters. This distance is the distance from the point to each edge of the raster on all four sides, so a distances of 1000 meters (1km) would lead to an area of analysis of 4 km<sup>2</sup>.
3. Press **go**. The calculations may take a while to run. The data will be displayed in the data tab when finished. Press **save file** to export. 

##### Step 3
1. After completing step 2, a scatterplot of the parameters will be displayed. If you previously have downloaded data after step 2, you can instead skip steps 1 and 2 and upload that file here. 
2. The comparison plot and table shows the values of your input sites relative to the other sites analyzed. Hover over each point to find the name of the site.   
3. Use the drop down menu to change the measure displayed. For landcover rasters there are three options:
  - Proportion: the proportion covered by land cover category in the area analyzed
  - Mean Patch Area: the mean patch size for the land cover category in the area analyzed. A larger mean patch area indicates larger and more contiguous patches of that land cover type. 
  - Total Area (m<sup>2</sup>): area covered by land cover category in the area analyzed

#### Credits
- Simon Smart/[shinyscholar](https://simon-smart88.github.io/shinyscholar/) for the mapping code.

#### Requests/issues?
Please log on github: https://github.com/BioDivHealth/ss-analyzer
