############
### Composite Index formation 
### Digital Exclusion Index - Following Severity Index vignette
### https://humanitarian-user-group.github.io/post/compositeindicator/
############

# Clear any existing data from the data set
rm(list = ls())

### Load packages and libraries

# pacman only needs to be installed once
# install.packages("pacman")

# Define vector of package names

package_names <- c("readr","readxl","dplyr","ggplot2","corrplot","psych",
                   "scales","ggiraph","ggrepel","Compind","ggcorrplot","kableExtra",
                   "reshape2","qgraph", "sf","cartography",
                   "raster", "dismo", "rgdal", "SpatialPosition","ggpubr",
                   "devtools","PerformanceAnalytics","hrbrthemes",
                   "Hmisc","PerformanceAnalytics","psych","Rcsdp","GPArotation",
                   "BBmisc","rgdal", "leaflet","sp","plotly","htmlwidgets","mapview",
                   "forcats","htmltools")



# This code installs all the other required packages if they are not currently installed and load all the libraries
pacman::p_load(char=package_names)
rm(package_names)




### Load data 



data_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Data/'
geo_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/gis/'
out_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/Figures/'
ref_dir <- 'C:/Users/pmee/OneDrive - University of Lincoln/Projects/Digital_Exclusion_Lincolnshire/refs/'


### Display on a map


### Read shapefile data 

## Lincolnshire data 
lsoa_linc <-  readOGR(dsn = geo_dir,
                 layer = "Lincolnshire LSOA")

## UK data
uk_geo <-  readOGR(dsn = geo_dir,
                      layer = "Map_UK")

### Project to Lat Long
lsoa_linc_ll <- spTransform(lsoa_linc, CRS("+init=epsg:4326"))

uk_geo_ll <- spTransform(uk_geo, CRS("+init=epsg:4326"))




### Maps



tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 50%;
    text-align: center;
    padding-left: 10px; 
    padding-right: 10px; 
    background: rgba(255,255,255,0.75);
    font-weight: bold;
    font-size: 12px;
  }
"))





linc_base_map <- leaflet(options = leafletOptions(minZoom = 9)) %>%
  # Choose here whether to use Open Street Map or Satellite imagery data in the background
  # addProviderTiles("Esri.WorldImagery") %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>%
  # addMapPane(name = "maplabels", zIndex = 420) %>% # higher zIndex rendered on top
  # addProviderTiles("CartoDB.PositronNoLabels") %>%
  # addProviderTiles("CartoDB.PositronOnlyLabels", 
  #                  options = leafletOptions(pane = "maplabels"),
  #                  group = "map labels") %>%
  addPolygons(data = uk_geo_ll, stroke = TRUE, 
                          weight=2.0,
               fillOpacity = 0.0)


setView(linc_base_map, lng=-0.1999702,lat=53.1178821,zoom = 9.4)


## Save as png
# map_file <- 'linc_factor_map.png'
# mapview::mapshot(lsoa_linc_map, file = paste0(out_dir,map_file))


### citation checking 

# bibfile = paste0(ref_dir,"leaf.bib")
# 
# cat(toBibtex(citation(package = "leaflet")),file= bibfile)

