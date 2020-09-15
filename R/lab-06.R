library(sf)        # vector manipulation
library(raster)    # raster manipulation
library(fasterize) # "faster" raster
library(whitebox)  # terrain analysis

# Data libraries
library(osmdata)   # OSM API
library(elevatr)   # Elevation  Web Tiles
library(measurements)
library(units)
library(tidyverse)
library(mapview)



#Collecting Data

basin = read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin/")

elev = elevatr::get_elev_raster(basin, z = 13) %>%
  crop(basin) %>% 
  mask(basin)

writeRaster(elev, filename =  "../../geog-176A-labs/data/basin_elev.tif", overwrite = TRUE)

new_units = elev * 3.281

osm = osmdata::opq(basin)%>% 
  add_osm_feature(key= 'building') %>% 
  osmdata_sf()

polygon = osm$osm_polygons %>% 
  st_centroid()

clip_build = st_intersection(polygon, basin)

railyway = polygon %>% 
  filter(amenity == 'railway')

streams = osmdata::opq(basin)%>% 
  add_osm_feature(key= 'waterway') %>% 
  osmdata_sf()

line = streams$osm_lines 

clip_streams = st_intersection(line, basin)


#Terrain Analysis

wbt_hillshade("/geog-176A-labs/data/basin_elev.tif", "/geog-176A-labs/data/basin_hillshade.tif")

hillshade = raster( "/geog-176A-labs/data/basin_hillshade.tif")

#Once processed, read your hillshade tif file into R and plot it using the color palette gray.colors(256, alpha = .5) (also, remove the legend)

plot(hillshade, box = FALSE, axes = FALSE, col= gray.colors(256, alpha = .5), legend = FALSE)

plot(basin, add= TRUE, lwd = 2)

plot(clip_streams, add= TRUE, lwd = 2, col = "blue")


