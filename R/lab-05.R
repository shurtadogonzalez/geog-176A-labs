#LAB 05

library(raster) # Raster Data handling
library(tidyverse) # Data Manipulation
library(getlandsat) # keyless Landsat data (2013-2017)
library(sf) # Vector data processing
library(mapview) # Rapid Interactive visualization
library(osmdata)
library(kableExtra)


data = ("data/uscities.csv")
us_cities = read_csv(data)

#Question 1

palo = us_cities %>% 
  st_as_sf(coords = c("lng", "lat"), crs= 4326) %>% 
  filter(city %in% "Palo") %>% 
  st_transform(5070) %>% 
  st_buffer(5000) %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_as_sf()

mapview(palo)

# Question 2

#2.1
bbwgs = palo %>% 
  st_transform(4326) 

palo= st_bbox(bbwgs)

scenes = getlandsat::lsat_scenes()


scenes_date = scenes %>% 
  filter(min_lat <= palo$ymin, max_lat >= palo$ymax,
         min_lon <= palo$xmin, max_lon >= palo$xmax,
         as.Date(acquisitionDate)== as.Date("2016-09-26"))
  
write.csv(scenes_date, file = "data/palo-flood-scene.csv", row.names = FALSE)


#2.2-2.4
#Work for this is in Rmd file 


#Question 3

#3.1
names(r) <- c("coastal", "blue", "green","red","NIR","SWIR1")

#3.2
plotRGB(r, r=4, g=3, b=2, stretch = "lin")

plotRGB(r, r=4, g=3, b=2, stretch = "hist")


#Question 4 

#4.1:Raster Algebra 

#Create 5 new rasters using the formulas for NDVI, NDWI, MNDWI, WRI and SWI

palette = colorRampPalette(c("blue", "white", "red"))

ndvi = (r$NIR - r$red)/ (r$NIR + r$red)


ndwi = (r$green- r$NIR)/ (r$green + r$NIR)


mndwi = (r$green - r$SWIR1)/ (r$green + r$SWIR1)


wri = (r$green + r$red)/ (r$NIR + r$SWIR1) 


swi = 1 / (sqrt(r$blue- r$SWIR1)) 


raster_stack <- raster::stack(ndvi, ndwi, mndwi, wri, swi) %>% 
  setNames(c("NDVI", "NDWI", "MNDWI", "WRI", "SWI"))

palette = colorRampPalette(c("blue", "white", "red"))

plot(raster_stack, col = palette(256))

#Describe the 5 images. How are they simular and where do they deviate?

#4.2: Raster Threshold 

thresholding1 = function(x){ifelse(x <= 0,1,NA)}

thresholding2 = function(x){ifelse(x >= 0,1,NA)}

thresholding3 = function(x){ifelse(x >= 0,1,NA)}

thresholding4 = function(x){ifelse(x >= 1,1,NA)}

thresholding5 = function(x){ifelse(x <= 5,1,NA)}

flood1 = calc(ndvi,threshold1)

flood2 = calc(ndwi, threshold2)

flood3 = calc(mndwi, threshold2)
flood4 = calc(wri, threshold3)
flood5 = calc(swi, threshold4)







