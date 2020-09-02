library(tidyverse)
library(rmapshaper)
library(measurements)
library(units)
library(sf)
library(readxl)
library(knitr)
library(ggrepel)
library(leaflet)
library(gghighlight)
library(rnaturalearthdata)
library(ggthemes)


#Question 1

#1.1
conus = USAboundaries::us_counties() %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070)

#1.2

cen_conus = conus %>%
  st_centroid() %>%
  st_union() %>%
  st_cast("MULTIPOINT")

#1.3
vori = cen_conus %>%
  st_voronoi() %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())


tri = cen_conus %>%
  st_triangulate() %>%
  st_cast() %>%
  st_as_sf() %>%
  mutate(id = 1:n())


grid_cov = st_make_grid(cen_conus, n = c(70, 50), square = TRUE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

hex_cov = st_make_grid(cen_conus, n= c(70,50), square = FALSE) %>%
  st_as_sf() %>%
  mutate(id = 1:n())

#1.4


vori = st_intersection(vori, st_union(conus))
plot_tess(vori, "Voroni Coverage") +
  geom_sf(data = conus, col = "darkred", size = .2)


tri = st_intersection(tri, st_union(conus))
plot_tess(tri, "Triangular Coverage") +
  geom_sf(data = conus, col = "darkred", size = .3)

grid_cov = st_intersection(grid_cov, st_union(conus))
plot_tess(grid_cov, "Square Coverage") +
  geom_sf(data = conus, col = "darkred", size = .3)


grid_cov = st_intersection(hex_cov, st_union(conus))
plot_tess(hex_cov, "Hexagonal Coverage") +
  geom_sf(data = conus, col = "darkred", size = .3)


#1.5

rmapshaper::ms_simplify

simp = ms_simplify(conus, keep= 0.4)

mapview::npts

mapview::npts(conus)
#51976

mapview::npts(simp)
#33485

#1.6
plot_tess = function(data, title){
  ggplot() +
    geom_sf(data = data, fill = "white", col = "navy", size = .2) +
    theme_void() +
    labs(title = title, caption = paste("This tesselation has:", nrow(data), "tiles" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}

#1.7

plot_tess(vori, "Voroni Coverage")

plot_tess(tri, "Triangular Coverage")

plot_tess(grid_cov, "Square Coverage")

plot_tess(hex_cov, "Hexagonal Coverage")

plot_tess(conus, "Original Coverage")


#Question 2

#2.1
tess_area <- function(data, title){
  calc = conus %>% 
    mutate(area = st_area(data)) %>% 
    set_units(data,"km^2") %>% 
    drop_units() %>% 
    data.frame() %>% 
    mutate(id = 1:n()) %>% 
    mutate(avg= mean(area)) %>% 
    mutate(std = sd(avg)) %>% 
    mutate(tot_area = sum(area)) %>% 
    return(calc)
}


dist_5070_km = function(g1, g2){
  g2 = st_transform(g2, crs = st_crs(g1))
  drop_units(set_units(st_distance(g1,g2),"km")) %>% 
    as.vector()
}

#2.2

tess_area(vori, "Voroni Coverage")
tess_area(tri, "Triangular Coverage")
tess_area(grid_cov, "Square Coverage")
tess_area(hex_cov, "Hexagonal Coverage")
tess_area(conus, "Original Coverage")

#2.3: Summarize 5 Coverage 

tess_summary = bind_rows(
  tess_area(tri ,"triangulation"),
  tess_area(vori, "voroni"),
  tess_area(grid_cov, "square"),
  tess_area(hex_cov, "hexagonal"),
  tess_area(conus, "originial"))
  

#2.4:Knitr Table


knitr::kable(can_df, caption = "Five Tessellations",
             col.names = c("triangulation", "voroni", "square","hexagonal", "originial"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)

#2.5: Comments on the traits of each tessellation
#
#
#
#
#


# Question 3 

#3.1
nid <- read_excel("data/NID2019_U.xlsx") %>% 
  filter(!is.na(LONGITUDE),!is.na(LATITUDE)) %>% 
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs= 4326) %>% 
  st_transform(5070)


#3.2

point_in_polygon3 = function(points, polygon, group){
  st_join(polygon, points) %>% 
    st_drop_geometry() %>% 
    count(get(group)) %>% 
    setNames(c(group, "n")) %>%
    left_join(polygon, by = group) %>% 
    st_as_sf() 
}

#3.3

#Your points are the dams
#Your polygons are the respective tessellation
#The id column is the name of the id columns you defined.

vor_jd = point_in_polygon3(nid, vori, "id")
tri_jd =point_in_polygon3(nid, tri, "id")
square_jd = point_in_polygon3(nid, grid_cov, "id")
hex_jd = point_in_polygon3(nid, hex_cov, "id")
og_jd = point_in_polygon3(nid, conus, "geoid")


#3.4

plot_pip = function(data, title){
  ggplot() + 
    geom_sf(data = data, aes(fill = n), col = NA, size = .2) + 
    scale_fill_viridis_c() + 
    theme_void() +
    labs(title = title, caption = paste("This Coverage has:", sum(data$n), "Dams" )) +
    theme(plot.title = element_text(hjust = .5, color =  "navy", face = "bold"))
}


#3.5
plot_pip(vor_jd, "Voroni Pip")
plot_pip(tri_jd, "Triangular Pip")
plot_pip(square_jd, "Square Pip")
plot_pip(hex_jd, "Hexagonal Pip")
plot_pip(og_jd, "Original Pip")


#3.6
#Comment on the influence of the tessellated surface in the visualization of point counts.






# Question 4

#4.1
unique(dams2$PURPOSES) %>% length

dam_freq <- strsplit(nid$PURPOSES, split = "") %>%
  unlist() %>% 
  table() %>% 
  as.data.frame() %>% 
  setNames(c("abbr", "count")) %>% 
  left_join(nid_classifier) %>% 
  mutate(lab = paste0(purpose, "\n(", abbr, ")"))


#I, C, S, R, P, F
#describe why you chose them

i_dams = nid %>%
  filter(grepl("I", PURPOSES) == TRUE)

c_dams = nid %>%
  filter(grepl("C", PURPOSES) == TRUE)

s_dams = nid %>%
  filter(grepl("S", PURPOSES) == TRUE)

r_dams = nid %>%
  filter(grepl("R", PURPOSES) == TRUE)

p_dams = nid %>%
  filter(grepl("P", PURPOSES) == TRUE)

f_dams = nid %>%
  filter(grepl("F", PURPOSES) == TRUE)


irrigation_tess = point_in_polygon3(i_dams, vori, "id")

flood_control_tess = vor_jd = point_in_polygon3(c_dams, vori, "id")

water_supply_tess = point_in_polygon3(s_dams, vori, "id")

recreation_tess = point_in_polygon3(r_dams, vori, "id")

fire_pro_tess = point_in_polygon3(p_dams, vori, "id")

fish_wildlife_tess = point_in_polygon3(nid, vori, "id")


#4.2

irrigation_plot = plot_pip(i_dams, "Irrigation Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

flood_control_plot= plot_pip(c_dams, "Flood Control Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

water_supply_plot= plot_pip(s_dams, "Water Supply Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

recreation_plot= plot_pip(r_dams, "Recreation Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

fire_pro_plot= plot_pip(p_dams, "Fire Protection Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))

fish_wildlife_plot = plot_pip(f_dams, "Fish and Wildlife Dams")+
  gghighlight::gghighlight(n > (mean(n)+ sd(n)))


