###############################
##Project: Lab 03 
## Script purpose: Week 3
## Date: 8/20/20
###############################


library(tidyverse)
library(sf)
library(units)
library(USAboundaries)
library(rnaturalearthdata)
library(knitr)

install.packages("ggrepel")
library(ggrepel)

install.packages('gghighlight')
library(gghighlight)

install.packages("ggthemes")
library(ggthemes)


data = ("data/uscities.csv")
us_cities = read.csv(data)


region= data.frame(region = state.region,
                   state_name = state.name)

south = USAboundaries::us_states(resolution = "low") %>% 
  right_join(region, by = "state_name") %>% 
  filter(region == "South")

###########################
plot(south$geometry)
plot(south$aland)

plot(south$awater)
#name thing but with a map
plot(south['awater'])
############################

cities = readr::read_csv("data/uscities.csv") %>% 
  st_as_sf(coords = c("lng", "lat"), crs= 4326) %>% 
  st_filter(south, .predicate = st_intersects) 

plot(south$geometry)
plot(cities$geometry, add= TRUE , pch= 16, cex= .1)


south_c = st_combine(south) %>% 
  st_cast("MULTILINESTRING")


south_c = st_transform(south_c, 5070)
cities = st_transform(cities, 5070)


cities = cities %>% 
  mutate(dist_to_state = st_distance(cities, south_c),
         dist_to_state = units:: set_units(dist_to_state, "km"),
         dist_to_state = units::drop_units(dist_to_state))

big_cities = cities %>% 
  group_by(state_name) %>% 
  slice_max(population, n=1)


ggplot() +
  geom_sf(data = south_c) +
  geom_sf(date = cities, aes(col = dist_to_state), size =.1) +
  gghighlight::gghighlight(population > 10000) +
  geom_sf(data = big_cities, col = "navy")
  scale_color_gradient(low = "gray", high = "red") +
  theme_classic() +
  ggrepl::geom_label_repel(
    data = big_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size= 3)

------------------
    
#1.1
eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

#1.2: US State Boundaries
conus = USAboundaries::us_states(resolution = "low") %>% 
  st_as_sf(coords = c("lng", "lat"), crs= 4326) %>% 
  filter(!state_name %in% c("Puerto Rico", "Alaska", "Hawaii")) %>% 
  st_transform(eqdc)

view(conus)

#1.3:North America Country Boundaries 
library(rnaturalearthdata)

nat_earth = rnaturalearthdata::countries110

country_bound = nat_earth %>% 
  st_as_sf(coords = c("lng", "lat"), crs= 4326) %>% 
  filter(name %in% c("United States", "Mexico","Canada")) %>% 
  st_transform(eqdc)

  
#1.4: City Locations 

data = ("data/uscities.csv")
us_cities = read.csv(data)

locations = us_cities %>% 
  st_as_sf(coords = c("lng", "lat"), crs= 4326)%>% 
  filter(!state_name %in% c("Puerto Rico", "Alaska", "Hawaii")) %>% 
  st_transform(eqdc)


#Question 2:

# 2.1:Distance to US Borders (km)

cb_u_ml <- st_union(country_bound) %>% 
  st_cast("MULTILINESTRING")


us_borders <- locations %>% 
  mutate(border_dist = st_distance(locations, cb_u_ml), 
         border_dist = units::set_units(border_dist, 'km'),
         border_dist = units::drop_units(border_dist)) 

top_us_borders <- us_borders %>% 
  select(city, state_name, border_dist) %>% 
  slice_max(border_dist, n = 5) 

us_borders_df <- as.data.frame(top_us_borders) %>% 
  select(city, state_name, border_dist)

knitr::kable(us_borders_df, caption = 'Top 5 Cities Furthest to USA Border',
             col.names = c("City", "State", "Distance (km)"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)
             

#2.2:Distance to States (km)

cb_c_ml <- st_combine(country_bound) %>% 
  st_cast("MULTILINESTRING")


us_borders <- locations %>% 
  mutate(border_state = st_distance(locations, cb_c_ml), 
         border_state = set_units(border_state, 'km')) %>% 
  select(city, state_name, border_state) %>% 
  slice_max(border_state, n = 5) %>% 
  arrange(-border_state) %>% 
  st_drop_geometry()
  

us_borders_cdf <- as.data.frame(us_borders) %>% 
  select(city, state_name, border_state)


knitr::kable(us_borders_cdf, caption = 'The Five Cities Farthest from a State Border',
             col.names = c("City", "State", "Distance (km)"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)


#2.3: Distance to Mexico (km)

mexico <- country_bound %>% 
  filter(sovereignt == "Mexico")

mx_cities <- locations %>% 
  mutate(mx_border = st_distance(locations, mexico), 
         mx_border = set_units(mx_border, 'km')) %>% 
  select(city, state_name, mx_border) %>% 
  slice_max(mx_border, n = 5) %>% 
  arrange(-mx_border)


mx_df <- as.data.frame(mx_cities) %>% 
  select(city, state_name, mx_border)


knitr::kable(mx_df, caption = "Five Cities Farthest from Mexico's Border",
             col.names = c("City", "State", "Distance (km)"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)


#2.4:Distance to Canada (km)

canada <- country_bound %>% 
  filter(sovereignt == "Canada")

can_cities <- locations %>% 
  mutate(can_border = st_distance(locations, canada), 
         can_border = set_units(can_border, 'km'),
         can_border = drop_units(can_border)) %>% 
  select(city, state_name, can_border) %>% 
  slice_max(can_border, n = 5) %>% 
  arrange(-can_border)


can_df <- as.data.frame(can_cities) %>% 
  select(city, state_name, can_border)


knitr::kable(can_df, caption = "Five Cities Farthest from Canada's Border",
             col.names = c("City", "State", "Distance (km)"),
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)

#Question 3

#3.1: Data

big_cities = cities %>% 
  select(city, state_name, population) %>% 
  slice_max(population, n=10) %>% 
  arrange(-population)
        
dev.off()

largest_cities_plot <- ggplot() +
  geom_sf(data = country_bound) +
  geom_sf(data = conus) +
  geom_sf(data = cb_c_ml) +
  geom_sf(data = big_cities, col= "black")+
  ggrepel::geom_label_repel(data = big_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",size= 3)+
  labs(title = '10 Most Populated US cities',
       x = 'Longitude',
       y = 'Latitude')

dev.off()
plot(large_cities_plot)

#3.2: City Distance from the Border

far_cities = us_borders_df %>% 
  slice_max(border_dist, n=5)


plot_far_cities <-ggplot() + 
  geom_sf(data = conus) +
  geom_sf(data = us_borders_df, aes(col = border_dist), size = .1) +
  geom_sf(data = far_cities) +
  scale_color_gradient(low = "gray", high = "red") +
  geom_label_repel(data = far_cities, aes( label = city, geometry = geometry), 
                   stat = 'sf_coordinates') +
  labs(title = 'Cities Farthest from the US Border',
       x = 'Longitude',
       y = 'Latitude') +
  theme_gray()

plot(plot_far_cities)


#3.3: City Distance from Nearest State
near_state <- us_borders_cdf %>% 
  slice_max(border_state, n=5)



plot_near_cities <-ggplot() + 
  geom_sf(data = conus) +
  geom_sf(data = us_borders_cdf, aes(col = border_state), size = .1) +
  geom_sf(data = near_state) +
  scale_color_gradient(low = "gray", high = "red") +
  geom_label_repel(data = near_state, aes( label = city, geometry = geometry), 
                   stat = 'sf_coordinates') +
  labs(title = 'Cities Farthest from the US Border',
       x = 'Longitude',
       y = 'Latitude') +
  theme_gray()


plot(near_cities_plot)

ggsave(near_cities_plot, file = "img/near_cities_plot.png")


#3.4: Equidistance boundary from Mexico and Canada

eq_cities = locations %>% 
  mutate(eq_dist = abs(can_border - mx_border),
         eq_dist = units::set_units(eq_dist, "km"),
         eq_dist = units::drop_units(eq_dist))
view(locations)

cities2 = cities %>% 
  filter(eq_dist < 100) %>% 
  slice_max(population, n = 5)


ggplot() +
  geom_sf(data = cb_u_ml) +
  geom_sf(data = cities2) +
  geom_sf(data = cities, aes(col = eq_dist), size = .1) +
  scale_color_gradient(low = "blue", high = "green") +
  gghighlight(eq_dist < 100) +
  geom_sf(data = dist_usaborder_table) +
  ggrepel::geom_label_repel(
    data = cities2,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 4) +
  labs(title = "Cities Equal Distance from Mexico and Canada Borders",
       subtitle = "Five Most Populous Cities",
       x = "",
       y = "",
       col = "Distance from Mexico and Canada Borders (km)") +
  theme_gray() ->
  mexcanbordermap


#Question 4

#4.1:Quantifing Border Zone

#How many cities are in this 100 mile zone? (100 miles ~ 160 kilometers)
#How many people live in a city within 100 miles of the border?
#What percentage of the total population is in this zone?
#Does it match the ACLU estimate in the link above?
#Report this information as a table.



qbz <- locations %>% 
  mutate(sum_pop = sum(population)) %>% 
  mutate(zones = st_distance(locations, cb_u_ml),
         zones = units::set_units(zones, "km"),
         zones = units::drop_units(zones)) %>% 
  filter(zones <= 160) %>% 
  summarise(bor_pop = sum(population), bor_per = (bor_pop/ sum_pop)*100, n_cities = n()) %>% 
  head(1) %>% 
  st_drop_geometry()

knitr::kable(qbz, 
             caption = 'Quantifing Border Zone', 
             col.names = c('Border Zone Population', 'Percent of Total US population', 'Number of Cities'), 
             format.args = list(big.mark = ",")) %>%
  kableExtra::kable_styling("striped", full_width = TRUE, font_size = NULL)


#4.2: Mapping Border Zone

#Make a map highlighting the cites within the 100 mile zone using gghighlight.
#Use a color gradient from ‘orange’ to ‘darkred’.
#Label the 10 most populous cities in the Dange Zone

border_zone <- locations %>%
  st_as_sf(coords = c('lng', 'lat'), crs = 4326) %>%
  mutate(dist_bor = st_distance(locations, cb_u_ml), 
         dist_bor =  units::set_units(dist_bor, 'km'), 
         dist_bor =  units::drop_units(dist_bor)) %>%
  filter(dist_bor <= 160) %>%
  select(city, state_name, dist_bor, population)

most_pop <- border_zone %>%
  select(state_name, population, city) %>%
  arrange(-population) %>%
  head(10)


qbz_plot = ggplot() +
  geom_sf(data = cb_u_ml) +
  geom_sf(data = locations, col = 'grey', size = .1) +
  geom_sf(data = border_zone, aes(col = dist_bor), size = .5) +
  gghighlight(dist_bor <= 160) +
  scale_color_gradient(low = 'orange', high = 'darkred') +
  ggrepel::geom_label_repel(data = most_pop, aes(city, state_name), col = 'black', size = 3) +
  geom_label_repel(data = most_pop, aes(geometry = geometry, label = city), stat = 'sf_coordinates') +
  labs(title = "Cities Within 160 km of the US border",
       x = 'Longitude',
       y = 'Latitude',
       color = 'Distance (km)')+
  theme_gray()




