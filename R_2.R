library(sf)
library(tidyverse)
library(osmdata)
library(tmap)




# Download Data from OSM ----

roads_osm <- opq(bbox = 'Saint Petersburg') %>%
    add_osm_feature(key = 'highway', value = c('motorway', 'trunk', 'primary', 'secondary', 'tertiary')) %>%
    osmdata_sf()

spb_roads = roads_osm$osm_lines %>% st_transform(crs = 32636) %>% dplyr::select(osm_id, name, highway)

plot(spb_roads[,1])

# Grid ----

hexd2  = function(area) {
    a = sqrt(area/(1.5*sqrt(3)))
    ds = sqrt(3)*a
    return(ds)
}

grid_spacing = hexd2(500000)

grid_poly <- st_make_grid(spb_roads, square = F, cellsize = c(grid_spacing, grid_spacing), flat_topped = TRUE) %>% st_sf() %>% mutate(id = 1:nrow(.), area = st_area(geometry)) 


plot(grid_poly[,1], col = 'white')
plot(st_geometry(spb_roads), add = T)


# aka Intersection and Join attributes by location (summary) in QGIS ----

roads_intersect = st_intersection(spb_roads, grid_poly) %>% mutate(length = st_length(geometry)) %>% as.data.frame() %>% group_by(id) %>% summarise(total_length = sum(length)) 

roads_grid = grid_poly %>% merge(roads_intersect, by = 'id') %>% mutate(density = (total_length/area)*1000)


plot(roads_grid[,5])


# Visualization with tmap  ----

tmap_mode("plot")

tm_shape(roads_grid) +
    tm_polygons("density",
                breaks=c(-Inf, 2, 4, 6, 8, 10, Inf),
                palette = "BuPu",
                border.col = "Grey",
                lwd = 0.3,
                title = "Roads density, km/km2") +
    tm_legend(legend.position = c("left", "bottom"))+
    tm_layout(title = "Road density in Saint Petersburg",
              fontfamily = 'Muller',
              title.size = 1.5,
              title.position = c("center", "top"),
              legend.format = list(text.separator = "-"))+
    tm_layout(inner.margins = c(0.1, 0.2, 0.1, 0.1))+
    tm_scale_bar(breaks = c(0,3))+
tm_shape(spb_roads) +
    tm_lines(col = 'white', lwd = 0.3)



tmap_mode("view")

tm_shape(roads_grid) +
    tm_polygons("density",
                breaks=c(-Inf, 2, 4, 6, 8, 10, Inf),
                palette = "BuPu",
                border.col = "Grey",
                lwd = 0.3,
                title = "Roads density, km/km2") +
    tm_legend(legend.position = c("left", "bottom"))+
    tm_layout(title = "Road density in Saint Petersburg",
              fontfamily = 'Muller',
              title.size = 1.5,
              title.position = c("center", "top"),
              legend.format = list(text.separator = "-"))+
    tm_layout(inner.margins = c(0.1, 0.2, 0.1, 0.1))+
    tm_scale_bar(breaks = c(0,3))+
    tm_shape(spb_roads) +
    tm_lines(col = 'white', lwd = 0.3)





# saving sf ---- 
#setwd("/Users/Belka/Desktop/R/")

st_write(roads_grid, 'spb_rdensity_grid_50ha.gpkg')
st_write(spb_roads, 'spb_roads_osm.gpkg')







