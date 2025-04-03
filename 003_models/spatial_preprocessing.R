### regions centroid calculations, spatial proximity matrix calculation


gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 

gdl_sub_nations<-sf::st_make_valid(gdl_sub_nations)
centroids<-sf::st_centroid(gdl_sub_nations)
gdl_sub_nations$lon<-sf::st_coordinates(centroids)[, 1]
gdl_sub_nations$lat<-sf::st_coordinates(centroids)[, 2]
gdl_sub_nations$geometry<-NULL

coords <- as.matrix(gdl_sub_nations[, c("lon", "lat")])

distances <- as.matrix(dist(coords))

inv_distances <- 1 / distances
diag(inv_distances) <- 0  # Remove self-connections (avoid Inf)

# Convert to a listw object
listw <- mat2listw(inv_distances, style = "W", zero.policy = TRUE)
save(listw, file="data/hdi_data/downloaded/GDL Shapefiles V6.2/listw_inv_dist_matrix.RData")
