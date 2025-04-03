
rm(list=ls())
# source("utils/libraries.R")
library(raster)
library(terra)
library(dplyr)
library(st)
library(sf)

data_dir<-"data/climate_data/projections"

ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")
ssp_names_high<-c("SSP1", "SSP1", "SSP2", "SSP3", "SSP5")


### nb: gridded 1km, so this is already population density in each squared km
pop_years<-c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)


# years<-2015:2100 # years in rasters 
years<-2020:2100 



### test 
load("D:/Marta/PhD/OneDrive - Politecnico di Milano/AAA - PhD/Projects/Impacts and Hazards/hdi_climate_impacts/data/climate_data/projections/projections_TM_CAN_ESM5_ssp126.RData")
cl<-all_rast$TM[[5]]
# cl<-terra::rast(cl)
# cl<-terra::project(cl,  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# crs(cl, parse=T)
# plot(cl)

library(ncdf4)
pop<-ncdf4::nc_open("data/data_population/isimip_pop_grid_05_proj/population_ssp1soc_0p5deg_annual_2006-2100.nc4")

var<-"number_of_people"
lon <- ncvar_get(pop, "lon")
lat <- ncvar_get(pop, "lat", verbose = F)
var.array <- ncvar_get(pop, var) # store the data in a 2-dimensional array
fillvalue <- ncatt_get(pop, var, "_FillValue")
var.array[which(var.array == fillvalue$value)] <- NA

## loop in values 
i=1
r <- raster(t(var.array[,,i]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
plot(r)
crs(r)
area<-area(r)
r<-r/area

#try<-gdl_shape_file[which(gdl_shape_file$iso_code=="BRA"), c(1,4)]

plot(w)
plot(cl)
areas<-area(cl)
w<- areas*r
try_agg_subnat = exactextractr::exact_extract(cl, gdl_sub_nations , fun = "weighted_mean", weights = w)
try_agg_subnat_area = exactextractr::exact_extract(cl, gdl_sub_nations , fun = "weighted_mean", weights = areas)


try_agg_subnat<-data.frame(try_agg_subnat)
try_agg_subnat$gdlcode<-gdl_sub_nations$gdlcode
try_agg_subnat[which(is.na(try_agg_subnat)),]

try_agg_subnat$na_val<-0
try_agg_subnat$na_val[which(is.na(try_agg_subnat$try_agg_subnat))]<-1
try_agg_subnat$na_val<-as.factor(try_agg_subnat$na_val)
try_agg_subnat<-inner_join(try_agg_subnat, gdl_sub_nations)
library(ggplot2)
try_agg_subnat<-sf::st_as_sf(try_agg_subnat)
g<-ggplot(try_agg_subnat, aes(fill=na_val))+geom_sf()
ggsave(g, filename="try_agg.jpeg", width=30, height=20 )

writeRaster(cl, 'climate.tif')
  
ssp<-"ssp126"
s<-ssp_names_high[which(ssp_names==ssp)]
pop_brick_list<-list()
y<-pop_years[2]

# pop_t <-terra::rast(file.path(paste0("data/data_population/pop_grid_proj/",s, "/", s, "_", y,".tif")))

pop_t <-raster(file.path(paste0("data/data_population/pop_grid_proj/",s, "/", s, "_", y,".tif")))
extent(pop_t)

ext <- extent(-180, 180, -90, 90)  # Define the extent
extent(pop_t) <- ext 
crs(pop_t)
area<-area(pop_t)
pop_t<-pop_t/area
plot(area)
plot(pop_t)

# ext(pop_t) <- ext(-179.75, 179.75, -89.75, 89.75 ) 


pop_resampled <- resample(pop_t, cl, method="bilinear")
plot(pop_resampled)

writeRaster(pop_resampled, 'pop.tif')

crs(pop_t)<-"+proj=longlat +datum=WGS84 +no_defs"
pop_t <- subst(pop_t, NA, 0) 
crs(pop_t, parse=T)
pop_t<-terra::project(pop_t,  "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
pop_t
crs(pop_t, parse=T)
plot(pop_t)
a_t<-terra::cellSize(pop_t, unit = "km")
crs(a_t, parse=T)
pop_t<-pop_t/a_t

pop_r <- raster::raster(pop_t)
crs(pop_r)
new<-raster::raster(ncols=720, nrows=360, xmn=-179.75, xmx=179.75, ymn=-89.75, ymx=89.75  , 
                    crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")

pop_r_crs<-raster::resample(pop_r , new , method='bilinear')
pop_r_crs
crs(pop_r_crs)
plot(pop_r_crs)

areas<-terra::cellSize(cl, unit = "km")
crs(areas)
plot(areas)
areas<-raster::raster(areas)
areas<-raster::resample(areas , new , method='bilinear')

gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 
crs(gdl_sub_nations, parse=T)


w<- areas*pop_r_crs
plot(w)
w_dat<-as.data.frame(w,xy = TRUE)
any(is.na(w_dat))

cl
w

extent(w)
extent(cl)

try<-gdl_shape_file[which(gdl_shape_file$iso_code=="BRA"), c(1,4)]
cl_res<-raster::resample(raster(cl) , new , method='bilinear')
try_agg_subnat = exactextractr::exact_extract(cl_res, try , fun = "weighted_mean", weights = w)
try_agg_subnat_area = exactextractr::exact_extract(cl_res, try , fun = "weighted_mean", weights = areas)

try_agg_subnat<-data.frame(try_agg_subnat)
try_agg_subnat$gdlcode<-try$gdlcode
try_agg_subnat<-inner_join(try_agg_subnat, gdl_sub_nations)
try_agg_subnat<-sf::st_as_sf(try_agg_subnat)
plot(try_agg_subnat)

agg_subnat = exactextractr::exact_extract(brick(cl), gdl_sub_nations , fun = "weighted_mean", weights = w)
agg_subnat<-data.frame(agg_subnat)
agg_subnat$gdlcode<-gdl_sub_nations$gdlcode
agg_subnat$na_val<-0
agg_subnat$na_val[which(is.na(agg_subnat$agg_subnat))]<-1
agg_subnat$na_val<-as.factor(agg_subnat$na_val)
agg_subnat<-inner_join(agg_subnat, gdl_sub_nations)
library(ggplot2)
agg_subnat<-sf::st_as_sf(agg_subnat)
g<-ggplot(agg_subnat, aes(fill=na_val))+geom_sf()
ggsave(g, filename="try_agg.jpeg", width=30, height=20 )

### 

for (scenario in c(1:5)){
  print(scenario)
  if (scenario==1){
    ssp<-"ssp126"
  }else if(scenario==2){
    ssp<-"ssp119"
  }else if(scenario==3){
    ssp<-"ssp245"
  }else if(scenario==4){
    ssp<-"ssp370"
  }else if(scenario==5){
    ssp<-"ssp585"}

  
  gc()
  
  s<-ssp_names_high[which(ssp_names==ssp)]
  
  #pop
  pop_brick_list<-list()
  for ( y in pop_years){
    pop_t <-terra::rast(file.path(paste0("data/data_population/pop_grid_proj/",s, "/", s, "_", y,".tif")))
    pop_t <- subst(pop_t, NA, 0) 
    a_t<-terra::cellSize(pop_t)
    pop_t<-pop_t/a
    
    # new<-rast(ncols=720, nrows=360, xmin=-179.75, xmax=179.75, ymin=-89.75, ymax=89.75  , 
    #           crs="+proj=longlat +datum=WGS84 +no_defs ")
    # pop_t_crs<-terra::resample(pop_t,new )
    
    pop_r <- raster::raster(pop_t)
    # a<-area(pop_r)
    # pop_r<-calc(pop_r, fun = function(x) x / a) 
    
    # crs adjust
    new<-raster::raster(ncols=720, nrows=360, xmn=-179.75, xmx=179.75, ymn=-89.75, ymx=89.75  , 
              crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0 ")
    
    pop_r_crs<-raster::projectRaster(pop_r , new)
    
    crs(pop)  = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
    # extent adjust 
    bb<-extent(-179.75, 179.75, -89.75, 89.75) # same as data
    extent(pop) <- bb
    pop <- setExtent(pop, bb, keepres=TRUE)
    
    
    
    pop_brick_list[[length(pop_brick_list)+1]]<-pop
    
    
  }
  
  
  file<-file.path(data_dir, paste0("projections_",var_names[1],"_", models_names[5], "_", ssp, ".RData"))
  load(file)
  data<-raster::brick(all_rast[[paste0(var_names[1])]]) #temporary data with raster of corresp variable , all years
  areas=area(data)
  weights = lapply(pop_brick_list , function (x){raster::resample(x, areas, method='bilinear')})
  
  weights_all<-list()
  for (y in 1:length(pop_years)){
    weights_all[[length(weights_all)+1]]<-
      weights_all[[length(weights_all)+1]]<-
      weights_all[[length(weights_all)+1]]<-
      weights_all[[length(weights_all)+1]]<-
      weights_all[[length(weights_all)+1]]<-
      weights[[y]]
  }
  weights_all[82:85]<-NULL
  save(weights_all,file= file.path(data_dir, paste0(ssp, "_resampled_pop_2020_2100.Rdata")))
  
  
}

