### need ssp isimip population data 

rm(list=ls())
# source("utils/libraries.R")
library(raster)
library(terra)
library(dplyr)
library(st)
library(sf)
library(ncdf4)

# args = base::commandArgs(trailingOnly=TRUE)
# 

# ### Countries borders 
# countries<-giscoR::gisco_get_countries()
# #simplify data countries
# countries<-countries[,c(3,6)]
# countries<-sf::st_as_sf(countries)
# # fix crs
# countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
# crs(countries)

### Sub-national shape file (hdi+components data)
gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 
crs(gdl_sub_nations)
#plot(gdl_sub_nations)

data_dir<-"data/climate_data/projections"
pop_dir<-"data/data_population/isimip_pop_grid_05_proj"

ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")
ssp_names_pop<-c("ssp1", "ssp1", "ssp2", "ssp3", "ssp5")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 

var_names<-c("PEXT", 
             "RR", 
             "TM", 
             "TVAR", 
             "WD")
new_var_names<-c("HW", 
                 "RX",
                 "SPI",
                 "SPEI")
var_names<-c(var_names, new_var_names)


years<-2015:2100 


###
# precompute areas climate 
load(file.path(data_dir, paste0("projections_TM_CAN_ESM5_ssp126.RData")))
data<-raster::brick(all_rast[["TM"]])
areas<-raster::area(data)

################################################################################


# for ssps climate correspondance


# loop in ssp names pop
for (n in 1:length(ssp_names)){
  
  # pop load 
  pop<-ncdf4::nc_open(file.path(pop_dir, paste0("population_",ssp_names_pop[n],"soc_0p5deg_annual_2006-2100.nc4")))
  var<-"number_of_people"
  lon <- ncvar_get(pop, "lon")
  lat <- ncvar_get(pop, "lat", verbose = F)
  var.array <- ncvar_get(pop, var) # store the data in a 2-dimensional array
  fillvalue <- ncatt_get(pop, var, "_FillValue")
  var.array[which(var.array == fillvalue$value)] <- NA
  
  #
  
  data_ssp<-data.frame()
  
  for (v in var_names){
    
    for (mod in models_names){
      
      gc()
      
      file<-file.path(data_dir, paste0("projections_",v,"_", mod, "_", ssp_names[n], ".RData"))
      
      if(file.exists(file)){
        
        load(file)
        data<-raster::brick(all_rast[[paste0(v)]]) #temporary data with raster of corresp variable , all years
        
        
        ### agg by mean
        for (year in years){
          
          # pop
          i<-year - 2005 #we want to start from 2015, but pop is from 2006 so we start from index 10 of array
          r <- raster(t(var.array[,,i]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
          area<-area(r)
          r<-r/area
          
          # climate
          id<- which(years==year)
          data_temp<-data[[id]]
         
          
          # weight 
          w<-areas*r
          
          # extraction weighting by pop and area 
          agg_subnat = exactextractr::exact_extract(data_temp, gdl_sub_nations, fun = "weighted_mean", weights = w)
          agg_subnat<-data.frame(agg_subnat)
          agg_subnat$gdlcode<-gdl_sub_nations$gdlcode
          colnames(agg_subnat)<-c(  "value", "gdlcode")
          agg_subnat$year<-year
          agg_subnat$model<-mod
          agg_subnat$ssp<-ssp_names[n]
          agg_subnat$variable<-paste0(v)
          
          data_ssp<-rbind(data_ssp, agg_subnat)
          
        }
      }
      
    }
    
  }
  
  
  
  ### save dataframes for each ssp
  write.csv(data_ssp, file = file.path(data_dir,paste0( ssp_names[n], "_projection_data_climate_gdlcode_pop_weight_2015_2100.csv")), row.names = FALSE)
  
  
}

################################################################################
