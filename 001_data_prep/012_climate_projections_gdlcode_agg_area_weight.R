### need ssp isimip population data 

rm(list=ls())
# source("utils/libraries.R")
library(raster)
library(terra)
library(dplyr)
library(st)
library(sf)

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
gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 
crs(gdl_sub_nations)
#plot(gdl_sub_nations)

data_dir<-"data/climate_data/projections"


ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")
ssp_names_high<-c("SSP1", "SSP1", "SSP2", "SSP3", "SSP5")

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


################################################################################

data_agg <- data.frame(expand.grid(year= years, gdlcode= gdl_sub_nations$gdlcode, model=models_names, ssp=ssp_names ))
weight=1

### extract for each var 
for (v in var_names){
  
  
  all_agg<-data.frame()
  for (mod in models_names){
    for (ssp in ssp_names){
      
      
      gc()
      file<-file.path(data_dir, paste0("projections_",v,"_", mod, "_", ssp, ".RData"))
      
      if(file.exists(file)){
        
        load(file)
        data<-raster::brick(all_rast[[paste0(v)]]) #temporary data with raster of corresp variable , all years
        
        areas=area(data)
        
        ### agg by mean
        #countries
        agg_subnat = exactextractr::exact_extract(data, gdl_sub_nations, fun = "weighted_mean", weights = areas*weight)
        agg_subnat$gdlcode<-gdl_sub_nations$gdlcode
        colnames(agg_subnat)<-as.character(c(years, "gdlcode"))
        agg_subnat<-reshape2::melt(agg_subnat, id.vars=c("gdlcode"))
        colnames(agg_subnat)<-c("gdlcode", "year", v)
        agg_subnat$year<-as.numeric(as.character(agg_subnat$year))
        length(unique(agg_subnat$gdlcode)) # 257
        agg_subnat$model=mod
        agg_subnat$ssp<-ssp
        
        all_agg<-rbind(all_agg, agg_subnat)
        
      }
      
      
    } 
  }
  
  ### complete dataframes of regional agg with new columns
  data_agg <- dplyr::inner_join(data_agg, all_agg)
  
  
}

################################################################################

### save all dataframes 
write.csv(data_agg, file = file.path(data_dir, "projection_data_climate_gdlcode_2015_2100.csv"), row.names = FALSE)

