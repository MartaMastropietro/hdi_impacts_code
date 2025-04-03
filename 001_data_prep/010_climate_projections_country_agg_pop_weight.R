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

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
crs(countries)

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


### nb: gridded 1km, so this is already population density in each squared km
pop_years<-c(2020, 2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100)


# years<-2015:2100 # years in rasters 
years<-2020:2100 

  
################################################################################
  



for (ssp in ssp_names){
  
  load(file.path(data_dir, paste0(ssp, "_resampled_pop_2020_2100.Rdata")))
  
  data_ssp<-data.frame()
  
  for (v in var_names){
  
  for (mod in models_names){
    
    gc()
    
    file<-file.path(data_dir, paste0("projections_",v,"_", mod, "_", ssp, ".RData"))
      
    if(file.exists(file)){
        
        load(file)
        data<-raster::brick(all_rast[[paste0(v)]]) #temporary data with raster of corresp variable , all years
        areas=area(data)
        
        ### agg by mean
        for (year in years){
          data_temp<-data[[paste0("X",year)]]
          weight<-weights_all[[which(years==year)]]
          agg_country = exactextractr::exact_extract(data_temp, countries, fun = "weighted_mean", weights = areas*weight)
          agg_country<-data.frame(agg_country)
          agg_country$ISO3_CODE<-countries$ISO3_CODE
          colnames(agg_country)<-c(  "value", "iso3")
          agg_country$year<-year
          agg_country$model=mod
          agg_country$ssp<-ssp
          agg_country$variable<-paste0(v)
          
          data_ssp<-rbind(data_ssp, agg_country)
          
        }
      }
    
    }
  
  }
  
  
  
  ### save dataframes for each ssp
  write.csv(data_ssp, file = file.path(data_dir,paste0( ssp, "_projection_data_climate_country_pop_weight_2020_2100.csv")), row.names = FALSE)
  
  
}

################################################################################
