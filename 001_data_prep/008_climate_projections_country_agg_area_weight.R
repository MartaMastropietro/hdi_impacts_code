
rm(list=ls())
library(raster)
library(terra)
library(dplyr)
library(st)
library(sf)

data_dir<-"data/climate_data/projections"


ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")

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

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
crs(countries)

data_country <- data.frame(expand.grid(year= years, iso3= countries$ISO3_CODE, model=models_names, ssp=ssp_names ))
weight=1

### extract for each var 
for (v in var_names){
  
  
  all_agg_country<-data.frame()
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
        agg_country = exactextractr::exact_extract(data, countries, fun = "weighted_mean", weights = areas*weight)
        agg_country$ISO3_CODE<-countries$ISO3_CODE
        colnames(agg_country)<-as.character(c(years, "iso3"))
        agg_country<-reshape2::melt(agg_country, id.vars=c("iso3"))
        colnames(agg_country)<-c("iso3", "year", v)
        agg_country$year<-as.numeric(as.character(agg_country$year))
        length(unique(agg_country$iso3)) # 257
        agg_country$model=mod
        agg_country$ssp<-ssp
        
        all_agg_country<-rbind(all_agg_country, agg_country)
        
      }
     
    
   } 
  }
  
  ### complete dataframes of regional agg with new columns
  data_country <- dplyr::inner_join(data_country, all_agg_country)
    
  
}

################################################################################

### save all dataframes 
write.csv(data_country, file = file.path(data_dir, "projection_data_climate_country_2015_2100.csv"), row.names = FALSE)

