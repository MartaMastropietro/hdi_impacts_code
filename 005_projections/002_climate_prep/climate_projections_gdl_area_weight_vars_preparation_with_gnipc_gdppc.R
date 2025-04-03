### lets add to climate projections also the projections of gnipc, and consequent variables interactions

rm(list=ls())
# source("utils/libraries.R")

library(readr)
library(dplyr)
library(data.table)


### out dir 

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/climate_projections_gdl"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### gdlcodes
gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_shape_file$geometry<-NULL
gdl_shape_file<-gdl_shape_file[, c("gdlcode", "iso_code")]
colnames(gdl_shape_file)<-c("gdlcode", "iso3")


### climate data 
# climate_projections<- arrow::read_feather(file.path(out_dir, "climate_projections_area_weight.feather"))
# load(file.path(out_dir, "climate_projections_area_weight.RData"))
# climate_projections<-as.data.frame(climate_projections)

ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI")

m_modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean")


library(dplyr)
library(data.table)

# Function to create lagged columns for multiple variables
create_lags_adap <- function(dt, varns, modns, m_modns, adap ,  max_lag = 10) {
  
  # 
  for (i in 1:length(modns)) {
    gc()
    for (lag_i in 1:max_lag) {
      dt[, paste0("lag_", lag_i, "_", adap[i], "_i_", varns[i]) := shift(get(paste0(adap[i])) * get(paste0(varns[i])), lag_i), by = .(gdlcode, model, ssp)]
    }
  }
  
  
  for (i in 1:length(modns)) {
    gc()
    for (lag_i in 1:max_lag) {
      dt[, paste0("lag_", lag_i, "_", adap[i], "_i_", modns[i]) := shift(get(paste0(adap[i])) * get(paste0(modns[i])), lag_i), by = .(gdlcode, model, ssp)]
    }
  }
  
  return(dt)
}


################################################################################

##### adap ######

### gnipc 
var_name<-"gnipc"

data_proj_gni<-read_csv(paste0("output/projections/original_comp/preproc_proj_", var_name, ".csv"))
colnames(data_proj_gni)<-c("iso3", "year", "ssp", "gni_pc", "pop")

data_proj_gni<-data_proj_gni %>% 
  arrange(ssp, iso3, year) %>% 
  group_by(ssp, iso3) %>% 
  mutate( log_gni_pc =  log(gni_pc) ) %>% 
  mutate( lag_log_gni_pc =  dplyr::lag(log_gni_pc, n = 1, default = NA ) )%>%
  ungroup()

data_proj_gni<-data_proj_gni[, c("iso3", "year", "ssp", "lag_log_gni_pc")]


### gdppc
var_name<-"gdp_pc"

data_proj_gdp<-read_csv(paste0("output/projections/original_comp/preproc_proj_", var_name, ".csv"))
colnames(data_proj_gdp)<-c("year",  "gdp_pc","iso3", "ssp", "pop")

data_proj_gdp<-data_proj_gdp %>% 
  arrange(ssp, iso3, year) %>% 
  group_by(ssp, iso3) %>% 
  mutate( log_gdp_pc =  log(gdp_pc) ) %>% 
  mutate( lag_log_grp_pc_usd =  dplyr::lag(log_gdp_pc, n = 1, default = NA ) )

data_proj_gdp<-data_proj_gdp[, c("iso3", "year", "ssp", "lag_log_grp_pc_usd")]

### gnipc 
adap_gni=rep('lag_log_gni_pc',10)

### gdppc
adap_gdp=rep('lag_log_grp_pc_usd',10)


################################################################################


regions<-unique(gdl_shape_file$gdlcode)

pb=progress::progress_bar$new(total=length(regions))
pb$tick(0)
for ( r in regions){
  
  load(file.path(out_dir,paste0(r,"_climate_projections_gdl_area_weight.RData"))) 
  climate_projections<-as.data.frame(climate_projections)
  
  climate_projections<-dplyr::left_join(climate_projections, data_proj_gni)
  climate_projections<-dplyr::left_join(climate_projections, data_proj_gdp)
  
  
  for (i in 1:length(varns)){  
    climate_projections[paste(adap_gni[i],'_i_',varns[i],sep='')] <- climate_projections[adap_gni[i]] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(modns)){ 
    climate_projections[paste(adap_gni[i],'_i_',modns[i],sep='')] <- climate_projections[adap_gni[i]] * climate_projections[modns[i]]
  } 
  
  for (i in 1:length(varns)){  
    climate_projections[paste(adap_gdp[i],'_i_',varns[i],sep='')] <- climate_projections[adap_gdp[i]] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(modns)){ 
    climate_projections[paste(adap_gdp[i],'_i_',modns[i],sep='')] <- climate_projections[adap_gdp[i]] * climate_projections[modns[i]]
  } 
  
  gc()
  # Convert the data frame to data.table
  setDT(climate_projections)
  
  climate_projections <- create_lags_adap(climate_projections, varns = varns,modns=modns, m_modns=m_modns, adap=adap_gni ,  max_lag = 10)
  
  climate_projections <- create_lags_adap(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  adap=adap_gdp ,  max_lag = 10)
  
  climate_projections<-as.data.frame(climate_projections)
  
  # save
  save(climate_projections, file=file.path(out_dir,paste0(r,"_climate_projections_gdl_area_weight_llgni_llgdp.RData")))
  pb$tick()
  
  
}

