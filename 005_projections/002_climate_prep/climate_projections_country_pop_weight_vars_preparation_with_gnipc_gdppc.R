### lets add to climate projections also the projections of gnipc, and consequent variables interactions



rm(list=ls())
source("utils/libraries.R")

### out dir 
out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### climate data 
climate_projections<- arrow::read_feather(file.path(out_dir, "climate_projections_pop_weight.feather"))
climate_projections<-as.data.frame(climate_projections)

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
create_lags <- function(dt, varns, modns, m_modns, adap ,  max_lag = 10) {
  
  
  # 
  for (i in 1:length(modns)) {
    gc()
    for (lag_i in 1:max_lag) {
      dt[, paste0("lag_", lag_i, "_", adap[i], "_i_", varns[i]) := shift(get(paste0(adap[i])) * get(paste0(varns[i])), lag_i), by = .(iso3, model, ssp)]
    }
  }
  
  # 
  # for (i in 1:length(modns)) {
  #   gc()
  #   for (lag_i in 1:max_lag) {
  #     dt[, paste0("lag_", lag_i, "_", adap[i], "_i_", modns[i]) := shift(get(paste0(adap[i])) * get(paste0(modns[i])), lag_i), by = .(iso3, model, ssp)]
  #   }
  # }
  
  return(dt)
}


################################################################################

### gnipc 
var_name<-"gnipc"

data_proj<-read_csv(paste0("output/projections/original_comp/preproc_proj_", var_name, ".csv"))
colnames(data_proj)<-c("iso3", "year", "ssp", "gni_pc", "pop")

data_proj<-data_proj %>% 
  arrange(ssp, iso3, year) %>% 
  group_by(ssp, iso3) %>% 
  mutate( log_gni_pc =  log(gni_pc) ) %>% 
  mutate( lag_log_gni_pc =  dplyr::lag(log_gni_pc, n = 1, default = NA ) )

data_proj<-data_proj[, c("iso3", "year", "ssp", "lag_log_gni_pc")]
climate_projections<-left_join(climate_projections, data_proj)


### gdppc
var_name<-"gdp_pc"

data_proj<-read_csv(paste0("output/projections/original_comp/preproc_proj_", var_name, ".csv"))
colnames(data_proj)<-c("year",  "gdp_pc","iso3", "ssp", "pop")

data_proj<-data_proj %>% 
  arrange(ssp, iso3, year) %>% 
  group_by(ssp, iso3) %>% 
  mutate( log_gdp_pc =  log(gdp_pc) ) %>% 
  mutate( lag_log_grp_pc_usd =  dplyr::lag(log_gdp_pc, n = 1, default = NA ) )

data_proj<-data_proj[, c("iso3", "year", "ssp", "lag_log_grp_pc_usd")]
climate_projections<-left_join(climate_projections, data_proj)


### gnipc 
adap_gni=rep('lag_log_gni_pc',10)

### gdppc
adap_gdp=rep('lag_log_grp_pc_usd',10)



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

climate_projections <- create_lags(climate_projections, varns = varns,modns=modns, m_modns=m_modns, adap=adap_gni ,  max_lag = 10)

climate_projections <- create_lags(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  adap=adap_gdp ,  max_lag = 10)

# Check if lag columns are created
print(names(climate_projections))



### save 
library(arrow)
arrow::write_feather(climate_projections, file.path(out_dir, "climate_projections_llgnipc_llgdppc_pop_weight.feather"))
