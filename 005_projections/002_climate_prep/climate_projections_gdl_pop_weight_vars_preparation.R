### future climate preprocessing, gdlcode level 

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


### historical climate
data_cl_hist<- read_csv("data/climate_data/era5/data_climate_gdl_1950_2023_updated.csv")

### proj climate
data_cl_dir<-"data/climate_data/projections"

ssp1<-read_csv(file.path(data_cl_dir, "ssp126_projection_data_climate_gdlcode_pop_weight_2015_2100.csv"))
ssp2<-read_csv(file.path(data_cl_dir, "ssp245_projection_data_climate_gdlcode_pop_weight_2015_2100.csv"))
ssp3<-read_csv(file.path(data_cl_dir, "ssp370_projection_data_climate_gdlcode_pop_weight_2015_2100.csv"))
ssp5<-read_csv(file.path(data_cl_dir, "ssp585_projection_data_climate_gdlcode_pop_weight_2015_2100.csv"))


climate_projections_big<-dplyr::bind_rows(ssp1, ssp2, ssp3, ssp5)
rm(ssp1,ssp2,ssp3,ssp5)
# climate_projections_big<-climate_projections_big%>%drop_na()

climate_projections_big <- climate_projections_big %>%
  pivot_wider(names_from = variable, values_from = value)


data_cl_hist<-data_cl_hist[which(data_cl_hist$year %in% 1990:2020),]
data_cl_hist <- data_cl_hist %>% group_by(gdlcode) %>% mutate(
  TM_mean=mean(TM), 
  RR_mean=mean(RR), 
  TVAR_mean=mean(TVAR), 
  HW_mean=mean(HW), 
  RX_mean=mean(RX), 
  PEXT_mean=mean(PEXT), 
  WD_mean=mean(WD), 
  SPI_mean=mean(SPI), 
  SPEI_mean=mean(SPEI), 
  PET_mean=mean(PET), 
)

data_cl_hist<-unique(data_cl_hist[, c(2,17:26)]) # drop variables with name equal in cl proj, drop year

### climate projections 
ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


# unify with hist
climate_projections_big<-dplyr::left_join(climate_projections_big, data_cl_hist)

### gdlcodes
gdl_shape_file<-read_csv("data/gdlcodes_iso.csv")

climate_projections_big<-dplyr::left_join(climate_projections_big, gdl_shape_file)


# setup variables 

varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI")

m_modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean")
gc()

################################################################################

regions<-unique(climate_projections_big$gdlcode)

pb=progress::progress_bar$new(total=length(regions))
pb$tick(0)

for (r in regions[1:length(regions)]){
  climate_projections<-climate_projections_big[climate_projections_big$gdlcode==r,]
  # climate manipulation
  
  climate_projections <- climate_projections %>% 
    arrange(gdlcode, model, ssp, year) %>% 
    group_by(gdlcode,  model, ssp) %>%
    dplyr::mutate(diff_TM = TM - dplyr::lag(TM, n = 1, default = NA),
                  diff_TVAR = TVAR - dplyr::lag(TVAR, n = 1, default = NA),
                  diff_RR = RR - dplyr::lag( RR, n = 1, default = NA), 
                  diff_HW = HW - dplyr::lag( HW, n = 1, default = NA), 
                  diff_SPEI = SPEI - dplyr::lag(SPEI , n = 1, default = NA), 
                  diff_SPI = SPI - dplyr::lag(SPI , n = 1, default = NA), 
                  diff_RX = RX - dplyr::lag( RX, n = 1, default = NA), 
                  diff_PEXT = PEXT - dplyr::lag( PEXT, n = 1, default = NA), 
                  diff_WD = WD - dplyr::lag( WD, n = 1, default = NA)
    )%>% 
    ungroup()
  
  
  for (i in 1:length(varns)){
    climate_projections[paste(modns[i],'_i_',varns[i],sep='')] <- climate_projections[modns[i]] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("TM",'_i_',varns[i],sep='')] <- climate_projections["TM"] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("RR",'_i_',varns[i],sep='')] <- climate_projections["RR"] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("TM",'_i_',modns[i],sep='')] <- climate_projections["TM"] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("RR",'_i_',modns[i],sep='')] <- climate_projections["RR"] * climate_projections[varns[i]]
  } 
  
  
  for (i in 1:length(varns)){
    climate_projections[paste(m_modns[i],'_i_',varns[i],sep='')] <- climate_projections[m_modns[i]] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("TM_mean",'_i_',varns[i],sep='')] <- climate_projections["TM_mean"] * climate_projections[varns[i]]
  } 
  
  for (i in 1:length(varns)){ # 
    climate_projections[paste("RR_mean",'_i_',varns[i],sep='')] <- climate_projections["RR_mean"] * climate_projections[varns[i]]
  } 
  
  
  # Convert the data frame to data.table
  setDT(climate_projections)
  
  # Function to create lagged columns for multiple variables
  # create_lags <- function(dt, varns, modns, m_modns, max_lag = 10) {
  #   # Step 1: Create standard lagged columns for each variable in `varns`
  #   for (var in varns) {
  #     for (lag_i in 1:max_lag) {
  #       dt[, paste0("lag_", lag_i, "_", var) := shift(get(var), lag_i), by = .(gdlcode, model, ssp)]
  #     }
  #   }
  #   
  #   # Step 2: Create lagged columns for combinations of `modns` and `varns`
  #   for (i in 1:length(modns)) {
  #     for (lag_i in 1:max_lag) {
  #       dt[, paste0("lag_", lag_i, "_", modns[i], "_i_", varns[i]) := shift(get(paste0(modns[i])) * get(paste0(varns[i])), lag_i), by = .(gdlcode, model, ssp)]
  #     }
  #   }
  #   
  #   # Step 3: Create lagged columns for combinations of `TM`, `RR` with `varns`
  #   for (v in varns) {
  #     for (lag_i in 1:max_lag) {
  #       dt[, paste0("lag_", lag_i, "_TM_i_", v) := shift(get("TM") * get(v), lag_i), by = .(gdlcode, model, ssp)]
  #       dt[, paste0("lag_", lag_i, "_RR_i_", v) := shift(get("RR") * get(v), lag_i), by = .(gdlcode, model, ssp)]
  #     }
  #   }
  #   
  #   # Step 4: Create lagged columns for combinations of `TM_mean`, `RR_mean`, `m_modns` with `varns`
  #   for (v in varns) {
  #     for (lag_i in 1:max_lag) {
  #       dt[, paste0("lag_", lag_i, "_TM_mean_i_", v) := shift(get("TM_mean") * get(v), lag_i), by = .(gdlcode, model, ssp)]
  #       dt[, paste0("lag_", lag_i, "_RR_mean_i_", v) := shift(get("RR_mean") * get(v), lag_i), by = .(gdlcode, model, ssp)]
  #     }
  #   }
  #   
  #   # Step 5: Create lagged columns for combinations of `m_modns` with `varns`
  #   for (i in 1:length(m_modns)) {
  #     for (lag_i in 1:max_lag) {
  #       dt[, paste0("lag_", lag_i, "_", m_modns[i], "_i_", varns[i]) := shift(get(paste0(m_modns[i])) * get(paste0(varns[i])), lag_i), by = .(gdlcode, model, ssp)]
  #     }
  #   }
  #   
  #   return(dt)
  # }
  
  create_lags_1 <- function(dt, varns, modns, m_modns, max_lag = 10) {
    
    
    # Step 1: Create standard lagged columns for each variable in `varns`
    for (var in varns) {
      gc()
      for (lag_i in 1:max_lag) {
        dt[, paste0("lag_", lag_i, "_", var) := shift(get(var), lag_i), by = .(gdlcode, model, ssp)]
      }
    }
    
    
    return(dt)
  }
  
  create_lags_2 <- function(dt, varns, modns, m_modns, max_lag = 10) {
    
    
    # Step 2: Create lagged columns for combinations of `modns` and `varns`
    for (i in 1:length(modns)) {
      gc()
      for (lag_i in 1:max_lag) {
        dt[, paste0("lag_", lag_i, "_", modns[i], "_i_", varns[i]) := shift(get(paste0(modns[i])) * get(paste0(varns[i])), lag_i), by = .(gdlcode, model, ssp)]
      }
    }
    
    return(dt)
  }
  
  create_lags_3 <- function(dt, varns, modns, m_modns, max_lag = 10) {
    
    
    # Step 3: Create lagged columns for combinations of `TM`, `RR` with `varns`
    for (v in varns) {
      gc()
      for (lag_i in 1:max_lag) {
        
        dt[, paste0("lag_", lag_i, "_TM_i_", v) := shift(get("TM") * get(v), lag_i), by = .(gdlcode, model, ssp)]
        dt[, paste0("lag_", lag_i, "_RR_i_", v) := shift(get("RR") * get(v), lag_i), by = .(gdlcode, model, ssp)]
      }
    }
    
    
    return(dt)
  }
  
  create_lags_4 <- function(dt, varns, modns, m_modns, max_lag = 10) {
    
    
    # Step 4: Create lagged columns for combinations of `TM_mean`, `RR_mean`, `m_modns` with `varns`
    for (v in varns) {
      gc()
      for (lag_i in 1:max_lag) {
        dt[, paste0("lag_", lag_i, "_TM_mean_i_", v) := shift(get("TM_mean") * get(v), lag_i), by = .(gdlcode, model, ssp)]
        dt[, paste0("lag_", lag_i, "_RR_mean_i_", v) := shift(get("RR_mean") * get(v), lag_i), by = .(gdlcode, model, ssp)]
      }
    }
    
    return(dt)
  }
  
  create_lags_5 <- function(dt, varns, modns, m_modns, max_lag = 10) {
    
    
    # Step 5: Create lagged columns for combinations of `m_modns` with `varns`
    for (i in 1:length(m_modns)) {
      gc()
      for (lag_i in 1:max_lag) {
        dt[, paste0("lag_", lag_i, "_", m_modns[i], "_i_", varns[i]) := shift(get(paste0(m_modns[i])) * get(paste0(varns[i])), lag_i), by = .(gdlcode, model, ssp)]
      }
    }
    
    return(dt)
  }
  
  
  
  
  
  climate_projections <- create_lags_1(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  max_lag = 10)
  climate_projections <- create_lags_2(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  max_lag = 10)
  climate_projections <- create_lags_3(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  max_lag = 10)
  climate_projections <- create_lags_4(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  max_lag = 10)
  climate_projections <- create_lags_5(climate_projections, varns = varns,modns=modns, m_modns=m_modns,  max_lag = 10)
  
  
  # Check if lag columns are created
  #print(names(climate_projections))
  
  ### save 
  
  save(climate_projections, file=file.path(out_dir,paste0(r,"_climate_projections_gdl_pop_weight.RData")))
  pb$tick()
}


