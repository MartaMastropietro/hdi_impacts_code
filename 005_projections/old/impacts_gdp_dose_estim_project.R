### project gdp as in kotz 2024
### functions for proj damages 


rm(list=ls())
source("utils/libraries.R")

source("scripts/005_projections/proj_functions.R")

### out dir 

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_area<-"output/projections/original_comp/area_weight_gdlcode"
if(!dir.exists(out_dir_area)){dir.create(out_dir_area)}


### codes for connecting to gdlcode  
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")


ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


##################################################################################


### iteratively, for each lag model, specification (try simple, with controls, with autoreg)


out_dir_lag<-file.path(out_dir_area, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

coefs_dir<-"output/models/gni_gdp_models/complete_id"

specifications<-c("mean_mod_spec")
types<-c("all_extr")#, "all_extr_adap")#, "all_extr_controls", "all_extr_autoreg")
out_variables<-c("lgrp_pc_usd")
# out_variables<-c( "eys" )

for (spec in specifications){
  out_dir_lag_spec<-file.path(out_dir_lag, spec)
  if(!dir.exists(out_dir_lag_spec)){dir.create(out_dir_lag_spec)}
  
  for (out_var in out_variables){
    out_dir_lag_spec_var<-file.path(out_dir_lag_spec, out_var)
    if(!dir.exists(out_dir_lag_spec_var)){dir.create(out_dir_lag_spec_var)}
    
    for (type in types){
      out_dir_lag_spec_var_type<-file.path(out_dir_lag_spec_var, type)
      if(!dir.exists(out_dir_lag_spec_var_type)){dir.create(out_dir_lag_spec_var_type)}
      
      gc()
      # data
      N<-8
      if(out_var=="lgrp_pc_usd"){
        data_proj<-read_csv(file.path(out_dir, paste0("preproc_proj_", "gdp_pc", ".csv")))
      }
      
      data_proj<-left_join(data_proj, gdlcodes_iso)
      
      
      coefs_table<-read_csv(file.path(coefs_dir,paste0("d",out_var, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coef.csv')))
      coefs<-coefs_table$x
      names(coefs)<-coefs_table$...1
      
      climate_projections_dir<-"output/projections/climate_projections_gdl"
      agg<-"area_weight"
      clim_damages_calc(out_dir_lag_spec_var_type,
                        climate_projections_dir,
                        agg,
                        gdlcodes_iso$gdlcode, 
                        coefs, paste0("d",out_var))
      cl_deltas<-arrow::read_feather(file.path(out_dir_lag_spec_var_type,paste0("d",out_var,"_climate_deltas_", agg ,".feather")))
      
      project_impacts(paste0("d",out_var),  
                      gdlcodes_iso$gdlcode, 
                      data_proj,
                      coefs , 
                      out_dir_lag_spec_var_type,
                      cl_deltas, 
                      maps=FALSE)
      
    }
    
  }
  
}


out_dir<-out_dir_lag_spec_var_type

regions<-gdlcodes_iso$gdlcode
