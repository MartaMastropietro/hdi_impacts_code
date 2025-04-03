# test proj functions

rm(list=ls())
source("utils/libraries.R")

source("scripts/005_projections/proj_functions.R")

### data for historical estimation of dam func
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
modns_2=c('TM_2','RR_2', "TVAR_2", "HW_2", "RX_2", "PEXT_2", "WD_2", "SPI_2", "SPEI_2", "PET_2")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns_2[i],sep='')] <- data[adap[i]] * data[modns_2[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}



### out dir 


### codes for connecting to gdlcode  
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")


ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


#######################

out_variables<-c( "leb", "gnipc" , "eys" )

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"

if(!dir.exists(out_dir)){dir.create(out_dir)}


#lags number 
N<-8

coefs_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_vars")#, "extr_only")

#define regions 
regions<-gdlcodes_iso$gdlcode#[ which(gdlcodes_iso$gdlcode %in% unique(data_proj_gni$gdlcode)) ]



spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]

spec_type<-"dlm"
effect<-"growth_eff"

nboot<-5

################################################################################

coefs_boot_calc<-function(coefs, vcov, seed=666, m=1000){
  # from waidelich et al 2024
  # round the matrix to avoid asymmetry due to precision issues, then draw
  vcov <- round(vcov, 18)
  set.seed(seed)
  out <- mvtnorm::rmvnorm(n = m, mu = coefs , Sigma = vcov) %>% t()
  
  # NOTE: due to imprecision, mvrnorm() can return extremely small non-zero values for variables with coefficient & covariance set to zero
  # to address this, we manually overwrite these values with a clean zero
  vars_excluded <- names(coefs)[coefs == 0]
  out[vars_excluded, ] <- 0
  return(out)
}

# VAR
for(out_var in out_variables){
  
  var_name<-out_var
  #var_name<-out_var<-out_variables[3]
  
  # coefs
  coefs_table<-read_csv(file.path(coefs_dir,paste0("gr_",out_var, '_',type, '_',spec,'_lagN',as.character(N),'_coef.csv')))
  coefs<-coefs_table$x
  names(coefs)<-coefs_table$...1
  # cov
  cov_table<-read_csv(file.path(coefs_dir,paste0("gr_",out_var, '_',type, '_',spec,'_lagN',as.character(N),'_cov.csv')))
  cov_table$...1<-NULL
  cov_table<-as.matrix(cov_table)
  
  climate_projections_dir<-"output/projections/climate_projections_gdl"
  agg<-"pop_weight"
  coefs_boot<-coefs_boot_calc(coefs, cov_table, m=5)
  
  for (n in 1:nboot){
    # save a diff delta for each boot run
    deltas_path<-file.path(out_dir_lag_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,"_", n, ".feather"))
    
    if(!file.exists(deltas_path)){
      coefs_boot_n<-coefs_boot[,n]
      cl_deltas<-clim_deltas_calc(climate_projections_dir,
                                  agg,
                                  gdlcodes_iso$gdlcode, 
                                  coefs_boot_n, 
                                  paste0("gr_",out_var))
      arrow::write_feather(cl_deltas,deltas_path )
    }else{
      cl_deltas<-arrow::read_feather(deltas_path)
    }
  }
}


# VAR
for(out_var in out_variables){
  
  out_dir_comp<-"output/projections/original_comp" 
  data_proj<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", out_var, ".csv")))
  data_proj<-left_join(data_proj, gdlcodes_iso)
  data<-data_proj
  
  colnames(data)[colnames(data)=="Scenario"]<-"ssp"
  
  # add growth under ssp (interpolated since values we use are)
  data<-data %>% 
    arrange(ssp, gdlcode, year) %>% 
    group_by(ssp, gdlcode) %>% 
    mutate(gr_value_interp = log(value_interp) - log(dplyr::lag(value_interp, n = 1, default = NA)))%>%
    ungroup()
  
  
  expand_data<-expand.grid( year = unique(data$year), 
                            gdlcode = unique(data$gdlcode), 
                            ssp = unique(data$ssp), 
                            model = models_names )
  
  data<-inner_join(data, expand_data)
  data$model<-as.character(data$model)
  
  
  
  # set values in first year available 
  y<-2024
  data<-data%>%filter(year>=y)
  data[which(data$year==y), c("gr_value_interp")]<-0
  
  data_all<-data
  
  for (n in as.numeric(1:nboot)){ # parallelize?
    # save a diff delta for each boot run
    
    deltas_path<-file.path(out_dir_lag_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,"_", n, ".feather"))
    # deltas_path<-file.path(out_dir_lag_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,".feather"))
    
    all_deltas<-arrow::read_feather(deltas_path)
    
    # unify with cl deltas
    data_all<-inner_join(data_all, all_deltas)
    data_all[which(data_all$year==y), c("delta_clim_proj")]<-0
    
    
    data_all<-data_all %>% 
      filter(ssp %in% ssp_names)%>% 
      arrange(model, ssp, gdlcode, year)%>% 
      group_by(model, ssp, gdlcode) %>%
      mutate(first_value_interp= first(value_interp)) %>%
      # mutate("effect_cc_{{n}}" := cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1)) %>%
      # mutate(effect_cc_old = cumprod( gr_value_interp + delta_clim_proj + 1)) %>%
      mutate("cc_proj_value_{{n}}" := if_else(year == y, first_value_interp ,
                                     first_value_interp*cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1) ) ) %>%
      # mutate(cc_proj_value_old = if_else(year == y, first_value_interp ,
      #                                first_value_interp*effect_cc_old ) ) %>%
      # mutate("cc_proj_perc_effect_{{n}}" := 100*("cc_proj_value_{{n}}" -  value_interp)/value_interp, 
      #        "cc_proj_effect_{{n}}" := "cc_proj_value_{{n}}" -  value_interp ) %>%
      #  mutate(cc_proj_perc_effect_old = 100*(cc_proj_value_old -  value_interp)/value_interp, 
      #         cc_proj_effect_old = cc_proj_value_old -  value_interp ) %>%
      ungroup()
    
    # erase prev delta
    data_all$delta_clim_proj<-NULL
    
  }
  
}

col_names_boot<-which(colnames(data_all) %in% c(paste0("cc_proj_value_",1:31,"L")))
data_all[,col_names_boot]
med<-apply(data_all[,col_names_boot], 1, median)

