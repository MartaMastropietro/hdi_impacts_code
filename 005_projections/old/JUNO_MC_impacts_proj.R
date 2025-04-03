

rm(list=ls())

########################## libraries
# xlsx write
require(openxlsx)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)
library(fixest)
library(modelsummary)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)

########################## read imput args

args = base::commandArgs(trailingOnly=TRUE)


o<-args[2]
#o<-2

if(o==1){
  out_var<-"eys"
}else if(o==2){
  out_var<-"leb"
}else if(o==3){
  out_var<-"gnipc"}


var_name<-out_var


########################### parallelize

# parallelize 
cores=detectCores()

# cl <- makeCluster(cores[1]-1) # locale 
 cl <- makeCluster(cores[1]) # zeus
# cl <- makeCluster(floor(cores[1]/3)) # zeus
 registerDoParallel(cl)

########################## functions

clim_deltas_calc<-function(
    climate_projections_dir,
    agg,
    regions, 
    coefs, 
    var_name
){
  
  all_deltas<-data.frame()
  
  vars<-names(coefs)
  
  pb=progress::progress_bar$new(total=length(regions))
  pb$tick(0)
  for ( r in regions ){
    gc()
    load(file.path(climate_projections_dir, paste0(r,"_climate_projections_gdl_",agg,".RData")))
    climate_projections<-as.data.frame(climate_projections)
    climate_projections<-climate_projections%>% mutate(
      TM_2=TM^2, 
      RR_2=RR^2, 
      TVAR_2=TVAR^2, 
      HW_2=HW^2, 
      RX_2=RX^2, 
      PEXT_2=PEXT^2, 
      WD_2=WD^2, 
      SPI_2=SPI^2, 
      SPEI_2=SPEI^2 
      #PET_2=PET^2
    )
    
    
    ## TODO insert here bootstrap of coefs, decide how to save (maybe one table per boot to avoid big data stored)
    climate_projections$delta_clim_proj=as.numeric(as.matrix(climate_projections[, c(vars)]) %*% c(coefs))  #rowSums(t(apply(as.matrix(climate_projections[, vars]), 1, function(x) coefs*x)),na.rm=T)
    
    all_deltas<-rbind(all_deltas, climate_projections[, c("gdlcode", "year", "model", "ssp", "delta_clim_proj")])
    pb$tick()
  }
  
  return(all_deltas)
}


coefs_boot_calc<-function(coefs, vcov, seed=666, m=1000){
  # from waidelich et al 2024
  # round the matrix to avoid asymmetry due to precision issues, then draw
  vcov <- round(vcov, 18)
  set.seed(seed)
  out <- mvtnorm::rmvnorm(n = m, mean = coefs , sigma = vcov) %>% t()
  
  # NOTE: due to imprecision, mvrnorm() can return extremely small non-zero values for variables with coefficient & covariance set to zero
  # to address this, we manually overwrite these values with a clean zero
  vars_excluded <- names(coefs)[coefs == 0]
  out[vars_excluded, ] <- 0
  return(out)
}
########################## setup 



#lags number 
N<-8

coefs_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_vars")#, "extr_only")


### codes for connecting to gdlcode  
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")

regions<-gdlcodes_iso$gdlcode#[ which(gdlcodes_iso$gdlcode %in% unique(data_proj_gni$gdlcode)) ]


ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]

spec_type<-"dlm"
effect<-"growth_eff"


### out dir 

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"


out_dir_lag<-file.path(out_dir_pop, "lag_models")
out_dir_lag_spec<-file.path(out_dir_lag, spec)
if(!dir.exists(out_dir_lag_spec)){dir.create(out_dir_lag_spec)}
out_dir_lag_spec_var<-file.path(out_dir_lag_spec, out_var)
if(!dir.exists(out_dir_lag_spec_var)){dir.create(out_dir_lag_spec_var)}
out_dir_lag_spec_var_type<-file.path(out_dir_lag_spec_var, type)
if(!dir.exists(out_dir_lag_spec_var_type)){dir.create(out_dir_lag_spec_var_type)}
out_dir_lag_spec_var_type_vars<-file.path(out_dir_lag_spec_var_type, vars)
if(!dir.exists(out_dir_lag_spec_var_type_vars)){dir.create(out_dir_lag_spec_var_type_vars)}



climate_projections_dir<-"output/projections/climate_projections_gdl"
agg<-"pop_weight"



out_dir_comp<-"output/projections/original_comp" 
data_proj<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", out_var, ".csv")))
data_proj<-left_join(data_proj, gdlcodes_iso)

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


data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))

load(file.path(climate_projections_dir, paste0(regions[1],"_climate_projections_gdl_",agg,".RData")))
climate_projections<-as.data.frame(climate_projections)
climate_projections$model_ssp<-as.factor(paste0(climate_projections$model, "_", climate_projections$ssp ))

data_all<-data_all%>%filter(ssp %in% ssp_names , !is.na(gdlcode) , model_ssp %in% climate_projections$model_ssp )


################################################################################
# boot calc, delta and impact calc for each n

nboot<-1000 

coefs_boot<-coefs_boot_calc(coefs, cov_table, m=nboot)

cc_proj_values<- foreach(n =(1:nboot) , .combine = 'cbind', .export = c("clim_deltas_calc"), .packages = c("dplyr") )%dopar%{
  
  coefs_boot_n<-coefs_boot[,n]
  all_deltas<-clim_deltas_calc(climate_projections_dir,
                               agg,
                               gdlcodes_iso$gdlcode, 
                               coefs_boot_n, 
                               paste0("gr_",out_var))
  
  # unify with cl deltas
  copy_dataall <- data_all
  
  copy_dataall<-inner_join(copy_dataall, all_deltas)
  copy_dataall[which(copy_dataall$year==y), c("delta_clim_proj")]<-0
  
  
  copy_dataall<-copy_dataall %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, gdlcode, year)%>% 
    group_by(model, ssp, gdlcode) %>%
    mutate(first_value_interp= first(value_interp)) %>%
    # mutate("effect_cc_{{n}}" := cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1)) %>%
    # mutate("cc_proj_value_{{n}}" := if_else(year == y, first_value_interp ,
    #                                         first_value_interp*cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1) ) ) %>%
    mutate(cc_proj_value = if_else(year == y, first_value_interp ,
                                            first_value_interp*cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1) ) ) %>%
    #mutate(cc_proj_perc_effect = 100*(cc_proj_value -  value_interp)/value_interp, 
    #       cc_proj_effect = cc_proj_value -  value_interp ) %>%
    ungroup()
  
  copy_dataall<-copy_dataall %>% 
   arrange(model, ssp, gdlcode, year)
  
  # cols<-which(colnames(copy_dataall) %in% c( "cc_proj_value" ))#, "cc_proj_perc_effect", "cc_proj_effect" ))
  copy_dataall$cc_proj_value

}


# reorder data as in function to preserve order 
data_all<-data_all %>% 
  arrange(model, ssp, gdlcode, year)
  
quant05 <- function(x){
  quantile(x, probs=0.05, na.rm=TRUE)
}
quant95 <- function(x){
  quantile(x, probs=0.95, na.rm=TRUE)
}
quant10 <- function(x){
  quantile(x, probs=0.10, na.rm=TRUE)
}
quant90 <- function(x){
  quantile(x, probs=0.90, na.rm=TRUE)
}
quant33 <- function(x){
  quantile(x, probs=0.33, na.rm=TRUE)
}
quant66 <- function(x){
  quantile(x, probs=0.66, na.rm=TRUE)
}
quant17 <- function(x){
  quantile(x, probs=0.17, na.rm=TRUE)
}
quant83 <- function(x){
  quantile(x, probs=0.83, na.rm=TRUE)
}



cc_proj_values<-as.data.frame(cc_proj_values)

med<-apply(cc_proj_values, 1, median)
mean<-apply(cc_proj_values, 1, mean)
q05<-apply(cc_proj_values, 1, quant05)
q95<-apply(cc_proj_values, 1, quant95)
q10<-apply(cc_proj_values, 1, quant10)
q90<-apply(cc_proj_values, 1, quant90)
q33<-apply(cc_proj_values, 1, quant33)
q66<-apply(cc_proj_values, 1, quant66)
q17<-apply(cc_proj_values, 1, quant17)
q83<-apply(cc_proj_values, 1, quant83)
 


data_all$cc_proj_value_mean<-mean
data_all$cc_proj_value_med<-med
data_all$cc_proj_value_q05<-q05
data_all$cc_proj_value_q95<-q95
data_all$cc_proj_value_q10<-q10
data_all$cc_proj_value_q90<-q90
data_all$cc_proj_value_q33<-q33
data_all$cc_proj_value_q66<-q66
data_all$cc_proj_value_q17<-q17
data_all$cc_proj_value_q83<-q83


arrow::write_feather(data_all,file.path(out_dir_lag,
                                        paste0("gr_",out_var,'_',spec,'_',type,'_',  agg, "_nlags",N,
                                               "_boot_impacts_intervals" , ".feather")) )

# also try to save all runs
cc_proj_values$ssp<-data_all$ssp
cc_proj_values$model<-data_all$model
cc_proj_values$gdlcode<-data_all$gdlcode
cc_proj_values$year<-data_all$year
cc_proj_values$cc_proj_value_mean<-mean
cc_proj_values$cc_proj_value_med<-med
cc_proj_values$cc_proj_value_q05<-q05
cc_proj_values$cc_proj_value_q95<-q95
cc_proj_values$cc_proj_value_q10<-q10
cc_proj_values$cc_proj_value_q90<-q90
cc_proj_values$cc_proj_value_q33<-q33
cc_proj_values$cc_proj_value_q66<-q66
cc_proj_values$cc_proj_value_q17<-q17
cc_proj_values$cc_proj_value_q83<-q83



arrow::write_feather(cc_proj_values,file.path(out_dir_lag,
                                        paste0("gr_",out_var,'_',spec,'_',type,'_',  agg, "_nlags",N,
                                               "_boot_impacts_all" , ".feather")) )

