

rm(list=ls())

source("scripts/003_models/feols_lags_plot_funcs.R")

source("utils/cross_validation_fixest.R")

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

# output dir
out_dir<-"output/models/final_lag_mods/oaat_add"
if(!dir.exists(out_dir)){dir.create(out_dir)}



### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

### data climate
data_cl <- read_csv("data/climate_data/era5/data_climate_gdl_pop_weight_1950_2023.csv")


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

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


# mean hist climate

# long term avg
data <- data %>% group_by(gdlcode) %>% mutate(
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

# ten years avg of previous years 
data<-left_join(data_cl, data)

library(slider)
data <- data %>%
  arrange(gdlcode, year) %>%  
  group_by(gdlcode) %>%
  mutate(
    TM_mean = mean(TM),
    RR_mean = mean(RR),
    TVAR_mean = mean(TVAR),
    HW_mean = mean(HW),
    RX_mean = mean(RX),
    PEXT_mean = mean(PEXT),
    WD_mean = mean(WD),
    SPI_mean = mean(SPI),
    SPEI_mean = mean(SPEI),
    PET_mean = mean(PET),
    # Compute 10-year moving averages
    TM_10y_mean = slide_dbl(TM, mean, .before = 9, .complete = TRUE),
    RR_10y_mean = slide_dbl(RR, mean, .before = 9, .complete = TRUE),
    TVAR_10y_mean = slide_dbl(TVAR, mean, .before = 9, .complete = TRUE),
    HW_10y_mean = slide_dbl(HW, mean, .before = 9, .complete = TRUE),
    RX_10y_mean = slide_dbl(RX, mean, .before = 9, .complete = TRUE),
    PEXT_10y_mean = slide_dbl(PEXT, mean, .before = 9, .complete = TRUE),
    WD_10y_mean = slide_dbl(WD, mean, .before = 9, .complete = TRUE),
    SPI_10y_mean = slide_dbl(SPI, mean, .before = 9, .complete = TRUE),
    SPEI_10y_mean = slide_dbl(SPEI, mean, .before = 9, .complete = TRUE),
    PET_10y_mean = slide_dbl(PET, mean, .before = 9, .complete = TRUE)
  )


# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns_mean=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")
m_modns_10y=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean")
m_modns_cont=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")


adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(m_modns_mean[i],'_i_',varns[i],sep='')] <- data[m_modns_mean[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_10y[i],'_i_',varns[i],sep='')] <- data[m_modns_10y[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_cont[i],'_i_',varns[i],sep='')] <- data[m_modns_cont[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 


for (i in 1:length(varns)){ # 
  data[paste("TM_10y_mean",'_i_',varns[i],sep='')] <- data["TM_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_10y_mean",'_i_',varns[i],sep='')] <- data["RR_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_mean[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_mean[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_10y[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_10y[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_cont[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_cont[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# go back to right time period -> exclude covid years 

data<-data%>%filter(year>=1990 & year<=2019)


v_temp<-which(varns=="diff_TM")
v_rain<-which(varns=="diff_RR")
v_tvar<-which(varns=="diff_TVAR")
v_hw<-which(varns=="diff_HW")
v_rx<-which(varns=="diff_RX")
v_pext<-which(varns=="diff_PEXT")
v_wd<-which(varns=="diff_WD")
v_spi<-which(varns=="diff_SPI")
v_spei<-which(varns=="diff_SPEI")



### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

pan_id<-c('gdlcode', 'year')


specs<-list("mean_mod", "mean_mod_10y")#, "cont_mod")

out_variables<-c("gr_gnipc", "gr_leb", "gr_eys")

cl_variables<-c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")


################################################################################

pattern<-"conflict|exp_edu|exp_health|trade|lag_gr|gr" # to discard coefficients rows correspondent to contr, autoreg part 


vars_corr<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)", "PET"),
  modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "", "" )
)

vars_corr_10y<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

vars_corr_cont<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET"),
  units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

save_model<-function(m, pattern, o, type, spec, NL, out_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  cov_iso=vcov(m, cluster=~iso3)[!grepl(pattern, row.names(vcov(m, cluster=~iso3))), !grepl(pattern, colnames(vcov(m, cluster=~iso3))) ]
  
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
  write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
  write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
  write.csv(cov_iso, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov_iso.csv')))
  
  return(list(tab, coefs, cov))
}


################################################################################
# assure same number of samples 
Nlags<-8
data<-data[data$year>as.numeric(min(data$year)+Nlags),]

################################################################################

# cross validation for lag number, one extr var at the time , with without tp 

# output dir
out_dir_cv<-file.path(out_dir, "cv")
if(!dir.exists(out_dir_cv)){dir.create(out_dir_cv)}


out_variables<-c("gr_gnipc", "gr_eys", "gr_leb")
model_types<-c("lags_tp_n_8_control", "lags_tp_n_6_control", "lags_tp_same_n_control", "lags_extr_alone")
mod_types<-c("mod_tm", "mod_rr", "mod_extr")
model_types_simple<-c("bhm_tp_control", "bhm_extr_alone", "fd_tp_control", "fd_extr_alone")
specs<-specs

results<-data.frame(expand.grid(out_var=out_variables, cl_var=m_modns_cont[-c(1:2)], 
                                model_type=model_types , spec=specs, mod_type=mod_types, 
                                n_lags_TM=c(0:8), n_lags_RR=c(0:8), n_lags_EXTR=1:9))
results<-rbind(results, data.frame(expand.grid(out_var=out_variables,cl_var=m_modns_cont[-c(1:2)], 
                                               model_type=model_types_simple[1:2] ,  
                                               spec="no_mod",mod_type="no_mod", 
                                               n_lags_TM=0, n_lags_RR=0, n_lags_EXTR=0)))
results<-rbind(results, data.frame(expand.grid(out_var=out_variables,cl_var=m_modns_cont[-c(1:2)], 
                                               model_type=model_types_simple[3:4] ,  
                                               spec="cont_mod",mod_type=mod_types, 
                                               n_lags_TM=0, n_lags_RR=0, n_lags_EXTR=0)))
results$loocv<-NA
results$l3ocv<-NA
results$aic<-NA
results$bic<-NA
results$wr2<-NA

N_temp_fix_8<-8
N_rain_fix_8<-8

N_temp_fix_6<-6
N_rain_fix_6<-6

for (n in 0:Nlags){
  
  for (o in out_variables){
    
    for (cl_var in m_modns_cont[-c(1:2)]){
      
      # n lags 
      N<-n
      
      if(N==0){
        
        spec<-"no_mod"
        mod_type="no_mod"
        N_temp<-0
        N_rain<-0
        
        
        model_type<-"bhm_tp_control"
        r=paste0("TM + TM_2 + RR + RR_2 + ", cl_var, "+", cl_var, "_2")
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        model_type<-"bhm_extr_alone"
        r=paste0( cl_var, "+", cl_var, "_2")
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        ####  first diff 
        
        spec="cont_mod"
        
        # extr mod
        mod_type="mod_extr"
        
        model_type<-"fd_tp_control"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", cl_var, "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        model_type<-"fd_extr_alone"
        r=paste0(cl_var, "+", cl_var, "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        # rr mod
        mod_type="mod_rr"
        
        model_type<-"fd_tp_control"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", "RR", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        model_type<-"fd_extr_alone"
        r=paste0( cl_var, "+", "RR", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        # tm mod
        mod_type="mod_tm"
        
        model_type<-"fd_tp_control"
        r=paste0(cl_var, "+", "TM", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        model_type<-"fd_extr_alone"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", "TM", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        
      }else if(N>0){
        
        
        
        for (spec in specs){
          
          # spec
          if(spec=="mean_mod"){
            vars_correspondaces=vars_corr
            m_modns=m_modns_mean
          }else if(spec=="mean_mod_10y"){
            vars_correspondaces=vars_corr_10y
            m_modns=m_modns_10y
          }else if(spec=="cont_mod"){
            vars_correspondaces=vars_corr_cont
            m_modns=m_modns_cont
          }
          
          
          
          v_extr<-which(m_modns_cont==cl_var)
          
          
          
          
          ### formula part of temp, rain contr 
          mean_cl_formula_8<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                    paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                    paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                    paste0("lag_",1:N_temp_fix_8,"_",varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_temp_fix_8,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_8,"_",varns[v_rain], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_8,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
          mean_cl_formula_6<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                    paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                    paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                    paste0("lag_",1:N_temp_fix_6,"_",varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_temp_fix_6,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_6,"_",varns[v_rain], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_6,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
          
          mean_cl_formula_same_n<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                         paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                         paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                         paste0("lag_",1:N,"_",varns[v_temp], collapse = "+"),"+", 
                                         paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                         paste0("lag_",1:N,"_",varns[v_rain], collapse = "+"),"+", 
                                         paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
          
          ext_formula<-paste0(paste0(varns[v_extr]), "+",
                              paste0(m_modns[v_extr], "_i_", varns[v_extr]),"+", 
                              paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                              paste0("lag_",1:N,"_",m_modns[v_extr],'_i_',varns[v_extr], collapse = "+"))
          ext_formula_tm<-paste0(paste0(varns[v_extr]), "+",
                                 paste0(m_modns[v_temp], "_i_", varns[v_extr]),"+", 
                                 paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                                 paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_extr], collapse = "+"))
          ext_formula_rr<-paste0(paste0(varns[v_extr]), "+",
                                 paste0(m_modns[v_rain], "_i_", varns[v_extr]),"+", 
                                 paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                                 paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_extr], collapse = "+"))
          
          
          ### extremes mod extremes
          model_type<-"lags_extr_alone"
          mod_type<-"mod_extr"
          N_temp<-0
          N_rain<-0
          r=paste0(ext_formula)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          ### tp + extremes mod extremes
          model_type<-"lags_tp_n_8_control"
          mod_type<-"mod_extr"
          N_temp<-N_temp_fix_8
          N_rain<-N_rain_fix_8
          r=paste0(mean_cl_formula_8 , "+", ext_formula)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          ### tp + extremes mod extremes
          model_type<-"lags_tp_n_6_control"
          mod_type<-"mod_extr"
          N_temp<-N_temp_fix_6
          N_rain<-N_rain_fix_6
          r=paste0(mean_cl_formula_6 , "+", ext_formula)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          
          ### tp + extremes mod extremes
          model_type<-"lags_tp_same_n_control"
          mod_type<-"mod_extr"
          N_temp<-N
          N_rain<-N
          r=paste0(mean_cl_formula_same_n , "+", ext_formula)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          
          
          ### extremes mod tm
          model_type<-"lags_extr_alone"
          mod_type<-"mod_tm"
          N_temp<-0
          N_rain<-0
          r=paste0(ext_formula_tm)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          ### tp + extremes mod tm
          model_type<-"lags_tp_n_8_control"
          mod_type<-"mod_tm"
          N_temp<-N_temp_fix_8
          N_rain<-N_rain_fix_8
          r=paste0(mean_cl_formula_8 , "+", ext_formula_tm)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          ### tp + extremes mod tm
          model_type<-"lags_tp_n_6_control"
          mod_type<-"mod_tm"
          N_temp<-N_temp_fix_6
          N_rain<-N_rain_fix_6
          r=paste0(mean_cl_formula_6 , "+", ext_formula_tm)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          ### tp + extremes mod extremes
          model_type<-"lags_tp_same_n_control"
          mod_type<-"mod_tm"
          N_temp<-N
          N_rain<-N
          r=paste0(mean_cl_formula_same_n , "+", ext_formula_tm)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          
          ### extremes mod rr
          model_type<-"lags_extr_alone"
          mod_type<-"mod_rr"
          N_temp<-0
          N_rain<-0
          r=paste0(ext_formula_rr)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          ### tp + extremes mod rr
          model_type<-"lags_tp_n_8_control"
          mod_type<-"mod_rr"
          N_temp<-N_temp_fix_8
          N_rain<-N_rain_fix_8
          r=paste0(mean_cl_formula_8 , "+", ext_formula_rr)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          ### tp 6 + extremes mod rr
          model_type<-"lags_tp_n_6_control"
          mod_type<-"mod_rr"
          N_temp<-N_temp_fix_6
          N_rain<-N_rain_fix_6
          r=paste0(mean_cl_formula_6 , "+", ext_formula_rr)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
          
          
          model_type<-"lags_tp_same_n_control"
          mod_type<-"mod_rr"
          N_temp<-N
          N_rain<-N
          r=paste0(mean_cl_formula_same_n , "+", ext_formula_rr)
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-which(results$out_var ==o & 
                      results$cl_var == cl_var &
                      results$model_type == model_type &
                      results$spec == spec &
                      results$mod_type == mod_type &
                      results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
          results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
          results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
          results$aic[id]<-AIC(m)
          results$bic[id]<-BIC(m)
          results$wr2[id]<-fixest::r2(m,type='wr2')
        }
        
      }
      
    }
    
  }
  
}



results <- apply(results,2,as.character)
write.csv(x=results, file=file.path(out_dir_cv, 'results_oaat_extr_lags.csv'))


### PLOT 

results <- read_csv(file.path(out_dir_cv, 'results_oaat_extr_lags.csv'))

for (o in out_variables){
  
  res<-results%>%filter(out_var==o)
  
  g1<-ggplot(res, aes(n_lags_EXTR, loocv, color=model_type))+geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g2<-ggplot(res, aes(n_lags_EXTR, l3ocv, color=model_type))+geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g3<-ggplot(res, aes(n_lags_EXTR, wr2, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g4<-ggplot(res, aes(n_lags_EXTR, aic, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g5<-ggplot(res, aes(n_lags_EXTR, bic, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  
  arrange<-ggpubr::ggarrange(g1,g2,g3,g4,g5, common.legend = TRUE, nrow=1, ncol=5)
  ggsave(file.path(out_dir_cv,paste0(o,"_lag_select_oaat.png")), arrange, width = 35, height = 7)
}

for (o in out_variables){
  for (cl in unique(results$cl_var)){
    
    res<-results%>%filter(out_var==o, cl_var==cl)
    
    g1<-ggplot(res, aes(n_lags_EXTR, loocv, color=model_type))+geom_point(aes(shape=mod_type))
    g2<-ggplot(res, aes(n_lags_EXTR, l3ocv, color=model_type))+geom_point(aes(shape=mod_type))
    g3<-ggplot(res, aes(n_lags_EXTR, wr2, color=model_type))  +geom_point(aes(shape=mod_type))
    g4<-ggplot(res, aes(n_lags_EXTR, aic, color=model_type))  +geom_point(aes(shape=mod_type))
    g5<-ggplot(res, aes(n_lags_EXTR, bic, color=model_type))  +geom_point(aes(shape=mod_type))
    
    arrange<-ggpubr::ggarrange(g1,g2,g3,g4,g5, common.legend = TRUE, nrow=1, ncol=5)%>%ggpubr::annotate_figure( top = ggpubr::text_grob(paste0(o, "_", cl)))
    ggsave(file.path(out_dir_cv,paste0(o, "_",cl,"_lag_select_oaat.png")), arrange, width = 35, height = 7)
    
  }
}



################################################################################


# cross validation for lag number, one extr var at the time , with without tp 
# lets check each variable either alone, tp, tp + conservative number of lags in other variables. 
# lets also check moderating variables: we want the best in terms of performance 

# output dir
out_dir_cv<-file.path(out_dir, "cv")
if(!dir.exists(out_dir_cv)){dir.create(out_dir_cv)}


out_variables<-c("gr_gnipc", "gr_eys", "gr_leb")
model_types<-c("lags_tp_n_mix_control", "lags_tp_n_8_control",  "lags_extr_alone", "lags_all_controls")
mod_types<-c("mod_tm", "mod_rr", "mod_extr")
model_types_simple<-c("bhm_tp_control", "bhm_extr_alone", "fd_tp_control", "fd_extr_alone")

results<-data.frame(expand.grid(out_var=out_variables, cl_var=m_modns_cont[-c(1:2)], 
                                model_type=model_types ,spec="mean_mod",  mod_type=mod_types, 
                                n_lags_TM=c(0:8), n_lags_RR=c(0:8), n_lags_EXTR=1:9))
results<-rbind(results, data.frame(expand.grid(out_var=out_variables,cl_var=m_modns_cont[-c(1:2)], 
                                               model_type=model_types_simple[1:2] ,  
                                               spec="no_mod",mod_type="no_mod", 
                                               n_lags_TM=0, n_lags_RR=0, n_lags_EXTR=0)))
results<-rbind(results, data.frame(expand.grid(out_var=out_variables,cl_var=m_modns_cont[-c(1:2)], 
                                               model_type=model_types_simple[3:4] ,  
                                               spec="cont_mod",mod_type=mod_types, 
                                               n_lags_TM=0, n_lags_RR=0, n_lags_EXTR=0)))
results$loocv<-NA
# results$l3ocv<-NA
results$aic<-NA
results$bic<-NA
results$wr2<-NA

N_temp_fix_8<-8
N_rain_fix_8<-8

N_temp_eys<-7
N_rain_eys<-7
N_temp_leb<-5
N_rain_leb<-5
N_temp_gnipc<-8
N_rain_gnipc<-8



for (n in 0:Nlags){
  
  for (o in out_variables){
    
    for (cl_var in m_modns_cont[-c(1:2)]){
      
      # n lags 
      N<-n
      
      if(N==0){
        
        spec<-"no_mod"
        mod_type="no_mod"
        N_temp<-0
        N_rain<-0
        
        
        model_type<-"bhm_tp_control"
        r=paste0("TM + TM_2 + RR + RR_2 + ", cl_var, "+", cl_var, "_2")
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        #results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        model_type<-"bhm_extr_alone"
        r=paste0( cl_var, "+", cl_var, "_2")
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        ####  first diff 
        
        spec="cont_mod"
        
        # extr mod
        mod_type="mod_extr"
        
        model_type<-"fd_tp_control"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", cl_var, "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        model_type<-"fd_extr_alone"
        r=paste0(cl_var, "+", cl_var, "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        # rr mod
        mod_type="mod_rr"
        
        model_type<-"fd_tp_control"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", "RR", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        model_type<-"fd_extr_alone"
        r=paste0( cl_var, "+", "RR", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        #results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        # tm mod
        mod_type="mod_tm"
        
        model_type<-"fd_tp_control"
        r=paste0(cl_var, "+", "TM", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        model_type<-"fd_extr_alone"
        r=paste0("diff_TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", cl_var, "+", "TM", "_i_diff_", cl_var)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        
      }else if(N>0){
        
        
        
      if(o=="gnipc"){
        N_temp_fix_mix=N_temp_gnipc
        N_rain_fix_mix=N_rain_gnipc
      }
      if(o=="leb"){
        N_temp_fix_mix=N_temp_leb
        N_rain_fix_mix=N_rain_leb
      }
      if(o=="eys"){
        N_temp_fix_mix=N_temp_eys
        N_rain_fix_mix=N_rain_eys
      }
      
        spec="mean_mod"
        
        vars_correspondaces=vars_corr
        m_modns=m_modns_mean
        
        v_extr<-which(m_modns_cont==cl_var)
        
        
        
        
        ### formula part of temp, rain contr 
        mean_cl_formula_8<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                  paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                  paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                  paste0("lag_",1:N_temp_fix_8,"_",varns[v_temp], collapse = "+"),"+", 
                                  paste0("lag_",1:N_temp_fix_8,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                  paste0("lag_",1:N_rain_fix_8,"_",varns[v_rain], collapse = "+"),"+", 
                                  paste0("lag_",1:N_rain_fix_8,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        mean_cl_formula_mix<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                    paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                    paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                    paste0("lag_",1:N_temp_fix_mix,"_",varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_temp_fix_mix,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_mix,"_",varns[v_rain], collapse = "+"),"+", 
                                    paste0("lag_",1:N_rain_fix_mix,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        mean_cl_formula_same_n<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                       paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                       paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                       paste0("lag_",1:N,"_",varns[v_temp], collapse = "+"),"+", 
                                       paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                       paste0("lag_",1:N,"_",varns[v_rain], collapse = "+"),"+", 
                                       paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        ext_formula<-paste0(paste0(varns[v_extr]), "+",
                            paste0(m_modns[v_extr], "_i_", varns[v_extr]),"+", 
                            paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                            paste0("lag_",1:N,"_",m_modns[v_extr],'_i_',varns[v_extr], collapse = "+"))
        ext_formula_tm<-paste0(paste0(varns[v_extr]), "+",
                               paste0(m_modns[v_temp], "_i_", varns[v_extr]),"+", 
                               paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                               paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_extr], collapse = "+"))
        ext_formula_rr<-paste0(paste0(varns[v_extr]), "+",
                               paste0(m_modns[v_rain], "_i_", varns[v_extr]),"+", 
                               paste0("lag_",1:N,"_",varns[v_extr], collapse = "+"),"+", 
                               paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_extr], collapse = "+"))
        
        
        ### extremes mod extremes
        model_type<-"lags_extr_alone"
        mod_type<-"mod_extr"
        N_temp<-0
        N_rain<-0
        r=paste0(ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        ### tp + extremes mod extremes
        model_type<-"lags_tp_n_8_control"
        mod_type<-"mod_extr"
        N_temp<-N_temp_fix_8
        N_rain<-N_rain_fix_8
        r=paste0(mean_cl_formula_8 , "+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        ### tp + extremes mod extremes
        model_type<-"lags_tp_n_mix_control"
        mod_type<-"mod_extr"
        N_temp<-N_temp_fix_mix
        N_rain<-N_rain_fix_mix
        r=paste0(mean_cl_formula_6 , "+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        #results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        ### tp + extremes mod extremes
        model_type<-"lags_tp_same_n_control"
        mod_type<-"mod_extr"
        N_temp<-N
        N_rain<-N
        r=paste0(mean_cl_formula_same_n , "+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        
        ### extremes mod tm
        model_type<-"lags_extr_alone"
        mod_type<-"mod_tm"
        N_temp<-0
        N_rain<-0
        r=paste0(ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        ### tp + extremes mod tm
        model_type<-"lags_tp_n_8_control"
        mod_type<-"mod_tm"
        N_temp<-N_temp_fix_8
        N_rain<-N_rain_fix_8
        r=paste0(mean_cl_formula_8 , "+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        ### tp + extremes mod tm
        model_type<-"lags_tp_n_mix_control"
        mod_type<-"mod_tm"
        N_temp<-N_temp_fix_mix
        N_rain<-N_rain_fix_mix
        r=paste0(mean_cl_formula_6 , "+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        ### tp + extremes mod extremes
        model_type<-"lags_tp_same_n_control"
        mod_type<-"mod_tm"
        N_temp<-N
        N_rain<-N
        r=paste0(mean_cl_formula_same_n , "+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        
        ### extremes mod rr
        model_type<-"lags_extr_alone"
        mod_type<-"mod_rr"
        N_temp<-0
        N_rain<-0
        r=paste0(ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        ### tp + extremes mod rr
        model_type<-"lags_tp_n_8_control"
        mod_type<-"mod_rr"
        N_temp<-N_temp_fix_8
        N_rain<-N_rain_fix_8
        r=paste0(mean_cl_formula_8 , "+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        ### tp 6 + extremes mod rr
        model_type<-"lags_tp_n_mix_control"
        mod_type<-"mod_rr"
        N_temp<-N_temp_fix_mix
        N_rain<-N_rain_fix_mix
        r=paste0(mean_cl_formula_6 , "+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
        
        model_type<-"lags_tp_same_n_control"
        mod_type<-"mod_rr"
        N_temp<-N
        N_rain<-N
        r=paste0(mean_cl_formula_same_n , "+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        id<-which(results$out_var ==o & 
                    results$cl_var == cl_var &
                    results$model_type == model_type &
                    results$spec == spec &
                    results$mod_type == mod_type &
                    results$n_lags_TM==N_temp  &  results$n_lags_RR==N_rain &  results$n_lags_EXTR==N )
        results$loocv[id]<-cross_validation_fixest(data_dem=data_dem, f=f)
        # results$l3ocv[id]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2019)
        results$aic[id]<-AIC(m)
        results$bic[id]<-BIC(m)
        results$wr2[id]<-fixest::r2(m,type='wr2')
        
      }
      
    }
    
  }
  
}



results <- apply(results,2,as.character)
write.csv(x=results, file=file.path(out_dir_cv, 'results_oaat_extr_lags.csv'))


### PLOT 

results <- read_csv(file.path(out_dir_cv, 'results_oaat_extr_lags.csv'))

for (o in out_variables){
  
  res<-results%>%filter(out_var==o)
  
  g1<-ggplot(res, aes(n_lags_EXTR, loocv, color=model_type))+geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g2<-ggplot(res, aes(n_lags_EXTR, l3ocv, color=model_type))+geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g3<-ggplot(res, aes(n_lags_EXTR, wr2, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g4<-ggplot(res, aes(n_lags_EXTR, aic, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  g5<-ggplot(res, aes(n_lags_EXTR, bic, color=model_type))  +geom_point(aes(shape=mod_type))+ facet_wrap(~cl_var)+ labs(title=paste0(o))
  
  arrange<-ggpubr::ggarrange(g1,g2,g3,g4,g5, common.legend = TRUE, nrow=1, ncol=5)
  ggsave(file.path(out_dir_cv,paste0(o,"_lag_select_oaat.png")), arrange, width = 35, height = 7)
}

for (o in out_variables){
  for (cl in unique(results$cl_var)){
    
    res<-results%>%filter(out_var==o, cl_var==cl)
    
    g1<-ggplot(res, aes(n_lags_EXTR, loocv, color=model_type))+geom_point(aes(shape=mod_type))
    g2<-ggplot(res, aes(n_lags_EXTR, l3ocv, color=model_type))+geom_point(aes(shape=mod_type))
    g3<-ggplot(res, aes(n_lags_EXTR, wr2, color=model_type))  +geom_point(aes(shape=mod_type))
    g4<-ggplot(res, aes(n_lags_EXTR, aic, color=model_type))  +geom_point(aes(shape=mod_type))
    g5<-ggplot(res, aes(n_lags_EXTR, bic, color=model_type))  +geom_point(aes(shape=mod_type))
    
    arrange<-ggpubr::ggarrange(g1,g2,g3,g4,g5, common.legend = TRUE, nrow=1, ncol=5)%>%ggpubr::annotate_figure( top = ggpubr::text_grob(paste0(o, "_", cl)))
    ggsave(file.path(out_dir_cv,paste0(o, "_",cl,"_lag_select_oaat.png")), arrange, width = 35, height = 7)
    
  }
}

