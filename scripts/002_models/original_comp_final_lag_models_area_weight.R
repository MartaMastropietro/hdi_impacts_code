
rm(list=ls())

source("scripts/003_models/feols_lags_plot_funcs.R")

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
out_dir<-"output/models/final_lag_mods"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/final_lag_mods/area_weight"
if(!dir.exists(out_dir)){dir.create(out_dir)}




### data
data<- read_csv("output/data_hdi_original_comp_climate_1990_2020_less_na.csv")

all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

### data climate
data_cl <- read_csv("data/climate_data/era5/data_climate_gdl_pop_weight_1950_2023.csv")
rr_cols <- grep("RR", names(data_cl), value = TRUE)
data_cl[rr_cols] <- lapply(data_cl[rr_cols], function(x) x / 360)


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
for ( i in 0:10){
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

# check
# data$TVAR_i_diff_TVAR
# data$lag_0_TVAR_i_diff_TVAR

# mean hist climate

# ten years avg of previous years 
# data<-left_join(data_cl, data)

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
for ( i in 0:10){
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

# go back to right time period

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

pattern<-"conflict|exp_edu|exp_health|trade|lag_gr|gr" # to discard coefficients rows correspondent to contr, ar part 


vars_corr<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)", "PET"),
  modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean"),
  units=c("°C", "mm/day", "°C", "°C",  "mm", "mm", "days", "", "", "" )
)

vars_corr_10y<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean"),
  units=c("°C", "mm/day", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

vars_corr_cont<-data.frame(
  varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET"),
  ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)",  "PET"),
  modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET"),
  units=c("°C", "mm/day", "°C", "°C",  "mm", "mm", "days", "", "" , "" )
)

save_model<-function(m, pattern, o, type, spec, NL, o_dir){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  cov_iso=vcov(m, cluster=~iso3)[!grepl(pattern, row.names(vcov(m, cluster=~iso3))), !grepl(pattern, colnames(vcov(m, cluster=~iso3))) ]
  cov_dk=vcov(m, "DK")[!grepl(pattern, row.names(vcov(m, "DK"))), !grepl(pattern, colnames(vcov(m, "DK"))) ]
  
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(o_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
  write.csv(coefs, file= file.path(o_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
  write.csv(cov, file=file.path(o_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
  write.csv(cov_iso, file=file.path(o_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov_iso.csv')))
  write.csv(cov_dk, file=file.path(o_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov_dk.csv')))
  
  return(list(tab, coefs, cov))
}


################################################################################
###########################################################################


### put all extr together, check again consistency among different schemes of mod

out_dir_final<-file.path(out_dir, "cons_N_lags_mix")
if(!dir.exists(out_dir_final)){dir.create(out_dir_final)}

out_dir_final_plots<-file.path(out_dir_final,"shocks_figures")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

# eys looks good at n=7,8 (no need for rr mod)
# leb tm rr look not sign, n=5 if we need control (no need for tm mod)
# gnipc tm 8 lags, rr not sign and no need control 

# n lags tp selected before
N_temp_eys<-7
N_rain_eys<-2
N_temp_leb<-1
N_rain_leb<-5
N_temp_gnipc<-8
N_rain_gnipc<-0


### use n mixture selected conservatively 
N="_mix_"
n<-N
data_temp<-data

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
  
  
  ######################
  
  o<-"gr_eys"
  N_temp<-N_temp_eys
  N_rain<-N_rain_eys
  
  ### formula part of temp, rain contr 
  mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                          paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                          paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                          paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:1,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:1,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:3,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))
  
  
  extr_formula<- paste0(c(wd_formula, 
                          
                          tvar_formula),
                        collapse = "+")
  
  extr_formula_big<-paste0(c(wd_formula, 
                             pext_formula,
                             rx_formula,  tvar_formula),
                           collapse = "+")
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_big"
  r=paste0(extr_formula_big
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_big_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr, + controls
  type<-"all_extr_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_big_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_big_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # contr
           "l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
  ######################
  
  o<-"gr_leb"
  
  N_temp<-N_temp_leb
  N_rain<-N_rain_leb
  
  ### formula part of temp, rain contr 
  mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                          paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                          paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                          paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:1,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:1,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_hw], "_i_", varns[v_hw]))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:1,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:1,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:3,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          tvar_formula, 
                          wd_formula),
                        collapse = "+")
  
  
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  
  ### all extr, + controls
  type<-"all_extr_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_eys, 0:6)+  l(gr_gnipc, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # contr
           "l(gr_gnipc, 0:6)+  l(gr_eys, 0:6)+l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
  ######################
  
  o<-"gr_gnipc"
  
  N_temp<-N_temp_gnipc
  N_rain<-N_rain_gnipc
  
  ### formula part of temp, rain contr 
  mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                          paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                          paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                          paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"))
  
  
  
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:9,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:9,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:7,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:6,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))
  
  extr_formula_big<- paste0(c(hw_formula, 
                              wd_formula, 
                              tvar_formula),
                            collapse = "+")
  extr_formula<- paste0(c(wd_formula, 
                          tvar_formula),
                        collapse = "+")
  
  models<-list()
  
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_big"
  r=paste0(extr_formula_big
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_big_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr, + controls
  type<-"all_extr_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_leb, 0:6)+ l(gr_eys, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_big_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_big_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_big, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_leb, 0:6)+ l(gr_eys, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # contr
           "l(gr_leb, 0:6)+ l(gr_eys, 0:6)+l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
  
}
