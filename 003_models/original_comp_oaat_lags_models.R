### compute lags models oaat, with differengt specs


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
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models/oaat_add"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_plots<-file.path(out_dir,"shocks_figures")
if(!dir.exists(out_dir_plots)){dir.create(out_dir_plots)}


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
  data[paste(m_modns[i],'_i_',varns[i],sep='')] <- data[m_modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_10y[i],'_i_',varns[i],sep='')] <- data[m_modns_10y[i]] * data[varns[i]]
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



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_10y[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_10y[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
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


specs<-list("mean_mod", "mean_mod_10y", "cont_mod")

out_variables<-c("gr_gnipc", "gr_leb", "gr_eys", "gr_mys")

cl_variables<-c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")



################################################################################

pattern<-"conflict|exp_edu|exp_health|trade|lag_gr" # to discard coefficients rows correspondent to contr, autoreg part 


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


for (n in 7:9){
  
  # n lags 
  N_temp<-n
  N_rain<-n
  N<-n
  
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
    
    for (o in out_variables){
      
      for (cl_var in m_modns_cont[-c(1:2)]){
        
        v_extr<-which(m_modns_cont==cl_var)
        
        models<-list()
        
        ### formula part of temp, rain contr 
        mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                                paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
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
        type<-paste0(cl_var , "_mod_extr")
        r=paste0(ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        ### extremes mod tm
        type<-paste0(cl_var ,"_mod_tm")
        r=paste0(ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        ### extremes mod rr
        type<-paste0(cl_var ,"_mod_rr")
        r=paste0(ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        
        ### tp + extremes mod extremes
        type<-paste0("tp_",cl_var ,"_mod_extr")
        r=paste0(mean_cl_formula,"+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # summary(m)
        # summary(m, vcov = ~ iso3)
        # summary(m, vcov = "DK")
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        ### tp + extremes mod tm
        type<-paste0("tp_",cl_var ,"_mod_tm")
        r=paste0(mean_cl_formula,"+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        ### tp + extremes mod rr
        type<-paste0("tp_",cl_var ,"_mod_rr")
        r=paste0(mean_cl_formula,"+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
        plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N, se="iso")
        
        
        
        modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", type, "_", spec ,"_lagN", n, "_models.html")))
        modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_",type, "_", spec ,"_lagN", n, "_models_dk.html")))
        modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", type, "_",spec ,"_lagN", n, "_models_iso.html")))
        
      }
      
    }
      
  }
  
}

  
  