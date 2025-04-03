
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
out_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models/reduced_years"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_plots<-paste0(out_dir, "/shocks_figures")
if(!dir.exists(out_dir_plots)){dir.create(out_dir_plots)}


### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
# no 2020
data<-data[-which(data$year>=2020), ]

#controls
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

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

pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(m_modns[i],'_i_',varns[i],sep='')] <- data[m_modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
  }
}


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


for (n in 7:9){
  
  
  # n lags 
  N_temp<-n
  N_rain<-n
  N<-n
  
  
  ################################################################################
  
  pattern<-"conflict|exp_edu|exp_health|trade|lag_gr" # to discard coefficients rows correspondent to controls, autoreg part 
  
  
  vars_correspondaces<-data.frame(
    varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI"),
    ext_names=c("Mean temp.", "Total precip.","Temp. variability","Heat waves","Max 5 days cum. rain","Extr. rainy days", "Wet days",   "Droughts (SPI)", "Droughts (SPEI)"),
    modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean"),
    # modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI"),
    units=c("°C", "mm", "°C", "°C",  "mm", "mm", "days", "", "" )
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
    tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
    
    write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coeftab.csv')))
    write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_coef.csv')))
    write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_',spec, "_lagN", NL,'_cov.csv')))
    
    return(list(tab, coefs, cov))
  }
  
  
  ### formula part of temp, rain controls 
  mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                          paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                          paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                          paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  mean_cl_adap_formula<-paste0(paste0("lag_",1:N_temp,"_",adap[v_temp],'_i_',varns[v_temp], collapse = "+"),"+",
                               paste0("lag_",1:N_rain,"_",adap[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  ##############################################################################
  ### each complete mod with/without control and autoreg 
  ### model specification with mean controls  
  
  spec<-"mean_mod"
  
  ##############################################################################
  
  o<-"gr_eys"
  
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  
  
  extr_formula<-paste0(c(wd_formula, pext_formula), collapse="+")
  
  models<-list()
  
  
  
  ### extremes alone
  type<-"wd"
  r=paste0(wd_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  type<-"pext"
  r=paste0(pext_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### mean cl only
  type<-"tp"
  r=paste0(mean_cl_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### all, moderating with chosen form
  type<-"all_vars"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### all, moderating with chosen form, + controls
  type<-"all_vars_controls"
  r=paste0(mean_cl_formula, "+",
           extr_formula, "+",
           "l(conflict, 0:8) + l(exp_edu, 0:8)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  
  ### all, moderating with chosen form, + autoreg
  type<-"all_vars_autoreg"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  ### all, moderating with chosen form, + autoreg + controls
  type<-"all_vars_autoreg_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+", 
           "l(conflict, 0:8) + l(exp_edu, 0:8)", "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### check
  # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  
  
  #### save tables
  
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_iso.html")))
  
  
  ######################
  
  o<-"gr_leb"
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"),collapse = "+")
  
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"),collapse = "+")
  
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:N,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:N,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"),collapse = "+")
  
  spi_formula<-paste0(paste0(varns[v_spi]), "+",
                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),collapse = "+")
  
  
  extr_formula<-paste0( c(hw_formula,wd_formula,tvar_formula) , collapse = "+")
  
  
  models<-list()
  
  
  ### spi only 
  type<-"spi"
  r=paste0(spi_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### hw only 
  type<-"hw"
  r=paste0(hw_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### wd only 
  type<-"wd"
  r=paste0(wd_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### tvar only 
  type<-"tvar"
  r=paste0(tvar_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### tp only 
  type<-"tp"
  r=paste0(mean_cl_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  
  ### all, moderating with chosen form
  type<-"all_vars"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  ### all, moderating with chosen form, + controls
  type<-"all_vars_controls"
  r=paste0(mean_cl_formula, "+",
           extr_formula, "+",
           "l(conflict, 0:8) + l(exp_health, 0:8)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### all, moderating with chosen form, + autoreg
  type<-"all_vars_autoreg"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### all, moderating with chosen form, + autoreg + controls
  type<-"all_vars_autoreg_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           "l(conflict, 0:8) + l(exp_health, 0:8)", "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  # ### all, moderating with chosen form + ADAP
  # type<-"all_vars_adap"
  # r=paste0(mean_cl_formula,"+", 
  #          extr_formula, "+",
  #          # adap
  #          mean_cl_adap_formula,"+",
  #          extr_adap_formula
  # )
  # 
  # f= as.formula(paste( o, "~", r, "|" ,i ))
  # m = fixest::feols(f, data , panel.id = pan_id)
  # summary(m)
  # summary(m, vcov = ~ iso3)
  # summary(m, vcov = "DK")
  # models[[length(models)+1]]<-m
  # # save 
  # save_model(m, pattern, o, type, spec, N,out_dir)
  
  
  
  ### check
  # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  
  
  #### save tables
  
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_iso.html")))
  
  
  ######################
  
  
  o<-"gr_gnipc"
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"),collapse = "+")
  
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"),collapse = "+")
  
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"),collapse = "+")
  
  spi_formula<-paste0(paste0(varns[v_spi]), "+",
                      paste0(m_modns[v_temp], "_i_", varns[v_spi]),"+", 
                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_spi], collapse = "+"),collapse = "+")
  
  extr_formula<-paste0( c(hw_formula,wd_formula,rx_formula, spi_formula) , collapse = "+")
  
  
  models<-list()
  
  ### hw only 
  type<-"hw"
  r=paste0(hw_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### wd only 
  type<-"wd"
  r=paste0(wd_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### rx only 
  type<-"rx"
  r=paste0(wd_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### spi only 
  type<-"spi"
  r=paste0(spi_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### tp only 
  type<-"tp"
  r=paste0(mean_cl_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  ### all, moderating with chosen form
  type<-"all_vars"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### all, moderating with chosen form, + controls
  type<-"all_vars_controls"
  r=paste0(mean_cl_formula, "+",
           extr_formula, "+",
           "l(conflict, 0:8) + l(trade, 0:8)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### all, moderating with chosen form, + autoreg
  type<-"all_vars_autoreg"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### all, moderating with chosen form, + autoreg + controls
  type<-"all_vars_autoreg_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           "l(conflict, 0:8) + l(trade, 0:8)", "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  # ### all, moderating with chosen form + ADAP
  # type<-"all_vars_adap"
  # r=paste0(mean_cl_formula,"+", 
  #          extr_formula, "+",
  #          # adap
  #          mean_cl_adap_formula,"+",
  #          extr_adap_formula
  # )
  # 
  # f= as.formula(paste( o, "~", r, "|" ,i ))
  # m = fixest::feols(f, data , panel.id = pan_id)
  # summary(m)
  # summary(m, vcov = ~ iso3)
  # summary(m, vcov = "DK")
  # models[[length(models)+1]]<-m
  # # save 
  # save_model(m, pattern, o, type, spec, N,out_dir)
  # plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  ### check
  # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
  
  
  #### save tables
  
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_iso.html")))
  
  
  ##############################################################################
  
  o<-"gr_mys"
  
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:N,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  
  spi_formula<-paste0(paste0(varns[v_spi]), "+",
                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"))
  
  extr_formula<-paste0(c(wd_formula, pext_formula, spi_formula), collapse="+")
  
  models<-list()
  
  
  
  ### extremes alone
  type<-"rx"
  r=paste0(rx_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  type<-"pext"
  r=paste0(pext_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  type<-"spi"
  r=paste0(spi_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### mean cl only
  type<-"tp"
  r=paste0(mean_cl_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  ### all, moderating with chosen form
  type<-"all_vars"
  r=paste0(mean_cl_formula,"+", 
           spi_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  ### all, moderating with chosen form, + controls
  type<-"all_vars_controls"
  r=paste0(mean_cl_formula, "+",
           spi_formula, "+",
           "l(conflict, 0:8) + l(exp_edu, 0:8)"
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  
  ### all, moderating with chosen form, + autoreg
  type<-"all_vars_autoreg"
  r=paste0(mean_cl_formula,"+", 
           spi_formula, "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  
  
  ### all, moderating with chosen form, + autoreg + controls
  type<-"all_vars_autoreg_controls"
  r=paste0(mean_cl_formula,"+", 
           spi_formula, "+", 
           "l(conflict, 0:8) + l(exp_edu, 0:8)", "+",
           # autoreg
           paste0("lag_",o)
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  summary(m)
  summary(m, vcov = ~ iso3)
  summary(m, vcov = "DK")
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N,out_dir)
  plot_lags(out_dir , vars_correspondaces, out_dir_plots, o, type, spec, N)
  
  #### save tables
  
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lagN", n, "_models_iso.html")))
  
  
}


################################################################################
################################################################################
################################################################################
