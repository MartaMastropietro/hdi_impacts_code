### compute lags models oaat, with differengt specs


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

### tp alone


out_dir_oaat<-file.path(out_dir, "oaat_add")
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_oaat_plots<-file.path(out_dir_oaat,"shocks_figures")
if(!dir.exists(out_dir_oaat_plots)){dir.create(out_dir_oaat_plots)}

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
    
    
    models<-list()
    
    plots_tm_rr<-list()
    plots_tm<-list()
    plots_rr<-list()
    
    for (n in 1:9){
      
      # n lags 
      N_temp<-n
      N_rain<-n
      N<-n
      
      ### formula part of temp, rain contr 
      mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                              paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                              paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                              paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                              paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                              paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                              paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
      
      tm_formula<-paste0(varns[v_temp], "+", 
                         paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                         paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                         paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"))
      
      rr_formula<-paste0(varns[v_rain], "+", 
                         paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                         paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                         paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
      
      
     
      
      ### tm + rr
      type<-paste0("tm_rr_mod")
      r=paste0(mean_cl_formula)
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      # summary(m)
      # summary(m, vcov = ~ iso3)
      # summary(m, vcov = "DK")
      models[[length(models)+1]]<-m
      # save 
      save_model(m, pattern, o, type, spec, N, out_dir_oaat)
      plots_tm_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
      plots_tm_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
      
      ### tm 
      type<-paste0("tm_mod")
      r=paste0(tm_formula)
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      # summary(m)
      # summary(m, vcov = ~ iso3)
      # summary(m, vcov = "DK")
      models[[length(models)+1]]<-m
      # save 
      save_model(m, pattern, o, type, spec, N, out_dir_oaat)
      plots_tm[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
      plots_tm[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
      
      ### rr
      type<-paste0("rr_mod")
      r=paste0(rr_formula)
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      # summary(m)
      # summary(m, vcov = ~ iso3)
      # summary(m, vcov = "DK")
      models[[length(models)+1]]<-m
      # save 
      save_model(m, pattern, o, type, spec, N, out_dir_oaat)
      plots_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
      plots_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
      
      
    }
    
    # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o, "_", spec ,"_lag", "_tp_rr_models.html")))
    # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o, "_", spec ,"_lag", "_tp_rr_models_dk.html")))
    # modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o, "_",spec ,"_lag", "_tp_rr_models_iso.html")))
    
    # tm rr
    g<-ggpubr::ggarrange(plotlist=plots_tm_rr[["rob"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_tm_rr_", spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
    
    g<-ggpubr::ggarrange(plotlist=plots_tm_rr[["iso"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_tm_rr_", spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
    
    #tm
    g<-ggpubr::ggarrange(plotlist=plots_tm[["rob"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_tm_", spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
    
    g<-ggpubr::ggarrange(plotlist=plots_tm[["iso"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_tm_", spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
    
    #rr
    g<-ggpubr::ggarrange(plotlist=plots_rr[["rob"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_rr_", spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
    
    g<-ggpubr::ggarrange(plotlist=plots_rr[["iso"]])
    ggsave(file.path(out_dir_oaat_plots, paste0(o, "_rr_", spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
    
  }
  
  
}

################################################################################
# extr

# select a certain lag number for climate controls tp: 
# plots:
# eys looks good at n=7,8 (no need for rr mod)
# leb tm rr look not sign, n=5 if we need control (no need for tm mod)
# gnipc tm 8 lags, rr not sign and no need control 



out_dir_oaat<-file.path(out_dir, "oaat_add")
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_oaat_plots<-file.path(out_dir_oaat,"shocks_figures")
if(!dir.exists(out_dir_oaat_plots)){dir.create(out_dir_oaat_plots)}

for (spec in specs[1]){
    
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
      if(o =="gr_gnipc"){
        mix_ntm=8
        mix_nrr=4
      }
      
      if(o =="gr_leb"){
        mix_ntm=5
        mix_nrr=5
      }
      
      if(o =="gr_eys"){
        mix_ntm=7
        mix_nrr=7
      }
      
      for (cl_var in m_modns_cont[-c(1:2)]){
        
        v_extr<-which(m_modns_cont==cl_var)
        
        models<-list()
        
        plots_mod_extr<-list()
        plots_mod_tm<-list()
        plots_mod_rr<-list()
       
        plots_contr_mod_extr<-list()
        plots_contr_mod_tm<-list()
        plots_contr_mod_rr<-list()
        
        plots_contr8_mod_extr<-list()
        plots_contr8_mod_tm<-list()
        plots_contr8_mod_rr<-list()
        
        plots_contr_mix_mod_extr<-list()
        plots_contr_mix_mod_tm<-list()
        plots_contr_mix_mod_rr<-list()
        
        for (n in 1:9){
          
          # n lags 
          N_temp<-n
          N_rain<-n
          N<-n
          
        ### formula part of temp, rain contr 
        mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                                paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        mean_cl_formula_n6<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                paste0("lag_",1:6,"_",varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                paste0("lag_",1:6,"_",varns[v_rain], collapse = "+"),"+", 
                                paste0("lag_",1:6,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        mean_cl_formula_n8<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                   paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                   paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                   paste0("lag_",1:8,"_",varns[v_temp], collapse = "+"),"+", 
                                   paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                   paste0("lag_",1:8,"_",varns[v_rain], collapse = "+"),"+", 
                                   paste0("lag_",1:8,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        mean_cl_formula_nmix<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                                   paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                                   paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                                   paste0("lag_",1:mix_ntm,"_",varns[v_temp], collapse = "+"),"+", 
                                   paste0("lag_",1:mix_ntm,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                                   paste0("lag_",1:mix_nrr,"_",varns[v_rain], collapse = "+"),"+", 
                                   paste0("lag_",1:mix_nrr,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
        
        
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
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_mod_extr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_mod_extr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        
        ### extremes mod tm
        type<-paste0(cl_var ,"_mod_tm")
        r=paste0(ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_mod_tm[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_mod_tm[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### extremes mod rr
        type<-paste0(cl_var ,"_mod_rr")
        r=paste0(ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_mod_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_mod_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        
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
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mod_extr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mod_extr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp + extremes mod tm
        type<-paste0("tp_",cl_var ,"_mod_tm")
        r=paste0(mean_cl_formula,"+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mod_tm[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mod_tm[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp + extremes mod rr
        type<-paste0("tp_",cl_var ,"_mod_rr")
        r=paste0(mean_cl_formula,"+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mod_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mod_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        
        # tp fixed at mixed lags previously selected
        
        ### tp_mix + extremes mod extremes
        type<-paste0("tp_mix_",cl_var ,"_mod_extr")
        r=paste0(mean_cl_formula_nmix,"+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # summary(m)
        # summary(m, vcov = ~ iso3)
        # summary(m, vcov = "DK")
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mix_mod_extr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mix_mod_extr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp_mix + extremes mod tm
        type<-paste0("tp_mix_",cl_var ,"_mod_tm")
        r=paste0(mean_cl_formula_nmix,"+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mix_mod_tm[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mix_mod_tm[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp_mix + extremes mod rr
        type<-paste0("tp_mix_",cl_var ,"_mod_rr")
        r=paste0(mean_cl_formula_nmix,"+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr_mix_mod_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr_mix_mod_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        
        
        
        # tp fixed at 8 lags 
        
        ### tp8 + extremes mod extremes
        type<-paste0("tp8_",cl_var ,"_mod_extr")
        r=paste0(mean_cl_formula_n8,"+", ext_formula)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        # summary(m)
        # summary(m, vcov = ~ iso3)
        # summary(m, vcov = "DK")
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr8_mod_extr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr8_mod_extr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp8 + extremes mod tm
        type<-paste0("tp8_",cl_var ,"_mod_tm")
        r=paste0(mean_cl_formula_n8,"+", ext_formula_tm)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr8_mod_tm[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr8_mod_tm[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        ### tp8 + extremes mod rr
        type<-paste0("tp8_",cl_var ,"_mod_rr")
        r=paste0(mean_cl_formula_n8,"+", ext_formula_rr)
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # save 
        save_model(m, pattern, o, type, spec, N, out_dir_oaat)
        plots_contr8_mod_rr[["rob"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N)
        plots_contr8_mod_rr[["iso"]][[n]]<-plot_lags(out_dir_oaat , vars_correspondaces, out_dir_oaat_plots, o, type, spec, N, se="iso")
        
        
        # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o,"_",cl_var, "_", spec ,"_lagN", n, "_models.html")))
        # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o,"_",cl_var, "_", spec ,"_lagN", n, "_models_dk.html")))
        # modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_oaat, paste0(o,"_",cl_var, "_",spec ,"_lagN", n, "_models_iso.html")))
        
        
        }
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_tm[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_tm", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_tm[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_tm", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_rr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_rr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_rr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_rr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_extr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_extr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=20,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_mod_extr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_mod_extr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=20,  bg = "white")
        
        # contr
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_tm[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_tm", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_tm[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_tm", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_rr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_rr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_rr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_rr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_extr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_extr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mod_extr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mod_extr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        
        # contr 8
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_tm[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_tm", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_tm[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_tm", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_rr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_rr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_rr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_rr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_extr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_extr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr8_mod_extr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr8_mod_extr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        
        # contr mix
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_tm[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_tm", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_tm[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_tm", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_rr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_rr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_rr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_rr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_extr[["rob"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_extr", "_" , spec, "_lags_1_to_9_rob.png" )), width=30, height=30,  bg = "white")
        # 
        g<-ggpubr::ggarrange(plotlist=plots_contr_mix_mod_extr[["iso"]])
        ggsave(file.path(out_dir_oaat_plots, paste0(o, "_",cl_var,"_contr_mix_mod_extr", "_" , spec, "_lags_1_to_9_iso.png" )), width=30, height=30,  bg = "white")
        
        
    }
    
  }
  
}



################################################################################


### put all extr together, check again consistency among different schemes of mod

out_dir_final<-file.path(out_dir, "conservative_N_lags_mix")
if(!dir.exists(out_dir_final)){dir.create(out_dir_final)}

out_dir_final_plots<-file.path(out_dir_final,"shocks_figures")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

# eys looks good at n=7,8 (no need for rr mod)
# leb tm rr look not sign, n=5 if we need control (no need for tm mod)
# gnipc tm 8 lags, rr not sign and no need control 

# n lags tp selected before
N_temp_eys<-7
N_rain_eys<-7
N_temp_leb<-5
N_rain_leb<-5
N_temp_gnipc<-8
N_rain_gnipc<-8


### use n mixture selected conservatively 
N="_mix_"
n<-N

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
                             pext_formula,
                             rx_formula, 
                          tvar_formula),
                           collapse = "+")
  
  extr_formula_small<-paste0(c(wd_formula, 
                               pext_formula, tvar_formula),
                             collapse = "+")
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_small"
  r=paste0(extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_small_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_small_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_small_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                     paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                     paste0("lag_",1:1,"_",varns[v_tvar], collapse = "+"),"+", 
                     paste0("lag_",1:1,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                       paste0("lag_",1:3,"_",varns[v_wd], collapse = "+"),"+", 
                       paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          rx_formula,
                          tvar_formula, 
                          wd_formula),
                        collapse = "+")
  
  
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:7,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:6,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:2,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:2,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                       paste0("lag_",1:8,"_",varns[v_rx], collapse = "+"),"+", 
                       paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          wd_formula, 
                          pext_formula),
                        collapse = "+")
  
  models<-list()
  
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_eys, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
}

  

################################################################################
### use n mixture selected conservatively, add one lag to verify shocks get reassorbed

out_dir_final<-file.path(out_dir, "conservative_N_lags_mix")
if(!dir.exists(out_dir_final)){dir.create(out_dir_final)}

out_dir_final_plots<-file.path(out_dir_final,"shocks_figures_plus1")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

# n lags tp selected before +1
N_temp_eys<-8
N_rain_eys<-8
N_temp_leb<-6
N_rain_leb<-6
N_temp_gnipc<-8
N_rain_gnipc<-8


N="_mix_plus1_"
n<-N

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
                     paste0("lag_",1:2,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:2,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:4,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:4,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:6,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:7,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))
  
  
  extr_formula<- paste0(c(wd_formula, 
                          pext_formula,
                          rx_formula, 
                          tvar_formula),
                        collapse = "+")
  
  extr_formula_small<-paste0(c(wd_formula, 
                               pext_formula, tvar_formula),
                             collapse = "+")
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_small"
  r=paste0(extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_small_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_small_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_small_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
                     paste0("lag_",1:2,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:2,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:6,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:6,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:2,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:2,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:4,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:4,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          rx_formula,
                          tvar_formula, 
                          wd_formula),
                        collapse = "+")
  
  
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:8,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:7,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:7,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:3,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:3,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          wd_formula, 
                          pext_formula),
                        collapse = "+")
  
  models<-list()
  
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_eys, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
}


################################################################################
### use 8 lags

out_dir_final<-file.path(out_dir, "conservative_N_lags_mix")
if(!dir.exists(out_dir_final)){dir.create(out_dir_final)}

out_dir_final_plots<-file.path(out_dir_final,"shocks_figures_8N")
if(!dir.exists(out_dir_final_plots)){dir.create(out_dir_final_plots)}

# n lags tp 
N_temp_eys<-8
N_rain_eys<-8
N_temp_leb<-8
N_rain_leb<-8
N_temp_gnipc<-8
N_rain_gnipc<-8


N="_8_"
n<-N

for (spec in specs[1]){ #only mean mod
  
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
                     paste0("lag_",1:8,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:8,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:8,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:8,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_tvar], collapse = "+"))
  
  
  extr_formula<- paste0(c(wd_formula, 
                          pext_formula,
                          rx_formula, 
                          tvar_formula),
                        collapse = "+")
  
  extr_formula_small<-paste0(c(wd_formula, 
                               pext_formula, tvar_formula),
                             collapse = "+")
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_small"
  r=paste0(extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_small_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_small_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_small_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
                     paste0("lag_",1:8,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:8,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:8,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:8,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:8,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          rx_formula,
                          tvar_formula, 
                          wd_formula),
                        collapse = "+")
  
  
  models<-list()
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
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
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:8,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:8,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:8,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:8,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:8,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          wd_formula, 
                          pext_formula),
                        collapse = "+")
  
  models<-list()
  
  
  
  ### all extr
  type<-"all_extr"
  r=paste0(extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
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
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N)
  plot_lags(out_dir_final , vars_correspondaces, out_dir_final_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_eys, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
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
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
}

################################################################################


# divide by hdi level 
out_dir_final_rob<-"output/models/final_lag_mods/conservative_N_lags_mix"
if(!dir.exists(out_dir_final_rob)){dir.create(out_dir_final_rob)}
out_dir_final_rob<-"output/models/final_lag_mods/conservative_N_lags_mix/by_hdi_lev"
if(!dir.exists(out_dir_final_rob)){dir.create(out_dir_final_rob)}

data_hdi_components_undp_national_1990_2021 <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")
iso_level<-data_hdi_components_undp_national_1990_2021%>%select(iso3, hdicode)
iso_level<-unique(iso_level)
iso_level$hdicode[which(iso_level$hdicode=="Medium")]<-"Med"
iso_level$hdicode[which(iso_level$hdicode=="Very High")]<-"Very_high"

data<-inner_join(data, iso_level)
missing_iso<-data[which(is.na(data$hdicode)),]
count_lev<-data%>%group_by(hdicode)%>%mutate(n=n())%>%select(hdicode, n)%>%ungroup()
count_lev<-unique(count_lev)
# more or less similar in count 


# n lags tp selected before
N_temp_eys<-7
N_rain_eys<-7
N_temp_leb<-5
N_rain_leb<-5
N_temp_gnipc<-8
N_rain_gnipc<-8


### use n mixture selected conservatively 
N="_mix_"
n<-N

spec="mean_mod"

for (hdi_lev in setdiff(unique(data$hdicode), c(NA))){
  
  out_dir_final_rob_lev<-file.path(out_dir_final_rob, paste0(hdi_lev))
  if(!dir.exists(out_dir_final_rob_lev)){dir.create(out_dir_final_rob_lev)}
  out_dir_final_rob_lev_plots<-file.path(out_dir_final_rob_lev,"shocks_figures")
  if(!dir.exists(out_dir_final_rob_lev_plots)){dir.create(out_dir_final_rob_lev_plots)}
  
  data_temp<-data%>%filter(hdicode == hdi_lev)
  
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
                          pext_formula,
                          rx_formula, 
                          tvar_formula),
                        collapse = "+")
  
  extr_formula_small<-paste0(c(wd_formula, 
                               pext_formula, tvar_formula),
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  ### all extr reduced version
  type<-"all_extr_small"
  r=paste0(extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  ### all extr + tp reduced version
  type<-"all_extr_small_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+ l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
  ### all extr small, + controls
  type<-"all_extr_small_tp_ar"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6)  ")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_small_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula_small, "+",
           # ar
           paste0("l(",o, ", 1:6) + l(gr_gnipc, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
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
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:5,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:5,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  tvar_formula<-paste0(paste0(varns[v_tvar]), "+",
                       paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                       paste0("lag_",1:1,"_",varns[v_tvar], collapse = "+"),"+", 
                       paste0("lag_",1:1,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:3,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:3,"_",m_modns[v_temp],'_i_',varns[v_wd], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          rx_formula,
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
  
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  
  
  ######################
  
  o<-"gr_gnipc"
  
  N_temp<-N_temp_gnipc
  N_rain<-N_rain_gnipc
  
  ### formula part of temp, rain contr 
  mean_cl_formula<-paste0(varns[v_temp], "+", varns[v_rain], "+", 
                          paste0(m_modns[v_temp], "_i_", varns[v_temp]), "+",
                          paste0(m_modns[v_rain], "_i_", varns[v_rain]), "+",
                          paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_temp,"_",m_modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                          paste0("lag_",1:N_rain,"_",m_modns[v_rain],'_i_',varns[v_rain], collapse = "+"))
  
  
  
  hw_formula<-paste0(paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:7,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:7,"_",m_modns[v_temp],'_i_',varns[v_hw], collapse = "+"))
  wd_formula<-paste0(paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_rain], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:5,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:5,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"))
  pext_formula<-paste0(paste0(varns[v_pext]), "+",
                       paste0(m_modns[v_temp], "_i_", varns[v_pext]),"+", 
                       paste0("lag_",1:2,"_",varns[v_pext], collapse = "+"),"+", 
                       paste0("lag_",1:2,"_",m_modns[v_temp],'_i_',varns[v_pext], collapse = "+"))
  rx_formula<-paste0(paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_temp], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:2,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:2,"_",m_modns[v_temp],'_i_',varns[v_rx], collapse = "+"))
  
  extr_formula<- paste0(c(hw_formula, 
                          wd_formula, 
                          pext_formula),
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  ### all extr tp 
  type<-"all_extr_tp"
  r=paste0(mean_cl_formula,"+", 
           extr_formula
  )
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
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
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  type<-"all_extr_tp_ar_controls"
  r=paste0(mean_cl_formula,"+", 
           extr_formula, "+",
           # ar
           paste0("l(",o, ", 1:6) +l(gr_eys, 0:6)+l(gr_leb, 0:6)+  l(conflict, 0:6) + l(exp_health, 0:6) + l(trade, 0:6) + l(exp_edu, 0:6)")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data_temp , panel.id = pan_id)
  models[[length(models)+1]]<-m
  # save 
  save_model(m, pattern, o, type, spec, N, out_dir_final_rob_lev)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N)
  plot_lags(out_dir_final_rob_lev , vars_correspondaces, out_dir_final_rob_lev_plots, o, type, spec, N, se="iso")
  
  
  
  #### save tables
  modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_", spec ,"_lagN", n, "_models_dk.html")))
  modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir_final_rob_lev, paste0(o, "_",spec ,"_lagN", n, "_models_iso.html")))
  

  
}


################################################################################

### plot on map 


type<-"all_extr_tp"
spec<-"mean_mod"

# median of diff by region
data <- data %>% group_by(gdlcode) %>% mutate(
  diff_TM_sd=sd(diff_TM, na.rm=TRUE), 
  diff_RR_sd=sd(diff_RR, na.rm=TRUE), 
  diff_TVAR_sd=sd(diff_TVAR, na.rm=TRUE), 
  diff_HW_sd=sd(diff_HW, na.rm=TRUE), 
  diff_RX_sd=sd(diff_RX, na.rm=TRUE), 
  diff_PEXT_sd=sd(diff_PEXT, na.rm=TRUE), 
  diff_WD_sd=sd(diff_WD, na.rm=TRUE), 
  diff_SPI_sd=sd(diff_SPI, na.rm=TRUE), 
  diff_SPEI_sd=sd(diff_SPEI, na.rm=TRUE), 
  diff_PET_sd=sd(diff_PET, na.rm=TRUE)
)%>%ungroup()%>%
  mutate(
    diff_TM_sd_mean=mean(diff_TM_sd, na.rm=TRUE), 
    diff_RR_sd_mean=mean(diff_RR_sd, na.rm=TRUE), 
    diff_TVAR_sd_mean=mean(diff_TVAR_sd, na.rm=TRUE), 
    diff_HW_sd_mean=mean(diff_HW_sd, na.rm=TRUE), 
    diff_RX_sd_mean=mean(diff_RX_sd, na.rm=TRUE), 
    diff_PEXT_sd_mean=mean(diff_PEXT_sd, na.rm=TRUE), 
    diff_WD_sd_mean=mean(diff_WD_sd, na.rm=TRUE), 
    diff_SPI_sd_mean=mean(diff_SPI_sd, na.rm=TRUE), 
    diff_SPEI_sd_mean=mean(diff_SPEI_sd, na.rm=TRUE), 
    diff_PET_sd_mean=mean(diff_PET_sd, na.rm=TRUE)
  )

o<-out_variables[1]

N<-"_mix_"
data_plot<-data

length(unique(data$gdlcode))
length(which(unique(data$diff_HW_sd)<0))
unique(data$diff_HW_sd_mean)
quantile(unique(data$TM_mean), 0.90)

gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

data_plot<-inner_join(data_plot, gdl_shape_file)

out_dir_final<-file.path(out_dir, "conservative_N_lags_mix")
if(!dir.exists(out_dir_final)){dir.create(out_dir_final)}

coefs<- read.table(file.path(out_dir_final,paste0(o, '_',type, '_',spec, "_lagN", N,'_coef.csv')), sep = ",", header = TRUE)
colnames(coefs)<-c("variable","x" )

cov <- read.table(file.path(out_dir_final,paste0(o, '_',type, '_',spec, "_lagN", N, "_cov.csv")), sep = ",", header = TRUE)
colnames(cov)<-c("X", paste0(cov$X))

coeftable <- read.table(file.path(out_dir_final,paste0(o, '_',type, '_',spec, "_lagN", N, "_coeftab.csv")), sep = ",", header = TRUE)
colnames(coeftable)<-c("variable","Estimate", "Std_error", "t_value", "p_value" )

wr2 <- coeftable[coeftable$variable == "wr2", "Estimate"]
BIC <- coeftable[coeftable$variable == "BIC", "Estimate"]
AIC <- coeftable[coeftable$variable == "AIC", "Estimate"]

all_vars_mods <- coefs[grepl("mean_i_", coefs$variable),]
all_vars <- coefs[!grepl("mean_i_", coefs$variable),]

extract_result <- extract_vars_mods(coefs, N)
varns <- extract_result[[1]]
mods <- extract_result[[2]]
modns <- extract_result[[3]]
NVLs <- extract_result[[4]]


# Calculate means of moderating variables and standard deviations of climate variables
means <- calc_means(data, modns, "gdlcode")
stds <- calc_stds(data, varns, "gdlcode")

# using sd only 
vars_correspondaces<-vars_corr
unit<-vars_correspondaces$unit[match(modns, vars_correspondaces$modns)]
scales<-stds*100
margunit<-rep("1std", length(scales))
efunit <- "(%-point growth rate)"


# Calculate marginal effects and their errors
MEs <- calc_specNL_MEs(coefs, varns, mods, means, NVLs)
errors <- calc_specNL_errors(cov, varns, mods, means, NVLs)

folder <- out_dir_final


varnlabels<-vars_correspondaces[ match( varns, vars_correspondaces$varns) , "ext_names" ]




library(ggplot2)
library(gridExtra)

# Color palette for lines
cols <- c("#66c2a5", "#fc8d62", "#8da0cb")  # Using Dark2 palette colors

# Initialize a list to store ggplot objects
plots <- list()

for (v in seq_along(varns)) {
  var <- varns[v]
  mod <- mods[v]
  NLs <- seq(0, NVLs[v])  # Lags from 0 to NVL
  labels <- sprintf("%.6g", means[v, ])
  
  MEv <- MEs[[var]]  # Matrix of marginal effects
  errorv <- errors[[var]]  # Matrix of errors
  
  # Create a data frame for ggplot
  data <- data.frame(
    Lag = rep(NLs, times = ncol(means)),
    Quantile = as.factor(rep(seq_len(ncol(means)),each = length(NLs) )),
    ME = as.vector(scales[v] * MEv),
    Lower = as.vector(scales[v] * (MEv - errorv)),
    Upper = as.vector(scales[v] * (MEv + errorv))
  )
  
  data<-data%>%group_by(Quantile)%>%mutate(cum_ME=sum(ME))
  
  # Base plot
  p <- ggplot(data, aes(x = Lag, y = ME, color = Quantile, fill = Quantile)) +
    geom_line(aes(group = Quantile), size = 0.8) +
    geom_point(aes(group = Quantile), size = 2) +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, linetype = 0) +
    scale_color_manual(values = cols[1:ncol(means)], labels = paste(labels, unit[v])) +
    scale_fill_manual(values = cols[1:ncol(means)], labels = paste(labels, unit[v])) +
    labs(
      x = "Lag (year)",
      y = paste("Effect per", margunit[v], "increase\n", efunit),
      title = varnlabels[v]
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      plot.title = element_text(size = 11, face = "bold"),
      axis.title.x = element_text(size = 10),
      axis.title.y = element_text(size = 10),
      axis.text = element_text(size = 8),
      legend.text = element_text(size = 8)
    ) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "black") 
  
  
  # Add plot to list
  plots[[v]] <- p
}

# Arrange the plots in a grid
#plot_grid <- do.call(grid.arrange, c(plots, nrow = 2, 
#                     top = grid::textGrob(
#                       sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
#                       gp = grid::gpar(fontsize = 10, fontface = "bold") ) ) )
library(grid)
if(length(plots)==1){
  nrows=1
}else{nrows=2}
plot_grid <- do.call(grid.arrange, c(plots, list(nrow = nrows, 
                                                 top = textGrob(
                                                   sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
                                                   gp = gpar(fontsize = 10, fontface = "bold")
                                                 )
)))
# # Add a global title
# grid::grid.text(
#   sprintf("WR2=%.3g / BIC=%.4g / AIC=%.4g", wr2, BIC, AIC),
#   x = 0.5, y = 0.98, just = "right", gp = grid::gpar(fontsize = 10, fontface = "bold")
#)

# Extract unique quantile values and cumulative effects
quantile_labels <- unique(data$Quantile)
cum_ME_values <- sapply(quantile_labels, function(q) unique(data$cum_ME[data$Quantile == q]))

# Create the new legend labels by appending the cumulative effect to the original labels
legend_labels <- paste(labels, unit[v], "(Cum. Effect:", round(cum_ME_values, 3), ")")

# Modify the plot
p <- ggplot(data, aes(x = Lag, y = ME, color = as.factor(Quantile), fill = as.factor(Quantile))) +
  geom_line(aes(group = Quantile), size = 0.8) +
  geom_point(aes(group = Quantile), size = 2) +
  geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.3, linetype = 0) +
  scale_color_manual(values = cols[1:length(quantile_labels)], labels = legend_labels) +
  scale_fill_manual(values = cols[1:length(quantile_labels)], labels = legend_labels) +
  labs(
    x = "Lag (year)",
    y = paste("Effect per", margunit[v], "increase\n", efunit),
    title = varnlabels[v]
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.title = element_text(size = 11, face = "bold"),
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text = element_text(size = 8),
    legend.text = element_text(size = 8)
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")

# Print plot
print(p)

i=1
v<-varns[i]
m<-mods[i]
data_plot[c(paste0(v, "_effect"))]<-NA

r<-gdl_shape_file$gdlcode[1]
data_plot[r,c(paste0(v, "_effect"))]<-data_plot[r,c(colnames(data)[grep(paste0(v,"\\b"),colnames(data))])]
