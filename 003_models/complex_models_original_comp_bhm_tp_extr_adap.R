
rm(list=ls())
source("utils/libraries.R")

# xlsx write
require(openxlsx)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)
library(plm)
library(gt)


# output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_bhm_tp_extr_adap_complex_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )
# data_country<-left_join(data_country,all_controls )

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

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

modns=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
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
    
  }
}


### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"



################################################################################
################################################################################
################################################################################

pattern<-"conflict|exp_edu|exp_health|trade|lag_gr"

save_model_bhm<-function(m, pattern, o, type, spec){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  tab=rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  
  write.csv(tab, file= file.path(out_dir,paste0(o, '_',type, '_bhm_',spec,'_coeftab.csv')))
  write.csv(coefs, file= file.path(out_dir,paste0(o, '_',type, '_bhm_',spec,'_coef.csv')))
  write.csv(cov, file=file.path(out_dir,paste0(o, '_',type, '_bhm_',spec,'_cov.csv')))
}


################################################################################

### models edu -> expected years of schooling

o<-"gr_eys"

models<-list()

# tp burke

r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### adding extremes, control by mean t 

# wd 

r <- "WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR + 
WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR + 
WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# wd lags
r <- "TM + TM_2 + RR + RR_2 +
      l(WD, 0:8)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# wd lags adap
r <- "TM + TM_2 + RR + RR_2 +
      l(WD, 0:8) + l(lag_log_gni_pc_i_WD, 0:8)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# wd adap

r <- "WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR + 
WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + 
lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "adap")

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "no_adap")



r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + l(exp_edu, 0:8) + l(conflict, 0:8)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars_controls", "no_adap")


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + l(exp_edu, 0:8) + l(conflict, 0:8)"
f <- as.formula(paste( o, "~", paste0("lag_",o, "+"), r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars_autoreg_controls", "no_adap")


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_tp_extr_adap_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_iso.html")))

################################################################################

################################################################################


### models health


o<-"gr_leb"

models<-list()

# tp burke

r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### adding extremes, control by mean t 


# TVAR 

r <- "TVAR + TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      TVAR + TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# TVAR lags
r <- "TM + TM_2 + RR + RR_2 +
      l(TVAR, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# TVAR adap

r <- "TVAR + TVAR_2 + lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      TVAR + TVAR_2 + lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      TVAR + TVAR_2 + 
lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# RX 

r <- "RX + RX_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "no_adap")


# RX lags
r <- "TM + TM_2 + RR + RR_2 +
      l(RX, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# RX adap

r <- "RX + RX_2 + lag_log_gni_pc:RX + lag_log_gni_pc:RX_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 + lag_log_gni_pc:RX + lag_log_gni_pc:RX_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 + 
lag_log_gni_pc:RX + lag_log_gni_pc:RX_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "adap")

# together 

r <- "RX + RX_2 +  TVAR + TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 +  TVAR + TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save_model_bhm(m, pattern, o, "all_vars", "no_adap")


r <- "TM + TM_2 + RR + RR_2 +
      l(RX, 0:6) + l(TVAR, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "RX + RX_2 +  TVAR + TVAR_2 + 
lag_log_gni_pc:RX + lag_log_gni_pc:RX_2 + 
 lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 + TVAR + TVAR_2+
      lag_log_gni_pc:RX + lag_log_gni_pc:RX_2 + 
 lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 +  TVAR + TVAR_2+
lag_log_gni_pc:RX + lag_log_gni_pc:RX_2 + 
lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2+
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save_model_bhm(m, pattern, o, "all_vars", "adap")


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_tp_extr_adap_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_iso.html")))


################################################################################

### models income
o<-"gr_gnipc"
models<-list()

# tp burke

r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### adding extremes, control by mean t 


r <- "TM + TM_2 + RR + RR_2+
      TVAR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# WD 

r <- "WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR+
      WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# WD lags
r <- "TM + TM_2 + RR + RR_2 +
      l(WD, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# WD adap

r <- "WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + 
lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# SPI 

r <- "SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# SPI lags
r <- "TM + TM_2 + RR + RR_2 +
      l(SPI, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# SPI adap

r <- "SPI + SPI_2 + lag_log_gni_pc:SPI +  lag_log_gni_pc:SPI_2 " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2+   lag_log_gni_pc:SPI+  lag_log_gni_pc:SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2 + 
lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2+
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      SPI + 
lag_log_gni_pc:SPI + 
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# together 

r <- "WD + WD_2 +   SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +  SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "no_adap")


r <- "TM + TM_2 + RR + RR_2 +
      l(WD, 0:6) +  l(SPI, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "WD + WD_2 +   SPI + SPI_2+
lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 + 
lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +  SPI + SPI_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 + 
  lag_log_gni_pc:SPI +lag_log_gni_pc:SPI_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +SPI + SPI_2+
 lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 + 
lag_log_gni_pc:SPI +lag_log_gni_pc:SPI_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +SPI + 
 lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 + 
lag_log_gni_pc:SPI +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
save_model_bhm(m, pattern, o, "all_vars", "adap")


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_tp_extr_adap_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_iso.html")))




################################################################################
### plots 
################################################################################

plot_climate_effect <- function(model, variable_name, output_var, data, income_levels, x_label, y_label) {
  
  # Extract the relevant coefficients and covariance matrix
  variable_names <- c(variable_name, 
                      paste0("lag_log_gni_pc_i_", variable_name), 
                      paste0(variable_name, "_2"), 
                      paste0("lag_log_gni_pc_i_", variable_name, "_2"))
  
  coefficients <- coef(model)[variable_names]
  cov_matrix <- vcov(model)[variable_names, variable_names]
  
  # Generate a sequence of the variable's values
  values <- seq(min(data[[variable_name]], na.rm = TRUE), max(data[[variable_name]], na.rm = TRUE), length.out = 1000)
  
  # Create an empty data frame to store results
  damages <- data.frame(values = values)
  
  # Loop over income levels to calculate predictions and confidence intervals
  for (i in seq_along(income_levels)) {
    # Create the matrix for the independent variables
    X <- cbind(values,                        # Variable
               values * income_levels[i],     # Variable * income level
               values^2,                      # Variable^2
               (values^2) * income_levels[i]) # Variable^2 * income level
    
    colnames(X) <- variable_names
    
    # Calculate predicted changes and standard errors
    predicted_change <- X %*% coefficients
    standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))
    
    # Calculate confidence intervals
    lower_ci <- predicted_change - 1.96 * standard_errors
    upper_ci <- predicted_change + 1.96 * standard_errors
    
    # Store results in the data frame
    damages[[paste0("predicted_change_", i)]] <- predicted_change
    damages[[paste0("lower_ci_", i)]] <- lower_ci
    damages[[paste0("upper_ci_", i)]] <- upper_ci
  }
  
  # Create the plot
  g <- ggplot(damages) +
    geom_line(aes(x = values, y = predicted_change_1), color = "blue") +
    geom_ribbon(aes(x = values, ymin = lower_ci_1, ymax = upper_ci_1, fill = 'Low \n(around 2898$)'), alpha = 0.2) +
    geom_line(aes(x = values, y = predicted_change_2), color = "green") +
    geom_ribbon(aes(x = values, ymin = lower_ci_2, ymax = upper_ci_2, fill = 'Median \n(around 8010$)'), alpha = 0.2) +
    geom_line(aes(x = values, y = predicted_change_3), color = "red") +
    geom_ribbon(aes(x = values, ymin = lower_ci_3, ymax = upper_ci_3, fill = 'High \n(around 18434$)'), alpha = 0.2) +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    xlab(x_label) +
    ylab(y_label) +
    scale_fill_brewer(name = "Income Level \n Per Capita", palette = "Set1")
  
  return(g)
}



plot_climate_effect_no_adap <- function(model, variable_name, output_var, data, x_label, y_label) {
  
  # Extract the relevant coefficients and covariance matrix
  variable_names <- c(variable_name, 
                      paste0(variable_name, "_2")
                      )
  
  coefficients <- coef(model)[variable_names]
  cov_matrix <- vcov(model)[variable_names, variable_names]
  
  # Generate a sequence of the variable's values
  values <- seq(min(data[[variable_name]], na.rm = TRUE), max(data[[variable_name]], na.rm = TRUE), length.out = 1000)
  
  # Create an empty data frame to store results
  damages <- data.frame(values = values)
  
  # Loop over income levels to calculate predictions and confidence intervals
  for (i in seq_along(income_levels)) {
    # Create the matrix for the independent variables
    X <- cbind(values,                        # Variable
               
               values^2                      # Variable^2
               ) 
    
    colnames(X) <- variable_names
    
    # Calculate predicted changes and standard errors
    predicted_change <- X %*% coefficients
    standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))
    
    # Calculate confidence intervals
    lower_ci <- predicted_change - 1.96 * standard_errors
    upper_ci <- predicted_change + 1.96 * standard_errors
    
    # Store results in the data frame
    damages[[paste0("predicted_change_", i)]] <- predicted_change
    damages[[paste0("lower_ci_", i)]] <- lower_ci
    damages[[paste0("upper_ci_", i)]] <- upper_ci
  }
  
  # Create the plot
  g <- ggplot(damages) +
    geom_line(aes(x = values, y = predicted_change), color = "blue") +
    geom_ribbon(aes(x = values, ymin = lower_ci, ymax = upper_ci), alpha = 0.2) +
    theme_bw() +
    theme(text = element_text(size = 14)) +
    xlab(x_label) +
    ylab(y_label) 
  
  return(g)
}


# Example usage of the function
income_levels <- as.numeric(c(summary(data$lag_log_gni_pc, na.rm = TRUE)[2], 
                              summary(data$lag_log_gni_pc, na.rm = TRUE)[3], 
                              summary(data$lag_log_gni_pc, na.rm = TRUE)[5]))

###############################################################################

# here we keep spi 2 just for plot sake, model works with linear 
o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <-"TM + TM_2 + RR + RR_2+
      WD + WD_2 +SPI + SPI_2+
 lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2 + 
lag_log_gni_pc_i_SPI +lag_log_gni_pc_i_SPI_2 +
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# coef plot 
coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on Income Growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


# 


g1 <- plot_climate_effect(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Temperature (°C)", 
  y_label = "LGNIPC change (%)"
)


g2 <- plot_climate_effect(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Wet Days (number)", 
  y_label = "LGNIPC change (%)"
)


g3 <- plot_climate_effect(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Rainfall (mm)", 
  y_label = "LGNIPC change (%)"
)


g4 <- plot_climate_effect(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Drought", 
  y_label = "LGNIPC change (%)"
)

ggpubr::ggarrange(g1,g3,g2,g4, nrow=2, ncol=2, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")), 
                   width = 2000, height = 1600)

### no adap

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +  SPI + SPI_2"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

g1 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  x_label = "Temperature (°C)", 
  y_label = "LGNIPC change (%)"
)


g2 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  x_label = "Wet Days (number)", 
  y_label = "LGNIPC change (%)"
)


g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  x_label = "Rainfall (mm)", 
  y_label = "LGNIPC change (%)"
)


g4 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  x_label = "Drought", 
  y_label = "LGNIPC change (%)"
)

ggpubr::ggarrange(g1,g3,g2,g4, nrow=2, ncol=2, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 1600, height = 1600)

########################################################

o<-"gr_leb"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 +
lag_log_gni_pc_i_RX + lag_log_gni_pc_i_RX_2 + 
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2"


f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on LIFEX growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)

#


g1 <- plot_climate_effect(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Mean Temperature", 
  y_label = "LIFEX change (%)"
)

g2 <- plot_climate_effect(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Rainfall", 
  y_label = "LIFEX change (%)"
)

g3 <- plot_climate_effect(
  model = m, 
  variable_name = "RX", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Max Cumulative 5 Days Rain", 
  y_label = "LIFEX change (%)"
)




ggpubr::ggarrange(g1,g2,g3, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")),
                   width = 2400, height = 800)

### no adap 

r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 "
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)


g1 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  x_label = "Mean Temperature", 
  y_label = "LIFEX change (%)"
)

g2 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  x_label = "Rainfall", 
  y_label = "LIFEX change (%)"
)

g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RX", 
  output_var = o, 
  data = data, 
  x_label = "Max Cumulative 5 Days Rain", 
  y_label = "LIFEX change (%)"
)




ggpubr::ggarrange(g1, g2,g3, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 2400, height = 800)


########################################################



o<-"gr_eys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + 
lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2 +
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on EYS growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###

g1 <- plot_climate_effect(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Mean Temperature", 
  y_label = "EYS change (%)"
)

g2 <- plot_climate_effect(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Rainfall", 
  y_label = "EYS change (%)"
)


g3 <- plot_climate_effect(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Wet Days (number)", 
  y_label = "EYS change (%)"
)


ggpubr::ggarrange(g1,g2, g3,  nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")), 
                   width = 2400, height = 1000)

### no adap

r <- "TM + TM_2 + RR + RR_2 +
      WD + WD_2"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)




g1 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  x_label = "Mean Temperature", 
  y_label = "EYS change (%)"
)

g2 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  x_label = "Rainfall", 
  y_label = "EYS change (%)"
)


g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  x_label = "Wet Days (number)", 
  y_label = "EYS change (%)"
)

ggpubr::ggarrange(g1,g2, g3, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 2400,height = 800)


###############################################################################
###############################################################################
###############################################################################

### plotting only significant component, using others as controls


out_dir<-file.path(out_dir, "plotting_only_relevant")
if(!dir.exists(out_dir)){dir.create(out_dir)}

o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <-"TM + TM_2 + RR + RR_2+
      WD + WD_2 +SPI + SPI_2+
 lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2 + 
lag_log_gni_pc_i_SPI +lag_log_gni_pc_i_SPI_2 +
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# coef plot 
coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on Income Growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


# 


g1 <- plot_climate_effect(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Temperature (°C)", 
  y_label = "LGNIPC change (%)"
)


g2 <- plot_climate_effect(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Wet Days (number)", 
  y_label = "LGNIPC change (%)"
)



g4 <- plot_climate_effect(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Drought", 
  y_label = "LGNIPC change (%)"
)

ggpubr::ggarrange(g1,g2,g4, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")), 
                   width = 1600, height = 800)

### no adap

r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 +  SPI + SPI_2"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

g1 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "TM", 
  output_var = o, 
  data = data, 
  x_label = "Temperature (°C)", 
  y_label = "LGNIPC change (%)"
)


g2 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  x_label = "Wet Days (number)", 
  y_label = "LGNIPC change (%)"
)


g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RR", 
  output_var = o, 
  data = data, 
  x_label = "Rainfall (mm)", 
  y_label = "LGNIPC change (%)"
)


g4 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  x_label = "Drought", 
  y_label = "LGNIPC change (%)"
)

ggpubr::ggarrange(g1,g3,g2,g4, nrow=2, ncol=2, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 1600, height = 1600)

########################################################

o<-"gr_leb"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 +
lag_log_gni_pc_i_RX + lag_log_gni_pc_i_RX_2 + 
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2"


f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on LIFEX growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)

#



g3 <- plot_climate_effect(
  model = m, 
  variable_name = "RX", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Max Cumulative 5 Days Rain", 
  y_label = "LIFEX change (%)"
)




ggpubr::ggarrange(g3, nrow=1, ncol=1, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")),
                   width = 800, height = 800)

### no adap 

r <- "TM + TM_2 + RR + RR_2+
      RX + RX_2 "
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)


g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "RX", 
  output_var = o, 
  data = data, 
  x_label = "Max Cumulative 5 Days Rain", 
  y_label = "LIFEX change (%)"
)


ggpubr::ggarrange(g3, nrow=1, ncol=1, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 800, height = 800)


########################################################



o<-"gr_eys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      WD + WD_2 + 
lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2 +
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on EYS growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###



g3 <- plot_climate_effect(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Wet Days (number)", 
  y_label = "EYS change (%)"
)

ggpubr::ggarrange( g3, nrow=1, ncol=1, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")), 
                   width = 800,height = 800)

### no adap

r <- "TM + TM_2 + RR + RR_2 +
      WD + WD_2"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)




g3 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "WD", 
  output_var = o, 
  data = data, 
  x_label = "Wet Days (number)", 
  y_label = "EYS change (%)"
)

ggpubr::ggarrange( g3, nrow=1, ncol=1, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 800,height = 800)



###############################################################################
###############################################################################
###############################################################################

### DISCARDED

### models edu -> mean years of schooling

o<-"gr_mys"

models<-list()

# tp burke

r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### adding extremes, control by mean t 

# PEXT 

r <- "PEXT + PEXT_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      PEXT + PEXT_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# PEXT lags
r <- "TM + TM_2 + RR + RR_2 +
      l(PEXT, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# PEXT adap

r <- "PEXT + PEXT_2 + lag_log_gni_pc:PEXT + lag_log_gni_pc:PEXT_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      PEXT + PEXT_2 + lag_log_gni_pc:PEXT + lag_log_gni_pc:PEXT_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      PEXT + PEXT_2 + 
lag_log_gni_pc:PEXT + lag_log_gni_pc:PEXT_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# SPI 

r <- "SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR +
      SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# SPI lags
r <- "TM + TM_2 + RR + RR_2 +
      l(SPI, 0:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# SPI adap

r <- "SPI + SPI_2 + lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2 + lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



r <- "diff_TM + diff_TM:TM + diff_RR + RR:diff_RR +
      SPI + SPI_2 + lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2 + 
lag_log_gni_pc:SPI + lag_log_gni_pc:SPI_2 +
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2+
lag_log_gni_pc:RR + lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_tp_extr_adap_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_tp_extr_adap_models_iso.html")))

######################################################
o<-"gr_mys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      SPI + SPI_2 + PEXT + PEXT_2 + 
lag_log_gni_pc_i_SPI + lag_log_gni_pc_i_SPI_2 +
lag_log_gni_pc_i_PEXT + lag_log_gni_pc_i_PEXT_2 +
lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2+
lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = "Shocks on EYS growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###



g1 <- plot_climate_effect(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Drought", 
  y_label = "MYS change (%)"
)


g2 <- plot_climate_effect(
  model = m, 
  variable_name = "PEXT", 
  output_var = o, 
  data = data, 
  income_levels = income_levels, 
  x_label = "Extremely Rainy Days (mm)", 
  y_label = "MYS change (%)"
)


ggpubr::ggarrange(g1,g2, nrow=1, ncol=2, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_adap_extremes_by_income_effects.png")), 
                   width = 1600, height = 800)

### no adap

r <- "TM + TM_2 + RR + RR_2 +
      SPI + SPI_2 + PEXT + PEXT_2"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)



g1 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "SPI", 
  output_var = o, 
  data = data, 
  x_label = "Drought", 
  y_label = "MYS change (%)"
)


g2 <- plot_climate_effect_no_adap(
  model = m, 
  variable_name = "PEXT", 
  output_var = o, 
  data = data, 
  x_label = "Extremely Rainy Days (mm)", 
  y_label = "MYS change (%)"
)


ggpubr::ggarrange(g1,g2, nrow=1, ncol=2, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0(o,"_bhm_extremes_effects.png")), 
                   width = 1600, height = 800)

