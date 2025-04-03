
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
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try/adding_controls"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
# data_country <- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")
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
### one at the time extreme variables addition
################################################################################

# mys

o<-"gr_mys"

r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_RX +  RR_i_diff_RX + 
diff_PEXT +  TM_i_diff_PEXT"

models<-list()

# no controls 
f <- as.formula(paste( o, "~", r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control exp
r_cont <-"l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc
r_cont <-"lag_log_gni_pc"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (8 lags)
r_cont <-"l(lgnipc, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg +  control exp 
r_cont <-"l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc +  control exp 
r_cont <-"lag_log_gni_pc + l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (6 years) +  control exp 
r_cont <-"l(lgnipc, 0:8) + l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_controls_models.html")))

################################################################################

# eys

o<-"gr_eys"

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR "
# 
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM+ lag_1_TM_i_diff_TM	+ lag_1_diff_RR + lag_1_RR_i_diff_RR+
# WD + WD_2 + PEXT + PEXT:TM "
# 
# r_cl<-"diff_TM + TM_i_diff_TM  + diff_RR + RR_i_diff_RR +
# PEXT + PEXT:TM "
# 
# r_cl<-"TM + TM_2 + RR + RR_2+
# WD + WD_2  "

# r_cl<-"diff_TM + TM_i_diff_TM  + diff_RR + RR_i_diff_RR +
# WD + WD_2  "

r_cl<-"diff_TM + TM_mean_i_diff_TM  + diff_RR + RR_mean_i_diff_RR +
WD + WD_2  "


# also with previous models, wd and wd_2 retain sign and similar magnitude 

models<-list()

# no controls 
f <- as.formula(paste( o, "~", r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control exp
r_cont <-"l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc
r_cont <-"lag_log_gni_pc"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (8 lags)
r_cont <-"l(lgnipc, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg +  control exp 
r_cont <-"l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc +  control exp 
r_cont <-"lag_log_gni_pc + l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (6 years) +  control exp 
r_cont <-"l(lgnipc, 0:8) + l(conflict, 0:8) + l(exp_edu, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_controls_models.html")))

################################################################################


o<-"gr_leb"

# r_cl<-"TM + TM_2 + RR + RR_2"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR"

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_WD + TM_i_diff_WD "
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_WD + TM_mean_i_diff_WD "
# # similar 

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_SPI + SPI_i_diff_SPI "
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_SPI + SPI_mean_i_diff_SPI "
# # quite diff, include for now: the mean level of spi in a region or the istantaneous one can vary a lot 


# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_TVAR + TVAR_i_diff_TVAR"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_TVAR + TVAR_mean_i_diff_TVAR"
# similar 


# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + HW_i_diff_HW"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + HW_mean_i_diff_HW"
# # similar 

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + TM_i_diff_HW"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + TM_mean_i_diff_HW"
# # similar 

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_RX + TM_i_diff_RX"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_RX + TM_mean_i_diff_RX"
# # # similar 


# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_WD + TM_i_diff_WD +
# diff_SPI + SPI_i_diff_SPI+
# diff_TVAR + TVAR_i_diff_TVAR "

# r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR +  
# diff_WD + TM_mean_i_diff_WD +
# diff_SPI + SPI_mean_i_diff_SPI+
# diff_TVAR + TVAR_mean_i_diff_TVAR +
# diff_HW + HW_mean_i_diff_HW"

r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR +  
diff_WD + TM_mean_i_diff_WD +
diff_TVAR + TVAR_mean_i_diff_TVAR +
diff_HW + HW_mean_i_diff_HW"


models<-list()

# no controls 
f <- as.formula(paste( o, "~", r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control exp
r_cont <-"l(conflict, 0:8) + l(exp_health, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc
r_cont <-"lag_log_gni_pc"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (8 lags)
r_cont <-"l(lgnipc, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg +  control exp 
r_cont <-"l(conflict, 0:8) + l(exp_health, 0:8)"
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc +  control exp 
r_cont <-"lag_log_gni_pc + l(conflict, 0:8) + l(exp_health, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (6 years) +  control exp 
r_cont <-"l(lgnipc, 0:8) + l(conflict, 0:8) + l(exp_health, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_controls_models.html")))


################################################################################


o<-"gr_gnipc"

# r_cl<-"TM + TM_2 + RR + RR_2"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR"
# r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR"

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR+  
# diff_WD + RR_i_diff_WD "
# r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR+
# diff_WD + RR_mean_i_diff_WD "
# similar 

# r_cl<-"TM + TM_2 + RR + RR_2 +  
# diff_SPI + SPI_i_diff_SPI "
# r_cl<-"TM + TM_2 + RR + RR_2+ 
# diff_SPI + SPI_mean_i_diff_SPI "
# discard

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_TVAR + TVAR_i_diff_TVAR"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_TVAR + TVAR_mean_i_diff_TVAR"
# similar 


# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + HW_i_diff_HW"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + HW_mean_i_diff_HW"
# # similar , discard 

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + RR_i_diff_HW"
# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_HW + RR_mean_i_diff_HW"
# # similar , discard 

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_RX + TM_i_diff_RX"
# r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR +  
# diff_RX + TM_mean_i_diff_RX"
# r_cl<-"TM + TM_2 + RR + RR_2 +  
# diff_RX + TM_mean_i_diff_RX"
# discard 


# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_WD + TM_i_diff_WD +
# diff_SPI + SPI_i_diff_SPI+
# diff_TVAR + TVAR_i_diff_TVAR "

# r_cl<-"diff_TM + TM_mean_i_diff_TM + diff_RR + RR_mean_i_diff_RR +  
# diff_WD + TM_mean_i_diff_WD +
# diff_SPI + SPI_mean_i_diff_SPI+
# diff_TVAR + TVAR_mean_i_diff_TVAR +
# diff_HW + HW_mean_i_diff_HW"

r_cl<-"l(diff_TM, 0:1) + l(TM_mean_i_diff_TM,0:1) + diff_RR + RR_mean_i_diff_RR +  
diff_WD + RR_mean_i_diff_WD + diff_PEXT + TM_mean_i_diff_PEXT + 
SPI "

# r_cl<-"diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
# diff_WD + RR_i_diff_WD +
# SPI "



models<-list()

# no controls 
f <- as.formula(paste( o, "~", r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), r_cl, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control exp
r_cont <-"l(conflict, 0:8) + l(trade, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc
r_cont <-"lag_log_gni_pc"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (8 lags)
r_cont <-"l(lgnipc, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# autoreg +  control exp 
r_cont <-"l(conflict, 0:8) + l(trade, 0:8)"
f <- as.formula(paste( o, "~", paste0("lag_",o,"+"), paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc +  control exp 
r_cont <-"lag_log_gni_pc + l(conflict, 0:8) + l(trade, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# control lag_log_gni_pc (6 years) +  control exp 
r_cont <-"l(lgnipc, 0:8) + l(conflict, 0:8) + l(trade, 0:8)"
f <- as.formula(paste( o, "~", paste0(r_cl, "+", r_cont), "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_controls_models.html")))


