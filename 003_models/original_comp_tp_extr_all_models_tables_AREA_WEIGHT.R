### component wise analysis 

rm(list=ls())

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
out_dir<-"output/models/original_components_tp_extr_all_models_performances"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_all_models_performances/area_weighting"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_1990_2020_less_na.csv")

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


# n lags 
N_temp<-8
N_rain<-4
N<-8

################################################################################

### models no extremes

### simple growth models: temp, prec


# simplest model edu eys
models<-list()

o <- "gr_eys"

# burke 
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke + adap
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff 
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + adap
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_iso.html")))


# simplest model edu mys
models<-list()

o <- "gr_mys"


# burke 
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke + adap
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff 
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + adap
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_iso.html")))


# simplest model health leb
models<-list()

o <- "gr_leb"


# burke 
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke + adap
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff 
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + adap
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_iso.html")))


# simplest model income gnipc
models<-list()

o <- "gr_gnipc"


# burke 
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke + adap
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff 
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + adap
i <- "gdlcode"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM +lag_1_lag_log_gni_pc_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_simple_growth_tp_iso.html")))

################################################################################

### test inclusion of extremes oatt, save tables 
library(gt)

extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

indeces<-c("gdlcode + year", 
           "gdlcode + iso3[year] + year", 
           "gdlcode + iso3[year] + iso3[year^2] + year", 
           "gdlcode + gdlcode[year] +gdlcode[year^2] + year ")
names(indeces)<-c("fe_simple", 
                  "fe_linear_iso", 
                  "fe_quad_iso" ,
                  "fe_quad_gdl")

out_vars<-c("gr_gnipc", "gr_mys", "gr_eys",  "gr_leb")

indeces_new<-c(
  "gdlcode + iso3[year] + iso3[year^2] + year" 
)

for (o in out_vars){
  for (ext in extr_variables){
    for (i in indeces_new){
      
      ### different kinds of models tested
      models<-list()
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2")
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext)
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"), "+", "TM_i_", ext)
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"), "+", "RR_i_", ext)
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <- paste0(ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR " )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR  + ", paste0("diff_",ext) )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0("diff_",ext) ,"+", "TM_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0("diff_",ext) ,"+", "RR_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <- paste0(paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0(ext) )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0(ext), " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0(ext), " + ", paste0(ext,"_2"), "+", "TM_i_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + lag_1_diff_RR + lag_1_RR_i_diff_RR + ", paste0(ext), " + ", paste0(ext,"_2"), "+", "RR_i_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      #  # adap 
      #  
      #  ###
      #  r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  
      #  
      #  ###
      #  r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  
      #  
      #  ###
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR" )
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("TM_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR" )
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("RR_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR" )
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR","+ ", paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("TM","_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR","+ ", paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("RR","_i_","diff_", ext ), 
      #              "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR","+ ", paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 +", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ), " + ", 
      #              paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", paste0("diff_",ext), " + ", paste0("TM","_i_","diff_", ext ), " + ", 
      #              paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      #  
      #  ###
      #  r <- paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 +", paste0("diff_",ext), " + ", paste0("RR","_i_","diff_", ext ), " + ", 
      #              paste0("lag_log_gni_pc","_i_","diff_", ext ))
      #  f <- as.formula(paste( o, "~", r, "|" ,i ))
      #  m <- fixest::feols(f, data, panel.id=pan_id)
      #  models[[length(models)+1]]<-m
      
      
      
      
      tab <- modelsummary(models,estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output = "gt") %>%
        tab_spanner(label = o) 
      gt::gtsave(tab, filename = file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model.html" )))
      
      tab <- modelsummary(models,vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output = "gt") %>%
        tab_spanner(label = o) 
      gt::gtsave(tab, filename = file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_dk.html" )))
      
      tab <- modelsummary(models,vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output = "gt") %>%
        tab_spanner(label = o) 
      gt::gtsave(tab, filename = file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_iso.html" )))
      
      # modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model.html" )))
      # modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_dk.html" )))
      # modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_iso.html" )))
      
      
    }
  }
}


# better aspect
# library(gt)
# tab <- modelsummary(models,estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output = "gt") %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename = file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model.html" )))


