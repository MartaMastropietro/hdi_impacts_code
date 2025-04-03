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
out_dir<-"output/models/components_tp_extr_all_models_performances"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET", 
        'diff_TM_2','diff_RR_2', "diff_TVAR_2", "diff_HW_2", "diff_RX_2", "diff_PEXT_2", "diff_WD_2", "diff_SPI_2", "diff_SPEI_2", "diff_PET_2")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET", 
        'TM_2','RR_2', "TVAR_2", "HW_2", "RX_2", "PEXT_2", "WD_2", "SPI_2", "SPEI_2", "PET_2")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 
       'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

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

plot(data$diff_income_index, data$diff_edu_index, col=factor(data$iso3))
plot(data$diff_income_index, data$diff_health_index, col=factor(data$iso3))
plot(data$diff_edu_index, data$diff_health_index, col=factor(data$iso3))

plot(data$income_index, data$edu_index, col=factor(data$iso3))
plot(data$income_index, data$health_index, col=factor(data$iso3))
plot(data$edu_index, data$health_index, col=factor(data$iso3))

################################################################################

### models no extremes

### simple growth models: temp, prec


# simplest model edu
models<-list()

o <- "diff_edu_index"
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

# lags

i <- "gdlcode + year "
r <- "TM + RR + TM_i_diff_TM + RR_i_diff_RR + l(TM, 1:8) + l(RR, 1:8) + l(TM_i_diff_TM, 1:8) + l(RR_i_diff_RR, 1:8)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_simple_growth_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_simple_growth_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_simple_growth_tp_iso.html"))


# simplest model health
models<-list()

o <- "diff_health_index"
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


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_simple_growth_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_simple_growth_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_simple_growth_tp_iso.html"))


# simplest model income
models<-list()

o <- "diff_income_index"
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


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_simple_growth_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_simple_growth_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_simple_growth_tp_iso.html"))

################################################################################

### test inclusion of extremes oatt, save tables 

extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

indeces<-c("gdlcode + year", 
           "gdlcode + iso3[year] + year", 
           "gdlcode + iso3[year] + iso3[year^2] + year", 
           "gdlcode + gdlcode[year] +gdlcode[year^2] + year ")
names(indeces)<-c("fe_simple", 
                  "fe_linear_iso", 
                  "fe_quad_iso" ,
                  "fe_quad_gdl")

out_vars<-c("diff_income_index", "diff_edu_index", "diff_health_index",  "diff_hdi")

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
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"),"+", "TM_i_", ext, "+", "RR_i_", ext)
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <- paste0(ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext) ,"+", "TM_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ),"+", "TM_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", "RR_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ),"+", "RR_i_","diff_", ext )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("TM + TM_i_diff_TM + RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <- paste0(paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ), 
                  "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR" )
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ), 
                  "+ lag_log_gni_pc_i_diff_TM","+ lag_log_gni_pc_i_diff_RR","+ ", paste0("lag_log_gni_pc","_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      ###
      r <- paste0(paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ),"+ ", paste0("lag_log_gni_pc","_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      
      
     
      
      modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model.html" )))
      modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_dk.html" )))
      modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,paste0(o, "_",ext,"_", names(indeces[indeces==i]),  "_model_iso.html" )))
      
      
    }
  }
}

library(gt)
tab <- modelsummary(models, output = "gt") %>%
  tab_spanner(label = o) 


################################################################################


### test inclusion of extremes oatt, all lags tested, 1 and 3 years of cv tested
# save metrics 
# adaptation? 
# autoreg?

extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

indeces<-c("gdlcode + year", 
           "gdlcode + iso3[year] + year", 
           "gdlcode + iso3[year] + iso3[year^2] + year", 
           "gdlcode + gdlcode[year] +gdlcode[year^2] + year ")
names(indeces)<-c("fe_simple", 
                  "fe_linear_iso", 
                  "fe_quad_iso" ,
                  "fe_quad_gdl")

out_vars<-c("diff_hdi", "diff_income_index", "diff_edu_index", "diff_health_index",  "gr_log_gni_pc")

metrics <-list()

for (o in out_vars){
  for (ext in extr_variables){
    for (i in indeces){
      
      ### different kinds of models tested
      models<-list()
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <- paste0(ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ### lags a la callahan 
      for (n in 1:N){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        v<-which(modns==ext)
        #construct equation
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
                 paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                 paste0("lag_",1:n,"_",modns[v], collapse = "+"),"+", 
                 paste0("lag_",1:n,"_",modns[v],'_i_',varns[v], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        
      }
      
      ### lags a la kotz
      for ( n in 1:N){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        v<-which(modns==ext)
        #construct equation
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
                 paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                 paste0("lag_",1:n,"_",varns[v], collapse = "+"),"+", 
                 paste0("lag_",1:n,"_",modns[v],'_i_',varns[v], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        
      }
    }
  }
}

save(metrics, file=file.path(out_dir, "all_comp_all_lags_models_metrics.RData"))


### same , with autoregressive comp
metrics <-list()

for (o in out_vars){
  for (ext in extr_variables){
    for (i in indeces){
      
      ### different kinds of models tested
      models<-list()
      
      ###
      r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o, "+") ,  r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o, "+") , r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <- paste0(ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o, "+") ,  r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
      f <- as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o, "+") , r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
      f <- as.formula(paste( o, "~", paste0("lag_", o, "+") , r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o, "+") , r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ###
      r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
      f <- as.formula(paste( o, "~", paste0("lag_", o, "+") , r, "|" ,i ))
      m <- fixest::feols(f, data, panel.id=pan_id)
      models[[length(models)+1]]<-m
      # cross val
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o, "+"), r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r ,"-1" ))
      data_dem$year<-data$year
      metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
      metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
      metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
      metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
      
      
      ### lags a la callahan 
      for (n in 1:N){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        v<-which(modns==ext)
        #construct equation
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
                 paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                 paste0("lag_",1:n,"_",modns[v], collapse = "+"),"+", 
                 paste0("lag_",1:n,"_",modns[v],'_i_',varns[v], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o, "+") ,  r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        
      }
      
      ### lags a la kotz
      for ( n in 1:N){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        v<-which(modns==ext)
        #construct equation
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
                 paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                 paste0("lag_",1:n,"_",varns[v], collapse = "+"),"+", 
                 paste0("lag_",1:n,"_",modns[v],'_i_',varns[v], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~",paste0("lag_", o, "+") ,  r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        models[[length(models)+1]]<-m
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o, "+") ,r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", paste0("lag_", o, "+") , r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        
      }
    }
  }
}


save(metrics, file=file.path(out_dir, "all_comp_all_lags_models_ar_metrics.RData"))
