
rm(list=ls())
source("utils/libraries.R")

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
out_dir<-"output/models/original_components_tp_extr_complex_models_try/area_weighting"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_1990_2020_less_na.csv")
data_country <- read_csv("output/data_hdi_original_comp_climate_country_1990_2020_less_na.csv")


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

# country data

for (i in 1:length(varns)){
  data_country[paste(modns[i],'_i_',varns[i],sep='')] <- data_country[modns[i]] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste(adap[i],'_i_',varns[i],sep='')] <- data_country[adap[i]] * data_country[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data_country[paste(adap[i],'_i_',modns[i],sep='')] <- data_country[adap[i]] * data_country[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',varns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',varns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("TM",'_i_',modns[i],sep='')] <- data_country["TM"] * data_country[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data_country[paste("RR",'_i_',modns[i],sep='')] <- data_country["RR"] * data_country[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data_country[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data_country)
    data_country[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data_country)
    
  }
}




################################################################################

### lets try local projections impulse response 



################################################################################
################################################################################
################################################################################
### one at the time extreme variables addition


### models edu -> mean years of schooling age 25+

o<-"gr_mys"


models<-list()

# tp burke
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "iso3 + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data_country, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



################################################################################

### adding extremes 

# extremes 

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2 + diff_RX + diff_RX:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +
     diff_PEXT + TM_i_diff_PEXT + diff_RX + RR_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR+
     PEXT + TM_i_PEXT + RX + RR_i_RX"
f <-as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR+
     PEXT + TM_i_PEXT + RX + RR_i_RX"
f <-as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "TM + TM_2 + RR + RR_2 +
     PEXT + TM_i_PEXT + RX + RR_i_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +
      diff_RX + diff_RX:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
       diff_RX + RR_i_diff_RX + diff_PEXT + TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
       diff_RX + RR_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      WD + WD_2 + RX + RX_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5)+
       diff_RX + RR_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1)  + l(RR_i_diff_WD, 1)+
       diff_RX + RR_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1)  + l(RR_i_diff_WD, 1)+
       diff_RX + RR_i_diff_RX +l(diff_RX, 1:5)  + l(RR_i_diff_RX, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5)+
       diff_RX + RR_i_diff_RX +l(diff_RX, 1:5)  + l(RR_i_diff_RX, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_iso.html")))


################################################################################

### models edu -> expected years of schooling

o<-"gr_eys"


models<-list()

# tp burke
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM  + diff_RR   + TM_i_diff_TM + RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "iso3 + year + iso3[year] +iso3[year^2]"
r <- "TM +  RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data_country, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



################################################################################

### adding extremes 

# extremes 

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + RR +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
        diff_RX + 
     RR_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
        diff_WD + 
     TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +  l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + 
      l(diff_TM, 1:5)  + l(TM_i_diff_TM, 1:5)+
      l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1)  + l(RR_i_diff_WD, 1)+
       diff_RX + RR_i_diff_RX +l(diff_RX, 1:5)  + l(RR_i_diff_RX, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR  + l(diff_RR, 1:5)  + l(RR_i_diff_RR, 1:5)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:5)  + l(RR_i_diff_WD, 1:5)+
       diff_RX + RR_i_diff_RX +l(diff_RX, 1:5)  + l(RR_i_diff_RX, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_iso.html")))


################################################################################


### models health
o<-"gr_leb"
models<-list()


# tp burke
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:2) + l(diff_RR, 1:9) + l(TM_i_diff_TM, 1:2) + l(RR_i_diff_RR, 1:9)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

################################################################################


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
      diff_TVAR  +
      diff_HW +  
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  +TVAR_i_diff_TVAR+
      diff_HW +
      diff_WD + WD_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + 
      diff_TVAR  + 
      diff_HW +  
      diff_WD + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  
      diff_WD + TM_i_diff_WD+
      diff_SPI + TM_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TVAR +TM_i_diff_TVAR +
      diff_HW + TM_i_diff_HW +  
      diff_WD + TM_i_diff_WD  + 
      diff_SPI + SPI_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:2) + l(diff_RR, 1:9) + l(TM_i_diff_TM, 1:2) + l(RR_i_diff_RR, 1:9)+
      diff_TVAR +TM_i_diff_TVAR +
      diff_HW + TM_i_diff_HW +  
      diff_WD + TM_i_diff_WD  + 
      diff_SPI + SPI_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR + TM:diff_TVAR +l(diff_TVAR, 1:3) + 
     diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) + 
     diff_WD + TM_i_diff_WD  + 
     diff_SPI + SPI_i_diff_SPI + l(diff_SPI, 1:5) + l(SPI_i_diff_SPI, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:2) + l(diff_RR, 1:9) + l(TM_i_diff_TM, 1:2) + l(RR_i_diff_RR, 1:9)+
      diff_TVAR + TM:diff_TVAR +l(diff_TVAR, 1:3) + 
     diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) + 
     diff_WD + TM_i_diff_WD  + 
     diff_SPI + SPI_i_diff_SPI + l(diff_SPI, 1:5) + l(SPI_i_diff_SPI, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR +TM_i_diff_TVAR +
      diff_HW + TM_i_diff_HW +  
      diff_WD + TM_i_diff_WD  + 
      diff_SPI + SPI_i_diff_SPI +
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR +
      lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_TVAR +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_iso.html")))


###

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  + 
      diff_HW +  
      diff_WD + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")




i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + lag_1_diff_TM +lag_1_TM_i_diff_TM +lag_1_diff_RR +lag_1_RR_i_diff_RR +
      diff_TVAR  + 
      diff_HW +  
      diff_WD + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + lag_1_diff_TM +lag_1_TM_i_diff_TM +lag_1_diff_RR +lag_1_RR_i_diff_RR +
      TVAR  + 
      HW +  
      WD + 
      SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")




################################################################################


### models income
o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + lag_1_diff_TM +lag_1_TM_i_diff_TM +lag_1_diff_RR  +
      diff_TVAR  + 
      diff_HW +  
      diff_WD + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models<-list()

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR+
      diff_TVAR  + TM_i_diff_TVAR+
      diff_WD + RR_i_diff_WD+
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models<-list()

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR+
      TVAR  + TVAR_2+
      WD + WD_2 + 
      SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + lag_1_diff_TM +lag_1_TM_i_diff_TM +lag_1_diff_RR  +
      WD + WD_2 +
      SPI + SPI_2 +
      HW + HW_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models<-list()


# tp burke
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# lags 4, 3 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

###################################
### extremes 

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_TVAR  +
      diff_HW + 
      diff_PEXT + 
      diff_WD + 
      diff_SPI + TM_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) + 
      diff_TVAR + TM_i_diff_TVAR +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + TM_i_diff_PEXT +
      diff_WD + TM_i_diff_WD  +
      diff_SPI + TM_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + TM_i_diff_PEXT +
      diff_WD + TM_i_diff_WD  +
      diff_SPI + TM_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + RR_i_diff_PEXT +
      diff_WD + RR_i_diff_WD  +
      diff_SPI + TM_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_TVAR + l(diff_TVAR, 1:5) + TM_i_diff_TVAR + l(TM_i_diff_TVAR, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + RR_i_diff_PEXT + l(diff_PEXT, 1:4) + l(RR_i_diff_PEXT, 1:4) +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6) + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_TVAR + l(diff_TVAR, 1:5) + TM_i_diff_TVAR + l(TM_i_diff_TVAR, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + RR_i_diff_PEXT + l(diff_PEXT, 1:4) + l(RR_i_diff_PEXT, 1:4) +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6) + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + RR_i_diff_PEXT +
      diff_WD + RR_i_diff_WD  +
      diff_SPI + TM_i_diff_SPI + 
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR +
      lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_PEXT +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_models_iso.html")))


###############################################################################

models<-list()


o<-"gr_gnipc"


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
  diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
  diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m





o<-"gr_leb"



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


o<-"gr_mys"


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

###

o<-"gr_eys"


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

names(models)<-c(rep("gr_grnipc", 3), rep("gr_leb", 3), rep("gr_mys", 3), rep("gr_eys", 3))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_comp_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  "all_comp_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  "all_comp_models_iso.html"))


################################################################################
################################################################################
################################################################################

### all 

models<-list()


o<-"gr_gnipc"


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
  diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI+ 
  lag_log_gni_pc_i_TM +lag_log_gni_pc_i_TM_2 +
 lag_log_gni_pc_i_RR +lag_log_gni_pc_i_RR_2 +
  lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

jtools::plot_coefs(m)

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 +
  diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI+ 
  lag_log_gni_pc_i_TM +lag_log_gni_pc_i_TM_2 +
  lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


o<-"gr_leb"


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI+ 
      lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR+
      lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI+lag_log_gni_pc_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM +  TM_i_diff_TM + 
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI+ 
      lag_log_gni_pc_i_diff_TM +
      lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



o<-"gr_mys"



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR+
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + 
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



o<-"gr_eys"



i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR+
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + 
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


names(models)<-c(rep("gr_gnipc", 2), rep("gr_leb", 2), rep("gr_mys", 2) ,rep("gr_eys", 2))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models_iso.html"))




###############################################################################


o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+
  diff_HW + TM_i_diff_HW + 
  diff_WD + RR_i_diff_WD  +
  diff_SPI+ 
  lag_log_gni_pc_i_TM +lag_log_gni_pc_i_TM_2 +
 lag_log_gni_pc_i_RR +lag_log_gni_pc_i_RR_2 +
  lag_log_gni_pc_i_diff_HW +lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
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
    title = "Shocks on Income Growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###

o<-"gr_leb"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM +  TM_i_diff_TM + 
      diff_TVAR  + TM_i_diff_TVAR+
      diff_HW +  TM_i_diff_HW+
      diff_WD + TM_i_diff_WD+
      diff_SPI + SPI_i_diff_SPI+ 
      lag_log_gni_pc_i_diff_TM +
      lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_SPI"
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


###


o<-"gr_mys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR+
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
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
    title = "Shocks on MYS growth")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###


o<-"gr_eys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2+
lag_log_gni_pc_i_diff_TM +lag_log_gni_pc_i_diff_RR+
      lag_log_gni_pc_i_WD+ lag_log_gni_pc_i_WD_2 "
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


###############################################################################



