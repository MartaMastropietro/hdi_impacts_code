### rationale: tm, rr controls, but discard non significant lags for each variable. add extremes

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
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_country <- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")
all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data, all_controls)
data_country<-left_join(data_country, all_controls)

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

# historical mean vars 
mean_modns=c('mTM','mRR', "mTVAR", "mHW", "mRX", "mPEXT", "mWD", "mSPI", "mSPEI", "mPET")

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


### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

################################################################################
### one at the time extreme variables addition
################################################################################

### models edu -> mean years of schooling age 25+

o<-"gr_mys"


models<-list()

# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_RR + lag_1_RR_i_diff_RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + rx, interacted with rr
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_RX +  RR_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + pext, interacted with tm
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  + 
diff_PEXT +  TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff model +spi
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# 
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_RX +  RR_i_diff_RX + 
diff_PEXT +  TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# all
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_RX +  RR_i_diff_RX + 
diff_PEXT +  TM_i_diff_PEXT + 
SPI+SPI_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_PEXT +  TM_i_diff_PEXT + 
SPI+SPI_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# 
r <- "diff_RX +  RR_i_diff_RX + 
diff_PEXT +  TM_i_diff_PEXT + 
SPI+SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m




modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

################################################################################

### models edu -> mean years of schooling age 25+

o<-"gr_eys"

models<-list()

# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + wd
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
WD +  WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# burke model + wd
r <- "TM + TM_2 + RR + RR_2  + 
WD +  WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + wd
r <- "WD +  WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m





modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

################################################################################

### models health -> life exp birth 

o<-"gr_leb"

models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
 
# diff model
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + HW
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + WD
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + RX
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff model + TVAR
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# rx and wd 
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  + 
diff_RX + TM_i_diff_RX +
diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# hw, tvar -> only tvar
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  + 
diff_HW  +
diff_TVAR + TVAR_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# tvar, wd, rx
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  + 
diff_TVAR + TVAR_i_diff_TVAR + 
diff_RX + TM_i_diff_RX +
diff_WD + TM_i_diff_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# tvar, wd, rx, spi
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_TVAR + TVAR_i_diff_TVAR + 
diff_WD + TM_i_diff_WD + 
diff_RX + TM_i_diff_RX +
diff_SPI + TM_i_diff_SPI  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

r <- "diff_TVAR + TVAR_i_diff_TVAR + 
diff_RX + TM_i_diff_RX +
diff_WD + TM_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )

################################################################################

### models gnipc

o<-"gr_gnipc"

models<-list()

# burke model
r <- "TM + TM_2 + RR + RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# diff model -> keep this, as we use this for all others, for easier comparison
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM + lag_1_TM_i_diff_TM   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + spi
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
     lag_1_diff_TM + lag_1_TM_i_diff_TM + 
diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# diff + wd
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
    lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      WD +WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
     lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      diff_WD + RR_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + pext
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
  lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      PEXT + TM_i_PEXT "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR  +
  lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      diff_PEXT + TM_i_diff_PEXT"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# diff + tvar (discard)
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
  lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      TVAR + RR_i_TVAR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
  lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      diff_TVAR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# diff + pext, wd, spi

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
  lag_1_diff_TM + lag_1_TM_i_diff_TM + 
      diff_PEXT + TM_i_diff_PEXT + 
diff_WD + RR_i_diff_WD +
diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

# pext, wd, spi

r <- "diff_PEXT + TM_i_diff_PEXT + 
diff_WD + RR_i_diff_WD +
diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_complex_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0(o,"_complex_models_iso.html")))

# tab <- modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"),  output = "gt" ) %>%
#   tab_spanner(label = o) 
# gt::gtsave(tab, filename =file.path(out_dir, paste0(o,"_complex_models.html")) )


################################################################################
### save adap models

adap_models<-list()

o<-"gr_mys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      diff_RX +  RR_i_diff_RX +
diff_PEXT +  TM_i_diff_PEXT + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR +
lag_log_gni_pc_i_diff_PEXT +lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

o<-"gr_mys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      diff_RX + 
diff_PEXT +  
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR +
lag_log_gni_pc_i_diff_PEXT +lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m



o<-"gr_eys"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
WD + WD_2 + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_WD +lag_log_gni_pc_i_WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m




o<-"gr_leb"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_WD + TM_i_diff_WD + 
diff_TVAR + TVAR_i_diff_TVAR + 
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m



o<-"gr_leb"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
diff_WD +
diff_TVAR +
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m


o<-"gr_gnipc"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + 
diff_WD + RR_i_diff_WD + 
diff_PEXT + TM_i_diff_PEXT + 
diff_SPI+
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

o<-"gr_gnipc"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +
      lag_1_diff_TM + lag_1_TM_i_diff_TM + 
diff_WD + 
diff_PEXT +
diff_SPI+
lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + 
lag_log_gni_pc_i_diff_WD +lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
adap_models[[length(adap_models)+1]]<-m

names(adap_models)<-c("gr_mys","gr_mys",  "gr_eys", "gr_leb", "gr_leb" ,"gr_gnipc", "gr_gnipc")

modelsummary(adap_models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0("_complex_adap_models.html")))
modelsummary(adap_models, vcov=lapply(adap_models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0("_complex_adap_models_dk.html")))
modelsummary(adap_models, vcov=lapply(adap_models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir,  paste0("_complex_adap_models_iso.html")))
