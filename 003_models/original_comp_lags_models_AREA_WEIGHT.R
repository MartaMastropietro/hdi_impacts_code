
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
out_dir<-"output/models/original_components_tp_extr_all_models_performances/lag_models_area_weight"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_original_comp_climate_1990_2020_less_na.csv")

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

# pan_id<-c('gdlcode', 'year')

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



# n lags 
N_temp<-8
N_rain<-8
N<-8

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



################################################################################

### each complete mod with/without control and autoreg 
### model specification with mean controls

spec<-"mean_mod_spec"
pattern<-"conflict|exp_edu|exp_health|trade|lag_gr" # to discard coefficients rows correspondent to controls, autoreg part 

save_model<-function(m, pattern, o, type, spec){
  # save 
  coefs=m$coefficients[!grepl(pattern, names(m$coefficients))]
  r2=r2(m,type='r2')
  ar2=r2(m,type='ar2')
  wr2=r2(m,type='wr2')
  BIC=BIC(m)
  AIC=AIC(m)
  cov=vcov(m)[!grepl(pattern, row.names(vcov(m))), !grepl(pattern, colnames(vcov(m))) ]
  table<-rbind(r2,ar2,wr2,BIC,AIC,coeftable(m)[!grepl(pattern, row.names(coeftable(m))), ] )
  #write.csv(table, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coeftable.csv')) )
  write.csv(coefs, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coef.csv')) )
  write.csv(cov, file.path(out_dir,paste0(o, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_cov.csv')) )
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



################################################################################

o<-"gr_mys"

extr_formula<-paste0(paste0(varns[v_pext]), "+",
                     paste0(m_modns[v_pext], "_i_", varns[v_pext]),"+", 
                     paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_pext],'_i_',varns[v_pext], collapse = "+"),"+",
                     paste0(varns[v_spi]), "+",
                     paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                     paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                     paste0(varns[v_rx]), "+",
                     paste0(m_modns[v_rx], "_i_", varns[v_rx]),"+", 
                     paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_rx],'_i_',varns[v_rx], collapse = "+"), "+",
                     paste0(varns[v_hw]), "+",
                     paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
                     paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_pext],'_i_',varns[v_pext], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_rx],'_i_',varns[v_rx], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"))

# extr_formula<-paste0(paste0(varns[v_spi]), "+",
#                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                      paste0(varns[v_hw]), "+",
#                      paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
# extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                           paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"))
models<-list()

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
PEXT + PEXT_2+
RX + RX_2 +
HW + HW_2 + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_PEXT + PEXT_mean_i_diff_PEXT+
diff_RX + RX_mean_i_diff_RX+
HW + HW_2 + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
HW + HW_2 + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
diff_HW + HW_mean_i_diff_HW + 
diff_SPI +SPI_mean_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


r <- "TM + TM_2 + RR + RR_2 + 
PEXT + PEXT_2+
RX + RX_2 +
HW + HW_2 + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

###


# oaat, moderating with longrun extreme itself 
for (v_ext in c(v_pext, v_rx, v_hw, v_spi)){
  r=paste0(mean_cl_formula,"+", 
           # part of extremes
           paste0(varns[v_ext]), "+",
           paste0(m_modns[v_ext], "_i_", varns[v_ext]),"+", 
           paste0("lag_",1:N,"_",varns[v_ext], collapse = "+"),"+", 
           paste0("lag_",1:N,"_",m_modns[v_ext],'_i_',varns[v_ext], collapse = "+")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
}


### all, moderating with extr
type<-"all_extr"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr, + controls
type<-"all_extr_controls"
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
save_model(m, pattern, o, type, spec)




### all, moderating with extr, + autoreg
type<-"all_extr_autoreg"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr + ADAP
type<-"all_extr_adap"
r=paste0(mean_cl_formula,"+", 
         extr_formula, "+",
         # adap
         mean_cl_adap_formula,"+",
         extr_adap_formula
)

f= as.formula(paste( o, "~", r, "|" ,i ))
m = fixest::feols(f, data , panel.id = pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save 
save_model(m, pattern, o, type, spec)



### check
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


#### save tables

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec , "_lags_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_iso.html")))

######################

o<-"gr_eys"

extr_formula<-paste0(paste0(varns[v_spi]), "+",
                     paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                     paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                     paste0(varns[v_wd]), "+",
                     paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                     paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                     paste0("lag_",1:N,"_",m_modns[v_wd],'_i_',varns[v_wd], collapse = "+"))
                    
extr_adap_formula<-paste0(
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_wd],'_i_',varns[v_wd], collapse = "+"))

# extr_formula<-paste0(paste0(varns[v_spi]), "+",
#                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                      paste0(varns[v_hw]), "+",
#                      paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
#                      paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
#                      paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"))
# extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
#                           paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"))
models<-list()

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + 
WD + WD_2 + 
SPI + SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

###


# oaat, moderating with longrun extreme itself 
for (v_ext in c(v_pext, v_rx, v_hw, v_spi)){
  r=paste0(mean_cl_formula,"+", 
           # part of extremes
           paste0(varns[v_ext]), "+",
           paste0(m_modns[v_ext], "_i_", varns[v_ext]),"+", 
           paste0("lag_",1:N,"_",varns[v_ext], collapse = "+"),"+", 
           paste0("lag_",1:N,"_",m_modns[v_ext],'_i_',varns[v_ext], collapse = "+")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
}


### all, moderating with extr
type<-"all_extr"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr, + controls
type<-"all_extr_controls"
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
save_model(m, pattern, o, type, spec)




### all, moderating with extr, + autoreg
type<-"all_extr_autoreg"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr + ADAP
type<-"all_extr_adap"
r=paste0(mean_cl_formula,"+", 
         extr_formula, "+",
         # adap
         mean_cl_adap_formula,"+",
         extr_adap_formula
)

f= as.formula(paste( o, "~", r, "|" ,i ))
m = fixest::feols(f, data , panel.id = pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save 
save_model(m, pattern, o, type, spec)



### check
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


#### save tables

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec , "_lags_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_iso.html")))

#################

o<-"gr_leb"

extr_formula<-paste0( paste0(varns[v_wd]), "+",
                      paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                      paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"),"+",
                      paste0(varns[v_hw]), "+",
                      paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
                      paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"),"+",
                      paste0(varns[v_spi]), "+",
                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                      paste0(varns[v_tvar]), "+",
                      paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                      paste0("lag_",1:N,"_",varns[v_tvar], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"),"+",
                      paste0(varns[v_rx]), "+",
                      paste0(m_modns[v_rx], "_i_", varns[v_rx]),"+", 
                      paste0("lag_",1:N,"_",varns[v_rx], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_rx],'_i_',varns[v_rx], collapse = "+"))
extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_wd],'_i_',varns[v_wd], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_tvar],'_i_',varns[v_tvar], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_rx],'_i_',varns[v_rx], collapse = "+"))
models<-list()

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
  diff_WD + WD_mean_i_diff_WD +
  diff_RX + RX_mean_i_diff_RX +
  diff_SPI + SPI_mean_i_diff_SPI + 
  diff_TVAR + TVAR_mean_i_diff_TVAR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
  WD + WD_2 + 
  SPI + SPI_2 +
  TVAR + TVAR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

###


# oaat, moderating with longrun extreme itself 
for (v_ext in c(v_pext, v_rx, v_hw, v_spi)){
  r=paste0(mean_cl_formula,"+", 
           # part of extremes
           paste0(varns[v_ext]), "+",
           paste0(m_modns[v_ext], "_i_", varns[v_ext]),"+", 
           paste0("lag_",1:N,"_",varns[v_ext], collapse = "+"),"+", 
           paste0("lag_",1:N,"_",m_modns[v_ext],'_i_',varns[v_ext], collapse = "+")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
}


### all, moderating with extr
type<-"all_extr"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr, + controls
type<-"all_extr_controls"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr, + autoreg
type<-"all_extr_autoreg"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr + ADAP
type<-"all_extr_adap"
r=paste0(mean_cl_formula,"+", 
         extr_formula, "+",
         # adap
         mean_cl_adap_formula,"+",
         extr_adap_formula
)

f= as.formula(paste( o, "~", r, "|" ,i ))
m = fixest::feols(f, data , panel.id = pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save 
save_model(m, pattern, o, type, spec)



### check
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


#### save tables

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec , "_lags_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_iso.html")))

######################


o<-"gr_gnipc"

extr_formula<-paste0( paste0(varns[v_wd]), "+",
                      paste0(m_modns[v_wd], "_i_", varns[v_wd]),"+", 
                      paste0("lag_",1:N,"_",varns[v_wd], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_rain],'_i_',varns[v_wd], collapse = "+"),"+",
                      paste0(varns[v_hw]), "+",
                      paste0(m_modns[v_hw], "_i_", varns[v_hw]),"+", 
                      paste0("lag_",1:N,"_",varns[v_hw], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_hw],'_i_',varns[v_hw], collapse = "+"),"+",
                      paste0(varns[v_spi]), "+",
                      paste0(m_modns[v_spi], "_i_", varns[v_spi]),"+", 
                      paste0("lag_",1:N,"_",varns[v_spi], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                      paste0(varns[v_tvar]), "+",
                      paste0(m_modns[v_tvar], "_i_", varns[v_tvar]),"+", 
                      paste0("lag_",1:N,"_",varns[v_tvar], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_tvar],'_i_',varns[v_tvar], collapse = "+"),"+",
                      paste0(varns[v_pext]), "+",
                      paste0(m_modns[v_pext], "_i_", varns[v_pext]),"+", 
                      paste0("lag_",1:N,"_",varns[v_pext], collapse = "+"),"+", 
                      paste0("lag_",1:N,"_",m_modns[v_pext],'_i_',varns[v_pext], collapse = "+"))
extr_adap_formula<-paste0(paste0("lag_",1:N,"_",adap[v_wd],'_i_',varns[v_wd], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_hw],'_i_',varns[v_hw], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_spi],'_i_',varns[v_spi], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_tvar],'_i_',varns[v_tvar], collapse = "+"),"+",
                          paste0("lag_",1:N,"_",adap[v_pext],'_i_',varns[v_pext], collapse = "+"))
models<-list()

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
diff_PEXT + PEXT_mean_i_diff_PEXT + 
diff_WD + WD_mean_i_diff_WD +
diff_SPI + SPI_mean_i_diff_SPI +
diff_TVAR + TVAR_mean_i_diff_TVAR +
diff_HW + HW_mean_i_diff_HW"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR +  
  PEXT + TM:PEXT +
  WD + WD_2 + 
  SPI + SPI_2 +
  TVAR + TVAR_2 + 
  HW + HW_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

###


# oaat, moderating with longrun extreme itself 
for (v_ext in c(v_pext, v_rx, v_hw, v_spi)){
  r=paste0(mean_cl_formula,"+", 
           # part of extremes
           paste0(varns[v_ext]), "+",
           paste0(m_modns[v_ext], "_i_", varns[v_ext]),"+", 
           paste0("lag_",1:N,"_",varns[v_ext], collapse = "+"),"+", 
           paste0("lag_",1:N,"_",m_modns[v_ext],'_i_',varns[v_ext], collapse = "+")
  )
  
  f= as.formula(paste( o, "~", r, "|" ,i ))
  m = fixest::feols(f, data , panel.id = pan_id)
  models[[length(models)+1]]<-m
}


### all, moderating with extr
type<-"all_extr"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr, + controls
type<-"all_extr_controls"
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
save_model(m, pattern, o, type, spec)

### all, moderating with extr, + autoreg
type<-"all_extr_autoreg"
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
save_model(m, pattern, o, type, spec)


### all, moderating with extr + ADAP
type<-"all_extr_adap"
r=paste0(mean_cl_formula,"+", 
         extr_formula, "+",
         # adap
         mean_cl_adap_formula,"+",
         extr_adap_formula
)

f= as.formula(paste( o, "~", r, "|" ,i ))
m = fixest::feols(f, data , panel.id = pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# save 
save_model(m, pattern, o, type, spec)



### check
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


#### save tables

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec , "_lags_models.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_dk.html")))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, paste0(o,"_", spec ,"_lags_models_iso.html")))

