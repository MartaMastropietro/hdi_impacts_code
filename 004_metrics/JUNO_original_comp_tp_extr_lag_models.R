
rm(list=ls())


### function for performing panel crossvalidation: 

cross_validation_fixest_1year = function (data_dem, f, y){
  data_train<-data_dem[ data_dem$year <y, ]
  data_test<-data_dem[ data_dem$year ==y, ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  return (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
}


cross_validation_fixest_3year = function (data_dem, f, y){
  
  # data_train<-data_dem[ data_dem$year <y, ]
  # data_test<-data_dem[ data_dem$year ==y, ]
  # m_cross_val <- fixest::feols(f, data_train)
  # pred<-predict(m_cross_val, data_test)
  # e1= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  # 
  # data_train<-data_dem[ data_dem$year <(y-1), ]
  # data_test<-data_dem[ data_dem$year >=(y-1), ]
  # m_cross_val <- fixest::feols(f, data_train)
  # pred<-predict(m_cross_val, data_test)
  # e2= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
  data_train<-data_dem[ data_dem$year != (y-2), ]
  data_test<-data_dem[ data_dem$year ==(y-2), ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e3= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
  r<-e3
  return(r)
}

cross_validation_fixest = function (data_dem, f, y, nmax){
  
  e<-list()
  years<-unique(data_dem$year)
  for (y in (nmax+1):length(years)){
    data_train<-data_dem[ data_dem$year !=years[y], ]
    data_test<-data_dem[ data_dem$year ==years[y], ]
    m_cross_val <- fixest::feols(f, data_train)
    pred<-predict(m_cross_val, data_test)
    e[[length(e)+1]]<-sqrt(mean((data_test[[o]]-pred)^2, na.rm=TRUE))
    
    
  }
  err= sqrt(mean((unlist(e)^2), na.rm=TRUE))
  
  return(err)
}







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

# parallelize 
cores=detectCores()

#cl <- makeCluster(cores[1]-1) # locale 
cl <- makeCluster(cores[1]) # zeus

#registerDoParallel(cl)


### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

# output dir
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics/components_tp_extr_lag_models_pop_weight_performances"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# add lag variables



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


for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 


for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[modns[i]]
} 


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 1:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
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

################################################################################

### test complete models for each resp variable separately, with/without mean climate controls, in level or diff 
### add interactions when relevant 

args = base::commandArgs(trailingOnly=TRUE)

name_i<-args[2]
#name_i = "fe_quad_iso"
print(name_i)
if(name_i == 1){
  i<-"gdlcode + year"
  name_i<-"fe_simple"
}else if(name_i == 2){
  i<-"gdlcode + iso3[year] + year"
  name_i<-"fe_linear_iso"
}else if(name_i == 3){
  i<-"gdlcode + iso3[year] + iso3[year^2] + year"
  name_i<-"fe_quad_iso"
}else if(name_i == 4){
  i<-"gdlcode + gdlcode[year] +gdlcode[year^2] + year"
  name_i<-"fe_quad_gdl"
}

print(i)

o<-args[3]
print(o)
if (o==1){
  o<-"gr_mys"
}else if(o==2){
  o<-"gr_eys"
}else if(o==3){
  o<-"gr_leb"
}else if(o==4){
  o<-"gr_gnipc"}

#o<-"diff_edu_index"
print(o)


autoreg<-args[4]
#autoreg<-0

print(autoreg)

################################################################################


### test inclusion of extremes oatt, all lags tested, cv save
# save metrics 
# adaptation? 
# autoreg?

extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

# max number of lags tested for each var 
n_matrix<-expand.grid(1:9, 1:9, 1:9)
names(n_matrix)<-c("N_temp", "N_rain", "N")


if (autoreg==0){
  
  metrics <-list()
  
  for (ext in extr_variables){
    
    
    # important: if no lags, we can use all dataset for training 
    nmax=0
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"burke_extr"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2") , "+", paste0("TM_i_", ext) )
    type<-"level"
    r_name<-"burke_extr_int_TM"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2") , "+", paste0("RR_i_", ext) )
    type<-"level"
    r_name<-"burke_extr_int_RR"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0(ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"extr"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"burke_adap_extr"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
    type<-"level"
    r_name<-"burke_adap_extr_adap"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("TM","_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff_inter_TM"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("RR","_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff_inter_RR"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0(paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
    type<-"diff"
    r_name<-"extr"
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###### lags #######
    
    v_temp<-which(modns=="TM")
    v_rain<-which(modns=="RR")
    v<-which(modns==ext)
    
    ###
    ##registerDoParallel(cl)
    
    type<-"lag_level_interact"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",modns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2') )
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    ##stopCluster(cl)
    
    ###
    ##registerDoParallel(cl)
    
    type<-"lag_level"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    ##stopCluster(cl)
    
    ###
    ##registerDoParallel(cl)
    
    type<-"lag_diff_interact_extr"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    ##stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_diff_interact_TM"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_diff_interact_RR"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
  }
  
  save(metrics, file=file.path(out_dir, paste0(name_i, "_",o,"_", "all_extr_all_lags_models_metrics.RData")))
  
} else if(autoreg ==1){ 
  
  metrics <-list()
  
  for (ext in extr_variables){
    ### different kinds of models tested
    models<-list()
    
    # important: if no lags, we can use all dataset for training 
    nmax=0
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"burke_extr"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2") , "+", paste0("TM_i_", ext) )
    type<-"level"
    r_name<-"burke_extr_int_TM"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+",r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2") , "+", paste0("RR_i_", ext) )
    type<-"level"
    r_name<-"burke_extr_int_RR"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0(ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"extr"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+",r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <-paste0("TM + TM_2 + RR + RR_2 + lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"))
    type<-"level"
    r_name<-"burke_adap_extr"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <-paste0("TM + TM_2 + RR + RR_2 +lag_log_gni_pc_i_TM + lag_log_gni_pc_i_TM_2 + lag_log_gni_pc_i_RR + lag_log_gni_pc_i_RR_2 + ", ext, " + ", paste0(ext,"_2"), "+",paste0( "lag_log_gni_pc_i_",ext," + "), paste0( "lag_log_gni_pc_i_",ext,"_2"))
    type<-"level"
    r_name<-"burke_adap_extr_adap"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+",r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+",r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("TM","_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff_inter_TM"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("RR","_i_","diff_", ext ))
    type<-"diff"
    r_name<-"diff_inter_RR"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###
    r <- paste0(paste0("diff_",ext), " + ", paste0(ext,"_i_","diff_", ext ))
    type<-"diff"
    r_name<-"extr"
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[ext]][[type]][[r_name]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[ext]][[type]][[r_name]][["loo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
    metrics[[ext]][[type]][[r_name]][["AIC"]]<-AIC(m)
    metrics[[ext]][[type]][[r_name]][["BIC"]]<-BIC(m)
    metrics[[ext]][[type]][[r_name]][["wr2"]]<-fixest::r2(m,type='wr2')
    # metrics[[ext]][[type]][[r_name]][["model"]]<-m
    
    ###### lags #######
    
    v_temp<-which(modns=="TM")
    v_rain<-which(modns=="RR")
    v<-which(modns==ext)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_level_interact"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",modns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_level"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_diff_interact_extr"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+", paste0("lag_", o), "+",r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_diff_interact_TM"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
    ###
    #registerDoParallel(cl)
    
    type<-"lag_diff_interact_RR"
    res<-foreach (n = 1:nrow(n_matrix)
    ) %dopar% {
      
      N_temp<-n_matrix$N_temp[n]
      N_rain<- n_matrix$N_rain[n]
      N<-n_matrix$N[n]
      nmax<-max(N_temp, N_rain, N)
      
      r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
               paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
               paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
               paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
               paste0("lag_",1:N,"_",varns[v], collapse = "+"),"+", 
               paste0("lag_",1:N,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
               sep='')
      
      f= as.formula(paste( o, "~", paste0("lag_", o), "+", r, "|" ,i ))
      m = fixest::feols(f, data , panel.id = pan_id)
      
      data_dem <- fixest::demean(as.formula(paste0(o , "+",paste0("lag_", o), "+", r , "~", i )), data = data, na.rm=FALSE)
      f <- as.formula(paste( o, "~", paste0("lag_", o), "+", r ,"-1" ))
      data_dem$year<-data$year
      
      
      list("l1yo_cv"=cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020),
           "l3yo_cv"=cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020),
           "loo_cv"=cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax),
           "AIC"=AIC(m),
           "BIC"=BIC(m) ,
           "wr2"=fixest::r2(m,type='wr2'))
      #,
      # "model"=m)
      
    }
    for (n in 1:nrow(n_matrix)){
      res_name<-paste0("TM_",n_matrix$N_temp[n], "_","RR_", n_matrix$N_rain[n],"_",ext,"_", n_matrix$N[n])
      names(res)[[n]]<-res_name
    }
    metrics[[ext]][[type]]<-res
    #stopCluster(cl)
    
  }
  
  save(metrics, file=file.path(out_dir,paste0( name_i, "_",o,"_", "all_extr_all_lags_models_ar_metrics.RData")))
}

