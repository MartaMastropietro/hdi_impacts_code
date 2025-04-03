### specific lags analysis with also cv 

### for each comp variable
### first test optimal number of lags for t,r alone, then add extremes 
### test with/without t, r controls for each extreme
### on top of that, also add adaptation here 

### component wise analysis 

rm(list=ls())


data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")


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
# library(parallel)
# library(foreach)
# library(doParallel)
library(fixest)
library(modelsummary)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)

# output dir
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models_metrics/components_tp_extr_lag_models_performances"
if(!dir.exists(out_dir)){dir.create(out_dir)}


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



################################################################################

### test complete models for each resp variable separately, with/without mean climate controls, in level or diff 
### add interactions when relevant 

#i<-args[2]
i<-"gdlcode + iso3[year] + iso3[year^2] + year"

#o<-args[3]
o<-"diff_edu_index"


if(o =="diff_edu_index"){
  extremes<-c("TVAR", "PEXT", "WD")
}else if(o =="diff_income_index"){
  extremes<-c("PEXT", "HW", "WD", "SPI")
}else if(o =="diff_health_index"){
  extremes<-c("TVAR", "HW", "WD", "SPI")
}







metrics <- list()
models <- list()


# BURKE
r <- paste0("TM + TM_2 + RR + RR_2 + ", extremes[1], " + ", paste0(extremes[1],"_2"), " + ", paste0("TM_i_", extremes[1]), " + ",
            extremes[2], " + ", paste0(extremes[2],"_2"), " + ", paste0("TM_i_", extremes[2])," + ",
            extremes[3], " + ", paste0(extremes[3],"_2"), " + ", paste0("TM_i_", extremes[3]))
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["level"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["level"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020) metrics[[o]][["level"]][["looyo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=10)
metrics[[o]][["level"]][["looyo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=10)
metrics[[o]][["level"]][["AIC"]]<-AIC(m)
metrics[[o]][["level"]][["BIC"]]<-BIC(m)
metrics[[o]][["level"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["level"]][["model"]]<-m

# DIFF
r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",extremes[1]), " + ", paste0("TM_i_","diff_", extremes[1] ) ,"+", 
            paste0("diff_",extremes[2]), " + ", paste0("TM_i_","diff_", extremes[2] ) ,"+", 
            paste0("diff_",extremes[3]), " + ", paste0("TM_i_","diff_", extremes[3] ) )
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["diff"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["AIC"]]<-AIC(m)
metrics[[o]][["diff"]][["BIC"]]<-BIC(m)
metrics[[o]][["diff"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["diff"]][["model"]]<-m


### lags LEVEL
for (N_temp in 1:8){
  for (N_rain in 1:8){
    for (n1 in 1:8){
      for (n2 in 1:8){
        for (n3 in 1:8){
     
          v_temp<-which(modns=="TM")
          v_rain<-which(modns=="RR")
          v1<-which(modns==extremes[1])
          v2<-which(modns==extremes[2])
          v3<-which(modns==extremes[3])
          
          # no interactions 
          r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",
                   paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+",
                   paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_level_no_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_no_interact"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_no_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_no_interact"]][[id]][["model"]]<-m
          
          
          #interactions, but level focus 
          r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",
                   paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",
                   paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                   paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_level_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_interact"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_interact"]][[id]][["model"]]<-m
          
          
          ### no t, p controls 
          
          # no interactions 
          r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                    paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),
                    sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["model"]]<-m
          
          
          #interactions, but level focus 
          r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                    paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+","+",paste0(modns[v_temp], "_i_", varns[v3]), "+",
                    paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),
                    sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["model"]]<-m
          
        }
      }
    }
  } 
}



### lags DIFF 
for (N_temp in 1:8){
  for (N_rain in 1:8){
    for (n1 in 1:8){
      for (n2 in 1:8){
        for (n3 in 1:8){
          
          v_temp<-which(modns=="TM")
          v_rain<-which(modns=="RR")
          v<-which(modns==extremes)
          
          #construct equation
          r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v1], "+",varns[v2], "+",varns[v3], "+",
                   paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",
                   paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                   paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_diff"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_diff"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_diff"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_diff"]][[id]][["model"]]<-m
          
          ### no controls t,p 
          #construct equation
          r=paste0(varns[v], "+", 
                   paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",
                   paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  )
          metrics[[o]][["lag_diff_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_diff_no_tp"]][[id]][["model"]]<-m
          
          
        }
      }
    }
  }
}


save(metrics, file=file.path(out_dir, paste0(o, "_complete_models_lags_metrics.RData")))


################################################################################

#############
### health index 
#############

o<-"diff_health_index"
extremes<-c("TVAR", "HW", "WD", "SPI")

metrics <- list()
models <- list()


# BURKE
r <- paste0("TM + TM_2 + RR + RR_2 + ", extremes[1], " + ", paste0(extremes[1],"_2"), " + ", paste0("TM_i_", extremes[1]), " + ",
            extremes[2], " + ", paste0(extremes[2],"_2"), " + ", paste0("TM_i_", extremes[2])," + ",
            extremes[3], " + ", paste0(extremes[3],"_2"), " + ", paste0("TM_i_", extremes[3])," + ",
            extremes[4], " + ", paste0(extremes[4],"_2"), " + ", paste0("TM_i_", extremes[4]))
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["level"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["level"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020) 
metrics[[o]][["level"]][["looyo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=10)
metrics[[o]][["level"]][["AIC"]]<-AIC(m)
metrics[[o]][["level"]][["BIC"]]<-BIC(m)
metrics[[o]][["level"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["level"]][["model"]]<-m

# DIFF
r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",extremes[1]), " + ", paste0("TM_i_","diff_", extremes[1] ) ,"+", 
            paste0("diff_",extremes[2]), " + ", paste0("TM_i_","diff_", extremes[2] ) ,"+", 
            paste0("diff_",extremes[3]), " + ", paste0("TM_i_","diff_", extremes[3] ),"+", 
            paste0("diff_",extremes[4]), " + ", paste0("TM_i_","diff_", extremes[4] ) )
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["diff"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["AIC"]]<-AIC(m)
metrics[[o]][["diff"]][["BIC"]]<-BIC(m)
metrics[[o]][["diff"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["diff"]][["model"]]<-m


### lags LEVEL
for (N_temp in 1:8){
  for (N_rain in 1:8){
     for (n1 in 1:8){
       for (n2 in 1:8){
         for (n3 in 1:8){
           for (n4 in 1:8){
          
          v_temp<-which(modns=="TM")
          v_rain<-which(modns=="RR")
          v1<-which(modns==extremes[1])
          v2<-which(modns==extremes[2])
          v3<-which(modns==extremes[3])
          v4<-which(modns==extremes[4])
          
          # no interactions 
          r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",modns[v4], "+",
                   paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+",
                   paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
          metrics[[o]][["lag_level_no_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_no_interact"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_no_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_no_interact"]][[id]][["model"]]<-m
          
          
          #interactions, but level focus 
          r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",modns[v4], "+",
                   paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                   paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                   paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                   paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),"+", 
                   paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
          metrics[[o]][["lag_level_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_interact"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_interact"]][[id]][["model"]]<-m
          
          
          ### no t, p controls 
          
          # no interactions 
          r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                    paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                    paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),
                    sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["model"]]<-m
          
          
          #interactions, but level focus 
          r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                    paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                    paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                    paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+", 
                    paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),"+", 
                    paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                    sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_level_interact_no_tp"]][[id]][["model"]]<-m
          
          }
        }
      }
    } 
  }
}


### lags DIFF 
for (N_temp in 1:8){
  for (N_rain in 1:8){
    for (n1 in 1:8){
      for (n2 in 1:8){
        for (n3 in 1:8){
          for (n4 in 1:8){
            
          v_temp<-which(modns=="TM")
          v_rain<-which(modns=="RR")
          v<-which(modns==extremes)
          
          #construct equation
          r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v1], "+",varns[v2], "+",varns[v3], "+",varns[v4], "+",
                   paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                   paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                   paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                   paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                   paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+",
                   paste0("lag_",1:n4,"_",varns[v4], collapse = "+"),"+", 
                   paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3,"_",extremes[4], "_", n4   )
          metrics[[o]][["lag_diff"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_diff"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_diff"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_diff"]][[id]][["model"]]<-m
          
          ### no controls t,p 
          #construct equation
          r=paste0(varns[v], "+", 
                   paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                   paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                   paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+",
                   paste0("lag_",1:n4,"_",varns[v4], collapse = "+"),"+", 
                   paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                   sep='')
          f= as.formula(paste( o, "~", r, "|" ,i ))
          m = fixest::feols(f, data , panel.id = pan_id)
          
          # cross val
          data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
          f <- as.formula(paste( o, "~", r ,"-1" ))
          data_dem$year<-data$year
          id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  ,"_",extremes[4], "_", n4 )
          metrics[[o]][["lag_diff_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["AIC"]]<-AIC(m)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["BIC"]]<-BIC(m)
          metrics[[o]][["lag_diff_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
          metrics[[o]][["lag_diff_no_tp"]][[id]][["model"]]<-m
          
          }
        }
      }
    }
  }
}


save(metrics, file=file.path(out_dir, paste0(o, "_complete_models_lags_metrics.RData")))



################################################################################

#############
### income index 
#############

o<-"diff_income_index"
extremes<-c("PEXT", "HW", "WD", "SPI")

metrics <- list()
models <- list()


# BURKE
r <- paste0("TM + TM_2 + RR + RR_2 + ", extremes[1], " + ", paste0(extremes[1],"_2"), " + ", paste0("TM_i_", extremes[1]), " + ",
            extremes[2], " + ", paste0(extremes[2],"_2"), " + ", paste0("TM_i_", extremes[2])," + ",
            extremes[3], " + ", paste0(extremes[3],"_2"), " + ", paste0("TM_i_", extremes[3])," + ",
            extremes[4], " + ", paste0(extremes[4],"_2"), " + ", paste0("TM_i_", extremes[4]))
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["level"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["level"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020) metrics[[o]][["level"]][["looyo_cv"]]<-cross_validation_fixest(data_dem=data_dem, f=f, nmax=10)
metrics[[o]][["level"]][["AIC"]]<-AIC(m)
metrics[[o]][["level"]][["BIC"]]<-BIC(m)
metrics[[o]][["level"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["level"]][["model"]]<-m

# DIFF
r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",extremes[1]), " + ", paste0("TM_i_","diff_", extremes[1] ) ,"+", 
            paste0("diff_",extremes[2]), " + ", paste0("TM_i_","diff_", extremes[2] ) ,"+", 
            paste0("diff_",extremes[3]), " + ", paste0("TM_i_","diff_", extremes[3] ),"+", 
            paste0("diff_",extremes[4]), " + ", paste0("TM_i_","diff_", extremes[4] ) )
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)

# cross val
data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
f <- as.formula(paste( o, "~", r ,"-1" ))
data_dem$year<-data$year
metrics[[o]][["diff"]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
metrics[[o]][["diff"]][["AIC"]]<-AIC(m)
metrics[[o]][["diff"]][["BIC"]]<-BIC(m)
metrics[[o]][["diff"]][["wr2"]]<-fixest::r2(m,type='wr2')
metrics[[o]][["diff"]][["model"]]<-m


### lags LEVEL
for (N_temp in 1:8){
  for (N_rain in 1:8){
    for (n1 in 1:8){
      for (n2 in 1:8){
        for (n3 in 1:8){
          for (n4 in 1:8){
            
            v_temp<-which(modns=="TM")
            v_rain<-which(modns=="RR")
            v1<-which(modns==extremes[1])
            v2<-which(modns==extremes[2])
            v3<-which(modns==extremes[3])
            v4<-which(modns==extremes[4])
            
            # no interactions 
            r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",modns[v4], "+",
                     paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+",
                     paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                     paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                     paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),
                     sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
            metrics[[o]][["lag_level_no_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_no_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_no_interact"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_level_no_interact"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_level_no_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_level_no_interact"]][[id]][["model"]]<-m
            
            
            #interactions, but level focus 
            r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v1], "+",modns[v2], "+",modns[v3], "+",modns[v4], "+",
                     paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                     paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                     paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                     paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                     paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
                     paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                     paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+", 
                     paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),"+", 
                     paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                     sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
            metrics[[o]][["lag_level_interact"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_interact"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_interact"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_level_interact"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_level_interact"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_level_interact"]][[id]][["model"]]<-m
            
            
            ### no t, p controls 
            
            # no interactions 
            r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                      paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                      paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                      paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                      paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),
                      sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_level_no_interact_no_tp"]][[id]][["model"]]<-m
            
            
            #interactions, but level focus 
            r=paste0( modns[v1], "+",modns[v2], "+",modns[v3], "+",
                      paste0(modns[v_temp], "_i_", varns[v1]), "+","+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                      paste0("lag_",1:n1,"_",modns[v1], collapse = "+"),"+", 
                      paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                      paste0("lag_",1:n2,"_",modns[v2], collapse = "+"),"+", 
                      paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                      paste0("lag_",1:n3,"_",modns[v3], collapse = "+"),"+", 
                      paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+", 
                      paste0("lag_",1:n4,"_",modns[v4], collapse = "+"),"+", 
                      paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                      sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0("TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3 ,"_",extremes[4], "_", n4  )
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_level_interact_no_tp"]][[id]][["model"]]<-m
            
          }
        }
      }
    } 
  }
}


### lags DIFF 
for (N_temp in 1:8){
  for (N_rain in 1:8){
    for (n1 in 1:8){
      for (n2 in 1:8){
        for (n3 in 1:8){
          for (n4 in 1:8){
            
            v_temp<-which(modns=="TM")
            v_rain<-which(modns=="RR")
            v<-which(modns==extremes)
            
            #construct equation
            r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v1], "+",varns[v2], "+",varns[v3], "+",varns[v4], "+",
                     paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                     paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                     paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                     paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                     paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                     paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
                     paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+",
                     paste0("lag_",1:n4,"_",varns[v4], collapse = "+"),"+", 
                     paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                     sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3,"_",extremes[4], "_", n4   )
            metrics[[o]][["lag_diff"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_diff"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_diff"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_diff"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_diff"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_diff"]][[id]][["model"]]<-m
            
            ### no controls t,p 
            #construct equation
            r=paste0(varns[v], "+", 
                     paste0(modns[v_temp], "_i_", varns[v1]), "+",paste0(modns[v_temp], "_i_", varns[v2]), "+",paste0(modns[v_temp], "_i_", varns[v3]), "+",paste0(modns[v_temp], "_i_", varns[v4]), "+",
                     paste0("lag_",1:n1,"_",varns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n1,"_",modns[v_temp],'_i_',varns[v1], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",varns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n2,"_",modns[v_temp],'_i_',varns[v2], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",varns[v3], collapse = "+"),"+", 
                     paste0("lag_",1:n3,"_",modns[v_temp],'_i_',varns[v3], collapse = "+"),"+",
                     paste0("lag_",1:n4,"_",varns[v4], collapse = "+"),"+", 
                     paste0("lag_",1:n4,"_",modns[v_temp],'_i_',varns[v4], collapse = "+"),
                     sep='')
            f= as.formula(paste( o, "~", r, "|" ,i ))
            m = fixest::feols(f, data , panel.id = pan_id)
            
            # cross val
            data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
            f <- as.formula(paste( o, "~", r ,"-1" ))
            data_dem$year<-data$year
            id<-paste0( "TM_",N_temp,"_",  "RR_",N_rain, "_",extremes[1], "_", n1, "_",extremes[2], "_", n2,"_",extremes[3], "_", n3  ,"_",extremes[4], "_", n4 )
            metrics[[o]][["lag_diff_no_tp"]][[id]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_diff_no_tp"]][[id]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
            metrics[[o]][["lag_diff_no_tp"]][[id]][["AIC"]]<-AIC(m)
            metrics[[o]][["lag_diff_no_tp"]][[id]][["BIC"]]<-BIC(m)
            metrics[[o]][["lag_diff_no_tp"]][[id]][["wr2"]]<-fixest::r2(m,type='wr2')
            metrics[[o]][["lag_diff_no_tp"]][[id]][["model"]]<-m
          }
        }
      }
    }
  }
}


save(metrics, file=file.path(out_dir, paste0(o, "_complete_models_lags_metrics.RData")))







