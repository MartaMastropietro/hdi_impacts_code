
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
  
  data_train<-data_dem[ data_dem$year <y, ]
  data_test<-data_dem[ data_dem$year ==y, ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e1= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
  data_train<-data_dem[ data_dem$year <(y-1), ]
  data_test<-data_dem[ data_dem$year >=(y-1), ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e2= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
  data_train<-data_dem[ data_dem$year <(y-2), ]
  data_test<-data_dem[ data_dem$year >=(y-2), ]
  m_cross_val <- fixest::feols(f, data_train)
  pred<-predict(m_cross_val, data_test)
  e3= (sqrt(sum((data_test[[o]]-pred)^2)/length(pred)))
  
  return ((e1+e2+e3)/3)
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

### 
### only t,p 

indeces<-c(
  "gdlcode + iso3[year] + iso3[year^2] + year"
)
names(indeces)<-c(
  "fe_quad_iso" )

out_vars<-c("diff_hdi", "diff_income_index", "diff_edu_index", "diff_health_index" )

metrics <-list()

for (o in out_vars){
  for (i in indeces){
    
    ### different kinds of models tested
    
    
    ###
    r <- paste0("TM + TM_2 + RR + RR_2 ")
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[o]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[o]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[o]][[i]][[r]][["AIC"]]<-AIC(m)
    metrics[[o]][[i]][[r]][["BIC"]]<-BIC(m)
    metrics[[o]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
    metrics[[o]][[i]][[r]][["model"]]<-m
    
    
    
    ###
    r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR ")
    f <- as.formula(paste( o, "~", r, "|" ,i ))
    m <- fixest::feols(f, data, panel.id=pan_id)
    
    # cross val
    data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
    f <- as.formula(paste( o, "~", r ,"-1" ))
    data_dem$year<-data$year
    metrics[[o]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
    metrics[[o]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
    metrics[[o]][[i]][[r]][["AIC"]]<-AIC(m)
    metrics[[o]][[i]][[r]][["BIC"]]<-BIC(m)
    metrics[[o]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
    metrics[[o]][[i]][[r]][["model"]]<-m
    
    
    
    ### lags a la callahan 
    for (N_temp in 1:8){
      for(N_rain in 1:8){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        #construct equation
        r=paste0(modns[v_temp], "+", modns[v_rain], "+",
                 paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        metrics[[o]][[i]][[r]][["model"]]<-m
        
        
      }
    }
    
    
    ### lags a la kotz
    for (N_temp in 1:8){
      for(N_rain in N_rain:8){
        v_temp<-which(modns=="TM")
        v_rain<-which(modns=="RR")
        #construct equation
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                 paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),
                 sep='')
        f= as.formula(paste( o, "~", r, "|" ,i ))
        m = fixest::feols(f, data , panel.id = pan_id)
        
        # cross val
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        metrics[[o]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
        metrics[[o]][[i]][[r]][["AIC"]]<-AIC(m)
        metrics[[o]][[i]][[r]][["BIC"]]<-BIC(m)
        metrics[[o]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
        metrics[[o]][[i]][[r]][["model"]]<-m
        
      }
    }
  }
}


save(metrics, file=file.path(out_dir, "all_comp_tp_lags_models_metrics.RData"))

i<-indeces
formulas_length<-74
formulas<-paste0("f_", 1:formulas_length)

metrics_data<-expand.grid(out_var=out_vars,   formula=formulas )
metrics_data$aic<-NA
metrics_data$bic<-NA
metrics_data$wr2<-NA
metrics_data$l1yo_cv<-NA
metrics_data$l3yo_cv<-NA


for (o in out_vars){
  for (f in 1:formulas_length){
    
    metrics_data[which(metrics_data$out_var == o &
                         metrics_data$formula == formulas[f]  ), "aic"]<- metrics[[o]][[i]][[f]][["AIC"]]
    metrics_data[which(metrics_data$out_var == o &
                         metrics_data$formula == formulas[f]  ), "bic"]<- metrics[[o]][[i]][[f]][["BIC"]]
    metrics_data[which(metrics_data$out_var == o &
                         metrics_data$formula == formulas[f]  ), "wr2"]<- metrics[[o]][[i]][[f]][["wr2"]]
    metrics_data[which(metrics_data$out_var == o &
                         metrics_data$formula == formulas[f]  ), "l1yo_cv"]<- metrics[[o]][[i]][[f]][["l1yo_cv"]]
    metrics_data[which(metrics_data$out_var == o ,
                       metrics_data$formula == formulas[f]  ), "l3yo_cv"]<- metrics[[o]][[i]][[f]][["l3yo_cv"]]
    
  }
}


ggplot(metrics_data)+ geom_boxplot(aes(x=formula, y=l1yo_cv)) + facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=formula, y=l3yo_cv)) + facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=formula, y=wr2))+ facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=formula, y=aic))+ facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=formula, y=bic))+ facet_wrap(~out_var)


# ### test inclusion of extremes oatt, all lags tested, 1 and 3 years of cv tested
# # save metrics 
# # adaptation? 
# # autoreg?
# # we interact variables with mean temperature in diff lags specs 
# 
# extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
# 
# indeces<-c(
#   "gdlcode + iso3[year] + iso3[year^2] + year"
# )
# names(indeces)<-c(
#   "fe_quad_iso" )
# 
# out_vars<-c("diff_hdi", "diff_income_index", "diff_edu_index", "diff_health_index" )
# 
# metrics <-list()
# 
# for (o in out_vars){
#   for (ext in extr_variables){
#     for (i in indeces){
#       
#       ### different kinds of models tested
#       models<-list()
#       
#       ###
#       r <- paste0("TM + TM_2 + RR + RR_2 + ", ext, " + ", paste0(ext,"_2"))
#       f <- as.formula(paste( o, "~", r, "|" ,i ))
#       m <- fixest::feols(f, data, panel.id=pan_id)
#       models[[length(models)+1]]<-m
#       # cross val
#       data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#       f <- as.formula(paste( o, "~", r ,"-1" ))
#       data_dem$year<-data$year
#       metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#       metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#       metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#       metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#       metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#       
#       ###
#       r <- paste0("diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + ", paste0("diff_",ext), " + ", paste0("TM_i_","diff_", ext ) )
#       f <- as.formula(paste( o, "~", r, "|" ,i ))
#       m <- fixest::feols(f, data, panel.id=pan_id)
#       models[[length(models)+1]]<-m
#       # cross val
#       data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#       f <- as.formula(paste( o, "~", r ,"-1" ))
#       data_dem$year<-data$year
#       metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#       metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#       metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#       metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#       metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#       
#       
#       ### lags LEVEL
#       for (n in 1:8){
#         for (N_temp in 1:8){
#           for (N_rain in 1:8){
#             v_temp<-which(modns=="TM")
#             v_rain<-which(modns=="RR")
#             v<-which(modns==ext)
#             
#             # no interactions 
#             r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+",
#                      paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+",
#                      paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
#                      paste0("lag_",1:n,"_",modns[v], collapse = "+"),
#                      sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#             
#             #interactions, but level focus 
#             r=paste0(modns[v_temp], "+", modns[v_rain], "+", modns[v], "+", 
#                      paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
#                      paste0("lag_",1:N_temp,"_",modns[v_temp], collapse = "+"),"+", 
#                      paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
#                      paste0("lag_",1:N_rain,"_",modns[v_rain], collapse = "+"),"+", 
#                      paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
#                      paste0("lag_",1:n,"_",modns[v], collapse = "+"),"+", 
#                      paste0("lag_",1:n,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
#                      sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#             
#             ### no t, p controls 
#             
#             # no interactions 
#             r=paste0( modns[v], "+",
#                       paste0("lag_",1:n,"_",modns[v], collapse = "+"),
#                       sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#             
#             #interactions, but level focus 
#             r=paste0( modns[v], "+", 
#                       paste0(modns[v_temp], "_i_", varns[v]), "+",
#                       paste0("lag_",1:n,"_",modns[v], collapse = "+"),"+", 
#                       paste0("lag_",1:n,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
#                       sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#           }
#         }
#       }
#       
#       
#       ### lags DIFF 
#       for (n in 1:8){
#         for (N_temp in 1:8){
#           for (N_rain in 1:8){
#             v_temp<-which(modns=="TM")
#             v_rain<-which(modns=="RR")
#             v<-which(modns==ext)
#             
#             #construct equation
#             r=paste0(varns[v_temp], "+", varns[v_rain], "+", varns[v], "+", 
#                      paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",paste0(modns[v_temp], "_i_", varns[v]), "+",
#                      paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
#                      paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
#                      paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
#                      paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),"+",
#                      paste0("lag_",1:n,"_",varns[v], collapse = "+"),"+", 
#                      paste0("lag_",1:n,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
#                      sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#             ### no controls t,p 
#             #construct equation
#             r=paste0(varns[v], "+", 
#                      paste0(modns[v_temp], "_i_", varns[v]), "+",
#                      paste0("lag_",1:n,"_",varns[v], collapse = "+"),"+", 
#                      paste0("lag_",1:n,"_",modns[v_temp],'_i_',varns[v], collapse = "+"),
#                      sep='')
#             f= as.formula(paste( o, "~", r, "|" ,i ))
#             m = fixest::feols(f, data , panel.id = pan_id)
#             models[[length(models)+1]]<-m
#             # cross val
#             data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#             f <- as.formula(paste( o, "~", r ,"-1" ))
#             data_dem$year<-data$year
#             metrics[[o]][[ext]][[i]][[r]][["l3yo_cv"]]<-cross_validation_fixest_3year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["l1yo_cv"]]<-cross_validation_fixest_1year(data_dem=data_dem, f=f, y=2020)
#             metrics[[o]][[ext]][[i]][[r]][["AIC"]]<-AIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["BIC"]]<-BIC(m)
#             metrics[[o]][[ext]][[i]][[r]][["wr2"]]<-fixest::r2(m,type='wr2')
#             
#             
#           }
#         }
#       }
#       
#     }
#   }
# }
# 
# save(metrics, file=file.path(out_dir, "all_comp_tp_extr_lags_models_all_variables_metrics.RData"))# 