
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
out_dir<-"output/models/components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/components_tp_extr_complex_models_try/area_weighting"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_climate_gdl_1990_2021.csv")

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

################################################################################

### lag selection only t,p 

varns=c('diff_TM', "diff_RR")
modns=c('TM', "RR")
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"
panel_id<-pan_id

out_dir_lag<-"output/models/components_tp_extr_complex_models_try/tp_lag_selection"
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 


NL=10

for (o in c("diff_edu_index", "diff_health_index", "diff_income_index")){
  
  table=expand.grid(n_tm=0:NL, n_rr=0:NL)
  table$loocv<-0
  table$rsq<-0
  table$aic<-0
  table$bic<-0
  
  v_temp<-which(varns=="diff_TM")
  v_rain<-which(varns=="diff_RR")
  
  for (N_rain in 0:NL){
    for (N_temp in 0:NL){
      
      nmax<-max(N_rain,N_temp)
      
      if(N_rain==0 & N_temp==0){
        
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]),
                 sep='')
        
        f= as.formula(paste( o, "~", r, "|" ,idx ))
        mod = fixest::feols(f, data , panel.id = pan_id)
        
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", idx )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        
      }else if(N_rain==0 & N_temp!=0){
        
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]),"+", 
                 paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),
                 sep='')
        
        
        f= as.formula(paste( o, "~", r, "|" ,idx ))
        mod = fixest::feols(f, data , panel.id = pan_id)
        
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", idx )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        
        
      }else if(N_rain!=0 & N_temp==0){
        
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]),"+", 
                 paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),
                 sep='')
        
        
        f= as.formula(paste( o, "~", r, "|" ,idx ))
        mod = fixest::feols(f, data , panel.id = pan_id)
        
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", idx )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        
        
      }else if(N_rain!=0 & N_temp!=0){
        r=paste0(varns[v_temp], "+", varns[v_rain], "+", 
                 paste0(modns[v_temp], "_i_", varns[v_temp]), "+",paste0(modns[v_rain], "_i_", varns[v_rain]), "+",
                 paste0("lag_",1:N_temp,"_",varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_temp,"_",modns[v_temp],'_i_',varns[v_temp], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",varns[v_rain], collapse = "+"),"+", 
                 paste0("lag_",1:N_rain,"_",modns[v_rain],'_i_',varns[v_rain], collapse = "+"),
                 sep='')
        
        f= as.formula(paste( o, "~", r, "|" ,idx ))
        mod = fixest::feols(f, data , panel.id = pan_id)
        
        data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", idx )), data = data, na.rm=FALSE)
        f <- as.formula(paste( o, "~", r ,"-1" ))
        data_dem$year<-data$year
        
        
      }
      
      table[which(table$n_tm==N_temp & table$n_rr==N_rain),"loocv"]=  cross_validation_fixest(data_dem=data_dem, f=f, nmax=nmax)
      table[which(table$n_tm==N_temp & table$n_rr==N_rain),"bic"]=  BIC(mod)
      table[which(table$n_tm==N_temp & table$n_rr==N_rain),"aic"]=  AIC(mod)
      table[which(table$n_tm==N_temp & table$n_rr==N_rain),"rsq"]=  r2(mod, "wr2")
      
    }
  }
  
  write.csv(table, file=file.path(out_dir_lag,paste0(o, "_forward_lag_selection_loocv_tp.csv")))
  
  g<-ggplot(table, aes(x=n_tm, y=n_rr, col=loocv))+geom_point(size=7)
  ggsave(file=file.path(out_dir_lag,paste0(o, "_forward_lag_selection_loocv_tp.jpeg")), g)
  
  g<-ggplot(table, aes(x=n_tm, y=n_rr, col=bic))+geom_point(size=7)
  ggsave(file=file.path(out_dir_lag,paste0(o, "_forward_lag_selection_bic_tp.jpeg")), g)
  
  g<-ggplot(table, aes(x=n_tm, y=n_rr, col=aic))+geom_point(size=7)
  ggsave(file=file.path(out_dir_lag,paste0(o, "_forward_lag_selection_aic_tp.jpeg")), g)
  
  g<-ggplot(table, aes(x=n_tm, y=n_rr, col=rsq))+geom_point(size=7)
  ggsave(file=file.path(out_dir_lag,paste0(o, "_forward_lag_selection_rsq_tp.jpeg")), g)
  
}


################################################################################

### lets try local projections impulse response 



################################################################################
################################################################################
################################################################################
### one at the time extreme variables addition


### models edu

o<-"diff_edu_index"


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
      WD + WD_2 + diff_RX + diff_RX:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      WD + WD_2 + diff_RX + diff_RX:RR"
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


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "education_index_models_iso.html"))


################################################################################


### models health
o<-"diff_health_index"
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


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models_iso.html"))



################################################################################


### models income
o<-"diff_income_index"
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



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models_iso.html"))



###############################################################################

models<-list()


o<-"diff_income_index"


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





o<-"diff_health_index"



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


o<-"diff_edu_index"


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


names(models)<-c(rep("diff_income", 3), rep("diff_health", 3), rep("diff_edu", 3))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_models_iso.html"))


################################################################################


models<-list()


o<-"diff_income_index"


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


o<-"diff_health_index"


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



o<-"diff_edu_index"



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


names(models)<-c(rep("diff_income", 2), rep("diff_health", 2), rep("diff_edu", 2))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "all_components_adap_models_iso.html"))




###############################################################################


o<-"diff_income_index"

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
    title = "Shocks on Income Index")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###

o<-"diff_health_index"

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
    title = "Shocks on Health Index")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)


###


o<-"diff_edu_index"

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
    title = "Shocks on Education Index")
ggsave(filename=file.path(out_dir, paste0(o,"_plot_adap.jpeg")), height = 4, width = 4)

###############################################################################



