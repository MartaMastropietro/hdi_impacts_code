### try more complete models for each variable



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
out_dir<-"output/models/components_tp_extr_complex_models_try"
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
N_temp<-6
N_rain<-4
N<-5







################################################################################
### one at the time extreme variables addition


### models edu

o<-"diff_edu_index"


# tp burke
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")




# lag selection

varns=c('diff_TM', "diff_RR")
modns=c('TM', "RR")
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"
panel_id<-pan_id

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 


NL=8

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)


for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation=paste(o,' ~ ')
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + ',sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = panel_id)
    print(summary(mod))
    BICs=append(BICs,BIC(mod))
    AICs=append(AICs,AIC(mod))
    wr2s=append(wr2s,r2(mod,type='wr2'))
    Ns  =append(Ns,nobs(mod))
  }
}

BICs<-BICs[-1]
AICs<-AICs[-1]
wr2s<-wr2s[-1]
Ns  <-  Ns[-1]

# write.csv(BICs,file.path(out_dir_temp, paste('lag_selectionBICs_',as.character(NL),'.csv',sep='')))
# write.csv(AICs,file.path(out_dir_temp,paste('lag_selectionAICs_',as.character(NL),'.csv',sep='')))
# write.csv(wr2s,file.path(out_dir_temp,paste('lag_selectionwr2s_',as.character(NL),'.csv',sep='')))
# write.csv(Ns,file.path(out_dir_temp,paste('lag_selectionNs_',as.character(NL),'.csv',sep='')))

# plot 

jpeg(file=file.path(out_dir,"lag_selection_edu_index_tp.jpeg"), width=2000, height = 1500)
par(mfrow=c(3,length(varns)))
nls<-seq(1,NL, length.out=NL)

for (v in 1:length(varns)){
  plot(nls,BICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="BICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,AICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="AICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,wr2s[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="wr2s", type="l")
}

dev.off()


# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + RR + l(TM, 1:5) + l(RR, 1:5) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + RR + TM_i_diff_TM + RR_i_diff_RR + l(TM, 1:5) + l(RR, 1:5) + l(TM_i_diff_TM, 1:5) + l(RR_i_diff_RR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


################################################################################

### adding extremes 

### wd 

# tp burke + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# significant if including WD_2

# tp burke + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + WD + WD_2 + TM_i_WD"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# significant if including WD_2


# tp kotz + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
      + RR + RR_2 +  diff_WD + TM_i_diff_WD + l(diff_WD, 1:4) +l(TM_i_diff_WD, 1:4) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# tp kotz + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + l(diff_TM, 1:4) +l(TM_i_diff_TM, 1:4)+
      + RR + RR_2 +  WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


### rx

# tp burke + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + RX + RX:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# no

# tp kotz + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
      + RR + RR_2 +  diff_RX + TM_i_diff_RX + l(diff_RX, 1:4) +l(TM_i_diff_RX, 1:4) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# no


### PEXT

# tp burke + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + PEXT + PEXT:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# no

# tp kotz + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
      + RR + RR_2 +  diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) +l(TM_i_diff_PEXT, 1:4) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# yes


### wd and pext 

# tp kotz + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
      + RR + RR_2 +  diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) +l(TM_i_diff_PEXT, 1:4) +
      + WD + WD_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# yes


### spi

# tp burke + spi
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + SPI + SPI:TM"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# no


# tp burke + spi lag
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + SPI + diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# tp kotz + spi
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
      + RR + RR_2 +  diff_SPI  + l(diff_SPI, 1:4)  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



### tvar, hw

# tp burke + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + TVAR + TVAR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp burke + tvar + HW
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + TVAR + TVAR_2 + HW + HW_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no


# tp kotz + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
     diff_TVAR  +TVAR_i_diff_TVAR + l(diff_TVAR, 1:3) + l(TVAR_i_diff_TVAR, 1:3) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# 3 lags, no rebound

# tp kotz + tvar + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
     diff_TVAR  +TVAR_i_diff_TVAR + l(diff_TVAR, 1:3) + l(TVAR_i_diff_TVAR, 1:3) + 
     diff_HW  +HW_i_diff_HW + l(diff_HW, 1:3) + l(HW_i_diff_HW, 1:3) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


# tp kotz + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
     diff_TVAR  +TVAR_i_diff_TVAR + l(diff_TVAR, 1:3) + l(TVAR_i_diff_TVAR, 1:3) + 
     diff_HW  +HW_i_diff_HW + l(diff_HW, 1:3) + l(HW_i_diff_HW, 1:3) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### tvar + wd + pext

# tp kotz + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:6) + l(diff_RR, 1:4) + l(TM_i_diff_TM, 1:6) + l(RR_i_diff_RR, 1:4)+
     diff_TVAR  +TVAR_i_diff_TVAR + l(diff_TVAR, 1:3) + l(TVAR_i_diff_TVAR, 1:3) + 
     RR + RR_2 +  diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) +l(TM_i_diff_PEXT, 1:4) +
     WD + WD_2 "
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

# lag selection

varns=c('diff_TM', "diff_RR")
modns=c('TM', "RR")
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"
panel_id<-pan_id

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 


NL=8

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)


for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation=paste(o,' ~ ')
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + ',sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = panel_id)
    print(summary(mod))
    BICs=append(BICs,BIC(mod))
    AICs=append(AICs,AIC(mod))
    wr2s=append(wr2s,r2(mod,type='wr2'))
    Ns  =append(Ns,nobs(mod))
  }
}

BICs<-BICs[-1]
AICs<-AICs[-1]
wr2s<-wr2s[-1]
Ns  <-  Ns[-1]

# write.csv(BICs,file.path(out_dir_temp, paste('lag_selectionBICs_',as.character(NL),'.csv',sep='')))
# write.csv(AICs,file.path(out_dir_temp,paste('lag_selectionAICs_',as.character(NL),'.csv',sep='')))
# write.csv(wr2s,file.path(out_dir_temp,paste('lag_selectionwr2s_',as.character(NL),'.csv',sep='')))
# write.csv(Ns,file.path(out_dir_temp,paste('lag_selectionNs_',as.character(NL),'.csv',sep='')))

# plot 

jpeg(file=file.path(out_dir,"lag_selection_health_index_tp.jpeg"), width=2000, height = 1500)
par(mfrow=c(3,length(varns)))
nls<-seq(1,NL, length.out=NL)

for (v in 1:length(varns)){
  plot(nls,BICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="BICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,AICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="AICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,wr2s[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="wr2s", type="l")
}

dev.off()

# lags
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### tvar

# tp + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + TVAR + TVAR_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5) + 
      diff_TVAR + l(diff_TVAR, 1:3) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes



### hw 

# tp + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + HW + HW:RR" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_HW + TM_i_diff_HW" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)  +
      diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

### rx

# tp + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + RX + RX_2 + RX:TM" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_RX + TM_i_diff_RX " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### pext

# tp + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + PEXT + PEXT_2 + PEXT:TM" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_PEXT + RR_i_diff_PEXT " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# inconclusive

# lags + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)  +
      diff_PEXT + RR_i_diff_PEXT + l(diff_PEXT, 1:4) + l(RR_i_diff_PEXT, 1:4)  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes, more lags work better 


### wd

# tp + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + WD + WD_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_WD + TM_i_diff_WD " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)  +
      diff_WD + TM_i_diff_WD  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes, more lags non sign


# lags + hw + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)  +
      diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) + diff_WD + TM_i_diff_WD "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes


### spi

# tp + SPI
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + SPI + SPI_2" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# inconsistent across errors 

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_SPI + TM_i_diff_SPI" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_SPI + TM_i_diff_SPI + l(diff_SPI, 1:6) + l(TM_i_diff_SPI, 1:6) " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + spi
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5)  +
      diff_SPI + TM_i_diff_SPI + l(diff_SPI, 1:6) + l(TM_i_diff_SPI, 1:6) "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes



# lags spi + hw + wd + tvar

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:3) + l(diff_RR, 1:5) + l(TM_i_diff_TM, 1:3) + l(RR_i_diff_RR, 1:5) +
      diff_TVAR + l(diff_TVAR, 1:3) + 
      diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) + 
      diff_WD + TM_i_diff_WD  + 
      diff_SPI + TM_i_diff_SPI + l(diff_SPI, 1:6) + l(TM_i_diff_SPI, 1:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR +
      diff_TVAR + l(diff_TVAR, 1:3) + diff_HW + TM_i_diff_HW + l(diff_HW, 1:6) + l(TM_i_diff_HW, 1:6) + diff_WD + TM_i_diff_WD  + diff_SPI + TM_i_diff_SPI + l(diff_SPI, 1:6) + l(TM_i_diff_SPI, 1:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes



modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "health_index_models_iso.html"))



################################################################################


### models health
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


# lag selection

varns=c('diff_TM', "diff_RR")
modns=c('TM', "RR")
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"
panel_id<-pan_id

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 


NL=8

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)


for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation=paste(o,' ~ ')
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + ',sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = panel_id)
    print(summary(mod))
    BICs=append(BICs,BIC(mod))
    AICs=append(AICs,AIC(mod))
    wr2s=append(wr2s,r2(mod,type='wr2'))
    Ns  =append(Ns,nobs(mod))
  }
}

BICs<-BICs[-1]
AICs<-AICs[-1]
wr2s<-wr2s[-1]
Ns  <-  Ns[-1]

# write.csv(BICs,file.path(out_dir_temp, paste('lag_selectionBICs_',as.character(NL),'.csv',sep='')))
# write.csv(AICs,file.path(out_dir_temp,paste('lag_selectionAICs_',as.character(NL),'.csv',sep='')))
# write.csv(wr2s,file.path(out_dir_temp,paste('lag_selectionwr2s_',as.character(NL),'.csv',sep='')))
# write.csv(Ns,file.path(out_dir_temp,paste('lag_selectionNs_',as.character(NL),'.csv',sep='')))

# plot 

jpeg(file=file.path(out_dir,"lag_selection_income_index_tp.jpeg"), width=2000, height = 1500)
par(mfrow=c(3,length(varns)))
nls<-seq(1,NL, length.out=NL)

for (v in 1:length(varns)){
  plot(nls,BICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="BICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,AICs[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="AICs", type="l")
}

for (v in 1:length(varns)){
  plot(nls,wr2s[((v-1)*NL +1):((v)*NL)]/1000, main=paste(varns[v]), xlab = "n lags", ylab="wr2s", type="l")
}

dev.off()

# lags 4, 3 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


### tvar

# tp + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + TVAR + TVAR:TM" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# lags + tvar
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3)+ 
      diff_TVAR + l(diff_TVAR, 1:5) + TM_i_diff_TVAR + l(TM_i_diff_TVAR, 1:5)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# 



### hw 

# tp + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + HW + HW:diff_TM" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_HW + TM_i_diff_HW" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + hw
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3)+
      diff_HW + TM_i_diff_HW  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

### rx

# tp + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + RX + RX_2 + RX:RR" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# tp + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_RX + RR_i_diff_RX " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
#no 

# lags + rx
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3) +
      diff_RX + RR_i_diff_RX  +  l(diff_RX, 1:4) + l(RR_i_diff_RX, 1:4)  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM +  l(diff_TM, 1:5) + l(TM_i_diff_TM, 1:5) +
       RX + RX_2 + RX:RR  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

### pext

# tp + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + PEXT + PEXT_2 + PEXT:TM" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# tp + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_PEXT + TM_i_diff_PEXT " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + pext
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3) +
      diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) + l(TM_i_diff_PEXT, 1:4)  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes


### wd

# tp + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + WD + WD_2 + WD:RR" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# no

# tp + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_WD + RR_i_diff_WD " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3) +
      diff_WD + RR_i_diff_WD  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes, 

# lags + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3)+
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes, 


# lags + wd
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM +  TM_i_diff_TM + l(diff_TM, 1:5) +  l(TM_i_diff_TM, 1:5) +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6)"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes


### spi

# tp + SPI
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2  + RR + RR_2 + SPI " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_SPI " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# 

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 + diff_SPI  + l(diff_SPI, 1:6)  " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + spi
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3) +
      diff_SPI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes

# lags + spi
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM  + l(diff_TM, 1:5) + l(TM_i_diff_TM, 1:5) +
      diff_SPI + l(diff_SPI, 1:5)  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m
# yes



# lags spi + hw + wd + tvar

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_RR + TM_i_diff_TM + RR_i_diff_RR + l(diff_TM, 1:4) + l(diff_RR, 1:3) + l(TM_i_diff_TM, 1:4) + l(RR_i_diff_RR, 1:3)+ 
      diff_TVAR + l(diff_TVAR, 1:5) + TM_i_diff_TVAR + l(TM_i_diff_TVAR, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) + l(TM_i_diff_PEXT, 1:4) +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6) + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TVAR + l(diff_TVAR, 1:5) + TM_i_diff_TVAR + l(TM_i_diff_TVAR, 1:5) +
      diff_HW + TM_i_diff_HW + 
      diff_PEXT + TM_i_diff_PEXT + l(diff_PEXT, 1:4) + l(TM_i_diff_PEXT, 1:4) +
      diff_WD + RR_i_diff_WD  +l(diff_WD, 1:6) + l(RR_i_diff_WD, 1:6) + 
      diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "income_index_models_iso.html"))
