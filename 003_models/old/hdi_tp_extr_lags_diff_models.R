### model following lags spec + level on level with/without autoregressive part (in first diff estimation)
### like kotz 2024 

### in general:
### for each, save aic, bic, r2, perform cv only on last year for each region, when data available. save also st err regional, country, DK
### in the end, for hdi model, perform robustness checks by erasing outliers-> indicator saturation, ask fra


rm(list=ls())

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
out_dir<-"output/models/hdi_tp_extr_lags_diff_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
colnames(data)

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

### setup 

pan_id<-c('gdlcode', 'year')

colnames(data)

################################################################################

### simple level models: temp, prec
models<-list()

# simplest model

o <- "diff_hdi"
i <- "gdlcode"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2+lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR+ lag_log_gni_pc:diff_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR+ lag_log_gni_pc:diff_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM_2 + diff_RR  + diff_RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR+ lag_log_gni_pc:diff_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM_2 + diff_RR + diff_RR_2 +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_TM_2 + lag_log_gni_pc:diff_RR+ lag_log_gni_pc:diff_RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_tp_iso.html"))

### simple level models with interaction: temp, prec
models<-list()

# simplest model

o <- "diff_hdi"
i <- "gdlcode"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR + lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_interact_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_interact_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_interact_tp_iso.html"))




### simple level models with interaction: temp, prec
### NB: no more individual time series if we estimate first diff 
models<-list()

# simplest model


o <- "diff_hdi"
i <- " year"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- " year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- " year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- " year"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- " year + iso3[year] +iso3[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- " year + gdlcode[year] +gdlcode[year^2]"
r <- "diff_TM + diff_TM:TM + diff_RR + diff_RR:RR +lag_log_gni_pc:diff_TM + lag_log_gni_pc:diff_RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_no_gdl_interact_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_no_gdl_interact_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_level_no_gdl_interact_tp_iso.html"))

################################################################################

# no extremes, lag selection based on different models, level mod

varns=c('diff_TM','diff_RR')
modns=c('TM','RR')
adap=c('lag_log_gni_pc','lag_log_gni_pc')
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # works better with level interaction
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)


for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
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
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

jpeg(file=file.path(out_dir,"lag_selection_hdi_tp.jpeg"))
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

### lets keep 6 for tm, 3 for rr, add adap

TNL<-6
RNL<-2
out_var<-"diff_hdi"

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")",  "|" , idx ))
mod0 = fixest::feols(f, data, panel.id=pan_id)
summary(mod0)

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")",  "|" , idx ))
mod02 = fixest::feols(f, data, panel.id=pan_id)
summary(mod02)

# f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")"," + lag_log_gni_pc:TM + lag_log_gni_pc:RR",  "|" , idx ))
# mod03 = fixest::feols(f, data, panel.id=pan_id)
# summary(mod03) 
# 

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL,")+ l(RR_i_diff_RR,0:",RNL, ")"," + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR ",  "|" , idx ))
mod04 = fixest::feols(f, data, panel.id=pan_id)
summary(mod04)
# 

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")","+l(TM_i_diff_TM,0:",TNL, ")+ l(lag_log_gni_pc_i_diff_RR,0:",RNL, ") + l(lag_log_gni_pc_i_diff_TM,0:",TNL, ")  ",  "|" , idx ))
mod05 = fixest::feols(f, data, panel.id=pan_id)
summary(mod05) 
# 

models<-list(mod0, mod02, mod04, mod05)

fvcov_dk<-function(x) vcov(x, "DK")
modelsummary(models,lapply(models, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_level_tp_lags_dk.html"))
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_level_tp_lags.html"))






### run model idx_iso, adap lev, only tp

NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + l(',adap[v],'_i_',modns[v],',0:',NL+1-r,') + ', sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + l(',adap[v],'_i_',modns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx_iso)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

# plot 

jpeg(file=file.path(out_dir,"lag_selection_adap_lev_no_extr.jpeg"))
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



### run model idx_iso, no adap , only tp

NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + ', sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx_iso)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

# plot 

jpeg(file=file.path(out_dir,"lag_selection_no_adap_no_extr.jpeg"))
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

# compare the two model performances in terms of rsq, aic, bic ( all of one, all of other, see what better )



varns=c('diff_TM','diff_RR',)
modns=c('TM','RR')
adap=c('lag_log_gni_pc','lag_log_gni_pc')


for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]] # level adap
} 

for (i in 1:length(varns)){ 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]] # diff adap
} 



### run model idx_iso, adap diff, only tp

NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + l(',adap[v],'_i_',varns[v],',0:',NL+1-r,') + ', sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + l(',adap[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx_iso)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

# plot 

jpeg(file=file.path(out_dir,"lag_selection_adap_diff_extr.jpeg"))
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



### run model idx_iso, adap lev, only tp

NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + l(',adap[v],'_i_',modns[v],',0:',NL+1-r,') + ', sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + l(',adap[v],'_i_',modns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx_iso)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

# plot 

jpeg(file=file.path(out_dir,"lag_selection_adap_lev_extr.jpeg"))
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



### run model idx_iso, no adap , only tp

NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + l(',modns[v],'_i_',varns[v],',0:',NL+1-r,') + ', sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + l(',modns[v],'_i_',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx_iso)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

# plot 

jpeg(file=file.path(out_dir,"lag_selection_no_adap_extr.jpeg"))
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


################################################################################
# adding extremes, lag selection based on different models, level mod
################################################################################


varns=c('diff_TM','diff_RR', "diff_WD", "diff_SPI", "diff_PET")
modns=c('TM','RR', "WD", "SPI", "PET")
adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc')
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # works better with level interaction
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

out_dir_lag_int_extr<-"output/models/first_diff_lags_hdi_models/lag_selection_int_extr"
if(!dir.exists(out_dir_lag_int_extr)){dir.create(out_dir_lag_int_extr)}

out_dir_temp<-out_dir_lag_int_extr

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
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
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

jpeg(file=file.path(out_dir_temp,"lag_selection_extremes.jpeg"), width=1500, height=1000)
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


# adding extremes, lag selection based on different models, level mod, also tvar

varns=c('diff_TM','diff_RR', "diff_WD", "diff_SPI", "diff_PET", "diff_TVAR")
modns=c('TM','RR', "WD", "SPI", "PET", "TVAR")
adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc', 'lag_log_gni_pc')
idx <- "gdlcode + year + iso3[year] +iso3[year^2]"

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # works better with level interaction
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

out_dir_lag_int_extr<-"output/models/first_diff_lags_hdi_models/lag_selection_int_extr"
if(!dir.exists(out_dir_lag_int_extr)){dir.create(out_dir_lag_int_extr)}

out_dir_temp<-out_dir_lag_int_extr

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    #construct equation
    equation='diff_hdi ~ '
    
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
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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

jpeg(file=file.path(out_dir_temp,"lag_selection_extremes_and_tvar.jpeg"),  width=1500, height=1000)
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



### select lag number
out_dir<-"output/models/first_diff_lags_hdi_models"

TNL<-6
RNL<-2
out_var<-"diff_hdi"

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")",  "|" , idx ))
mod0 = fixest::feols(f, data, panel.id=pan_id)
summary(mod0)

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")",  "|" , idx ))
mod02 = fixest::feols(f, data, panel.id=pan_id)
summary(mod02)

# f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")"," + lag_log_gni_pc:TM + lag_log_gni_pc:RR",  "|" , idx ))
# mod03 = fixest::feols(f, data, panel.id=pan_id)
# summary(mod03) 
# 

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(TM_i_diff_TM,0:",TNL, ")+ l(diff_RR,0:",RNL,")+ l(RR_i_diff_RR,0:",RNL, ")"," + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR ",  "|" , idx ))
mod04 = fixest::feols(f, data, panel.id=pan_id)
summary(mod04)
# 

f <- as.formula( paste(out_var ,"~",  " l(diff_TM,0:",TNL, ") + l(diff_RR,0:",RNL, ")+ l(RR_i_diff_RR,0:",RNL, ")","+l(TM_i_diff_TM,0:",TNL, ")+ l(lag_log_gni_pc_i_diff_RR,0:",RNL, ") + l(lag_log_gni_pc_i_diff_TM,0:",TNL, ")  ",  "|" , idx ))
mod05 = fixest::feols(f, data, panel.id=pan_id)
summary(mod05) 
# 

models<-list(mod0, mod02, mod04, mod05)

fvcov_dk<-function(x) vcov(x, "DK")
modelsummary(models,lapply(models, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir_temp, "models_hdi_level_tp_lags_dk.html"))
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir_temp, "models_hdi_level_tp_lags.html"))


