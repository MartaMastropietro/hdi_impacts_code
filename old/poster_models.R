################################################################################
# fig : table confront models ?
################################################################################

rm(list=ls())

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

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


out_dir<-"output/figures_poster_ems_2024"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

################################################################################
### model selection for extremes
################################################################################

### simple growth models: temp, prec
models<-list()

# simplest model

o <- "diff_hdi"
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m

o <- "diff_hdi"
i <- "gdlcode + year + gdlcode[year] +gdlcode[year^2]"
r <- "TM + TM_2 + RR + RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
models[[length(models)+1]]<-m


modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_growth_tp.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_growth_tp_dk.html"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), output=file.path(out_dir, "simple_growth_tp_iso.html"))

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

out_dir_lag_no_int_no_extr<-"output/models/first_diff_lags_hdi_models/lag_selection_int_no_extr"
if(!dir.exists(out_dir_lag_no_int_no_extr)){dir.create(out_dir_lag_no_int_no_extr)}

out_dir_temp<-out_dir_lag_no_int_no_extr

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

jpeg(file=file.path(out_dir_temp,"lag_selection.jpeg"))
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



################################################################################

### extremes one at the time 

out_dir<-"output/models/hdi_and_comp_extremes_models"

# hw

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " HW + HW_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " HW + HW_2  +lag_log_gni_pc:HW + lag_log_gni_pc:HW_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + HW + HW_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:RR_2 + HW + HW_2 +lag_log_gni_pc:HW + lag_log_gni_pc:HW_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2  + HW + HW_2  +lag_log_gni_pc:HW + lag_log_gni_pc:HW_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# not sign when adding controls t, r, t alone linear sign when adap

# tvar

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TVAR + TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TVAR + TVAR_2  +lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + TVAR + TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR +lag_log_gni_pc:RR_2+ TVAR + TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 +TVAR + TVAR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR +lag_log_gni_pc:RR_2 + lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +TVAR + TVAR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2  +lag_log_gni_pc:RR+ lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# sign, but changing sign concavity when adding adap

# rx

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "RX + RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "RX + RX_2  + lag_log_gni_pc:RX + lag_log_gni_pc:RX_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR  +RR_2 + RX + RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:RR_2 + RX + RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR + RX + RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:RR_2 + RX + RX_2 + lag_log_gni_pc:RX + lag_log_gni_pc:RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR + RX + RX_2 + lag_log_gni_pc:RX + lag_log_gni_pc:RX_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# sign only with adap 


# pet

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "PET + PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "PET + PET_2  +lag_log_gni_pc:PET + lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + PET + PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + PET + PET_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR +lag_log_gni_pc:RR_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+RR_2 + PET + PET_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR +lag_log_gni_pc:RR_2 + lag_log_gni_pc:PET + lag_log_gni_pc:PET_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# sign only with adap

# wd

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + lag_log_gni_pc:WD + lag_log_gni_pc:WD_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2  +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR +lag_log_gni_pc:RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+  WD + WD_2  +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2  +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd to keep, sign everywhere

# spi

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPI   + SPI_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPI    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPI   + lag_log_gni_pc:SPI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + SPI   + SPI_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR  + SPI   + SPI_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR  + SPI       "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + SPI   + SPI_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:RR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + SPI    +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# quadratic comp not sign


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + SPI    +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# not sign with adap


# spei

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPEI   + SPEI_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPEI    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPEI   + lag_log_gni_pc:SPEI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + SPEI   + SPEI_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR  + SPEI   + SPEI_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR  + SPEI       "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR +RR_2 + SPEI   + SPEI_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+lag_log_gni_pc:RR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + SPEI    +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# quadratic comp not sign


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + SPEI    +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR++lag_log_gni_pc:SPEI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# not sign with adap

########################

models<-list()

# keep wd, add others

# wd, spi
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+ WD + WD_2 + SPI +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# adap spi not sign here 


# wd, tvar
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +TVAR+TVAR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 + TVAR + TVAR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2  +lag_log_gni_pc:TVAR  + lag_log_gni_pc:TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + WD + WD_2 +TVAR+TVAR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 +  lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2  +lag_log_gni_pc:TVAR  + lag_log_gni_pc:TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# sign, when adap change sign

# wd, pet
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 + PET + PET_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 + lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +PET+PET_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:PET  + lag_log_gni_pc:PET_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# pet sign with adap, it overcomes wd when adap in iso3,dk errors



# wd, rx
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +RX+RX_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +RX+RX_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2 + lag_log_gni_pc:RX  + lag_log_gni_pc:RX_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# sign with adap


### all 
# wd, spi, tvar
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + TVAR + TVAR_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi, rx
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + RX + RX_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi, pet
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + PET + PET_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi, pet adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + PET + PET_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:PET  + lag_log_gni_pc:PET_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi, pet adap, rx
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + PET + PET_2 +RX+ lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:PET  + lag_log_gni_pc:PET_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi, pet adap, rx adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI + PET + PET_2 +RX+ lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:PET  + lag_log_gni_pc:PET_2 + lag_log_gni_pc:RX  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# not sign

# wd, spi adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# adap spi not sign here 

# wd, spi adap, pet
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
# models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# wd, spi adap, pet adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

#wd, spi, pet adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
#models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
# keep this 

# wd, spi adap, pet adap, tvar adap
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+ TVAR + TVAR_2 + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2 +lag_log_gni_pc:TVAR  + lag_log_gni_pc:TVAR_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


#names(models)<-c()

################################################################################
models<-list()

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2 + lag_log_gni_pc:WD +lag_log_gni_pc:WD_2  + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " SPI +  lag_log_gni_pc:SPI + TM + TM_2 + RR+ RR_2  + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TVAR + TVAR_2 +  lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2 + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " PET + PET_2 +  lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")




o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2 + SPI + lag_log_gni_pc:WD +lag_log_gni_pc:WD_2 + lag_log_gni_pc:SPI + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2  + SPI + PET + PET_2   + lag_log_gni_pc:WD +lag_log_gni_pc:WD_2 + lag_log_gni_pc:SPI +  lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
m_all<-m

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " WD + WD_2  + SPI + PET + PET_2  + TVAR + TVAR_2 + lag_log_gni_pc:WD +lag_log_gni_pc:WD_2 + lag_log_gni_pc:SPI +  lag_log_gni_pc:PET + lag_log_gni_pc:PET_2+  lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
m_all_tvar<-m


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "  SPI + PET + PET_2  + TVAR + TVAR_2 + lag_log_gni_pc:SPI +  lag_log_gni_pc:PET + lag_log_gni_pc:PET_2+  lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  + TM + TM_2 + RR+ RR_2 + + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")
m_all_tvar_no_wd<-m

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_selected_extremes.html")))
modelsummary(models,lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_selected_extremes_dk.html")))





################################################################################

### only some to highlight difference adap/no adap 

models<-list()


# extremes 

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI + PET + PET_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI + PET + PET_2 + TVAR + TVAR_2" # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI +PET + PET_2 + TVAR + TVAR_2 +
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI + lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# tp + extremes

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+  WD + WD_2 + SPI + PET + PET_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2+  WD + WD_2 + SPI + PET + PET_2 +TVAR + TVAR_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +TVAR + TVAR_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI +  lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2" # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# save

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes.html")))
modelsummary(models,lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_dk.html")))



##################################################################################
################################################################################

# adding extremes, lag selection based on different models, level mod

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





##################################################################################
### plot coefficient effects like kotz 2022

data<-data%>%group_by(gdlcode)%>%mutate(TM_sd=sd(TM), 
                                        RR_sd=sd(RR),
                                        WD_sd=sd(WD),
                                        SPI_sd=sd(SPI),
                                        PET_sd=sd(PET)
)

mean_sd_TM<-mean(unique(data$TM_sd))
mean_sd_RR<-mean(unique(data$RR_sd))
mean_sd_WD<-mean(unique(data$WD_sd))
mean_sd_SPI<-mean(unique(data$SPI_sd))
mean_sd_PET<-mean(unique(data$PET_sd))

### complete model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "  TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


######## plot, with conf intervals

lgnipc_values <- as.numeric(c(summary(data$lag_log_gni_pc, na.rm = TRUE)[2],summary(data$lag_log_gni_pc, na.rm = TRUE)[3],summary(data$lag_log_gni_pc, na.rm = TRUE)[5]) )
exp(lgnipc_values)

### pet
names<-c("PET", "PET:lag_log_gni_pc", "PET_2", "PET_2:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$PET,  na.rm = TRUE), max(data$PET, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # TM
           values * lgnipc_values[1],  # TM * l1_lgdppc
           values^2,              # TM_2
           (values^2) * lgnipc_values[1]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(values,                # TM
             values * lgnipc_values[2],  # TM * l1_lgdppc
             values^2,              # TM_2
             (values^2) * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(values,                # TM
             values * lgnipc_values[3],  # TM * l1_lgdppc
             values^2,              # TM_2
             (values^2) * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_pet<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+
  xlab("Potential Evapotranspiration")+ylab("HDI change (%)")+scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")



### wd 
names<-c("WD", "WD:lag_log_gni_pc", "WD_2", "WD_2:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$WD, na.rm = TRUE), max(data$WD, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # TM
           values * lgnipc_values[1],  # TM * l1_lgdppc
           values^2,              # TM_2
           (values^2) * lgnipc_values[1]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(values,                # TM
           values * lgnipc_values[2],  # TM * l1_lgdppc
           values^2,              # TM_2
           (values^2) * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(values,                # TM
             values * lgnipc_values[3],  # TM * l1_lgdppc
             values^2,              # TM_2
             (values^2) * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_wd<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+
  xlab("Wet Days (WD)")+ylab("HDI change (%)")+scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")




### spi
names<-c("SPI", "SPI:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$SPI, na.rm = TRUE), max(data$SPI, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                #
           values * lgnipc_values[1]) # 

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(values,                # TM
             values * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(values,                # TM
             values * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_spi<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+
  xlab("Drought Index (SPI)")+ylab("HDI change (%)")+
#labs(fill = "Income Per \nCapita Level")
 scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")

ggpubr::ggarrange(g_wd, g_pet, g_spi, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("diff_hdi_burke_extremes_by_income_damages.png")), width = 1800,
                 height = 500)


library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

### reduced model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI+ 
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coef(m)

######## plot, with conf intervals

### tm 
# Extract coefficients from the model
coefficients <- coef(m)[c(1,2,8,9)] #t coefs

# Extract the covariance matrix of the coefficients
cov_matrix <- vcov(m, )[c(1,2,8,9), c(1,2,8,9)] #t cov

fixed_l1_lgdppc <- as.numeric(summary(data$lag_log_gni_pc, na.rm = TRUE)[2])

# Generate a sequence of temperature values
values <- seq(min(data$TM, na.rm = TRUE), max(data$TM, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # TM
           values * fixed_l1_lgdppc,  # TM * l1_lgdppc
           values^2,              # TM_2
           (values^2) * fixed_l1_lgdppc) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- c("TM",  "TM:lag_log_gni_pc", "TM_2", "TM_2:lag_log_gni_pc")

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci  )
ggplot(damages)+geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci), alpha=0.2)+
  theme_bw()

plot(values, predicted_change, type = "l", col = "blue",
     ylim = range(c(lower_ci, upper_ci)),
     xlab = "Temperature (TM)", ylab = "Predicted HDI change (% points)"
)

# Add the confidence intervals to the plot
lines(values, lower_ci, col = "red", lty = 2)
lines(values, upper_ci, col = "red", lty = 2)


### rr
# Extract coefficients from the model
coefficients <- coef(m)[c(3,4,10,11)] #t coefs

# Extract the covariance matrix of the coefficients
cov_matrix <- vcov(m, )[c(3,4,10,11),c(3,4,10,11)] #t cov

fixed_l1_lgdppc <- as.numeric(summary(data$lag_log_gni_pc, na.rm = TRUE)[3])

# Generate a sequence of temperature values
values <- seq(min(data$RR, na.rm = TRUE), max(data$RR, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # 
           values * fixed_l1_lgdppc,  # 
           values^2,              # 
           (values^2) * fixed_l1_lgdppc) # 

# Column names must match the coefficients' names
colnames(X) <- c("RR",  "RR:lag_log_gni_pc", "RR_2", "RR_2:lag_log_gni_pc")

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

plot(values, predicted_change, type = "l", col = "blue",
     ylim = range(c(lower_ci, upper_ci)),
     xlab = "Rainfall (RR)", ylab = "Predicted HDI change (% points)"
)

# Add the confidence intervals to the plot
lines(values, lower_ci, col = "red", lty = 2)
lines(values, upper_ci, col = "red", lty = 2)


### wd
# Extract coefficients from the model
coefficients <- coef(m)[c(5,6,12,13)] # coefs

# Extract the covariance matrix of the coefficients
cov_matrix <- vcov(m, )[c(5,6,12,13),c(5,6,12,13)] #cov

fixed_l1_lgdppc <- as.numeric(summary(data$lag_log_gni_pc, na.rm = TRUE)[3])

# Generate a sequence of temperature values
values <- seq(min(data$WD, na.rm = TRUE), max(data$WD, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # 
           values * fixed_l1_lgdppc,  # 
           values^2,              # 
           (values^2) * fixed_l1_lgdppc) # 

# Column names must match the coefficients' names
colnames(X) <- c("WD",  "WD:lag_log_gni_pc", "WD_2", "WD_2:lag_log_gni_pc")

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- predicted_change - 1.96 * standard_errors
upper_ci <- predicted_change + 1.96 * standard_errors

plot(values, predicted_change, type = "l", col = "blue",
     ylim = range(c(lower_ci, upper_ci)),
     xlab = "Wet Days (WD)", ylab = "Predicted HDI change (% points)"
)

# Add the confidence intervals to the plot
lines(values, lower_ci, col = "red", lty = 2)
lines(values, upper_ci, col = "red", lty = 2)



### spi
# Extract coefficients from the model
coefficients <- coef(m)[c(7)] # coefs

# Extract the covariance matrix of the coefficients
cov_matrix <- vcov(m, )[c(7),c(7)] #cov

fixed_l1_lgdppc <- as.numeric(summary(data$lag_log_gni_pc, na.rm = TRUE)[3])

# Generate a sequence of temperature values
values <- seq(min(data$SPI, na.rm = TRUE), max(data$SPI, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values) # 

# Column names must match the coefficients' names
colnames(X) <- c("SPI")

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- predicted_change - 1.96 * standard_errors
upper_ci <- predicted_change + 1.96 * standard_errors

plot(values, predicted_change, type = "l", col = "blue",
     ylim = range(c(lower_ci, upper_ci)),
     xlab = "Drought Index (SPI)", ylab = "Predicted HDI change (% points)"
)

# Add the confidence intervals to the plot
lines(values, lower_ci, col = "red", lty = 2)
lines(values, upper_ci, col = "red", lty = 2)



################################################################################

### plot in map

### reduced model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 +SPI+ 
lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2   "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")



coefs<-m$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), tm_mean_damage=NA, rr_mean_damage=NA,  
                          spi_mean_damages=NA, 
                          wd_mean_damages=NA
                          
)

for (r in unique(data$gdlcode)){
  
  laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)

  ### mean on years
  tm<-mean(data$TM[data$gdlcode == r], na.rm = TRUE)
  rr<-mean(data$RR[data$gdlcode == r], na.rm = TRUE)
  spi<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)
  
  tm_plus1sd<-mean(data$TM[data$gdlcode == r], na.rm = TRUE)+sd(data$TM[data$gdlcode == r], na.rm = TRUE)
  rr_plus1sd<-mean(data$RR[data$gdlcode == r], na.rm = TRUE)+sd(data$RR[data$gdlcode == r], na.rm = TRUE)
  spi_plus1sd<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)+sd(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd_plus1sd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)+sd(data$WD[data$gdlcode == r], na.rm = TRUE)
  
  
  # tm damages
  mean_damages[mean_damages$gdlcode==r, c("tm_mean_damage") ]<-
    (tm_plus1sd*coefs["TM"] + (tm_plus1sd^2)*coefs["TM_2"] +
       tm_plus1sd*laghdi*coefs["TM:lag_log_gni_pc"] + (tm_plus1sd^2)*laghdi*coefs["TM_2:lag_log_gni_pc"]) - 
    (tm*coefs["TM"] + (tm^2)*coefs["TM_2"] +
    tm*laghdi*coefs["TM:lag_log_gni_pc"] + (tm^2)*laghdi*coefs["TM_2:lag_log_gni_pc"])
  
  
  # rr damages
  mean_damages[mean_damages$gdlcode==r, c("rr_mean_damage") ]<-
    (rr_plus1sd*coefs["RR"] + (rr_plus1sd^2)*coefs["RR_2"] +
       rr_plus1sd*laghdi*coefs["RR:lag_log_gni_pc"] +(rr_plus1sd^2)*laghdi*coefs["RR_2:lag_log_gni_pc"]) -
    (rr*coefs["RR"] + (rr^2)*coefs["RR_2"] +
       rr*laghdi*coefs["RR:lag_log_gni_pc"] +(rr^2)*laghdi*coefs["RR_2:lag_log_gni_pc"]) 
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]<-
    (wd_plus1sd*coefs["WD"] + (wd_plus1sd^2)*coefs["WD_2"] +
       wd_plus1sd*laghdi*coefs["WD:lag_log_gni_pc"] + (wd_plus1sd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] ) - 
    (wd*coefs["WD"] + (wd^2)*coefs["WD_2"] +
    wd*laghdi*coefs["WD:lag_log_gni_pc"] + (wd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] )
  
  
  # spi damages
  mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]<-
    (spi_plus1sd*coefs["SPI"] ) - spi*coefs["SPI"] 
  
  
  
  ## sum all
  #mean_damages[mean_damages$gdlcode==r, c("all_mean_damage") ]<-
  #  mean_damages[mean_damages$gdlcode==r, c("tm_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("rr_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]
  
   
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g1<-ggplot(mean_damages)+geom_sf( aes(fill=tm_mean_damage))+ scale_fill_gradient2()
g1
#ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_tm_adap_hdi_mean.png")), g, width=15, height=10)

g2<-ggplot(mean_damages)+geom_sf( aes(fill=rr_mean_damage))+ scale_fill_gradient2()
g2
#ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_rr_adap_hdi_mean.png")), g, width=15, height=10)

g3<-ggplot(mean_damages)+geom_sf( aes(fill=spi_mean_damage))+ scale_fill_gradient2()
g3
#ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_spi_adap_hdi_mean.png")), g, width=15, height=10)

g4<-ggplot(mean_damages)+geom_sf( aes(fill=wd_mean_damage))+ scale_fill_gradient2()
g4
#ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_wd_adap_hdi_mean.png")), g, width=15, height=10)

ggpubr::ggarrange(g1,g2,g3,g4, align="v") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("diff_hdi_burke_extremes_small_mean_plus1sd_damages.png")), width = 2100,
                   height = 700)


### complete model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          spi_mean_damages=NA, 
                          wd_mean_damages=NA,
                          pet_mean_damaeges=NA
                          
)

for (r in unique(data$gdlcode)){
  
  laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  spi<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)
  pet<-mean(data$PET[data$gdlcode == r], na.rm = TRUE)
  
  spi_plus1sd<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)+sd(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd_plus1sd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)+sd(data$WD[data$gdlcode == r], na.rm = TRUE)
  pet_plus1sd<-mean(data$PET[data$gdlcode == r], na.rm = TRUE)+sd(data$PET[data$gdlcode == r], na.rm = TRUE)
  
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]<-
    ((wd_plus1sd^2)*coefs["WD_2"] +
        (wd_plus1sd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] ) - 
    ((wd^2)*coefs["WD_2"] +
        (wd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] )
  
  
  # spi damages
  mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]<-
    (spi_plus1sd*coefs["SPI"] + (spi_plus1sd)*laghdi*coefs["SPI:lag_log_gni_pc"] ) - 
    (spi*coefs["SPI"]+ (spi)*laghdi*coefs["SPI:lag_log_gni_pc"] )
  
  # pet damages
  mean_damages[mean_damages$gdlcode==r, c("pet_mean_damage") ]<-
    (pet_plus1sd*coefs["PET"] + (pet_plus1sd^2)*coefs["PET_2"] +
       pet_plus1sd*laghdi*coefs["PET:lag_log_gni_pc"] +(pet_plus1sd^2)*laghdi*coefs["PET_2:lag_log_gni_pc"]) -
    (pet*coefs["PET"] + (pet^2)*coefs["PET_2"] +
       pet*laghdi*coefs["PET:lag_log_gni_pc"] +(pet^2)*laghdi*coefs["PET_2:lag_log_gni_pc"]) 
  
  
  ## sum all
  #mean_damages[mean_damages$gdlcode==r, c("all_mean_damage") ]<-
  #  mean_damages[mean_damages$gdlcode==r, c("tm_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("rr_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]+
  #  mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g1<-ggplot(mean_damages)+geom_sf( aes(fill=wd_mean_damage))+theme_bw()+  scale_fill_gradient2() + labs(fill="HDI change (%)") + ggtitle("Marginal effect of WD") + theme(plot.title = element_text(hjust = 0.5))
#g1
ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_mean_plus1sd_damages.png")), g1, width=15, height=10)



g2<-ggplot(mean_damages)+geom_sf( aes(fill=pet_mean_damage))+theme_bw()+ scale_fill_gradient2() +labs(fill="HDI change (%)")+ ggtitle("Marginal effect of PET")+ theme(plot.title = element_text(hjust = 0.5))
ggsave(filename=file.path(out_dir, paste0(o,"_burke_pet_mean_plus1sd_damages.png")), g2, width=15, height=10)


g3<-ggplot(mean_damages)+geom_sf( aes(fill=spi_mean_damage))+theme_bw() + scale_fill_gradient2() +labs(fill="HDI change (%)") + ggtitle("Marginal effect of SPI")+ theme(plot.title = element_text(hjust = 0.5))
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mean_plus1sd_damages.png")), g3, width=15, height=10)


ggpubr::ggarrange(g1,g2,g3, ncol=1, nrow=3,  common.legend=TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("diff_hdi_burke_extremes_mean_plus1sd_damages.png")), width =700,
                   height = 2100)
# g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damage))+ scale_fill_gradient2()
# ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_all_adap_hdi_mean.png")), g, width=15, height=10)






##################################################################################

### compare components

models_comp<-list()

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_income_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_edu_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_health_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

names(models_comp)<-c("diff_hdi","diff_income_index", "diff_edu_index",   "diff_health_index")

modelsummary(models_comp, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models_comp, vcov=lapply(models_comp, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models_comp, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_all_comp.html")))
modelsummary(models_comp,lapply(models_comp, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_all_comp_dk.html")))


jtools::plot_summs(models_comp, omit.coefs = c("TM", "TM_2", "RR", "RR_2", "TM:lag_log_gni_pc", "TM_2:lag_log_gni_pc", "RR:lag_log_gni_pc", "RR_2:lag_log_gni_pc"))



### plot, for spi, the different effects of a standard dev increase of the variables, in some specific points of high, middle, low income 

data_mean_income<- unique(data %>% group_by(gdlcode) %>% mutate(mean_log_gni_pc=mean(log_gni_pc, na.rm = TRUE)) %>%select(iso3, gdlcode, mean_log_gni_pc))
incomes_plot<-inner_join(gdl_shape_file, data_mean_income)
g<-ggplot(incomes_plot)+geom_sf( aes(fill=mean_log_gni_pc))+theme_bw()+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, paste0("mean_log_incomes.png")), g, width=15, height=10)

s<-summary(data_mean_income$mean_log_gni_pc)
data_mean_income[which(data_mean_income$mean_log_gni_pc==s[1]),]
data_mean_income[which.min(abs(data_mean_income$mean_log_gni_pc - s[2])),]
data_mean_income[which.min(abs(data_mean_income$mean_log_gni_pc - s[3])),]
data_mean_income[which.min(abs(data_mean_income$mean_log_gni_pc - s[5])),]
data_mean_income[which(data_mean_income$mean_log_gni_pc==s[6]),]
rep_codes<-c("MOZr102", "SDNr111" , "ALBr202", "HRVr102", "USAr109")

data_small<-data[which(data$gdlcode %in% rep_codes), ]
data_small_mean_sd<- unique(data_small %>% group_by(gdlcode) %>% mutate(mean_pet = mean(PET, na.rm=TRUE), 
                                                                        mean_wd = mean(WD, na.rm=TRUE), 
                                                                        mean_spi = mean(SPI, na.rm=TRUE), 
                                                                        mean_pet_plus1sd = mean(PET, na.rm=TRUE) + sd(PET, na.rm=TRUE), 
                                                                        mean_wd_plus1sd = mean(WD, na.rm=TRUE)+ sd(WD, na.rm=TRUE), 
                                                                        mean_spi_plus1sd = mean(SPI, na.rm=TRUE)+ sd(SPI, na.rm=TRUE),
                                                                        mean_log_gni_pc= mean(log_gni_pc, na.rm=TRUE)) %>% 
                              select(gdlcode, mean_pet, mean_wd, mean_spi,mean_pet_plus1sd,  mean_wd_plus1sd, mean_spi_plus1sd, mean_log_gni_pc)
                              
                            )

##### hdi
m<-models_comp[["diff_hdi"]]
coef(m)


### pet
coefficients <- coef(m)[c("PET", "PET_2", "PET:lag_log_gni_pc", "PET_2:lag_log_gni_pc")] # coefs

fixed_l1_lgdppc <- data_small_mean_sd$mean_log_gni_pc[1]

# Generate a sequence of values
values <- data_small_mean_sd$mean_pet_plus1sd

# Create a matrix for the independent variables
X <- cbind(values,                # 
           values^2,              # 
           values * fixed_l1_lgdppc,  # 
           (values^2) * fixed_l1_lgdppc) # 


predicted_change <- X %*% coefficients




################################################################################

# compare, with different data, the gdp comp (table, plots)

# data
data_gdp<-read_csv("data/gdp_data/data_gdp_dose_climate_pop_weight_1979_2019.csv")

o<-"dlgrp_pc_usd_2015" 
i<-"GID_1 + year +GID_0[year]+GID_0[year^2]"
pan_id<- c("GID_1", "year")
r <- "TM + TM_2 + RR+ RR_2 +RR:TM+ WD + WD_2 + SPI +lag_grp_pc_usd:TM + lag_grp_pc_usd:TM_2 + lag_grp_pc_usd:RR  +lag_grp_pc_usd:RR_2+ lag_grp_pc_usd:WD  + lag_grp_pc_usd:WD_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gdp_full<- fixest::feols(f, data_gdp, panel.id=pan_id)
summary(mod_gdp_full)
summary(mod_gdp_full, vcov = ~ GID_0)
summary(mod_gdp_full, vcov = "DK")

o<-"dlgrp_pc_usd_2015" 
i<-"GID_1 + year +GID_0[year]+GID_0[year^2]"
pan_id<- c("GID_1", "year")
r <- "TM + TM_2 + RR+ RR_2 +RR:TM+ WD + WD_2 + SPI +lag_grp_pc_usd:TM + lag_grp_pc_usd:TM_2 + lag_grp_pc_usd:RR  +lag_grp_pc_usd:RR_2+ lag_grp_pc_usd:WD  + lag_grp_pc_usd:WD_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gdp_small<- fixest::feols(f, data_gdp[which(data_gdp$year <= 2020 & data_gdp$year >= 1990),], panel.id=pan_id)
summary(mod_gdp_small)
summary(mod_gdp_small, vcov = ~ GID_0)
summary(mod_gdp_small, vcov = "DK")

o <- "diff_income_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
pan_id<- c("gdlcode", "year")
r <- "TM + diff_TM + TM:diff_TM + RR+ diff_RR  +RR:diff_RR+ RR:TM+WD + WD_2 +SPI +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2  "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gni_small<- fixest::feols(f, data, panel.id=pan_id)
summary(mod_gni_small)
summary(mod_gni_small, vcov = ~ iso3)
summary(mod_gni_small, vcov = "DK")

mods<-list(mod_gdp_full, mod_gdp_small, mod_gni_small )
mods
modelsummary(mods,  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# different, here we also use different countries 

### just keep common countries
common_countries<-intersect(data$iso3, data_gdp$GID_0)

o<-"dlgrp_pc_usd_2015" 
i<-"GID_1 + year +GID_0[year]+GID_0[year^2]"
pan_id<- c("GID_1", "year")
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 + SPI +lag_grp_pc_usd:TM + lag_grp_pc_usd:TM_2 + lag_grp_pc_usd:RR  +lag_grp_pc_usd:RR_2+ lag_grp_pc_usd:WD  + lag_grp_pc_usd:WD_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gdp_full<- fixest::feols(f, data_gdp[which(data_gdp$GID_0 %in% common_countries), ], panel.id=pan_id)
summary(mod_gdp_full)
summary(mod_gdp_full, vcov = ~ GID_0)
summary(mod_gdp_full, vcov = "DK")

o<-"dlgrp_pc_usd_2015" 
i<-"GID_1 + year +GID_0[year]+GID_0[year^2]"
pan_id<- c("GID_1", "year")
r <- "TM + TM_2 + RR+ RR_2 + WD + WD_2 + SPI +lag_grp_pc_usd:TM + lag_grp_pc_usd:TM_2 + lag_grp_pc_usd:RR  +lag_grp_pc_usd:RR_2+ lag_grp_pc_usd:WD  + lag_grp_pc_usd:WD_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gdp_small<- fixest::feols(f, data_gdp[which(data_gdp$year <= 2020 & data_gdp$year >= 1990 & data_gdp$GID_0 %in% common_countries),], panel.id=pan_id)
summary(mod_gdp_small)
summary(mod_gdp_small, vcov = ~ GID_0)
summary(mod_gdp_small, vcov = "DK")

o <- "diff_income_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
pan_id<- c("gdlcode", "year")
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_gni_small<- fixest::feols(f, data[which(data$iso3 %in% common_countries), ], panel.id=pan_id)
summary(mod_gni_small)
summary(mod_gni_small, vcov = ~ iso3)
summary(mod_gni_small, vcov = "DK")

mods<-list(mod_gdp_full, mod_gdp_small, mod_gni_small )
mods
modelsummary(mods,  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# different like before

hist(data_gdp$lgrp_pc_usd_2015)
hist(data_gdp[which(data_gdp$year <= 2020 & data_gdp$year >= 1990),]$lgrp_pc_usd)
hist(data_gdp[which(data_gdp$year <= 2020 & data_gdp$year >= 1990 & data_gdp$GID_0 %in% common_countries),]$lgrp_pc_usd)

hist(data[which(data$iso3 %in% common_countries),]$log_gni_pc)
hist(data$log_gni_pc)

################################################################################

### robustness to 1 at the time erase of each country 

### complete model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "  TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

rob_table <- data.frame(coef=c(coef(m)), se =sqrt(diag(vcov(m))))

#rob_table[,paste0("run_",1 )]<-0

for ( r in unique(data$iso3)){
  mod<-fixest::feols(f, data[which(data$iso3 != r), ], panel.id=pan_id)
  rob_table[,paste0("no_",r )]<-c(coef(mod))
  
}

vars_to_plot<-c("WD", "WD_2", "PET", "PET_2", "SPI", "WD:lag_log_gni_pc", "WD_2:lag_log_gni_pc", "PET:lag_log_gni_pc", "PET_2:lag_log_gni_pc", "SPI:lag_log_gni_pc")

wd_sd<-mean(aggregate(data$WD, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
pet_sd<-mean(aggregate(data$PET, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
spi_sd<-mean(aggregate(data$SPI, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
hdi_sd<-mean(aggregate(data$diff_hdi, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )

rob_table$variable<-rownames(rob_table)
rob_table$up<-rob_table$coef+1.96*rob_table$se
rob_table$low<-rob_table$coef-1.96*rob_table$se

rob_table<-rob_table[which(rob_table$variable %in% vars_to_plot),]
rob_table<-data.frame(t(rob_table))
t<-as.numeric(wd_sd/hdi_sd)
rob_table[1,]*
rob_table[1,]<-apply(rob_table[1,], 2,function(x) {x*t})

ggplot(rob_table)+geom_point(aes(x=variable, y=coef))+theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ 
  geom_segment(aes(x=variable, xend=variable, y=low, yend=up), 
               size=1,  colour="black")
