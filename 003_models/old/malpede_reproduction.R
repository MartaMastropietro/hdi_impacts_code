### malpede reproduction, with population weighted data

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
out_dir<-"output/models/models_malpede"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_climate_gdl_1990_2021.csv")
colnames(data)

gdl_shape_file <- sf:: st_read("data/hdi_data/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)


### setup 


data<-data[which(data$year<=2015), ]

summary(data)

# indeces specifics 

pan_id<-c('gdlcode', 'year')

idx_reg<- "gdlcode"
idx_small<- "gdlcode + year"
idx_m<- "gdlcode + year + iso3[year] "

out_var="lead_diff_hdi"

### try to reproduce as its written
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  + hdi  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   + hdi   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   + hdi  ", "|" , idx_m ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)

models<-list(mod_0, mod_1, mod_2)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_tp_dk.html"))


### it appears to be different, need to look more into it 


### try to reproduce as it should be if it followed diffenb and burke

out_var="diff_hdi"
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_m ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)

models<-list(mod_0, mod_1, mod_2)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod0_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod0_tp_dk.html"))

### try to reproduce without hdi

out_var="lead_diff_hdi"
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2   ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  ", "|" , idx_m ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)

models<-list(mod_0, mod_1, mod_2)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod1_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod1_tp_dk.html"))

### try to reproduce with hdi, but difference of current year

out_var="diff_hdi"
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2 +hdi ", "|" , idx_reg ))
mod_0<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  +hdi ", "|" , idx_small ))
mod_1<-fixest::feols(f, data, panel.id=pan_id)
f <- as.formula( paste(out_var ,"~", "RR + RR_2+ TM+ TM_2  +hdi", "|" , idx_m ))
mod_2<-fixest::feols(f, data, panel.id=pan_id)

models<-list(mod_0, mod_1, mod_2)

#summary classic
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod1_tp.html"))

fvcov_dk<-function(x) vcov(x, "DK")
vcov_dk<- lapply(models, FUN=fvcov_dk)
modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors") , output=file.path(out_dir, "models_malpede_mod1_tp_dk.html"))

