### test different variables for level, growth model -> divide scripts?

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
out_dir<-"output/models/hdi_tp_extr_burke_models"
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


################################################################################
### extremes one at the time 
################################################################################

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
r <- "  SPI + PET + PET_2  + TVAR + TVAR_2 + lag_log_gni_pc:SPI +  lag_log_gni_pc:PET + lag_log_gni_pc:PET_2+  lag_log_gni_pc:TVAR + lag_log_gni_pc:TVAR_2  + TM + TM_2 + RR+ RR_2  + lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2 "
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

### try lasso with pre demeaning, and always keeping TM, TM_2, RR, RR_2

library(glmnet)

keep_vars<-c("TM", "TM_2", "RR", "RR_2")
candidate_vars<-c("TVAR", "RX", "WD", "PET", "PEXT", "SPI", "HW", 
                  "TVAR_2", "RX_2", "WD_2", "PET_2", "PEXT_2", "SPI_2",  "HW_2")

# pre-run of fixed eff
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "1"
f <- as.formula(paste( o, "~", r, "|" ,i ))
mod_fe <- fixest::feols(f, data, panel.id=pan_id)
summary(mod_fe)
res_fe<-residuals(mod_fe, na.rm=FALSE)

all_vars <- c(keep_vars, candidate_vars)
X <- (model.matrix(as.formula(paste("~", paste(all_vars, collapse = "+"), "-1")), data = data))

# Use penalty.factor to ensure the keep_vars are not penalized (set penalty factor = 0 for them)
penalty_factor <- c(rep(0, length(keep_vars)), rep(1, length(candidate_vars)))

# LASSO
# erase rows where residuals na
X <- X[grep("\\d", res_fe),]
res_fe <- res_fe[grep("\\d", res_fe)]
# normalise 
X=scale(X, center = FALSE, scale=TRUE)

# cross-validation to select the best lambda (tuning parameter for LASSO)
cv_model <- glmnet::cv.glmnet(x=X, y=res_fe, alpha = 1, family="gaussian", penalty.factor = penalty_factor)
plot(cv_model)

lasso_model <- glmnet(x=X, y=res_fe, alpha = 1, family="gaussian",  penalty.factor = penalty_factor, lambda = cv_model$lambda.min)
coef(lasso_model)

# Step 6: Extract selected variables (non-zero coefficients at the best lambda)
selected_vars <- rownames(coef(cv_model, s = "lambda.min"))[coef(cv_model, s = "lambda.min") != 0]

# Step 7: Refit the fixed effects model with only the selected variables
final_formula <- as.formula(paste(dep_var, "~", paste(selected_vars, collapse = "+"), "|", fe_formula))
final_model <- feols(final_formula, data = data)


################################################################################
### try automatic exhaustive search, with three different metrics, and compare 
### each variable linear + squared
### with, without adap 

