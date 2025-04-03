### try adap + spi model, but differentiating by hdi level
### todo: try final models also
### todo: differentiate by income level, see changes

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
out_dir<-"output/models/hdi_and_comp_by_hdi_category_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
colnames(data)

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

### hdi categorizaton
HDR21_22_Composite_indices_complete_time_series <- read_csv("data/hdi_data/downloaded/undp_national/HDR21-22_Composite_indices_complete_time_series.csv")
hdi_cat<-unique(HDR21_22_Composite_indices_complete_time_series[, c(1,3)])
data<-inner_join(data, hdi_cat)


###


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)

colnames(data)

# out variables 
out_variables<-c("diff_hdi", "diff_health_index", "diff_edu_index", "diff_income_index", "gr_log_gni_pc")
hdicodes<-unique(data$hdicode)
data$hdicode[which(is.na(data$hdicode))]<-"Low" # somalia only
hdicodes<-unique(data$hdicode) # "Low"   "Medium"   "High"  "Very High"

pan_id<-c('gdlcode', 'year')

################################################################################

# base model: autoregressive, adaptation with hdi, spi

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2  + SPI +
      lag_hdi:TM + lag_hdi:TM_2 + lag_hdi:RR +lag_hdi:RR_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", paste0("lag_",o),"+", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

###

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
rhs <- "TM + TM_2+ SPI +
      lag_hdi:TM + lag_hdi:TM_2 "
for (o in out_variables){
  mods<-list()
  for (code in hdicodes){
    
    temp_data<-data[which(data$hdicode== code), ]
    f <- as.formula(paste( o, "~", paste0("lag_",o),"+", rhs, "|" ,i ))
    m <- fixest::feols(f, temp_data, panel.id=pan_id)
    summary(m)
    mods[[length(mods)+1]]<-m
    
    ### plot 
    coefs<-m$coefficients
    
    
  }
  
  modelsummary(mods, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
               output=file.path(out_dir, paste0(o, "_", "ar_burke_spi_all_categories.html")))
  modelsummary(mods, lapply(mods, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
               output=file.path(out_dir, paste0(o, "_", "ar_burke_spi_all_categories_dk.html")))
  modelsummary(mods, lapply(mods, FUN=fvcov_iso), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
               output=file.path(out_dir, paste0(o, "_", "ar_burke_spi_all_categories_iso.html")))
  
  
}
