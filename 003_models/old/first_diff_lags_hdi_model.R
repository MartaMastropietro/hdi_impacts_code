### model following lags spec + level on level with autoregressive part (in first diff estimation)

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
out_dir<-"output/models/first_diff_lags_hdi_models"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### data
data<- read_csv("output/data_hdi_hazards_gdl_pop_weight_1990_2021.csv")
colnames(data)

gdl_shape_file <- sf:: st_read("data/hdi_data/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

### setup 

# variables 
extr_var_names<-c("HWd_DUR","HWd_LARG", "HWd_EV",    # heatwaves
                  "CWn_DUR","CWn_LARG", "CWn_EV",    # coldwaves
                  "TC_VCD", "TC_VHD",                # thermal comfort
                  # "DDDs_HDD", "DDDs_CDD","DDDs_EDD", # degree-days
                  "SPEI_DUR", "SPEI_SEV",            # metereological drought 
                  "SPI_DUR", "SPI_SEV",              # metereological drought
                  "RRX_EXTR", "RRX_RR5d",            # rainfall extremes
                  # "CP_HW_DR", "CP_HW_RX",            # compound 
                  # "CP_CW_DR", "CP_CW_RX",            # compound    
                  # "CP_DR_RX", "CP_RX_WX",            # compound
                  "CL_PET"
                  #"CL_RR", "CL_TM" ,       # arid areas
                  # "CLCS_AI", "CLCS_AIv", "CLCS_KG",  # arid areas
                  # "GS_GSL", "GS_GSLf",               # growing season 
                  # "GS_GDD", "GS_GDDg"                # growing season 
)

out_variables_gr<-c("gr_hdi", "gr_health_index", "gr_edu_index", "gr_income_index", "gr_log_gni_pc")
out_variables_diff<-c("diff_hdi", "diff_health_index", "diff_edu_index", "diff_income_index", "diff_log_gni_pc")
out_variables_lev<-c("hdi", "health_index", "edu_index", "income_index", "log_gni_pc")

pan_id<-c('gdlcode', 'year')

idx_reg<-"gdlcode"
idx_small<-"gdlcode + year"
idx_c<-"gdlcode + year + iso3[year]"
idx<-"gdlcode + year + gdlcode[year] + gdlcode[year^2]"

indeces<-list(idx_reg, idx_small, idx_c,  idx)

colnames(data)

################################################################################

### initial model, only temp, prec

##
right_list_hdi<- c(
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2",  # level on level model: estimated as a first diff -> istantaneous effect
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi", # same with autoregressive comp
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",  # + adap
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",  # + dependence on base climate
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",    # + dependence on base climate + adap
  "diff_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",

  #                   
                   
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2", # diff on level -> permanent effect on annual difference 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2  + lag_diff_hdi", # same with autoregressive comp
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_diff_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
            
  # all   
  
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2" , 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "diff_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_diff_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2"
  
  )


models_hdi<-list()
for (i in indeces){
  for (r in right_list_hdi){
    f <- as.formula(paste(r, "|" ,i ))
    models_hdi[[i]][[r]] <- fixest::feols(f, data, panel.id=pan_id)
  }
}

modelsummary(models_hdi[[4]][1:8], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_lev_lev.html"))
modelsummary(models_hdi[[4]][9:16], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_diff_lev.html"))
modelsummary(models_hdi[[4]][17:24], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_diff_all.html"))

fvcov_dk<-function(x) vcov(x, "DK")
modelsummary(models_hdi[[4]][1:8],lapply(models_hdi[[4]][1:8], FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_lev_lev_dk.html"))
modelsummary(models_hdi[[4]][9:16], lapply(models_hdi[[4]][9:16], FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_diff_lev_dk.html"))
modelsummary(models_hdi[[4]][17:24],lapply(models_hdi[[4]][17:24], FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_tp_diff_all_dk.html"))

#####


right_list_hdi_gr <- c(#
  
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2",  # level (of log response) on level model: estimated as a first diff -> istantaneous effect
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi", # same with autoregressive comp
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",  
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",  
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",  
  "gr_hdi ~ diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",
  
  #                   
  
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2", # diff on level -> permanent effect on annual difference 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2  + lag_gr_hdi", # same with autoregressive comp
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_gr_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  
  # all   
  
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2" , 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ",
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2", 
  "gr_hdi ~ TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_gr_hdi + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2"
  
)



models_hdi_gr<-list()
for (i in indeces){
  for (r in right_list_hdi_gr){
    f <- as.formula(paste(r, "|" ,i ))
    models_hdi_gr[[i]][[r]] <- fixest::feols(f, data, panel.id=pan_id)
  }
}

modelsummary(models_hdi_gr[[4]][1:8], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_lev_lev.html"))
modelsummary(models_hdi_gr[[4]][9:16], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_diff_lev.html"))
modelsummary(models_hdi_gr[[4]][17:24], estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_diff_all.html"))

fvcov_dk<-function(x) vcov(x, "DK")
modelsummary(models_hdi_gr[[4]][1:8],lapply(models_hdi_gr[[4]][1:8], FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_lev_lev_dk.html"))
modelsummary(models_hdi_gr[[4]][9:16], lapply(models_hdi_gr[[4]][9:16], FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_diff_lev_dk.html"))
modelsummary(models_hdi_gr[[4]][17:24],lapply(models_hdi_gr[[4]][17:24], FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_diff_all_dk.html"))


### recalculate one by one to test stuff

# # diff, withouth and with autoregressive comp -> with level temp or diff temp, to assume persistent vs instantaneous effects 
# 
# out_var="diff_hdi"
# 
# f <- as.formula( paste(out_var ,"~", "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx_reg ))
# mod_diff_lev_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx_small ))
# mod_diff_lev_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx ))
# mod_diff_lev_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_lev_0_reg, mod_diff_lev_0_small, mod_diff_lev_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"))
# 
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx_reg ))
# mod_diff_lev_ar_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx_small ))
# mod_diff_lev_ar_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "TM_YY + TM_YY_2 + RR_YY + RR_YY_2  ", "|" , idx ))
# mod_diff_lev_ar_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_lev_ar_0_reg, mod_diff_lev_ar_0_small, mod_diff_lev_ar_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# f <- as.formula( paste(out_var ,"~", "TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2  ", "|" , idx ))
# mod_diff_lev_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "TM_YY + TM_YY_2 + RR_YY + RR_YY_2 + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2   ", "|" , idx ))
# mod_diff_lev_ar_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# 
# 
# # diff cl
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 ", "|" , idx_reg ))
# mod_diff_diff_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 ", "|" , idx_small ))
# mod_diff_diff_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2", "|" , idx ))
# mod_diff_diff_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_diff_0_reg, mod_diff_diff_0_small, mod_diff_diff_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"),fmt = fmt_sprintf("%.5e"))
# 
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2  ", "|" , idx_reg ))
# mod_diff_diff_ar_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 ", "|" , idx_small ))
# mod_diff_diff_ar_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2  ", "|" , idx ))
# mod_diff_diff_ar_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_diff_ar_0_reg, mod_diff_diff_ar_0_small, mod_diff_diff_ar_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:diff_TM_YY + lag_log_gni_pc:diff_TM_YY_2 + lag_log_gni_pc:diff_RR_YY + lag_log_gni_pc:diff_RR_YY_2", "|" , idx ))
# mod_diff_diff_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + lag_log_gni_pc:diff_TM_YY + lag_log_gni_pc:diff_TM_YY_2 + lag_log_gni_pc:diff_RR_YY + lag_log_gni_pc:diff_RR_YY_2", "|" , idx ))
# mod_diff_diff_ar_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# 
# 
# # diff cl + interaction
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", "|" , idx_reg ))
# mod_diff_diff_int_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY", "|" , idx_small ))
# mod_diff_diff_int_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY ", "|" , idx ))
# mod_diff_diff_int_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_diff_int_0_reg, mod_diff_diff_int_0_small, mod_diff_diff_int_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY  ", "|" , idx_reg ))
# mod_diff_diff_int_ar_0_reg<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY  ", "|" , idx_small ))
# mod_diff_diff_int_ar_0_small<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY  ", "|" , idx ))
# mod_diff_diff_int_ar_0<-fixest::feols(f, data, panel.id=pan_id)
# modelsummary(list(mod_diff_diff_int_ar_0_reg, mod_diff_diff_int_ar_0_small, mod_diff_diff_int_ar_0), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# f <- as.formula( paste(out_var ,"~", "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:diff_TM_YY + lag_log_gni_pc:diff_TM_YY_2 + lag_log_gni_pc:diff_RR_YY + lag_log_gni_pc:diff_RR_YY_2", "|" , idx ))
# mod_diff_diff_int_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# f <- as.formula( paste(out_var ,"~", paste0("lag_",out_var, "+") , "diff_TM_YY + diff_TM_YY_2 + diff_RR_YY + diff_RR_YY_2 + diff_TM_YY:TM_YY + diff_RR_YY:RR_YY + lag_log_gni_pc:diff_TM_YY + lag_log_gni_pc:diff_TM_YY_2 + lag_log_gni_pc:diff_RR_YY + lag_log_gni_pc:diff_RR_YY_2  ", "|" , idx ))
# mod_diff_diff_int_ar_adap_0<-fixest::feols(f, data, panel.id=pan_id)
# 
# 
# models<-list(mod_diff_lev_0, mod_diff_lev_ar_0, mod_diff_lev_adap_0, mod_diff_lev_ar_adap_0, mod_diff_diff_0, mod_diff_diff_ar_0 ,mod_diff_diff_adap_0, mod_diff_diff_ar_adap_0, mod_diff_diff_int_0, mod_diff_diff_int_ar_0 , mod_diff_diff_int_adap_0, mod_diff_diff_int_ar_adap_0 )
# 
# # display the most complete ones
# modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# fvcov_dk<-function(x) vcov(x, "DK")
# vcov_dk<- lapply(models, FUN=fvcov_dk)
# modelsummary(models,vcov=vcov_dk, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
# 
# modelsummary(models,vcov=~iso3, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))


### country, time fe + time varying regional trends test, for initial model
### same complete model + lags, only temp prec 

# add interaction with mean vars in past? how do we interpret this? change on change, but also depending on mean regional conditions
# idea, also add adaptations interaction with last year gdp? yes, but no lags for this, we interpret it as persistent (?) 

# for each base model, we add lags, save 

##############################################################################


# no extremes, lag selection based on different models

varns=c('diff_TM_YY','diff_RR_YY')
modns=c('TM_YY','RR_YY')
adap=c('lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(modns)){ # works better with level interaction
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 




##### first try, no interaction vars 
NL=10

BICs=c(0)
AICs=c(0)
wr2s=c(0)
Ns=c(0)

out_dir_lag_no_int_no_extr<-"output/models/first_diff_lags_hdi_models/lag_selection_no_int_no_extr"
if(!dir.exists(out_dir_lag_no_int_no_extr)){dir.create(out_dir_lag_no_int_no_extr)}

out_dir_temp<-out_dir_lag_no_int_no_extr

for (b in 1:length(varns)){
  
  for (r in 1:NL){
    
    # 
    equation='diff_hdi ~ '
    for (v in 1:length(varns)){
      if (v==b){
        equation=paste(equation,'l(',varns[v],',0:',NL+1-r,') + ',sep='')
      }
      else{
        equation=paste(equation,'l(',varns[v],',0:',NL,') + ',sep='') 
      }
    }
    equation=stringr::str_sub(equation,start=1L,end=-4L)
    equation=paste(equation,'|', idx)
    
    #run model
    mod = fixest::feols(eval(parse(text=equation)), data , panel.id = pan_id)
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


##### second try, interaction vars 
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

#####################

### lets keep model with differences, 3 lags for temp, 2 for rain


out_dir<-"output/models/first_diff_lags_hdi_models"

TNL<-3
RNL<-2
out_var<-"gr_hdi"

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(diff_RR_YY,0:",RNL, ")+ l(RR_YY_i_diff_RR_YY,0:",RNL, ")",  "|" , idx ))
mod0 = fixest::feols(f, data, panel.id=pan_id)
summary(mod0)

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(diff_RR_YY,0:",RNL, ") +diff_RR_YY_2+ RR_YY_i_diff_RR_YY",  "|" , idx ))
mod02 = fixest::feols(f, data, panel.id=pan_id)
summary(mod02)

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(diff_RR_YY,0:",RNL, ") + diff_RR_YY_2 + RR_YY_i_diff_RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",  "|" , idx ))
mod03 = fixest::feols(f, data, panel.id=pan_id)
summary(mod03) 
# best up to now

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(diff_RR_YY,0:",RNL, ") + diff_RR_YY_2 + RR_YY_i_diff_RR_YY + lag_log_gni_pc:diff_TM_YY + lag_log_gni_pc:diff_TM_YY_2 + lag_log_gni_pc:diff_RR_YY + lag_log_gni_pc:diff_RR_YY_2 ",  "|" , idx ))
mod04 = fixest::feols(f, data, panel.id=pan_id)
summary(mod04)
# works worst 

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(diff_RR_YY,0:",RNL, ") + diff_RR_YY_2 + RR_YY_i_diff_RR_YY +l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(lag_log_gni_pc_i_RR_YY,0:",RNL, ") + l(lag_log_gni_pc_i_TM_YY,0:",TNL, ")  ",  "|" , idx ))
mod05 = fixest::feols(f, data, panel.id=pan_id)
summary(mod05) 
# pretty shitty

models<-list(mod0, mod02, mod03, mod04, mod05)

fvcov_dk<-function(x) vcov(x, "DK")
modelsummary(models,lapply(models, FUN=fvcov_dk),  estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_lags_dk.html"))
modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, "models_hdi_gr_tp_lags.html"))


### lets keep the new best models with lags, maps of single effects, map of world with colors 
### plot damages for a mean situation in the years in each region: predicting damage response (only t,p) with a mean t, p and gni for each region. 

f <- as.formula( paste(out_var ,"~", paste0("lag_", out_var), "+ l(diff_TM_YY,0:",TNL, ") + l(TM_YY_i_diff_TM_YY,0:",TNL, ")+ l(diff_RR_YY,0:",RNL, ") + diff_RR_YY_2 + RR_YY_i_diff_RR_YY + lag_log_gni_pc:TM_YY + lag_log_gni_pc:TM_YY_2 + lag_log_gni_pc:RR_YY + lag_log_gni_pc:RR_YY_2 ",  "|" , idx ))

mod = fixest::feols(f, data, panel.id=pan_id)
summary(mod) 

mod = fixest::feols(f, data, panel.id=pan_id, vcov="DK")
summary(mod) 

coefs<-mod$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), t_mean_damage=NA, p_mean_damage=NA, t_plus_sd_damage=NA, p_plus_sd_damage=NA)

for (r in unique(data$gdlcode)){
  
  ### mean on years
  t<-mean(data$TM_YY[data$gdlcode == r], na.rm = TRUE)
  t_diff<- mean(data$diff_TM_YY[data$gdlcode == r], na.rm = TRUE)
  p<-mean(data$RR_YY[data$gdlcode == r], na.rm = TRUE)
  p_diff<- mean(data$diff_RR_YY[data$gdlcode == r], na.rm = TRUE)
  lgni<-mean(data$log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  # t damages
  mean_damages[mean_damages$gdlcode==r, 2]<-t_diff*coefs["diff_TM_YY"] +
    t_diff*coefs["l(diff_TM_YY, 1)"] + t_diff*coefs["l(diff_TM_YY, 2)"] +t_diff*coefs["l(diff_TM_YY, 3)"] +
    t_diff*t*coefs["TM_YY_i_diff_TM_YY"] + t_diff*t*coefs["l(TM_YY_i_diff_TM_YY, 2)"] +  t_diff*t*coefs["l(diff_TM_YY, 3)"]+ 
    t*lgni*coefs["lag_log_gni_pc:TM_YY"] + (t^2)*lgni*coefs["lag_log_gni_pc:TM_YY_2"]
  
  # p damages
  mean_damages[mean_damages$gdlcode==r, 3]<-p_diff*coefs["diff_RR_YY"] +
    p_diff*coefs["l(diff_RR_YY, 1)"] +
    p_diff*p*coefs["RR_YY_i_diff_RR_YY"] +
    p*lgni*coefs["lag_log_gni_pc:RR_YY"] 
  
  ### mean + sd on years
  t<-sd(data$TM_YY[data$gdlcode == r], na.rm = TRUE) + mean(data$TM_YY[data$gdlcode == r], na.rm = TRUE)
  t_diff<- sd(data$diff_TM_YY[data$gdlcode == r], na.rm = TRUE) + mean(data$diff_TM_YY[data$gdlcode == r], na.rm = TRUE)
  p<-sd(data$RR_YY[data$gdlcode == r], na.rm = TRUE)+mean(data$RR_YY[data$gdlcode == r], na.rm = TRUE)
  p_diff<- sd(data$diff_RR_YY[data$gdlcode == r], na.rm = TRUE)+mean(data$diff_RR_YY[data$gdlcode == r], na.rm = TRUE)
  lgni<-mean(data$log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  # t damages
  mean_damages[mean_damages$gdlcode==r, 4]<-t_diff*coefs["diff_TM_YY"] +
    t_diff*coefs["l(diff_TM_YY, 1)"] + t_diff*coefs["l(diff_TM_YY, 2)"] +t_diff*coefs["l(diff_TM_YY, 3)"] +
    t_diff*t*coefs["TM_YY_i_diff_TM_YY"] + t_diff*t*coefs["l(TM_YY_i_diff_TM_YY, 2)"] +  t_diff*t*coefs["l(diff_TM_YY, 3)"]+ 
    t*lgni*coefs["lag_log_gni_pc:TM_YY"] + (t^2)*lgni*coefs["lag_log_gni_pc:TM_YY_2"]
  
  # p damages
  mean_damages[mean_damages$gdlcode==r, 5]<-p_diff*coefs["diff_RR_YY"] +
    p_diff*coefs["l(diff_RR_YY, 1)"] +
    p_diff*p*coefs["RR_YY_i_diff_RR_YY"] +
    p*lgni*coefs["lag_log_gni_pc:RR_YY"] 
  
}
 
mean_damages<-inner_join(gdl_shape_file, mean_damages)

gt<-ggplot(mean_damages)+geom_sf( aes(fill=t_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_lags_mean_damages_t_mean.png"), gt, width=15, height=10)

gp<-ggplot(mean_damages)+geom_sf( aes(fill=p_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_lags_mean_damages_p_mean.png"), gp, width=15, height=10)

gt<-ggplot(mean_damages)+geom_sf( aes(fill=t_plus_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_lags_mean_damages_t_sd.png"), gt, width=15, height=10)

gp<-ggplot(mean_damages)+geom_sf( aes(fill=p_plus_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_lags_mean_damages_p_sd.png"), gp, width=15, height=10)



### same but best model without lags
f <- as.formula( paste( ))

mod = fixest::feols(f, data, panel.id=pan_id)
summary(mod) 

mod = fixest::feols(f, data, panel.id=pan_id, vcov="DK")
summary(mod) 

coefs<-mod$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), t_mean_damage=NA, p_mean_damage=NA, t_sd_damage=NA, p_sd_damage=NA)

for (r in unique(data$gdlcode)){
  
  ### mean on years
  t<-mean(data$TM_YY[data$gdlcode == r], na.rm = TRUE)
  t_diff<- mean(data$diff_TM_YY[data$gdlcode == r], na.rm = TRUE)
  p<-mean(data$RR_YY[data$gdlcode == r], na.rm = TRUE)
  p_diff<- mean(data$diff_RR_YY[data$gdlcode == r], na.rm = TRUE)
  lgni<-mean(data$log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  # t damages
  mean_damages[mean_damages$gdlcode==r, 2]<-t_diff*coefs["diff_TM_YY"] +
    t_diff*coefs["l(diff_TM_YY, 1)"] + t_diff*coefs["l(diff_TM_YY, 2)"] +t_diff*coefs["l(diff_TM_YY, 3)"] +
    t_diff*t*coefs["TM_YY_i_diff_TM_YY"] + t_diff*t*coefs["l(TM_YY_i_diff_TM_YY, 2)"] +  t_diff*t*coefs["l(diff_TM_YY, 3)"]+ 
    t*lgni*coefs["lag_log_gni_pc:TM_YY"] + (t^2)*lgni*coefs["lag_log_gni_pc:TM_YY_2"]
  
  # p damages
  mean_damages[mean_damages$gdlcode==r, 3]<-p_diff*coefs["diff_RR_YY"] +
    p_diff*coefs["l(diff_RR_YY, 1)"] +
    p_diff*p*coefs["RR_YY_i_diff_RR_YY"] +
    p*lgni*coefs["lag_log_gni_pc:RR_YY"] 
  
  ### sd on years
  t<-sd(data$TM_YY[data$gdlcode == r], na.rm = TRUE)
  t_diff<- sd(data$diff_TM_YY[data$gdlcode == r], na.rm = TRUE)
  p<-sd(data$RR_YY[data$gdlcode == r], na.rm = TRUE)
  p_diff<- sd(data$diff_RR_YY[data$gdlcode == r], na.rm = TRUE)
  lgni<-mean(data$log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  # t damages
  mean_damages[mean_damages$gdlcode==r, 4]<-t_diff*coefs["diff_TM_YY"] +
    t_diff*coefs["l(diff_TM_YY, 1)"] + t_diff*coefs["l(diff_TM_YY, 2)"] +t_diff*coefs["l(diff_TM_YY, 3)"] +
    t_diff*t*coefs["TM_YY_i_diff_TM_YY"] + t_diff*t*coefs["l(TM_YY_i_diff_TM_YY, 2)"] +  t_diff*t*coefs["l(diff_TM_YY, 3)"]+ 
    t*lgni*coefs["lag_log_gni_pc:TM_YY"] + (t^2)*lgni*coefs["lag_log_gni_pc:TM_YY_2"]
  
  # p damages
  mean_damages[mean_damages$gdlcode==r, 5]<-p_diff*coefs["diff_RR_YY"] +
    p_diff*coefs["l(diff_RR_YY, 1)"] +
    p_diff*p*coefs["RR_YY_i_diff_RR_YY"] +
    p*lgni*coefs["lag_log_gni_pc:RR_YY"] 
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

gt<-ggplot(mean_damages)+geom_sf( aes(fill=t_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_final_mean_damages_t_mean.png"), gt, width=15, height=10)

gp<-ggplot(mean_damages)+geom_sf( aes(fill=p_mean_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_final_mean_damages_p_mean.png"), gp, width=15, height=10)

gt<-ggplot(mean_damages)+geom_sf( aes(fill=t_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_final_mean_damages_t_sd.png"), gt, width=15, height=10)

gp<-ggplot(mean_damages)+geom_sf( aes(fill=p_sd_damage))+ scale_fill_gradient2()
ggsave(filename=file.path(out_dir, "hdi_gr_tp_final_mean_damages_p_sd.png"), gp, width=15, height=10)

################################################################################

### model coefficient robustness: iteratively fit them (with, without lags) without a region, to see coef variability (a sort of block bootstrap)
### plot coefficient variability obtained


### model with rainfall, temperature variability, pet change, test significance 





### compare final model with components

################################################################################

### model impulse response function?
