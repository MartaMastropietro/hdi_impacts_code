
args = base::commandArgs(trailingOnly=TRUE)


o<-args[2]
#o<-2
# kind<-args[3] # values, delta, perc_delta


if(o==1){
  out_var<-"edu_index"
}else if(o==2){
  out_var<-"lifex_index"
}else if(o==3){
  out_var<-"income_index"
}else if(o==4){
  out_var="hdi"}


library(readr)
#library(data.table)
library(reshape2)
library(dplyr)
library(purrr)

library(stringr)

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_extr_tp", "extr_only")
spec_type<-"dlm"
effect<-"growth_eff"
#lags number 
#N<-8
N<-"_mix_"

spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]

agg<-"pop_weight"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}


###
sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")




data<-arrow::read_feather(file.path(out_dir_lag,
                                    paste0(out_var ,'_values_deltas_',spec,'_',type,'_',  agg,"_nlags",N, 
                                           "_boot_impacts_all" , ".feather")) )
gc()

data<-data%>%group_by(gdlcode,iso3,ssp,year)%>%
  summarise(across(-c(model) , ~median(. , na.rm=TRUE)))

arrow::write_feather(data, file.path(out_dir_lag,
                                    paste0(out_var ,'_med_values_deltas_',spec,'_',type,'_',  agg,"_nlags",N, 
                                           "_boot_impacts_all" , ".feather")) )


gc()

quant05 <- function(x){
  quantile(x, probs=0.05, na.rm=TRUE)
}
quant95 <- function(x){
  quantile(x, probs=0.95, na.rm=TRUE)
}
quant10 <- function(x){
  quantile(x, probs=0.10, na.rm=TRUE)
}
quant90 <- function(x){
  quantile(x, probs=0.90, na.rm=TRUE)
}


### add deltas
if(out_var=="income_index"){
  gc()
  
  # if(kind=="diff"){
  #   data <- data %>%
  #     transmute(across(starts_with("income_index_cc."), 
  #                      delta = ~ . - income_index, 
  #                      .names = "{.col}")) %>%
  #     rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("delta.income_index_cc.")) 
  #     
  #   
  # }else if(kind=="perc_diff"){
  #   data <- data %>%
  #     transmute(across(starts_with("income_index_cc."), 
  #                      perc_delta = ~ (. - income_index) / income_index, 
  #                      .names = "{.col}")) %>%
  #     rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("perc_delta.income_index_cc."))
  #   
  # }
  data <- data %>%
    mutate(across(starts_with("income_index_cc."), 
                  .fns = list(
                    delta = ~ . - income_index, 
                    perc_delta = ~ (. - income_index) / income_index
                  ), 
                  .names = "{.fn}.{.col}")) %>%
    rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("delta.income_index_cc.")) %>%
    rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("perc_delta.income_index_cc."))
  
  
  data_int<-data[, c("gdlcode","iso3" ,"year", "ssp", "income_index")]
  
  
  data_int$delta_mean<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$delta_med<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$delta_q05<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
  data_int$delta_q95<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant95)
  data_int$delta_q10<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
  data_int$delta_q90<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant90)
  
  data_int$perc_delta_mean<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$perc_delta_med<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$perc_delta_q05<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
  data_int$perc_delta_q95<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)
  data_int$perc_delta_q10<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
  data_int$perc_delta_q90<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
  
  data_int$cc_mean<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$cc_med<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$cc_q05<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant05)
  data_int$cc_q95<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant95)
  data_int$cc_q10<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant10)
  data_int$cc_q90<-apply(data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant90)
  
  
}

if(out_var=="lifex_index"){
  gc()
  data <- data %>%
    mutate(across(starts_with("lifex_index_cc."), 
                  .fns = list(
                    delta = ~ . - lifex_index, 
                    perc_delta = ~ (. - lifex_index) / lifex_index
                  ), 
                  .names = "{.fn}.{.col}")) %>%
    rename_with(~ str_remove(.x, "lifex_index_cc\\."), starts_with("delta.lifex_index_cc.")) %>%
    rename_with(~ str_remove(.x, "lifex_index_cc\\."), starts_with("perc_delta.lifex_index_cc."))
  
  
  data_int<-data[, c("gdlcode","iso3" ,"year", "ssp",  "lifex_index")]
  
  
  data_int$delta_mean<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$delta_med<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$delta_q05<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
  data_int$delta_q95<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant95)
  data_int$delta_q10<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
  data_int$delta_q90<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant90)
  
  data_int$perc_delta_mean<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$perc_delta_med<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$perc_delta_q05<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
  data_int$perc_delta_q95<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)
  data_int$perc_delta_q10<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
  data_int$perc_delta_q90<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
  
  data_int$cc_mean<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$cc_med<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$cc_q05<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant05)
  data_int$cc_q95<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant95)
  data_int$cc_q10<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant10)
  data_int$cc_q90<-apply(data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant90)
  
  
}

if(out_var=="edu_index"){
  gc()
  data <- data %>%
    mutate(across(starts_with("edu_index_cc."), 
                  .fns = list(
                    delta = ~ . - edu_index, 
                    perc_delta = ~ (. - edu_index) / edu_index
                  ), 
                  .names = "{.fn}.{.col}")) %>%
    rename_with(~ str_remove(.x, "edu_index_cc\\."), starts_with("delta.edu_index_cc.")) %>%
    rename_with(~ str_remove(.x, "edu_index_cc\\."), starts_with("perc_delta.edu_index_cc."))
  
  
  data_int<-data[, c("gdlcode","iso3" ,"year", "ssp",  "edu_index")]
  
  
  data_int$delta_mean<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$delta_med<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$delta_q05<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
  data_int$delta_q95<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant95)
  data_int$delta_q10<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
  data_int$delta_q90<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant90)
  
  data_int$perc_delta_mean<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$perc_delta_med<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$perc_delta_q05<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
  data_int$perc_delta_q95<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)
  data_int$perc_delta_q10<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
  data_int$perc_delta_q90<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
  
  data_int$cc_mean<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$cc_med<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$cc_q05<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant05)
  data_int$cc_q95<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant95)
  data_int$cc_q10<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant10)
  data_int$cc_q90<-apply(data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant90)
  
  
}

if(out_var=="hdi"){
  
  gc()
  data <- data %>%
    mutate(across(starts_with("hdi_cc."), 
                  .fns = list(
                    delta = ~ . - hdi, 
                    perc_delta = ~ (. - hdi) / hdi
                  ), 
                  .names = "{.fn}.{.col}")) %>%
    rename_with(~ str_remove(.x, "hdi_cc\\."), starts_with("delta.hdi_cc.")) %>%
    rename_with(~ str_remove(.x, "hdi_cc\\."), starts_with("perc_delta.hdi_cc."))
  
  
  data_int<-data[, c("gdlcode","iso3" ,"year", "ssp", "hdi")]
  
  
  data_int$delta_mean<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$delta_med<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$delta_q05<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant05)
  data_int$delta_q95<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant95)
  data_int$delta_q10<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
  data_int$delta_q90<-apply(data%>%select(matches("^delta\\.\\d+$")), 1, quant90)
  
  data_int$perc_delta_mean<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$perc_delta_med<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$perc_delta_q05<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant05)
  data_int$perc_delta_q95<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant95)
  data_int$perc_delta_q10<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
  data_int$perc_delta_q90<-apply(data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)
  
  data_int$cc_mean<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
  data_int$cc_med<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, median, na.rm=TRUE)
  data_int$cc_q05<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant05)
  data_int$cc_q95<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant95)
  data_int$cc_q10<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant10)
  data_int$cc_q90<-apply(data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant90)
  
}



arrow::write_feather(data_int,file.path(out_dir_lag,
                                        paste0(out_var ,'_med_values_deltas_',
                                               spec,'_',type,'_',  agg,"_nlags",N, 
                                               "_boot_intervals" , ".feather")) )



################################################################################

