
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)

library(RColorBrewer)

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/final_proj"
if(!dir.exists(out_dir)){dir.create(out_dir)}

#lags number 
N<-"_mix_"
#N<-8

out_dir<-file.path(out_dir, paste0("N",N,"lags"))
if(!dir.exists(out_dir)){dir.create(out_dir)}



out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

out_variables<-c( "leb", "eys", "gnipc"  )

out_dir_comp<-"output/projections/original_comp" 

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_extr_tp", "extr_only")


spec_type<-"dlm"

effect<-"growth_eff"

spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]



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

################################################################################
################################################################################
################################################################################

# selected countries: 
# - compare with barplots, one shape for model, on x axis the three comp of index loss



out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

glob_data$delta_hdi_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_hdi_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_hdi_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_hdi_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_hdi_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_hdi_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_hdi_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_hdi_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$hdi_cc_mean<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$hdi_cc_med<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$hdi_cc_q10<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant10)
glob_data$hdi_cc_q90<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "model", "iso3", "year",  "hdi",
                        "delta_hdi_mean", "delta_hdi_med","delta_hdi_q10", "delta_hdi_q90",
                        "perc_delta_hdi_mean", "perc_delta_hdi_med","perc_delta_hdi_q10", "perc_delta_hdi_q90",
                        "hdi_cc_mean", "hdi_cc_med","hdi_cc_q10", "hdi_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-all_data

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_income_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_income_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_income_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_income_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_income_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_income_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_income_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_income_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$income_index_cc_mean<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$income_index_cc_med<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$income_index_cc_q10<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant10)
glob_data$income_index_cc_q90<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "model", "iso3", "year",  "income_index",
                        "delta_income_index_mean", "delta_income_index_med","delta_income_index_q10", "delta_income_index_q90",
                        "perc_delta_income_index_mean", "perc_delta_income_index_med","perc_delta_income_index_q10", "perc_delta_income_index_q90",
                        "income_index_cc_mean", "income_index_cc_med","income_index_cc_q10", "income_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_lifex_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_lifex_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_lifex_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_lifex_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_lifex_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_lifex_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_lifex_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_lifex_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$lifex_index_cc_mean<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$lifex_index_cc_med<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$lifex_index_cc_q10<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant10)
glob_data$lifex_index_cc_q90<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "model", "iso3", "year",  "lifex_index",
                        "delta_lifex_index_mean", "delta_lifex_index_med","delta_lifex_index_q10", "delta_lifex_index_q90",
                        "perc_delta_lifex_index_mean", "perc_delta_lifex_index_med","perc_delta_lifex_index_q10", "perc_delta_lifex_index_q90",
                        "lifex_index_cc_mean", "lifex_index_cc_med","lifex_index_cc_q10", "lifex_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)


out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))
glob_data$delta_edu_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_edu_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_edu_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_edu_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_edu_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_edu_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_edu_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_edu_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$edu_index_cc_mean<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$edu_index_cc_med<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$edu_index_cc_q10<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant10)
glob_data$edu_index_cc_q90<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "model", "iso3", "year",  "edu_index",
                        "delta_edu_index_mean", "delta_edu_index_med","delta_edu_index_q10", "delta_edu_index_q90",
                        "perc_delta_edu_index_mean", "perc_delta_edu_index_med","perc_delta_edu_index_q10", "perc_delta_edu_index_q90",
                        "edu_index_cc_mean", "edu_index_cc_med","edu_index_cc_q10", "edu_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)

write.csv(x=data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_all_idx_',type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)



#### median



out_var<-"hdi"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

# take median on models of all vars
glob_data<-glob_data%>%group_by(iso3,year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()

glob_data$delta_hdi_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_hdi_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_hdi_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_hdi_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_hdi_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_hdi_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_hdi_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_hdi_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$hdi_cc_mean<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$hdi_cc_med<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$hdi_cc_q10<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant10)
glob_data$hdi_cc_q90<-apply(glob_data%>%select(matches("^hdi_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "iso3", "year",  "hdi",
                        "delta_hdi_mean", "delta_hdi_med","delta_hdi_q10", "delta_hdi_q90",
                        "perc_delta_hdi_mean", "perc_delta_hdi_med","perc_delta_hdi_q10", "perc_delta_hdi_q90",
                        "hdi_cc_mean", "hdi_cc_med","hdi_cc_q10", "hdi_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-all_data

out_var<-"income_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

# take median on models of all vars
glob_data<-glob_data%>%group_by(iso3,year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()

glob_data$delta_income_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_income_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_income_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_income_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_income_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_income_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_income_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_income_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$income_index_cc_mean<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$income_index_cc_med<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$income_index_cc_q10<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant10)
glob_data$income_index_cc_q90<-apply(glob_data%>%select(matches("^income_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "iso3", "year",  "income_index",
                        "delta_income_index_mean", "delta_income_index_med","delta_income_index_q10", "delta_income_index_q90",
                        "perc_delta_income_index_mean", "perc_delta_income_index_med","perc_delta_income_index_q10", "perc_delta_income_index_q90",
                        "income_index_cc_mean", "income_index_cc_med","income_index_cc_q10", "income_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)

out_var<-"lifex_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

# take median on models of all vars
glob_data<-glob_data%>%group_by(iso3,year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()

glob_data$delta_lifex_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_lifex_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_lifex_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_lifex_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_lifex_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_lifex_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_lifex_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_lifex_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$lifex_index_cc_mean<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$lifex_index_cc_med<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$lifex_index_cc_q10<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant10)
glob_data$lifex_index_cc_q90<-apply(glob_data%>%select(matches("^lifex_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "iso3", "year",  "lifex_index",
                        "delta_lifex_index_mean", "delta_lifex_index_med","delta_lifex_index_q10", "delta_lifex_index_q90",
                        "perc_delta_lifex_index_mean", "perc_delta_lifex_index_med","perc_delta_lifex_index_q10", "perc_delta_lifex_index_q90",
                        "lifex_index_cc_mean", "lifex_index_cc_med","lifex_index_cc_q10", "lifex_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)


out_var<-"edu_index"
glob_data<-read_csv(file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")))

# take median on models of all vars
glob_data<-glob_data%>%group_by(iso3,year,ssp)%>%summarise(across(-model, median, na.rm = TRUE))%>%ungroup()

glob_data$delta_edu_index_mean<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$delta_edu_index_med<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$delta_edu_index_q10<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant10)
glob_data$delta_edu_index_q90<-apply(glob_data%>%select(matches("^delta\\.\\d+$")), 1, quant90)

glob_data$perc_delta_edu_index_mean<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$perc_delta_edu_index_med<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$perc_delta_edu_index_q10<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant10)
glob_data$perc_delta_edu_index_q90<-apply(glob_data%>%select(matches("^perc_delta\\.\\d+$")), 1, quant90)

glob_data$edu_index_cc_mean<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, mean, na.rm=TRUE)
glob_data$edu_index_cc_med<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, median, na.rm=TRUE)
glob_data$edu_index_cc_q10<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant10)
glob_data$edu_index_cc_q90<-apply(glob_data%>%select(matches("^edu_index_cc\\.\\d+$")), 1, quant90)

all_data<-glob_data[, c("ssp", "iso3", "year",  "edu_index",
                        "delta_edu_index_mean", "delta_edu_index_med","delta_edu_index_q10", "delta_edu_index_q90",
                        "perc_delta_edu_index_mean", "perc_delta_edu_index_med","perc_delta_edu_index_q10", "perc_delta_edu_index_q90",
                        "edu_index_cc_mean", "edu_index_cc_med","edu_index_cc_q10", "edu_index_cc_q90")]
# write.csv(x=all_data, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)
data<-inner_join(data, all_data)

write.csv(x=data, file=file.path(out_dir_lag,paste0('c_sel_med_d_s_agg_all_idx_',type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")), row.names = FALSE)

################################################################################
################################################################################
################################################################################



### plot with all models 

data<-read_csv(file.path(out_dir_lag,paste0('c_sel_d_s_agg_all_idx_',type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")))

out_dir_sel<-file.path(out_dir, "sel_countries")
if(!dir.exists(out_dir_sel)){dir.create(out_dir_sel)}

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer) 



for (y in c(2030, 2050,2080, 2100)){
  for (sc in unique(data$ssp)){
    
    
    # diff
    # Filter data for a specific year and SSP (update as needed)
    filtered_data <- data %>%
      filter(year == y, ssp == sc)  # Change year and SSP as needed
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3, model, 
             delta_hdi_mean, delta_income_index_mean, delta_edu_index_mean, delta_lifex_index_mean,
             delta_hdi_q10, delta_income_index_q10, delta_edu_index_q10, delta_lifex_index_q10,
             delta_hdi_q90, delta_income_index_q90, delta_edu_index_q90, delta_lifex_index_q90) %>%
      pivot_longer(cols = -c(iso3, model), 
                   names_to = c("variable", "stat"), 
                   names_pattern = "delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100*mean, fill = model)) +
      geom_col(position = "dodge", width = 0.7) +  # Grouped bars
      geom_errorbar(aes(ymin = 100*q10, ymax = 100*q90), 
                    position = position_dodge(width = 0.7), width = 0.2) +  # Error bars
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed") +  # Facet by country
      labs(x = "Variable", y = "Index Change (in % points)",
           fill = "Model") +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_d_",y, "_",sc, "_", type,'_',spec,"_", ".png")), 
           g,  width=10,height=10)
    
    
    # perc diff
    # Filter data for a specific year and SSP (update as needed)
    filtered_data <- data %>%
      filter(year == y, ssp == sc)  # Change year and SSP as needed
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3, model, 
             perc_delta_hdi_mean, perc_delta_income_index_mean, perc_delta_edu_index_mean, perc_delta_lifex_index_mean,
             perc_delta_hdi_q10, perc_delta_income_index_q10, perc_delta_edu_index_q10, perc_delta_lifex_index_q10,
             perc_delta_hdi_q90, perc_delta_income_index_q90, perc_delta_edu_index_q90, perc_delta_lifex_index_q90) %>%
      pivot_longer(cols = -c(iso3, model), 
                   names_to = c("variable", "stat"), 
                   names_pattern = "perc_delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100*mean, fill = model)) +
      geom_col(position = "dodge", width = 0.7) +  # Grouped bars
      geom_errorbar(aes(ymin = 100*q10, ymax = 100*q90), 
                    position = position_dodge(width = 0.7), width = 0.2) +  # Error bars
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed") +  # Facet by country
      labs(x = "Variable", y = "Percentage Index Change",
           fill = "Model") +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_p_d_",y, "_",sc, "_", type,'_',spec,"_", ".png")), 
           g,  width=10,height=10)
    
  }
}


################################################################################
################################################################################
################################################################################


### plot with median

data<-read_csv(file.path(out_dir_lag,paste0('c_sel_med_d_s_agg_all_idx_',type,'_',spec,'_', "_pop_weight_nlags_",N,"boot_int.csv")))

out_dir_sel<-file.path(out_dir, "sel_countries")
if(!dir.exists(out_dir_sel)){dir.create(out_dir_sel)}

library(ggplot2)
library(dplyr)
library(tidyr)
library(RColorBrewer) 

sel_countries<-c("USA", "ITA", "RUS", "CHN", "BRA", "ETH",  "AFG","IND")

# data <- data %>%
#   filter( iso3 %in% sel_countries) 


for (y in c(2030, 2050,2080, 2100)){
  for (sc in unique(data$ssp)){
    
    
    # diff
    # Filter data for a specific year and SSP (update as needed)
    filtered_data <- data %>%
      filter(year == y, ssp == sc, iso3 %in% sel_countries)  # Change year and SSP as needed
    filtered_data$iso3=factor(filtered_data$iso3, levels=sel_countries)
    filtered_data<-as.data.frame(filtered_data)
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3, 
             delta_hdi_mean, delta_income_index_mean, delta_edu_index_mean, delta_lifex_index_mean,
             delta_hdi_q10, delta_income_index_q10, delta_edu_index_q10, delta_lifex_index_q10,
             delta_hdi_q90, delta_income_index_q90, delta_edu_index_q90, delta_lifex_index_q90) %>%
      pivot_longer(cols = -iso3, 
                   names_to = c("variable", "stat"), 
                   names_pattern = "delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    # Define the variable order
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100 * mean, color = variable)) +
     # geom_col(width = 0.7) +  # Single bars for each variable
      geom_errorbar(aes(ymin = 100 * q10, ymax = 100 * q90), 
                    width = 0.3, size=1) +  # Error bars for q10 and q90
      geom_abline(slope=0, intercept=0)+
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by country
      labs(x = "Variable", y = "Index Change (in % points)",
           fill = "Variable") +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
    
    # Save the plot
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_med_d_", y, "_", sc, "_", type, '_', spec, ".png")), 
           g,width=8,height=5)
   
    
    # perc diff
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3, 
             perc_delta_hdi_mean, perc_delta_income_index_mean, perc_delta_edu_index_mean, perc_delta_lifex_index_mean,
             perc_delta_hdi_q10, perc_delta_income_index_q10, perc_delta_edu_index_q10, perc_delta_lifex_index_q10,
             perc_delta_hdi_q90, perc_delta_income_index_q90, perc_delta_edu_index_q90, perc_delta_lifex_index_q90) %>%
      pivot_longer(cols = -c(iso3), 
                   names_to = c("variable", "stat"), 
                   names_pattern = "perc_delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100 * mean, color = variable)) +
      #geom_col(width = 0.7) +  # Single bars for each variable
      geom_errorbar(aes(ymin = 100 * q10, ymax = 100 * q90), 
                    width = 0.2, size=1) +  # Error bars for q10 and q90
      geom_abline(slope=0, intercept=0)+
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by country
      labs(x = "Variable", y = "Percentage Index Change",
           fill = "Variable") +
      theme_bw() +
      scale_fill_brewer(palette = "Set3") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1))
    
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_med_p_d_",y, "_",sc, "_", type,'_',spec, ".png")),  
           g,width=8,height=5)
    
  }
}

# all ssp together

for (y in c(2030, 2050,2080, 2100)){
  #for (sc in unique(data$ssp)){
    
    
    # diff
    # Filter data for a specific year and SSP (update as needed)
    filtered_data <- data %>%
      filter(year == y,  iso3 %in% sel_countries)  # Change year and SSP as needed
    filtered_data$iso3=factor(filtered_data$iso3, levels=sel_countries)
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3, ssp,
             delta_hdi_mean, delta_income_index_mean, delta_edu_index_mean, delta_lifex_index_mean,
             delta_hdi_q10, delta_income_index_q10, delta_edu_index_q10, delta_lifex_index_q10,
             delta_hdi_q90, delta_income_index_q90, delta_edu_index_q90, delta_lifex_index_q90) %>%
      pivot_longer(cols = -c(iso3,ssp), 
                   names_to = c("variable", "stat"), 
                   names_pattern = "delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    # Define the variable order
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100 * mean, color = ssp)) +
      #geom_col(position = "dodge", width = 0.7) +  # Single bars for each variable
      geom_errorbar(position = "dodge",aes(ymin = 100 * q10, ymax = 100 * q90), 
                    width = 0.6, size=0.9) +  # Error bars for q10 and q90
      geom_abline(slope=0, intercept=0)+
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by country
      labs(x = "Variable", y = "Index Change (in % points)",
           fill = "Variable") +
      theme_bw() +
      scale_color_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            text = element_text(size = 14))
    
    
    # Save the plot
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_all_ssp_med_d_", y,  "_", type, '_', spec, ".png")), 
           g,width=9,height=6)
    
    
    # perc diff
    
    # Reshape data to long format for ggplot
    long_data <- filtered_data %>%
      select(iso3,  ssp,
             perc_delta_hdi_mean, perc_delta_income_index_mean, perc_delta_edu_index_mean, perc_delta_lifex_index_mean,
             perc_delta_hdi_q10, perc_delta_income_index_q10, perc_delta_edu_index_q10, perc_delta_lifex_index_q10,
             perc_delta_hdi_q90, perc_delta_income_index_q90, perc_delta_edu_index_q90, perc_delta_lifex_index_q90) %>%
      pivot_longer(cols = -c(iso3,ssp), 
                   names_to = c("variable", "stat"), 
                   names_pattern = "perc_delta_(.*)_(mean|q10|q90)") %>%
      pivot_wider(names_from = stat, values_from = value) %>%
      mutate(variable = recode(variable, 
                               hdi = "HDI", 
                               income_index = "Income Index", 
                               edu_index = "Edu Index", 
                               lifex_index = "Lifex Index"))
    
    variable_order <- c("Income Index", "Edu Index", "Lifex Index", "HDI")
    
    # Plot with ggplot
    g<-ggplot(long_data, aes(x = variable, y = 100 * mean, color = ssp)) +
      #geom_col(position = "dodge", width = 0.7) +  # Single bars for each variable
      geom_errorbar(position = "dodge",aes(ymin = 100 * q10, ymax = 100 * q90), 
                    width = 0.6, size=0.9) +  # Error bars for q10 and q90
      geom_abline(slope=0, intercept=0)+
      scale_x_discrete(limits = variable_order) + 
      facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by country
      labs(x = "Variable", y = "Percentage Index Change",
           fill = "Variable") +
      theme_bw() +
      scale_color_brewer(palette = "Set1") +
      theme(axis.text.x = element_text(angle = 60, hjust = 1),
            text = element_text(size = 14))
    
    ggsave(filename=file.path(out_dir_sel, paste0("c_sel_all_idx_all_ssp_med_p_d_",y,  "_", type,'_',spec, ".png")),  
           g,width=9,height=6)
    
 # }
}


for (sc in unique(data$ssp)){
    
data_comp<-data%>%filter(ssp == sc, iso3 %in% sel_countries)
data_comp$iso3=factor(data_comp$iso3, levels=sel_countries)


g<-ggplot(data_comp, aes(x = 100*delta_lifex_index_mean, y = 100*delta_income_index_mean, color = 100*delta_edu_index_mean)) +
  #geom_line() +  # Line plot for trend over time
  geom_point() +  # Points with size mapped to delta_lifex_index_mean
  scale_color_viridis_c(option = "magma")+ # Color scale for better visualization
  facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by iso3, allowing different y scales
  theme_bw() + 
  labs(
       x = "Lifex Index change",
       y = "Income Index change",
       color = "Edu Index change")

ggsave(filename=file.path(out_dir_sel, paste0("c_sel_comp_idx_med_d_",sc, "_", type,'_',spec, ".png")),  
       g,width=8,height=5)

# perc 
g<-ggplot(data_comp, aes(x = 100*perc_delta_lifex_index_mean, y = 100*perc_delta_income_index_mean, color = 100*perc_delta_edu_index_mean)) +
  #geom_line() +  # Line plot for trend over time
  geom_point() +  # Points with size mapped to delta_lifex_index_mean
  scale_color_viridis_c(option = "magma")+ # Color scale for better visualization
  facet_wrap(~iso3, scales = "fixed", ncol=4) +  # Facet by iso3, allowing different y scales
  theme_bw() + 
  labs(
       x = "Lifex Index perc change",
       y = "Income Index perc change",
       color = "Edu Index perc change")

ggsave(filename=file.path(out_dir_sel, paste0("c_sel_comp_idx_med_p_d_",sc, "_", type,'_',spec,".png")),  
       g,width=8,height=5)
}
