### analyse models in terms of metrics

# selection of level of aggregation fe?
# selection of lags?


rm(list=ls())

source("utils/libraries.R")


# output dir
out_dir<-"output/models_metrics"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/models_metrics/index_selection"
if(!dir.exists(out_dir)){dir.create(out_dir)}


# load metrics 
dir<-"output/models/components_tp_extr_all_models_performances"
load(file.path(dir, "all_comp_all_lags_models_metrics.RData"))

data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")


### normalize rmse to compare among variables 
iqrange_hdi<-quantile(data$diff_hdi, 0.75, na.rm=TRUE) - quantile(data$diff_hdi, 0.25, na.rm=TRUE)
iqrange_edu<-quantile(data$diff_edu_index, 0.75, na.rm=TRUE) - quantile(data$diff_edu_index, 0.25, na.rm=TRUE)
iqrange_health<-quantile(data$diff_health_index, 0.75, na.rm=TRUE) - quantile(data$diff_health_index, 0.25, na.rm=TRUE)
iqrange_income<-quantile(data$diff_income_index, 0.75, na.rm=TRUE) - quantile(data$diff_income_index, 0.25, na.rm=TRUE)
iqrange_gni<-quantile(data$gr_log_gni_pc, 0.75, na.rm=TRUE) - quantile(data$gr_log_gni_pc, 0.25, na.rm=TRUE)


out_vars_length<-5
extr_length<-8
indeces_length<-4
formulas_length<-21

out_vars<-c("diff_hdi", "diff_income_index", "diff_edu_index", "diff_health_index",  "gr_log_gni_pc")
out_vars_iqranges<-c(iqrange_hdi, iqrange_income, iqrange_edu, iqrange_health, iqrange_gni)

extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
indeces<-c("gdlcode + year", 
           "gdlcode + iso3[year] + year", 
           "gdlcode + iso3[year] + iso3[year^2] + year", 
           "gdlcode + gdlcode[year] +gdlcode[year^2] + year ")
names(indeces)<-c("fe_simple", 
                  "fe_linear_iso", 
                  "fe_quad_iso" ,
                  "fe_quad_gdl")
formulas<-paste0("f_", 1:formulas_length)

metrics_data<-expand.grid(out_var=out_vars, extr_var=extr_variables, idx=indeces, formula=formulas )
metrics_data$aic<-NA
metrics_data$bic<-NA
metrics_data$wr2<-NA
metrics_data$l1yo_cv<-NA
metrics_data$l3yo_cv<-NA

for ( o in 1:out_vars_length){
  for ( e in 1:extr_length){
    for ( i in 1:indeces_length){
      for ( f in 1:formulas_length){
        metrics_data[which(metrics_data$out_var == out_vars[o] &
                           metrics_data$extr_var == extr_variables[e] & 
                           metrics_data$idx == indeces[i] & 
                             metrics_data$formula == formulas[f]  ), "aic"]<- metrics[[o]][[e]][[i]][[f]][["AIC"]]
        metrics_data[which(metrics_data$out_var == out_vars[o] &
                             metrics_data$extr_var == extr_variables[e] & 
                             metrics_data$idx == indeces[i] & 
                             metrics_data$formula == formulas[f]  ), "bic"]<- metrics[[o]][[e]][[i]][[f]][["BIC"]]
        metrics_data[which(metrics_data$out_var == out_vars[o] &
                             metrics_data$extr_var == extr_variables[e] & 
                             metrics_data$idx == indeces[i] & 
                             metrics_data$formula == formulas[f]  ), "wr2"]<- metrics[[o]][[e]][[i]][[f]][["wr2"]]
        metrics_data[which(metrics_data$out_var == out_vars[o] &
                             metrics_data$extr_var == extr_variables[e] & 
                             metrics_data$idx == indeces[i] & 
                             metrics_data$formula == formulas[f]  ), "l1yo_cv"]<- metrics[[o]][[e]][[i]][[f]][["l1yo_cv"]]/out_vars_iqranges[o]
        metrics_data[which(metrics_data$out_var == out_vars[o] &
                             metrics_data$extr_var == extr_variables[e] & 
                             metrics_data$idx == indeces[i] & 
                             metrics_data$formula == formulas[f]  ), "l3yo_cv"]<- metrics[[o]][[e]][[i]][[f]][["l3yo_cv"]]/out_vars_iqranges[o]
      }
      
    }
    
  }
  
}


metrics_data$idx_name<-""
metrics_data$idx_name[which(metrics_data$idx==indeces[1])]<-names(indeces)[1]
metrics_data$idx_name[which(metrics_data$idx==indeces[2])]<-names(indeces)[2]
metrics_data$idx_name[which(metrics_data$idx==indeces[3])]<-names(indeces)[3]
metrics_data$idx_name[which(metrics_data$idx==indeces[4])]<-names(indeces)[4]


ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=l1yo_cv)) + facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=wr2))+ facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=aic))+ facet_wrap(~out_var)
ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=bic))+ facet_wrap(~out_var)


g<-ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=l1yo_cv, col=extr_var)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_lo1o_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=l3yo_cv, col=extr_var)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_lo3o_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=wr2, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_wr2_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=aic, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_aic_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data)+ geom_boxplot(aes(x=idx_name, y=bic, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_bic_ncv_index_analysis_boxplot.png"), width = 20, height=12)




g<-ggplot(metrics_data)+ geom_point(aes(x=idx_name, y=l1yo_cv, col=formula)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_lo1o_ncv_index_analysis.png"), width = 20, height=12)
#g<-ggplot(metrics_data[which(metrics_data$out_var=="gr_log_gni_pc"), ])+ geom_boxplot(aes(x=idx, y=l1yo_cv)) 
#ggsave(filename=file.path(out_dir, "gr_gni_all_lags_models_lo1o_ncv_index_analysis.png"), width = 20, height=12)

g<-ggplot(metrics_data)+ geom_point(aes(x=idx_name, y=l3yo_cv, col=formula)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_lo3o_ncv_index_analysis.png"), width = 20, height=12)
#g<-ggplot(metrics_data[which(metrics_data$out_var=="gr_log_gni_pc"), ])+ geom_boxplot(aes(x=idx, y=l3yo_cv)) 
#ggsave(filename=file.path(out_dir, "gr_gni_all_lags_models_lo3o_ncv_index_analysis.png"), width = 20, height=12)



# lets stick with third index, "gdlcode + iso3[year] + iso3[year^2] + year", it is consistently better in terms of cv
# check results with gdlcode agg


### lags 

# cal 
for (extr in extr_variables){
  dat_temp<- metrics_data[ which(metrics_data$extr_var == extr & metrics_data$formula %in% formulas_cal ), ]
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l3yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_lo3o_ncv_lag_level_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l1yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_lo1o_ncv_lag_level_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=aic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_aic_ncv_lag_level_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=bic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_bic_ncv_lag_level_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=wr2, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_wr2_ncv_lag_level_analysis.png")), width = 20, height=12)
  
}

# kotz 
for (extr in extr_variables){
  dat_temp<- metrics_data[ which(metrics_data$extr_var == extr & metrics_data$formula %in% formulas_kotz ), ]
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l3yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_lo3o_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l1yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_lo1o_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=aic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_aic_ncv_lag_diff_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=bic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_bic_ncv_lag_diff_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=wr2, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_wr2_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
}



###########

# load metrics with ar comp

load(file.path(dir, "all_comp_all_lags_models_ar_metrics.RData"))



out_vars_length<-5
extr_length<-8
indeces_length<-4
formulas_length<-21

out_vars<-c("diff_hdi", "diff_income_index", "diff_edu_index", "diff_health_index",  "gr_log_gni_pc")
extr_variables<-c("TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")
indeces<-c("gdlcode + year", 
           "gdlcode + iso3[year] + year", 
           "gdlcode + iso3[year] + iso3[year^2] + year", 
           "gdlcode + gdlcode[year] +gdlcode[year^2] + year ")
names(indeces)<-c("fe_simple", 
                  "fe_linear_iso", 
                  "fe_quad_iso" ,
                  "fe_quad_gdl")
formulas<-paste0("f_", 1:formulas_length)

metrics_data_ar<-expand.grid(out_var=out_vars, extr_var=extr_variables, idx=indeces, formula=formulas )
metrics_data_ar$aic<-NA
metrics_data_ar$bic<-NA
metrics_data_ar$wr2<-NA
metrics_data_ar$l1yo_cv<-NA
metrics_data_ar$l3yo_cv<-NA

for ( o in 1:out_vars_length){
  for ( e in 1:extr_length){
    for ( i in 1:indeces_length){
      for ( f in 1:formulas_length){
        metrics_data_ar[which(metrics_data_ar$out_var == out_vars[o] &
                             metrics_data_ar$extr_var == extr_variables[e] & 
                             metrics_data_ar$idx == indeces[i] & 
                             metrics_data_ar$formula == formulas[f]  ), "aic"]<- metrics[[o]][[e]][[i]][[f]][["AIC"]]
        metrics_data_ar[which(metrics_data_ar$out_var == out_vars[o] &
                             metrics_data_ar$extr_var == extr_variables[e] & 
                             metrics_data_ar$idx == indeces[i] & 
                             metrics_data_ar$formula == formulas[f]  ), "bic"]<- metrics[[o]][[e]][[i]][[f]][["BIC"]]
        metrics_data_ar[which(metrics_data_ar$out_var == out_vars[o] &
                             metrics_data_ar$extr_var == extr_variables[e] & 
                             metrics_data_ar$idx == indeces[i] & 
                             metrics_data_ar$formula == formulas[f]  ), "wr2"]<- metrics[[o]][[e]][[i]][[f]][["wr2"]]
        metrics_data_ar[which(metrics_data_ar$out_var == out_vars[o] &
                             metrics_data_ar$extr_var == extr_variables[e] & 
                             metrics_data_ar$idx == indeces[i] & 
                             metrics_data_ar$formula == formulas[f]  ), "l1yo_cv"]<- metrics[[o]][[e]][[i]][[f]][["l1yo_cv"]]/out_vars_iqranges[o]
        metrics_data_ar[which(metrics_data_ar$out_var == out_vars[o] &
                             metrics_data_ar$extr_var == extr_variables[e] & 
                             metrics_data_ar$idx == indeces[i] & 
                             metrics_data_ar$formula == formulas[f]  ), "l3yo_cv"]<- metrics[[o]][[e]][[i]][[f]][["l3yo_cv"]]/out_vars_iqranges[o]
      }
      
    }
    
  }
  
}

metrics_data_ar$idx_name<-""
metrics_data_ar$idx_name[which(metrics_data_ar$idx==indeces[1])]<-names(indeces)[1]
metrics_data_ar$idx_name[which(metrics_data_ar$idx==indeces[2])]<-names(indeces)[2]
metrics_data_ar$idx_name[which(metrics_data_ar$idx==indeces[3])]<-names(indeces)[3]
metrics_data_ar$idx_name[which(metrics_data_ar$idx==indeces[4])]<-names(indeces)[4]


g<-ggplot(metrics_data_ar)+ geom_boxplot(aes(x=idx_name, y=l1yo_cv, col=extr_var)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_lo1o_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data_ar)+ geom_boxplot(aes(x=idx_name, y=l3yo_cv, col=extr_var)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_lo3o_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data_ar)+ geom_boxplot(aes(x=idx_name, y=wr2, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_wr2_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data_ar)+ geom_boxplot(aes(x=idx_name, y=aic, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_aic_ncv_index_analysis_boxplot.png"), width = 20, height=12)

g<-ggplot(metrics_data_ar)+ geom_boxplot(aes(x=idx_name, y=bic, col=extr_var))+ facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_bic_ncv_index_analysis_boxplot.png"), width = 20, height=12)



g<-ggplot(metrics_data_ar)+ geom_point(aes(x=idx_name, y=l1yo_cv, col=formula)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_lo1o_ncv_index_analysis.png"), width = 20, height=12)
#g<-ggplot(metrics_data_ar[which(metrics_data_ar$out_var=="gr_log_gni_pc"), ])+ geom_boxplot(aes(x=idx, y=l1yo_cv)) 
#ggsave(filename=file.path(out_dir, "gr_gni_all_lags_models_ar_lo1o_ncv_index_analysis.png"), width = 20, height=12)

g<-ggplot(metrics_data_ar)+ geom_point(aes(x=idx_name, y=l3yo_cv, col=formula)) + facet_wrap(~out_var)
ggsave(filename=file.path(out_dir, "all_comp_all_lags_models_ar_lo3o_ncv_index_analysis.png"), width = 20, height=12)
#g<-ggplot(metrics_data_ar[which(metrics_data_ar$out_var=="gr_log_gni_pc"), ])+ geom_boxplot(aes(x=idx, y=l3yo_cv)) 
#ggsave(filename=file.path(out_dir, "gr_gni_all_lags_models_ar_lo3o_ncv_index_analysis.png"), width = 20, height=12)


### lets now see optimal lags -> facet wrap on resp variables, for each extreme, for the two separate lag formulas 
### here we are keeping 8 lags for tm, 4 for rr, from 1 to 8 for each extreme

names(metrics[[1]][[1]][[1]])
formulas_cal<-paste0("f_", 6:13)
formulas_kotz<-paste0("f_", 14:21)

# cal 
for (extr in extr_variables){
  dat_temp<- metrics_data_ar[ which(metrics_data_ar$extr_var == extr & metrics_data_ar$formula %in% formulas_cal ), ]
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l3yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_lo3o_ncv_lag_level_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l1yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_lo1o_ncv_lag_level_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=aic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_aic_ncv_lag_level_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=bic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_bic_ncv_lag_level_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=wr2, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_wr2_ncv_lag_level_analysis.png")), width = 20, height=12)
  
}

# kotz 
for (extr in extr_variables){
  dat_temp<- metrics_data_ar[ which(metrics_data_ar$extr_var == extr & metrics_data_ar$formula %in% formulas_kotz ), ]
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l3yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_lo3o_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=l1yo_cv, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_lo1o_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=aic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_aic_ncv_lag_diff_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=bic, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_bic_ncv_lag_diff_analysis.png")), width = 20, height=12)
  g<-ggplot(dat_temp)+ geom_point(aes(x=formula, y=wr2, col=idx_name)) + facet_wrap(~out_var)
  ggsave(filename=file.path(out_dir,paste0(extr, "_", "all_comp_all_lags_models_ar_wr2_ncv_lag_diff_analysis.png")), width = 20, height=12)
  
}




### new lags selection, with more specifications: adaptation inclusion in both 

