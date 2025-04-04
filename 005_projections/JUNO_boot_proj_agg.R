
rm(list=ls())

library(dplyr)

specifications<-c("mean_mod") #   ,"mean_mod_spec")
# types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
vars_in_proj<-c( "all_extr_tp", "extr_only")
spec_type<-"dlm"
effect<-"growth_eff"


spec<-specifications[1]
# type<-types[1]
vars<-vars_in_proj[1]
N="_mix_"

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}


n_boot<-1000

pop_projection_gdlcode <- readr::read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
gdlcodes_iso <- readr::read_csv("data/gdlcodes_iso.csv")
pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
pop_projection_gdlcode<-pop_projection_gdlcode%>%
  group_by(iso3, year, ssp)%>%
  mutate(pop_country=sum(pop_gdl))
pop_projection_gdlcode<-unique(pop_projection_gdlcode)


for(o in c("gr_gnipc", "gr_leb", "gr_eys")){
  
  data_int<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags_",N,"_boot_impacts_intervals.feather")))
  
  data_all<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.feather")))
  
  
  
  data_int<-inner_join(data_int, pop_projection_gdlcode )
  
  resp_types<-c("diff", "perc_diff", "cc_values")
  
  #########
  # TO REMOVE 
  
  # 
  # data_all<-data_int
  # data_all<-data_all[-which(is.na(data_all$cc_proj_value_mean)), ]
  # data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]
  # 
  # data_all$result.1<-sample(data_all$cc_proj_value_mean)
  # data_all$result.2<-sample(data_all$cc_proj_value_mean)
  # data_all$result.3<-sample(data_all$cc_proj_value_mean)
  # data_all$result.4<-sample(data_all$cc_proj_value_mean)
  # data_all$result.5<-sample(data_all$cc_proj_value_mean)
  #   
  # data_all<-data_all%>%select(result.1, result.2, result.3,result.4,result.5)
  # n_boot<-5
  # 
  ##########
  
  data_all<-data_all[-which(is.na(data_all$result.1)), ]
  data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]
  
  data_all$gdlcode<-data_int$gdlcode
  data_all$iso3<-data_int$iso3
  data_all$year<-data_int$year
  data_all$value_interp<-data_int$value_interp
  data_all$ssp<-data_int$ssp
  data_all$model<-data_int$model
  data_all$pop_gdl<-data_int$pop_gdl
  data_all$pop_country<-data_int$pop_country
  
  
  ssp_models<-data_all%>%select(ssp , model)
  ssp_models<-unique(ssp_models)
  ssp_models<-ssp_models%>%group_by(model)%>%mutate(n=n())
  
  models_keep<-unique(ssp_models$model[which(ssp_models$n==4)])
  
  data_all<-data_all%>%filter(model %in% models_keep)
  ssp_match=data.frame(ssp=unique(data_all$ssp), numeric_ssp=c(1:4))
  model_match=data.frame(model=unique(data_all$model), numeric_model=c(1:5))
  data_all<-left_join(data_all,ssp_match )
  data_all<-left_join(data_all,model_match )
  
  
  for (type in resp_types){
    
    if(type=="diff"){
      data<-data_all
      data[1:n_boot] <- data[1:n_boot] - data$value_interp
    }
    
    if(type=="perc_diff"){
      data<-data_all
      data[1:n_boot] <- 100*(data[1:n_boot] - data$value_interp)/ data$value_interp
    }
    
    if(type=="cc_values"){
      data<-data_all
    }
    
  
    ################################################################################
    # iso3 
    
    data_iso<-data %>%
      group_by(iso3, ssp, model, year) %>%
      summarise(across(result.1:result.1000, ~weighted.mean(. ,w=pop_gdl)))
    
    data_iso<-left_join(data_iso,ssp_match )
    data_iso<-left_join(data_iso,model_match )
    
    write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('country_agg','_', o,'_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
    
    # glob
    
    data_glob<-data %>%
      group_by(ssp, model, year) %>%
      summarise(across(result.1:result.1000, ~weighted.mean(. ,w=pop_gdl)))
    
    
    data_glob<-left_join(data_glob,ssp_match )
    data_glob<-left_join(data_glob,model_match )
    write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('glob_agg','_', o,'_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
    
  }   
}
    