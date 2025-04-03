### try different plots for projected damages 

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

################################################################################

################################################################################

### lag models plot with 

for(out_var in out_variables ){
  
  var_name<-out_var
  
  
  
  path<-file.path(out_dir_lag,paste0("gr_",out_var,'_',spec,'_',vars, "_pop_weight","_nlags",N,"_boot_impacts_intervals.feather"))
  data_all<- arrow::read_feather(path)
  
  pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
  colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
  gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
  pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
  pop_projection_gdlcode<-pop_projection_gdlcode%>%
    group_by(iso3, year, ssp)%>%
    mutate(pop_country=sum(pop_gdl))
  pop_projection_gdlcode<-unique(pop_projection_gdlcode)
  
  data_all<-inner_join(data_all, pop_projection_gdlcode )
  
  
  # add effects, perc effects wrt baseline, of different quantiles
  data_all<-data_all %>% 
    arrange(model, ssp, gdlcode, year)%>% 
    group_by(model, ssp, gdlcode) %>%
    # mean
    mutate(cc_proj_perc_effect_mean = 100*(cc_proj_value_mean -  value_interp)/value_interp, 
           cc_proj_effect_mean = cc_proj_value_mean -  value_interp ) %>%
    # med
    mutate(cc_proj_perc_effect_med = 100*(cc_proj_value_med -  value_interp)/value_interp, 
           cc_proj_effect_med = cc_proj_value_med -  value_interp ) %>%
    # q05
    mutate(cc_proj_perc_effect_q05 = 100*(cc_proj_value_q05 -  value_interp)/value_interp, 
           cc_proj_effect_q05 = cc_proj_value_q05 -  value_interp ) %>%
    # q95
    mutate(cc_proj_perc_effect_q95 = 100*(cc_proj_value_q95 -  value_interp)/value_interp, 
           cc_proj_effect_q95 = cc_proj_value_q95 -  value_interp ) %>%
    # q10
    mutate(cc_proj_perc_effect_q10 = 100*(cc_proj_value_q10 -  value_interp)/value_interp, 
           cc_proj_effect_q10 = cc_proj_value_q10 -  value_interp ) %>%
    # q90
    mutate(cc_proj_perc_effect_q90 = 100*(cc_proj_value_q90 -  value_interp)/value_interp, 
           cc_proj_effect_q90 = cc_proj_value_q90 -  value_interp ) %>%
    # q33
    mutate(cc_proj_perc_effect_q33 = 100*(cc_proj_value_q33 -  value_interp)/value_interp, 
           cc_proj_effect_q33 = cc_proj_value_q33 -  value_interp ) %>%
    # q66
    mutate(cc_proj_perc_effect_q66 = 100*(cc_proj_value_q66 -  value_interp)/value_interp, 
           cc_proj_effect_q66 = cc_proj_value_q66 -  value_interp ) %>%
    # q17
    mutate(cc_proj_perc_effect_q17 = 100*(cc_proj_value_q17 -  value_interp)/value_interp, 
           cc_proj_effect_q17 = cc_proj_value_q17 -  value_interp ) %>%
    # q83
    mutate(cc_proj_perc_effect_q83 = 100*(cc_proj_value_q83 -  value_interp)/value_interp, 
           cc_proj_effect_q83 = cc_proj_value_q83 -  value_interp ) %>%
    ungroup()
  
  
  # aggregate using sub_pop
  data_all<-data_all %>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year) %>%
    mutate(global_weighted_value_interp=weighted.mean(value_interp, pop_gdl, na.rm=TRUE))%>%
    # mean
    mutate(global_weighted_mean_cc_proj_value=weighted.mean(cc_proj_value_mean, pop_gdl, na.rm=TRUE), 
           global_weighted_mean_cc_proj_effect=weighted.mean(cc_proj_effect_mean, pop_gdl, na.rm=TRUE),
           global_weighted_mean_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_mean, pop_gdl, na.rm=TRUE))%>%
    # med
    mutate(global_weighted_med_cc_proj_value=weighted.mean(cc_proj_value_med, pop_gdl, na.rm=TRUE), 
           global_weighted_med_cc_proj_effect=weighted.mean(cc_proj_effect_med, pop_gdl, na.rm=TRUE),
           global_weighted_med_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_med, pop_gdl, na.rm=TRUE))%>%
    # q05
    mutate(global_weighted_q05_cc_proj_value=weighted.mean(cc_proj_value_q05, pop_gdl, na.rm=TRUE), 
           global_weighted_q05_cc_proj_effect=weighted.mean(cc_proj_effect_q05, pop_gdl, na.rm=TRUE),
           global_weighted_q05_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q05, pop_gdl, na.rm=TRUE))%>%
    # q95
    mutate(global_weighted_q95_cc_proj_value=weighted.mean(cc_proj_value_q95, pop_gdl, na.rm=TRUE), 
           global_weighted_q95_cc_proj_effect=weighted.mean(cc_proj_effect_q95, pop_gdl, na.rm=TRUE),
           global_weighted_q95_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q95, pop_gdl, na.rm=TRUE))%>%
    # q10
    mutate(global_weighted_q10_cc_proj_value=weighted.mean(cc_proj_value_q10, pop_gdl, na.rm=TRUE), 
           global_weighted_q10_cc_proj_effect=weighted.mean(cc_proj_effect_q10, pop_gdl, na.rm=TRUE),
           global_weighted_q10_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q10, pop_gdl, na.rm=TRUE))%>%
    # q90
    mutate(global_weighted_q90_cc_proj_value=weighted.mean(cc_proj_value_q90, pop_gdl, na.rm=TRUE), 
           global_weighted_q90_cc_proj_effect=weighted.mean(cc_proj_effect_q90, pop_gdl, na.rm=TRUE),
           global_weighted_q90_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q90, pop_gdl, na.rm=TRUE))%>%
    # q33
    mutate(global_weighted_q33_cc_proj_value=weighted.mean(cc_proj_value_q33, pop_gdl, na.rm=TRUE), 
           global_weighted_q33_cc_proj_effect=weighted.mean(cc_proj_effect_q33, pop_gdl, na.rm=TRUE),
           global_weighted_q33_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q33, pop_gdl, na.rm=TRUE))%>%
    # q66
    mutate(global_weighted_q66_cc_proj_value=weighted.mean(cc_proj_value_q66, pop_gdl, na.rm=TRUE), 
           global_weighted_q66_cc_proj_effect=weighted.mean(cc_proj_effect_q66, pop_gdl, na.rm=TRUE),
           global_weighted_q66_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q66, pop_gdl, na.rm=TRUE))%>%
    # q17
    mutate(global_weighted_q17_cc_proj_value=weighted.mean(cc_proj_value_q17, pop_gdl, na.rm=TRUE), 
           global_weighted_q17_cc_proj_effect=weighted.mean(cc_proj_effect_q17, pop_gdl, na.rm=TRUE),
           global_weighted_q17_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q17, pop_gdl, na.rm=TRUE))%>%
    # q83
    mutate(global_weighted_q83_cc_proj_value=weighted.mean(cc_proj_value_q83, pop_gdl, na.rm=TRUE), 
           global_weighted_q83_cc_proj_effect=weighted.mean(cc_proj_effect_q83, pop_gdl, na.rm=TRUE),
           global_weighted_q83_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q83, pop_gdl, na.rm=TRUE))
  
  
  data_all<-data_all %>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year, iso3) %>%
    mutate(country_weighted_value_interp=weighted.mean(value_interp, pop_gdl, na.rm=TRUE))%>%
    # mean
    mutate(country_weighted_mean_cc_proj_value=weighted.mean(cc_proj_value_mean, pop_gdl, na.rm=TRUE), 
           country_weighted_mean_cc_proj_effect=weighted.mean(cc_proj_effect_mean, pop_gdl, na.rm=TRUE),
           country_weighted_mean_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_mean, pop_gdl, na.rm=TRUE))%>%
    # med
    mutate(country_weighted_med_cc_proj_value=weighted.mean(cc_proj_value_med, pop_gdl, na.rm=TRUE), 
           country_weighted_med_cc_proj_effect=weighted.mean(cc_proj_effect_med, pop_gdl, na.rm=TRUE),
           country_weighted_med_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_med, pop_gdl, na.rm=TRUE))%>%
    # q05
    mutate(country_weighted_q05_cc_proj_value=weighted.mean(cc_proj_value_q05, pop_gdl, na.rm=TRUE), 
           country_weighted_q05_cc_proj_effect=weighted.mean(cc_proj_effect_q05, pop_gdl, na.rm=TRUE),
           country_weighted_q05_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q05, pop_gdl, na.rm=TRUE))%>%
    # q95
    mutate(country_weighted_q95_cc_proj_value=weighted.mean(cc_proj_value_q95, pop_gdl, na.rm=TRUE), 
           country_weighted_q95_cc_proj_effect=weighted.mean(cc_proj_effect_q95, pop_gdl, na.rm=TRUE),
           country_weighted_q95_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q95, pop_gdl, na.rm=TRUE))%>%
    # q10
    mutate(country_weighted_q10_cc_proj_value=weighted.mean(cc_proj_value_q10, pop_gdl, na.rm=TRUE), 
           country_weighted_q10_cc_proj_effect=weighted.mean(cc_proj_effect_q10, pop_gdl, na.rm=TRUE),
           country_weighted_q10_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q10, pop_gdl, na.rm=TRUE))%>%
    # q90
    mutate(country_weighted_q90_cc_proj_value=weighted.mean(cc_proj_value_q90, pop_gdl, na.rm=TRUE), 
           country_weighted_q90_cc_proj_effect=weighted.mean(cc_proj_effect_q90, pop_gdl, na.rm=TRUE),
           country_weighted_q90_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q90, pop_gdl, na.rm=TRUE))%>%
    # q33
    mutate(country_weighted_q33_cc_proj_value=weighted.mean(cc_proj_value_q33, pop_gdl, na.rm=TRUE), 
           country_weighted_q33_cc_proj_effect=weighted.mean(cc_proj_effect_q33, pop_gdl, na.rm=TRUE),
           country_weighted_q33_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q33, pop_gdl, na.rm=TRUE))%>%
    # q66
    mutate(country_weighted_q66_cc_proj_value=weighted.mean(cc_proj_value_q66, pop_gdl, na.rm=TRUE), 
           country_weighted_q66_cc_proj_effect=weighted.mean(cc_proj_effect_q66, pop_gdl, na.rm=TRUE),
           country_weighted_q66_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q66, pop_gdl, na.rm=TRUE))%>%
    # q17
    mutate(country_weighted_q17_cc_proj_value=weighted.mean(cc_proj_value_q17, pop_gdl, na.rm=TRUE), 
           country_weighted_q17_cc_proj_effect=weighted.mean(cc_proj_effect_q17, pop_gdl, na.rm=TRUE),
           country_weighted_q17_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q17, pop_gdl, na.rm=TRUE))%>%
    # q83
    mutate(country_weighted_q83_cc_proj_value=weighted.mean(cc_proj_value_q83, pop_gdl, na.rm=TRUE), 
           country_weighted_q83_cc_proj_effect=weighted.mean(cc_proj_effect_q83, pop_gdl, na.rm=TRUE),
           country_weighted_q83_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q83, pop_gdl, na.rm=TRUE))
  
  
  
  data_all$model<-as.factor(data_all$model)
  data_all$ssp<-as.factor(data_all$ssp)
  data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))
  
  
  ### global damages plot 
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    #geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    labs(
      
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_mean.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    #geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    labs(
      
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_mean.png")), g, width=15, height=10)
  
  
  ###
  
  ### 95%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q05_cc_proj_effect, ymax = global_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    
    #geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    #geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    #geom_line(aes( y = global_weighted_q05_cc_proj_effect, group=model_ssp, color=ssp)) + 
    #geom_line(aes( y = global_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_95.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q05_cc_proj_perc_effect, ymax = global_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    
    # geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = global_weighted_q05_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Perc Global Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_95.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q05_cc_proj_effect, ymax = country_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q05_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_95.png")), g, width=30, height=25)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q05_cc_proj_perc_effect, ymax = country_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    #
    # geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q05_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_95.png")), g, width=30, height=25)
  
  
  ### 90%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q10_cc_proj_effect, ymax = global_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    
    # geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = global_weighted_q10_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q10_cc_proj_perc_effect, ymax = global_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    
    # geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = global_weighted_q10_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Perc Global Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_90.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q10_cc_proj_effect, ymax = country_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q10_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_90.png")), g, width=30, height=25)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q10_cc_proj_perc_effect, ymax = country_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q10_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_90.png")), g, width=30, height=25)
  
  
  ### 66%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q17_cc_proj_effect, ymax = global_weighted_q83_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    
    # geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = global_weighted_q17_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_q83_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_66.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_ribbon(aes(ymin = global_weighted_q17_cc_proj_perc_effect, ymax = global_weighted_q83_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = global_weighted_q17_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = global_weighted_q83_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Perc Global Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_66.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q17_cc_proj_effect, ymax = country_weighted_q83_cc_proj_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q17_cc_proj_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q83_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_66.png")), g, width=30, height=25)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year ) ) +
    geom_ribbon(aes(ymin = country_weighted_q17_cc_proj_perc_effect, ymax = country_weighted_q83_cc_proj_perc_effect, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
    geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp), size=1) + 
    # 
    # geom_line(aes( y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    # geom_line(aes( y = country_weighted_q17_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    # geom_line(aes( y = country_weighted_q83_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3 + model) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_66.png")), g, width=30, height=25)
  
  
}


################################################################################
################################################################################
################################################################################
rm(g)

gc()

# create hdi projections 

data_proj_gni<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "gnipc", ".csv")))
length(unique(data_proj_gni$iso3))
data_proj_leb<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "leb", ".csv")))
length(unique(data_proj_leb$iso3))
data_proj_eys<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", "eys", ".csv")))
length(unique(data_proj_eys$iso3))


common_iso<-intersect(intersect(unique(data_proj_eys$iso3), unique(data_proj_gni$iso3)), unique(data_proj_leb$iso3))
data_proj_gni<-data_proj_gni%>%filter(iso3 %in% common_iso)
data_proj_leb<-data_proj_leb%>%filter(iso3 %in% common_iso)
data_proj_eys<-data_proj_eys%>%filter(iso3 %in% common_iso)

# max min values calculated over all horizon 1990-2100
# max min log gnipc
data_proj_gni$lgnipc<-log(data_proj_gni$value_interp)
max_lgnipc<-max(data_proj_gni$lgnipc, na.rm=TRUE)
min_lgnipc<-min(data_proj_gni$lgnipc, na.rm=TRUE)
# max min eys
max_eys<-max(data_proj_eys$value_interp, na.rm=TRUE)
min_eys<-min(data_proj_eys$value_interp, na.rm=TRUE)
# max min log gnipc
max_leb<-max(data_proj_leb$value_interp, na.rm=TRUE)
min_leb<-min(data_proj_leb$value_interp, na.rm=TRUE)


#############################


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

###

pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
pop_projection_gdlcode<-pop_projection_gdlcode%>%
  group_by(iso3, year, ssp)%>%
  mutate(pop_country=sum(pop_gdl))
pop_projection_gdlcode<-unique(pop_projection_gdlcode)


### 

data_all_gni<-arrow::read_feather(file.path(out_dir_lag,paste0("gr_gnipc",'_',spec,'_',vars, "_pop_weight","_nlags",N,"_boot_impacts_intervals.feather")))
data_all_gni<-inner_join(data_all_gni, pop_projection_gdlcode )
data_all_gni<-data_all_gni[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,
                              "value_interp", "cc_proj_value_mean","cc_proj_value_med","cc_proj_value_q10","cc_proj_value_q90","cc_proj_value_q17","cc_proj_value_q83")]
data_all_gni$log_value_interp<-log(data_all_gni$value_interp)
data_all_gni$log_cc_proj_value_mean<-log(data_all_gni$cc_proj_value_mean)
data_all_gni$log_cc_proj_value_med<-log(data_all_gni$cc_proj_value_med)
data_all_gni$log_cc_proj_value_q10<-log(data_all_gni$cc_proj_value_q10)
data_all_gni$log_cc_proj_value_q90<-log(data_all_gni$cc_proj_value_q90)
data_all_gni$log_cc_proj_value_q17<-log(data_all_gni$cc_proj_value_q17)
data_all_gni$log_cc_proj_value_q83<-log(data_all_gni$cc_proj_value_q83)

data_all_gni$income_index<-(data_all_gni$log_value_interp-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_mean<-(data_all_gni$log_cc_proj_value_mean-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_med<-(data_all_gni$log_cc_proj_value_med-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_q10<-(data_all_gni$log_cc_proj_value_q10-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_q90<-(data_all_gni$log_cc_proj_value_q90-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_q17<-(data_all_gni$log_cc_proj_value_q17-min_lgnipc)/(max_lgnipc-min_lgnipc)
data_all_gni$cc_income_index_q83<-(data_all_gni$log_cc_proj_value_q83-min_lgnipc)/(max_lgnipc-min_lgnipc)

data_all_leb<-arrow::read_feather(file.path(out_dir_lag,paste0("gr_leb","_",spec,'_',vars, "_pop_weight","_nlags",N,"_boot_impacts_intervals.feather")))
data_all_leb<-inner_join(data_all_leb, pop_projection_gdlcode )
data_all_leb<-data_all_leb[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,
                              "value_interp", "cc_proj_value_mean","cc_proj_value_med","cc_proj_value_q10","cc_proj_value_q90","cc_proj_value_q17","cc_proj_value_q83")]
data_all_leb$lifex_index<-(data_all_leb$value_interp-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_mean<-(data_all_leb$cc_proj_value_mean-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_med<-(data_all_leb$cc_proj_value_med-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_med<-(data_all_leb$cc_proj_value_med-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_q10<-(data_all_leb$cc_proj_value_q10-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_q90<-(data_all_leb$cc_proj_value_q90-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_q17<-(data_all_leb$cc_proj_value_q17-min_leb)/(max_leb-min_leb)
data_all_leb$cc_lifex_index_q83<-(data_all_leb$cc_proj_value_q83-min_leb)/(max_leb-min_leb)

data_all_eys<-arrow::read_feather(file.path(out_dir_lag,paste0("gr_eys",'_',spec,'_',vars, "_pop_weight","_nlags",N,"_boot_impacts_intervals.feather")))
data_all_eys<-inner_join(data_all_eys, pop_projection_gdlcode )
data_all_eys<-data_all_eys[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,
                              "value_interp", "cc_proj_value_mean","cc_proj_value_med","cc_proj_value_q10","cc_proj_value_q90","cc_proj_value_q17","cc_proj_value_q83")]
data_all_eys$edu_index<-(data_all_eys$value_interp-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_mean<-(data_all_eys$cc_proj_value_mean-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_med <-(data_all_eys$cc_proj_value_med-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_med <-(data_all_eys$cc_proj_value_med-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_q10 <-(data_all_eys$cc_proj_value_q10-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_q90 <-(data_all_eys$cc_proj_value_q90-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_q17 <-(data_all_eys$cc_proj_value_q17-min_eys)/(max_eys-min_eys)
data_all_eys$cc_edu_index_q83 <-(data_all_eys$cc_proj_value_q83-min_eys)/(max_eys-min_eys)


data_all_gni<-data_all_gni%>%filter(iso3 %in% common_iso)
data_all_leb<-data_all_leb%>%filter(iso3 %in% common_iso)
data_all_eys<-data_all_eys%>%filter(iso3 %in% common_iso)


data_all_gni$value_interp<-NULL
data_all_gni$cc_proj_value_mean<-NULL
data_all_gni$cc_proj_value_med <-NULL
data_all_gni$cc_proj_value_med <-NULL
data_all_gni$cc_proj_value_q10 <-NULL
data_all_gni$cc_proj_value_q90 <-NULL
data_all_gni$cc_proj_value_q17 <-NULL
data_all_gni$cc_proj_value_q83 <-NULL

data_all_gni$log_value_interp<-NULL
data_all_gni$log_cc_proj_value_mean<-NULL
data_all_gni$log_cc_proj_value_med <-NULL
data_all_gni$log_cc_proj_value_med <-NULL
data_all_gni$log_cc_proj_value_q10 <-NULL
data_all_gni$log_cc_proj_value_q90 <-NULL
data_all_gni$log_cc_proj_value_q17 <-NULL
data_all_gni$log_cc_proj_value_q83 <-NULL

data_all_leb$value_interp<-NULL
data_all_leb$cc_proj_value_mean<-NULL
data_all_leb$cc_proj_value_med <-NULL
data_all_leb$cc_proj_value_med <-NULL
data_all_leb$cc_proj_value_q10 <-NULL
data_all_leb$cc_proj_value_q90 <-NULL
data_all_leb$cc_proj_value_q17 <-NULL
data_all_leb$cc_proj_value_q83 <-NULL

data_all_eys$value_interp<-NULL
data_all_eys$cc_proj_value_mean<-NULL
data_all_eys$cc_proj_value_med <-NULL
data_all_eys$cc_proj_value_med <-NULL
data_all_eys$cc_proj_value_q10 <-NULL
data_all_eys$cc_proj_value_q90 <-NULL
data_all_eys$cc_proj_value_q17 <-NULL
data_all_eys$cc_proj_value_q83 <-NULL

# combine
data_all<-inner_join(data_all_gni, data_all_leb)
data_all<-inner_join(data_all, data_all_eys)

summary(data_all$income_index)
summary(data_all$lifex_index)
summary(data_all$edu_index)


hist(log(data_all$income_index))
hist(log(data_all$lifex_index))
hist(log(data_all$edu_index))

data_all$hdi<-(data_all$income_index*data_all$lifex_index*data_all$edu_index)^(1/3)

data_all$cc_hdi_mean<-(data_all$cc_income_index_mean*data_all$cc_lifex_index_mean*data_all$cc_edu_index_mean)^(1/3)
data_all$cc_hdi_med<-(data_all$cc_income_index_med*data_all$cc_lifex_index_med*data_all$cc_edu_index_med)^(1/3)
# data_all$cc_hdi_q10<-(data_all$cc_income_index_q10*data_all$cc_lifex_index_q10*data_all$cc_edu_index_q10)^(1/3)
# data_all$cc_hdi_q90<-(data_all$cc_income_index_q90*data_all$cc_lifex_index_q90*data_all$cc_edu_index_q90)^(1/3)
# data_all$cc_hdi_q17<-(data_all$cc_income_index_q17*data_all$cc_lifex_index_q17*data_all$cc_edu_index_q17)^(1/3)
# data_all$cc_hdi_q83<-(data_all$cc_income_index_q83*data_all$cc_lifex_index_q83*data_all$cc_edu_index_q83)^(1/3)

### delta method for ci hdi (geometric mean of three normal distributed vars)
# Compute log-transformed means
log_mean_income <- log(data_all$cc_income_index_mean)
log_mean_lifex  <- log(data_all$cc_lifex_index_mean)
log_mean_edu    <- log(data_all$cc_edu_index_mean)

# Compute standard errors using the quantiles (approximate method)
se_log_income <- (log(data_all$cc_income_index_q90) - log(data_all$cc_income_index_q10)) / (2 * 1.645)  # 90% CI
se_log_lifex  <- (log(data_all$cc_lifex_index_q90)  - log(data_all$cc_lifex_index_q10))  / (2 * 1.645)
se_log_edu    <- (log(data_all$cc_edu_index_q90)    - log(data_all$cc_edu_index_q10))    / (2 * 1.645)

# Delta method variance approximation
var_log_hdi <- (1/9) * (se_log_income^2 + se_log_lifex^2 + se_log_edu^2)
se_log_hdi  <- sqrt(var_log_hdi)

# Compute confidence intervals in log-space (using 1.645 for 90% CI)
log_hdi_mean <- (log_mean_income + log_mean_lifex + log_mean_edu) / 3
log_hdi_lower <- log_hdi_mean - 1.645 * se_log_hdi  # Adjusted for 90% CI
log_hdi_upper <- log_hdi_mean + 1.645 * se_log_hdi

# Convert back to original scale
data_all$cc_hdi_q10 <- exp(log_hdi_lower)
data_all$cc_hdi_q90 <- exp(log_hdi_upper)


data_all$delta_hdi_mean<-data_all$cc_hdi_mean-data_all$hdi
data_all$delta_hdi_med<-data_all$cc_hdi_med-data_all$hdi
data_all$delta_hdi_q10<-data_all$cc_hdi_q10-data_all$hdi
data_all$delta_hdi_q90<-data_all$cc_hdi_q90-data_all$hdi
# data_all$delta_hdi_q17<-data_all$cc_hdi_q17-data_all$hdi
# data_all$delta_hdi_q83<-data_all$cc_hdi_q83-data_all$hdi

data_all$delta_income_index_mean<-data_all$cc_income_index_mean-data_all$income_index
data_all$delta_income_index_med<-data_all$cc_income_index_med-data_all$income_index
data_all$delta_income_index_q10<-data_all$cc_income_index_q10-data_all$income_index
data_all$delta_income_index_q90<-data_all$cc_income_index_q90-data_all$income_index
# data_all$delta_income_index_q17<-data_all$cc_income_index_q17-data_all$income_index
# data_all$delta_income_index_q83<-data_all$cc_income_index_q83-data_all$income_index

data_all$delta_lifex_index_mean<-data_all$cc_lifex_index_mean-data_all$lifex_index
data_all$delta_lifex_index_med<-data_all$cc_lifex_index_med-data_all$lifex_index
data_all$delta_lifex_index_q10<-data_all$cc_lifex_index_q10-data_all$lifex_index
data_all$delta_lifex_index_q90<-data_all$cc_lifex_index_q90-data_all$lifex_index
# data_all$delta_lifex_index_q17<-data_all$cc_lifex_index_q17-data_all$lifex_index
# data_all$delta_lifex_index_q83<-data_all$cc_lifex_index_q83-data_all$lifex_index

data_all$delta_edu_index_mean<-data_all$cc_edu_index_mean-data_all$edu_index
data_all$delta_edu_index_med<-data_all$cc_edu_index_med-data_all$edu_index
data_all$delta_edu_index_q10<-data_all$cc_edu_index_q10-data_all$edu_index
data_all$delta_edu_index_q90<-data_all$cc_edu_index_q90-data_all$edu_index
# data_all$delta_edu_index_q17<-data_all$cc_edu_index_q17-data_all$edu_index
# data_all$delta_edu_index_q83<-data_all$cc_edu_index_q83-data_all$edu_index


# significance : agreement in 90% damage CI across at least 2/3 of climate models 
data_all <- data_all %>%
  mutate(same_sign_hdi_90 = sign(delta_hdi_q90) == sign(delta_hdi_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_hdi_90 = as.integer(mean(same_sign_hdi_90) >= 2/3), .groups = "drop") %>%
  right_join(data_all, by = c("gdlcode", "year", "ssp"))  
data_all <- data_all %>%
  mutate(same_sign_income_index_90 = sign(delta_income_index_q90) == sign(delta_income_index_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_income_index_90 = as.integer(mean(same_sign_income_index_90) >= 2/3), .groups = "drop") %>%
  right_join(data_all, by = c("gdlcode", "year", "ssp"))  
data_all <- data_all %>%
  mutate(same_sign_lifex_index_90 = sign(delta_lifex_index_q90) == sign(delta_lifex_index_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_lifex_index_90 = as.integer(mean(same_sign_lifex_index_90) >= 2/3), .groups = "drop") %>%
  right_join(data_all, by = c("gdlcode", "year", "ssp"))  
data_all <- data_all %>%
  mutate(same_sign_edu_index_90 = sign(delta_edu_index_q90) == sign(delta_edu_index_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_edu_index_90 = as.integer(mean(same_sign_edu_index_90) >= 2/3), .groups = "drop") %>%
  right_join(data_all, by = c("gdlcode", "year", "ssp")) 

# summarize in world
gc()
data_all <- data_all %>% 
  arrange(model, ssp, year) %>% 
  group_by(model, ssp, year) %>%
  
  # Compute weighted mean values
  mutate(global_weighted_hdi = weighted.mean(hdi, pop_gdl, na.rm = TRUE),
         global_weighted_income_index = weighted.mean(income_index, pop_gdl, na.rm = TRUE),
         global_weighted_lifex_index = weighted.mean(lifex_index, pop_gdl, na.rm = TRUE),
         global_weighted_edu_index = weighted.mean(edu_index, pop_gdl, na.rm = TRUE),
         global_weighted_cc_hdi_mean = weighted.mean(cc_hdi_mean, pop_gdl, na.rm = TRUE),
         global_weighted_cc_income_index_mean = weighted.mean(cc_income_index_mean, pop_gdl, na.rm = TRUE),
         global_weighted_cc_lifex_index_mean = weighted.mean(cc_lifex_index_mean, pop_gdl, na.rm = TRUE),
         global_weighted_cc_edu_index_mean = weighted.mean(cc_edu_index_mean, pop_gdl, na.rm = TRUE),
         global_weighted_delta_hdi_mean = weighted.mean(delta_hdi_mean, pop_gdl, na.rm = TRUE),
         global_weighted_delta_income_index_mean = weighted.mean(delta_income_index_mean, pop_gdl, na.rm = TRUE),
         global_weighted_delta_lifex_index_mean = weighted.mean(delta_lifex_index_mean, pop_gdl, na.rm = TRUE),
         global_weighted_delta_edu_index_mean = weighted.mean(delta_edu_index_mean, pop_gdl, na.rm = TRUE)) %>%
  
  # Compute standard deviations from confidence intervals (90% CI)
  mutate(delta_hdi_sd = (delta_hdi_q90 - delta_hdi_q10) / (2 * 1.645),
         delta_income_index_sd = (delta_income_index_q90 - delta_income_index_q10) / (2 * 1.645),
         delta_lifex_index_sd = (delta_lifex_index_q90 - delta_lifex_index_q10) / (2 * 1.645),
         delta_edu_index_sd = (delta_edu_index_q90 - delta_edu_index_q10) / (2 * 1.645)) %>%
  
  # Compute weighted variance for global uncertainty propagation
  mutate(global_delta_hdi_sd = sqrt(sum((pop_gdl / sum(pop_gdl))^2 * delta_hdi_sd^2, na.rm = TRUE)),
         global_delta_income_index_sd = sqrt(sum((pop_gdl / sum(pop_gdl))^2 * delta_income_index_sd^2, na.rm = TRUE)),
         global_delta_lifex_index_sd = sqrt(sum((pop_gdl / sum(pop_gdl))^2 * delta_lifex_index_sd^2, na.rm = TRUE)),
         global_delta_edu_index_sd = sqrt(sum((pop_gdl / sum(pop_gdl))^2 * delta_edu_index_sd^2, na.rm = TRUE))) %>%
  
  # Compute the final global confidence intervals (90% CI)
  mutate(global_weighted_delta_hdi_q10 = global_weighted_delta_hdi_mean - 1.645 * global_delta_hdi_sd,
         global_weighted_delta_hdi_q90 = global_weighted_delta_hdi_mean + 1.645 * global_delta_hdi_sd,
         global_weighted_delta_income_index_q10 = global_weighted_delta_income_index_mean - 1.645 * global_delta_income_index_sd,
         global_weighted_delta_income_index_q90 = global_weighted_delta_income_index_mean + 1.645 * global_delta_income_index_sd,
         global_weighted_delta_lifex_index_q10 = global_weighted_delta_lifex_index_mean - 1.645 * global_delta_lifex_index_sd,
         global_weighted_delta_lifex_index_q90 = global_weighted_delta_lifex_index_mean + 1.645 * global_delta_lifex_index_sd,
         global_weighted_delta_edu_index_q10 = global_weighted_delta_edu_index_mean - 1.645 * global_delta_edu_index_sd,
         global_weighted_delta_edu_index_q90 = global_weighted_delta_edu_index_mean + 1.645 * global_delta_edu_index_sd)

# write.csv(data_all, file=file.path(out_dir,paste0("hdi_",spec,'_',vars,"_", "projected_damage_boot_intervals.csv")))

### median across models

data_all<-data_all%>%
  group_by( gdlcode, iso3, ssp, year )%>%
  mutate(median_cc_hdi=median(cc_hdi_mean),
         median_cc_income_index=median(cc_income_index_mean),
         median_cc_lifex_index=median(cc_lifex_index_mean),
         median_cc_edu_index=median(cc_edu_index_mean),
         median_delta_hdi=median(delta_hdi_mean), 
         median_delta_income_index=median(delta_income_index_mean), 
         median_delta_lifex_index=median(delta_lifex_index_mean), 
         median_delta_edu_index=median(delta_edu_index_mean), 
         median_delta_hdi_q10=median(delta_hdi_q10), 
         median_delta_edu_index_q10=median(delta_edu_index_q10), 
         median_delta_income_index_q10=median(delta_income_index_q10), 
         median_delta_lifex_index_q10=median(delta_lifex_index_q10),
         median_delta_hdi_q90=median(delta_hdi_q90), 
         median_delta_edu_index_q90=median(delta_edu_index_q90), 
         median_delta_income_index_q90=median(delta_income_index_q90), 
         median_delta_lifex_index_q90=median(delta_lifex_index_q90))


gc()
data_median<-data_all%>% select(iso3, gdlcode, ssp, year , 
                                hdi, lifex_index, edu_index, income_index,
                                median_cc_hdi, median_cc_income_index, median_cc_lifex_index, median_cc_edu_index,
                                median_delta_hdi, median_delta_income_index  ,
                                median_delta_lifex_index, median_delta_edu_index, 
                                median_delta_hdi_q10, median_delta_income_index_q10  ,
                                median_delta_lifex_index_q10, median_delta_edu_index_q10, 
                                median_delta_hdi_q90, median_delta_income_index_q90  ,
                                median_delta_lifex_index_q90, median_delta_edu_index_q90, 
                                sign_edu_index_90, sign_income_index_90,sign_lifex_index_90, sign_hdi_90, 
                                pop_gdl) %>% distinct()

write_csv(data_median, file=file.path(out_dir, "data_median_sign.csv"))
#write_csv(data_all, file=file.path(out_dir, "data_all_hdi_proj.csv"))


##############


data_all$model<-as.factor(data_all$model)
data_all$ssp<-as.factor(data_all$ssp)
data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))


# for each model, global weighted hdi course and components 
gc()

### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_hdi_q10*100, ymax = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_hdi_mean*100, group=model_ssp, color=ssp), size=1) + 
  #geom_line(aes( y = global_weighted_delta_hdi_med*100, group=model_ssp, color=ssp ))+
  #geom_line(aes( y = global_weighted_delta_hdi_q10*100, group=model_ssp, color=ssp)) + 
  #geom_line(aes( y = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp ))+
  facet_wrap(~model)+
  labs(
    #
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


### 90%, CI all together 
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_hdi_q10*100, ymax = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.05) +
  geom_line(aes( y = global_weighted_delta_hdi_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  #geom_line(aes( y = global_weighted_delta_hdi_med*100, group=model_ssp, color=ssp ))+
  #geom_line(aes( y = global_weighted_delta_hdi_q10*100, group=model_ssp, color=ssp)) + 
  #geom_line(aes( y = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp ))+
  #facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_conf_int.png")), g, width=12, height=9)




g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_line(aes( y = global_weighted_delta_hdi_mean*100, group=model_ssp, color=ssp, linetype=model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
   labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_mean.png")), g, width=12, height=9)



# gni
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_income_index_q10*100, ymax = global_weighted_delta_income_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_income_index_mean*100, group=model_ssp, color=ssp), size=1) + 
   facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("income_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


# lifex 
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_lifex_index_q10*100, ymax = global_weighted_delta_lifex_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_lifex_index_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g,width=12, height=9)

# edu
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_edu_index_q10*100, ymax = global_weighted_delta_edu_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_edu_index_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("edu_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


############################################################################
### only using significant robust across models 

# for each model, global weighted hdi course and components 
gc()

### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_sign_hdi_q10*100, ymax = global_weighted_delta_sign_hdi_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_sign_hdi_mean*100, group=model_ssp, color=ssp), size=1) + 
  #geom_line(aes( y = global_weighted_delta_hdi_med*100, group=model_ssp, color=ssp ))+
  #geom_line(aes( y = global_weighted_delta_hdi_q10*100, group=model_ssp, color=ssp)) + 
  #geom_line(aes( y = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp ))+
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


### 90%, CI all together , only significant values
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_sign_hdi_q10*100, ymax = global_weighted_delta_sign_hdi_q90*100, group=model_ssp, color=ssp, fill=ssp, linetype = model), size=0.2, alpha = 0.03) +
  geom_line(aes( y = global_weighted_delta_sign_hdi_mean*100, group=model_ssp, color=ssp, linetype = model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  #geom_line(aes( y = global_weighted_delta_hdi_med*100, group=model_ssp, color=ssp ))+
  #geom_line(aes( y = global_weighted_delta_hdi_q10*100, group=model_ssp, color=ssp)) + 
  #geom_line(aes( y = global_weighted_delta_hdi_q90*100, group=model_ssp, color=ssp ))+
  #facet_wrap(~model)+
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90_conf_int.png")), g, width=12, height=9)


g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_line(aes( y = global_weighted_delta_sign_hdi_mean*100, group=model_ssp, color=ssp, linetype=model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_hdi_",spec,'_',vars,"_", "pop_w_glob_cc_dam_mean.png")), g, width=12, height=9)



# gni
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_sign_income_index_q10*100, ymax = global_weighted_delta_sign_income_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_sign_income_index_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_income_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_line(aes( y = global_weighted_delta_sign_income_index_mean*100, group=model_ssp, color=ssp, linetype=model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_income_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_mean.png")), g, width=12, height=9)


# lifex 
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_sign_lifex_index_q10*100, ymax = global_weighted_delta_sign_lifex_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_sign_lifex_index_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_lifex_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_line(aes( y = global_weighted_delta_sign_lifex_index_mean*100, group=model_ssp, color=ssp, linetype=model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_lifex_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_mean.png")), g, width=12, height=9)

# edu
### 90%
g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_ribbon(aes(ymin = global_weighted_delta_sign_edu_index_q10*100, ymax = global_weighted_delta_sign_edu_index_q90*100, group=model_ssp, color=ssp, fill=ssp), alpha = 0.3, linetype = 0) +
  geom_line(aes( y = global_weighted_delta_sign_edu_index_mean*100, group=model_ssp, color=ssp), size=1) + 
  facet_wrap(~model)+
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_edu_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=12, height=9)


g<-ggplot( data=data_all, aes(x = year ) ) +
  geom_line(aes( y = global_weighted_delta_sign_edu_index_mean*100, group=model_ssp, color=ssp, linetype=model), size=1) + 
  scale_shape_manual(values=1:nlevels(data_all$model)) +
  labs(
    
    x = "Year",
    y = "Loss (cc - baseline) in % points",
    color = "ssp",
    shape= "model"
  ) +
  scale_color_brewer(palette = "Dark2", aesthetics = c("colour", "fill") )+
  theme_bw()
# x11()
# print(g)
ggsave(filename=file.path(out_dir, paste0("sign_edu_index_",spec,'_',vars,"_", "pop_w_glob_cc_dam_mean.png")), g, width=12, height=9)



################################################################################





