### try different plots for projected damages 

library(readr)
library(dplyr)
library(ggplot2)

out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_variables<-c( "leb", "eys", "gnipc"  )

################################################################################

### lag models plot with 

for(out_var in out_variables ){
  
  var_name<-out_var
  
  out_dir_lag<-file.path(out_dir_pop, "lag_models")
  if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}
  
  specifications<-c("mean_mod") #   ,"mean_mod_spec")
  types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
  
  vars_in_proj<-c( "all_vars", "extr_only")
  
  
  spec_type<-"dlm"
  
  effect<-"growth_eff"
  
  
  #lags number 
  N<-8
  
  spec<-specifications[1]
  type<-types[1]
  vars<-vars_in_proj[1]
  
  
  path<-file.path(out_dir_lag,paste0("gr_",out_var,'_',spec,'_',vars, "_pop_weight_boot_impacts_intervals.feather"))
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
           global_weighted_q66_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q66, pop_gdl, na.rm=TRUE))
  
  
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
           country_weighted_q66_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect_q66, pop_gdl, na.rm=TRUE))
  
  
  
  data_all$model<-as.factor(data_all$model)
  data_all$ssp<-as.factor(data_all$ssp)
  data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))
  
  
  ### global damages plot 
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    labs(
      title = "Global Climate Change Impact",
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
    geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_mean.png")), g, width=15, height=10)
  
  
  ### 95%
  
  # g<-ggplot( data=data_all, aes(x = year ) ) +
  #   geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
  #   geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
  #   geom_line(aes( y = global_weighted_q05_cc_proj_effect, group=model_ssp, color=ssp)) + 
  #   geom_line(aes( y = global_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp ))+
  # labs(
  #     title = "Global Climate Change Impact",
  #     x = "Year",
  #     y = "Loss (cc - baseline)",
  #     color = "ssp",
  #     shape= "model"
  #   ) +
  #   theme_bw()
  # # x11()
  # # print(g)
  # ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_95.png")), g, width=15, height=10)
  # 
  # 
  # g<-ggplot( data=data_all, aes(x = year ) ) +
  #   geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   geom_line(aes( y = global_weighted_q05_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   scale_shape_manual(values=1:nlevels(data_all$model)) +
  #   labs(
  #     title = "Perc Global Climate Change Impact",
  #     x = "Year",
  #     y = "Percentage Loss (cc - baseline)/baseline",
  #     color = "ssp",
  #     shape= "model"
  #   ) +
  #   theme_bw()
  # #x11()
  # #print(g)
  # ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_95.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data.frame(data_all[which(data_all$iso3 %in% selected_countries),])
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year,  y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ) ) +
    geom_line()+
    geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q05_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_95.png")), g, width=15, height=10)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ) ) +
    geom_line()+
    geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q05_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_95.png")), g, width=15, height=10)
  
  
  ### 90%
  # g<-ggplot( data=data_all, aes(x = year ) ) +
  #   geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   geom_line(aes( y = global_weighted_q10_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   scale_shape_manual(values=1:nlevels(data_all$model)) +
  #   labs(
  #     title = "Global Climate Change Impact",
  #     x = "Year",
  #     y = "Loss (cc - baseline)",
  #     color = "ssp",
  #     shape= "model"
  #   ) +
  #   theme_bw()
  # # x11()
  # # print(g)
  # ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_90.png")), g, width=15, height=10)
  # 
  # g<-ggplot( data=data_all, aes(x = year ) ) +
  #   geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   geom_line(aes( y = global_weighted_q10_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
  #   geom_line(aes( y = global_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
  #   scale_shape_manual(values=1:nlevels(data_all$model)) +
  #   labs(
  #     title = "Perc Global Climate Change Impact",
  #     x = "Year",
  #     y = "Percentage Loss (cc - baseline)/baseline",
  #     color = "ssp",
  #     shape= "model"
  #   ) +
  #   theme_bw()
  # #x11()
  # #print(g)
  # ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_90.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year , y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model) ) +
    geom_line() + 
    geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q10_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_90.png")), g, width=15, height=10)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year ,  y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model) ) +
    geom_line() + 
    geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q10_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_90.png")), g, width=15, height=10)
  
  
  ### 33%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = global_weighted_q33_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = global_weighted_q66_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_33.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = global_weighted_q33_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = global_weighted_q66_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_33.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year , y = country_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp, linetype=model) ) +
    geom_line() + 
    geom_line(aes( y = country_weighted_med_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q33_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q66_cc_proj_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam_int_33.png")), g, width=15, height=10)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year , y = country_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model) ) +
    geom_line() + 
    geom_line(aes( y = country_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    geom_line(aes( y = country_weighted_q33_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    geom_line(aes( y = country_weighted_q66_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model ))+
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_perc_dam_int_33.png")), g, width=15, height=10)
  
  
  ##############################################################################
  
  ### global damages plot by model
  
  
  ### 95%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q05_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q95_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_95_by_mod.png")), g, width=15, height=10)
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q05_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q95_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_95_by_mod.png")), g, width=15, height=10)
  
  
  
  
  ### 90%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q10_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q90_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_90_by_mod.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q10_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q90_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_90_by_mod.png")), g, width=15, height=10)
  
  
  
  
  ### 33%
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q33_cc_proj_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q66_cc_proj_effect, group=model_ssp, color=ssp ))+
    facet_wrap(~model)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  # x11()
  # print(g)
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam_int_33_by_mod.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_mean_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_med_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
    geom_line(aes( y = global_weighted_q33_cc_proj_perc_effect, group=model_ssp, color=ssp)) + 
    geom_line(aes( y = global_weighted_q66_cc_proj_perc_effect, group=model_ssp, color=ssp ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam_int_33_by_mod.png")), g, width=15, height=10)
  
  
  
  
  
}


