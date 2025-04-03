### functions for proj damages 


rm(list=ls())
source("utils/libraries.R")

### out dir 

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_area<-"output/projections/original_comp/area_weight"
if(!dir.exists(out_dir_area)){dir.create(out_dir_area)}


### climate data 
gc()
climate_projections<- arrow::read_feather("output/projections/climate_projections_country_area_weight_llgnipc_llgdppc.feather")
gc()
climate_projections<-tibble::as_tibble(climate_projections)
gc()

ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


##################################################################################

project_impacts <- function(var_name, climate_projections, data, coefs, out_dir, maps=FALSE){
  
  colnames(data)[colnames(data)=="Scenario"]<-"ssp"
  
  data<-data %>% 
    arrange(ssp, iso3, year) %>% 
    group_by(ssp, iso3) %>% 
    mutate(gr_value_interp = log(value_interp) - log(dplyr::lag(value_interp, n = 1, default = NA)))
  
  
  expand_data<-expand.grid( year = unique(data$year), 
                            iso3 = unique(data$iso3), 
                            ssp = unique(data$ssp), 
                            model = models_names )
  
  data<-inner_join(data, expand_data)
  
  # coefficients damage function, when significant regional level
  
  vars<-names(coefs)
  
  climate_projections$delta_clim_proj=as.numeric(as.matrix(climate_projections[, c(vars)]) %*% c(coefs))  #rowSums(t(apply(as.matrix(climate_projections[, vars]), 1, function(x) coefs*x)),na.rm=T)
  
  data<-inner_join(data, climate_projections[, c("iso3", "year", "model", "ssp", "delta_clim_proj")])
  
  # set values in first year available to 1
  # y<-min(unique(data$year))
  # data[which(data$year==y), c("gr_value_interp", "delta_clim_proj")]<-0
  data<-data %>%
    filter(year >= 2030)
  y<-min(unique(data$year))
  
  #test to see if method works
  data<-data %>%
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, iso3, year)%>% 
    group_by(model, ssp, iso3) %>%
    mutate(first_value_interp= first(value_interp) ) %>%
    mutate(effect = cumprod(gr_value_interp + 1) ) %>%
    mutate(test_proj_value = if_else(year == y, first_value_interp ,
                                     first_value_interp*effect ) ) %>%
    ungroup()
  
  
  
  data<-data %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, iso3, year)%>% 
    group_by(model, ssp, iso3) %>%
    mutate(first_value_interp= first(value_interp)) %>%
    mutate(effect_cc = cumprod(gr_value_interp + delta_clim_proj + 1)) %>%
    mutate(cc_proj_value = if_else(year == y, first_value_interp ,
                                   first_value_interp*effect_cc ) ) %>%
    mutate(cc_proj_perc_effect = 100*(cc_proj_value -  value_interp)/value_interp, 
           cc_proj_effect = cc_proj_value -  value_interp ) %>%
    ungroup()
  
  # plot(data$cc_proj_effect, data$value_interp)
  
  data<-data %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year) %>%
    mutate(global_weighted_value_interp=weighted.mean(value_interp, pop, na.rm=TRUE), 
           global_weighted_cc_proj_value=weighted.mean(cc_proj_value, pop, na.rm=TRUE) )%>%
    mutate(  global_weighted_cc_proj_perc_effect= 100*(global_weighted_cc_proj_value -global_weighted_value_interp)/global_weighted_value_interp, 
             global_weighted_cc_proj_effect=global_weighted_cc_proj_value - global_weighted_value_interp) 
  
  
  data$model<-as.factor(data$model)
  data$ssp<-as.factor(data$ssp)
  data$model_ssp<-as.factor(paste0(data$model, "_", data$ssp ))
  
  
  ### global damages plot 
  
  g<-ggplot( data=data, aes(x = year, y = global_weighted_cc_proj_effect, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data$model)) +
    geom_line() + 
    geom_point(size=3)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #g
  ggsave(filename=file.path(out_dir, paste0(var_name,"_","pop_weighted_global_damage.png")), g, width=20, height=15)
  
  # perc damages wrt baseline
  g<-ggplot( data=data, aes(x = year, y = global_weighted_cc_proj_perc_effect, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data$model)) +
    geom_line() + 
    geom_point(size=3)+
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_","pop_weighted_perc_global_damage.png")), g, width=20, height=15)
  
  ###
  
  
  # Filter data for some countries
  selected_countries <- c("NOR", "RUS","CAN", 
                          "BRA", "ECU", "DZA", 
                          "AFG", "ZAF", "IND")
  plot_data <- data %>%
    filter(iso3 %in% selected_countries)
  
  ### line plots for some countries 
  
  g<-ggplot() +
    geom_point( data=plot_data, aes(x = year, y = cc_proj_effect,  group=model_ssp, color=ssp, shape=model)) + 
    facet_wrap(~ iso3, scales = "free_y") +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Loss (cc - baseline)",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()+
    theme(legend.position = "bottom") 
  
  ggsave(filename=file.path(out_dir, paste0(var_name,"_","selected_countries_damages.png")), g, width=15, height=10)
  
  # perc
  g<-ggplot() +
    geom_point( data=plot_data, aes(x = year, y = cc_proj_perc_effect,  group=model_ssp, color=ssp, shape=model)) + 
    facet_wrap(~ iso3, scales = "free_y") +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()+
    theme(legend.position = "bottom") 
  
  ggsave(filename=file.path(out_dir, paste0(var_name,"_","selected_countries_damages.png")), g, width=15, height=10)
  
  
  ### plot maps
  if(maps==TRUE){
    ### Countries borders 
    countries<-giscoR::gisco_get_countries()
    #simplify data countries
    countries<-countries[,c(3,6)]
    colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-"iso3"
    countries<-sf::st_as_sf(countries)
    
    years<-c(2030, 2045, 2060 ,2075, 2085, 2100)
    
    for (sc in unique(data$ssp)){
      for (mod in unique(data$model)){
        if( paste0(mod, "_", sc) %in% data$model_ssp ){
          plot_map<-data%>%
            filter(ssp %in% sc)%>%
            filter(model %in% mod)%>%
            filter(year %in% years)
          
          plot_map<-inner_join(plot_map, countries)
          plot_map<-sf::st_as_sf(plot_map)
          
          g<-ggplot(plot_map)+geom_sf( aes(fill=cc_proj_effect))+theme_bw()+  scale_fill_gradient2() + 
            labs(fill=paste0(var_name," change") )+ ggtitle("cc effect wrt baseline") + 
            facet_wrap(~ year) 
          ggsave(filename=file.path(out_dir, paste0(var_name,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
          
        }
      }
    }
  }
  
  
}

#############################################################################

### iteratively, for each lag model, specification (try simple, with controls, with autoreg)

out_dir_lag<-file.path(out_dir_area, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

coefs_dir<-"output/models/original_components_tp_extr_all_models_performances/lag_models_area_weight"

specifications<-c("mean_mod_spec")
types<-c("all_extr","all_extr_adap" )#"all_extr_controls", "all_extr_autoreg")
out_variables<-c("gnipc", "leb","eys", "mys")
#out_variables<-c( "eys")

for (spec in specifications){
  out_dir_lag_spec<-file.path(out_dir_lag, spec)
  if(!dir.exists(out_dir_lag_spec)){dir.create(out_dir_lag_spec)}
  
  for (out_var in out_variables){
    out_dir_lag_spec_var<-file.path(out_dir_lag_spec, out_var)
    if(!dir.exists(out_dir_lag_spec_var)){dir.create(out_dir_lag_spec_var)}
    
    for (type in types){
      out_dir_lag_spec_var_type<-file.path(out_dir_lag_spec_var, type)
      if(!dir.exists(out_dir_lag_spec_var_type)){dir.create(out_dir_lag_spec_var_type)}
      
      # data
      N<-8
      data_proj<-read_csv(file.path(out_dir, paste0("preproc_proj_", out_var, ".csv")))
      coefs_table<-read_csv(file.path(coefs_dir,paste0("gr_",out_var, '_',type, '_lagdiff_fix_',spec,'_lagN',as.character(N),'_coef.csv')))
      coefs<-coefs_table$x
      names(coefs)<-coefs_table$...1
      
      project_impacts(paste0("gr_",out_var) , climate_projections, data_proj, coefs , out_dir_lag_spec_var_type, maps=TRUE)
      
    }
    
  }
  
}




