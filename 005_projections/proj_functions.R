
clim_deltas_calc<-function(
                           climate_projections_dir,
                           agg,
                           regions, 
                           coefs, 
                           var_name
                           ){
  
  all_deltas<-data.frame()
  
  vars<-names(coefs)
  
  pb=progress::progress_bar$new(total=length(regions))
  pb$tick(0)
  for ( r in regions ){
    gc()
    load(file.path(climate_projections_dir, paste0(r,"_climate_projections_gdl_",agg,".RData")))
    climate_projections<-as.data.frame(climate_projections)
    climate_projections<-climate_projections%>% mutate(
      TM_2=TM^2, 
      RR_2=RR^2, 
      TVAR_2=TVAR^2, 
      HW_2=HW^2, 
      RX_2=RX^2, 
      PEXT_2=PEXT^2, 
      WD_2=WD^2, 
      SPI_2=SPI^2, 
      SPEI_2=SPEI^2 
      #PET_2=PET^2
    )
    
   
    ## TODO insert here bootstrap of coefs, decide how to save (maybe one table per boot to avoid big data stored)
    climate_projections$delta_clim_proj=as.numeric(as.matrix(climate_projections[, c(vars)]) %*% c(coefs))  #rowSums(t(apply(as.matrix(climate_projections[, vars]), 1, function(x) coefs*x)),na.rm=T)
    
    all_deltas<-rbind(all_deltas, climate_projections[, c("gdlcode", "year", "model", "ssp", "delta_clim_proj")])
    pb$tick()
  }
  
  return(all_deltas)
}




project_impacts <- function(var_name, regions,data, out_dir, all_deltas, spec_type, effect ,maps=FALSE){
  # var_name<-paste0("gr_",out_var)
  # climate_projections_dir<-climate_projections_dir
  # agg<-agg
  # regions<-gdlcodes_iso$gdlcode
  # data<-data_proj
  # out_dir<-out_dir_lag_spec_var_type
  # maps<-FALSE
  
  
  colnames(data)[colnames(data)=="Scenario"]<-"ssp"
  
  # add growth under ssp (interpolated since values we use are)
  data<-data %>% 
    arrange(ssp, gdlcode, year) %>% 
    group_by(ssp, gdlcode) %>% 
    mutate(gr_value_interp = log(value_interp) - log(dplyr::lag(value_interp, n = 1, default = NA)))%>%
    ungroup()
  
  
  expand_data<-expand.grid( year = unique(data$year), 
                            gdlcode = unique(data$gdlcode), 
                            ssp = unique(data$ssp), 
                            model =models_names )
  
  data<-inner_join(data, expand_data)
  data$model<-as.character(data$model)
  
  
  # set values in first year available 
  # y<-min(unique(data$year))
  # data[which(data$year==y), c("gr_value_interp")]<-0
  
  
  # unify with cl deltas
  data_all<-inner_join(data, all_deltas)
  
  # set values in first year available 
  y<-2024
  data_all<-data_all%>%filter(year>=y)
  data_all[which(data_all$year==y), c("delta_clim_proj")]<-0
  data_all[which(data_all$year==y), c("gr_value_interp")]<-0
  
  
  ### test to see if method works
  data_all<-data_all %>%
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, gdlcode, year)%>% 
    group_by(model, ssp, gdlcode) %>%
    mutate(first_value_interp= first(value_interp) ) %>%
    # mutate(effect_old = cumprod(gr_value_interp + 1) ) %>%
    mutate(effect = cumprod( (exp(gr_value_interp) -1) + 1) ) %>%
    # mutate(test_proj_value_old = if_else(year == y, first_value_interp , first_value_interp*effect_old ) ) %>%
    mutate(test_proj_value = if_else(year == y, first_value_interp ,first_value_interp*effect ) ) %>%
    ungroup()
  
  
  
  ###
  
  
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, gdlcode, year)%>% 
    group_by(model, ssp, gdlcode) %>%
    mutate(first_value_interp= first(value_interp)) %>%
    mutate(effect_cc = cumprod( (exp(gr_value_interp)-1)+ (exp(delta_clim_proj)-1) + 1)) %>%
    # mutate(effect_cc_old = cumprod( gr_value_interp + delta_clim_proj + 1)) %>%
    mutate(cc_proj_value = if_else(year == y, first_value_interp ,
                                   first_value_interp*effect_cc ) ) %>%
    # mutate(cc_proj_value_old = if_else(year == y, first_value_interp ,
    #                                first_value_interp*effect_cc_old ) ) %>%
    mutate(cc_proj_perc_effect = 100*(cc_proj_value -  value_interp)/value_interp, 
           cc_proj_effect = cc_proj_value -  value_interp ) %>%
    #  mutate(cc_proj_perc_effect_old = 100*(cc_proj_value_old -  value_interp)/value_interp, 
    #         cc_proj_effect_old = cc_proj_value_old -  value_interp ) %>%
    ungroup()
  
  
  
  
  
  # plot(data_all$cc_proj_effect, data_all$value_interp)
  
  
  pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
  colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
  gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
  pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
  pop_projection_gdlcode<-pop_projection_gdlcode%>%
    group_by(iso3, year, ssp)%>%
    mutate(pop_country=sum(pop_gdl))
  pop_projection_gdlcode<-unique(pop_projection_gdlcode)
  
  data_all<-inner_join(data_all, pop_projection_gdlcode )
  
  ### save 
  write.csv(data_all, file=file.path(out_dir,paste0(var_name,"_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
}




plot_proj_impacts <- function( var_name, regions,  out_dir,  spec_type, effect ,maps=FALSE){
  
  data_all<-  read_csv(file.path(out_dir,paste0(var_name,"_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
  data_all$model<-as.factor(data_all$model)
  data_all$ssp<-as.factor(data_all$ssp)
  data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))
  
  
  # aggregate using sub_pop
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year) %>%
    mutate(global_weighted_value_interp=weighted.mean(value_interp, pop_gdl, na.rm=TRUE), 
           global_weighted_cc_proj_value=weighted.mean(cc_proj_value, pop_gdl, na.rm=TRUE), 
           global_weighted_cc_proj_effect=weighted.mean(cc_proj_effect, pop_gdl, na.rm=TRUE),
           global_weighted_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect, pop_gdl, na.rm=TRUE))
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year, iso3) %>%
    mutate(country_weighted_value_interp=weighted.mean(value_interp, pop_gdl, na.rm=TRUE), 
           country_weighted_cc_proj_value=weighted.mean(cc_proj_value, pop_gdl, na.rm=TRUE), 
           country_weighted_cc_proj_effect=weighted.mean(cc_proj_effect, pop_gdl, na.rm=TRUE),
           country_weighted_cc_proj_perc_effect=weighted.mean(cc_proj_perc_effect, pop_gdl, na.rm=TRUE))
  
  
  
  data_all$model<-as.factor(data_all$model)
  data_all$ssp<-as.factor(data_all$ssp)
  data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))
  
  
  ### global damages plot 
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_proj_value, group=model_ssp, color=ssp, linetype=model)) + 
    #geom_line(aes(y = global_weighted_value_interp, group=model_ssp, color=ssp, linetype=model ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_values.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_proj_effect, group=model_ssp, color=ssp, linetype=model)) + 
    #geom_line(aes(y = global_weighted_value_interp, group=model_ssp, color=ssp, linetype=model ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_dam.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_proj_perc_effect, group=model_ssp, color=ssp, linetype=model)) + 
    #geom_line(aes(y = global_weighted_value_interp, group=model_ssp, color=ssp, linetype=model ))+
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_",spec_type, "_", effect,"_", "pop_w_glob_cc_perc_dam.png")), g, width=15, height=10)
  
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_cc_proj_effect, group=model_ssp, color=ssp, shape=model ) ) +
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
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_weight_dam.png")), g, width=15, height=10)
  
  # perc damages wrt baseline
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_cc_proj_perc_effect, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Perc Climate Change Impact",
      x = "Year",
      y = "Percentage Loss (cc - baseline)/baseline",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", "sel_c_perc_weight_dam.png")), g, width=15, height=10)
  
  
  ### plot maps
  if(maps==TRUE){
    
    
    gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
    gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
    gdl_shape_file<-sf::st_as_sf(gdl_shape_file)
    
    # ### Countries borders 
    # countries<-giscoR::gisco_get_countries()
    # #simplify data countries
    # countries<-countries[,c(3,6)]
    # colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-"iso3"
    # countries<-sf::st_as_sf(countries)
    
    years<-c(2030, 2045, 2060 ,2075, 2085, 2100)
    
    for (sc in unique(data$ssp)){
      for (mod in unique(data$model)){
        
        if( paste0(mod, "_", sc) %in% data$model_ssp ){
          
          plot_map<-data%>%
            filter(ssp %in% sc)%>%
            filter(model %in% mod)%>%
            filter(year %in% years)
          
          plot_map<-inner_join(plot_map, gdl_shape_file)
          plot_map<-sf::st_as_sf(plot_map)
          
          g<-ggplot(plot_map)+geom_sf( aes(fill=cc_proj_effect))+theme_bw()+  scale_fill_gradient2() + 
            labs(fill=paste0(var_name," change") )+ ggtitle("cc effect wrt baseline") + 
            facet_wrap(~ year) 
          ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
          
        }
      }
    }
  }
  
  
}


plot_proj_impacts_hdi<-function(regions,  out_dir, out_dir_comp,  spec_type, effect ,maps=FALSE){
  # new funcs plot for hdi 
  
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
  
  # max min values calculated over all horizon 
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
  
  # apply max min on projections to obtain indeces, combine by geometric mean
  data_all_gni<-read_csv(file.path(out_dir,paste0("gnipc","_",spec_type, "_", effect,"_", "projected_damage.csv")))
  data_all_gni<-data_all_gni[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,"value_interp", "cc_proj_value")]
  data_all_gni$log_value_interp<-log(data_all_gni$value_interp)
  data_all_gni$log_cc_proj_value<-log(data_all_gni$cc_proj_value)
  
  data_all_gni$income_index<-(data_all_gni$log_value_interp-min_lgnipc)/(max_lgnipc-min_lgnipc)
  data_all_gni$cc_income_index<-(data_all_gni$log_cc_proj_value-min_lgnipc)/(max_lgnipc-min_lgnipc)
  
  data_all_leb<-read_csv(file.path(out_dir,paste0("leb","_",spec_type, "_", effect,"_", "projected_damage.csv")))
  data_all_leb<-data_all_leb[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,"value_interp", "cc_proj_value")]
  data_all_leb$lifex_index<-(data_all_leb$value_interp-min_leb)/(max_leb-min_leb)
  data_all_leb$cc_lifex_index<-(data_all_leb$cc_proj_value-min_leb)/(max_leb-min_leb)
  
  data_all_eys<-read_csv(file.path(out_dir,paste0("eys","_",spec_type, "_", effect,"_", "projected_damage.csv")))
  data_all_eys<-data_all_eys[,c("year", "iso3", "ssp", "model", "gdlcode", "pop" ,"pop_gdl" ,"pop_country" ,"value_interp", "cc_proj_value")]
  data_all_eys$edu_index<-(data_all_eys$value_interp-min_eys)/(max_eys-min_eys)
  data_all_eys$cc_edu_index<-(data_all_eys$cc_proj_value-min_eys)/(max_eys-min_eys)
  
  data_all_gni<-data_all_gni%>%filter(iso3 %in% common_iso)
  data_all_leb<-data_all_leb%>%filter(iso3 %in% common_iso)
  data_all_eys<-data_all_eys%>%filter(iso3 %in% common_iso)
  
  data_all_gni$value_interp<-NULL
  data_all_gni$cc_proj_value<-NULL
  data_all_gni$log_value_interp<-NULL
  data_all_gni$log_cc_proj_value<-NULL
  data_all_leb$value_interp<-NULL
  data_all_leb$cc_proj_value<-NULL
  data_all_eys$value_interp<-NULL
  data_all_eys$cc_proj_value<-NULL
  
  
  # combine
  data_all<-inner_join(data_all_gni, data_all_leb)
  data_all<-inner_join(data_all, data_all_eys)
  
  data_all$hdi<-(data_all$income_index*data_all$lifex_index*data_all$edu_index)^(1/3)
  data_all$cc_hdi<-(data_all$cc_income_index*data_all$cc_lifex_index*data_all$cc_edu_index)^(1/3)
  data_all$delta_hdi<-data_all$cc_hdi-data_all$hdi
  
  
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year, iso3) %>%
    mutate(country_weighted_delta=weighted.mean(delta_hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_hdi=weighted.mean(cc_hdi, pop_gdl, na.rm=TRUE)) 
  
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year) %>%
    mutate(global_weighted_delta=weighted.mean(delta_hdi, pop_gdl, na.rm=TRUE))%>%
    mutate(global_weighted_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_cc_hdi=weighted.mean(cc_hdi, pop_gdl, na.rm=TRUE)) 
  
  
  write.csv(data_all, file=file.path(out_dir, paste0("hdi_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
  
  data_all$model<-as.factor(data_all$model)
  data_all$ssp<-as.factor(data_all$ssp)
  data_all$model_ssp<-as.factor(paste0(data_all$model, "_", data_all$ssp ))
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_delta*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "HDI loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("hdi_damages_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_hdi*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "HDI",
      x = "Year",
      y = "HDI in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("hdi_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_hdi*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "HDI with CC",
      x = "Year",
      y = "HDI cc in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("hdi_cc_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  ### plot select countries
  selected_countries <- c("CAN", "NOR", "RUS",
                          "BRA", "DZA", "IND", 
                          "ARG", "ZAF", "AUS")
  
  temp_dat<-data_all[which(data_all$iso3 %in% selected_countries),]
  
  # absolute deviations from baseline
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_delta*100, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "HDI loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0("hdi_damages_", spec_type, "_", effect,"_", "sel_c_pop_w.png")), g, width=15, height=10)
  
  ### plot maps
  if(maps==TRUE){
    
    
    gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
    gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
    gdl_shape_file<-sf::st_as_sf(gdl_shape_file)
    
    # ### Countries borders 
    # countries<-giscoR::gisco_get_countries()
    # #simplify data countries
    # countries<-countries[,c(3,6)]
    # colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-"iso3"
    # countries<-sf::st_as_sf(countries)
    
    years<-c(2030, 2045, 2060 ,2075, 2085, 2100)
    
    for (sc in unique(data_all$ssp)){
      for (mod in unique(data_all$model)){
        
        if( paste0(mod, "_", sc) %in% data_all$model_ssp ){
          
          plot_map<-data_all%>%
            filter(ssp %in% sc)%>%
            filter(model %in% mod)%>%
            filter(year %in% years)
          
          plot_map<-inner_join(plot_map, gdl_shape_file)
          plot_map<-sf::st_as_sf(plot_map)
          
          g<-ggplot(plot_map)+geom_sf( aes(fill=delta_hdi*100))+theme_bw()+  scale_fill_gradient2() + 
            labs(fill=paste0("HDI change in % points") )+ ggtitle("cc effect wrt baseline") + 
            facet_wrap(~ year) 
          ggsave(filename=file.path(out_dir, paste0(var_name,"_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
          
        }
      }
    }
  }
}

