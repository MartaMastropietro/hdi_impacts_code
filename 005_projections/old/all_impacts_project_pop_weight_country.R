### functions for proj damages 


rm(list=ls())
source("utils/libraries.R")

### out dir 

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight"
if(!dir.exists(out_dir_pop)){dir.create(out_dir_pop)}


### climate data 
gc()
climate_projections<- arrow::read_feather("output/projections/climate_projections_llgnipc_llgdppc_pop_weight.feather")
climate_projections<-as.data.frame(climate_projections)

ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


##################################################################################

# project_impacts(var_name , climate_projections, data_proj, coef_list[[cl_var]] , out_dir_var_sub, maps=FALSE)

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

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}

coefs_dir<-"output/models/original_components_tp_extr_all_models_performances/lag_models"

specifications<-c("mean_mod_spec")
types<-c("all_extr")#,"all_extr", "all_extr_controls", "all_extr_autoreg")
out_variables<-c( "eys")#, "gnipc", "leb")
#out_variables<-c( "mys")

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












var_name<-"leb"

# directory out
out_dir_var<-paste0(out_dir,"/",var_name)
if(!dir.exists(out_dir_var)){dir.create(out_dir_var)}

# directory data
data_dir<-paste0("output/projections/try_original_comp/preproc_proj_", var_name, ".csv")
data_proj<-read_csv(data_dir)

# impact fun, using controls

coefs<-c( 
  
  diff_SPI         = -1.003627e-05,  
  TM_i_diff_SPI    = -6.121635e-06  
  )

coef_list<-list( "cumulative"=coefs, 
                 "extr" = c( diff_SPI         = -1.003627e-05,  
                              TM_i_diff_SPI    = -6.121635e-06    )
)

for (cl_var in names(coef_list)){
  
  out_dir_var_sub<-paste0(out_dir_var,"/", cl_var)
  if(!dir.exists(out_dir_var_sub)){dir.create(out_dir_var_sub)}
  
  project_impacts(var_name , climate_projections, data_proj, coef_list[[cl_var]] , out_dir_var_sub, maps=FALSE)
  
} 

#################################################################################


var_name<-"mys"

# directory out
out_dir_var<-paste0(out_dir,"/",var_name)
if(!dir.exists(out_dir_var)){dir.create(out_dir_var)}

# directory data
data_dir<-paste0("output/projections/try_original_comp/preproc_proj_", var_name, ".csv")
data_proj<-read_csv(data_dir)

# impact fun

coefs<-c( 
  diff_TM = 6.508179e-05 , TM_i_diff_TM   = -9.889950e-06, 
  diff_RR  =  1.157440e-06 , RR_i_diff_RR  =  1.190000e-09, 
  diff_RX   =   5.450164e-05, RR_i_diff_RX  = -3.161000e-08 , 
  diff_PEXT   =  -8.669024e-05,  TM_i_diff_PEXT = 3.805950e-06 
)

coef_list<-list( "cumulative"=coefs, 
                 "extr" = c(diff_RX   =   5.450164e-05, RR_i_diff_RX  = -3.161000e-08 , 
                            diff_PEXT   =  -8.669024e-05,  TM_i_diff_PEXT = 3.805950e-06  )
)

for (cl_var in names(coef_list)){
  
  out_dir_var_sub<-paste0(out_dir_var,"/", cl_var)
  if(!dir.exists(out_dir_var_sub)){dir.create(out_dir_var_sub)}
  
  project_impacts(var_name , climate_projections, data_proj, coef_list[[cl_var]] , out_dir_var_sub, maps=FALSE)
  
} 


#################################################################################

var_name<-"gni_pc"

# directory out
out_dir_var<-paste0(out_dir,"/",var_name)
if(!dir.exists(out_dir_var)){dir.create(out_dir_var)}

# directory data
data_dir<-paste0("output/projections/try_original_comp/preproc_proj_", var_name, ".csv")
data_proj<-read_csv(data_dir)

# impact fun

coefs<-c( 
  diff_TM =2.422634e-03 , TM_i_diff_TM   = -1.757859e-05, 
  diff_RR  =  -1.952905e-05  , RR_i_diff_RR  =   6.180000e-09 , 
  diff_WD   = 3.563230e-04 , RR_i_diff_WD  = -2.273600e-07,
  diff_PEXT   =   -1.828487e-04 ,  TM_i_diff_PEXT =1.050560e-05, 
  diff_SPI    =   -6.191631e-04 
)

coef_list<-list( "cumulative"=coefs, 
                 "extr" = c( diff_WD   = 3.563230e-04 , RR_i_diff_WD  = -2.273600e-07,
                             diff_PEXT   =   -1.828487e-04 ,  TM_i_diff_PEXT =1.050560e-05, 
                             diff_SPI    =   -6.191631e-04  )
)

for (cl_var in names(coef_list)){
  
  out_dir_var_sub<-paste0(out_dir_var,"/", cl_var)
  if(!dir.exists(out_dir_var_sub)){dir.create(out_dir_var_sub)}
  
  project_impacts(var_name , climate_projections, data_proj, coef_list[[cl_var]] , out_dir_var_sub, maps=FALSE)
  
} 
