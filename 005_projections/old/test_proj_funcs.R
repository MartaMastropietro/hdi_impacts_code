# test proj functions

rm(list=ls())
source("utils/libraries.R")

source("scripts/005_projections/proj_functions.R")


### out dir 


### codes for connecting to gdlcode  
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")


ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")

models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                "UK_ESM1") 


#######################

out_variables<-c( "leb", "gnipc" , "eys" )

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"

if(!dir.exists(out_dir)){dir.create(out_dir)}


#lags number 
N<-8

coefs_dir<-"output/models/original_components_tp_extr_complex_models_try/lag_models"

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_vars") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")

vars_in_proj<-c( "all_vars")#, "extr_only")

#define regions 
regions<-gdlcodes_iso$gdlcode#[ which(gdlcodes_iso$gdlcode %in% unique(data_proj_gni$gdlcode)) ]



spec<-specifications[1]
type<-types[1]
vars<-vars_in_proj[1]

spec_type<-"dlm"
effect<-"growth_eff"


climate_projections_dir<-"output/projections/climate_projections_gdl"
agg<-"pop_weight"


out_dir_comp<-"output/projections/original_comp" 

##################################################################################
# VAR
for(out_var in out_variables){
  
  var_name<-out_var
#var_name<-out_var<-out_variables[3]

# coefs
coefs_table<-read_csv(file.path(coefs_dir,paste0("gr_",out_var, '_',type, '_',spec,'_lagN',as.character(N),'_coef.csv')))
coefs<-coefs_table$x
names(coefs)<-coefs_table$...1





out_dir_lag<-file.path(out_dir_pop, "lag_models")
out_dir_lag_spec<-file.path(out_dir_lag, spec)
if(!dir.exists(out_dir_lag_spec)){dir.create(out_dir_lag_spec)}
out_dir_lag_spec_var<-file.path(out_dir_lag_spec, out_var)
if(!dir.exists(out_dir_lag_spec_var)){dir.create(out_dir_lag_spec_var)}
out_dir_lag_spec_var_type<-file.path(out_dir_lag_spec_var, type)
if(!dir.exists(out_dir_lag_spec_var_type)){dir.create(out_dir_lag_spec_var_type)}
out_dir_lag_spec_var_type_vars<-file.path(out_dir_lag_spec_var_type, vars)
if(!dir.exists(out_dir_lag_spec_var_type_vars)){dir.create(out_dir_lag_spec_var_type_vars)}


data_proj<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", out_var, ".csv")))
data_proj<-left_join(data_proj, gdlcodes_iso)



deltas_path<-file.path(out_dir_lag_spec_var_type_vars,paste0("gr_",out_var,'_',type, "_climate_deltas_", agg ,".feather"))
cl_deltas<-arrow::read_feather(deltas_path)


data<-data_proj
all_deltas<-cl_deltas




################################################################################

# project_impacts <- function(var_name, regions,data, out_dir, all_deltas, spec_type, effect ,maps=FALSE){
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
  
  

  
##############################################################################
  # new funcs plot for single comp
  
  
  # data_all<-  read_csv(file.path(out_dir,paste0(var_name,"_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
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
  

  
}
  

##############################################################################
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
  
  ### plot lorenz 
  
  # gni
  data_mod_ssp<-data_all_gni[which(data_all_gni$year==2050 & data_all_gni$ssp=="ssp126" & data_all_gni$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp126 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp126 cc")
  
  data_mod_ssp<-data_all_gni[which(data_all_gni$year==2050 & data_all_gni$ssp=="ssp245" & data_all_gni$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp245 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp245 cc")
  
  data_mod_ssp<-data_all_gni[which(data_all_gni$year==2050 & data_all_gni$ssp=="ssp370" & data_all_gni$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  #x11()
  plot(lorenz_curve, main="gni - ssp370 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  #x11()
  plot(lorenz_curve, main="gni - ssp370 cc")
  
  data_mod_ssp<-data_all_gni[which(data_all_gni$year==2050 & data_all_gni$ssp=="ssp585" & data_all_gni$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp585 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="gni - ssp585 cc")
  
  # lifex
  data_mod_ssp<-data_all_leb[which(data_all_leb$year==2050 & data_all_leb$ssp=="ssp126" & data_all_leb$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp126 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp126 cc")
  
  data_mod_ssp<-data_all_leb[which(data_all_leb$year==2050 & data_all_leb$ssp=="ssp245" & data_all_leb$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp245 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp245 cc")
  
  data_mod_ssp<-data_all_leb[which(data_all_leb$year==2050 & data_all_leb$ssp=="ssp370" & data_all_leb$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp370 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp370 cc")
  
  data_mod_ssp<-data_all_leb[which(data_all_leb$year==2050 & data_all_leb$ssp=="ssp585" & data_all_leb$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp585 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="leb - ssp585 cc")
  
  # edu
  data_mod_ssp<-data_all_eys[which(data_all_eys$year==2050 & data_all_eys$ssp=="ssp126" & data_all_eys$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp126 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp126 cc")
  
  data_mod_ssp<-data_all_eys[which(data_all_eys$year==2050 & data_all_eys$ssp=="ssp245" & data_all_eys$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp245 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp245 cc")
  
  data_mod_ssp<-data_all_eys[which(data_all_eys$year==2050 & data_all_eys$ssp=="ssp370" & data_all_eys$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp370 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp370 cc")
  
  data_mod_ssp<-data_all_eys[which(data_all_eys$year==2050 & data_all_eys$ssp=="ssp585" & data_all_eys$model=="IPSL_CM6A"),]
  lorenz_curve <- ineq::Lc(data_mod_ssp$value_interp, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp585 no cc")
  lorenz_curve <- ineq::Lc(data_mod_ssp$cc_proj_value, data_mod_ssp$pop_gdl)
  x11()
  plot(lorenz_curve, main="eys - ssp585 cc")
  
  
  
  
  
  
  
  
  
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
  data_all$delta_income_index<-data_all$cc_income_index-data_all$income_index
  data_all$delta_lifex_index<-data_all$cc_lifex_index-data_all$lifex_index
  data_all$delta_edu_index<-data_all$cc_edu_index-data_all$edu_index
  
  
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year, iso3) %>%
    # hdi
    mutate(country_weighted_delta=weighted.mean(delta_hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_hdi=weighted.mean(cc_hdi, pop_gdl, na.rm=TRUE)) %>%
    # income
    mutate(country_weighted_delta_income_index=weighted.mean(delta_income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_income_index=weighted.mean(income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_income_index=weighted.mean(cc_income_index, pop_gdl, na.rm=TRUE)) %>%
    # lifex
    mutate(country_weighted_delta_lifex_index=weighted.mean(delta_lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_lifex_index=weighted.mean(cc_lifex_index, pop_gdl, na.rm=TRUE)) %>%
    # edu
    mutate(country_weighted_delta_edu_index=weighted.mean(delta_edu_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_edu_index=weighted.mean(edu_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_edu_index=weighted.mean(cc_edu_index, pop_gdl, na.rm=TRUE)) 
    
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year) %>%
    # hdi
    mutate(global_weighted_delta=weighted.mean(delta_hdi, pop_gdl, na.rm=TRUE))%>%
    mutate(global_weighted_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_cc_hdi=weighted.mean(cc_hdi, pop_gdl, na.rm=TRUE)) %>%
    # income
    mutate(global_weighted_delta_income_index=weighted.mean(delta_income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_income_index=weighted.mean(income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_cc_income_index=weighted.mean(cc_income_index, pop_gdl, na.rm=TRUE)) %>%
    # lifex
    mutate(global_weighted_delta_lifex_index=weighted.mean(delta_lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_cc_lifex_index=weighted.mean(cc_lifex_index, pop_gdl, na.rm=TRUE)) %>%
    # edu
    mutate(global_weighted_delta_edu_index=weighted.mean(delta_edu_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_edu_index=weighted.mean(edu_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(global_weighted_cc_edu_index=weighted.mean(cc_edu_index, pop_gdl, na.rm=TRUE)) 
  
  
  write.csv(data_all, file=file.path(out_dir,paste0("hdi_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
  
################################################################################
# hdi projections plot 
  
  data_all<-read_csv(file.path(out_dir,paste0("hdi_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
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
  
  #### components plots 
  
  ### income_index
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_delta_income_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Income Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("income_index_damages_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_income_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Income Index",
      x = "Year",
      y = "Income Index in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("income_index_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_income_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Income Index with CC",
      x = "Year",
      y = "Income Index cc in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("income_index_cc_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  ### lifex_index
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_delta_lifex_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Lifex Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("lifex_index_damages_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_lifex_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Lifex Index",
      x = "Year",
      y = "Lifex Index in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("lifex_index_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_lifex_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Lifex Index with CC",
      x = "Year",
      y = "Lifex Index cc in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("lifex_index_cc_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  ### edu_index
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_delta_edu_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Global Climate Change Impact",
      x = "Year",
      y = "Edu Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("edu_index_damages_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_edu_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Edu Index",
      x = "Year",
      y = "Edu Index in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("edu_index_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=data_all, aes(x = year ) ) +
    geom_line(aes( y = global_weighted_cc_edu_index*100, group=model_ssp, color=ssp, linetype=model)) + 
    geom_hline(yintercept=0,linewidth = 0.8)+
    labs(
      title = "Edu Index with CC",
      x = "Year",
      y = "Edu Index cc in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  #x11()
  #print(g)
  ggsave(filename=file.path(out_dir, paste0("edu_index_cc_",spec_type, "_", effect,"_", "glob_pop_w.png")), g, width=15, height=10)
  
  
  
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
  
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_delta_income_index*100, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Income Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0("income_index_damages_", spec_type, "_", effect,"_", "sel_c_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_delta_lifex_index*100, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Lifex Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0("delta_lifex_damages_", spec_type, "_", effect,"_", "sel_c_pop_w.png")), g, width=15, height=10)
  
  g<-ggplot( data=temp_dat, aes(x = year, y = country_weighted_delta_edu_index*100, group=model_ssp, color=ssp, shape=model ) ) +
    scale_shape_manual(values=1:nlevels(data_all$model)) +
    geom_line() + 
    geom_point(size=1.5)+geom_hline(yintercept=0,linewidth = 1.1)+
    facet_wrap(~ iso3) +
    labs(
      title = "Climate Change Impact",
      x = "Year",
      y = "Edu Index loss (cc - baseline) in % points",
      color = "ssp",
      shape= "model"
    ) +
    theme_bw()
  ggsave(filename=file.path(out_dir, paste0("edu_index_damages_", spec_type, "_", effect,"_", "sel_c_pop_w.png")), g, width=15, height=10)
  
  
  ###### maps ######
  
  out_dir_maps<-file.path(out_dir, "maps")
  if(!dir.exists(out_dir_maps)){dir.create(out_dir_maps)}
  
  gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
  gdl_shape_file<-gdl_shape_file[, c("gdlcode", "geometry")]
  gdl_shape_file<-sf::st_as_sf(gdl_shape_file)
  
  # ### Countries borders 
  # countries<-giscoR::gisco_get_countries()
  # #simplify data countries
  # countries<-countries[,c(3,6)]
  # colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-"iso3"
  # countries<-sf::st_as_sf(countries)
  
  
  ### maps on median across ssp
  
  data_median<-data_all%>%
    group_by( gdlcode, iso3, ssp, year )%>%
    mutate(median_mod_hdi=median(hdi),
           median_mod_cc_hdi=median(cc_hdi), 
           median_delta=median(delta_hdi))%>%
    mutate(median_mod_income_index=median(income_index),
           median_mod_cc_income_index=median(cc_income_index), 
           median_delta_income_index=median(delta_income_index))%>%
    mutate(median_mod_lifex_index=median(lifex_index),
           median_mod_cc_lifex_index=median(cc_lifex_index), 
           median_delta_lifex_index=median(delta_lifex_index))%>%
    mutate(median_mod_edu_index=median(edu_index),
           median_mod_cc_edu_index=median(cc_edu_index), 
           median_delta_edu_index=median(delta_edu_index))
  data_median<-data_median%>% select(iso3,gdlcode, ssp, year , median_mod_hdi, median_mod_cc_hdi,
           median_mod_income_index, median_mod_cc_income_index,
           median_mod_lifex_index , median_mod_cc_lifex_index,
           median_mod_edu_index, median_mod_cc_edu_index,
           median_delta, median_delta_income_index  ,
           median_delta_lifex_index, median_delta_edu_index, 
           pop_gdl) %>% distinct()
  
           
  years<-c(2030,  2060 , 2080, 2100)
  
  for (sc in unique(data_median$ssp)){
    gc()
        plot_map<-data_median%>%
          filter(ssp %in% sc)%>%
          filter(year %in% years)
        
        plot_map<-inner_join(plot_map, gdl_shape_file)
        plot_map<-sf::st_as_sf(plot_map)
        
        g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta*100))+theme_bw()+  scale_fill_gradient2() + 
          labs(fill=paste0("HDI change (% points)") )+ ggtitle("Effect wrt baseline") + 
          facet_wrap(~ year) 
        ggsave(filename=file.path(out_dir_maps, paste0("median_mod_hdi_", spec_type, "_", effect,"_", sc , "_damages.png")), g, width=15, height=10)
        
        g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_income_index*100))+theme_bw()+  scale_fill_gradient2() + 
          labs(fill=paste0("Income Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
          facet_wrap(~ year) 
        ggsave(filename=file.path(out_dir_maps, paste0("median_mod_income_index_", spec_type, "_", effect,"_", sc , "_damages.png")), g, width=20, height=15)
        
        g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_lifex_index*100))+theme_bw()+  scale_fill_gradient2() + 
          labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
          facet_wrap(~ year) 
        ggsave(filename=file.path(out_dir_maps, paste0("median_mod_lifex_index_", spec_type, "_", effect,"_", sc , "_damages.png")), g, width=20, height=15)
        
        g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_edu_index*100))+theme_bw()+  scale_fill_gradient2() + 
          labs(fill=paste0("Edu Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
          facet_wrap(~ year) 
        ggsave(filename=file.path(out_dir_maps, paste0("median_mod_edu_index_", spec_type, "_", effect,"_", sc , "_damages.png")), g, width=20, height=15)
    
  }
  
  # ### maps on all models separately
  # 
  # years<-c(2030,  2060 , 2080, 2100)
  # 
  # for (sc in unique(data_all$ssp)){
  #   for (mod in unique(data_all$model)){
  #     
  #     if( paste0(mod, "_", sc) %in% data_all$model_ssp ){
  #       
  #       plot_map<-data_all%>%
  #         filter(ssp %in% sc)%>%
  #         filter(model %in% mod)%>%
  #         filter(year %in% years)
  #       
  #       plot_map<-inner_join(plot_map, gdl_shape_file)
  #       plot_map<-sf::st_as_sf(plot_map)
  #       
  #       g<-ggplot(plot_map)+geom_sf( aes(fill=delta_hdi*100))+theme_bw()+  scale_fill_gradient2() + 
  #         labs(fill=paste0("HDI change (% points)") )+ ggtitle("Effect wrt baseline") + 
  #         facet_wrap(~ year) 
  #       ggsave(filename=file.path(out_dir_maps, paste0("hdi_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
  #       
  #       g<-ggplot(plot_map)+geom_sf( aes(fill=delta_income_index*100))+theme_bw()+  scale_fill_gradient2() + 
  #         labs(fill=paste0("Income Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
  #         facet_wrap(~ year) 
  #       ggsave(filename=file.path(out_dir_maps, paste0("income_index_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
  #       
  #       g<-ggplot(plot_map)+geom_sf( aes(fill=delta_lifex_index*100))+theme_bw()+  scale_fill_gradient2() + 
  #         labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
  #         facet_wrap(~ year) 
  #       ggsave(filename=file.path(out_dir_maps, paste0("lifex_index_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
  #       
  #       g<-ggplot(plot_map)+geom_sf( aes(fill=delta_edu_index*100))+theme_bw()+  scale_fill_gradient2() + 
  #         labs(fill=paste0("Edu Index change (% points)") )+ ggtitle("Effect wrt baseline") + 
  #         facet_wrap(~ year) 
  #       ggsave(filename=file.path(out_dir_maps, paste0("edu_index_", spec_type, "_", effect,"_", mod,"_", sc , "_damages.png")), g, width=20, height=15)
  #       
  #     }
  #   }
  # }

###############################################################################
# in another function, calculate future gini changes cc vs baseline
  data_all<-read.csv(file.path(out_dir,paste0("hdi_",spec_type, "_", effect,"_", "projected_damage.csv")))
  
  data_all<-data_all%>%drop_na()
  
  data_all<-data_all %>% 
    filter(ssp %in% ssp_names)%>% 
    arrange(model, ssp, year)%>% 
    group_by(model, ssp, year, iso3) %>%
    mutate(country_weighted_income_index=weighted.mean(income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_income_index=weighted.mean(cc_income_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_lifex_index=weighted.mean(cc_lifex_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_edu_index=weighted.mean(edu_index, pop_gdl, na.rm=TRUE)) %>%
    mutate(country_weighted_cc_edu_index=weighted.mean(cc_edu_index, pop_gdl, na.rm=TRUE)) 
  
  
  
  
  # calculate mean value hdi (country value), cc_hdi across models in same gdlcode, iso, ssp, year 
  data_country_median<-data_all%>%
    group_by( gdlcode, iso3, ssp, year )%>%
    mutate(median_mod_country_hdi=median(country_weighted_hdi),
           median_mod_country_cc_hdi=median(country_weighted_cc_hdi))%>%
    mutate(median_mod_country_income_index=median(country_weighted_income_index),
           median_mod_country_cc_income_index=median(country_weighted_cc_income_index))%>%
    mutate(median_mod_country_lifex_index=median(country_weighted_lifex_index),
           median_mod_country_cc_lifex_index=median(country_weighted_cc_lifex_index))%>%
    mutate(median_mod_country_edu_index=median(country_weighted_edu_index),
           median_mod_country_cc_edu_index=median(country_weighted_cc_edu_index))%>%
    select(iso3, ssp, year , median_mod_country_hdi, median_mod_country_cc_hdi,
           median_mod_country_income_index, median_mod_country_cc_income_index,
           median_mod_country_lifex_index , median_mod_country_cc_lifex_index,
           median_mod_country_edu_index, median_mod_country_cc_edu_index,
            pop_country)
  data_country_median$gdlcode<-NULL
  data_country_median<-data_country_median%>% distinct()
  
  # plot lorenz curve for each ssp 
  
  # library(lawstat)
  # 
  # data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp126"),]
  # lorenz.curve(data_ssp_y$median_mod_country_hdi, data_ssp_y$pop_country, 
  #              mul = TRUE)
  # lorenz.curve(data_ssp_y$median_mod_country_cc_hdi, data_ssp_y$pop_country, 
  #               mul = TRUE)
  
  
  # test another function, one ssp one year 
  
  # hdi 
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp126"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_hdi, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_hdi, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="hdi - ssp126 no cc")
  #x11()
  plot(lorenz_curve_cc, main="hdi - ssp126 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp245"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_hdi, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_hdi, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="hdi - ssp245 no cc")
  #x11()
  plot(lorenz_curve_cc, main="hdi - ssp245 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp370"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_hdi, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_hdi, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="hdi - ssp370 no cc")
  #x11()
  plot(lorenz_curve_cc, main="hdi - ssp370 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp585"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_hdi, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_hdi, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="hdi - ssp585 no cc")
  #x11()
  plot(lorenz_curve_cc, main="hdi - ssp585 cc")
  
  # income_index 
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp126"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_income_index, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_income_index, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="income_index - ssp126 no cc")
  #x11()
  plot(lorenz_curve_cc, main="income_index - ssp126 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp245"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_income_index, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_income_index, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="income_index - ssp245 no cc")
  #x11()
  plot(lorenz_curve_cc, main="income_index - ssp245 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp370"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_income_index, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_income_index, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="income_index - ssp370 no cc")
  #x11()
  plot(lorenz_curve_cc, main="income_index - ssp370 cc")
  
  data_ssp_y<-data_country_median[ which(data_country_median$year==2050 & data_country_median$ssp=="ssp585"),]
  #data_ssp1_2100 <- data_ssp1_2100[order(data_ssp1_2100$pop_country), ]
  lorenz_curve <- ineq::Lc(data_ssp_y$median_mod_country_income_index, data_ssp_y$pop_country)
  lorenz_curve_cc <- ineq::Lc(data_ssp_y$median_mod_country_cc_income_index, data_ssp_y$pop_country)
  #x11()
  plot(lorenz_curve, main="income_index - ssp585 no cc")
  #x11()
  plot(lorenz_curve_cc, main="income_index - ssp585 cc")
  
  
  
  
  
  
  
  # check formula
  # order data by HDI
  df <- data_ssp_y[order(data_ssp_y$median_mod_country_hdi), ]
  df_cc <- data_ssp_y[order(data_ssp_y$median_mod_country_cc_hdi), ]
  
  # cumulative values
  df$cum_pop <- cumsum(df$pop_country) / sum(df$pop_country)
  df$cum_hdi <- cumsum(df$median_mod_country_hdi * df$pop_country) / sum(df$median_mod_country_hdi * df$pop_country)
  df_cc$cum_pop <- cumsum(df_cc$pop_country) / sum(df_cc$pop_country)
  df_cc$cum_hdi <- cumsum(df_cc$median_mod_country_cc_hdi * df_cc$pop_country) / sum(df_cc$median_mod_country_cc_hdi * df_cc$pop_country)
  
  plot(df$cum_pop, df$cum_hdi, type = "l", col = "blue", lwd = 2,
       xlab = "cumulative pop", ylab = "cumulative HDI",
       main = "ssp585 no cc")
  abline(0, 1, col = "red", lty = 2) 
 
  plot(df_cc$cum_pop, df_cc$cum_hdi, type = "l", col = "blue", lwd = 2,
       xlab = "cumulative pop", ylab = "cumulative HDI",
       main = "ssp585 cc")
  abline(0, 1, col = "red", lty = 2) 
  
  
  #  cumulative quotas pop
  data_country_median<- data_country_median %>%
    arrange(median_mod_country_hdi)%>%
    group_by(  ssp , year)%>%
    mutate(pop_cum= cumsum(pop_country) / sum(pop_country))%>%
    mutate(hdi_pond = median_mod_country_hdi * pop_country)%>%
    mutate(hdi_cum_pond = cumsum(hdi_pond) / sum(hdi_pond))%>%
    mutate(hdi_cum = cumsum(median_mod_country_hdi) / sum(median_mod_country_hdi))
  
  data_country_median<- data_country_median %>%
    arrange(median_mod_country_cc_hdi)%>%
    group_by(  ssp , year)%>%
    mutate(pop_cum_cc= cumsum(pop_country) / sum(pop_country))%>%
    mutate(hdi_cc_pond = median_mod_country_cc_hdi * pop_country)%>%
    mutate(hdi_cc_cum_pond = cumsum(hdi_cc_pond) / sum(hdi_cc_pond))%>%
    mutate(hdi_cc_cum = cumsum(median_mod_country_cc_hdi) / sum(median_mod_country_cc_hdi))
  
  
  
  # Crea il grafico della Curva di Lorenz
  ggplot(data_country_median[ which(data_country_median$year==2080),], aes(x = pop_cum, y = hdi_cum_pond)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # Linea di equidistribuzione
    facet_wrap(~ ssp) +
    labs(title = "Lorenz curve for HDI",
         x = "Cumulative Pop",
         y = "Cumulative HDI") +
    theme_minimal()
 
   ggplot(data_country_median[ which(data_country_median$year==2080),], aes(x = pop_cum_cc, y = hdi_cc_cum_pond)) +
    geom_line() +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed") +  # Linea di equidistribuzione
    facet_wrap(~ ssp) +
    labs(title = "Lorenz curve for HDI",
         x = "Cumulative Pop",
         y = "Cumulative HDI") +
    theme_minimal()
  
  
  
  iso3_regions <- read_csv("data/iso3_regions.csv")
  iso3_regions<-iso3_regions[, c("alpha-3", "region", "sub-region")]
  colnames(iso3_regions)<-c("iso3", "region", "sub-region")
  
  data_all_median<-inner_join(data_all_median,iso3_regions )
  
  # calculate gini inside continents for hdi, cc_hdi
  
  library(DescTools)
  
  data_all_median<-data_all_median%>%
    group_by( ssp, year , region)%>%
    mutate(gini_region=Gini(median_mod_hdi, na.rm = TRUE), 
           cc_gini_region=Gini(median_mod_cc_hdi, na.rm = TRUE))%>%
    ungroup()
  
  data_all_median<-data_all_median%>%
    group_by( ssp, year , 'sub-region')%>%
    mutate(gini_sub_region=Gini(median_mod_hdi), 
           cc_gini_sub_region=Gini(median_mod_cc_hdi))%>%
    ungroup()
  
  g<-ggplot( data=data_all_median, aes(x = year ) ) +
    geom_line(aes( y = gini_sub_region, color=region, linetype = ssp)) + 
    geom_line(aes( y = cc_gini_sub_region, color=region, linetype = ssp)) + 
    labs(
      title = "Gini Evolution",
      x = "Year",
      y = "Gini index (on sub-regions)",
      color = "region"
     
    ) +
    theme_bw()
  x11()
  print(g)
  
  g<-ggplot( data=data_all_median, aes(x = year ) ) +
    geom_line(aes( y = cc_gini_region, color=region, linetype = ssp)) + 
    geom_line(aes( y = gini_region, color=region, linetype = ssp)) + 
    labs(
      title = "Gini Evolution",
      x = "Year",
      y = "Gini (no cc) index (on regions)",
      color = "region"
      
    ) +
    theme_bw()
  x11()
  print(g)
  
  g<-ggplot( data=data_all_median, aes(x = year ) ) +
    #geom_line(aes( y = gini_region, color=region, linetype = "solid")) + 
    geom_line(aes( y = cc_gini_region, color=region, linetype = ssp)) + 
    labs(
      title = "Gini Evolution",
      x = "Year",
      y = "Gini index (on regions)",
      color = "region"
      
    ) +
    theme_bw()
  x11()
  print(g)
  
  #ggsave(filename=file.path(out_dir, paste0("hdi_damages_",spec_type, "_", effect,"_", "pop_w.png")), g, width=15, height=10)
  
  
  # plot temporal evolution gini
  
  
  
  
  # do same for each country with number of subregions > 3
  # attach geometry
  # plot difference gini without cc, with cc
  
  ### Countries borders 
  countries<-giscoR::gisco_get_countries()
  #simplify data countries
  countries<-countries[,c(3,4)]
  
  years<-c(2030, 2045, 2060 ,2075, 2085, 2100)
  
  for (sc in unique(data$ssp)){
  plot_map<-data%>%
    filter(ssp %in% sc)%>%
    filter(year %in% years)
  
  plot_map<-inner_join(plot_map, gdl_shape_file)
  plot_map<-sf::st_as_sf(plot_map)
  
  g<-ggplot(plot_map)+geom_sf( aes(fill=cc_proj_effect))+theme_bw()+  scale_fill_gradient2() + 
    labs(fill=paste0(var_name," change") )+ ggtitle("cc effect wrt baseline") + 
    facet_wrap(~ year) 
  ggsave(filename=file.path(out_dir, paste0("gini_diff_", spec_type, "_", effect,"_", sc , ".png")), g, width=20, height=15)
  
  }
################################################################################
  # in another function, take all data_all, assign names to variables, attach historical values
  
# historical values:
  
data_hdi <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")

gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
  
ssp_names<-c("ssp126", "ssp245", "ssp370", "ssp585")
  
models_names<-c("CAN_ESM5", "CNRM_CM6", "CNRM_ESM2", 
                  "EC_EARTH3", "GFDL_ESM4", "IPSL_CM6A",
                  "MIRO_C6", "MPI_ESM1" ,  "MRI_ESM2", 
                  "UK_ESM1") 
  
  
  
out_dir_comp<-"output/projections/original_comp" 

out_variables<-c( "leb", "gnipc" , "eys", "mys" )

all_data_proj<-expand.grid(iso3=gdlcodes_iso$iso3, 
                           Scenario=ssp_names, 
                           year=2020:2100
                           )

for (out_var in out_variables ){
  
  data_proj<-read_csv(file.path(out_dir_comp, paste0("preproc_proj_", out_var, ".csv")))
  colnames(data_proj)[which(colnames(data_proj)=="value_interp")]<-paste0(out_var, "_value_interp")
  all_data_proj<-inner_join(all_data_proj, data_proj)
  all_data_proj$gr_value_interp<-NULL
  
}
  