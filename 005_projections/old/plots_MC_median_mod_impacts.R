
rm(list=ls())

library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

library(ggpattern)
#devtools::install_github("statnmap/HatchedPolygons")
#require(HatchedPolygons)


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

data_median<-read_csv(file.path(out_dir, "data_median_sign.csv"))

# to rethink in juno, specifically, calculate median of global average and its ci directly 
# data_median<-data_median %>% 
#    arrange( year,ssp)%>% 
#   group_by( year,ssp) %>%
#   # 
#   mutate(global_weighted_median_delta_hdi=weighted.mean(median_delta_hdi, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_delta_income_index=weighted.mean(median_delta_income_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_delta_lifex_index=weighted.mean(median_delta_lifex_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_delta_edu_index=weighted.mean(median_delta_edu_index, pop_gdl, na.rm=TRUE))%>%
#   
#   mutate(global_weighted_median_cc_hdi=weighted.mean(median_cc_hdi, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_cc_income_index=weighted.mean(median_cc_income_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_cc_lifex_index=weighted.mean(median_cc_lifex_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_cc_edu_index=weighted.mean(median_cc_edu_index, pop_gdl, na.rm=TRUE))%>%
#   
#   mutate(global_weighted_median_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_income_index=weighted.mean(income_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(global_weighted_median_edu_index=weighted.mean(edu_index, pop_gdl, na.rm=TRUE))%>%
#   
#   ungroup()
# 
# data_median <- data_median %>% 
#   arrange(year, ssp, iso3) %>% 
#   group_by(year, ssp, iso3) %>%
#   mutate(
#     # 
#     country_weighted_median_delta_hdi = weighted.mean(median_delta_hdi, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_income_index = weighted.mean(median_delta_income_index, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_lifex_index = weighted.mean(median_delta_lifex_index, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_edu_index = weighted.mean(median_delta_edu_index, pop_gdl, na.rm = TRUE),
#     
#     country_weighted_median_delta_hdi_q10 = weighted.mean(median_delta_hdi_q10, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_income_index_q10 = weighted.mean(median_delta_income_index_q10, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_lifex_index_q10 = weighted.mean(median_delta_lifex_index_q10, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_edu_index_q10 = weighted.mean(median_delta_edu_index_q10, pop_gdl, na.rm = TRUE),
#     
#     country_weighted_median_delta_hdi_q90 = weighted.mean(median_delta_hdi_q90, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_income_index_q90 = weighted.mean(median_delta_income_index_q90, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_lifex_index_q90 = weighted.mean(median_delta_lifex_index_q90, pop_gdl, na.rm = TRUE),
#     country_weighted_median_delta_edu_index_q90 = weighted.mean(median_delta_edu_index_q90, pop_gdl, na.rm = TRUE)
#   ) %>%
#   
#   mutate(country_weighted_median_cc_hdi=weighted.mean(median_cc_hdi, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_cc_income_index=weighted.mean(median_cc_income_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_cc_lifex_index=weighted.mean(median_cc_lifex_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_cc_edu_index=weighted.mean(median_cc_edu_index, pop_gdl, na.rm=TRUE))%>%
#   
#   mutate(country_weighted_median_hdi=weighted.mean(hdi, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_income_index=weighted.mean(income_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm=TRUE))%>%
#   mutate(country_weighted_median_edu_index=weighted.mean(edu_index, pop_gdl, na.rm=TRUE))%>%
#   
#   ungroup()
# 
# 
# # only sign
# 
# data_median <- data_median %>% 
#   arrange(year, ssp) %>% 
#   group_by(year, ssp) %>%
#   mutate(
#     # Apply weighting only to significant values
#     global_weighted_median_sign_delta_hdi = weighted.mean(ifelse(sign_hdi_90, median_delta_hdi, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_delta_income_index = weighted.mean(ifelse(sign_income_index_90, median_delta_income_index, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_delta_lifex_index = weighted.mean(ifelse(sign_lifex_index_90, median_delta_lifex_index, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_delta_edu_index = weighted.mean(ifelse(sign_edu_index_90, median_delta_edu_index, NA), pop_gdl, na.rm = TRUE),
#  
#     # values cc
#     global_weighted_median_sign_cc_hdi = weighted.mean(ifelse(sign_hdi_90, median_cc_hdi, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_cc_income_index = weighted.mean(ifelse(sign_income_index_90, median_cc_income_index, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_cc_lifex_index = weighted.mean(ifelse(sign_lifex_index_90, median_cc_lifex_index, NA), pop_gdl, na.rm = TRUE),
#     global_weighted_median_sign_cc_edu_index = weighted.mean(ifelse(sign_edu_index_90, median_cc_edu_index, NA), pop_gdl, na.rm = TRUE)
#     
#      ) %>%
#   ungroup()
# 
# data_median <- data_median %>% 
#   arrange(year, ssp, iso3) %>% 
#   group_by(year, ssp, iso3) %>%
#   mutate(
#     # Apply weighting only to significant values at the country level
#     country_weighted_median_sign_delta_hdi = weighted.mean(ifelse(sign_hdi_90, median_delta_hdi, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_income_index = weighted.mean(ifelse(sign_income_index_90, median_delta_income_index, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_lifex_index = weighted.mean(ifelse(sign_lifex_index_90, median_delta_lifex_index, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_edu_index = weighted.mean(ifelse(sign_edu_index_90, median_delta_edu_index, NA), pop_gdl, na.rm = TRUE),
#     
#     country_weighted_median_sign_delta_hdi_q10 = weighted.mean(ifelse(sign_hdi_90, median_delta_hdi_q10, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_income_index_q10 = weighted.mean(ifelse(sign_income_index_90, median_delta_income_index_q10, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_lifex_index_q10 = weighted.mean(ifelse(sign_lifex_index_90, median_delta_lifex_index_q10, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_edu_index_q10 = weighted.mean(ifelse(sign_edu_index_90, median_delta_edu_index_q10, NA), pop_gdl, na.rm = TRUE),
#     
#     country_weighted_median_sign_delta_hdi_q90 = weighted.mean(ifelse(sign_hdi_90, median_delta_hdi_q90, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_income_index_q90 = weighted.mean(ifelse(sign_income_index_90, median_delta_income_index_q90, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_lifex_index_q90 = weighted.mean(ifelse(sign_lifex_index_90, median_delta_lifex_index_q90, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_delta_edu_index_q90 = weighted.mean(ifelse(sign_edu_index_90, median_delta_edu_index_q90, NA), pop_gdl, na.rm = TRUE),
#     
#     # values cc
#     country_weighted_median_sign_cc_hdi = weighted.mean(ifelse(sign_hdi_90, median_cc_hdi, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_cc_income_index = weighted.mean(ifelse(sign_income_index_90, median_cc_income_index, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_cc_lifex_index = weighted.mean(ifelse(sign_lifex_index_90, median_cc_lifex_index, NA), pop_gdl, na.rm = TRUE),
#     country_weighted_median_sign_cc_edu_index = weighted.mean(ifelse(sign_edu_index_90, median_cc_edu_index, NA), pop_gdl, na.rm = TRUE),
#     
#   ) %>%
#   ungroup()

################################################################################

# specific ssp, iso damages with 90% CI 

data_hdi_climate_country_pop_weight_1990_2021 <- read_csv("output/data_hdi_climate_country_pop_weight_1990_2021.csv")


selected_countries<-c("AFG","UKR", "CAN", #t1
                      "ETH", "LBY", "MNG", #t2
                      "MRT" ,"VNM" ,"SAU")


out_dir_sel<-file.path(out_dir, "sel_countries")
if(!dir.exists(out_dir_sel)){dir.create(out_dir_sel)}


g<-ggplot(data_hdi_climate_country_pop_weight_1990_2021%>%filter(iso3 %in% selected_countries), 
          aes(x=TM, y=hdi, col=iso3))+geom_point()+theme_bw()+ 
  geom_label(data=data_hdi_climate_country_pop_weight_1990_2021%>%filter(iso3 %in% selected_countries, year==2005), aes(x=TM, y=hdi, label=iso3))+
  theme(legend.position = "none")
ggsave(filename=file.path(out_dir_sel, paste0( "sel_countries_hist_hdi_TM.png")), g, width=10, height=8)

try<-data_hdi_climate_country_pop_weight_1990_2021[which(data_hdi_climate_country_pop_weight_1990_2021$TM<10 & 
                                                      data_hdi_climate_country_pop_weight_1990_2021$hdi<60  ),]

for (y in c(2030, 2050,2080, 2100)){
  for (sc in unique(data_median$ssp)){
    
    plots_time_country<-list()
    plots_country<-list()
    plots_gdl<-list()
    
    for (c in selected_countries ){
      c_median<-data_median%>%filter(iso3==c)
      #c_median<-unique(c_median[, c(1:4,14:29)])
      
      
     # g<-ggplot(c_median%>%filter(ssp==sc))+
     #   geom_hline(yintercept=0, size=0.5)+
     #   geom_ribbon(aes(x=year,ymin=100*country_weighted_median_delta_hdi_q10, ymax=100*country_weighted_median_delta_hdi_q90, col="hdi"), alpha = 0.2, linetype = 0)+
     #   geom_line(aes(x=year,y=100*country_weighted_median_delta_hdi, col="hdi"))+
     #   geom_ribbon(aes(x=year,ymin=100*country_weighted_median_delta_edu_index_q10, ymax=100*country_weighted_median_delta_edu_index_q90, col="edu_index"), alpha = 0.2, linetype = 0)+
     #   geom_line(aes(x=year,y=100*country_weighted_median_delta_edu_index, col="edu_index"))+
     #   geom_ribbon(aes(x=year,ymin=100*country_weighted_median_delta_income_index_q10, ymax=100*country_weighted_median_delta_income_index_q90, col="income_index"), alpha = 0.2, linetype = 0)+
     #   geom_line(aes(x=year,y=100*country_weighted_median_delta_income_index, col="income_index"))+
     #   geom_ribbon(aes(x=year,ymin=100*country_weighted_median_delta_lifex_index_q10, ymax=100*country_weighted_median_delta_lifex_index_q90, col="lifex_index"), alpha = 0.2, linetype = 0)+
     #   geom_line(aes(x=year,y=100*country_weighted_median_delta_lifex_index, col="lifex_index"))+theme_bw()+
     #   labs(
     #     title = paste0(c),
     #     x = "Variable",
     #     y = "Index Loss (% points)")+theme_bw()
     # plots_time_country[[length(plots_time_country)+1]]<-g
      
      g<-ggplot(c_median%>%filter(ssp==sc, year==y))+
        geom_hline(yintercept=0, size=0.5)+
        geom_pointrange(aes(x="hdi",y=100*median_delta_hdi,  ymin = 100*median_delta_hdi_q10, ymax = 100*median_delta_hdi_q90, col="hdi"), linewidth=1.2)+
        geom_pointrange(aes(x="edu",y=100*median_delta_edu_index,  ymin = 100*median_delta_edu_index_q10, ymax = 100*median_delta_edu_index_q90, col="edu_index"))+
        geom_pointrange(aes(x="income",y=100*median_delta_income_index,  ymin = 100*median_delta_income_index_q10, ymax = 100*median_delta_income_index_q90, col="income_index"))+
        geom_pointrange(aes(x="lifex",y=100*median_delta_lifex_index,  ymin = 100*median_delta_lifex_index_q10, ymax = 100*median_delta_lifex_index_q90, col="lifex_index"))+facet_wrap(~gdlcode)+
        labs(
          title = paste0(c),
          x = "Variable",
          y = "Index Loss (% points)")+theme_bw()
      # geom_point(aes(x="hdi",y=median_delta_hdi, col="hdi"))+
      # geom_point(aes(x="edu",y=median_delta_edu_index, col="edu_index"))+
      # geom_point(aes(x="income",y=median_delta_income_index, col="income_index"))+
      # geom_point(aes(x="lifex",y=median_delta_lifex_index, col="lifex_index"))+facet_wrap(~gdlcode)
      plots_gdl[[length(plots_gdl)+1]]<-g
      
      # g<-ggplot(c_median%>%filter(ssp==sc, year==y))+
      #   geom_hline(yintercept=0, size=0.5)+
      #   geom_pointrange(aes(x="hdi",y=100*country_weighted_median_delta_hdi,  ymin = 100*country_weighted_median_delta_hdi_q10, ymax = 100*country_weighted_median_delta_hdi_q90, col="hdi"), linewidth=1.2 ,show.legend = F)+
      #   geom_pointrange(aes(x="edu",y=100*country_weighted_median_delta_edu_index,  ymin = 100*country_weighted_median_delta_edu_index_q10, ymax = 100*country_weighted_median_delta_edu_index_q90, col="edu_index"), show.legend = F)+
      #   geom_pointrange(aes(x="income",y=100*country_weighted_median_delta_income_index,  ymin = 100*country_weighted_median_delta_income_index_q10, ymax = 100*country_weighted_median_delta_income_index_q90, col="income_index"), show.legend = F)+
      #   geom_pointrange(aes(x="lifex",y=100*country_weighted_median_delta_lifex_index,  ymin = 100*country_weighted_median_delta_lifex_index_q10, ymax = 100*country_weighted_median_delta_lifex_index_q90, col="lifex_index"), show.legend = F)+
      #   labs(
      #     title = paste0(c),
      #     x = "Variable",
      #     y = "Index Loss (% points)")+theme_bw()
      # # geom_point(aes(x="hdi",y=country_weighted_median_sign_delta_hdi, col="hdi"))+
      # # geom_point(aes(x="edu",y=country_weighted_median_sign_delta_edu_index, col="edu_index"))+
      # # geom_point(aes(x="income",y=country_weighted_median_sign_delta_income_index, col="income_index"))+
      # # geom_point(aes(x="lifex",y=country_weighted_median_sign_delta_lifex_index, col="lifex_index"))
      # plots_country[[length(plots_country)+1]]<-g
    }
    
    # g1<-ggarrange(plotlist=plots_time_country, ncol=3, nrow=3)
    g2<-ggarrange(plotlist=plots_gdl, ncol=3, nrow=3, common.legend = TRUE)
    # g3<-ggarrange(plotlist=plots_country, ncol=3, nrow=3)
    
    # ggsave(filename=file.path(out_dir_sel, paste0("plots_times_countries_all_indeces_",sc, "_", spec_type, "_", effect,"_", ".png")), g1, width=15, height=15)
    ggsave(filename=file.path(out_dir_sel, paste0("plots_", y, "_gdlcodes_all_indeces_",sc, "_", spec_type, "_", effect,"_", ".png")), g2, width=20, height=20)
    # ggsave(filename=file.path(out_dir_sel, paste0("plots_", y, "_countries_all_indeces__",sc, "_", spec_type, "_", effect,"_", ".png")), g3, width=10, height=10)
    
    
  }
}

######
########################################### maps ###############################

# maps for selected years, all ssp separated


#percentage changes wrt baseline
data_median<-data_median%>%
  mutate(median_delta_perc_hdi=100*median_delta_hdi/hdi,
         median_delta_perc_income_index=100*median_delta_income_index/income_index,
         median_delta_perc_edu_index=100*median_delta_edu_index/edu_index,
         median_delta_perc_lifex_index=100*median_delta_lifex_index/lifex_index)


years<-c(2050, 2100)


for (sc in unique(data_median$ssp)){
  for (y in years){
    
    gc()
    plot_map<-data_median%>%
      filter(ssp %in% sc)%>%
      filter(year %in% y)
    
    plot_map<-inner_join(plot_map, gdl_shape_file)
    plot_map<-sf::st_as_sf(plot_map)
    
    # 90 , absolute changes
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_hdi*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("HDI change (% points)") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_hdi_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_income_index*100), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Income Index change (% points)") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_income_index_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01)  + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_income_index_", spec,'_',vars,"_", sc ,"_", y,  "_dam.png")), g,width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_lifex_index*100), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_lifex_index_90 == 0),
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01)  + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_dam.png")), g, width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_edu_index*100), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Edu Index change (% points)") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_edu_index_90 == 0),
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01) + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_edu_index_", spec,'_',vars,"_", sc , "_", y,"_dam.png")), g, width=9, height=4)
   
    # 90, percentage changes wrt baseline
    
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_perc_hdi), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
      labs(fill=paste0("HDI perc change") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data =plot_map %>% filter(sign_hdi_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_perc_income_index), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Income Index perc change") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_income_index_90 == 0), 
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01)  + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_income_index_", spec,'_',vars,"_", sc ,"_", y,  "_perc_dam.png")), g,width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_perc_lifex_index), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Lifex Index perc change") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_lifex_index_90 == 0),
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01)  + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_lifex_index_", spec,'_',vars,"_", sc ,"_", y, "_perc_dam.png")), g, width=9, height=4)
    
    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_perc_edu_index), color = 'black', lwd=0.08)+theme_bw()+  scale_fill_gradient2() + 
      labs(fill=paste0("Edu Index perc change") )+ ggtitle(paste0("Effect wrt baseline, ssp ",sc," year ", y)) + 
      geom_sf_pattern(data = plot_map %>% filter(sign_edu_index_90 == 0),
                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08, pattern_colour = NA,
                      fill = "white", size=0.1, pattern_spacing=0.01) + guides(pattern="non-sign")
    ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_edu_index_", spec,'_',vars,"_", sc , "_", y,"_perc_dam.png")), g, width=9, height=4)
    
  }
  
}

################################################################################
# one year, each var , 4 ssp, 4 maps together for common legend
#  y<-2100
#  
#  # hdi
#  plots<-list()
#  for (sc in unique(data_median$ssp)){
#    gc()
#    plot_map<-data_median%>%
#      filter(ssp %in% sc)%>%
#      filter(year %in% y)
#    
#    plot_map<-inner_join(plot_map, gdl_shape_file)
#    plot_map<-sf::st_as_sf(plot_map)
#    
#    g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_hdi*100), color = 'black', lwd=0.06)+ theme_bw()+ scale_fill_gradient2() + 
#      labs(fill=paste0("HDI change (% points)") )+ ggtitle(paste0(sc))+theme(plot.title=element_text(size=10))+theme_bw()+
#      geom_sf_pattern(data =plot_map %>% filter(sign_hdi_90 == 0), 
#                      pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.06,pattern_colour = NA,
#                      fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
#    plots[[length(plots)+1]]<-g
#  }
#  
#  gg<-ggpubr::ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend = TRUE, legend="right")
#  # gg<-ggpubr::annotate_figure(gg, left = text_grob(paste0("Effect wrt baseline, year ", y), 
#  #                                       color = "black", size = 14))
#  ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), gg, width=10, height=4)

# gc()
# plot_map<-data_median%>%
#   filter(year %in% y)
# plot_map<-inner_join(plot_map, gdl_shape_file)
# plot_map<-sf::st_as_sf(plot_map)
# 
# 
# gc()
# # lifex_index
# 
# g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_lifex_index*100), color = 'black', lwd=0.07)+ theme_bw()+ scale_fill_gradient2() + 
#   labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle(paste0("Effect wrt baseline, year ", y)) + 
#   geom_sf_pattern(data =plot_map %>% filter(sign_lifex_index_90 == 0), 
#                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.07,pattern_colour = NA,
#                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") +
#   theme_bw()+
#   facet_wrap(~ssp)
# ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_lifex_index_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), g, width=7, height=3)


# one map per ssp
# y<-2050
# 
# # absolute changes 
# 
# # hdi
# plots<-list()
# for (sc in unique(data_median$ssp)){
# gc()
# plot_map<-data_median%>%
#   filter(ssp %in% sc)%>%
#   filter(year %in% y)
# 
# plot_map<-inner_join(plot_map, gdl_shape_file)
# plot_map<-sf::st_as_sf(plot_map)
# 
# g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_hdi*100), color = 'black', lwd=0.06)+ theme_bw()+ scale_fill_gradient2() + 
#   labs(fill=paste0("HDI change (% points)") )+ ggtitle(paste0(sc))+theme(plot.title=element_text(size=10))+theme_bw()+
#   geom_sf_pattern(data =plot_map %>% filter(sign_hdi_90 == 0), 
#                   pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.06,pattern_colour = NA,
#                   fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
# plots[[length(plots)+1]]<-g
# }
# 
# gg<-ggpubr::ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend = TRUE, legend="right")
# # gg<-ggpubr::annotate_figure(gg, left = text_grob(paste0("Effect wrt baseline, year ", y), 
# #                                       color = "black", size = 14))
# ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_hdi_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), gg, width=10, height=4)
# 
# 
# 
# gc()
# # income_index
# plots<-list()
# for (sc in unique(data_median$ssp)){
#   gc()
#   plot_map<-data_median%>%
#     filter(ssp %in% sc)%>%
#     filter(year %in% y)
#   
#   plot_map<-inner_join(plot_map, gdl_shape_file)
#   plot_map<-sf::st_as_sf(plot_map)
#   
#   g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_income_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
#     labs(fill=paste0("Income Index change (% points)") )+ ggtitle(paste0(sc)) + 
#     geom_sf_pattern(data =plot_map %>% filter(sign_income_index_90 == 0), 
#                     pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
#                     fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
#   plots[[length(plots)+1]]<-g
# }
# 
# gg<-ggpubr::ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend = TRUE, legend="right")
# 
# ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_income_index_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), gg, width=15, height=6)
# 
# 
# 
# gc()
# # lifex_index
# plots<-list()
# for (sc in unique(data_median$ssp)){
#   gc()
#   plot_map<-data_median%>%
#     filter(ssp %in% sc)%>%
#     filter(year %in% y)
#   
#   plot_map<-inner_join(plot_map, gdl_shape_file)
#   plot_map<-sf::st_as_sf(plot_map)
#   
#   g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_lifex_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
#     labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle(paste0(sc)) + 
#     geom_sf_pattern(data =plot_map %>% filter(sign_lifex_index_90 == 0), 
#                     pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
#                     fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
#   plots[[length(plots)+1]]<-g
# }
# 
# gg<-ggpubr::ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend = TRUE, legend="right")
# 
# ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_lifex_index_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), gg, width=15, height=6)
# 
# 
# 
# 
# gc()
# # edu_index
# plots<-list()
# for (sc in unique(data_median$ssp)){
#   gc()
#   plot_map<-data_median%>%
#     filter(ssp %in% sc)%>%
#     filter(year %in% y)
#   
#   plot_map<-inner_join(plot_map, gdl_shape_file)
#   plot_map<-sf::st_as_sf(plot_map)
#   
#   g<-ggplot(plot_map)+geom_sf( aes(fill=median_delta_edu_index*100), color = 'black', lwd=0.08)+ theme_bw()+ scale_fill_gradient2() + 
#     labs(fill=paste0("Lifex Index change (% points)") )+ ggtitle(paste0(sc)) +  
#     geom_sf_pattern(data =plot_map %>% filter(sign_edu_index_90 == 0), 
#                     pattern = "stripe",pattern_fill = "gray50",color='black', lwd=0.08,pattern_colour = NA,
#                     fill = "white", size=0.1, pattern_spacing=0.01 )  + guides(pattern="non-sign") 
#   plots[[length(plots)+1]]<-g
# }
# 
# gg<-ggpubr::ggarrange(plotlist=plots, ncol=2, nrow=2, common.legend = TRUE, legend="right")
# 
# ggsave(filename=file.path(out_dir_maps, paste0("median_mod_sign_90_edu_index_", spec,'_',vars,"_all_ssp_", y,"_dam.png")), gg, width=15, height=6)



######################################### components relations #################

### barplot with importance of each comp, globally 

library(ggplot2)
library(dplyr)
library(tidyr)

# lets decompose hdi damage with logarithmic approximation, only sign
# data_median<-data_median%>%
#   mutate(comp_lifex= (1/3)*hdi*median_delta_lifex_index/lifex_index)%>%
#   mutate(comp_edu= (1/3)*hdi*median_delta_edu_index/edu_index)%>%
#   mutate(comp_income= (1/3)*hdi*median_delta_income_index/income_index)
# 
# data_median <- data_median %>% 
#   arrange(year, ssp) %>% 
#   group_by(year, ssp) %>%
#   mutate(
#     # Apply weighting only to significant values
#     glob_comp_lifex = weighted.mean(ifelse(sign_lifex_index_90, comp_lifex, NA), pop_gdl, na.rm = TRUE),
#     glob_comp_edu = weighted.mean(ifelse(sign_edu_index_90, comp_edu, NA), pop_gdl, na.rm = TRUE),
#     glob_comp_income = weighted.mean(ifelse(sign_income_index_90, comp_income, NA), pop_gdl, na.rm = TRUE)
#     )


data_median<-data_median%>%
  mutate(glob_comp_lifex= (1/3)*global_weighted_median_hdi*global_weighted_median_sign_delta_lifex_index/global_weighted_median_lifex_index)%>%
  mutate(glob_comp_edu= (1/3)*global_weighted_median_hdi*global_weighted_median_sign_delta_edu_index/global_weighted_median_edu_index)%>%
  mutate(glob_comp_income= (1/3)*global_weighted_median_hdi*global_weighted_median_sign_delta_income_index/global_weighted_median_income_index)


for (y in c(2030, 2050, 2080, 2100)){

# all
 hdi_val<- data_median %>% filter(year==y)%>%
   select(ssp, global_weighted_median_sign_delta_hdi)
 hdi_val<-unique(hdi_val)
 print(hdi_val)
data_long <- data_median %>% filter(year==y)%>%ungroup()%>%
  select(ssp, glob_comp_income, 
         glob_comp_lifex, 
         glob_comp_edu) %>%
  pivot_longer(cols = -ssp, 
               names_to = "component", 
               values_to = "delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("glob_comp_income", 
                                         "glob_comp_lifex", 
                                         "glob_comp_edu"),
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = ssp, y = delta_value*100, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
  scale_fill_manual(values =c("#FFA1CB", "#56B4F9", "#009E73") ) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(title = paste0("Year ",y),
       x = "SSP Scenario", 
       y = "Total Loss (% points)",
       fill = "Component") +
  ylim(-4.8, 0)+
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)


# sign, simple
data_long <- data_median %>% filter(year==y)%>%ungroup()%>%
  select(ssp, global_weighted_median_sign_delta_income_index, 
         global_weighted_median_sign_delta_lifex_index, 
         global_weighted_median_sign_delta_edu_index) %>%
  pivot_longer(cols = -ssp, 
               names_to = "component", 
               values_to = "delta_value")
data_long<-unique(data_long)
# 
data_long$component <- factor(data_long$component, 
                              levels = c("global_weighted_median_sign_delta_income_index", 
                                         "global_weighted_median_sign_delta_lifex_index", 
                                         "global_weighted_median_sign_delta_edu_index"),
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))
# 
g<-ggplot(data_long, aes(x = ssp, y = 100*delta_value, fill = component)) +
  geom_bar(stat = "identity", position = "stack") +  # 'fill' scales the bars to 100%
  scale_fill_manual(values =c("#FFA1CB", "#56B4F9", "#009E73") ) +
  #scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(title = paste0("Year ",y),
       x = "SSP Scenario", 
       y = "Total Loss (% points)",
       fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_simple_dam_share_ssp_comparison_",y, "_",spec,'_',vars, ".png")), g, width=8, height=5)

}


# over time 

data_long <- data_median %>%
  select(year, ssp, global_weighted_median_sign_delta_hdi,
         glob_comp_income, 
         glob_comp_lifex, 
         glob_comp_edu) %>%
  pivot_longer(cols = starts_with("glob_comp"), 
               names_to = "component", 
               values_to = "delta_value")
data_long<-unique(data_long)

data_long$component <- factor(data_long$component, 
                              levels = c("glob_comp_income", 
                                         "glob_comp_lifex", 
                                         "glob_comp_edu"),
                              labels = c("Income Index", "Life Expectancy Index", "Education Index"))

g<-ggplot(data_long, aes(x = year, y = delta_value*100, fill = as.factor(component))) +
  geom_area() +
  scale_fill_manual(values =c("#FFA1CB", "#56B4F9", "#009E73") ) +
  #geom_line(aes(x=year, y=100*global_weighted_median_sign_delta_hdi))+
  facet_wrap(~ ssp) +
  # scale_y_continuous(labels = scales::percent_format()) +  # Convert to percentage
  labs(
       x = "SSP Scenario", 
       y = "Total Loss (% points)",
       fill = "Component") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x labels for readability
ggsave(filename=file.path(out_dir, paste0("sign_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=8, height=5)


# data_long <- data_median %>%
#   select(year, ssp, 
#          global_weighted_median_sign_delta_income_index, 
#          global_weighted_median_sign_delta_lifex_index, 
#          global_weighted_median_sign_delta_edu_index) %>%
#   pivot_longer(cols = starts_with("global_weighted_median_sign_delta"), 
#                names_to = "component", 
#                values_to = "damage")
# data_long<-unique(data_long)
# 
# data_long$component <- factor(data_long$component, 
#                               levels = c("global_weighted_median_sign_delta_income_index", 
#                                          "global_weighted_median_sign_delta_lifex_index", 
#                                          "global_weighted_median_sign_delta_edu_index"),
#                               labels = c("Income Index", "Life Expectancy Index", "Education Index"))
# 
# 
# g<-ggplot(data_long, aes(x = year, y = damage*100, fill = as.factor(component))) +
#   geom_area() +
#   facet_wrap(~ ssp) +
#   labs(title = "Component Contribution to HDI Damage Over Time", 
#        x = "Year", 
#        y = "Damage Contribution", 
#        fill = "Component") +
#   scale_fill_manual(values = c("Income Index" = "#FFC0CB", 
#                                "Life Expectancy Index" = "#56B4E9", 
#                                "Education Index" = "#009E73")) +
#   theme_bw()
# ggsave(filename=file.path(out_dir, paste0("sign_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=10, height=5)
# 
# # all
# 
# data_long <- data_median %>%
#   select(year, ssp, 
#          global_weighted_median_delta_income_index, 
#          global_weighted_median_delta_lifex_index, 
#          global_weighted_median_delta_edu_index) %>%
#   pivot_longer(cols = starts_with("global_weighted_median_delta"), 
#                names_to = "component", 
#                values_to = "damage")
# data_long<-unique(data_long)
# 
# data_long$component <- factor(data_long$component, 
#                               levels = c("global_weighted_median_delta_income_index", 
#                                          "global_weighted_median_delta_lifex_index", 
#                                          "global_weighted_median_delta_edu_index"),
#                               labels = c("Income Index", "Life Expectancy Index", "Education Index"))
# 
# 
# g<-ggplot(data_long, aes(x = year, y = damage*100, fill = as.factor(component))) +
#   geom_area() +
#   facet_wrap(~ ssp) +
#   labs(title = "Component Contribution to HDI Damage Over Time", 
#        x = "Year", 
#        y = "Damage Contribution", 
#        fill = "Component") +
#   scale_fill_manual(values = c("Income Index" = "#FFC0CB", 
#                                "Life Expectancy Index" = "#56B4E9", 
#                                "Education Index" = "#009E73")) +
#   theme_bw()
# ggsave(filename=file.path(out_dir, paste0("all_evol_dam_share_ssp_comparison_",spec,'_',vars, ".png")), g, width=10, height=5)



################################################################################

# indeces relations
g<-ggplot(data_median, aes(x = global_weighted_median_delta_income_index, 
                     y = global_weighted_median_delta_lifex_index, 
                     color = global_weighted_median_delta_edu_index),size=2) +
  facet_wrap(~ ssp) +
  geom_point() +
  labs(title = "Income vs Life Expectancy Damages", 
       x = "Delta Income Index", 
       y = "Delta Life Expectancy Index", 
       color = "Delta Education Index") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename=file.path(out_dir, paste0("all_gni_lifex_relations_by_ssp_",spec,'_',vars, ".png")), g, width=10, height=7)

g<-ggplot(data_median, aes(x = global_weighted_median_delta_income_index, 
                           y = global_weighted_median_delta_edu_index, 
                           color = global_weighted_median_delta_lifex_index),size=2) +
  facet_wrap(~ ssp) +
  geom_point() +
  labs(title = "Income vs Education Damages", 
       x = "Delta Income Index", 
       y = "Delta Education Index", 
       color = "Delta Lifex Index") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave(filename=file.path(out_dir, paste0("all_gni_edu_relations_by_ssp_",spec,'_',vars, ".png")), g, width=10, height=7)


# indeces relations, sign
g<-ggplot(data_median, aes(x = global_weighted_median_sign_delta_income_index, 
                           y = global_weighted_median_sign_delta_lifex_index, 
                           color = global_weighted_median_sign_delta_edu_index), size=2) +
  facet_wrap(~ ssp) +
  geom_point() +
  labs(title = "Income vs Life Expectancy Damages", 
       x = "Delta Income Index", 
       y = "Delta Life Expectancy Index", 
       color = "Delta Education Index") +
  theme_bw() 
ggsave(filename=file.path(out_dir, paste0("sign_gni_lifex_relations_by_ssp_",spec,'_',vars, ".png")), g, width=10, height=7)

g<-ggplot(data_median, aes(x = global_weighted_median_sign_delta_income_index, 
                           y = global_weighted_median_sign_delta_edu_index, 
                           color = global_weighted_median_sign_delta_lifex_index), size=2) +
  facet_wrap(~ ssp) +
  geom_point() +
  labs(title = "Income vs Education Damages", 
       x = "Delta Income Index", 
       y = "Delta Education Index", 
       color = "Delta Lifex Index") +
  theme_bw() 
ggsave(filename=file.path(out_dir, paste0("sign_gni_edu_relations_by_ssp_",spec,'_',vars, ".png")), g, width=10, height=7)


