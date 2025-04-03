

out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/climate_exploration"
if(!dir.exists(out_dir)){dir.create(out_dir)}


climate_projections_dir<-"output/projections/climate_projections_gdl"
agg<-"pop_weight"

gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")

regions_ind<-gdlcodes_iso$gdlcode[which(gdlcodes_iso$iso3=="IND")]
proj_ind<-data.frame()

for (r in regions_ind){
  
  load(file.path(climate_projections_dir, paste0(r,"_climate_projections_gdl_",agg,".RData")))
  climate_projections<-as.data.frame(climate_projections)
  proj_ind<-rbind(proj_ind, climate_projections)
}

proj_ind$model<-as.factor(proj_ind$model)
proj_ind$ssp<-as.factor(proj_ind$ssp)
proj_ind$model_ssp<-as.factor(paste0(proj_ind$model, "_", proj_ind$ssp ))


data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

data$lag_gni_pc<-exp(data$lag_log_gni_pc)
data_mean <- data %>% group_by(gdlcode) %>% transmute(
  TM=mean(TM), 
  RR=mean(RR), 
  TVAR=mean(TVAR), 
  HW=mean(HW), 
  RX=mean(RX), 
  PEXT=mean(PEXT), 
  WD=mean(WD), 
  SPI=mean(SPI), 
  SPEI=mean(SPEI), 
  PET=mean(PET), 
  
  TM_2=mean(TM_2), 
  RR_2=mean(RR_2), 
  TVAR_2=mean(TVAR_2), 
  HW_2=mean(HW_2), 
  RX_2=mean(RX_2), 
  PEXT_2=mean(PEXT_2), 
  WD_2=mean(WD_2), 
  SPI_2=mean(SPI_2), 
  SPEI_2=mean(SPEI_2), 
  PET_2=mean(PET_2), 
  
  lag_gni_pc=mean(lag_gni_pc, na.rm=TRUE)
  
  # diff_TM= mean(diff_TM, na.rm=TRUE), 
  # diff_RR= mean(diff_RR, na.rm=TRUE)
  # 
  # lag_log_gni_pc_i_TM=mean(lag_log_gni_pc_i_TM), 
  # lag_log_gni_pc_i_RR=mean(lag_log_gni_pc_i_RR), 
  # lag_log_gni_pc_i_TVAR=mean(lag_log_gni_pc_i_TVAR), 
  # lag_log_gni_pc_i_HW=mean(lag_log_gni_pc_i_HW), 
  # lag_log_gni_pc_i_RX=mean(lag_log_gni_pc_i_RX), 
  # lag_log_gni_pc_i_PEXT=mean(lag_log_gni_pc_i_PEXT), 
  # lag_log_gni_pc_i_WD=mean(lag_log_gni_pc_i_WD), 
  # lag_log_gni_pc_i_SPI=mean(lag_log_gni_pc_i_SPI), 
  # lag_log_gni_pc_i_SPEI=mean(lag_log_gni_pc_i_SPEI), 
  # lag_log_gni_pc_i_PET=mean(lag_log_gni_pc_i_PET), 
  # 
  # lag_log_gni_pc_i_TM_2=mean(lag_log_gni_pc_i_TM_2), 
  # lag_log_gni_pc_i_RR_2=mean(lag_log_gni_pc_i_RR_2), 
  # lag_log_gni_pc_i_TVAR_2=mean(lag_log_gni_pc_i_TVAR_2), 
  # lag_log_gni_pc_i_HW_2=mean(lag_log_gni_pc_i_HW_2), 
  # lag_log_gni_pc_i_RX_2=mean(lag_log_gni_pc_i_RX_2), 
  # lag_log_gni_pc_i_PEXT_2=mean(lag_log_gni_pc_i_PEXT_2), 
  # lag_log_gni_pc_i_WD_2=mean(lag_log_gni_pc_i_WD_2), 
  # lag_log_gni_pc_i_SPI_2=mean(lag_log_gni_pc_i_SPI_2), 
  # lag_log_gni_pc_i_SPEI_2=mean(lag_log_gni_pc_i_SPEI_2), 
  # lag_log_gni_pc_i_PET_2=mean(lag_log_gni_pc_i_PET_2), 
  
)
data_mean<-unique(data_mean)

# RX
RX_hist<-data_mean[,c("gdlcode", "RX")]
colnames(RX_hist)<-c("gdlcode", "RX_hist")

proj_ind<-inner_join(proj_ind,
                RX_hist)

proj_ind$RX_diff<-proj_ind$RX-proj_ind$RX_hist
g<-ggplot() +
  geom_point( data=proj_ind, aes(x = year, y = RX_diff ,  group=model_ssp, color=ssp, shape=model)) + 
  geom_hline(yintercept=0,linewidth = 1.1)+
  facet_wrap(~ gdlcode) +
  labs(
    title = "RX - hist RX in India",
    x = "Year",
    y = "RX - RX_hist",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()+
  theme(legend.position = "bottom") 

ggsave(filename=file.path(out_dir, paste0("RX_diff_india_proj.png")), g, width=25, height=20)

g<-ggplot() +
  geom_point( data=proj_ind, aes(x = year, y = RX_hist )) + 
  geom_hline(yintercept=0,linewidth = 1.1)+
  facet_wrap(~ gdlcode) +
  labs(
    title = "hist RX in India",
    x = "Year",
    y = "RX_hist",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()+
  theme(legend.position = "bottom") 

ggsave(filename=file.path(out_dir, paste0("RX_hist_india_proj.png")), g, width=25, height=20)


g<-ggplot() +
  geom_point( data=proj_ind, aes(x = year, y = RX,  group=model_ssp, color=ssp, shape=model)) + 
  geom_hline(yintercept=0,linewidth = 1.1)+
  facet_wrap(~ gdlcode) +
  labs(
    title = "RX in India",
    x = "Year",
    y = "RX",
    color = "ssp",
    shape= "model"
  ) +
  theme_bw()+
  theme(legend.position = "bottom") 

ggsave(filename=file.path(out_dir, paste0("RX_india_proj.png")), g, width=25, height=20)


# gnipc
gnipc_hist<-data_mean[,c("gdlcode", "lag_gni_pc")]
colnames(gnipc_hist)<-c("gdlcode", "lag_gni_pc_hist")

proj_ind<-inner_join(proj_ind,
                     gnipc_hist)

g<-ggplot() +
  geom_point( data=proj_ind, aes(x = year, y = lag_gni_pc_hist)) + 
  geom_hline(yintercept=0,linewidth = 1.1)+
  facet_wrap(~ gdlcode) +
  labs(
    title = "gnipc in India",
    x = "Year",
    y = "gnipc"
  ) +
  theme_bw()+
  theme(legend.position = "bottom") 

ggsave(filename=file.path(out_dir, paste0("gnipc_india_proj.png")), g, width=25, height=20)

