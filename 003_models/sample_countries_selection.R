### find countries with low, med, high x low, med, high mean temp historically



rm(list=ls())

library(dplyr)
library(readr)
library(ggplot2)
source("scripts/003_models/feols_lags_plot_funcs.R")

spec<-"mean_mod"
type="all_extr_tp"
NL<-"_mix_"

out_dir<-"output/eda"

################################################################################
# hist pop data

library(readr)
data_pop <- read_csv("data/data_population/gdl_population_1990_2022.csv")
data_pop<-data_pop[, c(3,5,7:39)]
library(data.table)
data_pop <- melt(setDT(data_pop), id.vars = c("ISO_Code","GDLCODE"), variable.name = "year")
colnames(data_pop)<-c("iso3", "gdlcode", "year", "pop")

################################################################################

### data
data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")

all_controls <- read_csv("data/controls/all_controls.csv")

data<-left_join(data,all_controls )

### data climate
data_cl <- read_csv("data/climate_data/era5/data_climate_gdl_pop_weight_1950_2023.csv")


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(modns[i],'_i_',varns[i],sep='')] <- data[modns[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste(adap[i],'_i_',varns[i],sep='')] <- data[adap[i]] * data[varns[i]]
} 


for (i in 1:length(modns)){ # 
  data[paste(adap[i],'_i_',modns[i],sep='')] <- data[adap[i]] * data[modns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',modns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',modns[i],sep='')] <- data["RR"] * data[varns[i]]
} 



### for each variable, create up to 10 lags 
for ( i in 0:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",varns[v])]<-fixest::lag_fml(as.formula(paste0(varns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",modns[v])]<-fixest::lag_fml(as.formula(paste0(modns[v], "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(modns[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(modns[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste(adap[v],'_i_',modns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(adap[v],'_i_',modns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# check
# data$TVAR_i_diff_TVAR
# data$lag_0_TVAR_i_diff_TVAR

# mean hist climate

# avg and ten years avg of previous years 
data<-left_join(data_cl, data)

library(slider)
data <- data %>%
  arrange(gdlcode, year) %>%  
  group_by(gdlcode) %>%
  mutate(
    TM_mean = mean(TM),
    RR_mean = mean(RR),
    TVAR_mean = mean(TVAR),
    HW_mean = mean(HW),
    RX_mean = mean(RX),
    PEXT_mean = mean(PEXT),
    WD_mean = mean(WD),
    SPI_mean = mean(SPI),
    SPEI_mean = mean(SPEI),
    PET_mean = mean(PET),
    # Compute 10-year moving averages
    TM_10y_mean = slide_dbl(TM, mean, .before = 9, .complete = TRUE),
    RR_10y_mean = slide_dbl(RR, mean, .before = 9, .complete = TRUE),
    TVAR_10y_mean = slide_dbl(TVAR, mean, .before = 9, .complete = TRUE),
    HW_10y_mean = slide_dbl(HW, mean, .before = 9, .complete = TRUE),
    RX_10y_mean = slide_dbl(RX, mean, .before = 9, .complete = TRUE),
    PEXT_10y_mean = slide_dbl(PEXT, mean, .before = 9, .complete = TRUE),
    WD_10y_mean = slide_dbl(WD, mean, .before = 9, .complete = TRUE),
    SPI_10y_mean = slide_dbl(SPI, mean, .before = 9, .complete = TRUE),
    SPEI_10y_mean = slide_dbl(SPEI, mean, .before = 9, .complete = TRUE),
    PET_10y_mean = slide_dbl(PET, mean, .before = 9, .complete = TRUE)
  )


# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET")

m_modns_mean=c('TM_mean','RR_mean', "TVAR_mean", "HW_mean", "RX_mean", "PEXT_mean", "WD_mean", "SPI_mean", "SPEI_mean", "PET_mean")
m_modns_10y=c('TM_10y_mean','RR_10y_mean', "TVAR_10y_mean", "HW_10y_mean", "RX_10y_mean", "PEXT_10y_mean", "WD_10y_mean", "SPI_10y_mean", "SPEI_10y_mean", "PET_10y_mean")
m_modns_cont=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET")


adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

for (i in 1:length(varns)){
  data[paste(m_modns_mean[i],'_i_',varns[i],sep='')] <- data[m_modns_mean[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_10y[i],'_i_',varns[i],sep='')] <- data[m_modns_10y[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){
  data[paste(m_modns_cont[i],'_i_',varns[i],sep='')] <- data[m_modns_cont[i]] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM_mean",'_i_',varns[i],sep='')] <- data["TM_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_mean",'_i_',varns[i],sep='')] <- data["RR_mean"] * data[varns[i]]
} 


for (i in 1:length(varns)){ # 
  data[paste("TM_10y_mean",'_i_',varns[i],sep='')] <- data["TM_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR_10y_mean",'_i_',varns[i],sep='')] <- data["RR_10y_mean"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("TM",'_i_',varns[i],sep='')] <- data["TM"] * data[varns[i]]
} 

for (i in 1:length(varns)){ # 
  data[paste("RR",'_i_',varns[i],sep='')] <- data["RR"] * data[varns[i]]
} 


### for each variable, create up to 10 lags 
for ( i in 0:10){
  for (v in 1:length(varns)){
    
    data[paste0("lag_",i,"_",paste("TM_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_mean[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_mean[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR_10y_mean",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR_10y_mean",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_10y[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_10y[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
    data[paste0("lag_",i,"_",paste("TM",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("TM",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    data[paste0("lag_",i,"_",paste("RR",'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste("RR",'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    # also add each mean variable moderator to its same var
    data[paste0("lag_",i,"_",paste(m_modns_cont[v],'_i_',varns[v],sep=''))]<-fixest::lag_fml(as.formula(paste0(paste(m_modns_cont[v],'_i_',varns[v],sep=''), "~gdlcode+year")), i, data)
    
  }
}

# go back to right time period

data<-data%>%filter(year>=1990 & year<=2019)


################################################################################

# max min log gnipc
max_lgnipc<-max(data$lgnipc, na.rm=TRUE)
min_lgnipc<-min(data$lgnipc, na.rm=TRUE)
# max min eys
max_eys<-max(data$eys, na.rm=TRUE)
min_eys<-min(data$eys, na.rm=TRUE)
# max min log gnipc
max_leb<-max(data$leb, na.rm=TRUE)
min_leb<-min(data$leb, na.rm=TRUE)

data$income_index<-(data$lgnipc-min_lgnipc)/(max_lgnipc-min_lgnipc)
data$lifex_index<-(data$leb-min_leb)/(max_leb-min_leb)
data$edu_index<-(data$eys-min_eys)/(max_eys-min_eys)

data$hdi<-(data$income_index*data$lifex_index*data$edu_index)^(1/3)

data_pop$year<-as.numeric(as.character(data_pop$year))
data<-left_join(data,data_pop)


data<-data %>% 
  arrange( year,iso3)%>% 
  group_by( year, iso3) %>%
  mutate(country_hdi=weighted.mean(hdi, pop, na.rm=TRUE),
         country_mean_TM=weighted.mean(TM_mean, pop, na.rm=TRUE))

data_c<-unique(data%>% select(iso3,year,country_hdi, country_mean_TM)  )

df_selected <- data_c %>%
  group_by(year) %>%  # If you want to do this selection for each year separately
  mutate(
    TM_quantile = ntile(country_mean_TM, 3), # Divide temperature into 3 groups
    HDI_quantile = ntile(country_hdi, 3) # Divide HDI into 3 groups
  ) %>%
  ungroup() %>%
  filter(TM_quantile %in% c(1, 2, 3) & HDI_quantile %in% c(1, 2, 3)) %>%
  group_by(TM_quantile, HDI_quantile) %>%
  slice_sample(n = 1) # Randomly select one country per combination

# Print selected countries
print(df_selected)

df_selected <- data_c %>%
  group_by(year) %>%  # If you want to do this selection for each year separately
  mutate(
    TM_quantile = ntile(country_mean_TM, 3), # Divide temperature into 3 groups
    HDI_quantile = ntile(country_hdi, 3) # Divide HDI into 3 groups
  ) %>%
  ungroup() %>%
  group_by(TM_quantile, HDI_quantile) %>%
  filter(all(year %in% unique(data_c$year))) %>% # Ensure countries are in the same quantile groups each year
  slice_sample(n = 1) # Randomly select one country per combination

df_assigned <- data_c %>%
  group_by(year) %>%  # If you want to do this selection for each year separately
  mutate(
    TM_quantile = ntile(country_mean_TM, 3), # Divide temperature into 3 groups
    HDI_quantile = ntile(country_hdi, 3), # Divide HDI into 3 groups
    Group = paste0("T", TM_quantile, "_H", HDI_quantile) # Assign group label
  ) %>%
  ungroup()

library(tidyr)
df_assigned<-df_assigned%>%drop_na()

# plot 

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
colnames(countries)[colnames(countries)=="ISO3_CODE"]<-"iso3"
#plot(countries)

df_assigned<-inner_join(df_assigned,countries )
df_assigned<-sf::st_as_sf(df_assigned)

df_assigned$selected<-0
df_assigned$selected[which(df_assigned$iso3 %in% c("AFG","UKR", "CAN", #t1
                                                   "ETH", "LBY", "AUS", #t2
                                                   "MRT" ,"VNM" ,"SAU")) #t3
                     ]<-1
df_assigned$selected<-as.factor(df_assigned$selected)

library(ggplot2)
library(sf)

#group_colors <- viridis::viridis(9, option = "plasma")
g<-ggplot(df_assigned%>%filter(year==2019), aes(fill=Group), lwd=1)+
  geom_sf()+
  geom_sf_label(aes(label=iso3), size=1.4)+
  theme_bw()#+scale_fill_manual(values = setNames(group_colors, unique(df_assigned$Group))) 

ggsave(file.path(out_dir, paste0("temp_hdi_clustered_countries.png")), plot = g, width = 18, height = 10, dpi=300)
