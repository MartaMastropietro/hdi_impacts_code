
### prepare dataset for models: hdi + climate, add variables needed 
### some preliminary plots for variables 
### years 1990-2021

rm(list=ls())
source("utils/libraries.R")


# output dir
out_dir<-"output"
if(!dir.exists(out_dir)){dir.create(out_dir)}


### years of analysis
years<-1990:2020

### Inequality data for dividing income groups 
#  WIID_28NOV2023 <- read_excel("data/data_inequality/downloaded/WIID_28NOV2023/WIID_28NOV2023.xlsx")
#  data_ineq <- unique(WIID_28NOV2023[, c(3,47,50)])
#  data_ineq$incomegroup[which(data_ineq$c3=="VEN")]<-"Upper middle income" # 2022 world bank classification
#  colnames(data_ineq)<- c("iso3", "region_wb", "income_group")
#  
#  ### Sub-national shape file (hdi + components data)
#  gdl_shape_file <- sf::st_read("data/data_hdi_components_gender/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
#  gdl_continent<-gdl_shape_file[, c(1,2)]
#  gdl_lon_lat<-gdl_continent
#  gdl_lon_lat<- sf::st_make_valid(gdl_lon_lat)
#  gdl_lon_lat$centroid<-sf::st_centroid(gdl_lon_lat)
#  gdl_lon_lat$lon<-sf::st_coordinates(gdl_lon_lat$centroid)[1]
#  gdl_lon_lat$lat<-sf::st_coordinates(gdl_lon_lat$centroid)[2]
#  gdl_lon_lat$centroid<-NULL
#  
#  gdl_continent$geometry<-NULL
#  gdl_lon_lat$geometry<-NULL
#  
#  iso_continent<-gdl_shape_file[, c(2,3)]
#  iso_continent$geometry<-NULL
#  iso_continent<-unique(iso_continent)
#  colnames(iso_continent)<-c("continent", "iso3")
#  
#  ### Countries borders 
#  countries<-giscoR::gisco_get_countries()
#  #simplify data countries
#  countries<-countries[,c(3,6)]
#  countries<-sf::st_as_sf(countries)

### HDI and components import 
data_dir <- "data/hdi_data"
data_out_all <- read_csv(file.path(data_dir,"data_hdi_components_gender_subnat_1990_2021.csv"), show_col_types = FALSE)
# data_out_country <- read_csv(file.path(data_dir,"data_hdi_components_gender_national_1990_2021.csv"), show_col_types = FALSE)

### data climate and extremes
data_dir <- "data/climate_data/old_aggregated_spatial"
data_cl <- read_csv(file.path(data_dir, "data_extremes_gdl_pop_weight_1960_2022.csv"), show_col_types = FALSE)
# data_cl_country <- read_csv(file.path(data_dir, "data_extremes_country_pop_weight_1960_2022.csv"), show_col_types = FALSE)


### unify 
data <- dplyr::inner_join(data_cl, data_out_all) # if gdl not present in data, we dont add useless NA
data[is.na(data) | data=="Inf" | data=="-Inf"] = NA

### add column with iso info
options(max.print=1000000)
print(unique(data$gdlcode))
data$iso3<-substr(data$gdlcode, 1, 3)

# data_country <- dplyr::inner_join(data_out_country, data_cl_country)
# data_country[is.na(data_country) | data_country=="Inf" | data_country=="-Inf"] = NA

# #add lon lat to gdl
# data<-dplyr::inner_join(data, gdl_lon_lat)

###  cut years
data<-data[which(data$year<=range(years)[2] & data$year>=range(years)[1]), ]
# data_country<-data_country[which(data_country$year<=range(years)[2] & data_country$year>=range(years)[1]), ]



### add continent, region_wb, income_group
# data<- dplyr::left_join(data, data_ineq)
# sum(is.na(data$income_group)) 
# unique(data$gdlcode[is.na(data$income_group)]) # islands, kosovo, liechtenstein
# 
# data<- dplyr::left_join(data, gdl_continent)
# sum(is.na(data$continent)) #0

# data_country <- dplyr::left_join(data_country, data_ineq)
# sum(is.na(data_country$income_group)) 
# unique(data_country$iso3[is.na(data_country$income_group)]) # islands, liechtenstein
# 
# data_country <- dplyr::left_join(data_country, iso_continent)
# sum(is.na(data_country$continent)) #46
# unique(data_country$iso3[is.na(data_country$continent)]) # "MDV" "TUV"


# make percentage vars, interpret coefs as effect on 1 percentage point 
data$hdi <- data$hdi*100
data$health_index <- data$health_index*100
data$edu_index <- data$edu_index*100
data$income_index <- data$income_index*100
# gdi is between -1 and 1, turn into -100 and 100
data$gdi <- data$gdi*100 

# data_country$hdi <- data_country$hdi*100
# data_country$health_index <- data_country$health_index*100
# data_country$edu_index <- data_country$edu_index*100
# data_country$income_index <- data_country$income_index*100
# # gdi is between -1 and 1, turn into -100 and 100
# data_country$gdi <- data_country$gdi*100 


###############################################################################

# add log vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(log_hdi =log(hdi))%>% 
  dplyr::mutate(log_health_index =log(health_index))%>% 
  dplyr::mutate(log_edu_index =log(edu_index))%>% 
  dplyr::mutate(log_income_index =log(income_index))%>% 
  dplyr::mutate(log_gdi =log(gdi))%>% 
  ungroup()

# add diff log (growth rate) vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(gr_hdi = log_hdi - dplyr::lag(log_hdi, n = 1, default = NA))%>% 
  dplyr::mutate(gr_health_index = log_health_index - dplyr::lag(log_health_index, n = 1, default = NA))%>% 
  dplyr::mutate(gr_edu_index = log_edu_index - dplyr::lag(log_edu_index, n = 1, default = NA))%>% 
  dplyr::mutate(gr_income_index = log_income_index - dplyr::lag(log_income_index, n = 1, default = NA))%>% 
  dplyr::mutate(gr_gdi = log_gdi - dplyr::lag(log_gdi, n = 1, default = NA))%>% 
  dplyr::mutate(gr_log_gni_pc = log_gni_pc - dplyr::lag(log_gni_pc, n = 1, default = NA))%>% 
  ungroup()

# add diff vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_hdi = hdi - dplyr::lag(hdi, n = 1, default = NA))%>% 
  dplyr::mutate(diff_health_index = health_index - dplyr::lag(health_index, n = 1, default = NA))%>% 
  dplyr::mutate(diff_edu_index = edu_index - dplyr::lag(edu_index, n = 1, default = NA))%>% 
  dplyr::mutate(diff_income_index = income_index - dplyr::lag(income_index, n = 1, default = NA))%>% 
  dplyr::mutate(diff_gdi = gdi - dplyr::lag(gdi, n = 1, default = NA))%>% 
  dplyr::mutate(diff_log_gni_pc = log_gni_pc - dplyr::lag(log_gni_pc, n = 1, default = NA))%>% 
  ungroup()

# add diff log (growth rate) vars of time t+1 (lead instead of log)
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lead_gr_hdi = dplyr::lead(log_hdi, n = 1, default = NA) - log_hdi )%>% 
  dplyr::mutate(lead_gr_health_index = dplyr::lead(log_health_index, n = 1, default = NA) -  log_health_index )%>% 
  dplyr::mutate(lead_gr_edu_index =  dplyr::lead(log_edu_index, n = 1, default = NA) - log_edu_index )%>% 
  dplyr::mutate(lead_gr_income_index = dplyr::lead(log_income_index, n = 1, default = NA) - log_income_index )%>% 
  dplyr::mutate(lead_gr_gdi =  dplyr::lead(log_gdi, n = 1, default = NA) - log_gdi )%>% 
  dplyr::mutate(lead_gr_log_gni_pc = dplyr::lead(log_gni_pc, n = 1, default = NA) - log_gni_pc)%>% 
  ungroup()

# add diff vars of time t+1 (lead instead of log)
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lead_diff_hdi = - hdi + dplyr::lead(hdi, n = 1, default = NA))%>% 
  dplyr::mutate(lead_diff_health_index =  - health_index + dplyr::lead(health_index, n = 1, default = NA))%>% 
  dplyr::mutate(lead_diff_edu_index =  - edu_index + dplyr::lead(edu_index, n = 1, default = NA))%>% 
  dplyr::mutate(lead_diff_income_index = - income_index + dplyr::lead(income_index, n = 1, default = NA))%>% 
  dplyr::mutate(lead_diff_gdi = - gdi + dplyr::lead(gdi, n = 1, default = NA))%>% 
  dplyr::mutate(lead_diff_log_gni_pc = - log_gni_pc + dplyr::lead(log_gni_pc, n = 1, default = NA))%>% 
  ungroup()

# add lag diff vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_diff_hdi = dplyr::lag(diff_hdi, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_health_index = dplyr::lag(diff_health_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_edu_index =  dplyr::lag(diff_edu_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_income_index =  dplyr::lag(diff_income_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_gdi = dplyr::lag(diff_gdi, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_log_gni_pc =  dplyr::lag(diff_log_gni_pc, n = 1, default = NA))%>% 
  ungroup()

# add lag growth vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_gr_hdi = dplyr::lag(gr_hdi, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_health_index = dplyr::lag(gr_health_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_edu_index =  dplyr::lag(gr_edu_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_income_index =  dplyr::lag(gr_income_index, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_gdi = dplyr::lag(gr_gdi, n = 1, default = NA))%>% 
  dplyr::mutate(lag_gr_log_gni_pc =  dplyr::lag(gr_log_gni_pc, n = 1, default = NA))%>% 
  ungroup()

# add lead diff vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lead_diff_hdi = dplyr::lead(hdi, n = 1, default = NA) - hdi )%>% 
  dplyr::mutate(lead_diff_health_index =  dplyr::lead(health_index, n = 1, default = NA) - health_index )%>% 
  dplyr::mutate(lead_diff_edu_index =   dplyr::lead(edu_index, n = 1, default = NA) - edu_index)%>% 
  dplyr::mutate(lead_diff_income_index =   dplyr::lead(income_index, n = 1, default = NA) - income_index)%>% 
  ungroup()


# add lead vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lead_hdi = dplyr::lead(hdi, n = 1, default = NA)  )%>% 
  dplyr::mutate(lead_health_index =  dplyr::lead(health_index, n = 1, default = NA)  )%>% 
  dplyr::mutate(lead_edu_index =   dplyr::lead(edu_index, n = 1, default = NA) )%>% 
  dplyr::mutate(lead_income_index =   dplyr::lead(income_index, n = 1, default = NA) )%>% 
  ungroup()

# add lag vars
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_hdi = dplyr::lag(hdi, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_health_index =  dplyr::lag(health_index, n = 1, default = NA)  )%>% 
  dplyr::mutate(lag_edu_index =   dplyr::lag(edu_index, n = 1, default = NA) )%>% 
  dplyr::mutate(lag_income_index =   dplyr::lag(income_index, n = 1, default = NA) )%>% 
  dplyr::mutate(lag_log_gni_pc =   dplyr::lag(log_gni_pc, n = 1, default = NA) )%>% 
  ungroup()

### climate sq

data<-data %>% mutate(
  TM_YY_2 = TM_YY^2,
  RR_YY_2 = RR_YY^2,
  HWd_DUR_2= HWd_DUR^2 ,
  HWd_EV_2= HWd_EV^2 ,
  HWd_LARG_2= HWd_LARG^2 ,
  CWn_DUR_2= CWn_DUR^2 ,
  CWn_EV_2= CWn_EV^2 ,
  CWn_LARG_2= CWn_LARG^2,
  TC_VHD_2= TC_VHD^2 ,
  TC_VCD_2 =  TC_VCD^2 ,
  SPEI_DUR_2= SPEI_DUR^2,
  SPEI_SEV_2= SPEI_SEV^2,
  SPI_DUR_2 = SPI_DUR^2,
  SPI_SEV_2=SPI_SEV^2,
  RRX_EXTR_2=RRX_EXTR^2,
  RRX_RR5d_2= RRX_RR5d^2, 
  CL_PET_2=CL_PET^2
)

# add vars climate, lagged
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_TM_YY =  dplyr::lag(TM_YY, n = 1, default = NA))%>% 
  dplyr::mutate(lag_RR_YY =  dplyr::lag(RR_YY, n = 1, default = NA))%>% 
  dplyr::mutate(lag_HWd_DUR =  dplyr::lag(HWd_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_HWd_EV = dplyr::lag(HWd_EV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_HWd_LARG =   dplyr::lag(HWd_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(lag_CWn_DUR = dplyr::lag(CWn_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_CWn_EV =  dplyr::lag(CWn_EV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_CWn_LARG =  dplyr::lag(CWn_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(lag_TC_VHD =   dplyr::lag( TC_VHD, n = 1, default = NA))%>% 
  dplyr::mutate(lag_TC_VCD =   dplyr::lag( TC_VCD, n = 1, default = NA))%>% 
  dplyr::mutate(lag_SPEI_DUR =  dplyr::lag( SPEI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_SPEI_SEV =  dplyr::lag( SPEI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_SPI_DUR = dplyr::lag( SPI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_SPI_SEV = dplyr::lag( SPI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_RRX_EXTR =   dplyr::lag(RRX_EXTR , n = 1, default = NA))%>% 
  dplyr::mutate(lag_RRX_RR5d =   dplyr::lag( RRX_RR5d, n = 1, default = NA))%>% 
  dplyr::mutate(lag_CL_PET =  dplyr::lag( CL_PET, n = 1, default = NA))%>% 
  ungroup()

# add diff vars climate
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(diff_TM_YY = TM_YY - dplyr::lag(TM_YY, n = 1, default = NA))%>% 
  dplyr::mutate(diff_RR_YY = RR_YY - dplyr::lag(RR_YY, n = 1, default = NA))%>% 
  dplyr::mutate(diff_HWd_DUR = HWd_DUR - dplyr::lag(HWd_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(diff_HWd_EV = HWd_EV - dplyr::lag(HWd_EV, n = 1, default = NA))%>% 
  dplyr::mutate(diff_HWd_LARG =  HWd_LARG- dplyr::lag(HWd_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(diff_CWn_DUR = CWn_DUR - dplyr::lag(CWn_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(diff_CWn_EV = CWn_EV - dplyr::lag(CWn_EV, n = 1, default = NA))%>% 
  dplyr::mutate(diff_CWn_LARG = CWn_LARG - dplyr::lag(CWn_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(diff_TC_VHD =  TC_VHD- dplyr::lag( TC_VHD, n = 1, default = NA))%>% 
  dplyr::mutate(diff_TC_VCD =  TC_VCD- dplyr::lag( TC_VCD, n = 1, default = NA))%>% 
  dplyr::mutate(diff_SPEI_DUR = SPEI_DUR - dplyr::lag( SPEI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(diff_SPEI_SEV = SPEI_SEV - dplyr::lag( SPEI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(diff_SPI_DUR = SPI_DUR - dplyr::lag( SPI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(diff_SPI_SEV = SPI_SEV - dplyr::lag( SPI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(diff_RRX_EXTR =  RRX_EXTR- dplyr::lag(RRX_EXTR , n = 1, default = NA))%>% 
  dplyr::mutate(diff_RRX_RR5d =  RRX_RR5d- dplyr::lag( RRX_RR5d, n = 1, default = NA))%>% 
  dplyr::mutate(diff_CL_PET = CL_PET - dplyr::lag( CL_PET, n = 1, default = NA))%>% 
  ungroup()

# add diff squared vars climate
data<-data %>% mutate(
  diff_TM_YY_2 = diff_TM_YY^2,
  diff_RR_YY_2 = diff_RR_YY^2,
  diff_HWd_DUR_2= diff_HWd_DUR^2 ,
  diff_HWd_EV_2= diff_HWd_EV^2 ,
  diff_HWd_LARG_2= diff_HWd_LARG^2 ,
  diff_CWn_DUR_2= diff_CWn_DUR^2 ,
  diff_CWn_EV_2= diff_CWn_EV^2 ,
  diff_CWn_LARG_2= diff_CWn_LARG^2,
  diff_TC_VHD_2= diff_TC_VHD^2 ,
  diff_TC_VCD_2 =  diff_TC_VCD^2 ,
  diff_SPEI_DUR_2= diff_SPEI_DUR^2,
  diff_SPEI_SEV_2= diff_SPEI_SEV^2,
  diff_SPI_DUR_2 = diff_SPI_DUR^2,
  diff_SPI_SEV_2= diff_SPI_SEV^2,
  diff_RRX_EXTR_2= diff_RRX_EXTR^2,
  diff_RRX_RR5d_2= diff_RRX_RR5d^2, 
  diff_CL_PET_2= diff_CL_PET^2
)

# add diff vars climate, lagged
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_diff_TM_YY = dplyr::lag(diff_TM_YY, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_RR_YY = dplyr::lag(diff_RR_YY, n = 1, default = NA))%>% 
  ungroup()

# add diff vars climate, lagged
data <- data %>% 
  arrange(gdlcode, year) %>% 
  group_by(gdlcode) %>%
  dplyr::mutate(lag_diff_TM_YY = dplyr::lag(diff_TM_YY, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_RR_YY = dplyr::lag(diff_RR_YY, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_HWd_DUR =  dplyr::lag(diff_HWd_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_HWd_EV = dplyr::lag(diff_HWd_EV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_HWd_LARG =   dplyr::lag(diff_HWd_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_CWn_DUR = dplyr::lag(diff_CWn_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_CWn_EV =  dplyr::lag(diff_CWn_EV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_CWn_LARG =  dplyr::lag(diff_CWn_LARG, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_TC_VHD =   dplyr::lag( diff_TC_VHD, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_TC_VCD =   dplyr::lag( diff_TC_VCD, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_SPEI_DUR =  dplyr::lag( diff_SPEI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_SPEI_SEV =  dplyr::lag( diff_SPEI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_SPI_DUR = dplyr::lag( diff_SPI_DUR, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_SPI_SEV = dplyr::lag( diff_SPI_SEV, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_RRX_EXTR =   dplyr::lag(diff_RRX_EXTR , n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_RRX_RR5d =   dplyr::lag( diff_RRX_RR5d, n = 1, default = NA))%>% 
  dplyr::mutate(lag_diff_CL_PET =  dplyr::lag( diff_CL_PET, n = 1, default = NA))%>% 
  ungroup()


# # country
# # add log vars
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(log_hdi =log(hdi))%>% 
#   dplyr::mutate(log_health_index =log(health_index))%>% 
#   dplyr::mutate(log_edu_index =log(edu_index))%>% 
#   dplyr::mutate(log_income_index =log(income_index))%>% 
#   dplyr::mutate(log_gdi =log(gdi))%>% 
#   ungroup()
# 
# # add diff log (growth rate) vars
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(gr_hdi = log_hdi - dplyr::lag(log_hdi, n = 1, default = NA))%>% 
#   dplyr::mutate(gr_health_index = log_health_index - dplyr::lag(log_health_index, n = 1, default = NA))%>% 
#   dplyr::mutate(gr_edu_index = log_edu_index - dplyr::lag(log_edu_index, n = 1, default = NA))%>% 
#   dplyr::mutate(gr_income_index = log_income_index - dplyr::lag(log_income_index, n = 1, default = NA))%>% 
#   dplyr::mutate(gr_gdi = log_gdi - dplyr::lag(log_gdi, n = 1, default = NA))%>% 
#   dplyr::mutate(gr_log_gni_pc = log_gni_pc - dplyr::lag(log_gni_pc, n = 1, default = NA))%>% 
#   ungroup()
# 
# # add diff log (growth rate) vars of time t+1 (lead instead of log)
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(lead_gr_hdi = dplyr::lead(log_hdi, n = 1, default = NA) - log_hdi )%>% 
#   dplyr::mutate(lead_gr_health_index = dplyr::lead(log_health_index, n = 1, default = NA) -  log_health_index )%>% 
#   dplyr::mutate(lead_gr_edu_index =  dplyr::lead(log_edu_index, n = 1, default = NA) - log_edu_index )%>% 
#   dplyr::mutate(lead_gr_income_index = dplyr::lead(log_income_index, n = 1, default = NA) - log_income_index )%>% 
#   dplyr::mutate(lead_gr_gdi =  dplyr::lead(log_gdi, n = 1, default = NA) - log_gdi )%>% 
#   dplyr::mutate(lead_gr_log_gni_pc = dplyr::lead(log_gni_pc, n = 1, default = NA) - log_gni_pc)%>% 
#   ungroup()
# 
# # add diff vars climate
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(diff_TM_YY = TM_YY - dplyr::lag(TM_YY, n = 1, default = NA))%>% 
#   dplyr::mutate(diff_RR_YY = RR_YY - dplyr::lag(RR_YY, n = 1, default = NA))%>% 
#   ungroup()
# 
# # add diff vars climate, lagged
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(lag_diff_TM_YY = dplyr::lag(diff_TM_YY, n = 1, default = NA))%>% 
#   dplyr::mutate(lag_diff_RR_YY = dplyr::lag(diff_RR_YY, n = 1, default = NA))%>% 
#   ungroup()
# 
# # add vars climate, lagged
# data_country <- data_country %>% 
#   arrange(gdlcode, year) %>% 
#   group_by(gdlcode) %>%
#   dplyr::mutate(lag_TM_YY = dplyr::lag(TM_YY, n = 1, default = NA))%>% 
#   dplyr::mutate(lag_RR_YY = dplyr::lag(RR_YY, n = 1, default = NA))%>% 
#   ungroup()


### save dataset in model output folder, to have data and model together 

write.csv(data, file.path(out_dir,"data_hdi_hazards_gdl_pop_weight_1990_2021.csv"), row.names = FALSE)
# write.csv(data_country, file.path(out_dir,"data_hdi_hazards_country_pop_weight_1990_2021.csv"), row.names = FALSE)

