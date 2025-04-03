### redo global, by country aggregations, but taking only significant values into account 


args = base::commandArgs(trailingOnly=TRUE)


o<-args[2]
#o<-2
# kind<-args[3] # values, delta, perc_delta


if(o==1){
  out_var<-"edu_index"
}else if(o==2){
  out_var<-"lifex_index"
}else if(o==3){
  out_var<-"income_index"
}else if(o==4){
  out_var="hdi"}

library(readr)
#library(data.table)
library(reshape2)
library(dplyr)
library(purrr)

library(stringr)

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

agg<-"pop_weight"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}



pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
pop_projection_gdlcode<-pop_projection_gdlcode%>%
  group_by(iso3, year, ssp)%>%
  mutate(pop_country=sum(pop_gdl))
pop_projection_gdlcode<-unique(pop_projection_gdlcode)


nboot<-1000


###
sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")



quant05 <- function(x){
  quantile(x, probs=0.05, na.rm=TRUE)
}
quant95 <- function(x){
  quantile(x, probs=0.95, na.rm=TRUE)
}
quant10 <- function(x){
  quantile(x, probs=0.10, na.rm=TRUE)
}
quant90 <- function(x){
  quantile(x, probs=0.90, na.rm=TRUE)
}



data<-arrow::read_feather(file.path(out_dir_lag,
                                    paste0(out_var ,'_values_deltas_',spec,'_',type,'_',  agg,"_nlags",N, 
                                           "_boot_impacts_all" , ".feather")) )

data_int<-arrow::read_feather(file.path(out_dir_lag,
                                                  paste0(out_var ,'_values_deltas_',
                                                         spec,'_',type,'_',  agg,"_nlags",N, 
                                                         "_boot_intervals" , ".feather")) )

### define significant values for deltas, perc deltas. For each, compute a global weighted mean 

data$delta_q90<-data_int$delta_q90
data$delta_q10<-data_int$delta_q10
data$perc_delta_q90<-data_int$perc_delta_q90
data$perc_delta_q10<-data_int$perc_delta_q10

data<-inner_join(data,pop_projection_gdlcode)

### delta sign, agg only them 

data <- data %>%
  mutate(same_sign_delta_90 = sign(delta_q90) == sign(delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_delta_90 = as.integer(mean(same_sign_delta_90) >= 2/3), .groups = "drop") %>%
  right_join(data, by = c("gdlcode", "year", "ssp"))  

#glob

data_glob<-data %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(ifelse(sign_delta_90, . , NA) ,w=pop_gdl, na.rm=TRUE)))

write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('g_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")), row.names = FALSE)

rm(data_glob)
gc()


# iso3 
data_iso<-data %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(ifelse(sign_delta_90, . , NA)  ,w=pop_gdl, na.rm = TRUE)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('c_sel_d_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")), row.names = FALSE)
rm(data_iso)
gc()


### perc delta sign, agg only them 

data <- data %>%
  mutate(same_sign_perc_delta_90 = sign(perc_delta_q90) == sign(perc_delta_q10)) %>%
  group_by(gdlcode, year, ssp) %>%
  summarise(sign_perc_delta_90 = as.integer(mean(same_sign_perc_delta_90) >= 2/3), .groups = "drop") %>%
  right_join(data, by = c("gdlcode", "year", "ssp"))  


#glob

data_glob<-data %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(ifelse(sign_perc_delta_90, . , NA) ,w=pop_gdl, na.rm=TRUE)))

write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('g_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")), row.names = FALSE)

rm(data_glob)
gc()



# iso3 
data_iso<-data %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(ifelse(sign_perc_delta_90, . , NA)  ,w=pop_gdl, na.rm = TRUE)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('c_sel_pd_s_agg_',out_var,"_",type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_all.csv")), row.names = FALSE)
rm(data_iso)
gc()
