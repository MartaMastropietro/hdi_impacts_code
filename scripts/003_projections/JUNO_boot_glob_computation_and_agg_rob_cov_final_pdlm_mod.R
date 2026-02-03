
args = base::commandArgs(trailingOnly=TRUE)


o<-args[2]
#o<-2
# kind<-args[3] # values, delta, perc_delta


if(o==1){
  out_var<-"gr_eys"
}else if(o==2){
  out_var<-"gr_leb"
}else if(o==3){
  out_var<-"gr_gnipc"
}



library(readr)
#library(data.table)
library(reshape2)
library(dplyr)
library(purrr)

library(stringr)

specifications<-c("mean_mod") #   ,"mean_mod_spec")
types<-c("all_extr_tp") #  , "all_vars_adap")#, "all_vars_controls", "all_vars_autoreg")
#vars_in_proj<-c( "all_extr_tp", "extr_only")
spec_type<-"dlm"
effect<-"growth_eff"

#lags number 
#N<-8
N<-"mix"

spec<-specifications[1]
type<-"all_vars_pdlm"
vars<-"all_vars_pdlm"

agg<-"pop_weight"
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/projections/original_comp/pop_weight_gdlcode/test_proj_funcs/boot_interv"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir_pop<-"output/projections/original_comp/pop_weight_gdlcode"

out_dir_lag<-file.path(out_dir_pop, "lag_models")
if(!dir.exists(out_dir_lag)){dir.create(out_dir_lag)}


nboot<-1000


pop_projection_gdlcode <- read_csv("output/projections/original_comp/pop_projection_gdlcode_sum_2015_2100.csv")
colnames(pop_projection_gdlcode)[which(colnames(pop_projection_gdlcode)=="value")]<-"pop_gdl"
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")
pop_projection_gdlcode<-inner_join(pop_projection_gdlcode, gdlcodes_iso)
pop_projection_gdlcode<-pop_projection_gdlcode%>%
  group_by(iso3, year, ssp)%>%
  mutate(pop_country=sum(pop_gdl))
pop_projection_gdlcode<-unique(pop_projection_gdlcode)




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


###
sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")



data<-arrow::read_feather(file.path(out_dir_lag,
                                    paste0(out_var,'_',spec,'_',type,'_',  agg,"_nlags",N, 
                                           "_boot_impacts_rob_cov_all"  , ".feather")) )
data_int<-arrow::read_feather(file.path(out_dir_lag,
                                        paste0(out_var,'_',spec,'_',type,'_',  agg,"_nlags",N, 
                                               "_boot_impacts_rob_cov_intervals"  , ".feather")) )


data$gdlcode<-data_int$gdlcode
data$iso3<-data_int$iso3
data$year<-data_int$year
data$value_interp<-data_int$value_interp
data$ssp<-data_int$ssp
data$model<-data_int$model


data<-inner_join(data,pop_projection_gdlcode)

data <- data %>%
  mutate(across(starts_with("result"), 
                .fns = list(
                  delta = ~ . - value_interp, 
                  perc_delta = ~ (. - value_interp) / value_interp
                ), 
                .names = "{.fn}.{.col}")) 
data <- data %>%
  rename_with(~ str_remove(.x, "\\.result"), starts_with("delta.")) %>%
  rename_with(~ str_remove(.x, "\\.result"), starts_with("perc_delta."))


### agg

# # iso3 
# data_iso<-data %>% filter(iso3 %in% sel_countries)%>%
#   group_by(iso3, ssp, model, year) %>%
#   summarise(across(-gdlcode , ~weighted.mean(. ,w=pop_gdl, na.rm = TRUE)))
# 
# 
# write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('c_sel_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N, "_rob_cov_boot_all.csv")), row.names = FALSE)
# 
# gc()

### glob

data_glob<-data %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(. ,w=pop_gdl, na.rm=TRUE)))

write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('g_agg_',out_var,"_",type,'_',spec, "_pop_weight_nlags_",N,"_rob_cov_boot_all.csv")), row.names = FALSE)

####################
