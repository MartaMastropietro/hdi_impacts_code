
rm(list=ls())
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

# fix countries for aggregations 
sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")


# create hdi projections with past, future proj values 

out_dir_comp<-"output/projections/original_comp" 

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

# max min values calculated over all horizon 1990-2100
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

################################################################################


### try
o<-"gr_gnipc"
data_int_gnipc<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_intervals.feather")))
# to uncomment
data_all_gnipc<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_all.feather")))
data_int_gnipc<-inner_join(data_int_gnipc, pop_projection_gdlcode )
gc()
o<-"gr_leb"
data_int_leb<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_intervals.feather")))
# to uncomment
data_all_leb<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_all.feather")))
data_int_leb<-inner_join(data_int_leb, pop_projection_gdlcode )
gc()
o<-"gr_eys"
data_int_eys<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_intervals.feather")))
# to uncomment
data_all_eys<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_all.feather")))
data_int_eys<-inner_join(data_int_eys, pop_projection_gdlcode )
gc()
### to comment
# data_all<-data_int
# data_all<-data_all[-which(is.na(data_all$cc_proj_value_mean)), ]
# data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]

# data_all$result.1<-sample(data_all$cc_proj_value_mean)
# data_all$result.2<-sample(data_all$cc_proj_value_mean)
# data_all$result.3<-sample(data_all$cc_proj_value_mean)
#   
# data_all<-data_all%>%select(result.1, result.2, result.3)
# nboot<-3
###

data_all_gnipc$gdlcode<-data_int_gnipc$gdlcode
data_all_gnipc$iso3<-data_int_gnipc$iso3
data_all_gnipc$year<-data_int_gnipc$year
data_all_gnipc$value_interp<-data_int_gnipc$value_interp
data_all_gnipc$ssp<-data_int_gnipc$ssp
data_all_gnipc$model<-data_int_gnipc$model
data_all_gnipc$pop_gdl<-data_int_gnipc$pop_gdl
data_all_gnipc$pop_country<-data_int_gnipc$pop_country


data_all_leb$gdlcode<-data_int_leb$gdlcode
data_all_leb$iso3<-data_int_leb$iso3
data_all_leb$year<-data_int_leb$year
data_all_leb$value_interp<-data_int_leb$value_interp
data_all_leb$ssp<-data_int_leb$ssp
data_all_leb$model<-data_int_leb$model
data_all_leb$pop_gdl<-data_int_leb$pop_gdl
data_all_leb$pop_country<-data_int_leb$pop_country


data_all_eys$gdlcode<-data_int_eys$gdlcode
data_all_eys$iso3<-data_int_eys$iso3
data_all_eys$year<-data_int_eys$year
data_all_eys$value_interp<-data_int_eys$value_interp
data_all_eys$ssp<-data_int_eys$ssp
data_all_eys$model<-data_int_eys$model
data_all_eys$pop_gdl<-data_int_eys$pop_gdl
data_all_eys$pop_country<-data_int_eys$pop_country

### to comment
# data_all_gnipc<-data_all
# data_all_leb<-data_all
# data_all_eys<-data_all
###


gc()
#####

# indeces

# log values of gnipc
data_all_gnipc[,1:nboot]<-log(data_all_gnipc[,1:nboot])
data_all_gnipc$log_value_interp<-log(data_all_gnipc$value_interp)

# no cc income index
data_all_gnipc$income_index<-(data_all_gnipc$log_value_interp-min_lgnipc)/(max_lgnipc-min_lgnipc)
# cc income index
data_all_gnipc[,1:nboot]<-(data_all_gnipc[,1:nboot]-min_lgnipc)/(max_lgnipc-min_lgnipc)

gc()
# no cc lifex index
data_all_leb$lifex_index<-(data_all_leb$value_interp-min_leb)/(max_leb-min_leb)
# cc lifex index
data_all_leb[,1:nboot]<-(data_all_leb[,1:nboot]-min_leb)/(max_leb-min_leb)

gc()
# no cc edu index
data_all_eys$edu_index<-(data_all_eys$value_interp-min_eys)/(max_eys-min_eys)
# cc edu index
data_all_eys[,1:nboot]<-(data_all_eys[,1:nboot]-min_eys)/(max_eys-min_eys)


### rename
gc()
data_all_gnipc <- data_all_gnipc %>%
  rename_with(~ gsub("^result\\.", "income_index_cc.", .x), starts_with("result."))

gc()
data_all_leb <- data_all_leb %>%
  rename_with(~ gsub("^result\\.", "lifex_index_cc.", .x), starts_with("result."))

gc()
data_all_eys <- data_all_eys %>%
  rename_with(~ gsub("^result\\.", "edu_index_cc.", .x), starts_with("result."))

data_all_gnipc$value_interp<-NULL
data_all_gnipc$log_value_interp<-NULL
data_all_gnipc$cc_proj_value_med<-NULL
data_all_gnipc$cc_proj_value_mean<-NULL
data_all_gnipc$cc_proj_value_q10<-NULL
data_all_gnipc$cc_proj_value_q90<-NULL
data_all_gnipc$cc_proj_value_q05<-NULL
data_all_gnipc$cc_proj_value_q95<-NULL
data_all_gnipc$cc_proj_value_q33<-NULL
data_all_gnipc$cc_proj_value_q66<-NULL
data_all_gnipc$cc_proj_value_q17<-NULL
data_all_gnipc$cc_proj_value_q83<-NULL

data_all_leb$value_interp<-NULL
data_all_leb$cc_proj_value_med<-NULL
data_all_leb$cc_proj_value_mean<-NULL
data_all_leb$cc_proj_value_q10<-NULL
data_all_leb$cc_proj_value_q90<-NULL
data_all_leb$cc_proj_value_q05<-NULL
data_all_leb$cc_proj_value_q95<-NULL
data_all_leb$cc_proj_value_q33<-NULL
data_all_leb$cc_proj_value_q66<-NULL
data_all_leb$cc_proj_value_q17<-NULL
data_all_leb$cc_proj_value_q83<-NULL

data_all_eys$value_interp<-NULL
data_all_eys$cc_proj_value_med<-NULL
data_all_eys$cc_proj_value_mean<-NULL
data_all_eys$cc_proj_value_q10<-NULL
data_all_eys$cc_proj_value_q90<-NULL
data_all_eys$cc_proj_value_q05<-NULL
data_all_eys$cc_proj_value_q95<-NULL
data_all_eys$cc_proj_value_q33<-NULL
data_all_eys$cc_proj_value_q66<-NULL
data_all_eys$cc_proj_value_q17<-NULL
data_all_eys$cc_proj_value_q83<-NULL

colnames(data_all_gnipc)
colnames(data_all_leb)
colnames(data_all_eys)

dim(data_all_gnipc)
dim(data_all_leb)
dim(data_all_eys)

common_iso<-intersect(intersect(unique(data_all_gnipc$iso3), unique(data_all_leb$iso3)), unique(data_all_eys$iso3))
data_all_gnipc<-data_all_gnipc%>%filter(iso3 %in% common_iso)
data_all_leb<-data_all_leb%>%filter(iso3 %in% common_iso)
data_all_eys<-data_all_eys%>%filter(iso3 %in% common_iso)


### unite
# gc()
# data_all<-inner_join(data_all_gnipc, data_all_eys)
# gc()
# data_all<-inner_join(data_all, data_all_leb)


# compute_hdi_cc <- function(df) {
#   df %>%
#     mutate(
#       across(starts_with("income_index_cc."), 
#              ~ ( .x * get(sub("income", "lifex", cur_column())) * 
#                    get(sub("income", "edu", cur_column())) )^(1/3), 
#              .names = "hdi_cc.{.col}")
#     )%>%
#     rename_with(~ gsub("hdi_cc\\.income_index_cc\\.", "hdi_cc.", .x), starts_with("hdi_cc."))
#   
# }
# 
# data_all<-compute_hdi_cc(data_all)
# data_all$hdi<-(data_all$income_index*data_all$edu_index*data_all$lifex_index)^(1/3)


# # Extract common columns to keep (excluding bootstrapped index columns)
# common_cols <- intersect(names(data_all_gnipc), names(data_all_leb)) %>% 
#   intersect(names(data_all_eys)) %>% 
#   setdiff(grep("cc\\.", names(data_all_gnipc), value = TRUE))
# 
# # Compute HDI for each row while preserving memory
# data_hdi <- map2_dfr(
#   seq_len(nrow(data_all_gnipc)),  # Iterate over rows
#   seq_len(ncol(data_all_gnipc) - length(common_cols)),  # Iterate over bootstraps
#   function(i, j) {
#     # Get bootstrap column names dynamically
#     income_col <- paste0("income_index_cc.", j)
#     lifex_col <- paste0("lifex_index_cc.", j)
#     edu_col <- paste0("edu_index_cc.", j)
#     
#     # Compute HDI for the current row and bootstrap index
#     hdi_value <- (data_all_gnipc[[income_col]][i] * 
#                     data_all_leb[[lifex_col]][i] * 
#                     data_all_eys[[edu_col]][i])^(1/3)
#     
#     # Return a data frame with HDI values
#     tibble(
#       gdlcode = data_all_gnipc$gdlcode[i],
#       iso3 = data_all_gnipc$iso3[i],
#       year = data_all_gnipc$year[i],
#       ssp = data_all_gnipc$ssp[i],
#       model = data_all_gnipc$model[i],
#       !!paste0("hdi_cc.", j) := hdi_value
#     )
#   }
# )

library(data.table)

# Convert dataframes to data.tables for fast processing
data_all_gnipc<-setDT(data_all_gnipc)
data_all_leb<-setDT(data_all_leb)
data_all_eys<-setDT(data_all_eys)

# Identify bootstrap column indices (assuming they follow the "cc.X" pattern)
boot_cols <- grep("income_index_cc\\.", names(data_all_gnipc), value = TRUE)

# Create a new data.table for HDI results, copying common identifiers
data_hdi <- data_all_gnipc[, .(gdlcode, iso3, year, ssp, model)]

# Compute HDI for each bootstrap column
for (col in boot_cols) {
  # Generate corresponding column names for lifex and edu indices
  lifex_col <- sub("income", "lifex", col)
  edu_col <- sub("income", "edu", col)
  
  # Compute geometric mean for each row
  data_hdi[, (sub("income_index_cc", "hdi_cc", col)) := 
             (data_all_gnipc[[col]] * data_all_leb[[lifex_col]] * data_all_eys[[edu_col]])^(1/3)]
}

data_hdi$hdi<-(data_all_gnipc$income_index*data_all_leb$lifex_index*data_all_eys$edu_index)^(1/3)
data_hdi<-inner_join(data_hdi, pop_projection_gdlcode )

# Check the final dataset
print(head(data_hdi))


agg<-"pop_weight"
arrow::write_feather(data_hdi,file.path(out_dir_lag,
                                              paste0('hdi_',spec,'_',type,'_',  agg,"_nlags",N, 
                                                     "_boot_impacts_all" , ".feather")) )

gc()

arrow::write_feather(data_all_gnipc,file.path(out_dir_lag,
                                        paste0('income_index_',spec,'_',type,'_',  agg,"_nlags",N, 
                                               "_boot_impacts_all" , ".feather")) )

gc()

arrow::write_feather(data_all_eys,file.path(out_dir_lag,
                                        paste0('edu_index_',spec,'_',type,'_',  agg,"_nlags",N, 
                                               "_boot_impacts_all" , ".feather")) )


gc()
arrow::write_feather(data_all_leb,file.path(out_dir_lag,
                                        paste0('lifex_index_',spec,'_',type,'_',  agg,"_nlags",N, 
                                               "_boot_impacts_all" , ".feather")) )





