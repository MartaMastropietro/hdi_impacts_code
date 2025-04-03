
rm(list=ls())
library(readr)
#library(data.table)
library(reshape2)
library(dplyr)

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

o<-"gr_leb"
data_int_leb<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_intervals.feather")))
# to uncomment
data_all_leb<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_all.feather")))
data_int_leb<-inner_join(data_int_leb, pop_projection_gdlcode )

o<-"gr_eys"
data_int_eys<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_intervals.feather")))
# to uncomment
data_all_eys<-arrow::read_feather(file.path(out_dir_lag,paste0(o,'_',spec,'_',vars, "_pop_weight_nlags",N,"_boot_impacts_all.feather")))
data_int_eys<-inner_join(data_int_eys, pop_projection_gdlcode )

### to comment
# data_all<-data_int
# data_all<-data_all[-which(is.na(data_all$cc_proj_value_mean)), ]
# data_int<-data_int[-which(is.na(data_int$cc_proj_value_mean)), ]

# data_all$result.1<-sample(data_all$cc_proj_value_mean)
# data_all$result.2<-sample(data_all$cc_proj_value_mean)
# data_all$result.3<-sample(data_all$cc_proj_value_mean)
# data_all$result.4<-sample(data_all$cc_proj_value_mean)
# data_all$result.5<-sample(data_all$cc_proj_value_mean)
#   
# data_all<-data_all%>%select(result.1, result.2, result.3,result.4,result.5)
# nboot<-5
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
# data_all_gni<-data_all
# data_all_leb<-data_all
# data_all_eys<-data_all
###



#####



# log values of gnipc
data_all_gnipc[,1:nboot]<-log(data_all_gnipc[,1:nboot])
data_all_gnipc$log_value_interp<-log(data_all_gnipc$value_interp)

# no cc income index
data_all_gnipc$income_index<-(data_all_gnipc$log_value_interp-min_lgnipc)/(max_lgnipc-min_lgnipc)
# cc income index
data_all_gnipc[,1:nboot]<-(data_all_gnipc[,1:nboot]-min_lgnipc)/(max_lgnipc-min_lgnipc)


# no cc lifex index
data_all_leb$lifex_index<-(data_all_leb$value_interp-min_leb)/(max_leb-min_leb)
# cc lifex index
data_all_leb[,1:nboot]<-(data_all_leb[,1:nboot]-min_leb)/(max_leb-min_leb)


# no cc edu index
data_all_eys$edu_index<-(data_all_eys$value_interp-min_eys)/(max_eys-min_eys)
# cc edu index
data_all_eys[,1:nboot]<-(data_all_eys[,1:nboot]-min_eys)/(max_eys-min_eys)


### melt
# income_index
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
data_all_gnipc <- reshape2::melt(data_all_gnipc, id.vars = c("year", "iso3", "ssp", "model", "gdlcode",
                                                      "pop_gdl" ,"pop_country" , "income_index"), variable.name = "boot")
colnames(data_all_gnipc)[colnames(data_all_gnipc)=="value"]<-"income_index_cc"

#lifex_index
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
data_all_leb <- reshape2::melt(data_all_leb, id.vars = c("year", "iso3", "ssp", "model", "gdlcode",
                                                      "pop_gdl" ,"pop_country" , "lifex_index"), variable.name = "boot")
colnames(data_all_leb)[colnames(data_all_leb)=="value"]<-"lifex_index_cc"

#edu_index
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
data_all_eys <- reshape2::melt(data_all_eys, id.vars = c("year", "iso3", "ssp", "model", "gdlcode",
                                                      "pop_gdl" ,"pop_country" , "edu_index"), variable.name = "boot")
colnames(data_all_eys)[colnames(data_all_eys)=="value"]<-"edu_index_cc"

# check
# unique(data_all_gnipc$boot)
# unique(data_all_leb$boot)
# unique(data_all_eys$boot)


### combine

gc()
data_all<-inner_join(data_all_gni, data_all_leb)
gc()
data_all<-inner_join(data_all, data_all_eys)
gc()


data_all$hdi<-(data_all$income_index*data_all$lifex_index*data_all$edu_index)^(1/3)
data_all$hdi_cc<-(data_all$income_index_cc*data_all$lifex_index_cc*data_all$edu_index_cc)^(1/3)
gc()
# deltas, perc deltas
data_all$delta_income_index<-data_all$income_index_cc-data_all$income_index
data_all$delta_perc_income_index<-(data_all$income_index_cc-data_all$income_index)/data_all$income_index
gc()
data_all$delta_lifex_index<-data_all$lifex_index_cc-data_all$lifex_index
data_all$delta_perc_lifex_index<-(data_all$lifex_index_cc-data_all$lifex_index)/data_all$lifex_index
gc()
data_all$delta_edu_index<-data_all$edu_index_cc-data_all$edu_index
data_all$delta_perc_edu_index<-(data_all$edu_index_cc-data_all$edu_index)/data_all$edu_index
gc()
data_all$delta_hdi<-data_all$hdi_cc-data_all$hdi
data_all$delta_perc_hdi<-(data_all$hdi_cc-data_all$hdi)/data_all$hdi
gc()


arrow::write_feather(data_all,file.path(out_dir_lag,
                                        paste0('all_idx_',spec,'_',type,'_',  agg, "_nlags",N,
                                               "_boot_impacts_all" , ".feather")) )

################################################################################

#  data_all<-arrow::read_feather(file.path(out_dir_lag,paste0('all_idx_',spec,'_',type,'_',  agg, "_nlags",N,"_boot_impacts_all" , ".feather")))

data_all_int<-data_all%>%
  group_by(year, ssp, model, gdlcode, iso3)%>%
  transmute(
    #hdi
    hdi=mean(hdi, na.rm = TRUE), 
    #hdi cc
    mean_hdi_cc=mean(hdi_cc, na.rm = TRUE), 
    q10_hdi_cc=quantile(hdi_cc, probs=0.1, na.rm = TRUE),
    q05_hdi_cc=quantile(hdi_cc, probs=0.05, na.rm = TRUE),
    q90_hdi_cc=quantile(hdi_cc, probs=0.9, na.rm = TRUE),
    q95_hdi_cc=quantile(hdi_cc, probs=0.95, na.rm = TRUE),
    
    #income_index
    income_index=mean(income_index, na.rm = TRUE), 
    #income_index cc
    mean_income_index_cc=mean(income_index_cc, na.rm = TRUE), 
    q10_income_index_cc=quantile(income_index_cc, probs=0.1, na.rm = TRUE),
    q05_income_index_cc=quantile(income_index_cc, probs=0.05, na.rm = TRUE),
    q90_income_index_cc=quantile(income_index_cc, probs=0.9, na.rm = TRUE),
    q95_income_index_cc=quantile(income_index_cc, probs=0.95, na.rm = TRUE),
    
    #lifex_index
    lifex_index=mean(lifex_index, na.rm = TRUE), 
    #lifex_index cc
    mean_lifex_index_cc=mean(lifex_index_cc, na.rm = TRUE), 
    q10_lifex_index_cc=quantile(lifex_index_cc, probs=0.1, na.rm = TRUE),
    q05_lifex_index_cc=quantile(lifex_index_cc, probs=0.05, na.rm = TRUE),
    q90_lifex_index_cc=quantile(lifex_index_cc, probs=0.9, na.rm = TRUE),
    q95_lifex_index_cc=quantile(lifex_index_cc, probs=0.95, na.rm = TRUE),
    
    #edu_index
    edu_index=mean(edu_index, na.rm = TRUE), 
    #edu_index cc
    mean_edu_index_cc=mean(edu_index_cc, na.rm = TRUE), 
    q10_edu_index_cc=quantile(edu_index_cc, probs=0.1, na.rm = TRUE),
    q05_edu_index_cc=quantile(edu_index_cc, probs=0.05, na.rm = TRUE),
    q90_edu_index_cc=quantile(edu_index_cc, probs=0.9, na.rm = TRUE),
    q95_edu_index_cc=quantile(edu_index_cc, probs=0.95, na.rm = TRUE),
    
    # deltas
    
    # hdi
    mean_delta_hdi=mean(delta_hdi, na.rm = TRUE), 
    mean_delta_perc_hdi=mean(delta_perc_hdi, na.rm = TRUE), 
    q10_delta_hdi=quantile(delta_hdi, probs=0.1, na.rm = TRUE), 
    q10_delta_perc_hdi=quantile(delta_perc_hdi, probs=0.1, na.rm = TRUE), 
    q05_delta_hdi=quantile(delta_hdi, probs=0.05, na.rm = TRUE), 
    q05_delta_perc_hdi=quantile(delta_perc_hdi, probs=0.05, na.rm = TRUE), 
    q90_delta_hdi=quantile(delta_hdi, probs=0.9, na.rm = TRUE), 
    q90_delta_perc_hdi=quantile(delta_perc_hdi, probs=0.9, na.rm = TRUE), 
    q95_delta_hdi=quantile(delta_hdi, probs=0.95, na.rm = TRUE), 
    q95_delta_perc_hdi=quantile(delta_perc_hdi, probs=0.95, na.rm = TRUE), 
    
    
    # income_index
    mean_delta_income_index=mean(delta_income_index, na.rm = TRUE), 
    mean_delta_perc_income_index=mean(delta_perc_income_index, na.rm = TRUE), 
    q10_delta_income_index=quantile(delta_income_index, probs=0.1, na.rm = TRUE), 
    q10_delta_perc_income_index=quantile(delta_perc_income_index, probs=0.1, na.rm = TRUE), 
    q05_delta_income_index=quantile(delta_income_index, probs=0.05, na.rm = TRUE), 
    q05_delta_perc_income_index=quantile(delta_perc_income_index, probs=0.05, na.rm = TRUE), 
    q90_delta_income_index=quantile(delta_income_index, probs=0.9, na.rm = TRUE), 
    q90_delta_perc_income_index=quantile(delta_perc_income_index, probs=0.9, na.rm = TRUE), 
    q95_delta_income_index=quantile(delta_income_index, probs=0.95, na.rm = TRUE), 
    q95_delta_perc_income_index=quantile(delta_perc_income_index, probs=0.95, na.rm = TRUE), 
    
    
    # lifex_index
    mean_delta_lifex_index=mean(delta_lifex_index, na.rm = TRUE), 
    mean_delta_perc_lifex_index=mean(delta_perc_lifex_index, na.rm = TRUE), 
    q10_delta_lifex_index=quantile(delta_lifex_index, probs=0.1, na.rm = TRUE), 
    q10_delta_perc_lifex_index=quantile(delta_perc_lifex_index, probs=0.1, na.rm = TRUE), 
    q05_delta_lifex_index=quantile(delta_lifex_index, probs=0.05, na.rm = TRUE), 
    q05_delta_perc_lifex_index=quantile(delta_perc_lifex_index, probs=0.05, na.rm = TRUE), 
    q90_delta_lifex_index=quantile(delta_lifex_index, probs=0.9, na.rm = TRUE), 
    q90_delta_perc_lifex_index=quantile(delta_perc_lifex_index, probs=0.9, na.rm = TRUE), 
    q95_delta_lifex_index=quantile(delta_lifex_index, probs=0.95, na.rm = TRUE), 
    q95_delta_perc_lifex_index=quantile(delta_perc_lifex_index, probs=0.95, na.rm = TRUE), 
    
    
    # edu_index
    mean_delta_edu_index=mean(delta_edu_index, na.rm = TRUE), 
    mean_delta_perc_edu_index=mean(delta_perc_edu_index, na.rm = TRUE), 
    q10_delta_edu_index=quantile(delta_edu_index, probs=0.1, na.rm = TRUE), 
    q10_delta_perc_edu_index=quantile(delta_perc_edu_index, probs=0.1, na.rm = TRUE), 
    q05_delta_edu_index=quantile(delta_edu_index, probs=0.05, na.rm = TRUE), 
    q05_delta_perc_edu_index=quantile(delta_perc_edu_index, probs=0.05, na.rm = TRUE), 
    q90_delta_edu_index=quantile(delta_edu_index, probs=0.9, na.rm = TRUE), 
    q90_delta_perc_edu_index=quantile(delta_perc_edu_index, probs=0.9, na.rm = TRUE), 
    q95_delta_edu_index=quantile(delta_edu_index, probs=0.95, na.rm = TRUE), 
    q95_delta_perc_edu_index=quantile(delta_perc_edu_index, probs=0.95, na.rm = TRUE), 
    
  )%>%distinct()

# save 
write.csv(x=data_all_int, file=file.path(out_dir,paste0('all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_intervals.csv")), row.names = FALSE)


################################################################################


### aggregate glob
#data_all<-data_all[-which(data_all$boot %in% c("result.1", "result.2", "result.3")),]

data_all_glob<-data_all%>%
  group_by(year, ssp, model, boot)%>%
  transmute(global_w_hdi=weighted.mean(hdi, pop_gdl, na.rm = TRUE),
            global_w_hdi_cc=weighted.mean(hdi_cc, pop_gdl, na.rm = TRUE),
            global_w_income_index=weighted.mean(income_index, pop_gdl, na.rm = TRUE),
            global_w_income_index_cc=weighted.mean(income_index_cc, pop_gdl, na.rm = TRUE),
            global_w_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm = TRUE),
            global_w_lifex_index_cc=weighted.mean(lifex_index_cc, pop_gdl, na.rm = TRUE),
            global_w_edu_index=weighted.mean(edu_index, pop_gdl, na.rm = TRUE),
            global_w_edu_index_cc=weighted.mean(edu_index_cc, pop_gdl, na.rm = TRUE),
            # deltas
            global_w_delta_hdi=weighted.mean(delta_hdi, pop_gdl, na.rm = TRUE),
            global_w_delta_perc_hdi=weighted.mean(delta_perc_hdi, pop_gdl, na.rm = TRUE),
            global_w_delta_income_index=weighted.mean(delta_income_index, pop_gdl, na.rm = TRUE),
            global_w_delta_perc_income_index=weighted.mean(delta_perc_income_index, pop_gdl, na.rm = TRUE),
            global_w_delta_lifex_index=weighted.mean(delta_lifex_index, pop_gdl, na.rm = TRUE),
            global_w_delta_perc_lifex_index=weighted.mean(delta_perc_lifex_index, pop_gdl, na.rm = TRUE),
            global_w_delta_edu_index=weighted.mean(delta_edu_index, pop_gdl, na.rm = TRUE),
            global_w_delta_perc_edu_index=weighted.mean(delta_perc_edu_index, pop_gdl, na.rm = TRUE)
  )%>%distinct()
gc()

data_all_glob_int<-data_all_glob%>%
  group_by(year, ssp, model)%>%
  transmute(
    #hdi
    global_w_hdi=mean(global_w_hdi, na.rm = TRUE), 
    #hdi cc
    mean_global_w_hdi_cc=mean(global_w_hdi_cc, na.rm = TRUE), 
    q10_global_w_hdi_cc=quantile(global_w_hdi_cc, probs=0.1, na.rm = TRUE),
    q05_global_w_hdi_cc=quantile(global_w_hdi_cc, probs=0.05, na.rm = TRUE),
    q90_global_w_hdi_cc=quantile(global_w_hdi_cc, probs=0.9, na.rm = TRUE),
    q95_global_w_hdi_cc=quantile(global_w_hdi_cc, probs=0.95, na.rm = TRUE),
    
    #income_index
    global_w_income_index=mean(global_w_income_index, na.rm = TRUE), 
    #income_index cc
    mean_global_w_income_index_cc=mean(global_w_income_index_cc, na.rm = TRUE), 
    q10_global_w_income_index_cc=quantile(global_w_income_index_cc, probs=0.1, na.rm = TRUE),
    q05_global_w_income_index_cc=quantile(global_w_income_index_cc, probs=0.05, na.rm = TRUE),
    q90_global_w_income_index_cc=quantile(global_w_income_index_cc, probs=0.9, na.rm = TRUE),
    q95_global_w_income_index_cc=quantile(global_w_income_index_cc, probs=0.95, na.rm = TRUE),
    
    #lifex_index
    global_w_lifex_index=mean(global_w_lifex_index, na.rm = TRUE), 
    #lifex_index cc
    mean_global_w_lifex_index_cc=mean(global_w_lifex_index_cc, na.rm = TRUE), 
    q10_global_w_lifex_index_cc=quantile(global_w_lifex_index_cc, probs=0.1, na.rm = TRUE),
    q05_global_w_lifex_index_cc=quantile(global_w_lifex_index_cc, probs=0.05, na.rm = TRUE),
    q90_global_w_lifex_index_cc=quantile(global_w_lifex_index_cc, probs=0.9, na.rm = TRUE),
    q95_global_w_lifex_index_cc=quantile(global_w_lifex_index_cc, probs=0.95, na.rm = TRUE),
    
    #edu_index
    global_w_edu_index=mean(global_w_edu_index, na.rm = TRUE), 
    #edu_index cc
    mean_global_w_edu_index_cc=mean(global_w_edu_index_cc, na.rm = TRUE), 
    q10_global_w_edu_index_cc=quantile(global_w_edu_index_cc, probs=0.1, na.rm = TRUE),
    q05_global_w_edu_index_cc=quantile(global_w_edu_index_cc, probs=0.05, na.rm = TRUE),
    q90_global_w_edu_index_cc=quantile(global_w_edu_index_cc, probs=0.9, na.rm = TRUE),
    q95_global_w_edu_index_cc=quantile(global_w_edu_index_cc, probs=0.95, na.rm = TRUE),
    
    # deltas
    
    # hdi
    mean_global_w_delta_hdi=mean(global_w_delta_hdi, na.rm = TRUE), 
    mean_global_w_delta_perc_hdi=mean(global_w_delta_perc_hdi, na.rm = TRUE), 
    q10_global_w_delta_hdi=quantile(global_w_delta_hdi, probs=0.1, na.rm = TRUE), 
    q10_global_w_delta_perc_hdi=quantile(global_w_delta_perc_hdi, probs=0.1, na.rm = TRUE), 
    q05_global_w_delta_hdi=quantile(global_w_delta_hdi, probs=0.05, na.rm = TRUE), 
    q05_global_w_delta_perc_hdi=quantile(global_w_delta_perc_hdi, probs=0.05, na.rm = TRUE), 
    q90_global_w_delta_hdi=quantile(global_w_delta_hdi, probs=0.9, na.rm = TRUE), 
    q90_global_w_delta_perc_hdi=quantile(global_w_delta_perc_hdi, probs=0.9, na.rm = TRUE), 
    q95_global_w_delta_hdi=quantile(global_w_delta_hdi, probs=0.95, na.rm = TRUE), 
    q95_global_w_delta_perc_hdi=quantile(global_w_delta_perc_hdi, probs=0.95, na.rm = TRUE), 
    
    
    # income_index
    mean_global_w_delta_income_index=mean(global_w_delta_income_index, na.rm = TRUE), 
    mean_global_w_delta_perc_income_index=mean(global_w_delta_perc_income_index, na.rm = TRUE), 
    q10_global_w_delta_income_index=quantile(global_w_delta_income_index, probs=0.1, na.rm = TRUE), 
    q10_global_w_delta_perc_income_index=quantile(global_w_delta_perc_income_index, probs=0.1, na.rm = TRUE), 
    q05_global_w_delta_income_index=quantile(global_w_delta_income_index, probs=0.05, na.rm = TRUE), 
    q05_global_w_delta_perc_income_index=quantile(global_w_delta_perc_income_index, probs=0.05, na.rm = TRUE), 
    q90_global_w_delta_income_index=quantile(global_w_delta_income_index, probs=0.9, na.rm = TRUE), 
    q90_global_w_delta_perc_income_index=quantile(global_w_delta_perc_income_index, probs=0.9, na.rm = TRUE), 
    q95_global_w_delta_income_index=quantile(global_w_delta_income_index, probs=0.95, na.rm = TRUE), 
    q95_global_w_delta_perc_income_index=quantile(global_w_delta_perc_income_index, probs=0.95, na.rm = TRUE), 
    
    
    # lifex_index
    mean_global_w_delta_lifex_index=mean(global_w_delta_lifex_index, na.rm = TRUE), 
    mean_global_w_delta_perc_lifex_index=mean(global_w_delta_perc_lifex_index, na.rm = TRUE), 
    q10_global_w_delta_lifex_index=quantile(global_w_delta_lifex_index, probs=0.1, na.rm = TRUE), 
    q10_global_w_delta_perc_lifex_index=quantile(global_w_delta_perc_lifex_index, probs=0.1, na.rm = TRUE), 
    q05_global_w_delta_lifex_index=quantile(global_w_delta_lifex_index, probs=0.05, na.rm = TRUE), 
    q05_global_w_delta_perc_lifex_index=quantile(global_w_delta_perc_lifex_index, probs=0.05, na.rm = TRUE), 
    q90_global_w_delta_lifex_index=quantile(global_w_delta_lifex_index, probs=0.9, na.rm = TRUE), 
    q90_global_w_delta_perc_lifex_index=quantile(global_w_delta_perc_lifex_index, probs=0.9, na.rm = TRUE), 
    q95_global_w_delta_lifex_index=quantile(global_w_delta_lifex_index, probs=0.95, na.rm = TRUE), 
    q95_global_w_delta_perc_lifex_index=quantile(global_w_delta_perc_lifex_index, probs=0.95, na.rm = TRUE), 
    
    
    # edu_index
    mean_global_w_delta_edu_index=mean(global_w_delta_edu_index, na.rm = TRUE), 
    mean_global_w_delta_perc_edu_index=mean(global_w_delta_perc_edu_index, na.rm = TRUE), 
    q10_global_w_delta_edu_index=quantile(global_w_delta_edu_index, probs=0.1, na.rm = TRUE), 
    q10_global_w_delta_perc_edu_index=quantile(global_w_delta_perc_edu_index, probs=0.1, na.rm = TRUE), 
    q05_global_w_delta_edu_index=quantile(global_w_delta_edu_index, probs=0.05, na.rm = TRUE), 
    q05_global_w_delta_perc_edu_index=quantile(global_w_delta_perc_edu_index, probs=0.05, na.rm = TRUE), 
    q90_global_w_delta_edu_index=quantile(global_w_delta_edu_index, probs=0.9, na.rm = TRUE), 
    q90_global_w_delta_perc_edu_index=quantile(global_w_delta_perc_edu_index, probs=0.9, na.rm = TRUE), 
    q95_global_w_delta_edu_index=quantile(global_w_delta_edu_index, probs=0.95, na.rm = TRUE), 
    q95_global_w_delta_perc_edu_index=quantile(global_w_delta_perc_edu_index, probs=0.95, na.rm = TRUE), 
    
  )%>%distinct()

# save 
write.csv(x=data_all_glob, file=file.path(out_dir,paste0('glob_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
write.csv(x=data_all_glob_int, file=file.path(out_dir,paste0('glob_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_intervals.csv")), row.names = FALSE)



### aggregate country

gc()
data_all_country<-data_all%>%filter(iso3 %in% sel_countries)%>%
  group_by(year, ssp, model, boot,iso3)%>%
  transmute(country_w_hdi=weighted.mean(hdi, pop_gdl, na.rm = TRUE),
            country_w_hdi_cc=weighted.mean(hdi_cc, pop_gdl, na.rm = TRUE),
            country_w_income_index=weighted.mean(income_index, pop_gdl, na.rm = TRUE),
            country_w_income_index_cc=weighted.mean(income_index_cc, pop_gdl, na.rm = TRUE),
            country_w_lifex_index=weighted.mean(lifex_index, pop_gdl, na.rm = TRUE),
            country_w_lifex_index_cc=weighted.mean(lifex_index_cc, pop_gdl, na.rm = TRUE),
            country_w_edu_index=weighted.mean(edu_index, pop_gdl, na.rm = TRUE),
            country_w_edu_index_cc=weighted.mean(edu_index_cc, pop_gdl, na.rm = TRUE),
            # deltas
            country_w_delta_hdi=weighted.mean(delta_hdi, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_hdi=weighted.mean(delta_perc_hdi, pop_gdl, na.rm = TRUE),
            country_w_delta_income_index=weighted.mean(delta_income_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_income_index=weighted.mean(delta_perc_income_index, pop_gdl, na.rm = TRUE),
            country_w_delta_lifex_index=weighted.mean(delta_lifex_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_lifex_index=weighted.mean(delta_perc_lifex_index, pop_gdl, na.rm = TRUE),
            country_w_delta_edu_index=weighted.mean(delta_edu_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_edu_index=weighted.mean(delta_perc_edu_index, pop_gdl, na.rm = TRUE)
  )%>%distinct()

gc()
data_all_country_int<-data_all_country%>%
  group_by(year, ssp, model,iso3)%>%
  transmute(
    #hdi
    country_w_hdi=mean(country_w_hdi, na.rm = TRUE), 
    #hdi cc
    mean_country_w_hdi_cc=mean(country_w_hdi_cc, na.rm = TRUE), 
    q10_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.1, na.rm = TRUE),
    q05_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.05, na.rm = TRUE),
    q90_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.9, na.rm = TRUE),
    q95_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.95, na.rm = TRUE),
    
    #income_index
    country_w_income_index=mean(country_w_income_index, na.rm = TRUE), 
    #income_index cc
    mean_country_w_income_index_cc=mean(country_w_income_index_cc, na.rm = TRUE), 
    q10_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.1, na.rm = TRUE),
    q05_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.05, na.rm = TRUE),
    q90_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.9, na.rm = TRUE),
    q95_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.95, na.rm = TRUE),
    
    #lifex_index
    country_w_lifex_index=mean(country_w_lifex_index, na.rm = TRUE), 
    #lifex_index cc
    mean_country_w_lifex_index_cc=mean(country_w_lifex_index_cc, na.rm = TRUE), 
    q10_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.1, na.rm = TRUE),
    q05_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.05, na.rm = TRUE),
    q90_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.9, na.rm = TRUE),
    q95_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.95, na.rm = TRUE),
    
    #edu_index
    country_w_edu_index=mean(country_w_edu_index, na.rm = TRUE), 
    #edu_index cc
    mean_country_w_edu_index_cc=mean(country_w_edu_index_cc, na.rm = TRUE), 
    q10_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.1, na.rm = TRUE),
    q05_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.05, na.rm = TRUE),
    q90_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.9, na.rm = TRUE),
    q95_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.95, na.rm = TRUE),
    
    # deltas
    
    # hdi
    mean_country_w_delta_hdi=mean(country_w_delta_hdi, na.rm = TRUE), 
    mean_country_w_delta_perc_hdi=mean(country_w_delta_perc_hdi, na.rm = TRUE), 
    q10_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.1, na.rm = TRUE), 
    q10_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.1, na.rm = TRUE), 
    q05_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.05, na.rm = TRUE), 
    q05_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.05, na.rm = TRUE), 
    q90_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.9, na.rm = TRUE), 
    q90_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.9, na.rm = TRUE), 
    q95_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.95, na.rm = TRUE), 
    q95_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.95, na.rm = TRUE), 
    
    
    # income_index
    mean_country_w_delta_income_index=mean(country_w_delta_income_index, na.rm = TRUE), 
    mean_country_w_delta_perc_income_index=mean(country_w_delta_perc_income_index, na.rm = TRUE), 
    q10_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.1, na.rm = TRUE), 
    q10_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.1, na.rm = TRUE), 
    q05_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.05, na.rm = TRUE), 
    q05_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.05, na.rm = TRUE), 
    q90_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.9, na.rm = TRUE), 
    q90_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.9, na.rm = TRUE), 
    q95_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.95, na.rm = TRUE), 
    q95_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.95, na.rm = TRUE), 
    
    
    # lifex_index
    mean_country_w_delta_lifex_index=mean(country_w_delta_lifex_index, na.rm = TRUE), 
    mean_country_w_delta_perc_lifex_index=mean(country_w_delta_perc_lifex_index, na.rm = TRUE), 
    q10_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.1, na.rm = TRUE), 
    q10_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.1, na.rm = TRUE), 
    q05_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.05, na.rm = TRUE), 
    q05_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.05, na.rm = TRUE), 
    q90_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.9, na.rm = TRUE), 
    q90_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.9, na.rm = TRUE), 
    q95_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.95, na.rm = TRUE), 
    q95_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.95, na.rm = TRUE), 
    
    
    # edu_index
    mean_country_w_delta_edu_index=mean(country_w_delta_edu_index, na.rm = TRUE), 
    mean_country_w_delta_perc_edu_index=mean(country_w_delta_perc_edu_index, na.rm = TRUE), 
    q10_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.1, na.rm = TRUE), 
    q10_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.1, na.rm = TRUE), 
    q05_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.05, na.rm = TRUE), 
    q05_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.05, na.rm = TRUE), 
    q90_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.9, na.rm = TRUE), 
    q90_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.9, na.rm = TRUE), 
    q95_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.95, na.rm = TRUE), 
    q95_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.95, na.rm = TRUE), 
    
  )%>%distinct()

# save 
write.csv(x=data_all_country, file=file.path(out_dir,paste0('country_sel_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
write.csv(x=data_all_country_int, file=file.path(out_dir,paste0('country_sel_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_intervals.csv")), row.names = FALSE)


################################################################################

### median across models

data_all_med<-data_all%>%
  group_by( gdlcode, iso3, ssp, year , boot)%>%
  transmute(med_hdi=median(hdi, na.rm=TRUE),
            med_hdi_cc=median(hdi_cc, na.rm=TRUE),
            med_income_index=median(income_index, na.rm=TRUE),
            med_income_index_cc=median(income_index_cc, na.rm=TRUE),
            med_lifex_index=median(lifex_index, na.rm=TRUE),
            med_lifex_index_cc=median(lifex_index_cc, na.rm=TRUE),
            med_edu_index=median(edu_index, na.rm=TRUE),
            med_edu_index_cc=median(edu_index_cc, na.rm=TRUE),
            #deltas
            med_delta_hdi=median(delta_hdi, na.rm=TRUE),
            med_delta_perc_hdi=median(delta_perc_hdi, na.rm=TRUE),
            med_delta_income_index=median(delta_income_index, na.rm=TRUE),
            med_delta_perc_income_index=median(delta_perc_income_index, na.rm=TRUE),
            med_delta_lifex_index=median(delta_lifex_index, na.rm=TRUE),
            med_delta_perc_lifex_index=median(delta_perc_lifex_index, na.rm=TRUE),
            med_delta_edu_index=median(delta_edu_index, na.rm=TRUE),
            med_delta_perc_edu_index=median(delta_perc_edu_index, na.rm=TRUE)
            
      )%>%distinct()


data_all_med_int<-data_all_med%>%
  group_by(year, ssp, gdlcode, iso3)%>%
  transmute(
    #hdi
    med_hdi=mean(med_hdi, na.rm = TRUE), 
    #hdi cc
    mean_med_hdi_cc=mean(med_hdi_cc, na.rm = TRUE), 
    q10_med_hdi_cc=quantile(med_hdi_cc, probs=0.1, na.rm = TRUE),
    q05_med_hdi_cc=quantile(med_hdi_cc, probs=0.05, na.rm = TRUE),
    q90_med_hdi_cc=quantile(med_hdi_cc, probs=0.9, na.rm = TRUE),
    q95_med_hdi_cc=quantile(med_hdi_cc, probs=0.95, na.rm = TRUE),
    
    #income_index
    med_income_index=mean(med_income_index, na.rm = TRUE), 
    #income_index cc
    mean_med_income_index_cc=mean(med_income_index_cc, na.rm = TRUE), 
    q10_med_income_index_cc=quantile(med_income_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_income_index_cc=quantile(med_income_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_income_index_cc=quantile(med_income_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_income_index_cc=quantile(med_income_index_cc, probs=0.95, na.rm = TRUE),
    
    #lifex_index
    med_lifex_index=mean(med_lifex_index, na.rm = TRUE), 
    #lifex_index cc
    mean_med_lifex_index_cc=mean(med_lifex_index_cc, na.rm = TRUE), 
    q10_med_lifex_index_cc=quantile(med_lifex_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_lifex_index_cc=quantile(med_lifex_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_lifex_index_cc=quantile(med_lifex_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_lifex_index_cc=quantile(med_lifex_index_cc, probs=0.95, na.rm = TRUE),
    
    #edu_index
    med_edu_index=mean(med_edu_index, na.rm = TRUE), 
    #edu_index cc
    mean_edu_index_cc=mean(med_edu_index_cc, na.rm = TRUE), 
    q10_med_edu_index_cc=quantile(med_edu_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_edu_index_cc=quantile(med_edu_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_edu_index_cc=quantile(med_edu_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_edu_index_cc=quantile(med_edu_index_cc, probs=0.95, na.rm = TRUE),
    
    # deltas
    
    # hdi
    mean_med_delta_hdi=mean(med_delta_hdi, na.rm = TRUE), 
    mean_med_delta_perc_hdi=mean(med_delta_perc_hdi, na.rm = TRUE), 
    q10_med_delta_hdi=quantile(med_delta_hdi, probs=0.1, na.rm = TRUE), 
    q10_med_delta_perc_hdi=quantile(med_delta_perc_hdi, probs=0.1, na.rm = TRUE), 
    q05_med_delta_hdi=quantile(med_delta_hdi, probs=0.05, na.rm = TRUE), 
    q05_med_delta_perc_hdi=quantile(med_delta_perc_hdi, probs=0.05, na.rm = TRUE), 
    q90_med_delta_hdi=quantile(med_delta_hdi, probs=0.9, na.rm = TRUE), 
    q90_med_delta_perc_hdi=quantile(med_delta_perc_hdi, probs=0.9, na.rm = TRUE), 
    q95_med_delta_hdi=quantile(med_delta_hdi, probs=0.95, na.rm = TRUE), 
    q95_med_delta_perc_hdi=quantile(med_delta_perc_hdi, probs=0.95, na.rm = TRUE), 
    
    
    # income_index
    mean_med_delta_income_index=mean(med_delta_income_index, na.rm = TRUE), 
    mean_med_delta_perc_income_index=mean(med_delta_perc_income_index, na.rm = TRUE), 
    q10_med_delta_income_index=quantile(med_delta_income_index, probs=0.1, na.rm = TRUE), 
    q10_med_delta_perc_income_index=quantile(med_delta_perc_income_index, probs=0.1, na.rm = TRUE), 
    q05_med_delta_income_index=quantile(med_delta_income_index, probs=0.05, na.rm = TRUE), 
    q05_med_delta_perc_income_index=quantile(med_delta_perc_income_index, probs=0.05, na.rm = TRUE), 
    q90_med_delta_income_index=quantile(med_delta_income_index, probs=0.9, na.rm = TRUE), 
    q90_med_delta_perc_income_index=quantile(med_delta_perc_income_index, probs=0.9, na.rm = TRUE), 
    q95_med_delta_income_index=quantile(med_delta_income_index, probs=0.95, na.rm = TRUE), 
    q95_med_delta_perc_income_index=quantile(med_delta_perc_income_index, probs=0.95, na.rm = TRUE), 
    
    
    # lifex_index
    mean_med_delta_lifex_index=mean(med_delta_lifex_index, na.rm = TRUE), 
    mean_med_delta_perc_lifex_index=mean(med_delta_perc_lifex_index, na.rm = TRUE), 
    q10_med_delta_lifex_index=quantile(med_delta_lifex_index, probs=0.1, na.rm = TRUE), 
    q10_med_delta_perc_lifex_index=quantile(med_delta_perc_lifex_index, probs=0.1, na.rm = TRUE), 
    q05_med_delta_lifex_index=quantile(med_delta_lifex_index, probs=0.05, na.rm = TRUE), 
    q05_med_delta_perc_lifex_index=quantile(med_delta_perc_lifex_index, probs=0.05, na.rm = TRUE), 
    q90_med_delta_lifex_index=quantile(med_delta_lifex_index, probs=0.9, na.rm = TRUE), 
    q90_med_delta_perc_lifex_index=quantile(med_delta_perc_lifex_index, probs=0.9, na.rm = TRUE), 
    q95_med_delta_lifex_index=quantile(med_delta_lifex_index, probs=0.95, na.rm = TRUE), 
    q95_med_delta_perc_lifex_index=quantile(med_delta_perc_lifex_index, probs=0.95, na.rm = TRUE), 
    
    
    # edu_index
    mean_med_delta_edu_index=mean(med_delta_edu_index, na.rm = TRUE), 
    mean_med_delta_perc_edu_index=mean(med_delta_perc_edu_index, na.rm = TRUE), 
    q10_med_delta_edu_index=quantile(med_delta_edu_index, probs=0.1, na.rm = TRUE), 
    q10_med_delta_perc_edu_index=quantile(med_delta_perc_edu_index, probs=0.1, na.rm = TRUE), 
    q05_med_delta_edu_index=quantile(med_delta_edu_index, probs=0.05, na.rm = TRUE), 
    q05_med_delta_perc_edu_index=quantile(med_delta_perc_edu_index, probs=0.05, na.rm = TRUE), 
    q90_med_delta_edu_index=quantile(med_delta_edu_index, probs=0.9, na.rm = TRUE), 
    q90_med_delta_perc_edu_index=quantile(med_delta_perc_edu_index, probs=0.9, na.rm = TRUE), 
    q95_med_delta_edu_index=quantile(med_delta_edu_index, probs=0.95, na.rm = TRUE), 
    q95_med_delta_perc_edu_index=quantile(med_delta_perc_edu_index, probs=0.95, na.rm = TRUE), 
    
  )%>%distinct()

# save 
write.csv(x=data_all_med_int, file=file.path(out_dir,paste0('med_mod_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_intervals.csv")), row.names = FALSE)



### aggregate country

gc()
data_all_med_country<-data_all_med%>%filter(iso3 %in% sel_countries)%>%
  group_by(year, ssp,  boot, iso3)%>%
  transmute(country_w_hdi=weighted.mean(med_hdi, pop_gdl, na.rm = TRUE),
            country_w_hdi_cc=weighted.mean(med_hdi_cc, pop_gdl, na.rm = TRUE),
            country_w_income_index=weighted.mean(med_income_index, pop_gdl, na.rm = TRUE),
            country_w_income_index_cc=weighted.mean(med_income_index_cc, pop_gdl, na.rm = TRUE),
            country_w_lifex_index=weighted.mean(med_lifex_index, pop_gdl, na.rm = TRUE),
            country_w_lifex_index_cc=weighted.mean(med_lifex_index_cc, pop_gdl, na.rm = TRUE),
            country_w_edu_index=weighted.mean(med_edu_index, pop_gdl, na.rm = TRUE),
            country_w_edu_index_cc=weighted.mean(med_edu_index_cc, pop_gdl, na.rm = TRUE),
            # deltas
            country_w_delta_hdi=weighted.mean(med_delta_hdi, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_hdi=weighted.mean(med_delta_perc_hdi, pop_gdl, na.rm = TRUE),
            country_w_delta_income_index=weighted.mean(med_delta_income_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_income_index=weighted.mean(med_delta_perc_income_index, pop_gdl, na.rm = TRUE),
            country_w_delta_lifex_index=weighted.mean(med_delta_lifex_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_lifex_index=weighted.mean(med_delta_perc_lifex_index, pop_gdl, na.rm = TRUE),
            country_w_delta_edu_index=weighted.mean(med_delta_edu_index, pop_gdl, na.rm = TRUE),
            country_w_delta_perc_edu_index=weighted.mean(med_delta_perc_edu_index, pop_gdl, na.rm = TRUE)
  )%>%distinct()

gc()
data_all_med_country_int<-data_all_med_country%>%
  group_by(year, ssp, iso3)%>%
  transmute(
    #hdi
    med_country_w_hdi=mean(country_w_hdi, na.rm = TRUE), 
    #hdi cc
    mean_med_country_w_hdi_cc=mean(country_w_hdi_cc, na.rm = TRUE), 
    q10_med_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.1, na.rm = TRUE),
    q05_med_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.05, na.rm = TRUE),
    q90_med_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.9, na.rm = TRUE),
    q95_med_country_w_hdi_cc=quantile(country_w_hdi_cc, probs=0.95, na.rm = TRUE),
    
    #income_index
    med_country_w_income_index=mean(country_w_income_index, na.rm = TRUE), 
    #income_index cc
    mean_med_country_w_income_index_cc=mean(country_w_income_index_cc, na.rm = TRUE), 
    q10_med_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_country_w_income_index_cc=quantile(country_w_income_index_cc, probs=0.95, na.rm = TRUE),
    
    #lifex_index
    med_country_w_lifex_index=mean(country_w_lifex_index, na.rm = TRUE), 
    #lifex_index cc
    mean_med_country_w_lifex_index_cc=mean(country_w_lifex_index_cc, na.rm = TRUE), 
    q10_med_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_country_w_lifex_index_cc=quantile(country_w_lifex_index_cc, probs=0.95, na.rm = TRUE),
    
    #edu_index
    med_country_w_edu_index=mean(country_w_edu_index, na.rm = TRUE), 
    #edu_index cc
    mean_med_country_w_edu_index_cc=mean(country_w_edu_index_cc, na.rm = TRUE), 
    q10_med_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.1, na.rm = TRUE),
    q05_med_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.05, na.rm = TRUE),
    q90_med_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.9, na.rm = TRUE),
    q95_med_country_w_edu_index_cc=quantile(country_w_edu_index_cc, probs=0.95, na.rm = TRUE),
    
    # deltas
    
    # hdi
    mean_med_country_w_delta_hdi=mean(country_w_delta_hdi, na.rm = TRUE), 
    mean_med_country_w_delta_perc_hdi=mean(country_w_delta_perc_hdi, na.rm = TRUE), 
    q10_med_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.1, na.rm = TRUE), 
    q10_med_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.1, na.rm = TRUE), 
    q05_med_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.05, na.rm = TRUE), 
    q05_med_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.05, na.rm = TRUE), 
    q90_med_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.9, na.rm = TRUE), 
    q90_med_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.9, na.rm = TRUE), 
    q95_med_country_w_delta_hdi=quantile(country_w_delta_hdi, probs=0.95, na.rm = TRUE), 
    q95_med_country_w_delta_perc_hdi=quantile(country_w_delta_perc_hdi, probs=0.95, na.rm = TRUE), 
    
    
    # income_index
    mean_med_country_w_delta_income_index=mean(country_w_delta_income_index, na.rm = TRUE), 
    mean_med_country_w_delta_perc_income_index=mean(country_w_delta_perc_income_index, na.rm = TRUE), 
    q10_med_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.1, na.rm = TRUE), 
    q10_med_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.1, na.rm = TRUE), 
    q05_med_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.05, na.rm = TRUE), 
    q05_med_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.05, na.rm = TRUE), 
    q90_med_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.9, na.rm = TRUE), 
    q90_med_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.9, na.rm = TRUE), 
    q95_med_country_w_delta_income_index=quantile(country_w_delta_income_index, probs=0.95, na.rm = TRUE), 
    q95_med_country_w_delta_perc_income_index=quantile(country_w_delta_perc_income_index, probs=0.95, na.rm = TRUE), 
    
    
    # lifex_index
    mean_med_country_w_delta_lifex_index=mean(country_w_delta_lifex_index, na.rm = TRUE), 
    mean_med_country_w_delta_perc_lifex_index=mean(country_w_delta_perc_lifex_index, na.rm = TRUE), 
    q10_med_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.1, na.rm = TRUE), 
    q10_med_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.1, na.rm = TRUE), 
    q05_med_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.05, na.rm = TRUE), 
    q05_med_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.05, na.rm = TRUE), 
    q90_med_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.9, na.rm = TRUE), 
    q90_med_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.9, na.rm = TRUE), 
    q95_med_country_w_delta_lifex_index=quantile(country_w_delta_lifex_index, probs=0.95, na.rm = TRUE), 
    q95_med_country_w_delta_perc_lifex_index=quantile(country_w_delta_perc_lifex_index, probs=0.95, na.rm = TRUE), 
    
    
    # edu_index
    mean_med_country_w_delta_edu_index=mean(country_w_delta_edu_index, na.rm = TRUE), 
    mean_med_country_w_delta_perc_edu_index=mean(country_w_delta_perc_edu_index, na.rm = TRUE), 
    q10_med_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.1, na.rm = TRUE), 
    q10_med_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.1, na.rm = TRUE), 
    q05_med_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.05, na.rm = TRUE), 
    q05_med_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.05, na.rm = TRUE), 
    q90_med_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.9, na.rm = TRUE), 
    q90_med_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.9, na.rm = TRUE), 
    q95_med_country_w_delta_edu_index=quantile(country_w_delta_edu_index, probs=0.95, na.rm = TRUE), 
    q95_med_country_w_delta_perc_edu_index=quantile(country_w_delta_perc_edu_index, probs=0.95, na.rm = TRUE), 
    
  )%>%distinct()

# save 
#write.csv(x=data_all_country, file=file.path(out_dir,paste0('country_sel_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
write.csv(x=data_all_med_country_int, file=file.path(out_dir,paste0('med_country_sel_agg_all_idx','_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_intervals.csv")), row.names = FALSE)


############################



### add deltas
gc()
data_all_gnipc <- data_all_gnipc %>%
  mutate(across(starts_with("income_index_cc."), 
                .fns = list(
                  delta = ~ . - income_index, 
                  perc_delta = ~ (. - income_index) / income_index
                ), 
                .names = "{.fn}.{.col}")) %>%
  rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("delta.income_index_cc.")) %>%
  rename_with(~ str_remove(.x, "income_index_cc\\."), starts_with("perc_delta.income_index_cc."))


gc()
data_all_leb <- data_all_leb %>%
  mutate(across(starts_with("lifex_index_cc."), 
                .fns = list(
                  delta = ~ . - lifex_index, 
                  perc_delta = ~ (. - lifex_index) / lifex_index
                ), 
                .names = "{.fn}.{.col}")) %>%
  rename_with(~ str_remove(.x, "lifex_index_cc\\."), starts_with("delta.lifex_index_cc.")) %>%
  rename_with(~ str_remove(.x, "lifex_index_cc\\."), starts_with("perc_delta.lifex_index_cc."))

gc()
data_all_eys <- data_all_eys %>%
  mutate(across(starts_with("edu_index_cc."), 
                .fns = list(
                  delta = ~ . - edu_index, 
                  perc_delta = ~ (. - edu_index) / edu_index
                ), 
                .names = "{.fn}.{.col}")) %>%
  rename_with(~ str_remove(.x, "edu_index_cc\\."), starts_with("delta.edu_index_cc.")) %>%
  rename_with(~ str_remove(.x, "edu_index_cc\\."), starts_with("perc_delta.edu_index_cc."))

gc()
data_hdi <- data_hdi %>%
  mutate(across(starts_with("hdi_cc."), 
                .fns = list(
                  delta = ~ . - hdi, 
                  perc_delta = ~ (. - hdi) / hdi
                ), 
                .names = "{.fn}.{.col}")) %>%
  rename_with(~ str_remove(.x, "hdi_cc\\."), starts_with("delta.hdi_cc.")) %>%
  rename_with(~ str_remove(.x, "hdi_cc\\."), starts_with("perc_delta.hdi_cc."))

################################################################################


sel_countries<-c("CAN", "USA", "ITA", "RUS", "AUS", "VEN", "BRA", "ETH", "IND", "CHN", "NIG", "SDN", "VNM" , "AFG", "SAU")



### hdi 

# iso3 
data_iso<-data_hdi %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(. ,w=pop_gdl)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('country_agg','_hdi_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)

# glob

data_glob<-data_hdi %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(. ,w=pop_gdl)))

data_glob<-left_join(data_glob,ssp_match )
data_glob<-left_join(data_glob,model_match )
write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('glob_agg','_hdi_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)

### gnipc 

# iso3 
data_iso<-data_all_gnipc %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(. ,w=pop_gdl)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('country_agg','_gnipc_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)

# glob

data_glob<-data_all_gnipc %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(. ,w=pop_gdl)))

data_glob<-left_join(data_glob,ssp_match )
data_glob<-left_join(data_glob,model_match )
write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('glob_agg','_gnipc_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)


### leb 

# iso3 
data_iso<-data_all_leb %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(. ,w=pop_gdl)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('country_agg','_leb_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)

# glob

data_glob<-data_all_leb %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(. ,w=pop_gdl)))

data_glob<-left_join(data_glob,ssp_match )
data_glob<-left_join(data_glob,model_match )
write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('glob_agg','_leb_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)


### eys 

# iso3 
data_iso<-data_all_eys %>% filter(iso3 %in% sel_countries)%>%
  group_by(iso3, ssp, model, year) %>%
  summarise(across(-gdlcode , ~weighted.mean(. ,w=pop_gdl)))


write.csv(x=data_iso, file=file.path(out_dir_lag,paste0('country_agg','_eys_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)

# glob

data_glob<-data_all_eys %>%
  group_by(ssp, model, year) %>%
  summarise(across(-c(gdlcode, iso3) , ~weighted.mean(. ,w=pop_gdl)))

data_glob<-left_join(data_glob,ssp_match )
data_glob<-left_join(data_glob,model_match )
write.csv(x=data_glob, file=file.path(out_dir_lag,paste0('glob_agg','_eys_',type,'_',spec,'_',vars, "_pop_weight_nlags_",N,"boot_impacts_all.csv")), row.names = FALSE)
