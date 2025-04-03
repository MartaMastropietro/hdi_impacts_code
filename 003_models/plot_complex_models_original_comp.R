### plot effects 

### rationale: tm, rr controls, but discard non significant lags for each variable. add extremes

rm(list=ls())
source("utils/libraries.R")

# xlsx write
require(openxlsx)


# libraries needed
library(dplyr)
library(readr)
library(ggplot2)

library(gt)

# output dir
out_dir<-"output/models"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}
out_dir<-"output/models/original_components_tp_extr_complex_models_try"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")
data_country <- read_csv("output/data_hdi_original_comp_climate_country_pop_weight_1990_2020_less_na.csv")


### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

# setup for lag models 
varns=c('diff_TM','diff_RR', "diff_TVAR", "diff_HW", "diff_RX", "diff_PEXT", "diff_WD", "diff_SPI", "diff_SPEI", "diff_PET", 
        'diff_TM_2','diff_RR_2', "diff_TVAR_2", "diff_HW_2", "diff_RX_2", "diff_PEXT_2", "diff_WD_2", "diff_SPI_2", "diff_SPEI_2", "diff_PET_2")

modns=c('TM','RR', "TVAR", "HW", "RX", "PEXT", "WD", "SPI", "SPEI", "PET", 
        'TM_2','RR_2', "TVAR_2", "HW_2", "RX_2", "PEXT_2", "WD_2", "SPI_2", "SPEI_2", "PET_2")

adap=c('lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 
       'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc', 'lag_log_gni_pc','lag_log_gni_pc')

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
for ( i in 1:10){
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

data<-data%>%
  arrange(gdlcode, year)%>%
  group_by(gdlcode)%>%
  mutate(
  lag_gr_mys=dplyr::lag(gr_mys),
  lag_gr_eys=dplyr::lag(gr_eys),
  lag_gr_gnipc=dplyr::lag(gr_gnipc),
  lag_gr_leb=dplyr::lag(gr_leb)
)


### all with this fe spec 
i <- "gdlcode + year + iso3[year] +iso3[year^2]"

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

################################################################################
### plots
################################################################################

### for coefplots, be careful on r formulation, it has to be compatible with strsplit(r, " ")[[1]]

o<-"gr_mys"

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_RX +  RR_i_diff_RX + diff_PEXT +  TM_i_diff_PEXT "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

i <- "gdlcode + year"
f <- as.formula(paste( o, "~", paste0("lag_",o, "+"),r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
f <- as.formula(paste( o, "~", paste0("lag_",o, "+"),r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_RX +  RR_i_diff_RX + diff_PEXT +  TM_i_diff_PEXT "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


# similar results for extremes if we also add time varying trends, we test usefulness of fe, fe vs re, time fe 

form <-(paste( o, "~", r))
form_ar<-(paste( o, "~",  paste0("lag_",o, "+"),r))
ols<-lm(form, data=data)

fixed <- plm(form, data=data, index=c("gdlcode", "year"),effect="twoways", model="within")
summary(fixed)
fixed_ar<- plm(form_ar, data=data, index=c("gdlcode", "year"),effect="twoways", model="within")
summary(fixed_ar)
# pdata<-pdata.frame(data, index=c("gdlcode","year"))
# fixed_ar_gmm <- pgmm(formula=as.formula(paste0(form_ar ,"| lag(",o,", 2:10)")), 
#                      data = pdata, effect = "twoways", model = "twosteps")
# summary(fixed_ar_gmm)

random <- plm(form, data=data, index=c("gdlcode", "year"), model="random")

pFtest(fixed, ols)  # keep fe

phtest(fixed, random) # keep fe

fixed_no_time <-plm(form, data=data, index=c("gdlcode", "year"),effect="individual", model="within")
plmtest(fixed, c("time"), type=("bp")) # add time fe
###
  
# coef plot
m <- fixest::feols(f, data, panel.id=pan_id)
coefs_scaled<- m %>% tidy(conf.int = TRUE)
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_mod.jpeg")), height = 4, width = 4)

# map plot 
coefs<-m$coefficients
# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          rx_diff_mean_damages=NA,
                          pext_diff_mean_damages=NA,
                          all_mean_damages=NA
                          
)

### 
for (r in unique(data$gdlcode)){
  
  # laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  
  rx<-abs(sd(data$diff_WD[data$gdlcode == r], na.rm = TRUE))
  pext<-abs(sd(data$diff_PEXT[data$gdlcode == r], na.rm = TRUE))
  
  tm_contr<-mean(data$TM[data$gdlcode == r], na.rm = TRUE)
  rr_contr<-mean(data$RR[data$gdlcode == r], na.rm = TRUE)
  
    
    
  # rx damages
  mean_damages[mean_damages$gdlcode==r, c("rx_diff_mean_damages") ]<-
    rx*coefs["diff_RX"]  + rx*rr_contr*coefs["RR_i_diff_RX"] 
  
  # pext damages
  mean_damages[mean_damages$gdlcode==r, c("pext_diff_mean_damages") ]<-
    pext*coefs["diff_PEXT"] + pext*tm_contr*coefs["TM_i_diff_PEXT"] 
  
  
  # sum all
  mean_damages[mean_damages$gdlcode==r, c("all_mean_damages") ]<-
    mean_damages[mean_damages$gdlcode==r, c("rx_diff_mean_damages") ]+
    mean_damages[mean_damages$gdlcode==r, c("pext_diff_mean_damages") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damages))+theme_bw()+  scale_fill_gradient2() + 
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of extremes") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_regional_mean_damages.png")), g, width=18, height=15)

###############################################################################


o<-"gr_eys"

i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + WD + WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# similar results for extremes if we also add time varying trends, we test usefulness of fe, fe vs re, time fe 

form <-as.formula(paste( o, "~", r))
ols<-lm(form, data=data)
fixed <- plm(form, data=data, index=c("gdlcode", "year"),effect="twoways", model="within")
random <- plm(form, data=data, index=c("gdlcode", "year"), model="random")

pFtest(fixed, ols)  # keep fe

phtest(fixed, random) # keep fe

fixed_no_time <-plm(form, data=data, index=c("gdlcode", "year"),effect="individual", model="within")
plmtest(fixed, c("time"), type=("bp")) # add time fe
###

wd_opt<-(2.287319e-04 )/(2*7.061100e-07)

# coef plot

coefs_scaled<-m %>% tidy(conf.int = TRUE)
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_mod.jpeg")), height = 4, width = 4)

# map plot 
coefs<-m$coefficients
# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          wd_lev_mean_damages=NA, 
                          all_mean_damages=NA
                          
)

for (r in unique(data$gdlcode)){
  
  # laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  wd<-abs(sd(data$SPI[data$gdlcode == r], na.rm = TRUE))
  
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_lev_mean_damages") ]<-
    (wd*coefs["WD"] + ((wd)^2)*coefs["WD_2"] ) -
    - (wd_opt*coefs["WD"] + ((wd_opt)^2)*coefs["WD_2"] ) 
  
  
  
  # sum all
  mean_damages[mean_damages$gdlcode==r, c("all_mean_damages") ]<-
    mean_damages[mean_damages$gdlcode==r, c("wd_lev_mean_damages") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damages))+theme_bw()+  scale_fill_gradient2() + 
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of extremes") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_regional_mean_damages.png")), g, width=18, height=15)


###############################################################################


o<-"gr_leb"


i <- "gdlcode + year"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_TVAR + TVAR_i_diff_TVAR + diff_WD + TM_i_diff_WD + diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_TVAR + TVAR_i_diff_TVAR + diff_WD + TM_i_diff_WD + diff_RX + TM_i_diff_RX"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# similar results for extremes if we also add time varying trends, we test usefulness of fe, fe vs re, time fe 

form <-as.formula(paste( o, "~", r))
ols<-lm(form, data=data)
fixed <- plm(form, data=data, index=c("gdlcode", "year"),effect="twoways", model="within")
random <- plm(form, data=data, index=c("gdlcode", "year"), model="random")

pFtest(fixed, ols)  # keep fe

phtest(fixed, random) # keep fe

fixed_no_time <-plm(form, data=data, index=c("gdlcode", "year"),effect="individual", model="within")
plmtest(fixed, c("time"), type=("bp")) # add time fe
###

coefs_scaled<-m %>% tidy(conf.int = TRUE)

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_mod.jpeg")), height = 4, width = 4)


# map plot 
coefs<-m$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          wd_diff_mean_damages=NA, 
                          rx_diff_mean_damages=NA,
                          tvar_diff_mean_damages=NA,
                          all_mean_damages=NA
                          
)

for (r in unique(data$gdlcode)){
  
  # laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  
  rx<-abs(sd(data$diff_RX[data$gdlcode == r], na.rm = TRUE))
  wd<-abs(sd(data$diff_WD[data$gdlcode == r], na.rm = TRUE))
  tvar<-abs(sd(data$diff_TVAR[data$gdlcode == r], na.rm = TRUE))
  
  tm_contr<-mean(data$TM[data$gdlcode == r], na.rm = TRUE)
  tvar_contr<-mean(data$TVAR[data$gdlcode == r], na.rm = TRUE)
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_diff_mean_damages") ]<-
    wd*coefs["diff_WD"] + wd*tm_contr*coefs["TM_i_diff_WD"] 
  
  # rx damages
  mean_damages[mean_damages$gdlcode==r, c("rx_diff_mean_damages") ]<-
    rx*coefs["diff_RX"] + rx*tm_contr*coefs["TM_i_diff_RX"] 
  
  # tvar damages
  mean_damages[mean_damages$gdlcode==r, c("tvar_diff_mean_damages") ]<-
    tvar*coefs["diff_TVAR"] + tvar*tvar_contr*coefs["TVAR_i_diff_TVAR"] 
  
  
  
  # sum all
  mean_damages[mean_damages$gdlcode==r, c("all_mean_damages") ]<-
    mean_damages[mean_damages$gdlcode==r, c("wd_diff_mean_damages") ]+
    mean_damages[mean_damages$gdlcode==r, c("rx_diff_mean_damages") ]+
    mean_damages[mean_damages$gdlcode==r, c("tvar_diff_mean_damages") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damages))+theme_bw()+  scale_fill_gradient2() + 
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of extremes") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_regional_mean_damages.png")), g, width=18, height=15)

###############################################################################

o<-"gr_gnipc"


i <- "gdlcode + year "
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM + lag_1_TM_i_diff_TM + diff_PEXT + TM_i_diff_PEXT + diff_WD + RR_i_diff_WD + diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM + lag_1_TM_i_diff_TM + diff_PEXT + TM_i_diff_PEXT + diff_WD + RR_i_diff_WD + diff_SPI"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# similar results for extremes if we also add time varying trends, we test usefulness of fe, fe vs re, time fe 

form <-as.formula(paste( o, "~", r))
ols<-lm(form, data=data)
fixed <- plm(form, data=data, index=c("gdlcode", "year"),effect="twoways", model="within")
random <- plm(form, data=data, index=c("gdlcode", "year"), model="random")

pFtest(fixed, ols)  # keep fe

phtest(fixed, random) # keep fe

fixed_no_time <-plm(form, data=data, index=c("gdlcode", "year"),effect="individual", model="within")
plmtest(fixed, c("time"), type=("bp")) # add time fe

pbgtest(fixed) # serial correlation: better to use clustered err
###


coefs<-m %>% tidy(conf.int = TRUE)

coefs_scaled<-coefs

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_mod.jpeg")), height = 4, width = 4)


# map plot 
coefs<-m$coefficients
# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          wd_diff_mean_damages=NA, 
                          pext_diff_mean_damages=NA,
                          spi_diff_mean_damages=NA,
                          all_mean_damages=NA
                          
)

for (r in unique(data$gdlcode)){
  
  # laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  
  pext<-abs(sd(data$diff_PEXT[data$gdlcode == r], na.rm = TRUE))
  wd<-abs(sd(data$diff_WD[data$gdlcode == r], na.rm = TRUE))
  spi<-abs(sd(data$diff_SPI[data$gdlcode == r], na.rm = TRUE))
  
  tm_contr<-mean(data$TM[data$gdlcode == r], na.rm = TRUE)
  rr_contr<-mean(data$RR[data$gdlcode == r], na.rm = TRUE)
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_diff_mean_damages") ]<-
    wd*coefs["diff_WD"] + wd*rr_contr*coefs["RR_i_diff_WD"] 
  
  # pext damages
  mean_damages[mean_damages$gdlcode==r, c("pext_diff_mean_damages") ]<-
    pext*coefs["diff_PEXT"] + pext*tm_contr*coefs["TM_i_diff_PEXT"] 
  
  # spi damages
  mean_damages[mean_damages$gdlcode==r, c("spi_diff_mean_damages") ]<-
    spi*coefs["diff_SPI"] 
  
  
  
  # sum all
  mean_damages[mean_damages$gdlcode==r, c("all_mean_damages") ]<-
    mean_damages[mean_damages$gdlcode==r, c("wd_diff_mean_damages") ]+
    mean_damages[mean_damages$gdlcode==r, c("pext_diff_mean_damages") ]+
    mean_damages[mean_damages$gdlcode==r, c("spi_diff_mean_damages") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damages))+theme_bw()+  scale_fill_gradient2() + 
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of extremes") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 20))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_regional_mean_damages.png")), g, width=18, height=15)

# separate
g2<-ggplot(mean_damages)+geom_sf( aes(fill=wd_diff_mean_damages))+theme_bw()+ scale_fill_gradient2() +
  labs(fill=paste0(o, " change"))+ ggtitle("Marginal effect of WD")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_wd_regional_mean_damages.png")), g2, width=15, height=12)

g3<-ggplot(mean_damages)+geom_sf( aes(fill=pext_diff_mean_damages))+theme_bw() + scale_fill_gradient2() +
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of PEXT")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_pext_regional_mean_damages.png")), g3, width=15, height=12)

g4<-ggplot(mean_damages)+geom_sf( aes(fill=spi_diff_mean_damages))+theme_bw() + scale_fill_gradient2() +
  labs(fill=paste0(o, " change")) + ggtitle("Marginal effect of SPI")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_complex_mod_spi_regional_mean_damages.png")), g4, width=15, height=12)


################################################################################
### plots adap, original + gnipc interact
################################################################################

### for coefplots, be careful on r formulation, it has to be compatible with strsplit(r, " ")[[1]]

o<-"gr_mys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_RX +  RR_i_diff_RX + diff_PEXT + TM_i_diff_PEXT + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_mod.jpeg")), height = 4, width = 4)


###############################################################################


o<-"gr_eys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + WD + WD_2 + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_mod.jpeg")), height = 4, width = 4)

###############################################################################


o<-"gr_leb"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_TVAR + TVAR_i_diff_TVAR + diff_WD + TM_i_diff_WD + diff_RX + TM_i_diff_RX + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_diff_TVAR + lag_log_gni_pc_i_diff_WD + lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)

coefs_scaled<-coefs

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_mod.jpeg")), height = 4, width = 4)

###############################################################################

o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM + lag_1_TM_i_diff_TM + diff_PEXT + TM_i_diff_PEXT + diff_WD + RR_i_diff_WD + diff_SPI + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_WD + lag_log_gni_pc_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)

coefs_scaled<-coefs

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_mod.jpeg")), height = 4, width = 4)


###--------------------------------------------------------------------------###

################################################################################
### plots adap, original + gnipc interact
################################################################################

### for coefplots, be careful on r formulation, it has to be compatible with strsplit(r, " ")[[1]]

o<-"gr_mys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_RX + diff_PEXT + SPI + SPI_2 + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_RX + lag_log_gni_pc_i_SPI + lag_log_gni_pc_i_SPI_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_no_extr_interact_mod.jpeg")), height = 4, width = 4)


###############################################################################


o<-"gr_eys"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + WD + WD_2 + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_WD + lag_log_gni_pc_i_WD_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m %>% tidy(conf.int = TRUE)
coefs_scaled<-coefs
for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_no_extr_interact_mod.jpeg")), height = 4, width = 4)

###############################################################################


o<-"gr_leb"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + diff_TVAR + diff_WD  + diff_RX + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_log_gni_pc_i_diff_TVAR + lag_log_gni_pc_i_diff_WD + lag_log_gni_pc_i_diff_RX "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)

coefs_scaled<-coefs

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_no_extr_interact_mod.jpeg")), height = 4, width = 4)

###############################################################################

o<-"gr_gnipc"

i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "diff_TM + TM_i_diff_TM + diff_RR + RR_i_diff_RR + lag_1_diff_TM + lag_1_TM_i_diff_TM + diff_PEXT + diff_WD + diff_SPI + lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_RR + lag_1_lag_log_gni_pc_i_diff_TM + lag_log_gni_pc_i_diff_PEXT + lag_log_gni_pc_i_diff_WD + lag_log_gni_pc_i_diff_SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

coefs<-m %>% tidy(conf.int = TRUE)

coefs_scaled<-coefs

for ( i in 1:dim(coefs_scaled)[1]){
  name<-as.character(coefs_scaled[c("term")][i,])
  coefs_scaled[i,2]<- as.numeric(coefs_scaled[i,2])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,6]<- as.numeric(coefs_scaled[i,6])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  coefs_scaled[i,7]<- as.numeric(coefs_scaled[i,7])*mean(aggregate(data[[name]], list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
  
}

order<-setdiff(strsplit(r, " ")[[1]], c("+", ""))
coefs_scaled$term <- factor(coefs_scaled$term, levels = order)

coefs_scaled%>%ggplot(aes(estimate, term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  # add in a dotted line at zero
  geom_vline(xintercept = 0, lty = 2) +
  labs(
    x = "Scaled Coefficient",
    y = NULL,
    title = paste0("Effect on ", o)) + theme_bw()
ggsave(filename=file.path(out_dir, paste0(o,"_plot_complex_adap_no_extr_interact_mod.jpeg")), height = 4, width = 4)
