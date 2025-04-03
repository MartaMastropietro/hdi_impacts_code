
rm(list=ls())
source("utils/libraries.R")


out_dir<-"output/projections"
if(!dir.exists(out_dir)){dir.create(out_dir)}

out_dir<-"output/projections/original_comp"
if(!dir.exists(out_dir)){dir.create(out_dir)}

### Countries borders 
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,4)]
countries$geometry<-NULL



### socio econ projections 

data_dir<-"data/hdi_data/projections"

var_name<-"eys"

mean_years_sch_25_interp <- read_csv(file.path(out_dir, "preproc_proj_mys.csv"))


population_data_interp <-read_csv(file.path(out_dir, "preproc_proj_population.csv"))

################################################################################


# define expected years schooling, for later modelling
exp_years_sch<-mean_years_sch_25_interp
colnames(exp_years_sch)[which(colnames(exp_years_sch)=="value_interp")]<-"mys"


### eys modelling 
# load("D:/Marta/PhD/OneDrive - Politecnico di Milano/AAA - PhD/Projects/Impacts and Hazards/hdi_climate_impacts/output/projections/eys_mys_ar_by_country_model_coefs.RData")
data_hdi <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")

exp_years_sch_mod<-exp_years_sch

exp_years_sch_mod<-left_join(exp_years_sch_mod , data_hdi[, c(1,2,3,5)])

y2020<-exp_years_sch_mod[which(exp_years_sch_mod$year==2020),]
na_y2020<-unique(y2020[which(is.na(y2020$eys)),c("iso3")])

exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "ABW"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "VEN" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "CUW"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "VEN" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "ESH"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "MRT" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "GUM"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "PHL" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "MAC"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "HKG" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "NCL"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "AUS" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "PRI"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "DOM" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "PYF"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "NZL" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "SOM"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "ETH" & data_hdi$year==2020)]
exp_years_sch_mod[which(exp_years_sch_mod$iso3 == "VIR"), "eys"]<-data_hdi$eys[which(data_hdi$iso3 == "DOM" & data_hdi$year==2020)]

# coef_list[["ABW"]]<-coef_list[["VEN"]]
# coef_list[["CUW"]]<-coef_list[["VEN"]]
# coef_list[["ESH"]]<-coef_list[["MRT"]]
# coef_list[["GUM"]]<-coef_list[["PHL"]]
# coef_list[["MAC"]]<-coef_list[["HKG"]]
# coef_list[["NCL"]]<-coef_list[["AUS"]]
# coef_list[["PRI"]]<-coef_list[["DOM"]]
# coef_list[["PYF"]]<-coef_list[["NZL"]]
# coef_list[["SOM"]]<-coef_list[["ETH"]]
# coef_list[["VIR"]]<-coef_list[["DOM"]]

exp_years_sch_mod$mod_eys<-NA
exp_years_sch_mod$mod_eys[which(exp_years_sch_mod$year %in% c(2020))]<-exp_years_sch_mod$eys[which(exp_years_sch_mod$year %in% c(2020))]
exp_years_sch_mod$eys<-NULL
exp_years_sch_mod$eys<-exp_years_sch_mod$mod_eys
exp_years_sch_mod$mod_eys<-NULL

# exp_years_sch_mod <- exp_years_sch_mod %>% 
#   arrange(Scenario, iso3, year) %>% 
#   group_by(Scenario, iso3) %>% 
#   mutate(lmys=dplyr::lag(mys,1)
#         )

# simple mys linear relationship
exp_years_sch_mod$eys<-as.numeric( as.matrix(data.frame(x=rep(1, length(exp_years_sch_mod$mys)),y=exp_years_sch_mod$mys)) %*%  c( 5.6924322 ,  0.8353084  ) )

# for (ssp in ssp_names){
#   for (iso in unique(exp_years_sch_mod$iso3)){
#     for (y in 2020:2100){
#         exp_years_sch_mod$eys[ which(exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y & exp_years_sch_mod$iso3==iso) ] <-
#           sum(as.numeric(c(05.6924322 ,  0.8353084  ))*as.numeric(c(1,exp_years_sch_mod$mys[ which(exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y & exp_years_sch_mod$iso3==iso)] )))
#           #sum(as.numeric(c(0.23069401 , 0.99156740  ,0.06216589, -0.06400484 ))*as.numeric(c(1,
#           #                                                                                   exp_years_sch_mod$eys[ which(exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==(y-1) & exp_years_sch_mod$iso3==iso) ] , 
#           #                                                                                   exp_years_sch_mod$lmys[ which(exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y & exp_years_sch_mod$iso3==iso) ] , 
#           #                                                                                   exp_years_sch_mod$mys[ which(exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y & exp_years_sch_mod$iso3==iso) ] )))
#     }
#   }
# }

exp_years_sch_mod<-exp_years_sch_mod[, c("iso3", "year", "Scenario", "eys")]

exp_years_sch_mod<- inner_join(exp_years_sch_mod, population_data_interp)
colnames(exp_years_sch_mod)[colnames(exp_years_sch_mod)=="eys"]<-"value_interp"



# debias based on historical 2020 value 

data_hist_eys<-data_hdi[, c("iso3", "year", "eys")]

common_iso<-intersect(unique(data_hist_eys$iso3), unique(exp_years_sch_mod$iso3))

data_hist_eys<-data_hist_eys[ which(data_hist_eys$iso3 %in% common_iso), ]


ggplot(data_hist_eys[which(data_hist_eys$iso3=="CAN"  & data_hist_eys$year<=2025),], aes(x = year, y = eys)) +
  geom_line(size = 1)


exp_years_sch_mod<-exp_years_sch_mod[ which(exp_years_sch_mod$iso3 %in% common_iso), ]

exp_years_sch_mod<-exp_years_sch_mod[ -which(exp_years_sch_mod$year==2020), ]


for (ssp in unique(exp_years_sch_mod$Scenario)){
  for (c in unique(exp_years_sch_mod$iso3)){
    v <- data_hist_eys$eys[which(data_hist_eys$year==2021 & data_hist_eys$iso3==c)]
    delta <- v - exp_years_sch_mod$value_interp[which(exp_years_sch_mod$year==2021 & exp_years_sch_mod$iso3==c & exp_years_sch_mod$Scenario==ssp)] 
    for( y in 2021:2100)
      exp_years_sch_mod$value_interp[which(exp_years_sch_mod$iso3==c & exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y)] <-
      delta + exp_years_sch_mod$value_interp[which(exp_years_sch_mod$iso3==c & exp_years_sch_mod$Scenario==ssp & exp_years_sch_mod$year==y)] 
  }
}

colnames(data_hist_eys)[colnames(data_hist_eys)=="eys"]<-"value_interp"

unique(data_hist_eys$year)

complete_dat<-expand.grid(year=1990:2021, Scenario=unique(exp_years_sch_mod$Scenario), iso3=unique(data_hist_eys$iso3))
data_hist_eys<-inner_join(data_hist_eys ,complete_dat )

data_hist_eys$pop<-NA
exp_years_sch_mod<-rbind(exp_years_sch_mod, data_hist_eys)


ggplot(exp_years_sch_mod[which(exp_years_sch_mod$iso3=="CAN" & exp_years_sch_mod$year>=2020),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_line(size = 1)

ggplot(exp_years_sch_mod[which(exp_years_sch_mod$iso3=="ALB"  & exp_years_sch_mod$year<=2025),], aes(x = year, y = log(value_interp), color = Scenario)) +
  geom_line(size = 1)

ggplot(exp_years_sch_mod[which(exp_years_sch_mod$iso3=="ALB"  ),], aes(x = year, y = log(value_interp), color = Scenario)) +
  geom_line(size = 1)


### add pop proj to data

exp_years_sch_mod<-left_join(exp_years_sch_mod, population_data_interp)


### save 
write.csv(exp_years_sch_mod, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)






