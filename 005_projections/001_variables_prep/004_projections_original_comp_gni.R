
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

var_name<-"gnipc"

gdp_pc <- read_excel("data/hdi_data/projections/gdppc_ssp_data.xlsx")

population_data_interp<-read_csv(file.path(out_dir, "preproc_proj_population.csv"))

gdp_pc_interp <- read_csv(file.path(out_dir, "preproc_proj_gdp_pc.csv"))


################################################################################

# define  for later modelling
gnipc<-gdp_pc_interp
colnames(gnipc)[which(colnames(gnipc)=="value_interp")]<-"gdp_pc"

### gni modelling 

gnipc_mod<-gnipc

# hdi data for scenario debiasing
data_hdi <- read_csv("data/hdi_data/data_hdi_components_undp_national_1990_2021.csv")
data_hdi$lmys=log(data_hdi$mys)
data_hdi$leys=log(data_hdi$eys)
data_hdi$lgnipc=log(data_hdi$gnipc)



### simple gni linear relationship
gnipc_mod$lgdp_pc<-log(gnipc_mod$gdp_pc)

gnipc_mod$gnipc<-exp(as.numeric( as.matrix(data.frame(x=rep(1, length(gnipc_mod$lgdp_pc)),y=gnipc_mod$lgdp_pc)) %*%  c(0.082995    ,  0.988463     ) ))

gnipc_mod<-gnipc_mod[, c("iso3", "year", "Scenario", "gnipc")]

gnipc_mod<- inner_join(gnipc_mod, population_data_interp)
colnames(gnipc_mod)[colnames(gnipc_mod)=="gnipc"]<-"value_interp"

ggplot(gnipc_mod[which(gnipc_mod$iso3=="BRA"),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_line(size = 1)


# debias based on historical 2020 value 

data_hist_gni<-data_hdi[, c("iso3", "year", "gnipc")]

common_iso<-intersect(unique(data_hist_gni$iso3), unique(gnipc_mod$iso3))

data_hist_gni<-data_hist_gni[ which(data_hist_gni$iso3 %in% common_iso), ]


ggplot(data_hist_gni[which(data_hist_gni$iso3=="CAN"  & data_hist_gni$year<=2025),], aes(x = year, y = gnipc)) +
  geom_line(size = 1)


gnipc_mod<-gnipc_mod[ which(gnipc_mod$iso3 %in% common_iso), ]

gnipc_mod<-gnipc_mod[ -which(gnipc_mod$year==2020), ]


for (ssp in unique(gnipc_mod$Scenario)){
  for (c in unique(gnipc_mod$iso3)){
    v <- data_hist_gni$gnipc[which(data_hist_gni$year==2021 & data_hist_gni$iso3==c)]
    delta <- v - gnipc_mod$value_interp[which(gnipc_mod$year==2021 & gnipc_mod$iso3==c & gnipc_mod$Scenario==ssp)] 
    for( y in 2021:2100)
    gnipc_mod$value_interp[which(gnipc_mod$iso3==c & gnipc_mod$Scenario==ssp & gnipc_mod$year==y)] <-
     delta + gnipc_mod$value_interp[which(gnipc_mod$iso3==c & gnipc_mod$Scenario==ssp & gnipc_mod$year==y)] 
  }
}

colnames(data_hist_gni)[colnames(data_hist_gni)=="gnipc"]<-"value_interp"

unique(data_hist_gni$year)

complete_dat<-expand.grid(year=1990:2021, Scenario=unique(gnipc_mod$Scenario), iso3=unique(data_hist_gni$iso3))
data_hist_gni<-inner_join(data_hist_gni ,complete_dat )

data_hist_gni$pop<-NA
gnipc_mod<-rbind(gnipc_mod, data_hist_gni)


ggplot(gnipc_mod[which(gnipc_mod$iso3=="CAN" & gnipc_mod$year>=2020),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_line(size = 1)


ggplot(gnipc_mod[which(gnipc_mod$iso3=="ALB"  & gnipc_mod$year<=2025),], aes(x = year, y = log(value_interp), color = Scenario)) +
  geom_line(size = 1)

ggplot(gnipc_mod[which(gnipc_mod$iso3=="ALB"  ),], aes(x = year, y = log(value_interp), color = Scenario)) +
  geom_line(size = 1)




### add pop proj to data

gnipc_mod<-left_join(gnipc_mod, population_data_interp)


### save 
write.csv(gnipc_mod, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)


####################

### autoreg relationship

# gnipc_mod$pop<-NULL
# 
# data_hist_gni<-data_hdi[, c("iso3", "year", "lag_lgnipc", "lgnipc")]
# unique(data_hist_gni$year)
# data_hist_gni<-data_hist_gni[which(data_hist_gni$year<=2020),]
# 
# gnipc_mod$lag_lgnipc<-NA
# gnipc_mod$lgnipc<-NA
# 
# complete_dat<-expand.grid(year=1990:2020, Scenario=unique(gnipc_mod$Scenario), iso3=unique(data_hist_gni$iso3))
# data_hist_gni<-inner_join(data_hist_gni ,complete_dat )
# data_hist_gni$gdp_pc<-NA
# 
# common_iso<-intersect(unique(data_hist_gni$iso3), unique(gnipc_mod$iso3))
# 
# data_hist_gni<-data_hist_gni[ which(data_hist_gni$iso3 %in% common_iso), ]
# gnipc_mod<-gnipc_mod[ which(gnipc_mod$iso3 %in% common_iso), ]
# 
# gnipc_mod_all<-gnipc_mod[which(gnipc_mod$year>2020),]
# gnipc_mod_all<-rbind(gnipc_mod_all, data_hist_gni)
# 
# for (ssp in unique(gnipc_mod$Scenario)){
#   for (iso in unique(gnipc_mod$iso3)){
#     gnipc_mod_all$gdp_pc[which(gnipc_mod_all$year==2020 & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)]<-
#       gnipc_mod$gdp_pc[which(gnipc_mod_all$year==2020 & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)]
#   }
# }
# 
# gnipc_mod_all$lgdp_pc<-log(gnipc_mod_all$gdp_pc)
# 
# for (y in 2020:2100){
#   for (ssp in unique(gnipc_mod_all$Scenario)){
#     for (iso in unique(gnipc_mod_all$iso3)){
#       gnipc_mod_all$lgnipc[which(gnipc_mod_all$year==y & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)]<-
#         c(1, gnipc_mod_all$lgdp_pc[which(gnipc_mod_all$year==y & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)] ,
#           gnipc_mod_all$lag_lgnipc[which(gnipc_mod_all$year==y & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)] ) %*% c(0.03711585 , 0.22491492  ,0.77221180  )
#       
#       gnipc_mod_all$lag_lgnipc[which(gnipc_mod_all$year==(y+1) & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)]<-
#         gnipc_mod_all$lgnipc[which(gnipc_mod_all$year==y & gnipc_mod_all$iso3==iso &  gnipc_mod_all$Scenario==ssp)]
#     }
#   }
# }
# 
# 
# ggplot(gnipc_mod_all[which(gnipc_mod_all$iso3=="AUS"),], aes(x = year, y = lgdp_pc, color = Scenario)) +
#   geom_point()
