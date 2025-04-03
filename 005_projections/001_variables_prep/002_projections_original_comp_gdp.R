
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

var_name<-"gdp_pc"


population_data_interp<-read_csv(file.path(out_dir, "preproc_proj_population.csv"))

################################################################################

library(readxl)
# iiasa gdp, ppp 2005$, billion values
gdp<- read_excel("data/hdi_data/projections/IIASA_GDP.xlsx")
gdp$Variable<-NULL
gdp$Notes<-NULL
gdp$Model<-NULL
gdp$Variable<-NULL
gdp$Unit<-NULL

gdp <- data.table::melt(data.table::setDT(gdp), id.vars = c("Scenario","Region"), variable.name = "year")

gdp$year<-as.numeric(as.character(gdp$year))
colnames(gdp)[which(colnames(gdp)=="Region")]="iso3"

gdp[which(gdp$Scenario=="SSP1"), 1]="ssp126"
gdp[which(gdp$Scenario=="SSP2"), 1]="ssp245"
gdp[which(gdp$Scenario=="SSP3"), 1]="ssp370"
gdp[which(gdp$Scenario=="SSP5"), 1]="ssp585"
gdp[which(gdp$Scenario=="SSP4"), 1]="ssp4"

gdp$value<-gdp$value*1e+9

population_data_interp$pop<-population_data_interp$pop*1000

gdp_pc<-inner_join(gdp, population_data_interp)
gdp_pc$value<-gdp_pc$value/gdp_pc$pop

gdp_pc_interp<-data.frame()

# interpolate in years 
for (s in unique(gdp_pc$Scenario)){
  for (c in unique(gdp_pc$iso3)){
    
    temp<-gdp_pc[which(gdp_pc$Scenario == s & gdp_pc$iso3 == c & gdp_pc$year>=2020), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    gdp_pc_interp<-rbind(gdp_pc_interp,filler )
  }
  
}


ggplot(gdp_pc_interp[which(gdp_pc_interp$iso3=="AFG"),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_line(size = 1)


### save 
write.csv(gdp_pc_interp, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)







################################################################################
# old gdp

gdp_pc <- read_excel("data/hdi_data/projections/gdppc_ssp_data.xlsx")

### preproc gdp 

gdp_pc<-gdp_pc[, c(1:7)] # only ssp values for gdp pc 

colnames(gdp_pc)[which(colnames(gdp_pc)=="yr")]="year"
colnames(gdp_pc)[which(colnames(gdp_pc)=="ssp1_gdppc")]="ssp126"
colnames(gdp_pc)[which(colnames(gdp_pc)=="ssp2_gdppc")]="ssp245"
colnames(gdp_pc)[which(colnames(gdp_pc)=="ssp3_gdppc")]="ssp370"
colnames(gdp_pc)[which(colnames(gdp_pc)=="ssp5_gdppc")]="ssp585"
colnames(gdp_pc)[which(colnames(gdp_pc)=="ssp4_gdppc")]="ssp4"

gdp_pc<-melt(setDT(gdp_pc), id.vars = c("iso3","year"), variable.name = "Scenario")


ggplot(gdp_pc[which(gdp_pc$iso3=="BRA"),], aes(x = year, y = value, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years

gdp_pc_interp<-data.frame()

# interpolate in years 
for (s in unique(gdp_pc$Scenario)){
  for (c in unique(gdp_pc$iso3)){
    
    temp<-gdp_pc[which(gdp_pc$Scenario == s & gdp_pc$iso3 == c & gdp_pc$year>=2020), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    gdp_pc_interp<-rbind(gdp_pc_interp,filler )
  }
  
}


# add pop proj to data
gdp_pc_interp<-inner_join(gdp_pc_interp, population_data_interp)


### save 
write.csv(gdp_pc_interp, file = file.path(out_dir, paste0("preproc_proj_", var_name, ".csv")), row.names = FALSE)
