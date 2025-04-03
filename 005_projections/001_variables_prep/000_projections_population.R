### preprocess population

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



population_data<-read_delim("data/hdi_data/projections/population_size_000s_all_ssp_2020_2100.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

####################

# add iso
colnames(population_data)[which(colnames(population_data)=="Area")]="NAME_ENGL"
names<-unique(population_data$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names, names_countries)

population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Bolivia (Plurinational State of)", "Bolivia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Brunei Darussalam", "Brunei")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Cote d'Ivoire", "Côte D’Ivoire")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Czech Republic", "Czechia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Democratic People's Republic of Korea", "North Korea")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Democratic Republic of the Congo", "Democratic Republic of The Congo")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Hong Kong Special Administrative Region of China", "Hong Kong")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Iran (Islamic Republic of)", "Iran")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Lao People's Democratic Republic", "Laos")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Libyan Arab Jamahiriya", "Libya")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Macao Special Administrative Region of China", "Macau")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Micronesia (Federated States of)", "Micronesia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Myanmar", "Myanmar/Burma")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Occupied Palestinian Territory", "Palestine")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Republic of Korea", "South Korea")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Republic of Moldova", "Moldova")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Saint Vincent and the Grenadines", "Saint Vincent and The Grenadines")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Sao Tome and Principe", "São Tomé and Príncipe")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Swaziland", "Eswatini")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Syrian Arab Republic", "Syria")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Taiwan Province of China", "Taiwan")  # if you consider Taiwan separate
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "The former Yugoslav Republic of Macedonia", "North Macedonia")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United States Virgin Islands", "Us Virgin Islands")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "United States of America", "United States")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Venezuela (Bolivarian Republic of)", "Venezuela")
population_data$NAME_ENGL <- stringr::str_replace(population_data$NAME_ENGL, "Viet Nam", "Vietnam")
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Venezuela (Bolivarian Republic of)")]<-"Venezuela"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Bolivia (Plurinational State of)")]<-"Bolivia"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Micronesia (Federated States of)")]<-"Micronesia"
population_data$NAME_ENGL[which(population_data$NAME_ENGL=="Iran (Islamic Republic of)")]<-"Iran"
names<-unique(population_data$NAME_ENGL)
names_countries<-unique(countries$NAME_ENGL)
setdiff(names, names_countries)
setdiff(names_countries, names)

population_data<-inner_join(population_data , countries)
colnames(population_data)[which(colnames(population_data)=="ISO3_CODE")]="iso3"


# assumption: time frames refer to last year

population_data$year<-as.numeric(population_data$Year)
population_data$Year<-NULL

colnames(population_data)[which(colnames(population_data)=="Population")]="value"


ggplot(population_data[which(population_data$iso3=="BRA"),], aes(x = year, y = value, color = Scenario)) +
  geom_line(size = 1)

# interpolate in years
population_data<-population_data[, c("Scenario", "iso3", "year", "value")]
population_data_interp<-data.frame()

# interpolate in years 
for (s in unique(population_data$Scenario)){
  for (c in unique(population_data$iso3)){
    
    temp<-population_data[which(population_data$Scenario == s & population_data$iso3 == c  ), ]
    filler<-expand.grid(year=seq( 2020, 2100 , by=1 ))
    
    mod<-smooth.spline(temp$year, temp$value, all.knots = TRUE)
    pred<-predict(mod, filler)$y
    
    filler$value_interp<-as.numeric(pred[,1])
    filler$iso3<-c
    filler$Scenario<-s
    
    population_data_interp<-rbind(population_data_interp,filler )
  }
  
}

population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP1")]<-"ssp126" # or another, should duplicate 
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP2")]<-"ssp245"
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP3")]<-"ssp370"
population_data_interp$Scenario[which(population_data_interp$Scenario=="SSP5")]<-"ssp585" # or another, should duplicate 

ggplot(population_data_interp[which(population_data_interp$iso3=="BRA"),], aes(x = year, y = value_interp, color = Scenario)) +
  geom_point()

colnames(population_data_interp)[which(colnames(population_data_interp)=="value_interp")]<-"pop"


### save 
write.csv(population_data_interp, file = file.path(out_dir, paste0("preproc_proj_population.csv")), row.names = FALSE)



##############################################################################
##############################################################################
##############################################################################


### compute subnational future pop values aggregating isimip

library(raster)
library(terra)
library(dplyr)
library(st)
library(sf)
library(ncdf4)

### Countries borders , to test aggregations
countries<-giscoR::gisco_get_countries()
#simplify data countries
countries<-countries[,c(3,6)]
countries<-sf::st_as_sf(countries)
# fix crs
countries<-sf::st_transform(countries, crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
crs(countries)
colnames(countries)[which(colnames(countries)=="ISO3_CODE")]<-"iso3"

### Sub-national shape file (hdi+components data)
gdl_shape_file <- sf::st_read("data/hdi_data/downloaded/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
# fix crs
gdl_shape_file<-sf::st_transform(gdl_shape_file, crs ="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
gdl_sub_nations<-gdl_shape_file[, c(1,4)] 
crs(gdl_sub_nations)
#plot(gdl_sub_nations)

print(countries$geometry[countries$iso3=="AFG"])
print(gdl_shape_file$geometry[gdl_shape_file$iso_code=="AFG"])

library(readr)
gdlcodes_iso <- read_csv("data/gdlcodes_iso.csv")

data_dir<-"output/projections/original_comp"
pop_dir<-"data/data_population/isimip_pop_grid_05_proj"

ssp_names<-c("ssp126", "ssp119", "ssp245", "ssp370", "ssp585")
ssp_names_pop<-c("ssp1", "ssp1", "ssp2", "ssp3", "ssp5")

years<-2015:2100 

# for ssps climate correspondance


data_ssp_subnat<-data.frame()
data_ssp_nat<-data.frame()

# loop in ssp names pop
for (n in 1:length(ssp_names)){
  
  # pop load 
  pop<-ncdf4::nc_open(file.path(pop_dir, paste0("population_",ssp_names_pop[n],"soc_0p5deg_annual_2006-2100.nc4")))
  var<-"number_of_people"
  lon <- ncvar_get(pop, "lon")
  lat <- ncvar_get(pop, "lat", verbose = F)
  var.array <- ncvar_get(pop, var) # store the data in a 2-dimensional array
  fillvalue <- ncatt_get(pop, var, "_FillValue")
  var.array[which(var.array == fillvalue$value)] <- NA
  
  
  
  ### agg by mean
  for (year in years){
    
    # pop
    i<-year - 2005 #we want to start from 2015, but pop is from 2006 so we start from index 10 of array
    r <- raster(t(var.array[,,i]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs="EPSG:4326")
    
    # extraction subnat
    agg_subnat = exactextractr::exact_extract(r, gdl_sub_nations, fun = "sum")
    agg_subnat<-data.frame(agg_subnat)
    agg_subnat$gdlcode<-gdl_sub_nations$gdlcode
    agg_subnat$ssp<-ssp_names[n]
    colnames(agg_subnat)<-c(  "value", "gdlcode","ssp" )
    agg_subnat$year<-year
    
    data_ssp_subnat<-rbind(data_ssp_subnat, agg_subnat)
    
    # extraction nat
    agg_nat = exactextractr::exact_extract(r, countries, fun = "sum")
    agg_nat<-data.frame(agg_nat)
    agg_nat$iso3<-countries$iso3
    agg_nat$ssp<-ssp_names[n]
    colnames(agg_nat)<-c(  "value", "iso3","ssp" )
    agg_nat$year<-year
    
    data_ssp_nat<-rbind(data_ssp_nat, agg_nat)
    
  }
  
}

### save dataframes 
write.csv(data_ssp_nat, file = file.path(data_dir,paste0(  "pop_proj_iso3_sum_2015_2100.csv")), row.names = FALSE)
write.csv(data_ssp_subnat, file = file.path(data_dir,paste0(  "pop_projection_gdlcode_sum_2015_2100.csv")), row.names = FALSE)

pop_proj_country<-read_csv(file.path("output/projections/original_comp", paste0("preproc_proj_population.csv")))
colnames(pop_proj_country)[which(colnames(pop_proj_country)=="pop")]<-"pop_c"
colnames(pop_proj_country)[which(colnames(pop_proj_country)=="Scenario")]<-"ssp"
pop_proj_country$pop_c<-pop_proj_country$pop_c*1000

colnames(data_ssp_nat)[which(colnames(data_ssp_nat)=="value")]<-"pop_c_ind"
colnames(data_ssp_subnat)[which(colnames(data_ssp_subnat)=="value")]<-"pop_sub"
data_ssp_subnat<-inner_join(data_ssp_subnat, gdlcodes_iso)
data_ssp_subnat<-data_ssp_subnat%>%
  group_by(iso3, year, ssp)%>%
  transmute(pop_c_sum=sum(pop_sub))
data_ssp_subnat<-unique(data_ssp_subnat)

all_pop<-inner_join(pop_proj_country ,data_ssp_nat )
all_pop<-inner_join(all_pop ,data_ssp_subnat )

# check values
x11()
plot(all_pop$pop_c_ind , all_pop$pop_c )
abline(a=1,b=1)

x11()
plot( all_pop$pop_c_sum , all_pop$pop_c , col=as.factor(all_pop$iso3))
abline(a=1,b=1)

x11()
ggplot(all_pop, aes(x=pop_c_sum, y=pop_c, color=iso3))+ geom_point()


x11()
ggplot(all_pop[which(all_pop$iso3 %in% c("IND", "CHN", "NGA")),], aes(x=pop_c_sum, y=pop_c, color=iso3, shape=ssp, size=year))+ geom_point()


x11()
plot(all_pop$pop_c_ind , all_pop$pop_c_sum ) # practically the same :)
abline(a=1,b=1)
cor(all_pop$pop_c_ind , all_pop$pop_c_sum) # practically the same :)
