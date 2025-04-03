
rm(list=ls())

source("utils/libraries.R")

### data hdi, shape file

data_dir<-"data/hdi_data/downloaded/gdl_subnational"
out_dir<-"data/hdi_data"

### Sub-national shape file (hdi + components data)
gdl_shape_file <- sf:: st_read("data/data_hdi_components_gender/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")
gdl_sub_nations <- gdl_shape_file[, 1] 
gdl_sub_nations$geometry <- NULL

years<-1990:2021

data_all<-list()
for (y in years){
  
  name<-paste0("GDL-Custom-set-of-indicators-(",y,")-data.csv")
  df<-read.csv(file.path(data_dir, name))
  df$year<-y
  data_all[[length(data_all)+1]]<-df
  
}

data_hdi<-dplyr::bind_rows(data_all)

data<-data_hdi[, c(3,5,7:22,24)]
colnames(data)<-c("iso3", "gdlcode", "hdi", "health_index","edu_index","income_index","gdi",
                  "hdi_fem","hdi_male", "health_index_fem","health_index_male", "edu_index_fem","edu_index_male", "income_index_fem","income_index_male",
                  "log_gni_pc","log_gni_pc_fem", "log_gni_pc_male", "year" )

gdl_iso<-unique(data[, c(1,2)])
gdl_years<-expand.grid(gdlcode=gdl_sub_nations[,1], year=years)
gdl_years <- dplyr::inner_join(gdl_years,gdl_iso)

# make each couple year, gdlcode appear
data<- dplyr::left_join(gdl_years, data)

# overall incidence na 
(sum(is.na(data$hdi))/dim(data)[1])*100 # 
(sum(is.na(data$hdi_fem))/dim(data)[1])*100 #
(sum(is.na(data$hdi_male))/dim(data)[1])*100 # 

(sum(is.na(data$income_index))/dim(data)[1])*100 # 
(sum(is.na(data$income_index_fem))/dim(data)[1])*100 # 
(sum(is.na(data$income_index_male))/dim(data)[1])*100 # 

(sum(is.na(data$health_index))/dim(data)[1])*100 # 
(sum(is.na(data$health_index_fem))/dim(data)[1])*100 # 
(sum(is.na(data$health_index_male))/dim(data)[1])*100 # 

(sum(is.na(data$edu_index))/dim(data)[1])*100 # 
(sum(is.na(data$edu_index_fem))/dim(data)[1])*100 # 
(sum(is.na(data$edu_index_male))/dim(data)[1])*100 # 

(sum(is.na(data$log_gni_pc))/dim(data)[1])*100 # 
(sum(is.na(data$log_gni_pc_fem))/dim(data)[1])*100 # 
(sum(is.na(data$log_gni_pc_male))/dim(data)[1])*100 # 

# in 1999-2021 4%, 12% for gender divided
# in 1990-2021 7%, 36% for gender divided 


### save 
write.csv(data, file=file.path(out_dir, "data_hdi_components_gender_subnat_1990_2021.csv"), row.names = FALSE)


################################################################################

### data only national
data_nat<- data_hdi[which(data_hdi$Level=="National"), c(3,5,7:22,24)]
colnames(data_nat)<-c("iso3", "gdlcode", "hdi", "health_index","edu_index","income_index","gdi",
                  "hdi_fem","hdi_male", "health_index_fem","health_index_male", "edu_index_fem","edu_index_male", "income_index_fem","income_index_male",
                  "log_gni_pc","log_gni_pc_fem", "log_gni_pc_male", "year" )
length(unique(data_nat$iso3)) #187 countries

### save 
write.csv(data_nat, file=file.path(out_dir, "data_hdi_components_gender_national_1990_2021.csv"), row.names = FALSE)

###

### compare with original undp data national
data_un<- read.csv("data/hdi_data/downloaded/undp_national/HDR21-22_Composite_indices_complete_time_series.csv")

data_un<-data_un[-c(196:206), ] #keep countries only
length(unique(data_un$iso3)) #195 countries

data_un_hdi<- data_un[, c(1,3,6:37)]
data_un_le<- data_un[, c(1,3,38:69)]
data_un_eys<- data_un[, c(1,3,70:101)]
data_un_mys<- data_un[, c(1,3,102:133)]
data_un_gnipc<- data_un[, c(1,3,134:165)]
data_un_gdi<- data_un[, c(1,3,167:198)]

colnames(data_un_hdi)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                                            "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                                            "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                                            "2020", "2021")
colnames(data_un_le)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                         "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                         "2020", "2021")
colnames(data_un_eys)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                         "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                         "2020", "2021")
colnames(data_un_mys)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                        "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                        "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                        "2020", "2021")
colnames(data_un_gnipc)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                         "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                         "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                         "2020", "2021")
colnames(data_un_gdi)<-c("iso3", "hdicode", "1990", "1991", "1992", "1993", "1994", "1995", "1996","1997", "1998", "1999",
                        "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009", 
                        "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                        "2020", "2021")

data_un_hdi<-reshape2::melt(data_un_hdi, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_hdi)[colnames(data_un_hdi)=="value"]<-"hdi"
data_un_hdi$year<-as.numeric(as.character(data_un_hdi$year))

data_un_le<-reshape2::melt(data_un_le, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_le)[colnames(data_un_le)=="value"]<-"le"
data_un_le$year<-as.numeric(as.character(data_un_le$year))

data_un_eys<-reshape2::melt(data_un_eys, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_eys)[colnames(data_un_eys)=="value"]<-"eys"
data_un_eys$year<-as.numeric(as.character(data_un_eys$year))

data_un_mys<-reshape2::melt(data_un_mys, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_mys)[colnames(data_un_mys)=="value"]<-"mys"
data_un_mys$year<-as.numeric(as.character(data_un_mys$year))

data_un_gnipc<-reshape2::melt(data_un_gnipc, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_gnipc)[colnames(data_un_gnipc)=="value"]<-"gnipc"
data_un_gnipc$year<-as.numeric(as.character(data_un_gnipc$year))

data_un_gdi<-reshape2::melt(data_un_gdi, id.vars = c("iso3", "hdicode"), variable.name = "year")
colnames(data_un_gdi)[colnames(data_un_gdi)=="value"]<-"gdi"
data_un_gdi$year<-as.numeric(as.character(data_un_gdi$year))

data_final<-dplyr::inner_join(data_un_hdi,data_un_le )
data_final<-dplyr::inner_join(data_final,data_un_eys )
data_final<-dplyr::inner_join(data_final,data_un_mys )
data_final<-dplyr::inner_join(data_final,data_un_gnipc )
data_final<-dplyr::inner_join(data_final,data_un_gdi )

### save 
write.csv(data_final, file=file.path(out_dir, "data_hdi_components_undp_national_1990_2021.csv"), row.names = FALSE)


################################################################################
################################################################################
################################################################################

### data of each original component 
rm(list=ls())

source("utils/libraries.R")

data <- read_csv("data/hdi_data/downloaded/gdl_subnational_original_variables/GDL-Expected-years-schooling-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code", "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level","gdlcode", "year", "eys")
data_eys<-data
data_eys$leys<-log(data_eys$eys)

data <- read_csv("data/hdi_data/downloaded/gdl_subnational_original_variables/GDL-Mean-years-schooling-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code", "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level","gdlcode", "year", "mys")
data_mys<-data
data_mys$lmys<-log(data_mys$mys)


data <-read_csv("data/hdi_data/downloaded/gdl_subnational_original_variables/GDL-Log-Gross-National-Income-per-capita-in-1000-US-Dollars-(2011-PPP)-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code","Level", "GDLCODE", "year", "value")]
colnames(data)<- c("iso3","Level", "gdlcode", "year", "lgnipc")
data_lgnipc<-data

data <- read_csv("data/hdi_data/downloaded/gdl_subnational_original_variables/GDL-Life-expectancy-data.csv")
data<-reshape2::melt(data, id.vars = c("Country", "Continent", "ISO_Code",  "Level","GDLCODE", "Region" ), variable.name = "year")
data<-data[, c("ISO_Code",  "Level","GDLCODE", "year", "value")]
colnames(data)<- c("iso3", "Level", "gdlcode", "year", "leb")
data_leb<-data
data_leb$lleb<-log(data_leb$leb)


data<-inner_join(data_eys,data_mys)
data<-inner_join(data,data_lgnipc)
data<-inner_join(data,data_leb)

data_na<-unique(data%>%group_by(gdlcode)%>%mutate(na_mys=sum(is.na(mys)), 
                                  na_eys=sum(is.na(eys)), 
                                  na_leb=sum(is.na(leb)), 
                                  na_gni=sum(is.na(lgnipc)) ) %>% select(iso3,gdlcode, na_mys, na_eys, na_leb, na_gni))
hist(data_na$na_mys)
hist(data_na$na_eys)
hist(data_na$na_leb)
hist(data_na$na_gni)

too_much_na_codes<-unique(data_na$gdlcode[which(data_na$na_mys>=15 |data_na$na_eys>=15 |data_na$na_gni>=15 |data_na$na_leb>=15 )])

data_less_na<-data[-which(data$gdlcode %in% too_much_na_codes), ]

### save 
out_dir<-"data/hdi_data"
write.csv(data, file=file.path(out_dir, "data_hdi_original_components_1990_2022.csv"), row.names = FALSE)
write.csv(data_less_na, file=file.path(out_dir, "data_hdi_original_components_1990_2022_less_na.csv"), row.names = FALSE)

