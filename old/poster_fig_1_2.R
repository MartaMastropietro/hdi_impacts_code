
################################################################################
# fig 1: variance/mean of some climatic indicator
# idesas: focus on some country? with evolution on years, mean country and gridded underneath?
#         only add coasts borders
################################################################################


rm(list=ls())

source("utils/libraries.R")
library(maps)
library(ggplot2)
load("data/climate_data/era5/climate_data_1950_2023.RData")

out_dir<-"output/figures_poster_ems_2024"
if(!dir.exists(out_dir)){dir.create(out_dir)}


rast_SPI<-all_rast$SPI
sd_rast_SPI<-raster::calc(raster::stack(rast_SPI), fun=sd)

plot(sd_rast_SPI)
#map("world", add = TRUE)

# countries<-giscoR::gisco_get_countries(spatialtype = "BN")
# countries<-countries[,c(9,10)]
# countries<-sf::st_as_sf(countries)

countries<-giscoR::gisco_get_countries(spatialtype = "COASTL")
countries<-countries[,c(9,10)]
countries<-sf::st_as_sf(countries)


sd_rast_SPI_plot<-raster::as.data.frame((sd_rast_SPI), xy=TRUE)
colnames(sd_rast_SPI_plot)[colnames(sd_rast_SPI_plot)=="layer"]<-"SPI"
extents<-raster::extent(sd_rast_SPI)

g<-ggplot()+
  geom_raster( aes(x=x, y=y, fill=SPI), data=sd_rast_SPI_plot)+
  ylab("") +
  xlab("") +
  geom_sf(data=countries, lwd=0.3) +
  coord_sf(expand = FALSE) +
  #theme(panel.grid.major = element_line(color = gray(.5)))+
  #ggtitle("Standard Deviation of SPI (drought) in 1950-2023") +
  # scale_fill_gradient2(high="#1B4D3E")
  scale_fill_gradient2(high="#FEBE10")+theme( text = element_text(size = 30))
  # scale_fill_gradient2(high="#FF3800")
  
 
ggsave(filename=file.path(out_dir, paste0("spi_sd_1950_2023.png")), g, width=20, height=15)


###


rast_WD<-all_rast$WD
sd_rast_WD<-raster::calc(raster::stack(rast_WD), fun=sd)

plot(sd_rast_WD)
#map("world", add = TRUE)

# countries<-giscoR::gisco_get_countries(spatialtype = "BN")
# countries<-countries[,c(9,10)]
# countries<-sf::st_as_sf(countries)

countries<-giscoR::gisco_get_countries(spatialtype = "COASTL")
countries<-countries[,c(9,10)]
countries<-sf::st_as_sf(countries)


sd_rast_WD_plot<-raster::as.data.frame((sd_rast_WD), xy=TRUE)
colnames(sd_rast_WD_plot)[colnames(sd_rast_WD_plot)=="layer"]<-"WD"
extents<-raster::extent(sd_rast_WD)

g<-ggplot()+
  geom_raster( aes(x=x, y=y, fill=WD), data=sd_rast_WD_plot)+
  ylab("") +
  xlab("") +
  geom_sf(data=countries, lwd=0.3) +
  coord_sf(expand = FALSE) +
  #theme(panel.grid.major = element_line(color = gray(.5)))+
  #ggtitle("Standard Deviation of WD (drought) in 1950-2023") +
  # scale_fill_gradient2(high="#1B4D3E")
  scale_fill_gradient2(high="#0000FF")+theme( text = element_text(size = 30))
# scale_fill_gradient2(high="#FF3800")

summary(sd_rast_WD_plot$WD)
ggsave(filename=file.path(out_dir, paste0("WD_sd_1950_2023.png")), g, width=20, height=15)

###


rast_PET<-all_rast$PET
sd_rast_PET<-raster::calc(raster::stack(rast_PET), fun=sd)

plot(sd_rast_PET)
#map("world", add = TRUE)

# countries<-giscoR::gisco_get_countries(spatialtype = "BN")
# countries<-countries[,c(9,10)]
# countries<-sf::st_as_sf(countries)

countries<-giscoR::gisco_get_countries(spatialtype = "COASTL")
countries<-countries[,c(9,10)]
countries<-sf::st_as_sf(countries)


sd_rast_PET_plot<-raster::as.data.frame((sd_rast_PET), xy=TRUE)
colnames(sd_rast_PET_plot)[colnames(sd_rast_PET_plot)=="layer"]<-"PET"
extents<-raster::extent(sd_rast_PET)

g<-ggplot()+
  geom_raster( aes(x=x, y=y, fill=PET), data=sd_rast_PET_plot)+
  ylab("") +
  xlab("") +
  geom_sf(data=countries, lwd=0.3) +
  coord_sf(expand = FALSE) +
  #theme(panel.grid.major = element_line(color = gray(.5)))+
  #ggtitle("Standard Deviation of PET (drought) in 1950-2023") +
  # scale_fill_gradient2(high="#1B4D3E")
  scale_fill_gradient2(high="#FF3800")+theme( text = element_text(size = 30))
# scale_fill_gradient2(high="#FF3800")


ggsave(filename=file.path(out_dir, paste0("PET_sd_1950_2023.png")), g, width=20, height=15)


# mean
mean_rast_PET<-raster::calc(raster::stack(rast_PET), fun=mean)

#map("world", add = TRUE)

# countries<-giscoR::gisco_get_countries(spatialtype = "BN")
# countries<-countries[,c(9,10)]
# countries<-sf::st_as_sf(countries)

countries<-giscoR::gisco_get_countries(spatialtype = "COASTL")
countries<-countries[,c(9,10)]
countries<-sf::st_as_sf(countries)


mean_rast_PET_plot<-raster::as.data.frame((mean_rast_PET), xy=TRUE)
colnames(mean_rast_PET_plot)[colnames(mean_rast_PET_plot)=="layer"]<-"PET"
extents<-raster::extent(mean_rast_PET)

g<-ggplot()+
  geom_raster( aes(x=x, y=y, fill=PET), data=mean_rast_PET_plot)+
  ylab("") +
  xlab("") +
  geom_sf(data=countries, lwd=0.3) +
  coord_sf(expand = FALSE) +
  #theme(panel.grid.major = element_line(color = gray(.5)))+
  #ggtitle("Standard Deviation of PET (drought) in 1950-2023") +
  # scale_fill_gradient2(high="#1B4D3E")
  scale_fill_gradient2(high="#FF3800")+theme( text = element_text(size = 30))
# scale_fill_gradient2(high="#FF3800")


ggsave(filename=file.path(out_dir, paste0("PET_mean_1950_2023.png")), g, width=20, height=15)


###
################################################################################
# fig 2: hdi in some countries, evolution in time with sub regions 
# countries: spain, india, colombia, canada
# ideas
################################################################################

rm(list=ls())

source("utils/libraries.R")
library(maps)
library(ggplot2)

out_dir<-"output/figures_poster_ems_2024"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
data_national <- read_csv("data/hdi_data/data_hdi_components_gender_national_1990_2021.csv")

data_small<-data%>%
  filter(iso3 %in% c("COL", "ESP", "CAN", "IND"))
data_national_small<-data_national%>%
  filter(iso3 %in% c("COL", "ESP", "CAN", "IND"), year %in% 1990:2020)
data_national_small$hdi<-data_national_small$hdi*100
data_national_small$edu_index<-data_national_small$edu_index*100
data_national_small$income_index<-data_national_small$income_index*100
data_national_small$health_index<-data_national_small$health_index*100

g1<-ggplot(data_small)+ theme_bw()+
  geom_line(aes(x=year, y=hdi, group=gdlcode,color=iso3), linetype = "dashed", linewidth=0.5)+theme( text = element_text(size = 28))+
  geom_line(data=data_national_small, aes(x=year, y=hdi, color=iso3), linewidth=0.8)+ylim(0,100)+scale_color_brewer(palette="Set1")
g2<-ggplot(data_small)+theme_bw()+
  geom_line(aes(x=year, y=edu_index, group=gdlcode,color=iso3), linetype = "dashed", linewidth=0.5)+theme( text = element_text(size = 28))+
  geom_line(data=data_national_small, aes(x=year, y=edu_index, color=iso3), linewidth=0.8)+ylim(0,100)+scale_color_brewer(palette="Set1")
g3<-ggplot(data_small)+theme_bw()+
  geom_line(aes(x=year, y=income_index, group=gdlcode,color=iso3), linetype = "dashed", linewidth=0.5)+theme( text = element_text(size = 28))+
  geom_line(data=data_national_small, aes(x=year, y=income_index, color=iso3), linewidth=0.8)+ylim(0,100)+scale_color_brewer(palette="Set1")
g4<-ggplot(data_small)+theme_bw()+
  geom_line(aes(x=year, y=health_index, group=gdlcode,color=iso3), linetype = "dashed", linewidth=0.5)+theme( text = element_text(size = 28))+
  geom_line(data=data_national_small, aes(x=year, y=health_index, color=iso3), linewidth=0.8)+ylim(0,100)+scale_color_brewer(palette="Set1")
ggpubr::ggarrange(g1,g2,g3,g4, common.legend=TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("hdi_components_examples.png")), width = 1000,
           height = 700)

# add geograpical limits
gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

data_small<-inner_join(data_small, gdl_shape_file)

