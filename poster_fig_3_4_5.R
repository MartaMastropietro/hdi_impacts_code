### poster figures 3,4,5


rm(list=ls())

gdl_shape_file <- sf:: st_read("data/hdi_data/downloaded/gdl_subnational/GDL Shapefiles V6.2/GDL Shapefiles V6.2 large.shp")

gdl_shape_file<-gdl_shape_file[,c(1,4)]
gdl_shape_file<-sf::st_as_sf(gdl_shape_file)

# xlsx write
require(openxlsx)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)
library(fixest)
library(modelsummary)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)


out_dir<-"output/figures_poster_ems_2024"
if(!dir.exists(out_dir)){dir.create(out_dir)}

data<- read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")

### vcov: regional, dk, iso3
fvcov_dk<-function(x) vcov(x, "DK")
fvcov_iso<-function(x) vcov(x, cluster~iso3)


pan_id<-c('gdlcode', 'year')

summary(data$PET)
################################################################################

### plot in maps, complete model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


coefs<-m$coefficients

# fill data
mean_damages<- data.frame(gdlcode=unique(data$gdlcode), 
                          spi_mean_damages=NA, 
                          wd_mean_damages=NA,
                          pet_mean_damaeges=NA,
                          all_mean_damage=NA
                          
)

for (r in unique(data$gdlcode)){
  
  laghdi<-mean(data$lag_log_gni_pc[data$gdlcode == r], na.rm = TRUE)
  
  ### mean on years
  
  spi<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)
  pet<-mean(data$PET[data$gdlcode == r], na.rm = TRUE)
  
  spi_plus1sd<-mean(data$SPI[data$gdlcode == r], na.rm = TRUE)+sd(data$SPI[data$gdlcode == r], na.rm = TRUE)
  wd_plus1sd<-mean(data$WD[data$gdlcode == r], na.rm = TRUE)+sd(data$WD[data$gdlcode == r], na.rm = TRUE)
  pet_plus1sd<-mean(data$PET[data$gdlcode == r], na.rm = TRUE)+sd(data$PET[data$gdlcode == r], na.rm = TRUE)
  
  
  # wd damages
  mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]<-
    ((wd_plus1sd^2)*coefs["WD_2"] +
       (wd_plus1sd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] ) - 
    ((wd^2)*coefs["WD_2"] +
       (wd^2)*laghdi*coefs["WD_2:lag_log_gni_pc"] )
  
  
  # spi damages
  mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]<-
    (spi_plus1sd*coefs["SPI"] + (spi_plus1sd)*laghdi*coefs["SPI:lag_log_gni_pc"] ) - 
    (spi*coefs["SPI"]+ (spi)*laghdi*coefs["SPI:lag_log_gni_pc"] )
  
  # pet damages
  mean_damages[mean_damages$gdlcode==r, c("pet_mean_damage") ]<-
    (pet_plus1sd*coefs["PET"] + (pet_plus1sd^2)*coefs["PET_2"] +
       pet_plus1sd*laghdi*coefs["PET:lag_log_gni_pc"] +(pet_plus1sd^2)*laghdi*coefs["PET_2:lag_log_gni_pc"]) -
    (pet*coefs["PET"] + (pet^2)*coefs["PET_2"] +
       pet*laghdi*coefs["PET:lag_log_gni_pc"] +(pet^2)*laghdi*coefs["PET_2:lag_log_gni_pc"]) 
  
  
  # sum all
  mean_damages[mean_damages$gdlcode==r, c("all_mean_damage") ]<-
    mean_damages[mean_damages$gdlcode==r, c("wd_mean_damage") ]+
    mean_damages[mean_damages$gdlcode==r, c("spi_mean_damage") ]+
    mean_damages[mean_damages$gdlcode==r, c("pet_mean_damage") ]
  
  
}

mean_damages<-inner_join(gdl_shape_file, mean_damages)

g1<-ggplot(mean_damages)+geom_sf( aes(fill=wd_mean_damage))+theme_bw()+  scale_fill_gradient2() + 
  labs(fill="HDI change (%)") + ggtitle("Marginal effect of WD") + theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
#g1
ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_mean_plus1sd_damages.png")), g1, width=15, height=12)



g2<-ggplot(mean_damages)+geom_sf( aes(fill=pet_mean_damage))+theme_bw()+ scale_fill_gradient2() +
  labs(fill="HDI change (%)")+ ggtitle("Marginal effect of PET")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_burke_pet_mean_plus1sd_damages.png")), g2, width=15, height=12)


g3<-ggplot(mean_damages)+geom_sf( aes(fill=spi_mean_damage))+theme_bw() + scale_fill_gradient2() +
  labs(fill="HDI change (%)") + ggtitle("Marginal effect of SPI")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_burke_spi_mean_plus1sd_damages.png")), g3, width=15, height=12)

g4<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damage))+theme_bw() + scale_fill_gradient2() +
  labs(fill="HDI change (%)") + ggtitle("Sum of marginal effects for one sd increase of WD, SPI, PET")+ theme(plot.title = element_text(hjust = 0.5), text = element_text(size = 24))
ggsave(filename=file.path(out_dir, paste0(o,"_burke_all_mean_plus1sd_damages.png")), g4, width=15, height=12)


ggpubr::ggarrange(g1,g2,g3, ncol=1, nrow=3,  common.legend=TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("diff_hdi_burke_extremes_mean_plus1sd_damages.png")), width =700,
                   height = 2100)
# g<-ggplot(mean_damages)+geom_sf( aes(fill=all_mean_damage))+ scale_fill_gradient2()
# ggsave(filename=file.path(out_dir, paste0(o,"_burke_wd_spi_mod_mean_damages_all_adap_hdi_mean.png")), g, width=15, height=10)


################################################################################

### plot coefficients marginal effects like kotz 2022

data<-data%>%group_by(gdlcode)%>%mutate(TM_sd=sd(TM), 
                                        RR_sd=sd(RR),
                                        WD_sd=sd(WD),
                                        SPI_sd=sd(SPI),
                                        PET_sd=sd(PET)
)

mean_sd_TM<-mean(unique(data$TM_sd))
mean_sd_RR<-mean(unique(data$RR_sd))
mean_sd_WD<-mean(unique(data$WD_sd))
mean_sd_SPI<-mean(unique(data$SPI_sd))
mean_sd_PET<-mean(unique(data$PET_sd))

### complete model

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "  TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


######## plot, with conf intervals

lgnipc_values <- as.numeric(c(summary(data$lag_log_gni_pc, na.rm = TRUE)[2],summary(data$lag_log_gni_pc, na.rm = TRUE)[3],summary(data$lag_log_gni_pc, na.rm = TRUE)[5]) )
exp(lgnipc_values)

### pet
names<-c("PET", "PET:lag_log_gni_pc", "PET_2", "PET_2:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$PET,  na.rm = TRUE), max(data$PET, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                # TM
           values * lgnipc_values[1],  # TM * l1_lgdppc
           values^2,              # TM_2
           (values^2) * lgnipc_values[1]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(values,                # TM
             values * lgnipc_values[2],  # TM * l1_lgdppc
             values^2,              # TM_2
             (values^2) * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(values,                # TM
             values * lgnipc_values[3],  # TM * l1_lgdppc
             values^2,              # TM_2
             (values^2) * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_pet<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+theme( text = element_text(size = 34))+
  xlab("Potential Evapotranspiration (PET)")+ylab("HDI change (%)")+scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")



### wd 
names<-c( "WD_2", "WD_2:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$WD, na.rm = TRUE), max(data$WD, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(
           values^2,              # TM_2
           (values^2) * lgnipc_values[1]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(
             values^2,              # TM_2
             (values^2) * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(
             values^2,              # TM_2
             (values^2) * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_wd<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+theme( text = element_text(size = 34))+
  xlab("Wet Days (WD)")+ylab("HDI change (%)")+scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")

# ### wd 
# names<-c("WD", "WD:lag_log_gni_pc", "WD_2", "WD_2:lag_log_gni_pc")
# coefficients <- coef(m)[names] # t coefs
# cov_matrix <- vcov(m, )[names,names] # t cov
# 
# # Generate a sequence of temperature values
# values <- seq(min(data$WD, na.rm = TRUE), max(data$WD, na.rm = TRUE), length.out = 1000)
# 
# # Create a matrix for the independent variables
# X <- cbind(values,                # TM
#            values * lgnipc_values[1],  # TM * l1_lgdppc
#            values^2,              # TM_2
#            (values^2) * lgnipc_values[1]) # TM_2 * l1_lgdppc
# 
# # Column names must match the coefficients' names
# colnames(X) <- names
# 
# predicted_change <- X %*% coefficients
# 
# # Calculate the standard errors of the predicted values
# standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))
# 
# # Calculate the 95% confidence intervals
# lower_ci <- (predicted_change - 1.96 * standard_errors)
# upper_ci <- (predicted_change + 1.96 * standard_errors)
# 
# # Create a matrix for the independent variables
# X_1 <- cbind(values,                # TM
#              values * lgnipc_values[2],  # TM * l1_lgdppc
#              values^2,              # TM_2
#              (values^2) * lgnipc_values[2]) # TM_2 * l1_lgdppc
# 
# # Column names must match the coefficients' names
# colnames(X_1) <- names
# 
# predicted_change_1 <- X_1 %*% coefficients
# 
# # Calculate the standard errors of the predicted values
# standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))
# 
# # Calculate the 95% confidence intervals
# lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
# upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)
# 
# # Create a matrix for the independent variables
# X_2 <- cbind(values,                # TM
#              values * lgnipc_values[3],  # TM * l1_lgdppc
#              values^2,              # TM_2
#              (values^2) * lgnipc_values[3]) # TM_2 * l1_lgdppc
# 
# # Column names must match the coefficients' names
# colnames(X_2) <- names
# 
# predicted_change_2 <- X_2 %*% coefficients
# 
# # Calculate the standard errors of the predicted values
# standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))
# 
# # Calculate the 95% confidence intervals
# lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
# upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)
# 
# 
# damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
#                     predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
#                     predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )
# 
# g_wd<-ggplot(damages)+
#   geom_line(aes(x=values, y=predicted_change))+
#   geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
#   geom_line(aes(x=values, y=predicted_change_1))+
#   geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
#   geom_line(aes(x=values, y=predicted_change_2))+
#   geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
#   theme_bw()+theme( text = element_text(size =30))+
#   xlab("Wet Days (WD)")+ylab("HDI change (%)")+scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")




### spi
names<-c("SPI", "SPI:lag_log_gni_pc")
coefficients <- coef(m)[names] # t coefs
cov_matrix <- vcov(m, )[names,names] # t cov

# Generate a sequence of temperature values
values <- seq(min(data$SPI, na.rm = TRUE), max(data$SPI, na.rm = TRUE), length.out = 1000)

# Create a matrix for the independent variables
X <- cbind(values,                #
           values * lgnipc_values[1]) # 

# Column names must match the coefficients' names
colnames(X) <- names

predicted_change <- X %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors <- sqrt(diag(X %*% cov_matrix %*% t(X)))

# Calculate the 95% confidence intervals
lower_ci <- (predicted_change - 1.96 * standard_errors)
upper_ci <- (predicted_change + 1.96 * standard_errors)

# Create a matrix for the independent variables
X_1 <- cbind(values,                # TM
             values * lgnipc_values[2]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_1) <- names

predicted_change_1 <- X_1 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_1 <- sqrt(diag(X_1 %*% cov_matrix %*% t(X_1)))

# Calculate the 95% confidence intervals
lower_ci_1 <- (predicted_change_1 - 1.96 * standard_errors_1)
upper_ci_1 <- (predicted_change_1 + 1.96 * standard_errors_1)

# Create a matrix for the independent variables
X_2 <- cbind(values,                # TM
             values * lgnipc_values[3]) # TM_2 * l1_lgdppc

# Column names must match the coefficients' names
colnames(X_2) <- names

predicted_change_2 <- X_2 %*% coefficients

# Calculate the standard errors of the predicted values
standard_errors_2 <- sqrt(diag(X_2 %*% cov_matrix %*% t(X_2)))

# Calculate the 95% confidence intervals
lower_ci_2 <- (predicted_change_2 - 1.96 * standard_errors_2)
upper_ci_2 <- (predicted_change_2 + 1.96 * standard_errors_2)


damages<-data.frame(values=values,predicted_change=predicted_change,lower_ci=lower_ci, upper_ci=upper_ci , 
                    predicted_change_1=predicted_change_1,lower_ci_1=lower_ci_1, upper_ci_1=upper_ci_1 ,
                    predicted_change_2=predicted_change_2,lower_ci_2=lower_ci_2, upper_ci_2=upper_ci_2 )

g_spi<-ggplot(damages)+
  geom_line(aes(x=values, y=predicted_change))+
  geom_ribbon(aes(x=values, ymin=lower_ci, ymax=upper_ci, fill='Low \n(around 2898$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_1))+
  geom_ribbon(aes(x=values, ymin=lower_ci_1, ymax=upper_ci_1, fill='Median \n(around 8010$)'), alpha=0.2)+
  geom_line(aes(x=values, y=predicted_change_2))+
  geom_ribbon(aes(x=values, ymin=lower_ci_2, ymax=upper_ci_2, fill='High \n(around 18434$)'), alpha=0.2)+
  theme_bw()+theme( text = element_text(size = 34))+
  xlab("Drought Index (SPI)")+ylab("HDI change (%)")+
  #labs(fill = "Income Per \nCapita Level")
  scale_fill_brewer(name="Income Per \nCapita Level", breaks=c('Low \n(around 2898$)', 'Median \n(around 8010$)', 'High \n(around 18434$)'), palette="Set1")

ggpubr::ggarrange(g_wd, g_pet, g_spi, nrow=1, ncol=3, common.legend = TRUE, legend="right") %>%
  ggpubr::ggexport(filename = file.path(out_dir,paste0("diff_hdi_burke_extremes_by_income_damages_no_wd1.png")), width = 1800,
                   height = 500)


#library(RColorBrewer)
#par(mar=c(3,4,2,2))
#display.brewer.all()


################################################################################
### only some to highlight difference adap/no adap 

models<-list()

# tp
o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2 +lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR+ lag_log_gni_pc:RR_2 "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# o <- "diff_hdi"
# i <- "gdlcode + year + iso3[year] +iso3[year^2]"
# r <- "diff_TM + TM:diff_TM + diff_RR +diff_RR:RR+lag_diff_TM + lag_TM:lag_diff_TM + lag_diff_RR +lag_diff_RR:lag_RR +lag_log_gni_pc:diff_TM  + lag_log_gni_pc:diff_RR "
# f <- as.formula(paste( o, "~", r, "|" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# models[[length(models)+1]]<-m
# summary(m)
# summary(m, vcov = ~ iso3)
# summary(m, vcov = "DK")


# extremes 

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI + PET + PET_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# tp + extremes


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "  TM + TM_2 + RR + RR_2+  WD + WD_2 + SPI + PET + PET_2 " # also here if we add adap to spi it becomes insignificant 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI " # 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models[[length(models)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

# o <- "diff_hdi"
# i <- "gdlcode + year + iso3[year] +iso3[year^2]"
# r <- " TM + TM_2 + RR + RR_2 + WD + WD_2 + SPI  +
#       lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2  +
#       lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  " # also here if we add adap to spi it becomes insignificant 
# f <- as.formula(paste( o, "~", r, "|" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# models[[length(models)+1]]<-m
# summary(m)
# summary(m, vcov = ~ iso3)
# summary(m, vcov = "DK")
# 
# o <- "diff_hdi"
# i <- "gdlcode + year + iso3[year] +iso3[year^2]"
# r <- " TM + TM_2 + RR + RR_2 + PET + PET_2 + SPI  +
#       lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2  +
#       lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 + lag_log_gni_pc:SPI" # also here if we add adap to spi it becomes insignificant 
# f <- as.formula(paste( o, "~", r, "|" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# models[[length(models)+1]]<-m
# summary(m)
# summary(m, vcov = ~ iso3)
# summary(m, vcov = "DK")

# save

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models, vcov=lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes.html")))
modelsummary(models,lapply(models, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_dk.html")))

### plot different aic, bic, rsq

names(models)<-c("bhm", "bhm_adap", "extremes", "extremes_adap", "bhm_extremes", "bhm_extremes_adap")

to_plot<-data.frame(model=names(models), Rsq=NA, BIC=NA, AIC=NA)

for (m in names(models)){
  mod<-models[[m]]
  to_plot[which(to_plot$model==m), 2]<-r2(mod,type='r2')
  to_plot[which(to_plot$model==m), 3]<-AIC(mod)
  to_plot[which(to_plot$model==m), 4]<-BIC(mod)
}

g<-ggplot(data=to_plot, aes(x=AIC, y=BIC, size=Rsq, color=model, shape=model)) +
  geom_point(alpha=0.5)+theme_bw()+ scale_size_continuous(range = c(10, 15))+
  scale_y_reverse()+scale_x_reverse()+ scale_color_brewer(palette="Set1")+theme(text=element_text(size = 50))

ggsave(filename=file.path(out_dir, paste0("model_comparison.png")), g, width=12, height=10)


##########################################################################

### compare components

models_comp<-list()

o <- "diff_hdi"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- " TM + TM_2 + RR +RR_2+ WD + WD_2 + SPI +PET + PET_2 +
      lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  + lag_log_gni_pc:RR_2+
      lag_log_gni_pc:WD + lag_log_gni_pc:WD_2  +
      lag_log_gni_pc:PET + lag_log_gni_pc:PET_2 +lag_log_gni_pc:SPI "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_income_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_edu_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")


o <- "diff_health_index"
i <- "gdlcode + year + iso3[year] +iso3[year^2]"
r <- "TM + TM_2 + RR+ RR_2+WD + WD_2 +SPI + PET+ PET_2+lag_log_gni_pc:TM + lag_log_gni_pc:TM_2 + lag_log_gni_pc:RR  +lag_log_gni_pc:RR_2+ lag_log_gni_pc:WD  + lag_log_gni_pc:WD_2+ lag_log_gni_pc:SPI+ lag_log_gni_pc:PET+ lag_log_gni_pc:PET_2    "
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
models_comp[[length(models_comp)+1]]<-m
summary(m)
summary(m, vcov = ~ iso3)
summary(m, vcov = "DK")

names(models_comp)<-c("diff_hdi","diff_income_index", "diff_edu_index",   "diff_health_index")

modelsummary(models_comp, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))
modelsummary(models_comp, vcov=lapply(models_comp, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"))

modelsummary(models_comp, estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_all_comp.html")))
modelsummary(models_comp,lapply(models_comp, FUN=fvcov_dk), estimate = "{estimate}{stars}", statistic = NULL, gof_omit = c("Std.Errors"), fmt = fmt_sprintf("%.5e"), 
             output=file.path(out_dir, paste0("models_burke_extremes_all_comp_dk.html")))


jtools::plot_summs(models_comp, omit.coefs = c("TM", "TM_2", "RR", "RR_2", "TM:lag_log_gni_pc", "TM_2:lag_log_gni_pc", "RR:lag_log_gni_pc", "RR_2:lag_log_gni_pc"),scale = TRUE, robust = TRUE)

library(ggplot2)
library(dplyr)



###
library(dplyr)

coef_data <- lapply(models_comp, function(model) {
  coef_df <- as.data.frame(coefficients(model))
  confint_df <- confint(model,vcov = "DK")
  #confint_df <- confint(model)
  
  # Merge coefficients and confidence intervals
  coef_df <- coef_df %>%
    mutate(variable = rownames(coef_df)) %>%
    mutate(lower = confint_df[, 1], upper = confint_df[, 2])
  names(coef_df)[names(coef_df) == "coefficients(model)"] <- "estimate"
  
  return(coef_df)
})

# Assign a model identifier
for(i in names(coef_data)) {
  coef_data[[i]]$model <- paste0(i)
}

# Combine all data into one data frame
coef_data <- bind_rows(coef_data)

# check
# data$wd_adap<-data$WD*data$lag_log_gni_pc
# mean(aggregate(data$WD*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
# mean(aggregate(data$wd_adap, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )


wd_sd<-mean(aggregate(data$WD, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
pet_sd<-mean(aggregate(data$PET, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
spi_sd<-mean(aggregate(data$SPI, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
tm_sd<-mean(aggregate(data$TM, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
rr_sd<-mean(aggregate(data$RR, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )

wd2_sd<-mean(aggregate(data$WD_2, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
pet2_sd<-mean(aggregate(data$PET_2, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
spi2_sd<-mean(aggregate(data$SPI_2, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
tm2_sd<-mean(aggregate(data$TM_2, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
rr2_sd<-mean(aggregate(data$RR_2, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )


wd_adap_sd<-mean(aggregate(data$WD*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
pet_adap_sd<-mean(aggregate(data$PET*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
spi_adap_sd<-mean(aggregate(data$SPI*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
tm_adap_sd<-mean(aggregate(data$TM*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
rr_adap_sd<-mean(aggregate(data$RR*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )

wd2_adap_sd<-mean(aggregate(data$WD_2*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
pet2_adap_sd<-mean(aggregate(data$PET_2*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
spi2_adap_sd<-mean(aggregate(data$SPI_2*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
tm2_adap_sd<-mean(aggregate(data$TM_2*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )
rr2_adap_sd<-mean(aggregate(data$RR_2*data$lag_log_gni_pc, list(data$gdlcode), FUN=sd, na.rm=TRUE)$x, na.rm=TRUE )

std_devs <- c("TM" = tm_sd, "TM_2"=tm2_sd, "RR"=rr_sd, "RR_2"=rr2_sd, 
              "SPI"= spi_sd, "WD"=wd_sd, "WD_2"=wd2_sd, "PET"=pet_sd, "PET_2"=pet2_sd,
              "TM:lag_log_gni_pc" = tm_adap_sd, "TM_2:lag_log_gni_pc"=tm2_adap_sd, "RR:lag_log_gni_pc"=rr_adap_sd, "RR_2:lag_log_gni_pc"=rr2_adap_sd, 
              "SPI:lag_log_gni_pc"= spi_adap_sd, "WD:lag_log_gni_pc"=wd_adap_sd, "WD_2:lag_log_gni_pc"=wd2_adap_sd, "PET:lag_log_gni_pc"=pet_adap_sd, "PET_2:lag_log_gni_pc"=pet2_adap_sd
              )
coef_data$variable
# Rescale the coefficients and confidence intervals
coef_data <- coef_data %>%
  rowwise() %>%
  mutate(
    estimate_rescaled = estimate * std_devs[variable],
    lower_rescaled = lower * std_devs[variable],
    upper_rescaled = upper * std_devs[variable]
  )

coef_data$variable <- factor(coef_data$variable, levels=c("TM",  "TM:lag_log_gni_pc" ,
                                                          "TM_2",  "TM_2:lag_log_gni_pc",
                                                          "RR",  "RR:lag_log_gni_pc" ,
                                                          "RR_2",  "RR_2:lag_log_gni_pc",
                                                          "WD",  "WD:lag_log_gni_pc" ,
                                                          "WD_2",  "WD_2:lag_log_gni_pc",
                                                          "PET",  "PET:lag_log_gni_pc" ,
                                                          "PET_2",  "PET_2:lag_log_gni_pc",
                                                          "SPI",  "SPI:lag_log_gni_pc"  ))

g<-ggplot(coef_data, aes(x = variable, y = estimate_rescaled, color = model)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = lower_rescaled, ymax = upper_rescaled),
                width = 0.9,linewidth=1.5, 
                position = position_dodge(width = 0.5)) +
  labs(
    x = "Variable",
    y = "Rescaled Coefficient"
  ) +theme_bw()+geom_hline(yintercept =0, col = "black")+scale_color_brewer(name="Model", breaks=c('diff_hdi','diff_income_index', 'diff_health_index', 'diff_edu_index' ), palette="Set1")+
  theme(legend.position = "right",axis.text.x = element_text(angle = 90),text = element_text(size = 50)) +scale_x_discrete(labels = c("TM",  "TM_adap" ,
                                                                                                                                    "TM_2",  "TM_2_adap",
                                                                                                                                    "RR",  "RR_adap" ,
                                                                                                                                    "RR_2",  "RR_2_adap",
                                                                                                                                    "WD",  "WD_adap" ,
                                                                                                                                    "WD_2",  "WD_2_adap",
                                                                                                                                    "PET",  "PET_adap" ,
                                                                                                                                    "PET_2",  "PET_2_adap",
                                                                                                                                    "SPI",  "SPI_adap"  ))

ggsave(filename=file.path(out_dir, paste0("components_coefs_scaled_comparison.png")), g, width=20, height=15)

