### preprocess controls 
rm(list=ls())
source("utils/libraries.R")

# xlsx write
require(openxlsx)

# libraries needed
library(dplyr)
library(readr)
library(ggplot2)
library(plm)

library(gt)


data<- read_csv("output/data_hdi_original_comp_climate_pop_weight_1990_2020_less_na.csv")



### controls 

### conflicts 
locations_of_ongoing_armed_conflicts <- read_csv("data/controls/locations-of-ongoing-armed-conflicts/locations-of-ongoing-armed-conflicts.csv")
colnames(locations_of_ongoing_armed_conflicts)<-c("entity", "iso3", "year", "conflict")
locations_of_ongoing_armed_conflicts<-locations_of_ongoing_armed_conflicts[ ,c(2,3,4)]

###
expenditure_edu<- read_csv("data/controls/government-expenditure-education-share-gdp/7ece3243-6722-4c3e-8101-f888ddc0ea70_Data.csv")
expenditure_edu<-expenditure_edu[, c(4:16)]
colnames(expenditure_edu)<-c("iso3", c(paste0(1990:2023)))
expenditure_edu<-melt(expenditure_edu, id.vars = c("iso3"), variable.name = "year")
expenditure_edu$value[which(expenditure_edu$value=="..")]<-NA
colnames(expenditure_edu)<-c("iso3", "year" , "exp_edu")
expenditure_edu$year<-as.numeric(as.character(expenditure_edu$year))
expenditure_edu$exp_edu<-as.numeric(as.character(expenditure_edu$exp_edu))
expenditure_edu$exp_edu<-(expenditure_edu$exp_edu - min(expenditure_edu$exp_edu, na.rm=TRUE))/(max(expenditure_edu$exp_edu, na.rm=TRUE)-min(expenditure_edu$exp_edu, na.rm=TRUE))  
expenditure_edu$exp_edu<-logit(expenditure_edu$exp_edu)


### 
expenditure_health <- read_csv("data/controls/public-health-expenditure-share-gdp/public-health-expenditure-share-gdp.csv")
expenditure_health <- expenditure_health[, c(2,3,4)]
colnames(expenditure_health)<-c("iso3", "year" , "exp_health")
expenditure_health$exp_health<-(expenditure_health$exp_health - min(expenditure_health$exp_health, na.rm=TRUE))/(max(expenditure_health$exp_health, na.rm=TRUE)-min(expenditure_health$exp_health, na.rm=TRUE))  
expenditure_health$exp_health<-logit(expenditure_health$exp_health)

###
trade<- read_csv("data/controls/trade-as-share-of-gdp/trade-as-share-of-gdp.csv")
trade<-trade[, c(2,3,4)]
colnames(trade)<-c("iso3", "year" , "trade")
trade$trade<-(trade$trade - min(trade$trade, na.rm=TRUE))/(max(trade$trade, na.rm=TRUE)-min(trade$trade, na.rm=TRUE))  
trade$trade<-logit(trade$trade)


### unify
controls<-expand.grid(iso3=unique(data$iso3), year=unique(data$year))
controls<-left_join(controls, trade)
controls<-left_join(controls, expenditure_health)
controls<-left_join(controls, expenditure_edu)


set.seed(17)

# libraries parallel
library(parallel)
library(foreach)
library(doParallel)


# parallelize 
cores=detectCores()

cl <- makeCluster(cores[1]-1) # locale 
#cl <- makeCluster(cores[1]) # zeus

registerDoParallel(cl)

imp<-Amelia::amelia(controls, m = 5, ts = "year", cs = "iso3",  polytime = 1,   intercs = TRUE, 
                    parallel="multicore",  ncpus = getOption("amelia.ncpus", 6L))
tscsPlot(imp, cs = "USA", main = "imputation", var = "exp_health")

save(imp,file="data/controls/imputed_health_edu_trade_shares_gdp.RData" )


all_controls<-inner_join(locations_of_ongoing_armed_conflicts, imp[["imputations"]][["imp1"]])
write.csv(all_controls, file="data/controls/all_controls.csv", row.names = FALSE)
