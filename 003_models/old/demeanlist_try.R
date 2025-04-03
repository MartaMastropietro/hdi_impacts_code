### trying to use demeanlist() or fixest::demean to demean data and get same results as fixest feols with fe. 
### be areful with trends, they must appear in a certain order to entail same results as fe 

library(lfe)
oldopts <- options("lfe.threads")
options(lfe.threads = 2)

## create a matrix
#mtx <- data.frame(matrix(rnorm(999), ncol = 3))
#mtx<-as.data.frame(lapply(mtx, function(cc) cc[ sample(c(TRUE, NA), prob = c(0.85, 0.15), size = length(cc), replace = TRUE) ]))
#
## a list of factors
#rgb <- c("red", "green", "blue")
#fl <- replicate(2, factor(sample(rgb, nrow(mtx), replace = TRUE)), simplify = FALSE)
#
#names(fl) <- paste("g", seq_along(fl), sep = "")
## centre on all means
#mtx0 <- demeanlist(mtx, fl, na.rm=TRUE)
#head(data.frame(mtx0))
#head(data.frame(mtx, fl))
#     
#o <- "X1"
#i <- "g1 + g2 "
#r <- "X2 + X3"
#f <- as.formula(paste( o, "~", r, "|" ,i ))
#m <- fixest::feols(f, data.frame(mtx, fl))
#summary(m)
#
#m_dem<-lm(X1 ~ X2 + X3 -1, data=data.frame(mtx0, fl) )
#summary(m_dem)

###

library(tidyr)
data<- readr::read_csv("output/data_hdi_climate_gdl_pop_weight_1990_2021.csv")
pan_id<-c('gdlcode', 'year')

### test erasing na
o <- "diff_hdi"
i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)


data<-data%>%drop_na(o)

i <- "gdlcode + year"
r <- "TM + TM_2 + RR + RR_2"
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)
# ok

### test demean list only with simple fe
dem_list<-list(year=as.factor(data$year), gdlcode=as.factor(data$gdlcode) )
dem_dat<-data[ , setdiff(names(data), c("gdlcode", "year", "iso3"))]
dem_dat<-demeanlist(dem_dat, dem_list)
f <- as.formula(paste0( o, "~", r, " -1"))
m_dem<-fixest::feols( f, data=data.frame(dem_dat, dem_list))
summary(m_dem, cluster="gdlcode")
# ok, remember to cluster!


### now test demeaning with more complex fe
o <- "diff_hdi"
i <- "gdlcode+  iso3[year] + year " 
r <- "TM + TM_2 + RR + RR_2 " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

# f <- as.formula(paste( o, "~", "1 |" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# summary(m)

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM","TM_2", "RR","RR_2" )], f = data[, c("gdlcode", "year")])
f <- as.formula(paste( o, "~", r ,"-1" ))
#m <- fixest::feols(f, data_dem_bis)
#summary(m)
m <- fixest::feols(f, data_dem)
summary(m)


o <- "diff_hdi"
i <- "gdlcode+  gdlcode[year] + year " 
r <- "TM + TM_2 + RR + RR_2 " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

# f <- as.formula(paste( o, "~", "1 |" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# summary(m)

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM","TM_2", "RR","RR_2" )], f = data[, c("gdlcode", "year")])
f <- as.formula(paste( o, "~", r ,"-1" ))
#m <- fixest::feols(f, data_dem_bis)
#summary(m)
m <- fixest::feols(f, data_dem)
summary(m)


o <- "diff_hdi"
i <- "gdlcode+  gdlcode[year]+ gdlcode[year^2] + year " 
r <- "TM + TM_2 + RR + RR_2 " 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

# f <- as.formula(paste( o, "~", "1 |" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# summary(m)

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM","TM_2", "RR","RR_2" )], f = data[, c("gdlcode", "year")])
f <- as.formula(paste( o, "~", r ,"-1" ))
#m <- fixest::feols(f, data_dem_bis)
#summary(m)
m <- fixest::feols(f, data_dem)
summary(m)


o <- "diff_hdi"
i <- "gdlcode+  iso3[year]+ iso3[year^2] + year " 
r <- "TM + TM_2 + RR + RR_2 +TM:lag_log_gni_pc" 
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

# f <- as.formula(paste( o, "~", "1 |" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# summary(m)

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM","TM_2", "RR","RR_2" )], f = data[, c("gdlcode", "year")])
f <- as.formula(paste( o, "~", r ,"-1" ))
#m <- fixest::feols(f, data_dem_bis)
#summary(m)
m <- fixest::feols(f, data_dem)
summary(m)

# with lags
i=2
v<-"TM"
data[paste0("lag_",i,"_",v)]<-lag(as.formula(paste0(v, "~gdlcode+year")), i, data)

l<- paste0("lag_",1:2,"_",v, collapse = "+")
o <- "diff_hdi"
i <- "gdlcode+  iso3[year]+ iso3[year^2] + year " 
r <- paste0("TM + TM_2 + RR + RR_2 +" , l)
f <- as.formula(paste( o, "~", r, "|" ,i ))
m <- fixest::feols(f, data, panel.id=pan_id)
summary(m)

# f <- as.formula(paste( o, "~", "1 |" ,i ))
# m <- fixest::feols(f, data, panel.id=pan_id)
# summary(m)

data_dem <- fixest::demean(as.formula(paste0(o , "+", r , "~", i )), data = data, na.rm=FALSE)
#data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM","TM_2", "RR","RR_2" )], f = data[, c("gdlcode", "year")])
f <- as.formula(paste( o, "~", r ,"-1" ))
#m <- fixest::feols(f, data_dem_bis)
#summary(m)
m <- fixest::feols(f, data_dem)
summary(m)






#################


data_dem_bis = fixest::demean(X = data[, c("diff_hdi", "TM", "TM_2", "RR", "RR_2" )], f = data[, c("gdlcode",  "iso3", "year")],
                     slope.vars = data$year, slope.flag = c(0, 1, 0) )
f <- as.formula(paste( o, "~", r ,"-1" ))
m <- fixest::feols(f, data_dem_bis)
summary(m)
m <- fixest::feols(f, data_dem)
summary(m)


r <- "TM + TM_2 + RR + RR_2" 
f <- as.formula(paste( o, "~", r, "-1" ))
m <- fixest::feols(f, data)
summary(m)

dem_list<-list(year=as.factor(data$year), gdlcode=as.factor(data$gdlcode) , iso3_year=as.factor(as.factor(data$iso3)*data$year))
dem_dat<-data[ , setdiff(names(data), c("gdlcode", "year", "iso3"))]
dem_dat<-demeanlist(dem_dat, dem_list)
f <- paste0( o, "~", r, " -1")
m_dem<-lm(formula = f, data=dem_dat)
summary(m_dem)

