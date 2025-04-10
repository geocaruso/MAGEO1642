#spatial.models.R

#Demo spatial lag and error models with Luxembourg apartment prices
#See course for theory and data
#
#Read the data and check the coordinates system
com<-sf::st_read("data/ext/DATA_COM.shp")
sf::st_crs(com) #https://epsg.io/2169

#Descriptive stats
#summary(com)
hist(com$MPRICEM2)
com101<-com[com$N>4,] #number of sales: at least 4 per commune 
centro101<-sf::st_centroid(com101)

#Mapping the dependent variable:
source("R/ggplot.themap.R")

map<-ggplot.themap(com101,"MPRICEM2")
map

#OLS model against 6 attributes
OLS1<-lm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI), data=com101)
summary(OLS1)
plot(OLS1)
lmtest::bptest(OLS1)

#Defining spatial neighbours and weights
comk4<-spdep::knearneigh(centro101, k=4) #1 and 2 being the X and Y columns in coords
comk6<-spdep::knearneigh(centro101, k=6)
## Create neighbours lists:
comk4nb<-spdep::knn2nb(comk4)
comk6nb<-spdep::knn2nb(comk6)
## Create spatial weights (row standaradized)
comk4w<-spdep::nb2listw(comk4nb,style="W") 
comk6w<-spdep::nb2listw(comk6nb,style="W")

#LISA
source("R/localmoran.withquads.R")
source("R/ggplot.themap.f.R")

LISA_k4_MPRICEM2<-localmoran.withquads(com101, "MPRICEM2", comk4w, signif=0.05)
LISA.colours=c("n.s." = "white", "L.L." = "darkblue", "L.H."="lightblue", "H.L." = "pink", "H.H."="darkred") 
LISA_k4_MPRICEM2_map<-ggplot.themap.f(LISA_k4_MPRICEM2,"signif_quad",
                          cl.colours = LISA.colours)
LISA_k4_MPRICEM2_map
spdep::moran.plot(com101$MPRICEM2, listw=comk4w)
spdep::moran.test(com101$MPRICEM2, listw=comk4w)

com101$logMPRICEM2<-log(com101$MPRICEM2)
LISA_k6_logMPRICEM2<-localmoran.withquads(com101, "logMPRICEM2", comk6w, signif=0.05)
LISA_k6_logMPRICEM2_map<-ggplot.themap.f(LISA_k6_logMPRICEM2,"signif_quad",
                                      cl.colours = LISA.colours)
LISA_k6_logMPRICEM2_map
spdep::moran.plot(com101$logMPRICEM2, listw=comk4w)
spdep::moran.test(com101$logMPRICEM2, listw=comk4w)


#Moran's I and LISA on residuals of OLS
com101$residOLS1<-resid(OLS1)
ggplot.themap(com101,"residOLS1")
LISA_k6_resid<-localmoran.withquads(com101, "residOLS1", comk6w, signif=0.05)
LISA_k6_resid_map<-ggplot.themap.f(LISA_k6_resid,"signif_quad",
                                         cl.colours = LISA.colours)
LISA_k6_resid_map
spdep::moran.plot(com101$residOLS1, listw=comk6w)
spdep::moran.test(com101$residOLS1, listw=comk6w)

#LMtests for spatial lag processes
spdep::lm.LMtests(OLS1,comk6w,test=c("LMerr", "RLMerr","LMlag","RLMlag", "SARMA"))
#Indicates I should do LMlag model (autoregressive) but we do also others
#test_all<-spdep::lm.LMtests(OLS1,comk6w,test="all")
#WARNING Please update scripts to use lm.RStests in place of lm.LMtests

test_all_robust<-spdep::lm.RStests(OLS1,comk6w,test="all")
summary(test_all_robust)


# SPATIAL MODELS

#1. Spatial Lag Model
LAG1<-spatialreg::lagsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                       data=com101, listw= comk6w)
summary(LAG1)
spatialreg::impacts(LAG1, listw= comk6w)

#2. Spatial Error Model
ERR1<-spatialreg::errorsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                       data=com101, listw= comk6w)
summary(ERR1)
spatialreg::impacts(ERR1, listw= comk6w)#No need, same as summary no indriect spillovers

ERR11<-spatialreg::spautolm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                            data=com101, listw= comk6w)


#3. Combined spatial lag and error Model
SAC1<-spatialreg::sacsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
data=com101, listw= comk6w)
summary(SAC1)
spatialreg::impacts(SAC1, listw= comk6w)

#4. Durbin , i.e. lagged covariates only
SLX1<-spatialreg::lmSLX(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                             data=com101, listw= comk6w, Durbin=TRUE)
   # Durbin is TRUE by construction in this case and defaulted to all covariates 
summary(SLX1)
spatialreg::impacts(SLX1, listw= comk6w)
summary(spatialreg::impacts(SLX1, listw= comk6w))

#DURBIN added, i.e. adding lagged X effects in LAG and ERR models
#5. Spatial lag and Durbin
LAG1durbin<-spatialreg::lagsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                           data=com101, listw= comk6w, Durbin = TRUE)
summary(LAG1durbin)
spatialreg::impacts(LAG1durbin, listw= comk6w)

#6. Spatial Error and Durbin
ERR1durbin<-spatialreg::errorsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                             data=com101, listw= comk6w,Durbin = TRUE)
summary(ERR1durbin)
spatialreg::impacts(ERR1durbin, listw= comk6w)
summary(spatialreg::impacts(ERR1durbin, listw= comk6w))

#7. Spatial error and lag (combined) +SLX
SAC1Durbin<-spatialreg::sacsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                           data=com101, listw= comk6w, Durbin = TRUE)
summary(SAC1Durbin)
spatialreg::impacts(SAC1Durbin, listw= comk6w)


SAC2Durbin<-spatialreg::sacsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007,
                                 data=com101, listw= comk6w, Durbin = TRUE)
summary(SAC2Durbin)
spatialreg::impacts(SAC2Durbin, listw= comk6w)
