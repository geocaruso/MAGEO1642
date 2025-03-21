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

#Indicates I should do LMlag model (autoregressive) but we do also error for fun:
LAG1<-spatialreg::lagsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                       data=com101, listw= comk6w)
summary(LAG1)

ERR1<-spatialreg::errorsarlm(log(MPRICEM2)~log(MSIZE)+log(VPHC)+DENS2007+log(RAGRI)+log(RFOREST)+log(DI),
                       data=com101, listw= comk6w)
summary(ERR1)





