#Spatial depedence model tutorial
#as of Marie Line example

#Data
com106<-sf::st_read("data/DATA_SpatialLagError/DATA_COM.shp") 
head(com106)
com101<-com106[com106$N>4,] #minimum number of observations

ggplot.themap(com101,"MPRICEM2") #Y of interest

#Logged variables
com101$lnMPRICEM2<-log(com101$MPRICEM2)
com101$lnMSIZE<-log(com101$MSIZE)
com101$lnVPHC<-log(com101$VPHC)
com101$lnRagri<-log(com101$RAGRI)
com101$lnForest<-log(com101$RFOREST)
com101$lnDI<-log(com101$DI)

#OLS Model
OLSmodel<-lm(lnMPRICEM2~lnMSIZE+
               lnVPHC+
               DENS2007+
               lnRagri+
               lnForest+
               lnDI,
             data=com101)
summary(OLSmodel)

#Heteroskedasticity test
lmtest::bptest(OLSmodel)
 #no heterosk

car::vif(OLSmodel)

#Map the residuals
com101$resOLS<-residuals(OLSmodel)

ggplot.themap(com101,"resOLS",style = "jenks")

#Spatial weights
#contiguity.
#lutouch<-sf::st_touches(com101) #LEADS TO 6 POLYGONS HAVING NO NEIGHBOUR
#lutouchnb<-as.nb.sgbp(lutouch)
lutouchnb<-spdep::poly2nb(com101)
contQcom101W<-spdep::nb2listw(lutouchnb,style="W")

#Moran's I
moran_i_of_anyvariable<- spdep::moran.test(com101$resOLS, listw=contQcom101W)
moran_i<-spdep::lm.morantest(OLSmodel,contQcom101W,alternative="greater")

#appernetly no problem of autocorrelation in residuals

#LISA ...later
#
#lm tests: Suggests a lag effect but no error effect
LMOLS<-spdep::lm.LMtests(OLSmodel,contQcom101W,test=c("LMerr", "RLMerr","LMlag","RLMlag", "SARMA"))

#Let's do lag model
SARmodel<-spatialreg::lagsarlm(lnMPRICEM2~lnMSIZE+
                       lnVPHC+
                       DENS2007+
                       lnRagri+
                       lnForest+
                       lnDI,
                     data=com101,
                     listw= contQcom101W)

SEMmodel<-spatialreg::errorsarlm(lnMPRICEM2~lnMSIZE+
                                 lnVPHC+
                                 DENS2007+
                                 lnRagri+
                                 lnForest+
                                 lnDI,
                               data=com101,
                               listw= contQcom101W)

SACmodel<-spatialreg::sacsarlm(lnMPRICEM2~lnMSIZE+
                                 lnVPHC+
                                 DENS2007+
                                 lnRagri+
                                 lnForest+
                                 lnDI,
                               data=com101,
                               listw= contQcom101W)

com101$resSAC<-residuals(SACmodel)
ggplot.themap(com101,"resSAC")
