#'LISA.mapping.workflow.R
#'
#' A workflow to compute Moran's I and LISA's using an sf input
#' and mapping clusters (and scatterplotting) with ggplot.
#' Example with Luxembourg population densities
#'
#' Resources for spdep and sf:
#' https://r-spatial.github.io/spdep/articles/nb_sf.html#summary
#' https://r-spatial.github.io/sf/reference/geos_unary.html

# Data
lux102sf_density1821<-readRDS("data/lux102sf_density1821.rds")

#Transform to natioanl reference system rather than WGS84
#see LURES (LUREF) https://coordinateconverter.geoportail.lu/
#https://epsg.io/2169
lux2169<-sf::st_transform(lux102sf_density1821,crs = 2169)
##  x,y coordinates of centroids for knn and displaying neighbours lists
lu<-sf::st_centroid(lux2169)

# 1. Neighbours lists----
## 1.1.Based on knn ----
luk4<-spdep::knearneigh(lu, k=4)
luk10<-spdep::knearneigh(lu, k=10)
## Create neighbours lists:
luk4nb<-spdep::knn2nb(luk4)
luk10nb<-spdep::knn2nb(luk10)
## ggplotting neighbours
luk4nb_lines<-spdep::nb2lines(luk4nb, coords=st_geometry(lu), as_sf=TRUE)
luk10nb_lines<-spdep::nb2lines(luk10nb, coords=st_geometry(lu), as_sf=TRUE)

gk<-ggplot()+
  geom_sf(data=lux2169, fill="goldenrod", col="white")+
  geom_sf(data=luk10nb_lines)+
  geom_sf(data=luk4nb_lines, color="cyan")+
  theme_bw()
gk

## 1.2.Based on contiguity or max distance from polygons----
# contiguity
lutouch<-sf::st_touches(lux2169)
# within distance
luwithin5km<-st_is_within_distance(lux2169, dist=5000)

# Both result in a sgbp object. Not a nb list as defined in spdep
# Conversion function suggested by R Bivand:
# https://cran.r-hub.io/web/packages/spdep/vignettes/nb_sf.html
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}
lutouchnb<-as.nb.sgbp(lutouch)
luwithin5kmnb<-as.nb.sgbp(luwithin5km)

## ggplotting neighbours
lutouchnb_lines<-spdep::nb2lines(lutouchnb, coords=st_geometry(lu), as_sf=TRUE)
luwithin5kmnb_lines<-spdep::nb2lines(luwithin5kmnb, coords=st_geometry(lu), as_sf=TRUE)
#Each separately and with colours for each origin otherwise not quite informative
gt<-ggplot()+
  geom_sf(data=lux2169, fill="black", col=NA)+
  geom_sf(data=lutouchnb_lines, aes(col=i_ID))+
  theme_bw()+ theme(legend.position = "none")+
  ggtitle("Touching neighbours")
gt

g5<-ggplot()+
  geom_sf(data=lux2169, fill="black", col=NA)+
  geom_sf(data=luwithin5kmnb_lines, aes(col=i_ID))+
  theme_bw()+ theme(legend.position = "none")+
  ggtitle("Within 5km neighbours")
g5

#Still not great since depends on what is drawn next and neighbour j of i is i of j
#Let's randomized destinations coordinates
m<-matrix(unlist(lutouchnb_lines$geometry),ncol=4,byrow = TRUE)
mj<-data.frame(m)
mj[,2]<-mj[,2]+runif(nrow(mj), -2000, 2000) #up to 2 km change of locations
mj[,4]<-mj[,4]+runif(nrow(mj), -2000, 2000)
names(mj)<-c("x","xend","y","yend")
lutouchnb_lines_mj<-cbind(lutouchnb_lines,mj)

gt2<-ggplot()+
  geom_sf(data=lux2169, fill="black", col=NA)+
  geom_segment(data=lutouchnb_lines_mj, aes(x=x,y=y,xend=xend,yend=yend,col=i_ID))+
  theme_bw()+ theme(legend.position = "none")+
  ggtitle("Touching neighbours")
gt2
#Well it is actually not that much clearer, I agree ;-)

#sink to a pdf
pdf(file="output/Lux_neighbours.pdf")
print(gk)
print(gt)
print(gt2)
print(g5)
dev.off()


## 2. spatial weights----
#row-standardised weight matrices
luk4w<-spdep::nb2listw(luk4nb,style="W") 
luk10w<-spdep::nb2listw(luk10nb,style="W") 
lutouchw<-spdep::nb2listw(lutouchnb,style="W")
luwithin5kmw<-spdep::nb2listw(luwithin5kmnb,style="W")
summary(lutouchw)

## 3. Moran's I----
#Pick up a variable and a weight matrix
luDensity<-st_drop_geometry(lux2169) #subsetting variable would otherwise keep geometry
x<-luDensity[,"Density2023"]
mylistw<-luk4w
moran_i<- spdep::moran.test(x, listw=mylistw)

## 4. Moran's scatterplot----
spdep::moran.plot(x, listw=mylistw)

#To build this with ggplot, remind that Moran's I is the slope of regressing
# spatial lag of a variable by the variable itself,
# both expressed in deviations to mean
dev_x<-x-mean(x) #express variable as deviation to mean
spatial_lag_dev_x<-spdep::lag.listw(mylistw,dev_x) #lag the deviation to mean
morlm <- lm(spatial_lag_dev_x ~ dev_x)
morlm_itcpt<- morlm$coefficients[1]
morlm_i <- morlm$coefficients[2] #slope

#Compare:
morlm_i-moran_i$estimate[1]

#Moran's scatterplot can be rebuilt with base R plot with
plot(dev_x,spatial_lag_dev_x)
abline(morlm_itcpt,morlm_i,col="red")
abline(h=0,lty=2,col="blue")
abline(v=0,lty=2,col="blue")

#and ggplot way:
zdf<-data.frame(dev_x,spatial_lag_dev_x)
ggplot(data=zdf)+
  geom_point(aes(x=dev_x,y=spatial_lag_dev_x))+
  geom_hline(yintercept=0, linetype="dashed",
             color = "blue", size=0.3)+
  geom_vline(xintercept=0, linetype="dashed",
             color = "blue", size=0.3)+
  geom_abline(aes(intercept=morlm_itcpt,slope=morlm_i),col="red")+
  theme_bw()


## 5. LISA ----
mIlocal_x<- spdep::localmoran(x, listw=mylistw)
LISA_x<-as.data.frame(mIlocal_x)
head(LISA_x)

# Don't confuse!! LISA statistics Ii is not the spatial lag !!
# LISA Ii is the product of x and the spatial lag of x
# (both in deviations)
# and thus is positive for HH and LL quadrants and negative otherwise
# as one can see from following
plot(spatial_lag_dev_x, LISA_x$Ii)

#Identify quadrants for mapping clusters

LISA_x$quad<-factor("n.s", levels=c("n.s.","L.L.","L.H.","H.L.","H.H."))
LISA_x$quad[dev_x <0 & spatial_lag_dev_x<0] <- "L.L."
LISA_x$quad[dev_x <0 & spatial_lag_dev_x>0] <- "L.H."
LISA_x$quad[dev_x >0 & spatial_lag_dev_x<0] <- "H.L."
LISA_x$quad[dev_x >0 & spatial_lag_dev_x>0] <- "H.H."

# statistical significance level
signif <- 0.05
LISA_x$signif_quad<-LISA_x$quad
LISA_x$signif_quad[LISA_x[,"Pr(z != E(Ii))"]>signif]<-"n.s."  #zero to n.s lisa types

# And mapping with our categorical mapping function
     #### source("R/ggplot.themap.f.R") #in case #####
luxLISA<-cbind(lux2169,LISA_x)
LISA.colours=c("n.s." = "white", "L.L." = "darkblue", "L.H."="lightblue", "H.L." = "pink", "H.H."="darkred") 
glisa<-ggplot.themap.f(luxLISA,"signif_quad", cl.colours = LISA.colours)

#Let's build a wrapping function to facilitate the process:
# with our 2 arguments (x and mylistw set at Step 3 above) and the sf
#
localmoran.withquads<-function(sf, varname, w, signif=0.05){
  svar<-sf::st_drop_geometry(sf)[,varname]
  LISA_x<-as.data.frame(spdep::localmoran(svar, listw=w))
  LISA_x$quad<-factor("n.s", levels=c("n.s.","L.L.","L.H.","H.L.","H.H."))
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x<0] <- "L.L."
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x>0] <- "L.H."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x<0] <- "H.L."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x>0] <- "H.H."
  LISA_x$signif_quad<-LISA_x$quad
  LISA_x$signif_quad[LISA_x[,5]>signif]<-"n.s."  #zero to n.s lisa types
  return(cbind(sf,LISA_x))
} 

LISA.colours=c("n.s." = "white", "L.L." = "darkblue", "L.H."="lightblue", "H.L." = "pink", "H.H."="darkred") 

LISA_D1821_k4<-localmoran.withquads(lux2169, "Density1821", luk4w, signif=0.05)
g1821_k4<-ggplot.themap.f(LISA_D1821_k4,"signif_quad",
                cl.colours = LISA.colours)
g1821_k4

LISA_D1821_k10<-localmoran.withquads(lux2169, "Density1821", luk10w, signif=0.05)
g1821_k10<-ggplot.themap.f(LISA_D1821_k10,"signif_quad",
                          cl.colours = LISA.colours)
g1821_k10

LISA_D1821_w5<-localmoran.withquads(lux2169, "Density1821", luwithin5kmw, signif=0.05)
g1821_w5<-ggplot.themap.f(LISA_D1821_k4,"signif_quad",
                          cl.colours = LISA.colours)
g1821_w5

LISA_D2023_k10<-localmoran.withquads(lux2169, "Density2023", luk10w, signif=0.05)
g2023_k10<-ggplot.themap.f(LISA_D2023_k10,"signif_quad",
                          cl.colours = LISA.colours)
g2023_k10

#sink to a pdf
pdf(file="output/Lux_LISAs.pdf")
print(g1821_k4)
print(g1821_k10)
print(g1821_w5)
print(g2023_k10)
dev.off()



#More:
#See https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-2.html
#And https://cran.r-project.org/web/packages/rgeoda/rgeoda.pdf

