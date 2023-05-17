#MAPPING TIPS AND MAPPING LISA USING SF AND GGPLOT2
#Geoffrey Caruso - MAGEO16.42 - Summer Semester 2023

#PREAMBLE----
#Quite some learning outcomes:
# sf and gpkg (bye bye shapefiles)
# ggplot2 (for graphs and maps)
# make a loop!
# print map series to pdf
# include OSM map tiles or vectors
# make a function!
# choropleth mapping for continuopus data after dicretisation (classInt)
# choropleth mapping for discrete data 
# Applying and mapping LISA


##Libraries----
library(ggplot2)
library(sf)
library(osmdata) #to get OSM vector data
library(maptiles) #to get OSM tile data
library(tidyterra) #to use tiles in ggplot
library(classInt)
library(RColorBrewer)
library(legocolors)
library(spdep)

#DATA----
## Vector data----

#I download the data communes4326.geojson from
#https://data.public.lu/fr/datasets/limites-administratives-du-grand-duche-de-luxembourg/
LUX<-sf::st_read("https://data.public.lu/fr/datasets/r/16103fa4-7ff1-486a-88bc-5018353957ea")
st_write(LUX,"OUT/Communes102_4326.gpkg")

#Download population density since 1821!
apiURL_1821_2023 <- "https://lustat.statec.lu/rest/data/LU1,DF_X020,1.0/.A?startPeriod=1821&endPeriod=2023"
data_1821_2023 <- data.frame(rsdmx::readSDMX(apiURL_1821_2023))
#For some reason, the 'libelles' is not given, only a specification code,
# per commune and canton. But the libelles is present in the csv download.
# For example the one with population:
LU1<-read.csv("DATA/LU1_DF_X021_1.0_A..csv")

lu<-unique(LU1$SPECIFICATION..Spécification)
#from which we see "CT..." is for Cantons, which we remove
lu102<-lu[stringr::str_sub(lu, 1, 2)=="CM"]
#lu102 has 102 communes. Their code is the 7 first characters,
# and their name start from character 10. We split lu102 in 2columns``
CMCODE<-stringr::str_sub(lu102, 1, 7)
CMNAME<-stringr::str_sub(lu102, 10)
CMLU102<-data.frame(cbind(CMCODE,CMNAME))

#which we can merge to (the informative columns of) data_1821_2023,
# which is in long format (years in rows)
data1821sub<-data_1821_2023[,c("SPECIFICATION","obsTime","obsValue")]
# and removing also the cantons aggregates:
data1821sub2<-data1821sub[stringr::str_sub(data1821sub[,"SPECIFICATION"], 1, 2)=="CM",]

df1821<-merge(data1821sub2,CMLU102,by.x="SPECIFICATION", by.y="CMCODE", all.x = TRUE,sort=FALSE)

#Note that the CMCODE does not match LAU2 (as of geoportail LUX) so a matching
# needs to be made on names for being able to map.
setdiff(unique(df1821$CMNAME), unique(LUX$COMMUNE))
setdiff(unique(LUX$COMMUNE),unique(df1821$CMNAME))
# and names are dangerous beasts. Yet in this case only 3 differ and by not much:
setdiff(unique(df1821$CMNAME), unique(LUX$COMMUNE))
#[1] "Lac de la Haute-Sûre" "Rosport - Mompach"   
#[3] "Redange-sur-Attert"  
setdiff(unique(LUX$COMMUNE),unique(df1821$CMNAME))
#[1] "Lac de la Haute Sûre" "Rosport-Mompach"     
#[3] "Redange"


#However it is the most recent dataset with only 102 communes 
# and I want the 116 because I will later link with the census of 2011 where there are 116 communes

#I read the data from file obtained from ...
LUX116<-sf::st_read("DATA/Luxembourg/shapefile/communes_116.shp")

#I map the data with ggplot
mymap_plot<-ggplot(data=LUX116)+
  geom_sf(fill="lightblue",col='darkred')+
  theme_bw()
mymap_plot

#I map the data with ggplot but different colors
mymap_plot2<-ggplot(data=LUX116)+
  geom_sf(fill="darkblue",col='yellow')+
  theme_bw()
mymap_plot2

#I sink the 2 maps into a pdf
pdf(file = "OUT/PDF/mymaps.pdf")
mymap_plot
mymap_plot2
dev.off()

#Now another pdf with a map of each commune after a loop  and creating a gpkgfor each
pdf(file="OUT/PDF/mycommunes.pdf")
for (i in 1:dim(LUX116)[1]) {
  iLUX<-LUX116[i,] #we subset the i'th observation (municipality)
                    #keeping all the attributes including the geometry
  myplot_of_i<-ggplot()+
    geom_sf(data=iLUX,fill="lightblue",col='yellow')+
    theme_bw()
message(paste(i," out of", dim(LUX116)[1]))
print(myplot_of_i)
#st_write(iLUX,paste0("OUT/GPKG/i",i,".gpkg")) #Don't want this every time
}
dev.off()

#I save LUX116 as a geopackage
st_write(LUX116,"OUT/GPKG/LUX116.gpkg")


#Now let's add some OSM data on top

#Before we need to transfrom our sf into a projection that is used worldwide including in OSM
LUX116_4326<-st_transform(LUX116,crs = 4326) #coordinates into geographic system WGS84

pdf(file="OUT/PDF/mycommunes_cycle_tree.pdf")
for (i in 1:dim(LUX116_4326)[1]) {
  iLUX4326<-LUX116_4326[i,]
  iname<-iLUX4326$COMMUNE
  ibb<-st_bbox(iLUX4326)
  iquery <- opq(bbox = ibb) #function from osmdata pkg
  iquery2<-add_osm_feature(opq=iquery,key = "cycleway")
  iquery3<-add_osm_feature(opq=iquery,key = "natural", value="tree")
  iosm2<-osmdata_sf(iquery2) #iosm2 is a simple feature with the cycleways objects
  iosm3<-osmdata_sf(iquery3)
  
  myplot_of_i_OSM<-ggplot()+
    geom_sf(data=iLUX4326,fill=NA,col='orange')+
    geom_sf(data=iosm2$osm_lines, col='magenta')+ #We take only the line objects from cycleways
    geom_sf(data=iosm3$osm_points, col="darkgreen")+ #We take the trees as points
    ggtitle(iname)+
    theme_bw()
  message(i)
  print(myplot_of_i_OSM)
}
dev.off()

osm <- list(src = "OSM",
            q = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            sub = c("a", "b", "c"),
            cit = "© OpenStreetMap contributors.")
LUX<-sf::st_read("https://data.public.lu/fr/datasets/r/16103fa4-7ff1-486a-88bc-5018353957ea")
i=11 #Esch
LUX[i,] #subset
iosmtile <- get_tiles(LUX[i,], provider=osm,crop = TRUE, zoom = 12)

#terra::plotRGB(iosmtile)#Plotting a SpatRaster, an object made by terra
#but we do all our maps with ggplot so we want to plot with the tidyterra 

ggplot()+
  geom_spatraster_rgb(data = iosmtile)+ #this geom comes with tidyterra
  geom_sf(data=LUX[i,],fill=NA,col='orange',lwd=2)+
  theme_bw()


#Joining data

#This is the file with the attributes we woul like to map
census<-read.csv("DATA/Luxembourg/Lux_Census_2011.csv")

#Merge is equivalent to join in arcgis
LUCE<-merge(x = LUX116, y=census, by.x="CODES_116", by.y="CODES_116")
#LUCE_2<-merge(x = LUX116, y=census, by="CODES_116")
#LUCE_1<-merge(x = LUX116, y=census)

ggplot()+
  geom_boxplot(data=LUCE,aes(x=Inhab_km2_2011))

ggplot()+
  geom_boxplot(data=LUCE,aes(x=Inhab_km2_2011, y=CANT_116, col=CANT_116))


ggplot(data=LUCE, aes(x=CANT_116, y=Inhab_2011)) +
  geom_bar(stat="identity")+
  coord_flip()

ggplot()+
  geom_sf(data=LUCE,aes(fill=Inhab_km2_2011))
#we don' like it because values are not discretized


#Discretized map of a continuous variable

#Parameters
nclasses<-5
low.colour<-"lightyellow"
high.colour<-"darkred"
outline.colour<-"goldenrod"
outline.width<-0.2
#Processing
cl.intvl<-classInt::classIntervals(LUCE$Inhab_km2_2011, nclasses)
cl.value<-factor(classInt::findCols(cl.intvl))
cl.colours<-classInt::findColours(cl.intvl, c(low.colour,high.colour))
leglabels<-paste(format(round(cl.intvl$brks[1:nclasses],digits=2), nsmall=2),
                 format(round(cl.intvl$brks[2:(nclasses+1)],digits=2), nsmall=2),
                 sep=" - ")
sc.f<-scale_fill_manual(name = "Inhab per km2 (2011)",
                        breaks = seq(1:nclasses),
                        values=attr(cl.colours,"palette"),
                        labels=leglabels)
#Plotting
ggplot()+
  geom_sf(data=LUCE,aes(fill=cl.value))+ sc.f

ggplot()+
  geom_sf(data=LUCE,aes(fill=cl.value),
          colour=outline.colour,
          size=outline.width)+
  sc.f+
  theme_bw()

#Since we can't remember everything and it is cumbersome to write
# all this for a map let's make a function that only takes 2 main inputs (arguments):
# the sf , the variable of interest, and supplementary options (arguments) to which give default values`:
# the number of classes, the discretisation method, the colours and outline width

ggplot.themap_beta<-function(sf,varname, #Aguments in parentheses,
                  cl.style="quantile",
                  n.classes=5,
                  low.colour="lightyellow",
                  high.colour="darkred",
                  cl.colours=attr(findColours(cl.intvl, c(low.colour,high.colour)),"palette"),
                  cl.breaks=NULL,
                  outline.colour="#ffce00",
                  outline.width=0.2,
                  n.digits=2,
                  leg.title=varname,
                  main.title=paste(substitute(sf),varname),
                  sub.title=cl.style,
                  ggtheme=theme_bw()
                  ){              #  then what the function does
  svar<-st_drop_geometry(sf)[,varname]
  cl.intvl<-classIntervals(svar, n.classes,style=cl.style,fixedBreaks=cl.breaks)
  cl.value<-factor(findCols(cl.intvl))
  leg.labels<-paste(format(round(cl.intvl$brks[1:n.classes],digits=n.digits), nsmall=2),
                   format(round(cl.intvl$brks[2:(n.classes+1)],digits=n.digits), nsmall=2),
                   sep=" - ")
  sc.f<-scale_fill_manual(name = leg.title,
                          breaks = seq(1:n.classes),
                          values=cl.colours,
                          labels=leg.labels)
  themap<-ggplot()+
    geom_sf(data=sf,aes(fill=cl.value),
            colour=outline.colour,
            size=outline.width)+
    sc.f+
    ggtheme+
    ggtitle(label=main.title, subtitle=sub.title)
  return(themap)             #  Finally what it returns
}

##Apply our ggplot Thematic map function
gp1<-ggplot.themap_beta(LUCE,"Inhab_km2_2011") 
gp2<-ggplot.themap_beta(LUCE,"Foreigners_sh_2011",n.classes=6,leg.title="Share of population", main.title = "Foreigners in Luxembourg (census 2011)" ) 
gp3<-ggplot.themap_beta(LUCE,"HomeOwners_sh_2011",n.classes=7, high.colour="deeppink4")
gp4<-ggplot.themap_beta(LUCE,"DepRate_2011", cl.style="fisher",main.title = "Dependency rate (census 2011)",high.colour="darkgreen" ) 
gp5<-ggplot.themap_beta(LUCE,"DepRate_2011", cl.style="fisher",main.title = "Dependency rate (census 2011)", high.colour="darkgreen", ggtheme=theme_void() ) 

myggtheme<- function () { #a bit of National Geographic flavour
    theme_bw() %+replace% 
      theme(
        panel.grid.major  = element_line(color = NA),
        panel.background = element_rect(fill = "grey10"),
        panel.border = element_rect(color = "#ffce00", fill = NA, linewidth=10))
  }
  
gp6<-ggplot.themap_beta(LUCE,"DepRate_2011", cl.style="fisher",main.title = "Dependency rate (census 2011)", high.colour="darkred", ggtheme=myggtheme()) 

my5colours<-c("yellow","green","blue","purple","darkred") #if you need to set your own colours
gp7<-ggplot.themap_beta(LUCE,"DepRate_2011", cl.colours = my5colours) 

mybreaks<-c(30,35,40,45,50,60)  #Sometimes you want your own breaks (comparison maps)
gp8<-ggplot.themap_beta(LUCE,"DepRate_2011", cl.colours = my5colours,
                   cl.style="fixed",cl.breaks=mybreaks) 

pdf(file="OUT/PDF/mychoropleths.pdf")
print(gp1)
print(gp2)
print(gp3)
print(gp4)
print(gp5)
print(gp6)
print(gp7)
print(gp8)
dev.off()


#Improved function to avoid some repetitions with RclassInt arguments (use of ellipsis...)
ggplot.themap<-function(sf,varname, n=5,...,#Aguments in parentheses, dots for further arguments passed to ClassInt
                        low.colour="lightyellow",
                        high.colour="darkred",
                        cl.colours=attr(findColours(cl.intvl, c(low.colour,high.colour)),"palette"),
                        outline.colour="#ffce00",
                        outline.width=0.2,
                        n.digits=2,
                        leg.title=varname,
                        main.title=paste(substitute(sf),varname),
                        sub.title=attr(cl.intvl, "style"),
                        ggtheme=theme_bw()
){              #  then what the function does
  svar<-st_drop_geometry(sf)[,varname]
  dots<-list(...)
  cl.intvl<-classIntervals(svar, n=n, dots, fixedBreaks=dots$fixedBreaks)
  n.classes<-length(cl.intvl$brks)-1
  cl.value<-factor(findCols(cl.intvl))
  leg.labels<-paste(format(round(cl.intvl$brks[1:n.classes],digits=n.digits), nsmall=2),
                    format(round(cl.intvl$brks[2:(n.classes+1)],digits=n.digits), nsmall=2),
                    sep=" - ")
  sc.f<-scale_fill_manual(name = leg.title,
                          breaks = seq(1:n.classes),
                          values=cl.colours,
                          labels=leg.labels)
  themap<-ggplot()+
    geom_sf(data=sf,aes(fill=cl.value),
            colour=outline.colour,
            size=outline.width)+
    sc.f+
    ggtheme+
    ggtitle(label=main.title, subtitle=sub.title)
  return(themap)             #  Finally what it returns
}

gp11<-ggplot.themap(LUCE,"Inhab_km2_2011",n=5,style="fisher") 
gp12<-ggplot.themap(LUCE,"DepRate_2011",n=5,style="fixed", fixedBreaks=c(0,20,40,60,80,100))
gp13<-ggplot.themap(LUCE,"Foreigners_sh_2011",n=5,style="quantile", cl.colours=brewer.pal(5, "YlOrRd"))



#The above is for continuous data that are reclassified, we now implement
# a (simpler) function for categorical data, i.e. factors


ggplot.themap.f_beta<-function(sf,varname,
                               outline.colour="#ffce00",
                               outline.width=0.2,
                               leg.title=varname,
                               main.title=paste(substitute(sf),varname),
                               ggtheme=theme_bw()
){              
  svar<-st_drop_geometry(sf)[,varname]
  #The ggplot default is to use scale_fill_hue as a default,
  # which means picking colours at regular interval of a 360° colours wheel 
  # sc.f is does not necessary except for usinf the leg.title
  sc.f<-scale_fill_hue(name = leg.title) 
  
  themap<-ggplot()+
    geom_sf(data=sf,aes(fill=svar),
            colour=outline.colour,
            size=outline.width)+
    sc.f+
    ggtheme+
    ggtitle(label=main.title)
  return(themap)
}

ggplot.themap.f_beta(LUCE,"IDCANT_116")


#We now modify to take any palette we would like, given the number of 
# used levels in the factor

#Number of categories can for example be retrieved by
length(unique(LUCE$CANT_116))

ggplot.themap.f<-function(sf,varname,
                               cl.colours=NULL,
                               outline.colour="#ffce00",
                               outline.width=0.2,
                               leg.title=varname,
                               main.title=paste(substitute(sf),varname),
                               ggtheme=theme_bw()
){              
  svar<-st_drop_geometry(sf)[,varname]
  sc.f<-if (!is.null(cl.colours)) { #check if there is a color palette provided
    scale_fill_manual(values=cl.colours, name = leg.title)
  } else {
    scale_fill_hue(name = leg.title)
    }
  themap<-ggplot()+
    geom_sf(data=sf,aes(fill=svar),
            colour=outline.colour,
            size=outline.width)+
    sc.f+
    ggtheme+
    ggtitle(label=main.title)
  return(themap)             #  Finally what it returns
}

ggplot.themap.f(LUCE,"CANT_116") #This is the same as prviously
ggplot.themap.f(LUCE,"CANT_116", cl.colours = legocolors::legocolors[2:13,]$hex, outline.colour="white")
ggplot.themap.f(LUCE,"CANT_116", cl.colours = brewer.pal(12, "Set3"))

#If a named vector is provided fo colors, then you can enforce a colour to a value:
district.colours=c("Grevenmacher" = "darkgreen", "Luxembourg" = "darkred", "Diekirch"="darkorange", "Wallonie"="blue") 
ggplot.themap.f(LUCE,"DISTRICT_1", cl.colours = brewer.pal(3, "Set3"))
ggplot.themap.f(LUCE,"DISTRICT_1", cl.colours = district.colours)

#As we know Wallonie is not a valid district and thus is simply ignored.
# This is going to be useful for mapping when you don't know before if the value is present or not (e.g. LISA maps)


#Adapting sp based codes to use sf to compute LISA's

#Spatial weights
#based on knn, requires x,y coordinates of polygon 
# see https://r-spatial.github.io/spdep/articles/nb_sf.html#summary
# and https://r-spatial.github.io/sf/reference/geos_unary.html


#Create neighbours lists:
#knn
LUCE_pt_sf<-st_centroid(LUCE)
LUCE_knn<-spdep::knearneigh(LUCE_pt_sf, k=4)
LUCE_k4_nb<-spdep::knn2nb(LUCE_knn)
LUCE_k10_nb<-spdep::knn2nb(knearneigh(LUCE_pt_sf, k=10))
plot(LUCE_k10_nb,coords=st_geometry(LUCE_pt_sf))
#plot(LUCE_k10_nb,coords=st_coordinates(LUCE_pt_sf))#same
plot(LUCE_k4_nb,coords=st_geometry(LUCE_pt_sf), col="green", add=TRUE)

#or based on some sf functions:
 #contiguity
LUCE_touch<-sf::st_touches(LUCE)
#within distance !!slow be careful or quicker through points:

#LUCE_within5km<-st_is_within_distance(LUCE, dist=5000) #SLOW
LUCE_pt_within5km<-st_is_within_distance(LUCE_pt_sf, dist=5000)

#Both result in a sgbp object. Not a nb list as defined in spdep
#I think there is no built in function to convert but one is suggested by R Bivand:
# https://cran.r-hub.io/web/packages/spdep/vignettes/nb_sf.html
as.nb.sgbp <- function(x, ...) {
  attrs <- attributes(x)
  x <- lapply(x, function(i) { if(length(i) == 0L) 0L else i } )
  attributes(x) <- attrs
  class(x) <- "nb"
  x
}

LUCE_touch_nb<-as.nb.sgbp(LUCE_touch)

plot(LUCE_k10_nb,coords=st_geometry(LUCE_pt_sf))
plot(LUCE_k4_nb,coords=st_geometry(LUCE_pt_sf), col="green", add=TRUE)
plot(LUCE_touch_nb,coords=st_geometry(LUCE_pt_sf), col="blue", add=TRUE)

#From nb lists to spatial weights:
#row-standardised weight matrix (from contiguity)
LUCE_k4_W<-nb2listw(LUCE_k4_nb,style="W")
summary(LUCE_k4_W)
LUCE_touch_W<-nb2listw(LUCE_touch_nb,style="W",zero.policy = TRUE ) #One municipality do not touch others...? TBC
summary(LUCE_touch_W,zero.policy = TRUE )

#Global Moran
x<-LUCE$Foreigners_sh_2011
#x<-LUCE$Inhab_km2_2011
mylistw<-LUCE_k4_W

moran_i<- spdep::moran.test(x, listw=mylistw)
moran_i
#Note that Moran's I results from regressing the spatial lag of a variable by the variable,
#both expressed in deviations to mean
dev_x<-x-mean(x) #express variable as deviation to mean
spatial_lag_dev_x<-lag.listw(mylistw,dev_x) #lag the deviation to mean
morlm <- lm(spatial_lag_dev_x ~ dev_x)
morlm_itcpt<- morlm$coefficients[1]
morlm_i <- morlm$coefficients[2] #retrieves the slope

#Compare:
morlm_i
moran_i$estimate[1]

# same as below since x and list of weights defined above
moran.plot(x, listw=mylistw)

#Note that Moran's scatterplot can be constructed as
plot(dev_x,spatial_lag_dev_x)
abline(morlm_itcpt,morlm_i,col=2)
abline(h=0,lty=2,col=4)
abline(v=0,lty=2,col=4)

#Computing Local Moran’s I
mIlocal_x<- localmoran(x, listw=mylistw)
head(mIlocal_x)

#Turns LISA output as a dataframe
LISA_x<-as.data.frame(mIlocal_x)
names(LISA_x)

#Note that LISA statistics Ii is not the spatial lag
# but the product of x and the spatial lag of x
# (both in deviations and multiplied by a constant)
# and so is positive for HH and LL quadrants and negative otherwise
# as one can see from following plot
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
LUCE_LISA<-cbind(LUCE,LISA_x)
LISA.colours=c("n.s." = "white", "L.L." = "darkblue", "L.H."="lightblue", "H.L." = "pink", "H.H."="darkred") 

ggplot.themap.f(LUCE_LISA,"signif_quad", cl.colours = LISA.colours)


#To simplify a little the process,
localmoran.withquads<-function(var,w,signif=0.05){ #Takes variable and list of weights
  LISA_x<-as.data.frame(localmoran(x, listw=mylistw))
  LISA_x$quad<-factor("n.s", levels=c("n.s.","L.L.","L.H.","H.L.","H.H."))
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x<0] <- "L.L."
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x>0] <- "L.H."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x<0] <- "H.L."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x>0] <- "H.H."
  LISA_x$signif_quad<-LISA_x$quad
  LISA_x$signif_quad[LISA_x[,5]>signif]<-"n.s."  #zero to n.s lisa types
  return(cbind(var,LISA_x))
} 

LISA.colours=c("n.s." = "white", "L.L." = "darkblue", "L.H."="lightblue", "H.L." = "pink", "H.H."="darkred") 

LISA_foreigners<-localmoran.withquads(LUCE$Foreigners_sh_2011, LUCE_k4_W, signif=0.05)
LUCE_LISA_foreigners<-cbind(LUCE,LISA_foreigners) #append to the origin sf object
ggplot.themap.f(LUCE_LISA_foreigners,"signif_quad", cl.colours = LISA.colours)


#For more:
#See https://spatialanalysis.github.io/handsonspatialdata/global-spatial-autocorrelation-2.html
#And https://cran.r-project.org/web/packages/rgeoda/rgeoda.pdf
#
#
#
#

my1stfun<-function(a,b,c=3){
  res<-(a+b)/c
  return(res)
  }


