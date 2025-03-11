#ggplot sf and classInt for discretization

#data
lux102sf_density1821<-readRDS("data/lux102sf_density1821.rds")

# Compare ggplot2 (and other continuous ) mapping may be misleading.

g<-ggplot()+
  geom_sf(data=lux102sf_density1821,
          aes(fill=Density2023))
g

# Thematic cartographers like to discretize continuous values in classes that 
#  make sense given the distribution of hte variable or for comparability with
#  other maps.
#  This is the point of the classInt package. If you are used to mapping
#  with esri software or qgis, you will find classInt has all the discretization
#  methods you would want and more.

#Example with one of the density in
varname<-"Density2023"
svar<-sf::st_drop_geometry(lux102sf_density1821)[,varname]

#quantile
cl.intvl<-classInt::classIntervals(svar,
                                   n=5,
                                   style="quantile")
cl.intvl

#
cl.intvl<-classInt::classIntervals(svar,
                                   n=5,
                                   style="quantile")
cl.intvl




