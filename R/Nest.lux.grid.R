#Nest.lux.grid.R
#
#Creates a 100m (or any res multiple of 1000m) grid within the 1km grid
# (as defined by the EU)
# Case of Luxembourg

#data
lukm<-readRDS("output/lukm3035.RDS")
ingrid<-lukm[1:10,]# smaller subsample
ingrid<-lukm# smaller subsample
p<-ggplot()+geom_sf(data=ingrid,fill=NA,colour="blue")

#get the 5 coords pairs for each base grid cell
dfcoords<-data.frame(sf::st_coordinates(ingrid))
splitted<-split(dfcoords,dfcoords$L2) #dfcoords$L2 identifies polygons
names(splitted)<-paste0("IN",ingrid$CELLCODE) #reuse base cells names with IN first to avoid numbers ID

#sq.polyg gives the 5 coordinates pairs of the new (smaller) square polygons
#given the requested resolution r and list of base polygons origin x and origin y
sq.polyg<-function(x,y,base_r=1000,r=250){
  f<-base_r/r #division factor (must be integer)
  xs<-seq(from=x, to=x+base_r-r, by=r)
  ys<-seq(from=y, to=y+base_r-r, by=r)
  xsys<-cbind(rep(xs,f),rep(ys,each=f))
  p<-cbind(xsys, #starting point
           xsys[,1],xsys[,2]+r,   #2nd point
           xsys[,1]+r,xsys[,2]+r, #3rd point
           xsys[,1]+r,xsys[,2],   #4th point
           xsys)                  #end point is start point
}

newsquares<-lapply(splitted,function(s){
  sq.polyg(x=s$X[1],y=s$Y[1])
})

#row2poly transform a set of 5 coordinates in a row matrix into a
# polygon geometry (crs will be added later)
row2poly <- function(x) {
  m <- matrix(x, ncol = 2, byrow = TRUE)
  poly <- sf::st_polygon(list(m))
  return(poly)
}

#applies row2poly to newsquares and transform each row into
# an sf object (st_sfc then st_sf) per previous feature (list of sf)

sflist<-lapply(newsquares,function(a){
  polya<-apply(a, 1, function(x) {row2poly(x)})
  st_sf(st_sfc(polya))
  })

fullsf<- do.call(rbind, sflist)
sf::st_crs(fullsf)<-sf::st_crs(lukm)

p2<-ggplot()+
  geom_sf(data=fullsf, fill=NA)+
  geom_sf(data=ingrid,fill=NA,colour="blue",linewidth=1)
p2
