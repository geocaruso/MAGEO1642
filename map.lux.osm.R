#map.lux.osm.R
#
#Demo of how to retrieve osm data from R, bothe vector data and tiles
#Demo for each municipality of Luxembourg

#Data: 102 municipalities from geopackage WGS84----
LUX<-sf::st_read("data/Communes102_4326.gpkg")

LUX[,"Xcentro"]<-sf::st_coordinates(sf::st_centroid(LUX))[,"X"]
LUX[,"Ycentro"]<-sf::st_coordinates(sf::st_centroid(LUX))[,"Y"]

#Map of municipalities with label at centroid----
p_lab<-ggplot2::ggplot(data=LUX)+
  ggplot2::geom_sf(fill="white",col='grey')+
  ggplot2::geom_text(ggplot2::aes(x=Xcentro,
                          y=Ycentro,
                          label=COMMUNE),size=1.5)+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.title = ggplot2::element_blank())
p_lab

pdf("output/Lux_map_labels.pdf")
print(p_lab)
dev.off()

#We loop over each municipality to make a map of municipality per page:
pdf(file="output/Lux_atlas_osm.pdf")
for (i in 1:dim(LUX)[1]) {
  iLUX<-LUX[i,] #we subset the i'th observation (municipality)
  iname<-LUX[i,"COMMUNE"]
  # keeping all the attributes including the geometry
  myplot_of_i<-ggplot2::ggplot()+
    ggplot2::geom_sf(data=iLUX,fill="white",col='grey')+
    ggplot2::theme_bw()+
    ggplot2::ggtitle(iname)
  message(paste(i," out of", dim(LUX)[1]))
  print(myplot_of_i)
  #st_write(iLUX,paste0("OUT/GPKG/i",i,".gpkg")) #Don't want this every time
}
dev.off()

#Atlas with OSM vector data----
# Enrich our atlas after retrieving some vector features (buildings and tress)
#  as a sf 

pdf(file="output/Lux_atlas_osm.pdf") #May take a few minutes
for (i in 1:dim(LUX)[1]) {
  iLUX<-LUX[i,] #we subset the i'th observation (municipality)
  # keeping all the attributes including the geometry
  iname<-LUX[i,"COMMUNE"]

  iopq <- osmdata::opq(bbox = sf::st_bbox(iLUX)) #function from osmdata pkg
  iquery1<-osmdata::add_osm_feature(opq=iopq,key = "building")
  iquery2<-osmdata::add_osm_feature(opq=iopq,key = "natural", value="tree")
  iosm1<-osmdata::osmdata_sf(iquery1)
  iosm2<-osmdata::osmdata_sf(iquery2)
  
  ibuildings<-iosm1$osm_polygons #from list of objects, keeps only polygons of buildings
  itrees<-iosm2$osm_points #from list of objects, keeps only points of trees
  #Add a field when the feature "overlaps" (intersects) the polygon the municipality
  #TRUE if intersects, FALSE if in the bbox but beyond the borders
  ibuildings[,"itsct"] <- sf::st_intersects(x = ibuildings, y = iLUX, sparse = FALSE)
  itrees[,"itsct"] <- sf::st_intersects(x = itrees, y = iLUX, sparse = FALSE)
  
  ibuildings_in<-ibuildings[sf::st_drop_geometry(ibuildings[,"itsct"])==TRUE,]
  itrees_in<-itrees[sf::st_drop_geometry(itrees[,"itsct"])==TRUE,]
  
    myplot_of_i<-ggplot2::ggplot()+
    ggplot2::geom_sf(data=iLUX,fill="white",col='grey')+
    ggplot2::geom_sf(data=ibuildings_in,col="darkgrey")+
    ggplot2::geom_sf(data=itrees_in ,col="darkgreen", size=0.1)+
    ggplot2::theme_bw()+
    ggplot2::ggtitle(paste(iname,nrow(ibuildings_in),"buildings and ", nrow(itrees_in),"trees"))
  myplot_of_i
  
  message(paste(i," out of", dim(LUX)[1]))
  print(myplot_of_i)
}
dev.off()


#OSM tiles----
# (images rather than vector objects)

osm <- list(src = "OSM",
            q = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
            sub = c("a", "b", "c"),
            cit = "© OpenStreetMap contributors.")
i=11 #Esch
LUX[i,] #subset
iosmtile <- maptiles::get_tiles(LUX[i,], provider=osm,crop = TRUE, zoom = 12)

#terra::plotRGB(iosmtile)#Plotting a SpatRaster, an object made by terra
#but we do all our maps with ggplot so we want to plot with the tidyterra 

p<-ggplot2::ggplot()+
  tidyterra::geom_spatraster_rgb(data = iosmtile)+ #this geom comes with tidyterra
  ggplot2::geom_sf(data=LUX[i,],fill=NA,col='orange',lwd=2)+
  ggplot2::theme_bw()
p

