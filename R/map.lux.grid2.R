#map.lux.grid.R

#The Lux 1km grid can be downloaded as a zipped shapefile from
#https://www.eea.europa.eu/data-and-maps/data/eea-reference-grids-2
#It is unzipped in "data/EEA_ref_grid_lux", which contains
# the 1km, 10km and 100km tiles.

#1km grid:
lukm_eea<-sf::st_read("data/EEA_ref_grid_lux/lu_1km.shp")
sf::st_crs(lukm_eea) #showing it is EPSG 3035 (Lambert Equal Area cnetered on lat 52 long 10)
#10 an 100km
lu10km_eea<-sf::st_read("data/EEA_ref_grid_lux/lu_10km.shp")
lu100km_eea<-sf::st_read("data/EEA_ref_grid_lux/lu_100km.shp")

#Municipalities for comparison after reprojection
LUX<-sf::st_read("data/Communes102_4326.gpkg")
lucom3035<-sf::st_transform(LUX,crs=sf::st_crs(lukm_eea))

#Map of municipalities and eea grids----
p<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=lucom3035,fill="darkgrey",col='yellow',)+
  ggplot2::geom_sf(data=lu100km_eea,fill=NA,col='darkblue')+
  ggplot2::geom_sf(data=lu10km_eea,fill=NA,col='blue')+
  ggplot2::geom_sf(data=lukm_eea,fill=NA,col='lightblue')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle("EEA grids Luxembourg (1,10, 100km)")
p

#As one can see the eea data is buffered around Luxembourg.
# Many cells are not needed.
# Select only those 1km overlapping Luxembourg communes:

lukm_itsct<-sf::st_intersects(lukm_eea, lucom3035, sparse=FALSE)
# Note this is a 7186 cells X 102 communes matrix (because of sparse=FALSE)
# Cells with rowSums >0 are thus overlapping and to be kept
lukm_eea[,"n_intscts"]<-rowSums(lukm_itsct) #number of intersectiong communes by each cell

lukm = lukm_eea[lukm_eea$n_intscts>0,]
#dim(lukm) # 2794 grid cells

p2<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=lucom3035,fill="darkgrey",col='yellow')+
  ggplot2::geom_sf(data=lukm,fill=NA,col='darkgreen')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle("EEA 1km grids intersecting Luxembourg")
p2

#Save as a gpkg for external use
sf::st_write(lukm,"output/lukm3035.gpkg") #Warning to be checked
#Save as a gpkg for R use
saveRDS(lukm,"output/lukm3035.RDS")

pdf("output/Lux_grids_map.pdf")
print(p)
print(p2)
dev.off()

#map number of communes per cell
p3<-ggplot2::ggplot()+
  +     ggplot2::geom_sf(data=lucom3035,fill="darkgrey",col='yellow')+
  +     ggplot2::geom_sf(data=lukm,ggplot2::aes(fill=n_intscts),col='darkgreen')
p3

lucom3035[,"com_m2"]<-sf::st_area(lucom3035) #adds surface of commune
luintsection<-sf::st_intersection(lukm,lucom3035) #intersects
luintsection[,"itsct_m2"]<-sf::st_area(luintsection) #adds surface of intersected
luintsection[,"share_of_com"]<-luintsection$itsct_m2/luintsection$com_m2 #surface share of commune
luintsection[,"share_of_cell"]<-luintsection$itsct_m2/1000000 #surface share of cell

p4<-ggplot2::ggplot()+
      ggplot2::geom_sf(data=luintsection,ggplot2::aes(fill=share_of_com),col='darkgreen')
p4
