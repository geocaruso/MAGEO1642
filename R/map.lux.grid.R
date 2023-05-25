#map.lux.grid.R

#Script to obtain the 1km grid (squared polygons) for Luxembourg from European
# reference grid. The grid is limited to the Luxembourg borders and intersected
#  wiht the municipalities (LAU2) to build correspondence tables
#   (one cell - many municipalities and one cell - one municipality based on max surface)
#   

#1. Data----
# The Lux 1km grid can be downloaded as a zipped shapefile from
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

#Map of municipalities and eea grids
p<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=lucom3035,fill="darkgrey",col='yellow',)+
  ggplot2::geom_sf(data=lu100km_eea,fill=NA,col='darkblue')+
  ggplot2::geom_sf(data=lu10km_eea,fill=NA,col='blue')+
  ggplot2::geom_sf(data=lukm_eea,fill=NA,col='lightblue')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle("EEA grids Luxembourg (1,10, 100km)")
p

#2. Subsetting to Lux territory----
#As one can see the eea data is buffered around Luxembourg.
# Many cells are not needed.
# Select only those 1km overlapping Luxembourg communes:

lukm_itsct<-sf::st_intersects(lukm_eea, lucom3035, sparse=FALSE)
# Note this is a 7186 cells X 102 communes matrix (because of sparse=FALSE)
# Cells with rowSums >0 are thus overlapping and to be kept
lukm_eea[,"n_intscts"]<-rowSums(lukm_itsct) #number of intersecting communes by each cell
lukm_eea$n_intscts_f<-factor(lukm_eea$n_intscts)#number of intersecting communes by each cell as factor

lukm = lukm_eea[lukm_eea$n_intscts>0,]
#dim(lukm) # 2794 grid cells


p2<-ggplot2::ggplot()+
  ggplot2::geom_sf(data=lucom3035,fill="darkgrey",col='yellow')+
  ggplot2::geom_sf(data=lukm,fill=NA,col='darkgreen')+
  ggplot2::theme_bw()+
  ggplot2::ggtitle("EEA 1km grids intersecting Luxembourg")
p2

#Save as a gpkg for external use
sf::st_write(lukm,"output/lukm3035.gpkg", delete_dsn=TRUE) #Warning to be checked
#Save as a gpkg for R use
saveRDS(lukm,"output/lukm3035.RDS")

#map number of communes per cell
source("R/ggplot.themap.f.R")
p3<-ggplot.themap.f(lukm,"n_intscts_f",main.title = "Number of communes intersecting each cell")
p3b<-p3+ggplot2::geom_sf(data=lucom3035,fill=NA,col='white')
p3b

#3. Tabulating areas and correspondence tables ----
##3.1. Intersection----
# to get two-ways "tabulation of areas" between cells and communes

lucom3035[,"com_m2"]<-sf::st_area(lucom3035) #adds surface of commune
luintsection<-sf::st_intersection(lukm,lucom3035) #intersects
luintsection[,"itsct_m2"]<-sf::st_area(luintsection) #adds surface of intersected
luintsection[,"share_of_com"]<-as.numeric(luintsection$itsct_m2/luintsection$com_m2) #surface share of commune
luintsection[,"share_of_cell"]<-as.numeric(luintsection$itsct_m2/1000000) #surface share of cell

source("R/ggplot.themap.R")
p4<-ggplot.themap(luintsection,"share_of_com",n=8, n.digits = 4,
              cl.colours = viridis::inferno(8))
p4

##3.2. Keep shares in wide and unique corresp to build corresp table
wide_cell_LAU2_m2<-reshape2::dcast(sf::st_drop_geometry(luintsection),CELLCODE~LAU2,value.var = "itsct_m2")
wide_cell_LAU2_m2[is.na(wide_cell_LAU2_m2)] <- 0 #could use na rm later but won't work with col.max
rowSums(wide_cell_LAU2_m2[,2:103]) #to check all rows sum to 1
 #OK except external orders OK
colSums(wide_cell_LAU2_m2[,2:103]) #to check sums are surface of communes
 #OK see lucom3035$com_m2 (re-ordered)

##3.3. Select max LAU2 and add as a column plus cells contribution (share)
maxm2LAU2<-names(wide_cell_LAU2_m2)[max.col(wide_cell_LAU2_m2[,2:103])+1]
wide_cell_LAU2_m2$maxm2LAU2<-maxm2LAU2 
#with frequency of LAU2s across cells compute 1/freq to get contribution share
# assumes equal contribution even is surface is not entirely in cell,
# to be consistent with each cell being allocated to a single LAU2
Each_share_of_LAU2<-data.frame(1/table(wide_cell_LAU2_m2$maxm2LAU2))
names(Each_share_of_LAU2)<-c("LAU2","sh_of_LAU2")
#Merge LAU2 shares to cells
m_w_s<-merge(wide_cell_LAU2_m2,Each_share_of_LAU2,by.x="maxm2LAU2",by.y="LAU2",)

#rearrange (CELLCODE then maxLAU2 and share before tabulate area matrix)
Grid1km_LAU2_m2_1<-m_w_s[,c(2,1,105,3:104)]
names(Grid1km_LAU2_m2_1)[4:105]<-paste0("m2_",names(Grid1km_LAU2_m2_1)[4:105])

#reorder with lukm order
Grid1km_LAU2_m2<-Grid1km_LAU2_m2_1[match(lukm$CELLCODE,Grid1km_LAU2_m2_1$CELLCODE),]
write.csv(Grid1km_LAU2_m2,"output/Grid1km_LAU2_m2.csv")

##4. Printing
pdf("output/Lux_grids_map.pdf")
print(p)
print(p2)
print(p3b)
print(p4)
dev.off()
