
#Data zipped tif (6.5 Gb) population 100m resolution global scale
#
#url_pointer<-"https://data.jrc.ec.europa.eu/dataset/2ff68a52-5b5b-4a22-8f40-c41da8332cfe#dataaccess"
#url_data<-"https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_54009_100/V1-0/"
local_path<-"/Users/geoffrey.caruso/Dropbox/GEOF_DATABASE/WORLD/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0/GHS_POP_E2020_GLOBE_R2023A_54009_100_V1_0.tif"
local_path2<-"/Users/geoffrey.caruso/Dropbox/GEOF_DATABASE/LUCAS/params/"
local_path3<-"/Users/geoffrey.caruso/Dropbox/GEOF_DATABASE/LUCAS/zonalsums/"

pop_world<-terra::rast(local_path)

gpkg.lst<-list.files(local_path2)
citynames<-substr(gpkg.lst,start = 1,stop = 7)

library(ggplot2)

lapply(citynames,function(x){
    message(x)
  isf0<-sf::st_read(paste0(local_path2,x,".gpkg"))
  isf<-isf0[,c(1,5,6)]
  isf_Mollw<-sf::st_transform(isf,crs = sf::st_crs(POPWORLD))
  zonalpop<-terra::zonal(pop_world,vect(isf_Mollw),'sum', na.rm=FALSE)
  zonalpop_sf<-cbind(isf_Mollw,zonalpop)
  names(zonalpop_sf)[4]<-"pop_ghsl2020"
  write.csv(sf::st_drop_geometry(zonalpop_sf),file = paste0(local_path3,x,".csv"))

  #map
  sub<-zonalpop_sf[zonalpop_sf$pop_ghsl2020>1,]
  
  p<-ggplot(sub)+
    geom_sf(aes(fill=log10(pop_ghsl2020)), col=NA)+
    scale_fill_viridis_c(direction=-1,option = "inferno")+
    theme_bw()+
    ggtitle(paste(x, zonalpop_sf$fua_name[1]),subtitle = paste0("Total population = ",round(sum(zonalpop_sf$pop_ghsl2020))))
 
  pdf(file = paste0(local_path3,x,"_map.pdf"))
  print(p)
  dev.off()
  
   })
