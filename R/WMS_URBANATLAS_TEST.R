
url = "https://image.discomap.eea.europa.eu/arcgis/services/Corine/CLC2018_WM/MapServer/WmsServer"
leaflet() %>% 
  addWMSTiles( url, layers = "12",
               options = WMSTileOptions(format = "image/png", transparent = T)) %>% 
  setView(lng = 6, lat = 49, zoom = 8)

UAurl<-"https://image.discomap.eea.europa.eu/arcgis/services/UrbanAtlas/UA_UrbanAtlas_2018/MapServer/WMSServer?service=WMS&request=GetCapabilities&version=1.3.0"

UAurl<-"https://image.discomap.eea.europa.eu/arcgis/services/UrbanAtlas/UA_UrbanAtlas_2018/MapServer/WMSServer"
leaflet() %>% 
  addWMSTiles(UAurl, layers = "Land Use Raster",
               #options = WMSTileOptions(format = "image/png", transparent = T)
               ) %>% 
  setView(lng = 6, lat = 49, zoom = 10)



bwk_client <- ows4R::WFSClient$new(UAurl)
