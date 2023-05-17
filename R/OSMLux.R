


i=1
LUX[i,]
maptiles::get_tiles(iLUX, provider = osm, crop = TRUE, zoom = 6)


osm <- list(
  src = "OSM",
  q = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
  sub = c("a", "b", "c"),
  cit = "© OpenStreetMap contributors."
)

iLUX <- readRDS("/home/tim/Téléchargements/iLUXGeoffrey.tgz")

tiles <- get_tiles(x = iLUX, provider = osm, crop = TRUE,
                   zoom = 10, verbose = TRUE)
#> http://b.tile.openstreetmap.org/10/529/349.png => /tmp/RtmpGCAnl3/OSM_10_529_349.png
#> Zoom:10
#> Data and map tiles sources:
#> © OpenStreetMap contributors.
plot_tiles(tiles)

plot(st_geometry(iLUX), add = T)