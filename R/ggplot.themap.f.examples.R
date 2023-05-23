#' ggplot.themap.f.examples.R
#' 
#' Examples of using ggplot.themap.f with Luxembourg data

#function
source("R/ggplot.themap.f.R")

#data
lux102sf_density1821<-readRDS("data/lux102sf_density1821.rds")

p<-ggplot.themap.f(lux102sf_density1821,"CANTON") #default colours and titles
p

# with Color Brewer or Lego bricks colours
pb<-ggplot.themap.f(lux102sf_density1821,"CANTON",
                cl.colours = RColorBrewer::brewer.pal(12, "Set3"),
                main.title="Luxembourg cantons",leg.title=NULL)
pb

plego<-ggplot.themap.f(lux102sf_density1821,"CANTON",
                cl.colours = legocolors::legocolors[2:13,]$hex,
                outline.colour="white",
                main.title="Luxembourg cantons",leg.title=NULL)
plego

#If a named vector is provided, enforces a match of values and colours
pb2<-ggplot.themap.f(lux102sf_density1821, "DISTRICT", cl.colours = RColorBrewer::brewer.pal(3, "Set3"))
pb2

district.colours=c("Grevenmacher" = "darkolivegreen1", "Luxembourg" = "darkolivegreen3", "Diekirch"="darkolivegreen4", "Wallonie"="orange") 
pdistr<-ggplot.themap.f(lux102sf_density1821, "DISTRICT",
                        cl.colours = district.colours,
                        main.title="Luxembourg former districts", leg.title=NULL)
#As we know Wallonie is not a valid district of Luxembourg and is thus ignored.
# This is going to be useful for mapping when you don't know before if a category is present or not (e.g. LISA maps)
pdistr

#Since the retuned outpu is a ggplot object, we can still modify ex-post:
#For example removing the legend:
pdistr+ theme(legend.position = "none")
# This case is useful if one maps many categories,
#  such as all communes
pcom<-ggplot.themap.f(lux102sf_density1821,"LAU2",
                        main.title="Luxembourg 102 communes")
#The legend would take up the whole map space, so:
pcom<-pcom+ theme(legend.position = "none")

#Note, theoretically the map could work with only 4 colours.
#The minimum number of colours is implemented in tmap.
# tm_shape(lux102sf_density1821) +
#  tm_polygons(col = "MAP_COLORS", minimize = TRUE)


pdf(file="output/Lux_atlas_categorical.pdf")
print(p)
print(pb)
print(plego)
print(pb2)
print(pdistr)
print(pcom)
dev.off()

