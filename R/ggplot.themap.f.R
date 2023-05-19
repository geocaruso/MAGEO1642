#' ggplot.themap.f.R
#'
#' A function to make choropleth maps for categorical data using
#' an sf input where the attributes to be mapped are included in the dataframe.
#' Uses ggplot2 in a similar way as ggplot.themap (for discretized continuous
#'  data) using ggplot2::scale_fill_manual
#' 
#' @param sf A polygon sf including the variable to be mapped (choropleth)
#' @param varname Name of variable to be mapped
#' @param cl.colours To supply a vector of colours. Named vector using
#'  categories values as names to have a match of ex-ante defined colours 
#' @param outline.colour Polygon outline colour
#' @param outline.width Polygon outline width
#' @param leg.title Character string for legend title
#' @param main.title Character string for map title
#' @param sub.title Character string for map sub title
#' @param ggtheme ggplot theme
#'
#' @return
#' @export
#'
ggplot.themap.f<-function(sf,varname,
                          cl.colours=NULL,
                          outline.colour="#ffce00",
                          outline.width=0.2,
                          leg.title=varname,
                          main.title=paste(substitute(sf),varname),
                          sub.title=NULL,
                          ggtheme=ggplot2::theme_bw()
){              
  svar<-sf::st_drop_geometry(sf)[,varname]
  sc.f<-if (!is.null(cl.colours)) { #check if there is a color palette provided
    ggplot2::scale_fill_manual(values=cl.colours, name = leg.title)
  } else {
    ggplot2::scale_fill_hue(name = leg.title)
  }
  themap<-ggplot2::ggplot()
  themap<-themap+ggplot2::geom_sf(data=sf,ggplot2::aes(fill=svar),
            colour=outline.colour,
            size=outline.width)
  themap<-themap+sc.f
  themap<-themap+ggtheme
  themap<-themap+ggplot2::ggtitle(label=main.title,subtitle=sub.title)
  
  return(themap)
}

#' @examples
#Examples with Luxembourg data
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

