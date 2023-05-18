#ggplot.themap.R
#Geoffrey Caruso
#
# Description----
# A function to make choropleth maps for continuous data using
#  an sf input where the attributes to be mapped are included.
#  Uses ggplot2 and classInt for flexible discretization
#   (and allowing for own (fixed) breaks and colours to be set)

# Libraries----
library(classInt)
library(sf)
library(ggplot2)
library(RColorBrewer)

# function----
ggplot.themap<-function(sf,varname,
                        n=5, style='quantile', fixedBreaks=NULL,
                        low.colour="lightyellow",
                        high.colour="darkred",
                        cl.colours=attr(findColours(cl.intvl, c(low.colour,high.colour)),"palette"),
                        outline.colour="#ffce00",
                        outline.width=0.2,
                        n.digits=2,
                        leg.title=varname,
                        main.title=paste(substitute(sf),varname),
                        sub.title=attr(cl.intvl, "style"),
                        ggtheme=theme_bw(),...
                        ){
  svar<-sf::st_drop_geometry(sf)[,varname]
  cl.intvl<-classIntervals(svar, n=n, style=style, fixedBreaks=fixedBreaks)
  n.classes<-length(cl.intvl$brks)-1
  cl.value<-factor(findCols(cl.intvl))
  leg.labels<-paste(format(round(cl.intvl$brks[1:n.classes],digits=n.digits), nsmall=2),
                    format(round(cl.intvl$brks[2:(n.classes+1)],digits=n.digits), nsmall=2),
                    sep=" - ")
  sc.f<-scale_fill_manual(name = leg.title,
                          breaks = seq(1:n.classes),
                          values=cl.colours,
                          labels=leg.labels)
  themap<-ggplot()+
    geom_sf(data=sf,aes(fill=cl.value),
            colour=outline.colour,
            size=outline.width)+
    sc.f+
    ggtheme+
    ggtitle(label=main.title, subtitle=sub.title)
  return(themap)
}

#Examples with Luxembourg data----
lux102sf_density1821<-readRDS("data/lux102sf_density1821.rds")

# Compare ggplot2 basic mapping with this function
g<-ggplot()+
  geom_sf(data=lux102sf_density1821,
          aes(fill=Density2023))
g

p<-ggplot.themap(lux102sf_density1821,"Density1821")
p

# Improved map
myvar<-"Density1821"
p<-ggplot.themap(lux102sf_density1821,myvar,
                 leg.title= "inh. /sq. km",
                 main.title=paste("Population density",
                                  stringr::str_sub(myvar, 8)))
p

# all into pdf - quantile map
pdf(file="output/Lux_atlas_density_qt.pdf")
  for (i in 6:62) {
    myvar<-names(lux102sf_density1821)[i]
    p<-ggplot.themap(lux102sf_density1821,myvar,
                     leg.title= "inh. /sq. km",
                     main.title=paste("Population density",
                                      stringr::str_sub(myvar, 8)))
    print(p)
  }
dev.off()

# all into pdf - jenks map
pdf(file="output/Lux_atlas_density_jenks.pdf")
for (i in 6:62) {
  myvar<-names(lux102sf_density1821)[i]
  p<-ggplot.themap(lux102sf_density1821,myvar, n=6,
                   leg.title= "inh. /sq. km",
                   main.title=paste("Population density",
                                    stringr::str_sub(myvar, 8)))
  print(p)
}
dev.off()

# all into pdf - fixed breaks to visualize increase

#make fixed breaks from log sequence and use colorbrewer:
mybrks<-exp(seq(log(15), log(2600), length.out = 11)) #reproduced from lseq in emdbook pkg

pdf(file="output/Lux_atlas_density_fixed.pdf")
for (i in 6:62) {
  myvar<-names(lux102sf_density1821)[i]
  p<-ggplot.themap(lux102sf_density1821,myvar,
                   style="fixed",fixedBreaks=mybrks,
                   cl.colours=rev(brewer.pal(11, "Spectral")),
                   leg.title= "inh. /sq. km",
                   main.title=paste("Population density",
                                    stringr::str_sub(myvar, 8)))
  print(p)
}
dev.off()

# Animated gif from pdf (written next to file)
pdf2animgif<-function(pdfpath,fps=1){
  mypdf<-magick::image_read_pdf(paste0(pdfpath,".pdf"))
  mygif<-magick::image_animate(mypdf,fps = fps)
  magick::image_write(mygif, path = paste0(pdfpath,".gif"))
  }

pdf2animgif("output/Lux_atlas_density_fixed")

