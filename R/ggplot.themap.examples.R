#' ggplot.themap.examples.R
#' 
#' Examples of using ggplot.themap with Luxembourg data

#function
source("R/ggplot.themap.R")

#libraries
library(ggplot2)

#data
lux102sf_density1821<-readRDS("data/lux102sf_density1821.rds")

# Compare ggplot2 basic mapping with this function
g<-ggplot()+
  geom_sf(data=lux102sf_density1821,
          aes(fill=Density2023))
g

p<-ggplot.themap(lux102sf_density1821,"Density1821")
#p<-ggplot.themap(lux102sf_density1821,"Density1821",style = "jenks",n=7)
p


# Improved map titles
myvar<-"Density1821"
p<-ggplot.themap(lux102sf_density1821,myvar,
                 leg.title= "inh. /sq. km",
                 main.title=paste("Population density",
                                  stringr::str_sub(myvar, 8)))
p

# all into pdf - quantile maps
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

# all into pdf - jenks maps
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

# all into pdf - fixed breaks to visualize increase across time with
#  single legend. ALso example of colorbrewer for palette

#make fixed breaks from log sequence
mybrks<-exp(seq(log(15), log(2600), length.out = 11)) #reproduced from lseq in emdbook pkg

pdf(file="output/Lux_atlas_density_fixed.pdf")
for (i in 6:62) {
  myvar<-names(lux102sf_density1821)[i]
  p<-ggplot.themap(lux102sf_density1821,myvar,
                   style="fixed",fixedBreaks=mybrks,
                   cl.colours=rev(RColorBrewer::brewer.pal(11, "Spectral")),
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


