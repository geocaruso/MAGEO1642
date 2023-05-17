ggplot.themap_beta<-function(sf,varname, #Aguments in parentheses,
                             cl.style="quantile",
                             n.classes=5,
                             low.colour="lightyellow",
                             high.colour="darkred",
                             cl.colours=attr(findColours(cl.intvl, c(low.colour,high.colour)),"palette"),
                             cl.breaks=NULL,
                             outline.colour="#ffce00",
                             outline.width=0.2,
                             n.digits=2,
                             leg.title=varname,
                             main.title=paste(substitute(sf),varname),
                             sub.title=cl.style,
                             ggtheme=theme_bw()
){              #  then what the function does
  svar<-st_drop_geometry(sf)[,varname]
  cl.intvl<-classIntervals(svar, n.classes,style=cl.style,fixedBreaks=cl.breaks)
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
  return(themap)             #  Finally what it returns
}
