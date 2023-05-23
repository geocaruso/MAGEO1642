#' ggplot.themap.R
#' 
#' A function to make choropleth maps for continuous data using
#' an sf input where the attributes to be mapped are included in the dataframe.
#' Uses ggplot2 (scale_fill_manual) and classInt for flexible discretization
#' (and allowing for own (fixed) breaks and colours to be set)
#' 
#' @param sf A polygon sf including the variable to be mapped (choropleth)
#' @param varname Name of variable to be mapped
#' @param n Number of classes 
#' @param style A discretization style as used in classInt::classIntervals
#' @param fixedBreaks If style is "fixed", breaks are chosen and input here
#'  (similar to classInt::classIntervals)
#' @param low.colour Polygon fill colour for lower end values.
#'  Other colours interpolated up to high.colour. See classInt::findColours
#' @param high.colour Polygon fill colour for upper end values
#' @param cl.colours To supply a full list of n colours rather than low and high colours
#' @param outline.colour Polygon outline colour
#' @param outline.width Polygon outline width
#' @param n.digits Digits for legend display
#' @param leg.title Character string for legend title
#' @param main.title Character string for map title
#' @param sub.title Character string for map sub title
#' @param ggtheme ggplot theme
#'
#' @return A ggplot object using geom_sf for polygons and scale_fill_manual
#'  for rendering the discretized values
#'  
#' @export
#' @examples See ggplot.themap.examples.R
#'
ggplot.themap<-function(sf,varname,
                        n=5, style='quantile', fixedBreaks=NULL,
                        low.colour="lightyellow",
                        high.colour="darkred",
                        cl.colours=attr(classInt::findColours(cl.intvl, c(low.colour,high.colour)),"palette"),
                        outline.colour="#ffce00",
                        outline.width=0.2,
                        n.digits=2,
                        leg.title=varname,
                        main.title=paste(substitute(sf),varname),
                        sub.title=attr(cl.intvl, "style"),
                        ggtheme=ggplot2::theme_bw()
                        ){
  svar<-sf::st_drop_geometry(sf)[,varname]
  cl.intvl<-classInt::classIntervals(svar, n=n, style=style, fixedBreaks=fixedBreaks)
  n.classes<-length(cl.intvl$brks)-1
  cl.value<-factor(classInt::findCols(cl.intvl))
  leg.labels<-paste(format(round(cl.intvl$brks[1:n.classes],digits=n.digits), nsmall=2),
                    format(round(cl.intvl$brks[2:(n.classes+1)],digits=n.digits), nsmall=2),
                    sep=" - ")
  sc.f<-ggplot2::scale_fill_manual(name = leg.title,
                          breaks = seq(1:n.classes),
                          values=cl.colours,
                          labels=leg.labels)
  themap<-ggplot2::ggplot()
  themap<-themap+ggplot2::geom_sf(data=sf,ggplot2::aes(fill=cl.value),
            colour=outline.colour,
            size=outline.width)
  themap<-themap+sc.f
  themap<-themap+ggtheme
  themap<-themap+ggplot2::ggtitle(label=main.title, subtitle=sub.title)
  
  return(themap)
}

