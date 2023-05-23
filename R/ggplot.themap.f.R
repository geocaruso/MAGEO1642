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
#' @examples See ggplot.themap.f.examples.R
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

