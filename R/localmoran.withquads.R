localmoran.withquads<-function(sf, varname, w, signif=0.05){
  svar<-sf::st_drop_geometry(sf)[,varname]
  LISA_x<-as.data.frame(spdep::localmoran(svar, listw=w))
  dev_x<-svar-mean(svar)
  spatial_lag_dev_x<-spdep::lag.listw(w,dev_x)
  LISA_x$quad<-factor("n.s", levels=c("n.s.","L.L.","L.H.","H.L.","H.H."))
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x<0] <- "L.L."
  LISA_x$quad[dev_x <0 & spatial_lag_dev_x>0] <- "L.H."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x<0] <- "H.L."
  LISA_x$quad[dev_x >0 & spatial_lag_dev_x>0] <- "H.H."
  LISA_x$signif_quad<-LISA_x$quad
  LISA_x$signif_quad[LISA_x[,5]>signif]<-"n.s."  #zero to n.s lisa types
  return(cbind(sf,LISA_x))
} 
