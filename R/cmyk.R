# Load necessary libraries
library(sf)
library(ggplot2)

circles <- list(
    st_buffer(st_point(c(0.5, 0.9)), dist = 0.3),
    st_buffer(st_point(c(0.3, 0.6)), dist = 0.3),
    st_buffer(st_point(c(0.7, 0.6)), dist = 0.3)
    )
circles_sfc <- st_sfc(circles)
maincolors <- c("yellow","cyan","magenta" )

circles_sf <- st_sf(geometry = circles_sfc, maincolors = maincolors)

ggplot(data = circles_sf[1:3,]) +
  geom_sf(aes(fill = maincolors), alpha = 1)+
  scale_fill_identity()
  #scale_fill_manual(values=maincolors, name=maincolors)
  
intersections_sf <- st_intersection(circles_sf)
intersections_sf$secondcolors<-c("yellow","green","cyan","blue","grey90","red","magenta")
      
cmyk_sf<-cbind(intersections_sf,st_coordinates(st_centroid(intersections)))

ggplot(data = cmyk_sf) +
  geom_sf(aes(fill = secondcolors), alpha = 1)+
  scale_fill_identity()+
  geom_text(aes(label=secondcolors,x=X,y=Y))+
  theme_void()

# Applied to represent spatial reg models:

cmyk_spatialreg<-cmyk_sf
cmyk_spatialreg$param<-c("\u03c1Wy",
                         "\u03c1Wy; \u03b8WX",
                         "\u03b8WX",
                         "\u03b8WX; \u03bbW\u03bc",
                         "\u03c1Wy; \u03b8WX; \u03bbW\u03bc",
                         "\u03c1Wy; \u03bbW\u03bc",
                         "\u03bbW\u03bc")
cmyk_spatialreg$Acronym<-c("SLM","SDM","SLX","SDEM","GNS","SAC","SEM")
cmyk_spatialreg$Name<-c("Spatial Lag Model",
                        "Spatial Durbin Model",
                        "Spatial Lagged X Model",
                        "Spatial Durbin \nError Model",
                        "General Nested \nSpatial Model",
                        "Combined Spatial \nAutocorrelation Model",
                        "Spatial Error Model")
cmyk_spatialreg$aka<-c("Spatial Auto Regressive Model (SAR)",
                        "",
                        "",
                        "",
                        "",
                        "Spatial AutoRegressive - AutoRegressive model (SARAR)",
                        "")
cmyk_spatialreg$Estim<-c("lagsarlm(...)",
                        "lagsarlm(..., Durbin=TRUE)",
                        "lmSLX(..., Durbin=TRUE i.e. default)",
                        "errorsarlm(..., Durbin=TRUE)",
                        "sacsarlm(..., Durbin=TRUE)",
                        "sacsarlm(...)",
                        "errorsarlm(...)")


spatialreg_cmyk<-ggplot(data = cmyk_spatialreg) +
  geom_sf(aes(fill = shades::brightness(secondcolors, 0.9)), color=NA)+
  scale_fill_identity()+
  geom_text(aes(label=param,x=X,y=Y-0.05), col="grey20",size=5, fontface = "bold")+
  geom_text(aes(label=Name,x=X,y=Y+0.01), size=5)+
  geom_text(aes(label=Acronym,x=X,y=Y+0.06), size=8, fontface = "bold")+
  geom_text(aes(label=Estim,x=X,y=Y-0.03), fontface = "italic",col="black")+
  labs(title="The family of cross-section Spatial Regression Models",
       subtitle="y = \u03b1 + \u03c1Wy + \u03b2X + \u03b8WX + \u03bc   with \u03bc=\u03bbW\u03bc + \u03b5",
       caption="Wy,WX,W\u03bc are spatially lagged y, X, \u03bc (see spdep::nb2listw() and lag.listw()). \n\u03c1,\u03b8, \u03bb are their respective coefficients (see spatialreg package). \nUse spatialreg::impacts for marginal effects as soon as \u03c1 or \u03b8 is included so that simultaneity is accounted for.")+
  theme_void()+
  theme(plot.subtitle = element_text(size = 15, hjust = 0.5, face="bold"), 
        plot.title = element_text(size = 20, hjust = 0.5,face="bold"),
        plot.caption = element_text(size = 12, hjust = 0))+
  geom_textcurve(aes(x=0.2, y=0.93, xend = 0.8, yend = 0.93), hjust = 0.5, 
               curvature = -0.85, label = "Dependent variable spillover", col="goldenrod", linewidth = 0, size=6)+
  geom_textcurve(aes(x=0.05, y=0.8, xend = 0.5, yend = 0.38), hjust = 0.5, 
               curvature = 0.85, label = "Covariates spillover", col="darkblue", linewidth = 0, size=6)+
  geom_textcurve(aes(x=0.45, y=0.38, xend = 0.95, yend = 0.8), hjust = 0.5, 
                 curvature = 0.85, label = "Unobserved spillover", col="pink", linewidth = 0, size=6)+
  geom_point(size = 20, x=0.35, y=1.26, pch = 19,col=shades::brightness("yellow", 0.9))+
  geom_point(size = 20, x=0.49, y=1.26, pch = 19,col=shades::brightness("cyan", 0.9))+
  geom_point(size = 20, x=0.69, y=1.26, pch = 19,col=shades::brightness("magenta", 0.9))

spatialreg_cmyk

png("output/spatialreg_cmyk.png",width=1440, height=1400, res = 144)
print(spatialreg_cmyk)
dev.off()

