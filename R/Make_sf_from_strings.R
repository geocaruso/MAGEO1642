# Into sf----

## Making sf from strings----

### POINTS AND MULTIPOINTS----
P1<-"POINT (2.5 3.5)"
P2<-"POINT (0 5.5)"
P3<-"POINT (5 5.5)"
P4<-"MULTIPOINT ((0 2),(1 1), (4 1), (5 2))"
Pdf<-data.frame(geom=rbind(P1,P2,P3,P4))
Psf<-sf::st_as_sf(Pdf, wkt = "geom" ) #or wkt=1 since column number of geometry
Psf$id<-LETTERS[1:nrow(Psf)]
plot(Psf)

### LINES AND MULTILINES----
L1<-"LINESTRING (0 2, 1 1, 4 1, 5 2)"
L2<-"LINESTRING (2.5 3, 2.5 4)"
L3<-"MULTILINESTRING ((0 5, 0 6), (5 5, 5 6))"
Ldf<-data.frame(geom=rbind(L1,L2,L3))
Lsf<-sf::st_as_sf(Ldf, wkt = "geom" )
Lsf$id<-LETTERS[1:nrow(Lsf)]
plot(Lsf)

### POLYGONS----
POL1<-"POLYGON ((0 0, 0 7, 7 7, 7 0, 0 0))"
POL2<-"POLYGON ((0 7, 0 8, 7 8, 7 7, 0 7))"
POL3<-"POLYGON ((0 8, 3 12, 7 8, 0 8),(2 9, 3 10, 4 9, 2 9 ))" #with a hole
POLdf<-data.frame(geom=rbind(POL1,POL2,POL3))
POLsf<-sf::st_as_sf(POLdf, wkt = "geom" ) 
POLsf$id<-LETTERS[1:nrow(POLsf)]
plot(POLsf)

### Mix of geometries!----
PLsf<-rbind(Lsf,Psf) 
plot(PLsf)

## Making sf from coordinates matrix----
# A non string but matrix of coordinates approach
# See example at "https://r-spatial.github.io/sf/reference/st.html"
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
st_polyg<-sf::st_polygon(list(outer))
st_polyg
class(st_polyg)
geometryset<-sf::st_sfc(st_polyg)
poly_sf<-sf::st_as_sf(geometryset)

# or in one shot:
outer = matrix(c(0,0,10,0,10,10,0,10,0,0),ncol=2, byrow=TRUE)
poly_sf<-sf::st_as_sf(sf::st_sfc(sf::st_polygon(list(outer))))


## The Classroom----
Tatiana<-    "POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"
Yama<-       "POLYGON ((1 0, 1 1, 2 1, 2 0, 1 0))"
Catharina<-  "POLYGON ((2 0, 2 1, 3 1, 3 0, 2 0))"
Valerie<-    "POLYGON ((3 0, 3 1, 4 1, 4 0, 3 0))"
Kyrill<-     "POLYGON ((3 1, 3 2, 4 2, 4 1, 3 1))"
Fatima<-     "POLYGON ((2 1, 2 2, 3 2, 3 1, 2 1))"
Claire<-     "POLYGON ((0 1, 1 2, 0 2, 0 1))"

Classroomdf<-data.frame(geom=rbind(Tatiana,Yama,
                                   Catharina,Valerie,
                                   Kyrill, Fatima,
                                   Claire))
Classroomsf<-sf::st_as_sf(Classroomdf, wkt = "geom" ) 
Classroomsf$id<-c("Tatiana","Yama","Catharina","Valerie", "Kyrill","Fatima","Claire")
Classroomsf$id2<-1:nrow(Classroomsf)


### Display----
plot(Classroomsf[,"id"])

### Neighbours----
spdep::knearneigh(Classroomsf, k=2)

#### knn----
#Points are in need for nearest neighbours:
CentroidalStudents<-sf::st_centroid(Classroomsf)
plot(CentroidalStudents[,"id"])

k<-2 #try 3
knn_classroom<-spdep::knearneigh(CentroidalStudents, k=k)

# A bit of fine tuning to show names
knnnamed<-matrix(Classroomsf$id[knn_classroom$nn],ncol = k)
rownames(knnnamed)<-Classroomsf$id
knnnamed

# knn matrix to "list of neighbours" i.e. nblist
nblist_classroom<-spdep::knn2nb(knn_classroom)
nblist_classroom

nblist_classroom_lines<-spdep::nb2lines(nblist_classroom, coords=sf::st_geometry(CentroidalStudents), as_sf=TRUE)
plot(nblist_classroom_lines)

nblist_classroom_lines<-spdep::nb2lines(nblist_classroom, coords=sf::st_geometry(CentroidalStudents)+runif(nrow(CentroidalStudents),min = -0.3,max=0.3), as_sf=TRUE)
ggplot()+
  geom_sf(data=Classroomsf, aes(fill=factor(id2)), alpha=0.5)+
  geom_sf(data=nblist_classroom_lines, aes(col=factor(i), linewidth=i), alpha=0.6)

#### Contiguity or Distance----
# contiguity
touching<-sf::st_touches(Classroomsf)
touching
# within a distance of 1.5
d<-0 #try 0 (compare with touching), 1 etc.
withind<-sf::st_is_within_distance(Classroomsf, dist=d)
withind
#A SGBP i.e. sparse geometry binary predicate is returned !

#which is very useful but not the exact same format as required
# by spdep to make spatial weight matrices

# Conversion function as.nb.sgbp() suggested by R Bivand:
source("R/as.nb.sgbp.R")

touchingnb<-as.nb.sgbp(touching)
withindnb<-as.nb.sgbp(withind)
touchingnb
withindnb

### Spatial weights in the Classroom----
knnw<-spdep::nb2listw(nblist_classroom,style="W")  #btw This one is not symmetric
summary(knnw)
touchingw<-spdep::nb2listw(touchingnb,style="W") #W is defalut and means row standardized
summary(touchingw)

# Observe link number distribution and least and most connected

#The weight matrices are presented as list but you can also create squared (non-sparse) matrices:
spdep::nb2mat(touchingnb, style="B")
spdep::nb2mat(touchingnb, style="W")
spdep::nb2mat(nblist_classroom,style="B")
spdep::nb2mat(nblist_classroom,style="W")

#Compare also
rowSums(spdep::nb2mat(touchingnb,style="B"))
rowSums(spdep::nb2mat(nblist_classroom,style="B"))

## More about Spatial Geometry Binary Predicates (SGBP)----
# Suppose another layer
#
CofA<-"POLYGON ((0 0, 1.5 0, 1.5 3, -1 3, 0 0))"
CofB<-"POLYGON ((3.5 0, 4 0, 4 0.7, 3.5 0.7, 3.5 0))"
CoffeeLovers<-sf::st_as_sf(data.frame(geom=rbind(CofA,CofB)), wkt = "geom")
CoffeeLovers$Area<-c("A","B")

ggplot()+
  geom_sf(data=Classroomsf, aes(fill=factor(id2)), alpha=0.5)+
  geom_sf(data=CoffeeLovers, aes(fill=Area), alpha=0.7)

### Spatial Intersects----
sf::st_intersects(Classroomsf,CoffeeLovers)
sf::st_intersects(Classroomsf,CoffeeLovers, sparse = FALSE)

### Spatial Joins----
sf::st_join(Classroomsf,CoffeeLovers)
sf::st_join(CoffeeLovers,Classroomsf)

###TOPOLOGY IS THERE YOU CAN PERFORM ANY GEOPROCESSING OVERLAY OPERATIONS!!

#Distance matrices!
sf::st_distance(Classroomsf,CoffeeLovers)

#Fully within?
sf::st_within(Classroomsf,CoffeeLovers)
sf::st_within(CoffeeLovers, Classroomsf)

#Spatial intersects (cutting)including geometric processing
intsctions<-sf::st_intersection(Classroomsf,CoffeeLovers,)
intsctions
ggplot()+
  geom_sf(data=Classroomsf, fill="pink", alpha=0.5)+
  geom_sf(data=CoffeeLovers, fill="blue", alpha=0.7)+
  geom_sf(data=intsctions, fill="NA",col="red", linewidth=2)

unions<-sf::st_union(Classroomsf,CoffeeLovers)
unions
ggplot()+
  geom_sf(data=Classroomsf, fill="pink", alpha=0.5)+
  geom_sf(data=CoffeeLovers, fill="blue", alpha=0.7)+
  geom_sf(data=unions, fill="NA",col="red", linewidth=2)

symdiffs<-sf::st_sym_difference(Classroomsf,CoffeeLovers)
symdiffs
ggplot()+
  geom_sf(data=Classroomsf, fill="pink", alpha=0.5)+
  geom_sf(data=CoffeeLovers, fill="blue", alpha=0.7)+
  geom_sf(data=symdiffs, fill="NA",col="red", linewidth=2)
