# get.lux.data.R
#
# Get some open data per municipalities of Luxembourg----

## Polygons----
# Download and convert to sf communal polygon vector data (geojson) from geoportail of Luxembourg:
# source: https://data.public.lu/fr/datasets/limites-administratives-du-grand-duche-de-luxembourg/

##!!! BEWARE THIS LINK HAS BEEN UPDATED TO LAST OFFICIAL RELEASE HENCE 100 COMMUNES!
  url<-"https://data.public.lu/fr/datasets/r/16103fa4-7ff1-486a-88bc-5018353957ea"
lux102sf<-sf::st_read(url) #102 because it is the latest vintage with 102 communes, crs is 4326
lux100sf<-sf::st_read(url) #100 because it is the latest vintage with 102 communes, crs is 4326

#save as geopackage for future use
sf::st_write(lux102sf,"data/Communes102_4326.gpkg", delete_dsn = TRUE)
sf::st_write(lux100sf,"data/Communes100_4326.gpkg", delete_dsn = TRUE)

## Attributes----
# Download population density since 1821! from STATEC
apiURL_1821_2023 <- "https://lustat.statec.lu/rest/data/LU1,DF_X020,1.0/.A?startPeriod=1821&endPeriod=2023"
data_1821_2023 <- data.frame(rsdmx::readSDMX(apiURL_1821_2023))
saveRDS(data_1821_2023, "data/data_1821_2023.rds")

## Cleaning attributes----
# For some reason, the 'libelles' is not given although asked for on the website,
# only a specification code is given to uniquely identify communes (and cantons)
# Hopefully the "libelles" are present via csv download.
# We use a csv with population 2015 to 2023, downloaded manually from same source
lu1<-read.csv("data/LU1_DF_X021_1.0_A..csv") 

lu<-unique(lu1[,"SPECIFICATION..Spécification"])
#from which we see starting characters "CT" is for Cantons, which we remove
lu102<-lu[stringr::str_sub(lu, 1, 2)=="CM"]
#lu102 has now 102 communes.

#A unique id is available with 7 first characters,
# while commune names start from character 10.
# We thus split lu102 in 2 columns``
CMCODE<-stringr::str_sub(lu102, 1, 7)
CMNAME<-stringr::str_sub(lu102, 10)
CMLU102<-data.frame(cbind(CMCODE,CMNAME))

# which we can merge to  columns of interest in data_1821_2023,
#, which is in long format (years as rows)
data1821sub<-data_1821_2023[,c("SPECIFICATION","obsTime","obsValue")]
# and removing also the cantons and country aggregates (selecting CM only)
data1821sub2<-data1821sub[stringr::str_sub(data1821sub[,"SPECIFICATION"], 1, 2)=="CM",]

df1821m<-merge(data1821sub2,CMLU102,by.x="SPECIFICATION", by.y="CMCODE", all.x = TRUE,sort=FALSE)
#reorganize and rename SPECIFICATION as CMCODE, obsValue as Density and obsTime as Year
df1821mo<-df1821m[,c(1,4,2,3)]
names(df1821mo)<-c("CMCODE","CMNAME","Year","Density")
#Density as numeric
df1821mo$Density<-as.numeric(df1821mo$Density)

#Cast attribute table (df1821mo) in wide format and adapt column names
df1821wide<-reshape2::dcast(df1821mo, CMCODE + CMNAME ~ Year, value.var="Density")
names(df1821wide)[-c(1,2)]<-paste0("Density",names(df1821wide)[-c(1,2)])

## Merge with sf----

# Note that the CMCODE does not match the European LAU2 convention used by the 
#  geoportail for the spatial data hence our lux102sf. Matching
#  needs to be made on names before matching CMCODE with LAU2.
#  But names are dangerous beasts, and we need to worked 3 differences
#   (out of 102) manually:
setdiff(unique(df1821wide$CMNAME), unique(lux102sf$COMMUNE))
#[1] "Lac de la Haute-Sûre" "Rosport - Mompach"   
#[3] "Redange-sur-Attert"  
setdiff(unique(lux102sf$COMMUNE),unique(df1821wide$CMNAME))
#[1] "Lac de la Haute Sûre" "Rosport-Mompach"     
#[3] "Redange"

# Manual replacement (in attributes table, i.e. using names as of geoportail)
df1821<-df1821wide
df1821[df1821[,"CMNAME"]=="Lac de la Haute-Sûre","CMNAME"]<-"Lac de la Haute Sûre"
df1821[df1821[,"CMNAME"]=="Rosport - Mompach","CMNAME"]<-"Rosport-Mompach"
df1821[df1821[,"CMNAME"]=="Redange-sur-Attert","CMNAME"]<-"Redange"

#merge with corrected names
lux102sf_density1821<-merge(lux102sf,df1821,by.x="COMMUNE",by.y="CMNAME")

## Save ----
#   sf with attributes for use in R
saveRDS(lux102sf_density1821,"data/lux102sf_density1821.rds")
#   sf as geopackage with attributes
sf::st_write(lux102sf_density1821,"data/lux102_4326_density1821.gpkg",delete_dsn=TRUE)
#   LAU2 to CMCODE correspondance table (with names) as a csv
write.csv(sf::st_drop_geometry(lux102sf_density1821[,c("COMMUNE","LAU2","CMCODE")]),"data/LAU2_CMCODE_table.csv")

