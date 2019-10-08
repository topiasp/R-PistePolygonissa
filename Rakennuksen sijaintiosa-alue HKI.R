library(data.table)
library(jsonlite)

rm(list=ls())
# Kestaan hetken
# URLssa Uudenmaan rakennukset (01)
# Täältä saa tarvittaessa vaikka kaikki: https://www.avoindata.fi/data/fi/dataset/rakennusten-osoitetiedot-koko-suomi


URL <- 'https://www.avoindata.fi/data/dataset/cf9208dc-63a9-44a2-9312-bbd2c3952596/resource/7e0697f8-809b-4476-b378-d742a4f9aab1/download/01_osoitteet_2019-08-15.opt'

RAKENNUKSET <- fread(URL,sep=";",
                     # Veikkaa tyypit väärin
                     colClasses = 'character'
                     ) # Voi lukea read.tablellakin, mutta kestaa vielä kauemmin

# Helsingin rakennukset ja sarakkeille uudet nimet
names(RAKENNUKSET)[c(2,5,6)] <- c('sijaintikunta','pohjoiskoordinaatti','itakoordinaatti')
HKI <- RAKENNUKSET[RAKENNUKSET$sijaintikunta=='091',]

HKI$pohjoiskoordinaatti <- as.integer(HKI$pohjoiskoordinaatti)
HKI$itakoordinaatti <- as.integer(HKI$itakoordinaatti)

# avoindata:Piirijako_peruspiiri
# avoindata:Piirijako_osaalue
# avoindata:Piirijako_pienalue
# avoindata:Piirijako_suurpiiri
# Valitaan kartta-layer
KARTTA_LAYER <-  'avoindata:Piirijako_osaalue'

# URL, josta voidaan hakea alueidenrajat
# Huom. outputFormat on JSON
URL <- paste0('https://kartta.hel.fi/ws/geoserver/avoindata/wfs?service=WFS&version=1.0.0&request=GetFeature&typeName=',KARTTA_LAYER,'&outputFormat=json&srsName=EPSG:3067')

# Luetaan GEOJSON ja parsitaan se
GEOJSON_RAW <- readLines(URL,warn=F)
GEOJSON <- jsonlite::fromJSON(GEOJSON_RAW)



# Mätsätään kukin rakennus polygoniin

# Kätevämpään muotoon polygonit
geoms <- lapply(GEOJSON$features$geometry[,2], function(geom) data.frame(x=geom[1,,1],y=geom[1,,2]) )

# Bounding box
allGeoms <- do.call('rbind',geoms)
x_min <- min(allGeoms$x)
x_max <- max(allGeoms$x)
y_min <- min(allGeoms$y)
y_max <- max(allGeoms$y)


# Apurifunktio
overlaps <- function(number,range) {
  
  return(
    ( number >= range[1] &  number<= range[2])
    |
    ( number >= range[2] &  number<= range[1])
  )
  
}

# Idea täältä:
# https://www.geeksforgeeks.org/how-to-check-if-a-given-point-lies-inside-a-polygon/
geomContainsPoint  <- function(geom,point) {
    
  lineToInfX <- c(point$itakoordinaatti,x_max)#max(geom$x))
  lineToinfY <- point$pohjoiskoordinaatti
  
  ## Lead for geom
  polyPoints <- nrow(geom)
  
  geom$x_next  <-  c(geom$x[2:polyPoints],geom$x[1])
  geom$y_next  <-  c(geom$y[2:polyPoints],geom$y[1])
  
  overlappingLines <- geom[
                              overlaps(lineToinfY,geom[,c('y','y_next')]) 
                              & 
                                (
                                    
                                    overlaps(geom$x,lineToInfX)
                                    |
                                    overlaps(geom$x_next,lineToInfX)
                                )
                            ,]
  
   return(nrow(overlappingLines) %% 2 != 0)
  
}

### Testikartta

plot(NULL,xlim=c(x_min,x_max),ylim=c(y_min,y_max))
invisible(sapply(geoms,function(geom) {  lines(geom$x,geom$y,type="l") }))

## Random rakennus
point <- HKI[sample(seq(nrow(HKI)),1),]

# Piirretaan sen sijainti
points(point$itakoordinaatti,point$pohjoiskoordinaatti,col='red',pch=16)

# Etsitään ja piirretään löydetyn polygonin rajat
whereIsPoint <- geoms[ sapply(geoms,function(geom)  geomContainsPoint(geom,point)   ) ][[1]]
lines(whereIsPoint$x,whereIsPoint$y,col='blue',lwd=2)




