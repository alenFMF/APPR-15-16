## rworldmap
require(rworldmap)

## ggplot2
require(ggplot2)

## maptools (readShapeSpatial)
library(maptools)

## pretvorbe med koordinatnimi sistemi
library(rgdal)

## ggmap
library(ggmap)

## ggvis
library(ggvis)

## rgeos - readOGR
library(rgeos)

## RColorBrewer - barvne palete
library(RColorBrewer)

## Obdelava razpredelnic
library(dplyr)

## Pobiranje podatkov iz spletnih tabel
library(rvest)

library(geojsonio)
library(spdplyr)
library(rmapshaper)
library(leaflet)

#######
## paket: rworldmap
#######

map <- getMap(resolution="low")
# svet
plot(map, main="Svet", xlab="Longitude", ylab="Latitude")

# izrez Slovenija
plot(map, xlim=c(13.5,16.5), ylim=c(45,47))

# Zemljevid je predstavljen z razredom "SpatialPolygonsDataFrame"
class(map)

######
## Zemljevidi v ggplot2
# require(ggplot2)
######

map2 <- map_data("world")

# Zemljevid je predstavljen kot data.frame v posebni obliki
str(map2)

# vsaka vrstica pomeni točko v regiji. 
# poligone določa zaporedje order. 
# posamezen poligon določa skupin group

regije <- unique(map2$region)
length(regije)

# Katere regije vsebuje?
"Slovenia" %in% regije
"Yugoslavia" %in% regije
"Italy" %in% regije
# To je nekoliko star zemljevid ...

# Izris z ggplot: zapolnjeni poligoni s pobarvanim robom
ggplot(map2, aes(x=long, y=lat, group=group, colour=region)) + 
  geom_polygon() + 
  labs(title="Svet - osnovna slika") +
  theme(legend.position="none")   # ker je preveč država je privzeta legenda nesmiselna

# Zapolnjeni poligoni s pobarvano notranjostjo
ggplot(map2, aes(x=long, y=lat, group=group, fill=region)) + 
  geom_polygon() + 
  labs(title="Svet - osnovna slika") +
  theme(legend.position="none")

# Pobarvane obrobe poligonov 
ggplot(map2, aes(x=long, y=lat, group=group, colour=region)) + 
  geom_path() + 
  labs(title="Svet - osnovna slika") +
  theme(legend.position="none")



# Pretvorba SpatialPolygonsDataFrame v data.frame. Regija gre v spremenljivko "id"
map3 <- fortify(map)
str(map3)

ggplot(map3, aes(x=long, y=lat, group=group, colour=id)) +
  geom_path() + 
  coord_cartesian(xlim=c(13.2,16.7), ylim=c(45.2,47)) + # izrez
  labs(title="Svet - Rworldmap") +
  theme(legend.position="none")

##### Datoteke SHP
## Uradni slovenski poligoni
## http://www.e-prostor.gov.si/si/dostop_do_podatkov/dostop_do_podatkov/
# library(maptools)
#####

gpclibPermit()
SIob <- readShapeSpatial("OB/OB.shp")
print("Pazi! Ali si ustrezno nastavil delovno področje?")
class(SIob)

# Risanje s paketom base
plot(SIob)

# Hitro risanje z barvanjem poligonov
spplot(SIob, "POVRSINA")

# Naredimo dodaten stolpec v SpatialPolygonDataFrame
SIob$Povrsinakm2 <- SIob$POVRSINA / 1e6

# Razbitje zaloge vrednosti na 7 intervalov
brks <- quantile(SIob$Povrsinakm2, seq(0,1,1/7))

######
## Uporaba barvnih palet
# library(RColorBrewer)
######

# Barvna paleta je vektor barvnih kod
pal <- brewer.pal(8,'Greens')
pal

## Pretvorba med kodnimi tabelami
imena <- SIob$OB_UIME %>% as.character %>% iconv("cp1250", "utf8")

## Preslikava v barvno paleto
col <- pal[findInterval(SIob$Povrsinakm2,brks,all.inside=TRUE)]
# Izris z barvanjem po preslikavi v barvno paleto. Uporaba knjižnice 'base'
plot(SIob,col=col)
text(coordinates(SIob),labels=imena,cex=0.2)
title("Slovenija - velikost občin")
leg <- paste(as.integer(brks[1:7]),as.integer(brks[2:8]),sep="-")
legend("bottomright",legend=leg,fill=pal,cex=0.75)

########
# Izvažanje v PDF
#######$

## Enkrat moramo uvoziti sistemske fonte 
library(extrafont)
font_import()   # to zna trajati ...
## potem vsakič naredimo naslednje
library(extrafont)
loadfonts()

## Izris v PDF
cairo_pdf("velikost_obcin.pdf", family="Arial", width=11.7,height=8.3)
plot(SIob,col=col,bg="lightyellow",lwd=0.4, axes=TRUE)
text(coordinates(SIob),labels=imena,cex=0.4)
title("Slovenija - velikost občin")
legend("bottomright",legend=leg,fill=pal)
dev.off()


# Izberemo lahko pravokotnik izreza
plot(SIob,col=col,xlim=c(430000,490000),ylim=c(84000,125000),bg="lightyellow")
text(coordinates(SIob),labels=imena,cex=0.7)

#####
## Pretvorbe med koordinatnimi sistemi
# library(rgdal)
#####

## Koordinatni sistem WGS84 = (longitude, latitude)
WGS84 <- CRS("+proj=longlat +ellps=WGS84")

## Koordinatni sistem, ki ga uporablja GURS
EPSG3912 <- CRS("+init=epsg:3912")

# Alternativni način branja SHP datotek
map4 <- readOGR("OB", layer = "OB")
# Nastavitev koordinatnega sistema (če ga že ne uspemo prebrati)
map4@proj4string <- EPSG3912

# Transformacija v drugi koordinatni sistem
map5 <- spTransform(map4, WGS84)
obcine <- fortify(map5)

#########
## Izrisi čez Google Maps
## library(ggmap)
#########

# Pridobivanje pogleda na Google Maps
zemljevid <- get_map(location="Ljubljana, Slovenia", zoom=8, maptype="road")

# Izris nad google maps
ggmap(zemljevid) + geom_path(data=obcine, aes(x=long, y=lat, group=group, colour=id)) +
  labs(title="Slovenija na Google Maps") +
  theme(legend.position="none")


########
## ggvis za risanje zemljevidov
# library(ggvis)
########
obcine %>% 
  ggvis(~long, ~lat) %>%
  group_by(id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  set_options(keep_aspect=TRUE)

#####
## Redčenje poligonov
## library(rgeos)
#####

obcineSP <- readOGR("OB", layer = "OB")
obcineSP@proj4string <- EPSG3912

obcineSP <- spTransform(obcineSP, WGS84)

# Redcimo s funkcijo gSimplify
obcineSP2 <- gSimplify(obcineSP, tol=0.001, topologyPreserve = TRUE)

obcine <- fortify(obcineSP2)

obcine %>% 
  ggvis(~long, ~lat) %>%
  group_by(id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  set_options(keep_aspect=TRUE)

## Izris v PDF iz ggplot

cairo_pdf("velikost_obcin_2.pdf", family="Arial", width=11.7,height=8.3)
ggplot(obcine, aes(x=long, y=lat, group=group, fill=id)) + 
  geom_polygon() + 
  labs(title="Občine") +
  theme(legend.position="none")
dev.off()

###
# Uporaba knjižnice leaflet in formata geojson
###
obcineSP <- readOGR("OB", layer = "OB")
obcineSP@proj4string <- EPSG3912


obcineSP2 <- obcineSP %>% 
    spTransform(WGS84) %>%
    mutate(Povrsinakm2=POVRSINA / 1e6) 

obcineSP2$OB_UIME <- obcineSP2 %>% .$OB_UIME %>% as.character %>% iconv("cp1250", "utf8") 

n = 7
brks <- obcineSP2 %>% 
        .$Povrsinakm2 %>% 
        quantile(seq(0,1,1/n))
pal <- brewer.pal(n,'Greens')

col <- obcineSP2 %>%
      .$Povrsinakm2 %>%
      findInterval(brks, all.inside=TRUE) %>%
      pal[.]

# qpal <- colorQuantile(pal, obcineSP3$Povrsinakm2, n=7)
legenda <- paste(  
  brks[1:n] %>% round %>% as.character(), 
  "-",
  brks[2:(n+1)] %>% round %>% as.character(), " tisoč", sep = "")

obcineSP3 <- obcineSP2 %>% 
             mutate(barva=col, 
                    napis=paste(OB_UIME,"\n\n", round(Povrsinakm2), "tisoč prebivalcev")
                    )
#%>% 
#            gSimplify(tol=0.001, topologyPreserve = TRUE)




leaflet(obcineSP3) %>%
  addTiles() %>%  
  addPolygons(stroke = TRUE, smoothFactor = 0.3, weight = 1, 
              fillOpacity = 1, fillColor = ~barva, 
              color = "black", label = ~napis) %>%
  addMarkers(lat=46.0419567, lng=14.4877623, popup="Fakulteta za matematiko in fiziko") %>%
  addLegend(values = ~barva, opacity = 1.0,
            colors=~pal, labels=~legenda,  
            labFormat = labelFormat(transform = identity))



# 46.0419567,14.4877623
# # pretvorba v format GeoJSON
# obcineJSON <- obcineSP3 %>% 
#   geojson_json() %>%
#   ms_simplify() 
# 
# leaflet() %>% addTiles() %>% 
#               addGeoJSON(obcineJSON)


######
## Uporaba geokodiranja in mesta v sloveniji
######

url <- "https://sl.wikipedia.org/wiki/Seznam_mest_v_Sloveniji"

seja <- html_session(url)
stran <- seja %>% read_html()

kraji <- stran %>% html_nodes(xpath="(//table)[1]//td[1]/a") %>% html_text()
stPreb <- stran %>% 
          html_nodes(xpath="(//table)[1]//td[2]") %>% 
          html_text() %>%
          gsub("\\[\\d+\\]", "", .) %>%   # čiščenje footnotov
          gsub("\\.", "", .) %>%  # odstranjevanje pike na tisočicah
          as.integer


# Geocodiranje naslovov preko Google Maps API
koordinate <- geocode(paste(kraji, "Slovenia", sep=","))
tab <- data.frame(kraj=kraji, stPrebivalcev=stPreb) %>%
        cbind(koordinate)


tab %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircles(lng = ~lon, lat = ~lat, weight = 1,
             radius = ~sqrt(stPrebivalcev), popup = ~paste(kraj, ", ", stPrebivalcev, sep="")
  )
      
    



# zemljevid <- get_map(location="Ljubljana, Slovenia", zoom=8, maptype="roadmap", color="bw")


# ########
# ## ggmap ne dela prav. Treba ga je inštalirati iz sourca
# install.packages("ggmap", type = "source"), mogoče skupaj z ggplot2
# ########
# ggmap(zemljevid) + geom_point(data=tab, aes(x=lon, y=lat, size=ST_PREBIVALCEV, colour=ST_PREBIVALCEV)) +
#   geom_text(data=tab, aes(label=ime), color="red", size=2)
# 
# ########
# ## Model za vizualizacijo gostote
# ########
# ggmap(zemljevid) + stat_density2d(data = tab
#                , aes(x = lon, y = lat, fill = ..level.. , alpha = ..level..)
#                , size = 1, bins = 10, geom = "polygon")
# 
# 
# stran <- read_xml("Tabela.html")
# stran %>% html_nodes(xpath="//table[3]")
# 
# tabela <- htmltab("file://Tabela.html", which = "//table[3]")
