
####################################
## Zemljevidi s pomočjo ggplot2
####################################
require(dplyr)
require(ggplot2)

# primer v knjižnici ggplot
svet <- map_data("world")

# Zemljevid je predstavljen kot data.frame v posebni obliki
svet %>% View

# vsaka vrstica pomeni točko v regiji. 
# poligone določa zaporedje order. 
# posamezen poligon določa skupin group

svet$region %>% 
  unique() %>%
  length()

# Katere regije vsebuje?
"Slovenia" %in% map2$region
"Italy" %in% map2$region

# Izris z ggplot: zapolnjeni poligoni s pobarvanim robom
svet %>%
  ggplot(aes(x=long, y=lat, group=group, colour=region)) + 
  geom_polygon() + 
  labs(title="Svet - osnovna slika") +
  theme(legend.position="none")   # ker je preveč držav je privzeta legenda nesmiselna

# Zapolnjeni poligoni s pobarvano notranjostjo
svet %>%
  ggplot(aes(x=long, y=lat, group=group, fill=region)) + 
  geom_polygon() + 
  labs(title="Svet - osnovna slika") +
  theme(legend.position="none")

## Alternativni zemljevidi
library(rworldmap)
svet2 <- getMap(resolution="low")

# Drugačen format, SpatialPolygonsDataFrame
svet2 %>% class

# Kako izgleda?
svet2 %>% View
svet2 %>% 
  data.frame %>%
  View

# Pretvorba SpatialPolygonsDataFrame v data.frame. Regija gre v spremenljivko "id"
svet3 <- fortify(svet2)

svet3 %>% 
  View()

# Izrez Slovenije
svet3 %>% 
  ggplot(aes(x=long, y=lat, group=group, colour=id)) +
  geom_path() + 
  coord_cartesian(xlim=c(13.2,16.7), ylim=c(45.2,47)) + # izrez
  labs(title="Svet - Rworldmap") +
  theme(legend.position="none")

############################################################
## Datoteke SHP
## Uradni slovenski poligoni. Včasih se jih je dalo dobiti tukaj:
## http://www.e-prostor.gov.si/si/dostop_do_podatkov/dostop_do_podatkov/
# library(maptools)
############################################################

# "OB" je pot do mape, kjer je .shp datoteka
library(rgdal)
SIob <- readOGR("OB", layer = "OB", encoding="cp1250")

# Tip objekta `SpatialPolygonsDataFrame`
SIob %>% class()

# data.frame/tibble brez geometrije 

SIob %>% as_tibble() %>% View

# Priprava za ggplot(fortify)
SIob_fort <- SIob %>% 
  fortify(region="OB_ID") 

SIob_fort %>% View 

# osnovna slika
SIob_fort %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE) +
  theme_bw()
  
# zelo tenke črte, v PDF
SIob_fort %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.01) +
  theme_bw() +
  ggsave("zemljevid.pdf", device = "pdf")

# Poenostavljanje poligonov za izris
library(rmapshaper)

# Poenostavitev traja nekaj časa, zato jo naredimo ponavadi le enkrat
SIobSim <- SIob %>%
  ms_simplify()

# Potem pa izvajamo različne izrise
SIobSim %>%
  fortify() %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.1) +
  theme_bw() +
  ggsave("zemljevid.pdf", device = "pdf")
  
# S parametrom `keep` nastavimo delež točk, ki ga obdržimo

SIob %>%
  ms_simplify(keep=0.001) %>%
  fortify() %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.1) +
  theme_bw() 

SIob %>%
  ms_simplify(keep=0.01) %>%
  fortify() %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.1) +
  theme_bw()
  
# Izris poenostavljene variante brez ozadja
SIobSim %>%
  fortify() %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.1) +
  theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  ) 


brezOzadja <- theme_bw() +
  theme(
    axis.line=element_blank(),
    axis.text.x=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    panel.background=element_blank(),
    panel.border=element_blank(),
    panel.grid.major=element_blank(),
    panel.grid.minor=element_blank(),
    plot.background=element_blank()
  ) 

SIobSim %>%
  fortify() %>%
  ggplot(aes(x=long, y=lat, group=group)) + 
  geom_path(show.legend = FALSE, size=0.1) + 
  brezOzadja

#######################################
## Barvanje občin po površini
#######################################

SIobSim_df <- SIobSim %>% 
  as_tibble()

SIobSim_df %>% View

# "OB_ID" -> id
SIobSim_fort <- SIobSim %>%
  fortify(region = "OB_ID")

SIobSim_fort %>% View

SIobSim_fort %>% sapply(class)

SIobSim_df %>% sapply(class)

podatki <- SIobSim_df %>% 
  mutate(
    id = as.character(OB_ID),
    povrsina = OB_POV/1e6,
  ) %>% # id občine, ime, površina ter koordinati centroida  
  select(id, povrsina)    

podatki %>% View  

# "fortify" tabeli pridružimo še originalno tabelo iz SHP
SIobSimGG <- SIobSim_fort %>% 
  left_join(podatki, by="id")

SIobSimGG %>% View

brks <- quantile(SIobSimGG$povrsina, seq(0,1,1/7))

brks

require(RColorBrewer)
paleta <- brewer.pal(8,'Greens')  

paleta

# Barvanje s pomočjo palete iz knjižnice Brewer
SIobSimGG %>% 
  mutate(
    kvantil=factor(findInterval(SIobSimGG$povrsina, brks, all.inside=TRUE))
  ) %>%
  ggplot(aes(x=long, y=lat, group=group, fill=kvantil)) + 
  geom_polygon() + 
  scale_fill_brewer(type=8, palette="Greens") + brezOzadja

# Dodajanje imen

imena <- coordinates(SIob) %>% 
  as_tibble() %>% 
  rename(
    x=1,
    y=2
  ) %>% 
  mutate(
    ime=SIob$OB_UIME
  )

imena %>% View()
legendaLabele <- paste(as.integer(brks[1:7]),as.integer(brks[2:8]),sep="-")

legendaLabele

require(latex2exp)
require(emojifont)

SIobSimGG %>% 
  mutate(
    kvantil=factor(findInterval(SIobSimGG$povrsina, brks, all.inside=TRUE))
  ) %>%
  ggplot() + 
  geom_polygon(aes(x=long, y=lat, group=group, fill=kvantil), color="black", size=0.001) + 
  scale_fill_brewer(
    type=8, palette="Greens", 
    labels=legendaLabele, 
    name=TeX("Površina v $km^2$")
  ) +
  geom_text(data=imena, aes(x, y, label=ime), size=1) +
  labs(title="Velikost slovenskih občin") +
  brezOzadja +
  ggsave("obcine.pdf", device = "pdf")


#############################################
## Pretvorbe med koordinatnimi sistemi
# library(rgdal)
#############################################

library(rgdal)
## Koordinatni sistem WGS84 = (longitude, latitude)
WGS84 <- CRS("+proj=longlat +ellps=WGS84")

## Koordinatni sistem, ki ga uporablja GURS
EPSG3912 <- CRS("+init=epsg:3912")

map4 <- readOGR("OB", layer = "OB", encoding="cp1250")
# Nastavitev koordinatnega sistema (če ga že ne uspemo prebrati is .shp datotek)
map4@proj4string <- EPSG3912

# Transformacija v drugi koordinatni sistem

map5 <- spTransform(map4, WGS84)
obcine <- map5 %>%
  ms_simplify(keep = 0.01) %>%
  fortify()

## Alternativna knjižnica ggplot2-ju, 
require(ggvis)

obcine %>%
  ggvis(~long, ~lat) %>%
  group_by(id) %>%
  layer_paths(strokeOpacity:=0.5, stroke:="#7f7f7f") %>%
  set_options(keep_aspect=TRUE)


################################################
# Uporaba knjižnice leaflet
################################################
require(leaflet)
require(spdplyr)
obcineSP <- readOGR("OB", layer = "OB", encoding="cp1250")
obcineSP@proj4string <- EPSG3912


obcineSP2 <- obcineSP %>% 
    spTransform(WGS84) %>%
    ms_simplify() %>%
    mutate(povrsina=OB_POV / 1e6) 

brks <- quantile(obcineSP2$povrsina, seq(0,1,1/7))
paleta <- brewer.pal(n,'Greens')

barva <- paleta[findInterval(obcineSP2$povrsina, brks, all.inside=TRUE)]

barva

obcineSP2 %>% 
  mutate(
    barva=barva
  ) %>%
  leaflet() %>%
  addTiles() %>%
  addPolygons(stroke = TRUE, smoothFactor = 0.3, weight = 1, 
              fillOpacity = 0.5, fillColor = ~barva, 
              color = "black", label = ~OB_UIME) %>%
  addMarkers(lat=46.0419567, lng=14.4877623, popup="Fakulteta za matematiko in fiziko") %>%
  addLegend(opacity = 1.0,
            values=~povrsina,
            colors=paleta, labels=legendaLabele,
            title="Površina slovenskih <br> občin v km<sup>2</sup>",
            labFormat = labelFormat(transform = identity))
# Format geoJSON
library(geojsonio)
# pretvorba v format GeoJSON
obcineJSON <- obcineSP2 %>%
  geojson_json()

leaflet() %>% addTiles() %>%
  addGeoJSON(obcineJSON)

##################################################
## Knjižnica tmap
##################################################

require(tmap)
require(spdplyr)

map4 <- readOGR("OB", layer = "OB", encoding="cp1250")
map4@proj4string <- EPSG3912
map5 <- spTransform(map4, WGS84)

obcine <- map5 %>%
  ms_simplify(keep = 0.01) 

tmap_mode("plot")
obcine %>% 
  mutate(
    povrsina=OB_POV/1e6
  ) %>%
  tm_shape() +
  tm_polygons(
    col="povrsina", palette=paleta, 
    title=TeX("Površina v $km^2$"),
    labels=legendaLabele,
    id="OB_UIME"
  ) +
  tm_layout(title="Velikost slovenskih občin")


tmap_mode("view")
obcine %>% 
  mutate(
    povrsina=OB_POV/1e6
  ) %>%
  tm_shape() +
  tm_polygons(
    col="povrsina", palette=paleta, 
    title="Površina v km<sup>2</sup>",
    labels=legendaLabele,
    id="OB_UIME"
  ) +
  tm_layout(title="Velikost slovenskih občin")



################################################
## Uporaba geokodiranja in mesta v Sloveniji
################################################

require(rvest)
require(tmaptools)  # geokodiranje

url <- "https://sl.wikipedia.org/wiki/Seznam_mest_v_Sloveniji"

stran <- read_html(url)

kraji <- stran %>% html_nodes(xpath="(//table)[1]//td[1]/a") %>% html_text()
stPreb <- stran %>% 
          html_nodes(xpath="(//table)[1]//td[2]") %>% 
          html_text() %>% 
          gsub("\\[\\d+\\]", "", .) %>%   # čiščenje footnotov
          gsub("\\.", "", .) %>%  # odstranjevanje pike na tisočicah
          as.integer

# Geokodiranje (pridobivanje GPS koordinat preko OpenStreetMap)
# To zna trajati ene 2 minuti
koordinate <- geocode_OSM(kraji)

koordinate %>% View 

prebivalci <- data.frame(
  kraj=kraji, 
  prebivalci=stPreb, 
  stringsAsFactors = FALSE
)

prebivalci %>% View

obcinePrebivalci <- prebivalci %>% 
  left_join(koordinate, by=c("kraj"="query")) %>% 
  select(kraj, prebivalci, lat, lon) 

obcinePrebivalci %>% View

obcinePrebivalciSP <- st_as_sf(obcinePrebivalci, 
    coords = c("lon", "lat"),
    crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
)

obcine %>% 
  mutate(
    povrsina=OB_POV/1e6
  ) %>%
  tm_shape() +
  tm_polygons(
    col="povrsina", palette=paleta, 
    title="Površina v km<sup>2</sup>",
    labels=legendaLabele,
    id="OB_UIME"
  ) +
  tm_layout(title="Velikost slovenskih občin") +
  tm_shape(obcinePrebivalciSP) +
    tm_bubbles(size="prebivalci")

