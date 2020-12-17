require(ggplot2)
require(dplyr)

########################################################
## Hierarhično razvrščanje
########################################################

iris %>% View

# Pregled podatkov
risba <- iris %>% 
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, col=Species)) + 
  geom_point() +
  geom_text(
    data=iris %>% mutate(n = row_number()), 
    aes(label=n),
    size=2,
    color="black",
    nudge_x = 0.03,
    nudge_y = 0.03
  ) + theme_minimal()

risba + ggsave("risba.pdf", device="pdf")

# Ročna standardizacija enega vektorja
L <- iris$Sepal.Length
z <- (L-mean(L))/sd(L)
z

# Hkratna standardizacija matrike
data <- iris %>% 
  select(Sepal.Length, Sepal.Width) %>%
  as.matrix() %>%
  scale()
  
data %>% View

# Hierarhično razvrščanje
D <- dist(data) # matrika različnosti
model <- hclust(D) # model

### Rezultat razvrščanja
## Dobimo opis postopka razvrščanja
names(model)

# Algoritem za razvrščanje: ward.D, ward.D2, single, 
# complete, average, mcquitty, median, centroid
# Lahko podamo kot parameter
model$method

# Tip metrike: pridobljen iz matrike različnosti
# Npr. za D <- dist(X, method="manhattan") bi dobili "manhattan"
model$dist.method

# Opis zaporedja združevanja skupin
model$merge %>% View

# Višine, ko je prišlo do združevanja
model$height

# Preureditev elementov, da bomo lahko lepo izrisali dendrogram
model$order

# Poimenovanja podatkovnih enot. 
model$labels

# Opisni klic funkcije, ki je sprožila izračun
model$call

### Vizualizacija rezultata - dendrogram
plot(model, hang=-1,  cex=0.3, main="Iris")

# S pomočjo paketa `ggdendro`
require(ggdendro)

ggdendrogram(model, labels=TRUE) 

ggdendrogram(model, rotate=TRUE, labels=TRUE) + 
  theme(
    axis.text.y = element_text(
      size = rel(0.2), 
      margin = margin(r=-20),
      color=iris$Species[model$order]
    )
  ) +
  ggsave("dendro.pdf", device="pdf") 

# https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html


# Pridobitev podatkov za izris z ggplot
ddata <- dendro_data(model, type = "rectangle")

names(ddata)

segment(ddata) %>% View

ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) + 
  theme_dendro()

ddata$labels$label
iris$Species[model$order]

ddata$labels$label <- paste(iris$Species[model$order], ddata$labels$label %>% as.character)
ddata$labels$Species <- iris$Species[model$order]

dendrogg <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size=0.1) + 
  coord_flip()  + 
  scale_y_reverse(expand = c(0.2, 0)) + 
  geom_text(
    data=ddata$labels, 
    aes(x=x, y=y, label=label, colour=Species), hjust=0, size=1, nudge_y = 0.03) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

dendrogg + ggsave("dendrogg.pdf", device="pdf")


############################################################
## Izbira skupin (razvrstitev) glede na "rez" dendrograma
############################################################

## Primer: glej https://joyofdata.shinyapps.io/hclust-shiny/
# Dolocimo razvrstitev  p
# p[i]=k  - enota i pripada skupini k
p <- cutree(model, k=2)
p

# Alternativno(cela metoda)

cut2 <- hcut(data, k=2, hc_method = "complete")$cluster
# Pozor: pri hclust je privzeta metoda 
model$method
# Pri hcut pa "ward.D2"
p - cut2

plot(model)
rect.hclust(model, k=2, border=1:2)

require(factoextra)

# Iščemo očitno koleno - ga ni
fviz_nbclust(data, FUN = hcut, method = "wss")

# Iščemo maksimum
fviz_nbclust(data, FUN = hcut, method = "silhouette")

# Na grafu dobimo za točko `k`, vrednost `gap(k)`, ter interval `[gap(k) - s_k, gap(k) + s_k]`
# Iščemo prvi tak `k`, da velja gap(k) >= gap(k + 1) - s_{k+1}
fviz_nbclust(data, FUN = hcut, method = "gap_stat", nstart=25, nboot=300)

# 2 skupini
iris %>% 
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, col=p)) + 
  geom_point() + ggsave("risba2.pdf", device="pdf")

# skupine 
p3 <- cutree(model, k=3)
iris %>% 
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, col=p3)) + 
  geom_point() + ggsave("risba3.pdf", device="pdf")

###############################################################
## Metoda voditeljev
########################################################

# Izbrati moramo število voditeljev
# k = 3

data <- iris %>% 
  select(Sepal.Length, Sepal.Width) %>%
  as.matrix() %>%
  scale()

modelK <- kmeans(data, 3)

names(modelK)

# Razporeditev v skupine
modelK$cluster

# Končni centri 
modelK$centers

# Vsota kvadratov vseh razdalj
modelK$totss

# Vsote kvadratov razdaj do centrov za posamezne skupine
modelK$withinss

# sum(modelK$withinss)
modelK$tot.withinss

# totss - tot.withinss
modelK$betweenss

# Število točk v vsaki skupini
modelK$size

# Število iteracij algoritma
modelK$iter

## Kako dobro smo zadeli Species za k = 3
iris %>% 
  mutate(skupina=modelK$cluster) %>%
  ggplot(aes(x=Sepal.Length, y=Sepal.Width, col=Species, shape=factor(skupina))) + 
  geom_point()

## Kaj pa če želimo najti "pravi" k?
fviz_nbclust(data, FUN = kmeans, method = "wss")
fviz_nbclust(data, FUN = kmeans, method = "silhouette")
fviz_nbclust(data, FUN = kmeans, method = "gap_stat", nstart=25, nboot=300)

###########################################################
## Razvrščanje v večih dimenzijah
###########################################################

# Problem - ne moremo si grafično narisati
# Prednost - mogoče dodatne dimenzije bolje ločijo podatke

# Standardizacija
data <- iris %>% 
  select(1:4) %>% 
  as.matrix() %>%
  scale()

data %>% View

# Hierarhično razvrščanje
D <- dist(data) # matrika različnosti
model <- hclust(D) # model

# Izris
ggdendrogram(model, rotate=TRUE, labels=TRUE) + 
  theme(
    axis.text.y = element_text(
      size = rel(0.2), 
      margin = margin(r=-20),
      color=iris$Species[model$order]
    )
  ) + ggsave("dendro4D.pdf", device="pdf")
  
# V 4D je malo bolje, a po barvah vidimo, da drugih dveh vrst ne bomo uspeli zelo dobro ločiti.

# Koliko skupin?
fviz_nbclust(data, FUN = hcut, method = "wss")
fviz_nbclust(data, FUN = hcut, method = "silhouette")
fviz_nbclust(data, FUN = hcut, method = "gap_stat", nstart=25, nboot=300)

# Lahko bi v obeh algoritmih uporabili drugačne parametre
# - druge algoritme (metode)
# - druge razdalje

########################################################
## Dodatni viri
## http://onlinelibrary.wiley.com/doi/10.1002/0470854774.app1/pdf
## http://en.wikipedia.org/wiki/Hierarchical_clustering
## http://xray.bmc.uu.se/kurs/BioinfX3/Clustering.pdf
## http://www.jstatsoft.org/v53/i09/paper
## http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/vegdist.html
## https://atrebas.github.io/post/2019-06-08-lightweight-dendrograms/
## https://uc-r.github.io/kmeans_clustering#elbow
## https://uc-r.github.io/hc_clustering
## http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning

