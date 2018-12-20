require(ggplot2)
require(dplyr)
require(ggdendro)
## http://onlinelibrary.wiley.com/doi/10.1002/0470854774.app1/pdf
## http://en.wikipedia.org/wiki/Hierarchical_clustering
## http://xray.bmc.uu.se/kurs/BioinfX3/Clustering.pdf
## http://www.jstatsoft.org/v53/i09/paper
## http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/vegdist.html

#######
## Hierarhično razvrščanje
######

head(iris)
iris %>% ggplot(aes(Sepal.Length, Sepal.Width, col=Species)) + geom_point()

L <- iris$Sepal.Length

#Standardizacija
s <- (L-mean(L))/sd(L)
head(s)
# Standardizacija večih stolpcev
X <- scale(as.matrix(iris[,1:4]))
head(X)

# Matrika različnosti
D <- dist(X)
head(D)
# Hierarhično razvrščanje
model <- hclust(D)
names(model)

# Izris
ggdendrogram(model, rotate = FALSE, size = 2)

# https://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html

# uporabimo Wardovo metodo združevanja
model <- hclust(D,method="ward.D")
ggdendrogram(model, rotate = TRUE, size = 2)


# plot(t,hang=-1,cex=0.1,main="Iris")

# Dolocimo razvrstitev  p
# p[i]=k  - enota i pripada skupini k
p <- cutree(model, k=3)

df <- iris %>% mutate(p = p)

df %>% filter(p==1 & Species != "setosa") %>% select(Species)
df %>% filter(p==2) %>% select(Species)
df %>% filter(p==3) %>% select(Species)


ddata <- dendro_data(model, type = "rectangle")

ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + scale_y_reverse(expand = c(0.2, 0)) + 
  theme_dendro()

ddata$labels$label <- paste(iris$Species, ddata$labels$label %>% as.character)
ddata$labels$Species <- iris$Species

ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip()  + 
  scale_y_reverse(expand = c(0.2, 0)) + 
  geom_text(data=ddata$labels, aes(x=x, y=y, label=label, colour=Species), hjust=0, size=1) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())





