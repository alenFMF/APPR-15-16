require(ggplot2)
require(dplyr)
require(rworldmap)

head(Orange)

ggplot(data=Orange, aes(x=Orange$age, y=Orange$circumference)) + geom_point()
ggplot(data=Orange, aes(x=age, y=circumference)) + geom_point()

ggplot(data=Orange %>% filter(Tree==1), aes(x=age, y=circumference)) + geom_line()
ggplot(data=Orange %>% filter(Tree==1), aes(x=age, y=circumference)) + geom_line() + geom_point(col="red")

ggplot(data=Orange, aes(x=Tree, y=circumference)) + geom_boxplot() 
ggplot(data=Orange, aes(x=Tree, y=circumference)) + geom_boxplot() + geom_points()

ggplot(data=Orange, aes(x=circumference)) + geom_histogram() 
ggplot(data=Orange, aes(x=circumference)) + geom_histogram(binwidth=50)

ggplot(data=Orange, aes(x=circumference)) + geom_histogram(binwidth=50) + geom_vline(xintercept=median(Orange$circumference), col="red")

ggplot(data=Orange, aes(x=circumference)) + geom_density() 

ggplot(data=Orange, aes(x=circumference, y=age)) + geom_point() + facet_grid(~Tree) 

levels(Orange$Tree) <- sort(levels(Orange$Tree))

ggplot(data=Orange, aes(x=circumference, y=age)) + geom_point() + geom_line(col="red") + facet_wrap(~Tree, ncol=2) 

ggplot(data=Orange, aes(x=circumference, y=age, col=Tree))  + geom_line()

head(iris)

ggplot(data=iris, aes(x=Petal.Length, fill=Species))  + geom_histogram()
ggplot(data=iris, aes(x=Petal.Length, color=Species))  + geom_histogram()
ggplot(data=iris, aes(x=Petal.Length, fill=Species))  + geom_histogram(color="black") 

head(movies)
tipi = names(movies)[18:23]
seznam = list()
for (i in 1:length(tipi)) {
    tip = tipi[[i]]
    seznam[[i]] <- movies %>% 
        filter_(paste(tip, "==", 1)) %>% 
        select(Budget=budget, Short, Year=year) %>%
        mutate(Type=tip)
}
myMovies <- do.call(rbind, seznam)

ggplot(data=myMovies, aes(x=Type, fill=Type))  + geom_bar() 

ggplot(data=myMovies, aes(x=Type, fill=factor(Short)))  + geom_bar() 

ggplot(data=myMovies, aes(x=Type, fill=factor(Short)))  + geom_bar(position="stack") 
ggplot(data=myMovies, aes(x=Type, fill=factor(Short)))  + geom_bar(position="dodge") 
ggplot(data=myMovies, aes(x=Type, fill=factor(Short)))  + geom_bar(position="fill") 


ggplot(data=myMovies, aes(x=Type, y=Budget))  + geom_boxplot() 
ggplot(data=myMovies, aes(x=Type, y=Budget))  + geom_boxplot() + scale_y_log10()

ggplot(data=myMovies, aes(x=Type, y=Budget))  + geom_boxplot() + scale_y_log10() + geom_point()
ggplot(data=myMovies, aes(x=Type, y=Budget))  + geom_jitter() + geom_boxplot(alpha=I(0.6)) + scale_y_log10() 


ggplot(data=ToothGrowth, aes(x=dose, y=len))  + geom_point() 
ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))  + geom_point() 
ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))  + geom_point() + facet_grid(.~supp) 
ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))  + geom_point() + facet_grid(.~supp) + stat_smooth(method = "lm")


p <- ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))
summary(myPlot)
myNewPlot <- myPlot +geom_point()

myNewPlot + geom_point(aes(col=NULL)) +
  theme(legend.position="none")

ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp)) +
  geom_point() + coord_flip()


ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp)) +
  geom_point() + coord_fixed(ratio=0.1)

myMovies$RoundYear <- signif(myMovies$Year, digits = 3)

### Faceting wxith orientation by columns
ggplot(data=myMovies,aes(Budget)) + geom_histogram(binwith=1) + facet_grid(.~Type)+ scale_x_log10()

### Faceting with orientation by rows
ggplot(data=myMovies,aes(Budget)) + geom_histogram(binwith=1) + facet_grid(Type~.) + scale_x_log10()

ggplot(data=myMovies,aes(Budget)) + geom_histogram(binwith=1) + facet_grid(RoundYear~Type) + scale_x_log10()

ggplot(data=subset(myMovies, RoundYear>1980), aes(Budget)) + geom_histogram(binwith=1) + facet_grid(.~Type+RoundYear) + scale_x_log10()


ggplot(data=myMovies %>% filter(Year>1999), aes(Budget)) + geom_histogram() + facet_wrap(~Year, nrow=2) + scale_x_log10()

----

ggplot(data=iris, aes(x=Petal.Length,color=Species,fill=Species)) + geom_histogram(alpha=I(0.5))


ggplot(data=iris, aes(x=Petal.Length,color=Species,fill=Species)) + geom_density(alpha=I(0.5))


-
ggplot(data=myMovieData, aes(x=Type,fill=factor(Short))) + geom_bar()


ggplot(data=myMovies, aes(Type,Budget)) + geom_jitter() + geom_boxplot(alpha=I(0.6)) + scale_y_log10()

ggplot(data=ToothGrowth, aes(x=dose,
                             y=len)) + geom_point() +
  stat_smooth() + facet_grid(.~supp)

ggplot(data=Orange, aes)

require(rworldmap)
require(ggplot2)
zemljevid <- getMap(resolution="coarse")
plot(zemljevid)
z2 <- fortify(zemljevid)
ggplot() + geom_path(data = z2, aes(x = long, y = lat, map_id = id, group = group))




plot(zemljevid)
plot(zemljevid[zemljevid$SOVEREIGNT == "Slovenia",])

ggplot() + geom_polygon(data=zemljevid, aes(x=long, y=lat))

ggplot(zemljevid, aes(x=long, y=lat, group=group)) + geom_polygon()

ggplot() + geom_map(data=zemljevid, map=zemljevid, aes(map_id = region, x= long, y=lat)) + geom_map(fill="white", col="black")

map.world <- map_data(map="world")
map.world$name_len <- nchar(map.world$region) + sample(nrow(map.world))

ggplot() + geom_map(data=zemljevid, map=zemljevid, aes(map_id=region, x=long, y=lat, fill=name_len))

zemljevid <- as.data.frame(zemljevid)
ggplot() + geom_map(data=zemljevid, map=zemljevid, aes(map_id=region, x=long, y=lat, fill=name_len))


ggplot(z2, aes(x = long, y = lat)) + geom_path(aes(group = group)) + geom_polygon(aes(fill=z2$MAP_COLOR))


library(raster)
library(rworldmap)
library(ggplot2)

iran<- raster::getData("GADM", country = "Iran", level = 1)
map <- fortify(iran)
ggplot() + geom_path(data = z2, aes(x = long, y = lat, map_id = id, group = group))
