require(ggplot2)
require(dplyr)

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


p <- ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))
summary(myPlot)
myNewPlot <- myPlot +geom_point()

myNewPlot + geom_point(aes(col=NULL)) +
  theme(legend.position="none")

ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp)) +
  geom_point() + coord_flip()


ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp)) +
  geom_point() + coord_fixed(ratio=0.1)

### Faceting with orientation by columns
ggplot(data=myMovieData,aes(Budget)) + geom_histogram(binwith=1) +
  facet_grid(.~Type)+ scale_x_log10()

### Faceting with orientation by rows
ggplot(data=myMovieData,aes(Budget)) + geom_histogram(binwith=1) +
  facet_grid(Type~.) + scale_x_log10()

ggplot(data=myMovieData,aes(Budget)) + geom_histogram(binwith=1) +
  facet_grid(roundYear~Type) + scale_x_log10()


ggplot(data=subset(myMovieData, roundYear>1980),
       aes(Budget)) +
  geom_histogram(binwith=1) +
  facet_grid(.~Type+roundYear) + s
cale_x_log10()


ggplot(data=subset(myMovieData,Year>1999),
       aes(Budget)) +
  geom_histogram() + facet_wrap(~Year, nrow=2) +
  scale_x_log10()

ggplot(data=iris, aes(x=Petal.Length,color=Species,fill=Species)) + geom_histogram(alpha=I(0.5))


ggplot(data=iris, aes(x=Petal.Length,color=Species,fill=Species)) +
  geom_density(alpha=I(0.5))

ggplot(data=myMovieData, aes(x=Type,fill=factor(Short))) + geom_bar()


ggplot(data=myMovieData,
       aes(Type,Budget)) +
  geom_jitter() + geom_boxplot(alpha=I(0.6)) +
  scale_y_log10()

ggplot(data=ToothGrowth, aes(x=dose,
                             y=len)) + geom_point() +
  stat_smooth() + facet_grid(.~supp)

ggplot(data=Orange, aes)