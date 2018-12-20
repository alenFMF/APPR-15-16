require(ggplot2)
require(dplyr)

# pregled podatkov
cars %>% View
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point()

# izračun modela
fit <- lm(dist ~ speed, data=cars)
summary(fit)

# izris modela
ggplot(cars, aes(x = speed, y = cars$dist)) + 
    geom_point() + 
    geom_smooth(method=lm) 

# brez intervalov zaupanja
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method=lm, se=FALSE) 

# primer predikcije
newSpeed <- data.frame(speed=c(15, 20))
predict(fit, newSpeed)
napoved <- newSpeed %>% mutate(dist=predict(fit, .))
View(napoved)

# izris napovedi
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_point(data=napoved, aes(x=speed, y=dist), color='red', size=3)
  
# dodajanje enačbe kot anotacije  
enacba <- function(x) {
  lm_coef <- list(a = round(coef(x)[1], digits = 2),
                  b = round(coef(x)[2], digits = 2),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

enacba(fit)

ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm) +
  geom_point(data=napoved, aes(x=speed, y=dist), color='red', size=3) +
  annotate("text", x = 20, y = 10, label = enacba(fit), parse = TRUE) +
  ggtitle('Linearna regresija') + 
  theme_bw()


