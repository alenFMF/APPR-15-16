require(ggplot2)
require(dplyr)

# pregled podatkov - razdalje zaustavljanja pri danih hitrostih
cars %>% View
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point()

# izračun modela
model <- lm(dist ~ speed, data=cars)

# Koeficienti premice
model$coefficients

# Residuali (razlike do premice)
model$residuals

# Dodatne statistike modela
s <- summary(model)

# Npr. R^2 https://en.wikipedia.org/wiki/Coefficient_of_determination
s$r.squared

# Pregled nekaterih statistik modela
require(ggfortify)
autoplot(model)

# izris linearnega modela (ne istega!)
ggplot(cars, aes(x = speed, y = dist)) + 
    geom_point() + 
    geom_smooth(method=lm, formula=y~x) 

# brez intervalov zaupanja
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point() + 
  geom_smooth(method=lm, formula=y~x, se=FALSE) 

# primer predikcije. Kakšna je hitrost na razdaljah 15, 20
newSpeed <- data.frame(speed=c(15, 20))
predict(fit, newSpeed)
napoved <- newSpeed %>% mutate(dist=predict(fit, .))

napoved %>% View

# izris napovedi
ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, formula=y~x) +
  geom_point(data=napoved, aes(x=speed, y=dist), color='red', size=3)
  
# dodajanje enačbe kot anotacije  
enacba <- function(x) {
  lm_coef <- list(a = coef(x)[1] %>% round(digits = 2) %>% as.numeric,
                  b = coef(x)[2] %>% round(digits = 2) %>% as.numeric(),
                  r2 = round(summary(x)$r.squared, digits = 2));
  lm_eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(R)^2~"="~r2,lm_coef)
  as.character(as.expression(lm_eq));                 
}

enacba(fit)

ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(shape=1) + 
  geom_smooth(method=lm, formula = y~x) +
  geom_point(data=napoved, aes(x=speed, y=dist), color='red', size=3) +
  annotate("text", x = 20, y = 10, label = enacba(fit), parse = TRUE) +
  ggtitle('Linearna regresija') + 
  theme_bw()

# Intervali zaupanja, predikcijski intervali
# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
