require(ggplot2)
require(dplyr)

#################################
# Osnovno risanje z ggplot
#################################

# Testni podadtki
head(Orange)
Orange %>% View
# Risanje točk
ggplot(data=Orange, aes(x=Orange$age, y=Orange$circumference)) + geom_point()

# Skrajšano naslavljanje stolpcev
ggplot(Orange, aes(x=age, y=circumference)) + geom_point()

# Obdelava podatkov pred risanjem. Risanje črt
Orange %>% 
  filter(Tree==1) %>% 
  ggplot(aes(x=age, y=circumference)) + 
  geom_line(size=10, colour="red")

# Risanje črt in točke
Orange %>% 
  filter(Tree==1) %>% 
  ggplot(aes(x=age, y=circumference)) + 
  geom_line() + 
  geom_point(col="red")

# Še malo časovnih vrst
head(economics)

# Izpisi na oseh
economics %>%
  ggplot(aes(x=date, y=unemploy)) + 
  geom_line() + 
  labs(title="Nezaposlenost po letih") +
  ylab("Število nezaposlenih") +
  xlab("Leto")

#################################
# Prikaz porazdelitev
#################################

# Prikaz približnih porazdelitev podatkov
Orange %>% 
  ggplot(aes(x=Tree, y=circumference)) + 
  geom_boxplot() 

# Približne porazdelitve in vrednosti podatkov.
Orange %>% 
  ggplot(aes(x=Tree, y=circumference)) + 
  geom_boxplot() + 
  geom_point()

# Histogrami (frekvenčne porazdelitve), avtomatični koši
Orange %>% 
  ggplot(aes(x=circumference)) + 
  geom_histogram() 

# Histogram, določena velikost koša
Orange %>% 
  ggplot(aes(x=circumference)) + 
  geom_histogram(binwidth=50)

# Dodajanje vertikalne daljice na graf
Orange %>%
  ggplot(aes(x=circumference)) + 
  geom_histogram(binwidth=50) + 
  geom_vline(xintercept=median(Orange$circumference), col="red")

# Prikaz približne (zvezne) gostote porazdelitve
Orange %>%
  ggplot(aes(x=circumference)) + 
  geom_density() 

#################################
# Facet-i
#################################

# Faceti (ekvivalent "group by" na grafih)
Orange %>% 
  ggplot(aes(x=circumference, y=age)) + 
  geom_point() + 
  facet_grid(.~Tree) # ena vrstica, posamezno drevo v stolpcih

# Popravljanje vrstnega reda za izris pri faktorjih
levels(Orange$Tree) <- sort(levels(Orange$Tree))
# Izris v dolgi lomljeni vrstici, fiksno število stolpcev (2)
Orange %>% 
  ggplot(aes(x=circumference, y=age)) + 
  geom_point() + 
  geom_line(col="red") + 
  facet_wrap(~Tree, ncol=2)  # v eni dolgi vrstici, 

# Uporaba drugih estetskih parametrov (barva)
Orange %>% 
  ggplot(aes(x=circumference, y=age, col=Tree))  + 
  geom_line()

# Drugačna testna množica
head(iris)

# Pregled porazdelitev podatkov (parameter `fill` - barva notranjosti stolpcev)
iris %>% 
  ggplot(aes(x=Petal.Length, fill=Species)) + 
  geom_histogram()

# Pregled porazdelitev podatkov (parameter `color` - obroba)
iris %>% 
  ggplot(aes(x=Petal.Length, color=Species)) + 
  geom_histogram()

# Fiksna barva obrobe
iris %>% 
  ggplot(aes(x=Petal.Length, fill=Species)) + 
  geom_histogram(color="black") 

# Malo bolj `fancy` (polprosjno)
iris %>%
  ggplot(aes(x=Petal.Length,color=Species,fill=Species)) + 
  geom_histogram(alpha=I(0.5))

# Zvezen približek porazdelitve (ob upoštevanju prosojnosti)
iris %>%
  ggplot(aes(x=Petal.Length,color=Species,fill=Species)) + 
  geom_density(alpha=I(0.5))

# Primer z drugo podatkovno zbirko - movies

#################################
# Analiza po filmskih žanrih
#################################

require(ggplot2movies)
require(tidyr)
movies %>% View

# Tabela filmov po žanrih (z Budgetom in indikacijo, ali gre za kratek film)
myMovies <- movies %>%
  pivot_longer(18:23, names_to="Type", values_to="JeTipa") %>% 
  filter(JeTipa == 1) %>% 
  select(Budget=budget, Short, Year=year, Type) 

myMovies %>% View

# Porazdelitev po žanrih
myMovies %>% 
  ggplot(aes(x=Type, fill=Type)) + 
  geom_bar() 

# Porazdelitev po žanrih z deleži kratkih filmov
myMovies %>%
  ggplot(aes(x=Type, fill=factor(Short))) + 
  geom_bar() 

# Ekvivalentno (privzeta vrednost parametra `position` je "stack")
myMovies %>%
  ggplot(aes(x=Type, fill=factor(Short))) + 
  geom_bar(position="stack") 

# Alternativni prikaz deležev en ob drugem
myMovies %>%
  ggplot(aes(x=Type, fill=factor(Short))) + 
  geom_bar(position="dodge") 

# Alternativni prikaz deležev od celote
myMovies %>%
  ggplot(aes(x=Type, fill=factor(Short))) + 
  geom_bar(position="fill") 

# Porazdelitev podatkov po žanrih s pomočjo box_plot
myMovies %>%
  ggplot(aes(x=Type, y=Budget)) + 
  geom_boxplot() 

# Logaritemska skala
myMovies %>%
  ggplot(aes(x=Type, y=Budget)) + 
  geom_boxplot() + 
  scale_y_log10()

# Dodamo še točke
myMovies %>%
  ggplot(aes(x=Type, y=Budget)) + 
  geom_boxplot() + 
  scale_y_log10() + 
  geom_point()

# Točke še malce raztresemo in naredimo delno prosojne
myMovies %>%
  ggplot(aes(x=Type, y=Budget)) + 
  geom_jitter() + 
  geom_boxplot(alpha=I(0.6)) + 
  scale_y_log10() 

# Zaokrožimo leto na desetletje
myMovies$RoundYear <- signif(myMovies$Year, digits = 3)

myMovies %>% View

# Porazdelitev budgetov po žanrih (histogrami)
myMovies %>%
  ggplot(aes(x=Budget)) + 
  geom_histogram(binwith=1) + 
  facet_grid(.~Type) + 
  scale_x_log10()

# Faceting v drugi smeri
myMovies %>%
  ggplot(aes(x=Budget)) + 
  geom_histogram(binwith=1) + 
  facet_grid(Type~.) + 
  scale_x_log10()

# Primerjava budgetov po žanrih in desetletjih in shranjevanje kompliciranega grafa v PDF

library(scales)  # scale_x_continuous za formatiranje števil na osi x
library(emojifont)  # Šumniki v PDF
# Komplicirana vizualizacija, ki jo shranimo v PDF
myMovies %>%
  ggplot(aes(x=Budget)) + 
  geom_histogram(bins=50) + 
  facet_grid(RoundYear~Type) + 
  scale_x_log10() +
  theme(axis.text.x = element_text(
    face="bold", color="#000000", size=6, angle=90)) +
  scale_x_continuous(labels = comma) +
  labs(title="Porazdelitev filmskih proračunov skozi desetletjih") +
  ylab("Število filmov") +
  xlab("Velikost proračuna v USD") +
  ggsave("slika.pdf", device = "pdf")

# Porazdelitev po žanrih in desetletjih, po letu 1980, malo drugače
myMovies %>% 
  filter(RoundYear > 1980) %>%
  ggplot(aes(x=Budget)) + 
  geom_histogram(binwith=1) + 
  facet_grid(.~Type+RoundYear) + 
  scale_x_log10()

# Budgeti po letih od 2000 dalje
myMovies %>%
  filter(Year > 1999) %>%
  ggplot(aes(x=Budget)) + 
  geom_histogram() + 
  facet_wrap(~Year, nrow=2) + 
  scale_x_log10()

# Še ena zanimiva vizualizacija (balončki)
myMovies %>%
  ggplot(aes(x=Year, y=Type, size=Budget)) +
  geom_point(alpha=I(0.5))

#################################
# Pregled podatkov ToothGrowth
#################################

help(ToothGrowth)

# Vpliv doz na dolžino
ToothGrowth %>%
  ggplot(aes(x=dose, y=len)) + 
  geom_point() 

# Vpliv doz na dolžino glede na tip dodatka
ToothGrowth %>%
  ggplot(aes(x=dose, y=len, col=supp)) +
  geom_point() 

# Vpliv doz na dolžino glede na tip dodatka - ločeni medsebojno primerljivi grafi
ToothGrowth %>%
  ggplot(aes(x=dose, y=len, col=supp)) +
  geom_point() + 
  facet_grid(.~supp) 

# Dodamo še predikcijski linearni model z intervali zaupanja
ToothGrowth %>%
  ggplot(aes(x=dose, y=len, col=supp)) +
  geom_point() + 
  facet_grid(.~supp) + 
  stat_smooth(method = "lm")

# (Delno) Risbo lahko tudi shranimo v spremenljivko
risba <- ggplot(data=ToothGrowth, aes(x=dose, y=len, col=supp))
summary(risba)  # Opis risbe

# izris
risba

# Jo popravimo, in novo risbo shranimo v drugo spremenljivko
nova <- risba + geom_point()

nova 


# Zbiršemo barvanje po suplementu in legendo
nova + 
  geom_point(aes(col=NULL)) +
  theme(legend.position="none")

# Obrat koordinatnih osi
ToothGrowth %>%
  ggplot(aes(x=dose, y=len, col=supp)) +
  geom_point() + 
  coord_flip()

# Določanje razmerja na sliki in izrez izrisanega dela
ToothGrowth %>%
  ggplot(aes(x=dose, y=len, col=supp)) +
  geom_point() + 
  coord_fixed(ratio=0.05, xlim=c(1,2))




