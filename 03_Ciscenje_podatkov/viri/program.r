require(dplyr)
require(tidyr)
require(readr)


# Poskusi
# Gola datoteka
uvoz <- read_csv2("0955201ss.csv")

# Če je napaka povezana z "multibyte string", gotovo nimamo prave kodne tabele
uvoz <- read_csv2("0955201ss.csv", 
                  locale=locale(encoding="cp1250"))
  
problems(uvoz)  
View(uvoz)

# Zaradi zelo čudnega formata prebere samo en stoplec. Vsilimo vse stolpce z imeni.
stolpci <- c("VISOKOSOLSKI_ZAVOD", "VRSTA_IZOBRAZEVANJA", "LETNIK", "NACIN_STUDIJA", "SPOL" , "STUDIJSKO_LETO", "ST_STUDENTOV")
uvoz <- read_csv2("0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci)

problems(uvoz)  
View(uvoz)

# Boljše :). Prve tri vrstice (oz. štiri v datoteki bi izpustili)        
uvoz <- read_csv2("0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci,
                  skip=4)

View(uvoz)

# V resnici nas zanimajo podatki samo do (prebrane) vrstice 7162
uvoz <- read_csv2("0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci,
                  skip=4,
                  n_max=7162)

View(uvoz)

# Namesto črtic "-" bi v zadnjem stolpcu želeli NA
uvoz <- read_csv2("0955201ss.csv", 
                     locale=locale(encoding="cp1250"),
                     col_names=stolpci,
                     skip=4,
                     n_max=7162,
                     na=c("", " ", "-"))
                     
View(uvoz)

# Zaradi 'hierarhičnega uvoza' bi radi, da se vsi dimenzijski stolpci ponavljajo (razen STUDIJSKO_LETO !)

podatki <- uvoz %>% 
           fill(1:5) %>% 
           drop_na(STUDIJSKO_LETO)

podatki %>% View


# Preverimo ustreznost tipov
sapply(podatki, class)

# Pretvorba tipa z readr parse_* funkcijo
podatki$ST_STUDENTOV <- parse_integer(podatki$ST_STUDENTOV)

sapply(podatki, class)

podatki %>% View

# Obdelava po stolpcih. Stolpec LETNIK
# pregled vrednosti
table(podatki$LETNIK)

## DPLYR

# Pretvorba v krajši zapis. Skonstruirajmo preslikavo.
letniki <- c("1","2","3","4","5","6","Abs")
imena <- c("1. letnik", "2. letnik", "3. letnik", "4. letnik", "5. letnik", "6. letnik", "Absolventi")
tab2 <- data.frame(letnik=letniki, ime=imena)
tab2$ime <- as.character(tab2$ime)  # data.frame naredi factor

head(tab2)
sapply(tab2, class)

zdruzena <- podatki %>% inner_join(tab2, c("LETNIK"="ime"))

zdruzena %>% View

sapply(zdruzena, class)

# Vse vrstice, v katerih so ženske

filter(zdruzena, SPOL=="Ženske")

zdruzena %>% filter(SPOL=="Ženske") %>% View

# Vse vrstice, v katerih so ženske vpisane po letu 2011

zdruzena %>% filter(SPOL=="Ženske" & STUDIJSKO_LETO > 2011) %>% View

# Uredi po ST_STUDENTOV

zdruzena %>% arrange(ST_STUDENTOV) %>% View

# Uredi po STUDIJSKO_LETO in znotraj njih po ST_STUDENTOV padajoče

zdruzena %>% arrange(STUDIJSKO_LETO, desc(ST_STUDENTOV)) %>% View

# Izberi samo stolpce STUDIJSKO_LETO, ST_STUDENTOV in SPOL 

zdruzena %>% select(STUDIJSKO_LETO, ST_STUDENTOV, SPOL) %>% View

# Ob tem še preimenuj stolpec STUDIJSKO_LETO -> LETO

zdruzena %>% select(LETO=STUDIJSKO_LETO, ST_STUDENTOV, SPOL) %>% View

# Na tabeli zdruzena preimenuj stolpec STUDIJSKO_LETO -> LETO

zdruzena %>% rename(LETO=STUDIJSKO_LETO) %>% View

# Funkcije dplyr ne spreminjajo originalne tabele!
zdruzena %>% View

# Za katera leta imamo podatke

zdruzena %>% select(STUDIJSKO_LETO) %>% distinct()

# Koliko študentov je bilo vpisanih vsako leto

zdruzena %>% 
    group_by(STUDIJSKO_LETO) %>% 
    summarize(VPIS=sum(ST_STUDENTOV, na.rm=TRUE))

# Koliko je bilo vpisanih po spolih za posamezna leta

zdruzena %>% 
    group_by(SPOL, STUDIJSKO_LETO) %>% 
    summarize(VPIS=sum(ST_STUDENTOV, na.rm=TRUE))

# Koliko žensk in koliko moških je bilo vpisanih na posameznih vrstah študija na univerzi

tmp1 <- zdruzena %>% 
   filter(VISOKOSOLSKI_ZAVOD == "Univerze - SKUPAJ") %>%
   select(VRSTA_IZOBRAZEVANJA, SPOL, ST_STUDENTOV) 

tmp2 <- tmp1 %>% 
   filter(SPOL=="Moški") %>% 
   group_by(VRSTA_IZOBRAZEVANJA) %>%
   summarize(Moški=sum(ST_STUDENTOV, na.rm=TRUE))

tmp3 <- tmp1 %>% 
  filter(SPOL=="Ženske") %>% 
  group_by(VRSTA_IZOBRAZEVANJA) %>%
  summarize(Ženske=sum(ST_STUDENTOV, na.rm=TRUE))

tmp4 <- tmp2 %>% 
        inner_join(tmp3) %>%
        select(VRSTA_IZOBRAZEVANJA, Moški, Ženske) %>%
        arrange(VRSTA_IZOBRAZEVANJA) %>%
        mutate(deležMoški=round(Moški/(Moški + Ženske), 2), deležŽenske=Ženske/(Moški + Ženske))

View(tmp4) 
