require(dplyr)
require(tidyr)
require(readr)


# Poskusi
# Gola datoteka
uvoz <- read_csv2("viri/0955201ss.csv")

# Če je napaka povezana z "multibyte string", gotovo nimamo prave kodne tabele
uvoz <- read_csv2("viri/0955201ss.csv", 
                  locale=locale(encoding="cp1250"))
  
problems(uvoz) %>% View
View(uvoz)

# Zaradi zelo čudnega formata prebere samo en stoplec. Vsilimo vse stolpce z imeni.
stolpci <- c("VISOKOSOLSKI_ZAVOD", "VRSTA_IZOBRAZEVANJA", "LETNIK", "NACIN_STUDIJA", "SPOL" , "STUDIJSKO_LETO", "ST_STUDENTOV")
uvoz <- read_csv2("viri/0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci)

problems(uvoz)  
View(uvoz)

# Boljše :). Prve tri vrstice (oz. štiri v datoteki bi izpustili)        
uvoz <- read_csv2("viri/0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci,
                  skip=4)

View(uvoz)

# V resnici nas zanimajo podatki samo do (prebrane) vrstice 7162
uvoz <- read_csv2("viri/0955201ss.csv", 
                  locale=locale(encoding="cp1250"),
                  col_names=stolpci,
                  skip=4,
                  n_max=7162)

View(uvoz)

# Namesto črtic "-" bi v zadnjem stolpcu želeli NA
uvoz <- read_csv2("viri/0955201ss.csv", 
                     locale=locale(encoding="cp1250"),
                     col_names=stolpci,
                     skip=4,
                     n_max=7162,
                     na=c("", " ", "-"))
                     
View(uvoz)

sapply(uvoz, class)

# Zaradi 'hierarhičnega uvoza' bi radi, da se vsi dimenzijski stolpci ponavljajo (razen STUDIJSKO_LETO !)

podatki <- uvoz %>% 
           fill(1:5) %>% 
           drop_na(STUDIJSKO_LETO)

podatki %>% View


# Preverimo ustreznost tipov
sapply(podatki, class)

# Pretvorba tipa z readr parse_* funkcijo (ni potrebna)
# podatki$ST_STUDENTOV <- parse_integer(podatki$ST_STUDENTOV)

sapply(podatki, class)

podatki %>% View

# Obdelava po stolpcih. Stolpec LETNIK
# pregled vrednosti
table(podatki$LETNIK)


# Pretvorba v krajši zapis. Skonstruirajmo preslikavo.
letniki <- c("1","2","3","4","5","6","Abs")
imena <- c("1. letnik", "2. letnik", "3. letnik", "4. letnik", "5. letnik", "6. letnik", "Absolventi")
tab2 <- data.frame(letnik=letniki, ime=imena, stringsAsFactors = FALSE)

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
    summarize(VPIS=sum(ST_STUDENTOV, na.rm=TRUE)) %>%
    View

# "štih proba"
zdruzena %>% 
  filter(STUDIJSKO_LETO==2006) %>% 
  pull(ST_STUDENTOV) %>%   # enako kot $ST_STUDENTOV - vrne vektor kot stolpec
  sum(na.rm=TRUE)

# Koliko je bilo vpisanih po spolih za posamezna leta

zdruzena %>% 
    group_by(SPOL, STUDIJSKO_LETO) %>% 
    summarize(VPIS=sum(ST_STUDENTOV, na.rm=TRUE)) %>%
    View

# Koliko žensk in koliko moških je bilo vpisanih na posameznih vrstah študija na univerzi

zdruzena %>% 
  filter(VISOKOSOLSKI_ZAVOD == "Univerze - SKUPAJ") %>%
  select(VRSTA_IZOBRAZEVANJA, SPOL, ST_STUDENTOV) %>% 
  group_by(VRSTA_IZOBRAZEVANJA, SPOL) %>% 
  summarize(STEVILO=sum(ST_STUDENTOV, na.rm=TRUE)) %>% 
  spread(SPOL, STEVILO) %>% 
  arrange(VRSTA_IZOBRAZEVANJA) %>% 
  mutate(
    deležMoški=round(Moški/(Moški + Ženske), 2), 
    deležŽenske=round(Ženske/(Moški + Ženske), 2)
  ) %>%
  View

################

## https://pxweb.stat.si/pxweb/Dialog/varval.asp?ma=0955201s&ti=&path=../Database/Dem_soc/09_izobrazevanje/08_terciarno_izobraz/01_09552_vpisani_dodiplomska/&lang=2
# celovito preoblikovanje tabel v tidy data
read_csv2("viri/0955201ss-nova-nontidy.csv", 
           locale=locale(encoding="cp1250"),
           col_names=TRUE,
           skip=4,
           n_max=1092,
           na=c("", " ", "-")
          ) %>% 
          rename(
            VISOKOSOLSKI_ZAVOD = 1, 
            VRSTA_IZOBRAZEVANJA = 2,
            LETNIK = 3,
            NACIN_STUDIJA = 4
          ) %>% 
          fill(1:4) %>% 
          .[4:nrow(.), ] %>% 
          # drop_na(NACIN_STUDIJA) %>%
          filter(
            VISOKOSOLSKI_ZAVOD != "Visokošolski zavodi - SKUPAJ",
            VRSTA_IZOBRAZEVANJA != "Vrsta izobraževanja - SKUPAJ",
            LETNIK != "Letniki - SKUPAJ",
            NACIN_STUDIJA != "Način študija - SKUPAJ"
          ) %>% 
          gather(SOLSKO_LETO, ST_STUDENTOV, -1:-4) %>%
          separate(SOLSKO_LETO, into=c("LETO", "DRUGO_LETO"), sep='/') %>% 
          # View
          # pull(LETO) %>%
          # unique() %>%
          # as.integer() %>%
          # (function(x) {x >= 2000}) %>%   # ad-hoc funkcija
          # all()   # all(vec) - ali so vse vrednosti v vektorju TRUE
          mutate(
            DRUGO_LETO=paste0("20",DRUGO_LETO) %>% as.integer(),
            LETO=as.integer(LETO)
          ) %>%
          View



