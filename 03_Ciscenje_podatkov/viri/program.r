require(zoo)
require(dplyr)

stolpci <- c("VISOKOSOLSKI_ZAVOD", "VRSTA_IZOBRAZEVANJA", "LETNIK", "NACIN_STUDIJA", "SPOL" , "STUDIJSKO_LETO", "ST_STUDENTOV")

podatki <- read.csv2("0955201ss.csv", fileEncoding="cp1250", skip=4, nrow=7166-4, col.names=stolpci, header=FALSE)
for (ime in stolpci[c(-6,-7)]) {
    podatki[[ime]][podatki[ime] == " "] <- NA;
    podatki[[ime]] <- na.locf(podatki[[ime]]);
}

tabela <- podatki[!is.na(podatki$STUDIJSKO_LETO),]
tabela$ST_STUDENTOV[tabela$ST_STUDENTOV == "-"] <- NA
tabela$ST_STUDENTOV <- as.integer(tabela$ST_STUDENTOV)

letniki <- c("1","2","3","4","5","6","Abs")
imena <- c("1. letnik", "2. letnik", "3. letnik", "4. letnik", "5. letnik", "6. letnik", "Absolventi")

tab2 <- data.frame(letnik=letniki, ime=imena)
tab2$ime <- as.character(tab2$ime)

zdruzena <- inner_join(tabela, tab2, c("LETNIK"="ime"))

# Vse vrstice, v katerih so ženske

filter(zdruzena, SPOL=="Ženske")

# Vse vrstice, v katerih so ženske vpisane po letu 2011

filter(zdruzena, SPOL=="Ženske" & STUDIJSKO_LETO > 2011)

# Uredi po ST_STUDENTOV

arrange(zdruzena, ST_STUDENTOV)

# Uredi po STUDIJSKO_LETO in znotraj njih po ST_STUDENTOV padajoče

arrange(zdruzena, STUDIJSKO_LETO, desc(ST_STUDENTOV))

# Izberi samo stolpce STUDIJSKO_LETO, ST_STUDENTOV in SPOL 

select(zdruzena, STUDIJSKO_LETO, ST_STUDENTOV, SPOL)

# Ob tem še preimenuj stolpec STUDIJSKO_LETO -> LETO

select(zdruzena, LETO=STUDIJSKO_LETO, ST_STUDENTOV, SPOL)

# Na tabeli zdruzena preimenuj stolpec STUDIJSKO_LETO -> LETO

rename(zdruzena, LETO=STUDIJSKO_LETO)

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
        left_join(tmp3) %>%
        select(VRSTA_IZOBRAZEVANJA, Moški, Ženske) %>%
        arrange(VRSTA_IZOBRAZEVANJA) %>%
        mutate(faktor=round(Ženske/Moški, 2))
     