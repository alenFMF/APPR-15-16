library("readxl")   # funkcije za branje Excelovih datotek 
library("openxlsx")   # funkcije za pisanje Excelovih datotek
library("dplyr")    # funkcije za lažjo manipulacijo operacij na razpredelnicah

# Uvoz razpredelnice iz prvega lista Excelove datoteke
podatki <- read_xlsx("viri/primer.xlsx")
View(podatki)    # Interaktivni izpis razpredelnice v RStudiu

# Alternativni uvoz, če želimo izbrati list
podatki <- read_xlsx("viri/primer.xlsx", sheet="List1")
View(podatki)

# Izračun stolpca vrednosti iz dveh stolpcev
print(podatki$Št_prijateljev/podatki$Starost)

podatki$Št_prijateljev_na_leto <- podatki$Št_prijateljev/podatki$Starost
podatki %>% View   # uporaba oparatorja "veriženje" iz knjižnice dplyr. 

podatki$Št_prijateljev_na_leto <- round(podatki$Št_prijateljev_na_leto, 2)
podatki %>% View

# Izračun novega stolpca s pomočjo funkcije mutate (dplyr)
podatki <- podatki %>% 
       mutate(Št_prijateljev_na_leto=round(Št_prijateljev/Starost, 2))
podatki %>% View

# Kopiranje stolpca v vektor ...
priimki <- podatki$Priimek
priimki %>% print

# ... in brisanje
podatki$Priimek <- NULL
podatki %>% View

# Dodajanje stolpca iz shranjenega vektorja
podatki$Priimek <- priimki
podatki %>% View

# Brisanje (in dodajanje stolpca s pomočjo knjižnice dplyr)
# Znak '-' v funkciji 'select' pomeni - "brez tega stolpca" 
podatki2 <- podatki %>% select(-Priimek)    
podatki2 %>% View

# Ponovno dodajanje stolpca
podatki2 %>% mutate(Priimek=priimki) %>% View

# Preureditev stolpcev (vrstni red stolpcev)
# Razpredelnico smatramo kot seznam stolpcev in uporabimo operator []
podatki[c(1,5,2,3,4)] %>% View

podatki <- podatki[c(1,5,2,3,4)]
podatki %>% View

# Izbor določenih stolpcev
podatki[c("Ime", "Št_prijateljev")]

# z dplyr in funkcijo select
podatki %>% select(Ime, Št_prijateljev)

# Preimenovanje stolpca
names(podatki)[4] <- "ST_PRIJATELJEV"
names(podatki) %>% print

# Preimenovanje s pomočjo knjižnice dplyr
podatki <- podatki %>% rename(st_prijateljev=ST_PRIJATELJEV) 
podatki %>% names %>% print

# Filtriranje vrstic, ki ustrezajo pogoju
podatki[podatki$Starost >= 15, ]
podatki[podatki$Starost >= 15 & podatki$Starost <= 30, c("Ime")]

# Filtriranje s knjižnico dplyr
podatki %>% filter(Starost >= 15)
podatki %>% filter(Starost >= 15 & Starost <= 30) %>% select(Ime)

# Preurejanje (sortiranje po vrsticah)
podatki$Starost %>% print 

# Izračun vektorja indeksov prerazporeditve, ki uredi zaporedje
order(podatki$Starost)  

# Nova razpredelnica s prerazporejenimi vrsticami
novi <- podatki[order(podatki$Starost), ]
novi %>% View

podatki %>% View

# Urejanje s knjižnico dplyr
podatki %>% arrange(Starost)

# Urejanje v obratnem vrstnem redu
podatki %>% arrange(desc(Starost))

# Zapis nove razpredelnice v Excelovo datoteko
write.xlsx(novi, "viri/primer2.xlsx")

# Celotno zaporedje (dejanskih) transformacij zapisano s pomočjo veriženja
read_xlsx("viri/primer.xlsx") %>%
  mutate(Št_prijateljev_na_leto=round(Št_prijateljev/Starost, 2)) %>%   # izračunaj nov stolpec
  rename(st_prijateljev=Št_prijateljev) %>%   # preimenovanje stolpca
  arrange(Starost) %>%   # preurejanje vrstic
  write.xlsx("viri/primer2.xlsx")  # zapis na novo datoteko

# Zapis v evropski CSV format
podatki %>% write.csv2("viri/primer2.csv",fileEncoding = "utf8", row.names = FALSE)

# Branje iz evropskega CSV formata

read_csv2("viri/primer2.csv") %>% View

