require(dplyr)

#############################################
## Regularni izrazi
#############################################

# ## Delo z nizi
# 
# - Skoraj vedno delamo s tekstovnim zapisom podatkov (datoteke, splet)
# - Pogosta operacija: prepoznavanje zapisa v določenem formatu
# - Katero število predstavlja niz "10202"? 
# - A je v nizu "102a" sploh število? Kaj pa v nizu "1O2"?
# - Radi bi uporabili samo 2. indeks v nizu "matrika[2,34]" 
# - Včasih moramo biti robustni na dodatne presledke: "matrika[2,  34 ]"
# - V stolpcu imamo podatke v nizu v obliki "2,32.1, 0.4"
#   in bi radi to razbili na 3 številske vrednosti.
# 
# 
# ## Vzorci  {.build}
# 
# - Vizualno bi znali opisati vzorce.
# - Težje bi bilo napisati kodo, ki pregleduje niz znak po znak 
#   in se odloča o ujemanju, podnizih, delitvah, ...
# - Podobno, ponavljajoče se delo; veliko možnosti za napake
# - Regularni izrazi 
#    - jezik za opis vzorcev
#    - funkcije za opis manipulacij nad nizi, ki se ujemajo z vzorci

# Plonkec za delo z nizi:
# https://github.com/rstudio/cheatsheets/raw/master/strings.pdf

#############################################
## Osnovni vzorci, metaznaki
#############################################

## Dobesedni vzorec, identificiranje vzorca z grep
niz <- "avto"
vzorec <- "avto"
grep(vzorec, niz)

## Uporaba grep in grepl nad vektorji
nizi <- c("avto", "avtomobil", "avokado", "avtokado", "navto")
vzorec <- "avto"
grep(vzorec, nizi)
grepl(vzorec, nizi)

# Modernejša knjižnica za delo z nizi `stringr`
require(stringr)

str_which(nizi, vzorec)

# Prilagojena za veriženje
nizi %>% str_which(vzorec)
nizi %>% str_detect(vzorec)

# Vzorec od začetka niza
nizi <- c("avto", "avtomobil", "avokado", "avtokado", "navto")
nizi %>% str_which("^avto")

# Grafičen pogled ujemanja
nizi %>% str_view_all("^avto")

## Vzorec z začetkom in koncem niza (celotni v vzorec). Vzorec '.' = kateri koli znak 
nizi %>% str_view_all("^av..$")

## Dvojna poševnica (ubežni znak v vzorcu). Posebni vzorci
# - Metaznaki: ```. \ | ( ) [ { ^ $ * + ?```
# - `\`- ubežni znak
# - `.` - kateri koli (eden) znak
# - `^` - začetek niza
# - `$` - konec niza


## Alfanumerični znak. `str_view_all` pokaže vsa ujemanja
nizi2 <- c(" ", "a", "1", "A", "%", "\t")

nizi2 %>% str_view_all("\\w")

nizi2 %>% str_which("\\w")

## Ne-alfanumerični znak
nizi2 %>% str_view_all("\\W")

## Beli znak
nizi2 %>% str_view_all("\\s")

## Ne-beli znak
nizi2 %>% str_view_all("\\S")

## Števka
nizi2 %>% str_view_all("\\d")

## Alternativni zapis 
nizi2 %>% str_view_all("[:digit:]")

## Znak različen od števke
nizi2 %>% str_view_all("\\D")
nizi2 %>% str_view_all("[^[:digit:]]")

## 'Eden izmed'
c("avto", "bus", "ne", "vozi") %>% 
    str_view_all("^[abc]\\w\\w")

## Tričrkovni niz iz malih črk ang. abecede
c("Čas", "teče", "nič", "ne", "reče:", "tik", "tak") %>% 
    str_view_all("^[a-z][a-z][a-z]$")

## Tričrkovni niz, z nekaterimi šumniki
c("Čas", "teče", "nič", "ne", "reče:", "tik", "tak") %>% 
  str_view_all("^[a-zA-ZčšžČŠŽ][a-zčšž][a-zčšž]$")

## Alternativni vzorci (eden ali drugi) - (X|Y)
## Podizraz v oklepajih = skupina (oz. podvzorec)
c("1", "20", "0", "nič", "to je 100%", "09") %>%
    str_view_all("^((\\d)|([1-9]\\d))$")


#############################################
## Ponavljanja vzorcev
#############################################

# - Operatorji za ponavljanje delujejo na prejšnji znak ali skupino
# - `?` - kvečjemu ena ponovitev 
# - `*` - nič ali več ponovitev 
# - `+` - ena ali več ponovitev 
# - `{m}` – natanko `m` ponovitev 
# - `{m, n}` – `m` do `n` ponovitev 
# - `{m, }` – vsaj `m` ponovitev 


## Nič ali več ponovitev predhodnega vzorca (znaka ali skupine)
c("", "slika", "je", "vredna", "1000", "besed.") %>%
    str_view_all("^[a-z]*$")

## Ena ali več ponovitev predhodnega vzorca (skupine)
c("", "slika", "je", "vredna", "1000", "besed.") %>%
  str_view_all("^[a-z]+$")

## Nizi iz besed iz malih ang. črk ločeni z enim presledkom 
c("besede", "ali pa stavki", "123 ni", "aa  bbbb") %>%
  str_view_all("^([a-z]+ )*[a-z]+$")

## 3 do 5 ponovitev predhodnega vzorca (skupine)
c("ta", "beseda", "nima", "pomena") %>%
  str_view_all("^[a-z]{3,5}$")

## Predznačena cela števila

c("0", "+1", "01", "-99") %>%
    str_view_all("^[+-]?(0|[1-9][0-9]*)$")

#############################################
## Podvzorci, grupe, zamenjave v nizih
#############################################

## Zamenjevanje v nizih
# Zamenjajmo vsa zaporedja 'aaa' z enim 'a'-jem
c("aaaaabeceda", "aaaaavto") %>%
  str_replace("a+", "a")

# - Podvzorce označimo s skupinami (okrogli oklepaji)
# - Skupine identificiamo po številki
# ```
# (....(..(.))..(..(.)...(.))
#  1    2  3     4  5     6
#  ```

## Podvzorci: komponente vektorja
nizi4 <- c(
  "(1,2)", 
  "( -2, 7)", 
  "(    -3   ,     45)", 
  "(a, 3)"
)


vzorec <- "\\(\\s*([+-]?(0|[1-9][0-9]*))\\s*,\\s*([+-]?(0|[1-9][0-9]*))\\s*\\)"
                  
## "Zamenjava" na "ujetih" podvzorcih
# Zamenjaj cel niz s prvim podvzorcem (osnovni R) - če se kaj ujema, če ne pusti
gsub(vzorec, "\\1", nizi4)

# knjižnica `stringr`
nizi4 %>% str_replace(vzorec, "\\1")

# Vse grupe hkrati
nizi4 %>% str_match(vzorec)

nizi4 %>% 
  str_match(vzorec) %>%  # matrika ujemanj
  .[,c(2,4)] %>%         # 2. in 4. stolpec kot niz 
  apply(2, as.integer) # pretvori stolpca v celo število, če gre


#############################################
## Razcepljaje nizov
#############################################

## Separacija večkratnih vrednosti v stolpcu: Višina, Širina in Dolžina
cudenStolpec <- c("12, 3; 8", "6 1,2") 
separator <- "[\\,\\; ] *"

cudenStolpec %>%
  str_split(separator)
  
podatki <- cudenStolpec %>%
  str_split_fixed(separator, n=Inf) %>% 
  apply(2, as.integer) %>% 
  as_tibble() %>% 
  rename(x=1, y=2, z=3) %>% 
  mutate(
    volumen=x*y*z
  ) 

podatki %>% View

#################################################
#### Primer: imenik oseb na FMF
#################################################

require(rvest)
## Imenik oseb na FMF
url <- "http://www.fmf.uni-lj.si/si/iskanje?q=.&cmd=+I%C5%A1%C4%8Di+&m=any&wm=beg"

## Branje spletne strani
stran <- read_html(url)

## Shranjevanje v tekstovno datoteko
write_xml(stran, "imenik.html")

## Branje spletne strani iz tekstovne datoteke
stran <- read_html("imenik.html")

## Pridobitev nazivov 
nazivi <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[@class='title-left']") %>% 
  html_text() %>%
  str_trim()

nazivi %>% View

## Pridobitev imen
imena <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]//b//a") %>% 
  html_text()

imena %>% View 

## Pridobitev id številk podstrani
id <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]//b//a/@href") %>% 
  html_text() %>% 
  str_extract("\\d+") %>% 
  as.integer()

id %>% View

## Pridobitev e-mail naslovov z uporbo regularnih izrazov
email <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>% 
  str_match("mailto:([^\\\"]+)") %>%  
  .[,2]

email %>% View
  

## Neujemajoči se mejli, čiščenje

emailVzorec <- "\\w+(\\.\\w+)*@\\w+(\\.\\w+)+"

cudniEmaili <- . %>%  
  as_tibble() %>%
  rename(email=1) %>%
  filter(
    !str_detect(email,emailVzorec)
  ) %>% View

email %>% cudniEmaili

emailCist <- email %>% 
  str_replace("sergio.cabello%20", "sergio.cabello@fmf.uni-lj.si") %>% 
  str_replace("%5B%5Bat%5D%5D", "@") %>%
  str_replace("%20at%20", "@") %>% 
  str_replace("%20\\(atat\\)%20", "@") 

## Pridobitev telefonskih številk
telefonske <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>% 
  str_extract("\\+386[\\d\\(\\) ]+") %>% 
  str_replace("\\(0\\)", "0") %>% 
  str_replace("\\(", "") %>%
  str_trim() %>% 
  str_replace("^\\+386 1 4766$", NA_character_)

telefonske %>% View

## Pridobitev internih številk
interna <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  str_match("Interna:\\s*([0-9]+)") %>% 
  .[,2]

interna %>% View
## Pridobitev številk sobe

soba <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>% 
  str_match("Soba:([^<]+)") %>% 
  .[,2] %>% 
  str_trim()

soba %>% View

## Sestavljanje končne razpredelnice
imenik <- data.frame(
  Naziv=nazivi, 
  Oseba=imena, 
  Email=emailCist, 
  Telefon=telefonske, 
  Interna=interna, 
  Soba=soba,
  stringsAsFactors = FALSE
)

imenik %>% View

# list(nazivi, imena, emailCist, telefonske, interna, soba) %>% sapply(class)


