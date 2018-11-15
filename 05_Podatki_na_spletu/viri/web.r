## Obdelava podatkov
require(dplyr)

## Manipuliranje HTML in XML datotek
require(rvest)
require(XML)

## HTTP komunikacija
require(httr)

## strapplyc
require(gsubfn)

## Branje in pisanje v JSON format
require(jsonlite)

#############
## Primer 1: Imenik iz spletne strani FMF
#############

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
  html_text()

## Pridobitev imen
imena <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]//b//a") %>% html_text()

## Pomožni funkciji 'indeks' in 'prvi'
indeks <- function(x, i) {if(length(x) >= i) x[[i]] else ""}
prvi <- . %>% indeks(1)

## Primera
prvi(c("a", "b"))
prvi(character(0))
#prvi <- function(x) {index(x, 1)}

## Pridobitev id številk podstrani
id <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]//b//a/@href") %>% 
  html_text() %>% 
  strapplyc("(\\d+)") %>% 
  sapply(prvi) %>% 
  as.integer()

## Pridobitev e-mail naslovov z uporbo regularnih izrazov
email <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("mailto:([^\\\"]+)") %>%
  sapply(prvi) %>% 
  sub("^([^%]+)(%20\\(?(at)+\\)?%20)([^%]+)", "\\1@\\4", .)

## Pridobitev telefonskih številk
telefonska <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("\\+386[0-9 ]*[0-9]+") %>%
  sapply(prvi)


## Pridobitev internih številk
interna <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("Interna:\\s*([0-9]+)") %>%
  sapply(prvi)

## Pridobitev številk sobe
soba <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("Soba:\\s*([0-9]+)") %>%
  sapply(prvi) %>%
  as.integer()

## Sestavljanje končne razpredelnice
imenik <- data.frame(Naziv=nazivi, Oseba=imena, Email=email, Telefon=telefonska, Interna=interna, Soba=soba )


########################
## Primer 2 - pridobivanje geografskih podatkov o uličnih številkah
########################

url0 <- "http://prostor3.gov.si/iokno/iokno.jsp"

## Podatki v iframe
url <- "http://prostor3.gov.si/iokno/iskalnik_naslovi.jsp?ssid=255B1E35BFD50942E05018AC40D32BF1&user=null"

url <- "http://prostor3.gov.si/iokno/iskalnik_naslovi.jsp?ssid=256173CBFDA77837E05018AC40D36FBA&user=null"
## Seja (ne želimo se ukvarjati s cookie-ji)
seja <- html_session(url)
## Pridobitev forme
forma <- (seja %>% read_html() %>% html_form())[[1]]
## Izpolnjevanje polj v formi
formaNova <- set_values(forma, 
                        'NASELJE'='Ljubljana', 
                        'ULICA'= 'jadranska')
## Pošiljanje forme
seja2 <- submit_form(seja, formaNova)                        
                   
## Obdelava rezultatov
stran <- seja2 %>% read_html() 
tabela1 <- stran %>% html_nodes(xpath="//table[@id='naslovi']") %>% 
      html_table() %>%
      .[[1]]
names(tabela1) <- tabela1[1,]
tabela1 <- tabela1[-1,]

## Pridobivanje imena ulice
ulica <- stran %>% 
  html_nodes(xpath="(//table[@id='naslovi']//tr/td[3])[not(@bgcolor)]") %>% 
  html_text() %>% 
  gsub("^\\s+|\\s+$","", .) 

## Ulične številke
stevilke <- stran %>% 
      html_nodes(xpath="(//table[@id='naslovi']//tr/td[4])[not(@bgcolor)]") %>% 
      html_text() %>% 
      gsub("^\\s+|\\s+$","", .) 

## Abecedni dodatki k uličnim številkam
dodatki <- stran %>% 
      html_nodes(xpath="(//table[@id='naslovi']//tr/td[5])[not(@bgcolor)]") %>% 
      html_text() %>% 
      gsub("^\\s+|\\s+$","", .) 

## Lepljenje uličnih številk in dodatkov
stevilkeUlic <- paste(stevilke, dodatki, sep="")

stran %>% html_nodes(xpath="(//table[@id='naslovi']//tr/td[1])[not(@bgcolor)]") %>% length()

## Gauss-Kruegerjeve koordinate iz klicev funkcij v Javascriptu
rezultati <- stran %>% html_nodes(xpath="//table[@id='naslovi']//tr/td[6]/img/@onclick") %>% html_text() %>% strapplyc("(\\d+)")
x <- rezultati %>% sapply(. %>% indeks(1)) %>% as.integer()
y <- rezultati %>% sapply(. %>% indeks(2)) %>% as.integer()

## Končana razpredelnica
podatki <- data.frame(Ulica=ulica, Stevilka=stevilkeUlic, X=x, Y=y)

podatki <- tabela1 %>% mutate(X=x, Y=y) %>% .[-6]

##########################
## Primer 4: Razpoložljive zaposlitve na zavodu za zaposlovanje
## !!! Nedokončano
##########################

## Povezava nam vrne prvo stran rezultatov
url <- "http://www.ess.gov.si/iskalci_zaposlitve/prosta_delovna_mesta/seznam?q=1&s=1"

## Pridobivanje strani - httr
r <- GET(url)
stran <- r %>% read_html()

## Pridobivanje strani 
pridobiID <- function(stran) {
  stran %>% html_nodes(xpath="//tbody//tr[@class='alt' or @class='talt']//td[1]//a//@href") %>%
    html_text() %>% 
    strapplyc("IDEPD=(\\d+)") %>%
    sapply(prvi)
}

idStevilke <- pridobiID(stran)

## Primer z eno id stevilko
id <- idStevilke[[1]]

## Pomožna funkcija za nalaganje in shranjevanje podstrani
nalozi_stran_ESS <- function(id, prefix="test") {
  url2 <- "http://www.ess.gov.si/iskalci_zaposlitve/prosta_delovna_mesta/posamezno?IDEPD="
  url2x <- paste(url2, id, sep="")
  r2 <- GET(url2x)
  write(content(r2, "text"), paste(prefix, id, ".html", sep=""))
}

## Primer uporabe
prefix <- "test"
for (id in idStevilke) nalozi_stran_ESS(id, prefix=prefix)

## Obdelava ene strani iz datoteke
obdelaj <- function(id, prefix) {
  stran2 <- read_html(paste(prefix, id, ".html", sep=""))
  kljuc <- stran2 %>% 
    html_nodes(xpath="//tbody//tr[@class='alt' or @class='talt']//td[@class='firstcol']") %>% 
    html_text()
  zapis <- stran2 %>% 
    html_nodes(xpath="//tbody//tr[@class='alt' or @class='talt']//td[not(@class)]") %>% 
    html_text() %>% 
    as.list()
  names(zapis) <- kljuc
  data.frame(zapis)
}

## Sestavljanje data.frame iz podatkov v datotekah
all <- data.frame()
for (id in idStevilke) {
  df <- obdelaj(id, prefix=prefix)
  # Združevanje data.frame-ov po istih stolpcih
  all <- bind_rows(all, df)
}




dodajElement function(forma, element, name) {
  forma$fields <- c(forma$fields, name=element)
}

seja <- html_session(url)
## Predhodno shranjena forma z vsemi skriti polji (dodanimi z javascriptom)
forma1 <- (read_html("ZRSZ - Seznam.html") %>% html_form())[[1]]
## Prebrana forma
forma2 <- (seja %>% read_html() %>% html_form())[[1]]

pridobiID(seja %>% read_html())

forma3 <- set_values(forma1, 
                     '__VIEWSTATE'=forma2$fields[['__VIEWSTATE']]$value,
                     '__VIEWSTATEGENERATOR'=forma2[['__VIEWSTATEGENERATOR']]$value
                     '__EVENTTARGET'='ctl00$centerContainer$ctl01$GridView1', 
                     '__EVENTARGUMENT'='Page$5',
                     '__LASTFOCUS'=""       
)

s2 <- submit_form(seja, forma3)
pridobiID(s2 %>% read_html())

doc <- seja %>% read_html()
forma <- (doc %>% html_nodes(xpath="//form[1]"))[[1]]
nd <- xmlNode("input", attrs=c(type="hidden", name="__EVENTTARGET", id="__EVENTTARGET", value=""))



glava <- function(niz) {
  tmp <- strapplyc(niz, "^([^\\]+)\\:(.+)$")
  data.frame(kljuc=indeks(tmp, 1), vrednost=indeks(tmp, 2))
}


#########
## Primer 4: API QUANDL 
#########

## Pridobivanje podatkov iz API na QANDL
r <- GET("https://www.quandl.com/api/v3/datasets/LBMA/GOLD.json")
# text <- content(r, "text")

## Branje iz formata JSON
data <- fromJSON(content(r, "text"))

data$dataset %>% names() %>% print()

## Pridobivanje razpredelnice in obdelava podatkov
tabela <- data.frame(data$dataset$data, stringsAsFactors=FALSE)
names(tabela) <- data$dataset$column_name
print(sapply(tabela, class))

## Pretvorba stolpcev
for (name in names(tabela)[-1]) {
  tabela[[name]] <- as.double(tabela[[name]])
}

print(sapply(tabela, class))
print(head(tabela))

###########
## Primer 6
## Branje enostavnega html iz predavanj
###########

stran <- read_html("primer.html")
stran %>% html_nodes(xpath="/html/head/meta/@content") %>% html_text()

prviStolpec <- stran %>% html_nodes(xpath="//table[1]/tr/td[1]") %>% 
    html_text() %>% 
    gsub("^\\s+|\\s+$","", .) %>%
    as.integer()

drugiStolpec <- stran %>% html_nodes(xpath="//table[1]/tr/td[2]") %>% 
  html_text() %>% 
  gsub("^\\s+|\\s+$","", .) %>%
  as.integer()

data.frame(prvi=prviStolpec, drugi=drugiStolpec)

########
## Razno iz predavanj - glej Primer 1 za celoto
########

# url <- "http://www.fmf.uni-lj.si/si/iskanje?q=.&cmd=+I%C5%A1%C4%8Di+&m=any&wm=beg"
# seja <- html_session(url)
# stran <- seja %>% read_html()

# nazivi <- stran %>% html_nodes(xpath="//table[@class='directory-list']//tr/td[@class='title-left']") %>%
#     html_text() %>%
#     gsub("^\\s+|\\s+$","", .)


# imena <- stran %>% html_nodes(xpath="//table[@class='directory-list']//tr/td[2]/b") %>% html_text()

## Pomožni funkciji 'indeks' in 'prvi'
# indeks <- function(x, i) {if(length(x) >= i) x[[i]] else ""}
# prvi <- . %>% indeks(1)


# stran %>% html_nodes(xpath="//table[@class='directory-list']//tr/td[2]") %>% 
#           sapply(toString) %>%
#           strapplyc( "<a[^>]+>([^<]+)<") %>%
#           sapply(. %>% indeks(2)) %>% length

