## Obdelava podatkov
require(dplyr)

## Manipuliranje HTML in XML datotek
require(rvest)

#############
## Primer 1: Branje tabele iz Wikipedije
#############

url <- "https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(nominal)"

read_html(url) %>%
    html_nodes(xpath = "//table[@class='wikitable']//table") %>% 
    .[[1]] %>%
    html_table() %>% View
    rename(
      Country=2,
      GDP=3
    ) %>%
    mutate(
      GDP = parse_number(GDP),
      Country = gsub("\\[[^]]*\\]","", Country)
    ) 
  
#########
## Primer 2: API QUANDL 
#########

## HTTP komunikacija
require(httr)
    
## Branje in pisanje v JSON format
require(jsonlite)
    
url <- "https://www.quandl.com/api/v3/datasets/LBMA/GOLD.json"
## Pridobivanje podatkov iz API na QANDL
json <- GET(url) %>%
  content("text") %>%  # tekst
  fromJSON()    #json v R strukturi

stolpci <- json$dataset$column_name
podatki <- json$dataset$data

tabela <- data.frame(data$dataset$data, stringsAsFactors=FALSE)
tabela <- data$dataset$data %>% as_tibble(.name_repair = "unique")
names(tabela) <- data$dataset$column_name
tabela %>% sapply(class) # Preverimo tip stopcev
tabela[,-1] <- tabela[,-1] %>% lapply(as.numeric)  # Pretvorba nizov
tabela %>% sapply(class) # Preverimo še enkrat
tabela %>% View

names(tabela) <- data$dataset$column_name
print(sapply(tabela, class))


#############
## Primer 3: Imenik iz spletne strani FMF
#############

## strapplyc
require(gsubfn)

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

