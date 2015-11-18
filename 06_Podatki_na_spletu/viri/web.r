require(dplyr)
## Knjižnica za lažje manipuliranje XML in HTML datotek
require(rvest)
##
require(gsubfn)

#webpage <- read_html("http://www.reed.edu/ir/geographic_states.html")


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

## Pridobitev številk podstrani
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
  sapply(prvi)


## Pridobitev e-mailov
email <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("mailto:([^\\\"]+)") %>%
  sapply(prvi)

## Pridobitev telefonskih številk
telefonska <- stran %>% 
  html_nodes(xpath = "//table[@class='directory-list']//td[not(@class)]") %>% 
  sapply(toString) %>%
  strapplyc("\\+386 1 4766 [0-9 ]+") %>%
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


