library(dplyr)
library(rvest)
library(stringr)
library(readr)
library(openxlsx)
library(tidyr)
library(data.table)
library(visNetwork)

# Glej videe, ki opisujejo nastanek te datoteke.
#  https://youtu.be/We58xRU0vew 
#  https://youtu.be/hJQWqo0WHE8 
#  https://youtu.be/hSQm_5gtltM 
#  https://youtu.be/UtzsW7F0KJ0 
#  https://youtu.be/p_GDDJJiRPg 

# Za dano vozlišče poišče besedilo v znački na poti.
valueForPath <- function(record, path) {
  record %>% 
    html_node(xpath=path) %>% 
    html_text()
}

# Vrne vektor besedil v značkah na poteh iz vozlišč seznama (množice vozlišč)
valuesForPath <- function(records, path) {
  records %>% sapply(function(record) {valueForPath(record, path)})
}

# Vrne s separatorjem ločene nize v značkah, ki jih dobimo če gremo po vseh možnih poteh iz 
# XML vozlišča (značke) record. Nize združi v en niz, kjer so ločeni s separatorjem.
listValueForPath <- function(record, path, sep=";") {
  record %>% 
    html_nodes(xpath=sprintf("%s/text()", path)) %>% 
    as.character() %>% 
    str_replace_all(" *, *", sep) %>%
    str_replace_all("\\.$", "") %>%
    paste0(collapse = sep)
}

# Izvede zgornjo funkcijo na seznamu (množici) XML vozlišč (znački)
listValuesForPath <- function(records, path, sep = ";") {
  records %>% sapply( . %>% listValueForPath(path, sep))
}

# Prebere XML datoteko, izloči seznam značk "record" in iz njih pridobiva tekstovne stolpce 
# glede na različne poti iz značke record.
# Vrne tabelo
processXML <- function(fname) {
  records <- fname %>% 
    read_xml() %>% html_nodes(xpath="/records/record")
  tibble(
    source_x = records %>% valuesForPath("publisher"),
    title = records %>% valuesForPath("title"),
    doi = records %>% valuesForPath("doi"),
    license = "CC BY 4.0",
    abstract = records %>% valuesForPath("abstract"),
    publish_time = records %>% valuesForPath("publicationDate"),
    journal = records %>% valuesForPath("journalTitle"),
    url = records %>% valuesForPath("fullTextUrl"),
    volume = records %>% valuesForPath("volume"),
    issue = records %>% valuesForPath("issue"),
    start_page = records %>% valuesForPath("startPage"),
    end_page = records %>% valuesForPath("endPage"),
    authors = records %>% listValuesForPath("authors/author/name"),
    keywords = records %>% listValuesForPath("keywords/keyword")
  )
}

# Prebere obe datoteki v dve tabeli.
data1 <- processXML("data/amc-1-1--8-1.xml")
data2 <- processXML("data/amc-8-2--20-1.xml")

# Tabeli združimo in oštevilčimo vrstice (id članka)
data <- data1 %>% 
  bind_rows(data2) %>%
  mutate(
    id = row_number()
  ) 

# Shranjevanje združene tabele v CSV
data %>% 
  write_csv("data/amc.csv")

# read_csv("data/amc.csv") %>% View

# Shranjevanje v Excel datoteko. Pri tem stolpec z URL povezavami naredimo aktivne
data %>%
  (function(data) {
    class(data$url) <- "hyperlink"
    data
  }) %>%
  write.xlsx("data/amc.xlsx")

##################################
# Preoblikovanje v tidy data
##################################

# Čuden presledek
zw.space <- c(8203,32,8203) %>% intToUtf8()   # Med analizo se nam je pojavil čuden "presledek"

data.sep <- data %>%
  separate_rows("authors", sep=";") %>%    # Ločevanje stolpcev v vrstice
  separate_rows("keywords", sep=";") %>% 
  rename(
    author = authors,
    keyword = keywords
  ) %>% 
  drop_na(start_page) %>%
  filter(author != zw.space) %>%
  mutate(   # Prazni keywordi v NA
    keyword=ifelse(
      keyword == "",
      NA,
      keyword
    )
  ) 
  
# data.sep %>% View

###############################################################
### Normalizacija (pretvorba v tidy data) v zvezi z avtorji

# Izračun frekvence zastopanosti avtorjev (namen - pregled podatkov)
system.time({
  freq.author <- data.sep %>% 
    group_by(author) %>%
    summarise(cnt = n_distinct(id)) %>% 
    arrange(desc(cnt))
})

# freq.author %>% View

# Izračun frekvence zastopanosti avtorjev - še hitrejši način, primeren za delo 
# z večjimi podatkovji (hitrejši) - uporaba knjižnice data.table
data.sep.DT <- data.sep %>% as.data.table() 

system.time({
  # data.sep.DT[, .(cnt=length(unique(id))), by=author][order(-cnt)]
  freq.author <- data.sep.DT %>%
    .[, .(cnt=length(unique(id))), by=author] %>%
    .[order(-cnt)]
})
  
# freq.author %>% View

# avtorji na članku
author.paper <- data.sep %>%
  select(id, author) %>%
  distinct()

# enolični avtorji, ki jim dodelimo ključ "aid"
Author <- author.paper %>%
  select(author) %>%
  distinct() %>%
  arrange(author) %>%
  mutate(
    aid=row_number()
  )

# Tabela povezava avtorjev na članke preko "id" in "aid".
AuthorPaper <- author.paper %>%
  inner_join(Author, by="author") %>%
  select(id, aid)

###############################################################
### Normalizacija (pretvorba v tidy data) v zvezi z avtorji

# Pregled frekvenc ključnih besed in možnih anomalij
freq.keyword <- data.sep %>% 
  group_by(keyword) %>%
  summarise(cnt = n_distinct(id)) %>% 
  arrange(desc(cnt))

freq.keyword %>% View

# Povezava ključnih besed na članke
keyword.paper <- data.sep %>%
  select(id, keyword) %>%
  distinct()

# Enolične ključne besede z dodelitvijo ključa "kid"
Keyword <- keyword.paper %>%
  select(keyword) %>%
  distinct() %>%
  arrange(keyword) %>%
  mutate(
    kid=row_number()
  ) 

# Povezava ključne besede na članek preko "id" in "kid"
KeywordPaper <- keyword.paper %>%
  inner_join(Keyword, by="keyword") %>%
  select(id, kid)

# Tabela člankov, vsak določen z "id", brez stolpcev "authors" in "keywords".
Paper <- data %>% 
  select(-authors, -keywords)

# Shranjevanje tabel v tidy data obliki oz. v "normalizirani" obliki
Author %>% write_csv("data/author.csv")
Keyword %>% write_csv("data/keyword.csv")
Paper %>% write_csv("data/paper.csv")
AuthorPaper %>% write_csv("data/author-paper.csv")
KeywordPaper %>% write_csv("data/keyword-paper.csv")

# Tako lahko iz normaliziranih tabel sestavimo tabelo "data.sep", ki je primerna za razne analize
# z uporabo filtriranj in agregacij (group_by, summarise)
Paper %>%
  inner_join(AuthorPaper, by="id") %>%
  inner_join(Author, by="aid") %>% 
  inner_join(KeywordPaper, by="id") %>%
  inner_join(Keyword, by="kid")

#######################
### Analiza prispevkov avtorjev v reviji in medsebojnega sodelovanja preko avtorstev
### Uporaba metod analize omrežij

# Tabela uteži prispevkov enega avtorja za posamezen članek
author.paper.weight <- AuthorPaper %>%
  group_by(id) %>%
  summarise(weight=1/n()) %>% 
  inner_join(AuthorPaper, by="id") 

# Pregled
author.paper.weight %>%
  inner_join(Author, by="aid") %>% 
  group_by(aid, author) %>%
  summarise(total.contribution = sum(weight)) %>%
  arrange(desc(total.contribution)) %>% View


# Izračun povezav med avtorji. Če avtorja sodelujeta na kakem članku, sta povezana.
# Primer: če sta na nekem članku to 2 izmed 5 avtorjev, vsak od avtorjev pridobi utež 1/5.
# Če avtorja sodelujeta (sta soavtorja) na več člankih, se prispevki seštejejo v celotno utež na
# povezavi med avtorjema. 
author.author.links <- author.paper.weight %>% 
  inner_join(author.paper.weight, by="id") %>% # Ustvarjanje povezav preko skupnih člankov 
  mutate(                                      # t.i. "množenje omrežij"
    from=aid.x,
    to=aid.y,
    weight=weight.x
  ) %>%
  select(from, to, weight) %>%     # Izvleček povezav za sodelovanje na posameznem članku
  group_by(from, to) %>%      # združevanje vseh povezav pri sodelovanju
  summarise(n = sum(weight)) %>%   
  filter(from < to)   # ohranimo samo povezave v eno smer (tudi brez zank). 
     # zanke bi določale celoten prispevek avtorjev kot zgoraj

# Seznam avtorjev z njihovimi prispevki
author.all <- author.paper.weight %>%
  inner_join(Author, by="aid") %>% 
  group_by(aid, author) %>%
  summarise(total.contribution = sum(weight)) %>%
  arrange(desc(total.contribution)) 

# Dobimo omrežje
# VOZLIŠĆA: author.all, identifikator avtorja je "aid"
# POVEZAVE: author.author.links, imajo tudi utež "n", kot opisano

# Če je veliko točk in povezav so vizualizacije lahko nepregledne in/ali počasne
# Poglejmo si podomrežje na 100 najproduktivnejših avtorjih (glede na revijo)
author.top100 <- author.all %>% 
  head(100)

# "aid"-ji teh avtorjev
author.top100.ids <- author.top100 %>% pull(aid)

# Filtriranje povezav, ki so v podomrežju
author.author.links.top100 <- author.author.links %>%
  filter(from %in% author.top100.ids & to %in% author.top100.ids)

# Primer uporabe vizualizacije s knjižnico visNetwork
# nodes <- data.frame(id = 1:3)
# edges <- data.frame(from = c(1,2), to = c(1,3))
# visNetwork(nodes, edges, width = "100%")

author.author.links.top100 %>% View

# Vizualizacija omrežja sodelovanj na 100 najproduktivnejših avtorjih
visNetwork(
  author.top100 %>% mutate(
    id=aid,   # identifikator mora biti v stolpcu "id"
    label=sprintf("%s (%.2f)", author, total.contribution),  # labele vozlišč
    title=sprintf("%s (%.2f)", author, total.contribution),  # labele "na klik"
    value=total.contribution  # Velikost vozlišča
  ),
  author.author.links.top100 %>% mutate(
    width=n*10,   # Debelina povezave - pomnožimo jo glede na "občutek", da lepše izgleda
    # label=sprintf("%.2f", n), # Napis na povezavi (ga ne želimo)
    title=sprintf("%.2f", n)  # Napis na povezavi (ob kliku/oz. potovanju miške nad njo)
  ),
  width="100%",   # Velikost na spletni strani
  height="800px"   
) %>% visSave("authors.html") # Shranjevanje v HTML datoteko (stran), ki jo lahko odpremo v brskalniku

# Poskus izrisa celotnega omrežja.
# POZOR: datoteka se kreira hitro. Za pregledovanje v brskalniku pa je treba biti potrpežljiv,
# saj lahko začetni izris zaradi velikosti omrežja traja tudi preko 1 min
visNetwork(
  author.all %>% mutate(
    id=aid,
    label=sprintf("%s (%.2f)", author, total.contribution),
    title=sprintf("%s (%.2f)", author, total.contribution),
    value=total.contribution
  ),
  author.author.links %>% mutate(
    width=n*10,
    # label=sprintf("%.2f", n),
    title=sprintf("%.2f", n)
  ),
  width="100%",
  height="800px"
) %>% visSave("authors-all-SLOW.html")

# OPOMBA:
# Za hitrejše risanje bi bilo smiselno preizkusiti še knjižnico networkD3 ali morda narediti 
# izračun začetnega približka pozicioniranja vozlišč že v R. 