require(dplyr)
## Zaradi funkcije strapplyc
require(gsubfn)

## Dobesedni vzorec, identificiranje vzorca z grep
niz <- "avto"
vzorec <- "avto"
grep(vzorec, niz)

## Uporaba grep in grepl nad vektorji
nizi <- c("avto", "avtomobil", "avokado", "avtokado", "navto")
vzorec <- "avto"
grep(vzorec, nizi)
grepl(vzorec, nizi)

## Vzorec od začetka niza
grep("^avto", c("avto", "avtomobil", "avokado", "avtokado", "navto"))

## Vzorec z začetkom in koncem niza (celotni v vzorec). Vzorec '.' = kateri koli znak 
grep("^av..$", c("avto", "avtomobil", "avokado", "avtokado", "navto"))

## Dvojna poševnica (ubežni znak v vzorcu). Posebni vzorci

## Alfanumerični znak
grep("\\w", c(" ", "a", "1", "A", "%", "\t"))
## Ne-alfanumerični znak
grep("\\W", c(" ", "a", "1", "A", "%", "\t"))
## Beli znak
grep("\\s", c(" ", "a", "1", "A", "%", "\t"))
## Ne-beli znak
grep("\\S", c(" ", "a", "1", "A", "%", "\t"))
## Števka
grep("\\d", c(" ", "a", "1", "A", "%", "\t"))
## Znak različen od števke
grep("\\D", c(" ", "a", "1", "A", "%", "\t"))

## Možnost
grep("^[abc]\\w\\w", c("avto", "bus", "ne", "vozi"))

## Tričrkovni niz iz malih črk ang. abecede
grep("^[a-z][a-z][a-z]$", c("Čas", "teče", "nič", "ne", "reče:", "tik", "tak"))

## Tričrkovni niz, z nekaterimi šumniki
grep("^[a-zA-ZčšžČŠŽ][a-zčšž][a-zčšž]$", c("Čas", "teče", "nič", "ne", "reče:", "tik", "tak"))

## Alternativni vzorci (eden ali drugi) - (X|Y)
## Podizraz v oklepajih = skupina (oz. podvzorec)
grep("^((\\d)|([1-9]\\d))$", c("1", "20", "0", "nič", "to je 100%", "09"))

## Nič ali več ponovitev predhodnega vzorca (znaka ali skupine)
grep("^[a-z]*$", c("slika", "je", "vredna", "1000", "besed."))

## Ena ali več ponovitev predhodnega vzorca (skupine)
grep("^[a-z]+$", c("", "slika", "je", "vredna", "1000", "besed."))

## Nizi iz besed iz malih ang. črk ločeni z enim presledkom 
grep("^([a-z]+ )*[a-z]+$", c("besede", "ali pa stavki", "123 ni"))

## 3 do 5 ponovitev predhodnega vzorca (skupine)
grep("^[a-z]{3,5}$", c("ta", "beseda", "nima", "pomena"))

## Predznačena cela števila
grep("^[+-]?(0|[1-9][0-9]*)$", c("0", "+1", "01", "-99"))

## Podvzorci: komponente vektorja
nizi <- c("(1,2)", "( -2, 7)", "(    -3   ,     45)", "(a, 3)")
vzorec <- "\\(\\s*([+-]?(0|[1-9][0-9]*))\\s*,\\s*([+-]?(0|[1-9][0-9]*))\\s*\\)"
## "Replace" na "ujetih" podvzorcih
komp1 <- sub(vzorec, "\\1", nizi)
## Logični indeks kjer vzorec "ne prime"
lidx <- !grepl(vzorec, nizi)
komp1[lidx] <- NA

komp2 <- sub(vzorec, "\\3", nizi)
komp2[lidx] <- NA
as.integer(komp1)
as.integer(komp2)

## strapplyc - iskanje podvzorcev
skupine <- strapplyc(nizi, vzorec)

## Pomožna funkcija: vrne i-ti niz v zaporedju ali prazen niz, če tega ni
indeks <- function(x, i) {if(length(x) >= i) x[[i]] else ""}

## sapply sestavi vektor po ekstrakciji i-tega elementa v seznamih "ujetih" podvzorcev
x <- sapply(skupine, . %>% indeks(1)) %>% as.integer
y <- sapply(skupine, . %>% indeks(3)) %>% as.integer

## Separacija večkratnih vrednosti v stolpcu: Višina, Širina in Dolžina
cudenStolpec <- c("12, 3; 8", "6,1,2") 
separator <- "[\\,\\;]"
spl <- strsplit(cudenStolpec, separator)
sapply(spl, . %>% indeks(3))
unlist(spl)
as.integer(unlist(spl))
mat <- matrix(as.integer(unlist(spl)), ncol=3, byrow=TRUE)
print(mat)
df <- data.frame(mat)
print(df)
colnames(df) <- c("Dolžina", "Širina", "Višina")
print(df)

## Izračun in dodajanje novega stolpca
df %>% mutate(Volumen=Višina*Širina*Dolžina)



