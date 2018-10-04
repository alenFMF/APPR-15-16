########################################
# UVOD V R
# avtor: Alen Orbanić
########################################

# Ukaze v tej datoteki lahko izvajamo v RStudiu tako, da se postavimo na ustrezno vrstico
# in pritisnemo 'Ctrl + Enter' (oz. Cmd + Enter na Mac OS).
# Lahko pa tudi zaporedoma pritiskamo to kombinacijo in tako izvajamo vrstice eno za drugo
# pri čemer se komentarji avtomatično preskočijo

## SPREMENLJIVKE IN PRIREJANJE

# '<-' prireditveni operator, lahko uporabimo tudi '='
a <- 1     

# V spremenljivkah hranimo objekte različnega tipa.
# Izpis tipa objekta
class(a)   

# Eksplicitni vnos celih števil
b = 1L
class(b)

# Niz navedemo v dvojnih navednicah
niz <- "To je niz"  

# Avtomatični izpis vsebine
niz     

TRUE -> c     # Prirejamo lahko tudi v drugo stran
class(c)

d <- x <- 2 + 3i   # Prirejamo lahko tudi zaporedoma
class(x)
d

rm(d)   # Brisanje spremenljivk
d

2*(a <- 1.23)   # Prireditev v oklepajih vrne rezultat

A             # R loči velike in male črke 

## VEKTORJI
10

1:20  # generiranje vektorja števil 1 do 20
(v <- 1:20)   # prireditev z izpisom

5:2   # generiranje vektorja števil v padajočem vrstnem redu

c(10, 12, 5)   # konstrukcija vektorja iz števil

v <- c(2, 3, 4)  # združevanje vektorjev
c(1, v, 5)

# TIPI IN PRETVARJANJE
x <- 1:5
class(x)
is.numeric(x)  # ali je 'x' tipa 'numeric'?is.character(x)
is.character(x) # ali je 'x' tipa 'character'?

#POZOR: pika je sestavni del imena funkcije (ni operator kot pri Pyhtonu)

as.logical(x)    # pretvorba v vektor logičnih vrednosti
as.character(x)   # pretvorba v vektor nizov

c(1.3, "a")  # implicitna pretvorba v vektor nizov
c(TRUE, 5)   # implicitna pretvorba v vektor števil
c("a", "TRUE")    # implicitna pretvorba v vektor nizov

# Nesmiselne pretvorbe vrnejo ```NA``` (oznaka za manjkajočo vrednost)
as.numeric(c("a", "b"))   
as.logical(c("a","b"))

## Ustvarjanje vektorjev
vector("numeric", length=10)
integer(10)
character(5)

seq(0.75, 1.4, 0.1)  # vektor z vrednostmi od 'n' do 'm' s korakom 'k'
rep(c(1, 2, 3), 3)   # vektor s 'k' ponovitvami vektorja 'v'

## Logične vrednosti

TRUE
FALSE
NA      # prazna vrednost je tudi logična vrednost

!TRUE
FALSE || TRUE
NA | TRUE     # kar koli bi bil prvi argument, je vrednost izraza 'TRUE'
TRUE & NA     # ker ne poznamo vrednosti drugega argumenta, je rezultat neznan ('NA')

1 != 2   # ali je 1 različno od 2
10 > 5.5  # ali je 10 strogo večje od 5.5

is.integer(1)     # ali je 1 celo število?
is.finite(20)     # ali je 20 končna vrednost
is.finite(Inf)    # 'Inf' predstavlja pozitivno neskončnost

1/0      # deljenje 1 z 0 vrne neskončnost
10/Inf   # deljenje z 0 vrne 0

Inf-Inf   # rezultat ni definiran (ni število)
sqrt(-1)
0/0

## Operacije na vektorjih
c(1,2) + c(3,4)   # seštevanje po komponentah
1:6 + c(1,2)  # drugi vektor je prekratek -> krožno dopolnjevanje
1:10 %% 3     # operator 'ostanek', krožno dopolnjevenje enega elementa 
1:5 * c(2, 3)  # neuspešno krožno dopolnjevanje

v <- c("a", "b", "c", "d", "e")
v[[3]]    # element na mestu 3
# PAZI: indeksi se v R štejejo od 1 dalje (ne od 0)!
  
## Operacije na vektorjih
  
#Podzaporedja. Operator ```[]```
v[c(3,4)]   # podzaporedje iz 3. in 4. člena

# Negativni indeksi povejo, katere člene odstranimo
v[c(-1,-5)]   # odstrani 1. in 5. člena

# Logični indeksi (logične maske - TRUE - ostane, FALSE - odpade)
v[c(TRUE, FALSE)]   # lihi elementi (krožno dopolnjevanje, ki se lahko "ne izide")


## Atributi 
v <- 1:6
attr(v, "nekaj") <- c(1,2)   # prirejanje atributa 'nekaj'
attr(v, "drugi") <- "Drugi"
v

attributes(v)   # seznam atributov
attributes(v)[[2]]   # drugi atribut iz seznama

# Atribut 'names' - poimenovanje komponent
v <- c(a=1, b=2, 3)  # poimenovanje komponent vektorja
v    
attributes(v)

# Sklic na komponento preko imena
v[["a"]]  # element poimenovan 'a'
v[c("a", "b")]   # podzaporedje dobljeno preko imen komponent

## Faktorji - vektorji z omejenim naborom vrednosti
# kategorične spremenljivke (npr. spol M/Ž, krvna skupina, ...)

v <- c("a", "b", "b", "a")
f <- factor(v)   # ustvarjanje faktorja iz nabora komponent vektorja
class(f)

# interna reprezentacija faktorjev
capture.output(dput(factor(v)))

# konstrukcija s pomočjo interne reprezentacije
f <- structure(c(1L, 2L, 2L, 1L), .Label = c("a", "b"), class = "factor")
attributes(f)
class(f)

# Matrike
# Matrika je vektor z dodatnim atributom ```dim```
a <- matrix(1:6, ncol = 3, nrow = 2)  # konstruktor matrike
attributes(a)
class(a)

v <- 1:6
class(v)
attr(v, "dim") <- c(2,3)  # pretvorba vektorja v matriko dimenzije 2 x 3
v
class(v)

a <- structure(1:6, dim=c(2,3))   # "surovo" sestavljanje matrike
class(a)

# Večdimenzionalna tabela
b <- array(1:12, c(2, 3, 2))      # večdimenzionalna tabela dimenzij 2x3x2
b
class(b)

dim(b) <- c(2, 6)  # prerazporeditev 2x3x2 dim. tabele v 2x6 dim. matriko
b
class(b)

# Razne dimenzije
length(b)    # dolžina vektorja
nrow(b)      # število vrstic
ncol(b)      # število stolpcev
dim(b)       # vektor dimenzij

## Poimenovanja vrstic in stolpcev {.smaller}

rownames(b) <- c(1,2)
b
colnames(b) <- c("prvi", "drugi", "tretji", "četrti", "peti", "šesti")
b


b[2,3] # Dostop do elementov na indeksih

## Seznami 

list("a", 1)
seznam <- list(prvi="a", drugi=1)

seznam[[1]]
seznam$drugi

## Razpredelnice

placila <- data.frame(Ime=c("Janez", "Anja", "Francelj"), Izplacilo=c(100, 120, 90))
placila
row.names(placila) <- c("prvo", "drugo", "tretje")
placila

## Sestavljeni izrazi

{a <- 1; b <- 2 + a; a + b}

## Pogojni stavek 'if'

a <- 1
if (a > 0) print("večji") 
if (a == 5) print("Je pet.") else print("Ni pet") 

a <- 1
if (a < 3) {a <- a + 1; a <- a + 1; print("Morda pa je zdaj čez tri ...")} 
if (a < 3) {
  a <- a + 1; 
  a <- a + 1;   
  print("Morda pa je zdaj čez tri ...")
} 

## Zanki 'while' in 'repeat'

i <- 0
while (i < 3) {print(i); i <- i + 1}

i <- 0
repeat {print(i); i <- i + 1; if (i > 2) break}

## Funkcije

f <- function(a) {a*a}
f(3)
# Primer rekurzije
gcd <- function(a, b) {if(b == 0) abs(a) else gcd(b, a %% b)}
gcd(12, 15)

gcd <- function(a, b) { if(b == 0) return(abs(a)) else return(gcd(b, a %% b))}
gcd(12, 15)

# Definicije lastnih operatorjev
"%m%" <- function(a,b) min(a,b)
3 %m% 4

# Nastavljanje delovnega področja (```Session -> Set working directory```) 
setcwd("/Users/alen")
getwd()
