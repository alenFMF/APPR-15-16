require(dplyr)
require(ggplot2)

N <- 100000

## Generiranje tabele števil

# Posamezno
t1 <- system.time({
  nakljucna <- sapply(1:N, function(x) sample(1:10, 1))  
})
print(t1)

# Z vgrajeno funkcijo
t2 <- system.time({
  nakljucna <- runif(N, 1, 11) %>% floor
})
print(t2)

names(t2) 
class(t2)
typeof(t2)
length(t2)
t2["elapsed"]

# Z vgrajeno funkcijo, brez vektorizacije funkcije
t3 <- system.time({
  nakljucna <- runif(N, 1, N) %>% sapply(floor)
})
print(t3)

N <- 10000000
nakljucna <- runif(N, 1, N) %>% floor

# O(n) - g(n) = 30n
najdi <- function(x, k, seznam) {
  for(i in 1:k) {
    m <- seznam[[i]]
    if(m == x) return(TRUE)
  }
  return(FALSE)
}

velikost <- c()
cas <- c()
delez <- 50
for(i in seq(N/delez,N,N/delez)) {
  t <- system.time(najdi(25, i, nakljucna))  
  print(t)
  velikost <- c(velikost, i)
  cas <- c(cas, t["elapsed"])
}
meritve <- data.frame(velikost=velikost, cas <- cas)
meritve %>% ggplot(aes(x=velikost, y=cas)) + 
  geom_point() + geom_smooth(method=lm, formula=y~x)

######################
## Urejanje seznama
######################

system.time({
  sort(nakljucna)
})

izmeri <- function(nakljucna, delez, f) {
  N <- length(nakljucna)
  velikost <- c()
  cas <- c()
  for(i in seq(N/delez,N,N/delez)) {
    seznam <- nakljucna[1:i]
    t <- system.time(f(seznam))  
    print(t)
    velikost <- c(velikost, i)
    cas <- c(cas, t["user.self"])
  }
  meritve <- data.frame(velikost=velikost, cas <- cas)
  meritve %>% ggplot(aes(x=velikost, y=cas)) + 
    geom_point() + geom_smooth(method=lm, formula=y~x)
}

# O(n log(n))
izmeri(nakljucna, 20, sort)

#################################################
## Urejanje z izbiranjem
#################################################

# k1*(n(n+1)/2) + k2*n ... C*n^2 .. O(n^2)
urediIzbiraj <- function(seznam) {
  n <- length(seznam)
  for(i in 1:n) {
    mnInd <- i
    mn <- seznam[[i]]
    for(j in i:n) {
      el <- seznam[[j]]
      if(el < mn) {
        mn <- el
        mnInd <- j
      }
    }
    tmp <- seznam[[i]]
    seznam[[i]] <- mn
    seznam[[mnInd]] <- tmp
  }
  seznam
}



seznam <- runif(10, 1, 20) %>% floor
print(seznam)
urediIzbiraj(seznam)

N <- 10000
nakljucna <- runif(N, 1, N) %>% floor

izmeri(nakljucna, 20, urediIzbiraj)

## Za uporabo paketa Rcpp je morda potrebno namestiti kake prevajalnike
## za programski jezik C/C++. Poglej dokumentacijo o paketu.
## https://teuder.github.io/rcpp4everyone_en/020_install.html

library(Rcpp)

cppFunction('IntegerVector urediIzbirajC(IntegerVector seznam) {
  int n = seznam.size();
  IntegerVector novSeznam(n);
  for(int i = 0; i < n; ++i) {
    novSeznam[i] = seznam[i];
  }
  for(int i = 0; i < n; ++i) {
    int mnInd = i;
    int mn = novSeznam[i];
    for(int j = i; j < n; j++) {
      int el = novSeznam[j];
      if(el < mn) {
        mn = el;
        mnInd = j;
      }
    }
    int tmp = novSeznam[i];
    novSeznam[i] = mn;
    novSeznam[mnInd] = tmp; 
  }
  return novSeznam;
}')

seznam <- runif(10, 1, 20) %>% floor
print(seznam)
urediIzbiraj(seznam)
urediIzbirajC(seznam)

N <- 10000
nakljucna <- runif(N, 1, N) %>% floor

system.time({
  urediIzbiraj(nakljucna)  
})

system.time({
  urediIzbirajC(nakljucna)  
})

N <- 100000
nakljucna <- runif(N, 1, N) %>% floor

izmeri(nakljucna, 20, urediIzbirajC)
izmeri(nakljucna, 20, sort)
