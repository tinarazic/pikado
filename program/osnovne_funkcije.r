####################################################################################################################
# Osnovne funckije
####################################################################################################################

# postavimo seme, da bodo rezultati enaki
set.seed(123)

# funkcija, ki nariše tablo
drawBoard(new = TRUE)

# funckija za met puščice
met <- function(tarcaX, tarcaY, stdX, stdY, n = 1){
  # funckija sprejme zeljeni x in y kooridinati meta puscice
  # in standardni odklon meta v smeri x in v smeri y
  # vrne x in y koordinati meta
  # predpostavimo, da je met puscice porazdeljen normalno
  # poljubni parameter n določi število simulacij meta
  
  povprecje <- c(tarcaX, tarcaY)
  # kovariancna matrika
  matrika.sigma <- matrix(c(stdX^2, 0, 0, stdY^2),2)
  
  rezultatXY <- mvrnorm(n, mu = povprecje, Sigma = matrika.sigma) 
  
  return(rezultatXY)
}

# funckija, ki vrne tocke pri metu puscice
rezultat.meta <- function(koordinataX, koordinataY){
  # funckija, ki sprejme koordinati zadetka in 
  # vrne število točk
  kot.polja <- 2*pi/20 # 20 razdelkov na tabli, kot v stopinjah
  tocke <- c(20,  1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
  koti <- seq(0, 2 * pi - kot.polja, by = kot.polja)
  koti <- koti -  0.5 * pi / 10
  # kote zamaknemo za 9 stopinj, saj gledamo kot, da gre os y cez sredino polja 20 in 3
  # os x pa čez sredino polja 11 in 6, tako je koordinatno izhodišče v središču table. 
  
  # na kateri nivojnici je puscica oziroma razdalja od sredica table
  razdalja.od.sredisca <- (koordinataX^2 + koordinataY^2)^.5
  
  kot.meta <- if (razdalja.od.sredisca == 0) {0} else { (0.5 * pi) - asin(koordinataY/razdalja.od.sredisca)}
  kot.meta <- if (koordinataX < 0 ) { pi + (pi - kot.meta) } else { kot.meta }
  
  # imamo velikost kota meta, potrebno je dolociti se v katerem polju je pristala puscica
  # s spremenljivko izbira.kota ugotovimo v katerem polju(krožnem izseku) je
  izbira.kota <- NA
  for (i in 1:19){
    if ((koti[i] < kot.meta) & (koti[i+1] >= kot.meta)) {izbira.kota <- i} 
  } 
  if (is.na(izbira.kota)==TRUE) {izbira.kota <- 20}
  
  # potrebno je še definirati na katerem pasu je pristala pucica
  # 1 = inner bull, 2 = outer bull, 3 = enojne tocke, 4 = trojne tocke, 5 = enojne tocke, 6 = dvojne tocke, 7 = nic tock
  pas <- if (razdalja.od.sredisca <= 12.7/2) {1
  } else if (razdalja.od.sredisca >= 12.7/2 & razdalja.od.sredisca < 31.8/2)  {2
  } else if (razdalja.od.sredisca >= 31.8/2 & razdalja.od.sredisca < 99)   {3
  } else if (razdalja.od.sredisca >= 99     & razdalja.od.sredisca < 107)  {4
  } else if (razdalja.od.sredisca >= 107    & razdalja.od.sredisca < 162)  {5
  } else if (razdalja.od.sredisca >= 162    & razdalja.od.sredisca < 170)  {6 
  } else {7}
  
  pas.tocke <- c(50, 25, 1, 3, 1, 2, 0)
  tocke.meta <- if (pas <= 2) { pas.tocke[pas] } else { tocke[izbira.kota] * pas.tocke[pas] }    
  return(tocke.meta)
}

# funckija, ki vrne tocke pri metu puscice
rezultat.meta2 <- function(koordinataX, koordinataY){
  # funckija, ki sprejme vektor koordinat zadetka in vrne polje
  # vrne character, kjer je vključen tudi pas
  kot.polja <- 2*pi/20 # 20 razdelkov na tabli, kot v stopinjah
  tocke <- c(20,  1, 18, 4, 13, 6, 10, 15, 2, 17, 3, 19, 7, 16, 8, 11, 14, 9, 12, 5)
  koti <- seq(0, 2 * pi - kot.polja, by = kot.polja)
  koti <- koti -  0.5 * pi / 10
  # kote zamaknemo za 9 stopinj, saj gledamo kot, da gre os y cez sredino polja 20 in 3
  # os x pa čez sredino polja 11 in 6, tako je koordinatno izhodišče v središču table. 
  
  # na kateri nivojnici je puscica oziroma razdalja od sredica table
  razdalja.od.sredisca <- (koordinataX^2 + koordinataY^2)^.5
  
  kot.meta <- ifelse(razdalja.od.sredisca == 0,0, (0.5 * pi) - asin(koordinataY/razdalja.od.sredisca))
  kot.meta <- ifelse(koordinataX < 0, pi + (pi - kot.meta), kot.meta)
  
  # imamo velikost kota meta, potrebno je dolociti se v katerem polju je pristala puscica
  # s spremenljivko izbira.kota ugotovimo v katerem polju(krožnem izseku) je
  stevilo.koordinat <- length(razdalja.od.sredisca)
  izbira.kota <- rep(NA, stevilo.koordinat)
  for (j in 1:stevilo.koordinat){
    for (i in 1:19){
      izbira.kota[j] <- if ((koti[i] < kot.meta[j]) & (koti[i+1] >= kot.meta[j])) {i} else {izbira.kota[j]}
    }
  } 
  izbira.kota <- ifelse(is.na(izbira.kota)==TRUE, 20, izbira.kota)
  
  # potrebno je še definirati na katerem pasu je pristala pucica
  # 1 = inner bull, 2 = outer bull, 3 = enojne tocke, 4 = trojne tocke, 5 = enojne tocke, 6 = dvojne tocke, 7 = nic tock
  pas <- ifelse(razdalja.od.sredisca <= 12.7/2, 1,
                ifelse(razdalja.od.sredisca >= 12.7/2 & razdalja.od.sredisca < 31.8/2, 2,
                       ifelse(razdalja.od.sredisca >= 31.8/2 & razdalja.od.sredisca < 99,3,
                              ifelse(razdalja.od.sredisca >= 99  & razdalja.od.sredisca < 107, 4,
                                     ifelse(razdalja.od.sredisca >= 107 & razdalja.od.sredisca < 162,5,
                                            ifelse(razdalja.od.sredisca >= 162 & razdalja.od.sredisca < 170,6,
                                                   7))))))
  
  pas.tocke <- c(50, 25, 1, 3, 1, 2, 0)
  tocke.meta <- rep(0, length(pas))
  for (i in 1:length(pas)){
    if (pas[i] <=2){
      tocke.meta[i] <- sprintf("%s-BULL",pas.tocke[pas[i]])
    }
    else {
      tocke.meta[i] <- sprintf("%s-%s",tocke[izbira.kota[i]], pas.tocke[pas[i]])
    }
  }
  return(tocke.meta)
}

# test
# rezultat.meta2(c(20,10, 0,-30,-30), c(-11,20,0,-10,10))
# rezultat.meta(20, -11)
# rezultat.meta(10, 20)
# rezultat.meta(0, 0)
# rezultat.meta(-30, -10)
# rezultat.meta(-30, 10)

