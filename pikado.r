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

####################################################################################################################
####################################################################################################################

# OPTIMALNA STRATEGIJA

# optimalna strategija je razdeljena na dva dela 
# 1) maksimiziranje števila zadetih tock
# 2) minimiziranje števila rund do konca
# 
# POJASNILO:
# na zacetku želimo maksimizirati stevilo zadetih tock in čim bolj nizati število 501
# ko se blizamo koncu igre, pa maksimiziranje tock ni vec optimalno, saj moramo koncati tocno na 0
# ce bi se naprej maksimizirali tocke, bi zasli na negativno stran, 
# to pa v igri pomeni povratek tock kot na zacetku "runde"
# imamo se dodatno omejitev, da je potrebno koncati z zadetkom v inner bull ali v  pas z dvojnimi tockami
# zato bomo v drugem delu igre optimalno strategijo implementirali z minimiziranjem števila rund do konca igre

# Implementacija 1: maksimiziranje stevila tock
# glej datoteko strategija1.r

# razdelimo tablo na mrezo tock
mreza.ciljnih.tock <- function(N=170){
  tockeX <- vector()
  tockeY <- vector()
  delta <- 170/N
  for (m in -N:N){
    for (n in -N:N){
      if (n^2 + m^2 <= N^2){
        x <- n * delta
        y <- m * delta
        tockeX <- c(tockeX, x)
        tockeY <- c(tockeY, y)
      }
    }
  }
  drawBoard(new = TRUE)
  points(tockeX, tockeY, col="red", pch=18)
  tocke <- cbind(tockeX, tockeY)
  return(tocke)
}

# test
tocke <- mreza.ciljnih.tock(25)

# v vsaki tocki izracunamo pričakovano vrednost števila dobljenih točk
# točka z najvišjo pričakovano vrednostjo je optimalna cilja tocka 
# za izračun pricakovane vrednosti uporabimo monte carlo integracijo

povprecni.rezultat <- function(n, tarcaX, tarcaY, stdX, stdY) {
  # funckija sprejme parameter n, ki pove število simulacij meta
  # zeljeni x in y koordinati puscice in standardna odklona
  # vrne povprečno število točk za met v ciljano tocko
  meti <- as.data.frame(met(tarcaX, tarcaY, stdX, stdY, n))
  names(meti) <- c("koordinataX", "koordinataY")
  rezultati <- ddply(meti,c("koordinataX", "koordinataY"), function(df) rezultat.meta(df$koordinataX, df$koordinataY))
  names(rezultati) <- c("koordinataX", "koordinataY", "tocke")
  return(mean(rezultati$tocke))
}

# test
# povprecni.rezultat(100,0,0,5,5)

# Monte Carlo

MonteCarlo <- function(tocke, stdX, stdY, k = 10000){
  # funkcija sprejme tocke in standardne odklone,
  # požene Monte Carlo metodo v vsaki točki
  # in vrne pricakovane vrednosti rezultata pri ciljanju v določeno točko
  tocke <- as.data.frame(tocke)
  names(tocke) <- c("tarcaX", "tarcaY")
  pricakovana.vrednost <- ddply(tocke, c("tarcaX", "tarcaY"), 
                               function(df) povprecni.rezultat(k, df$tarcaX, df$tarcaY, stdX, stdY))
  names(pricakovana.vrednost) <- c("tarcaX", "tarcaY", "rezultat")
  return(pricakovana.vrednost)
}
  
# test
MCpricakovana.vrednost.profesionalec <- MonteCarlo(tocke, 5, 5, k = 100)
MCpricakovana.vrednost.rekreativec<- MonteCarlo(tocke, 20, 20, k = 100)
MCpricakovana.vrednost.zacetnik <- MonteCarlo(tocke, 40, 40, k = 100)

# Heatmap 
barvna.tabla <- function(tabela.pricakovanih.vrednosti){
  # Gradient color
  pal <- wes_palette("Darjeeling1", 100, type = "continuous")
  ggplot(tabela.pricakovanih.vrednosti, aes(tarcaX, tarcaY, fill= rezultat)) + 
    geom_tile() +
    scale_fill_gradientn(colours = rainbow(4)) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    coord_equal()
}

nivo.igralca <- c("Profesionalec", "Rekreativec", "Začetnik")
profesionalec <- barvna.tabla(MCpricakovana.vrednost.profesionalec)
rekreativec <- barvna.tabla(MCpricakovana.vrednost.rekreativec)
zacetnik <- barvna.tabla(MCpricakovana.vrednost.zacetnik)

optimalna.ciljna.tocka <- function(stdX, stdY, k = 100, N=25){
  # sprejme standardne odklone in vrne točko, ki ima najvišjo pričakovano vrednost glede na Monte Carlo
  # torej vrne optimalno ciljno tocko
  tocke <- mreza.ciljnih.tock(N)
  pricakovana.vrednost <- MonteCarlo(tocke, stdX, stdY, k)
  urejena.pricakovana.vrednost <- pricakovana.vrednost[order(pricakovana.vrednost$rezultat, decreasing = TRUE), ]
  ciljna.tocka <- urejena.pricakovana.vrednost[1,1:2]
  return(ciljna.tocka)
}

# test
cilj <- optimalna.ciljna.tocka(5,5,100,25)

optimalni.rezultat <- function(ciljna.tocka){
  # funkcija sprejme ciljo tocko(kartezicni kooridnati x in y) in vrne ciljni rezultat
  rezultat <- rezultat.meta(ciljna.tocka[1], ciljna.tocka[2])
  return(rezultat)
}

# test
rezultat <- optimalni.rezultat(cilj)


####################################################################################################################
####################################################################################################################

# Implementacija 2: minimiziranje stevila rund do konca igre

vektor.razdalj <- function(){
  # funckija vrne razdalje na katerih želimo točke
  vektor <- c(0, 11.25, 16,24,32,40,48,56,64,72,80,88,96,103,108,116,124,132,140,148,156,166,175,185,195,5000)
  return(vektor)
}

vektor.kotov <- function(stevilo.zarkov.polje){
  # funckija sprejme stevilo zarkov v enem polju
  # in vrne vektor kotov
  stevilo.zarkov = stevilo.zarkov.polje*20
  vektor <- c()
  for (i in 1:60){
    kot.stopinje = i*360/60
    kot.radiani = 2*pi*kot.stopinje/360
    vektor = c(vektor,kot.radiani)
  }
  return(vektor)
}

razdalje <- vektor.razdalj()
koti <- vektor.kotov(3)

# funckija, ki definira točke na tabli
tarce.na.tabli <- function(razdalje, koti){
  # funckija sprejme vektor razdalj in kotov,
  # za vsak kot na vsaki razdalji nariše točko na tabli in vrne tabelo točk
  drawBoard(new = TRUE)
  koorX <- vector()
  koorY <- vector()
  for (kot in koti){
    # pretvorba v kartezične koordinate
    x = razdalje*cos(kot)
    y = razdalje* sin(kot)
    points(x, y, col="red", pch=18)
    koorX <- c(koorX, x)
    koorY <- c(koorY, y)
  }
  tarce <- cbind(koorX, koorY)
  return(tarce)
}

tarce <- tarce.na.tabli(razdalje, koti)

vektor.moznih.rezultatov.meta <- function(){
  # funckija vrne vse možne rezultate ciljanja v tarco
  vektor <- vector()
  for (i in 1:20){
    vektor <- c(vektor, i, i*2, i*3)
  }
  vektor <- c(vektor, 25, 50)
  return(vektor)
}
mozni.rezultati <- vektor.moznih.rezultatov.meta()

prehodne.verjetnosti <- function(tarce, mozni.rezultati, stevilo.simulacij, stdX, stdY){
  # funckija sprejme tabelo ciljanih tocke(tarce) in vektor moznih rezultatov
  # za vsako tarco izracuna prehodne verjetnosti:
  # verjetnost, da z metom puscice dobimo rezultat r, če ciljamo v tarco p
  # za vsako tarco aproksimiramo verjetnosti z uporabo metode Monte Carlo
  # parameter stevilo.simulacij pove, kolikokrat simuliramo met v dano tarco
  st.tarc <- length(tarce[,1])
  st.rezultatov <- length(mozni.rezultati)
  tarcaX <- tarce[,1]
  tarcaY <- tarce[,2]
  stevilo.puscic.p = stevilo.simulacij
  prehodne.verjetnosti <- as.data.frame(matrix(1,ncol = st.rezultatov , nrow = st.tarc))
  for (p in 1:st.tarc){
    meti <- as.data.frame(met(tarcaX[p], tarcaY[p], stdX, stdY, stevilo.simulacij))
    names(meti) <- c("koordinataX", "koordinataY")
    rezultati <- ddply(meti,c("koordinataX", "koordinataY"), function(df) rezultat.meta(df$koordinataX, df$koordinataY))
    names(rezultati) <- c("koordinataX", "koordinataY", "tocke")
    for (r in 1: st.rezultatov){
      stevilo.zadetkov.r = sum(rezultati$tocke == mozni.rezultati[r])
      prehodna.verjetnost.p.r = stevilo.zadetkov.r/stevilo.puscic.p
      prehodne.verjetnosti[p,r] = prehodna.verjetnost.p.r
    }
    colnames(prehodne.verjetnosti) <- paste(mozni.rezultati)
    rownames(prehodne.verjetnosti) <- paste(tarce)
  }
  return(prehodne.verjetnosti)
}

prehodne.verjetnosti <- prehodne.verjetnosti(tarce, mozni.rezultati, 5, 5,5)

# TO DO: prehodne verjetnosti stanj (mozna stanja ???)
# TO DO: zacetne vrednosti za algoritem
# TO DO: iterativni algoritem
# TO DO: minimum = optimalna tocka

# TO DO: izracun vrednosti za 3 nivoje igralca: amater, rekreativec, profesionalec
# TO DO: simualacija igre za vsak nivo
# TO DO: igra proti računalniku?

