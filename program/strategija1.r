####################################################################################################################
# Optimalna strategija 1: maksimiziranje stevila tock pri metu puscice
####################################################################################################################

# poklicemo osnovne funckije
source("lib/libraries.r")
source("program/osnovne_funkcije.r")
source("program/strategija1_rezultati.r")

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
                                function(df) {incProgress(1/(length(tocke[,1])));
                                povprecni.rezultat(k, df$tarcaX, df$tarcaY, stdX, stdY)
                                })
  names(pricakovana.vrednost) <- c("tarcaX", "tarcaY", "rezultat")
  return(pricakovana.vrednost)
}

# test
#MCpricakovana.vrednost.profesionalec <- MonteCarlo(tocke, 5, 5, k = 100)
#MCpricakovana.vrednost.rekreativec<- MonteCarlo(tocke, 20, 20, k = 100)
#MCpricakovana.vrednost.zacetnik <- MonteCarlo(tocke, 40, 40, k = 100)
#dump(c("MCpricakovana.vrednost.profesionalec", "MCpricakovana.vrednost.rekreativec",
#       "MCpricakovana.vrednost.zacetnik"), file = "program/strategija1_rezultati.r")

# Heatmap 
barvna.tabla <- function(tabela.pricakovanih.vrednosti){
  # Gradient color
  pal <- wes_palette("Darjeeling1", 100, type = "continuous")
  ggplot(tabela.pricakovanih.vrednosti, aes(tarcaX, tarcaY, fill= rezultat)) + 
    geom_tile() +
    scale_fill_gradientn(colours = rainbow(4)) + 
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) + 
    coord_equal() +
    theme_bw()
}

nivo.igralca <- c("Profesionalec", "Rekreativec", "Začetnik")
odklon.zacetnik <- "Odklon puscice zacetnika: 40 mm" 
odklon.rekreativec <- "Odklon puscice rekreativec: 20 mm" 
odklon.profesionalec <- "Odklon puscice profesionalec: 5 mm"
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
#cilj <- optimalna.ciljna.tocka(5,5,100,25)

optimalni.rezultat <- function(ciljna.tocka){
  # funkcija sprejme ciljo tocko(kartezicni kooridnati x in y) in vrne ciljni rezultat
  rezultat <- rezultat.meta(ciljna.tocka[1], ciljna.tocka[2])
  return(rezultat)
}

# test
#rezultat <- optimalni.rezultat(cilj)
