####################################################################################################################
# Optimalna strategija 2: minimziranje stevila rund do konca igre
####################################################################################################################

# poklicemo osnovne funckije
source("lib/libraries.r")
source("program/osnovne_funckije.r")

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
    colnames(meti) <- c("koordinataX", "koordinataY")
    rezultati <- ddply(meti,c("koordinataX", "koordinataY"), function(df) rezultat.meta(df$koordinataX, df$koordinataY))
    colnames(rezultati) <- c("koordinataX", "koordinataY", "tocke")
    for (r in 1: st.rezultatov){
      stevilo.zadetkov.r = sum(rezultati$tocke == mozni.rezultati[r])
      prehodna.verjetnost.p.r = stevilo.zadetkov.r/stevilo.puscic.p
      prehodne.verjetnosti[p,r] = prehodna.verjetnost.p.r
    }
    colnames(prehodne.verjetnosti) <- paste(mozni.rezultati)
  }
  return(prehodne.verjetnosti)
}

prehodne.verjetnosti <- prehodne.verjetnosti(tarce, mozni.rezultati, 5, 5,5)

# TO DO: prehodne verjetnosti stanj (mozna stanja)
# TO DO: zacetne vrednosti za algoritem
# TO DO: iterativni algoritem
# TO DO: minimum = optimalna tocka

