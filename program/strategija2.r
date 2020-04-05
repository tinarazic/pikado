####################################################################################################################
# Optimalna strategija 2: minimziranje stevila rund do konca igre
####################################################################################################################

# poklicemo osnovne funckije
source("lib/libraries.r")
source("program/osnovne_funkcije.r")
source("program/strategija2_rezultati.r")

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
  vektor <- c(0)
  for (i in 1:20){
    vektor <- c(vektor, i, i*2, i*3)
  }
  vektor <- c(vektor, 25, 50)
  return(vektor)
}

mozni.rezultati <- vektor.moznih.rezultatov.meta()

vektor.moznih.rezultatov.meta2 <- function(){
  # enako kot funckija vektor.moznih.rezultatov.meta le da vključi pas, zato vrne vektor character
  vektor <- c("0-0")
  for (i in 1:20){
    vektor <- c(vektor, sprintf("%s-%s", i, 1), sprintf("%s-%s", i*2, 2), sprintf("%s-%s", i*3, 3))
  }
  vektor <- c(vektor, "25-BULL", "50-BULL")
  return(vektor)
}

vektor.moznih.rezultatov.meta3 <- function(){
  # enako kot funckija vektor.moznih.rezultatov.meta le da vrne vektor unique vrednsoti
  vektor <- c(0)
  for (i in 1:20){
    vektor <- c(vektor, i, i*2, i*3)
  }
  vektor <- c(vektor, 25, 50)
  return(unique(vektor))
}

mozni.rezultati <- vektor.moznih.rezultatov.meta()
mozni.rezultati2 <- vektor.moznih.rezultatov.meta2()
mozni.rezultati3 <- vektor.moznih.rezultatov.meta3()

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

prehodne.verjetnosti <- prehodne.verjetnosti(tarce, mozni.rezultati3, 5, 5,5)
dump(c("prehodne.verjetnosti"), file = "program/strategija2_rezultati.r")

mozna.stanja <- function(igra, mozni.rezultati){
  # sprejme parameter igra (301/501) in vektor moznih rezultatov 
  # časovno zahtevna funckija, ki vrne vektor moznih stanj
  # imamo tudi podvojene vrednosti v vektorju
  vektor <- vector()
  for (S1 in igra:1){
    vektor <- c(vektor, sprintf("%s-1-%s", S1,S1))
    rezultati = mozni.rezultati[mozni.rezultati <= S1]
    for (r in rezultati){
      S2 = S1 - r
      vektor <- c(vektor, sprintf("%s-2-%s", S2,S1))
      rezultati2 = rezultati[rezultati <= S2]
      for (r2 in rezultati2){
        S3 = S2 - r2
        vektor <- c(vektor, sprintf("%s-3-%s", S3,S1))
      }
    }
  }
  return(vektor)
}

#stanja <- mozna.stanja(301, mozni.rezultati)
#dump(c("stanja"), file = "program/strategija2_rezultati.r")

mozna.stanja2 <- function(igra){
  # slaba funckija, ker vsebuje veliko stanj, ki niso mozna
  vektor <- c()
  for (S1 in igra:1){
    vektor <- c(vektor, sprintf("%s-%s-%s", S1,1, S1))
    for (t in 2:3){
      for (S in igra:1){
        vektor <- c(vektor, sprintf("%s-%s-%s", S,t, S1))
      }
    }
  }
  return(vektor)
}

# stanja2 <- mozna.stanja2(301)

enolicna.stanja <- unique(stanja)
prehodne.verjetnosti.tocka <- as.data.frame(prehodne.verjetnosti[1,])

# na tem mestu se odločim poenostaviti igro zharadi časovne zahtevnosti algoritmov
# Igralec konča igro, ko pride točno na 0 (ne potrebuje double out)
# v primeru negativnih točk se vrne na stanje točk na začetku runde

prehodna.matrika.tocka <- function(stanja, prehodne.verjetnosti.tocka, mozni.rezultati){
  # funckija vrne prehodno matriko stanj pri ciljanju v točko p
  # sprejme vsa mozna stanja, verjetnosti zadetka v posamezno polje, če ciljamo v točko p
  # in vektor možnih rezultatov
  velikost <- length(stanja)
  matrika <- as.data.frame(matrix(0, ncol = velikost, nrow = velikost))
  colnames(matrika) <- stanja
  i <- 1
  for (stanje in stanja){
    loceno <- strsplit(stanje, "[-]")[[1]]
    S <-  as.integer(loceno[1])
    t <- as.integer(loceno[2])
    S1 <- as.integer(loceno[3])
    for (stanje_crta in stanja){
      loceno_crta <- strsplit(stanje_crta, "[-]")[[1]]
      S_crta <-  as.integer(loceno_crta[1])
      t_crta <- as.integer(loceno_crta[2])
      S1_crta <- as.integer(loceno_crta[3])
      rezultat = S - S_crta
      if (t_crta == (t+1) | t_crta == 1){
        # lahko pridemo samo iz 1->1,2, 2->1,3, 3 -> 1
          if (rezultat >= 0 & is.element(rezultat, mozni.rezultati)){
            # prehod iz 1 -> 2 ali iz 2 -> 3
            matrika[i, c(stanje_crta)] = prehodne.verjetnosti.tocka[,c(as.character(rezultat))]
          }
      }
    }
    stanje_bust <- sprintf("%s-1-%s", S1,S1)
    verjetnost.nonbust <- sum(matrika[i, ])
    matrika[i, c(stanje_bust)] = 1 - verjetnost.nonbust
    i <- i + 1
  }
  return(matrika)
}

# test
#prehodna.matrika.tocka.1 <- prehodna.matrika.tocka(enolicna.stanja, prehodne.verjetnosti.tocka, mozni.rezultati3) 
# traja neznano dolgo, več kot 10 ur? 
# Error: cannot allocate vector of size 313 Kb


# TO DO: zacetne vrednosti za algoritem
# TO DO: iterativni algoritem
# TO DO: minimum = optimalna tocka

