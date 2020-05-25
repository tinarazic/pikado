####################################################################################################################
# Optimalna strategija 2: minimziranje stevila rund do konca igre
####################################################################################################################

# poklicemo osnovne funckije
source("lib/libraries.r")
source("program/osnovne_funkcije.r")

vektor.razdalj <- function(){
  # funckija vrne razdalje na katerih želimo točke
  vektor <- c(0, 11.25,70,103,130,166,240)
  return(vektor)
}

vektor.kotov <- function(stevilo.zarkov.polje){
  # funckija sprejme stevilo zarkov v enem polju
  # in vrne vektor kotov
  stevilo.zarkov = stevilo.zarkov.polje*20
  vektor <- c()
  for (i in 1:stevilo.zarkov){
    kot.stopinje = i*360/stevilo.zarkov 
    kot.radiani = 2*pi*kot.stopinje/360
    vektor = c(vektor,kot.radiani)
  }
  return(vektor)
}

razdalje <- vektor.razdalj()
koti <- vektor.kotov(1)

# funckija, ki definira točke na tabli
tarce.na.tabli <- function(razdalje, koti){
  # funckija sprejme vektor razdalj in kotov,
  # za vsak kot na vsaki razdalji nariše točko na tabli in vrne tabelo točk
  drawBoard(new = TRUE)
  koorX <- vector()
  koorY <- vector()
  for (kot in koti){
    # pretvorba v kartezične koordinate
    x = razdalje[3:6]*cos(kot)
    y = razdalje[3:6]* sin(kot)
    koorX <- c(koorX, x)
    koorY <- c(koorY, y)
  }
  # dodamo točko v središče
  sredisce <- razdalje[1]
  # dodamo točko v outer bull 
  x.outer <- razdalje[2]*cos(pi/2)
  y.outer <- razdalje[2]*sin(pi/2)
  # dodamo točko v območje brez pik
  x.brez.pik <- razdalje[7]*cos(pi/4)
  y.brez.pik <- razdalje[7]*sin(pi/4)
  koorX <- c(koorX, sredisce, x.outer, x.brez.pik)
  koorY <- c(koorY, sredisce, y.outer, y.brez.pik)
  # narišemo točke
  points(koorX, koorY, col="red", pch=18)
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

mozni.rezultati <- vektor.moznih.rezultatov.meta3()

prehodne.verjetnosti <- function(tarce, mozni.rezultati, stevilo.simulacij, stdX, stdY){
  # funckija sprejme tabelo ciljnih tocke(tarce) in vektor moznih rezultatov
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

prehodne.verjetnosti.profesionalec <- prehodne.verjetnosti(tarce, mozni.rezultati, 100, 5,5)
prehodne.verjetnosti.rekreativec <- prehodne.verjetnosti(tarce, mozni.rezultati, 100, 20,20)
prehodne.verjetnosti.zacetnik <- prehodne.verjetnosti(tarce, mozni.rezultati, 100, 40,40)

mozna.stanja <- function(igra, mozni.rezultati){
  # sprejme parameter igra (301/501) in vektor moznih rezultatov
  # časovno zahtevna funckija, ki vrne vektor moznih stanj (številski vektor)
  # imamo tudi podvojene vrednosti v vektorju
  vektor <- vector()
  for (S1 in igra:0){
    vektor <- c(vektor, S1* 10000 + 1 * 1000 + S1)
    rezultati = mozni.rezultati[mozni.rezultati <= S1]
    for (r in rezultati){
      S2 = S1 - r
      vektor <- c(vektor, S2 * 10000 + 2* 1000 + S1)
      rezultati2 = rezultati[rezultati <= S2]
      for (r2 in rezultati2){
        S3 = S2 - r2
        vektor <- c(vektor, S3* 10000 + 3 * 1000 + S1)
      }
    }
  }
  return(vektor)
}

stanja <- mozna.stanja(301, mozni.rezultati)
test.stanja.60 <- mozna.stanja(60, mozni.rezultati)
test.stanja.60 <- unique(test.stanja.60)

mozna.stanja.niz <- function(igra, mozni.rezultati){
  # sprejme parameter igra (301/501) in vektor moznih rezultatov 
  # časovno zahtevna funckija, ki vrne vektor moznih stanj (vektor nizov)
  # imamo tudi podvojene vrednosti v vektorju
  vektor <- vector()
  for (S1 in igra:0){
    vektor <- c(vektor, S1* sprintf("%s-1-%s", S1,S1))
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

enolicna.stanja <- unique(stanja)
test.stanja <- enolicna.stanja[1:1000]
prehodne.verjetnosti.profesionalec.tocka <- as.data.frame(prehodne.verjetnosti.profesionalec[1,])

dump(c("prehodne.verjetnosti.profesionalec",
       "prehodne.verjetnosti.rekreativec", 
       "prehodne.verjetnosti.zacetnik",
       "enolicna.stanja"),file = "program/strategija2_rezultati1.r")

# na tem mestu se odločim poenostaviti igro zharadi časovne zahtevnosti algoritmov
# Igralec konča igro, ko pride točno na 0 (ne potrebuje double out)
# v primeru negativnih točk se vrne na stanje točk na začetku runde

prehodna.matrika.tocka <- function(stanja, prehodne.verjetnosti.tocka, mozni.rezultati){
  # funckija vrne prehodno matriko stanj pri ciljanju v točko p
  # sprejme vsa mozna stanja, verjetnosti zadetka v posamezno polje, če ciljamo v točko p
  # in vektor možnih rezultatov
  velikost <- length(stanja)
  matrika <- Matrix(data=0,nrow=velikost,ncol=velikost,sparse=TRUE)
  colnames(matrika) <- as.character(stanja)
  colnames(prehodne.verjetnosti.tocka) <- as.character(mozni.rezultati)
  i <- 1
  for (stanje in stanja){
    S <-  stanje %/% 10000 # stanje pred trenutnim metom
    t <- (stanje - 10000* S)%/%1000  # st.meta
    S1 <- stanje %% 1000 # stanje na začetku runde
    rezultati = mozni.rezultati[mozni.rezultati <= S]
    for (rezultat in rezultati){
      S_crta <- S - rezultat
      if (t == 3) {
        t_crta <- 1
        S1_crta <- S_crta
      }
      else {
        t_crta <- t+1
        S1_crta <- S1
      }
      stanje.crta <- as.character(S_crta* 10000 + t_crta*1000 + S1_crta)
      matrika[i, stanje.crta] <- prehodne.verjetnosti.tocka[, c(as.character(rezultat))]
    }
    stanje.bust <- as.character(S1* 10000 + 1*1000 + S1)
    verjetnost.nonbust <- sum(matrika[i, ])
    matrika[i, stanje.bust] = 1 - verjetnost.nonbust
    print(i)
    i <- i + 1
  }
  return(matrika)
}

## test
prehodna.matrika.tocka.1 <- prehodna.matrika.tocka(enolicna.stanja, 
                                                   as.data.frame(prehodne.verjetnosti.profesionalec[1,]),
                                                  mozni.rezultati)

# vse prehodne matrike spravimo v array
tabela.verjetnosti <- function(stanja, prehodne.verjetnosti, mozni.rezultati){
  # funckija sprejme matriko prehodnih verjetnosti ciljnih točk
  # možna stanja in možne rezultate
  # vrne array prehodnih verjetnostih iz stanja S v stanje S', pri dani ciljni točki
  id <- 1
  st.tock.p <-length(prehodne.verjetnosti[,1])
  st.stanj <- length(stanja)
  tabela <-  prehodna.matrika.tocka.1
  for (i in 2:st.tock.p){
    tabela <- rbind(tabela,
                    prehodna.matrika.tocka(stanja,as.data.frame(prehodne.verjetnosti[i,]), mozni.rezultati))
    print(i)
  }
  return(tabela)
}

#verjetnosti.profesionalec <- tabela.verjetnosti(enolicna.stanja, prehodne.verjetnosti.profesionalec, mozni.rezultati)
#dump(c("verjetnosti.profesionalec"), file = "program/strategija2_verjetnosti_profesionalec.r")

# fiksni vektor C(S)

vektorC <- function(stanja){
  velikost <- length(stanja)
  vektor <- Matrix(data=0,nrow=velikost,ncol=1,sparse=TRUE)
  i <- 1 
  for (stanje.crta in stanja){
    S <-  stanje.crta %/% 10000
    t.crta <- (stanje.crta - 10000* S)%/%1000
    if (t.crta == 1){
      vektor[i] <- 1
    }
    i <- i  + 1
  }
  return(vektor)
} 

fixni.vektorC <- vektorC(enolicna.stanja)

# 1 korak ITERATIVNEGA ALGORITMA
izracunV <- function(vektorC, stariV, verjetnosti){
  st.tock.p <- 83
  minimum <- Matrix(data=100,nrow=40093,ncol=1)
  for (i in 0:(st.tock.p-1)){
    index1 <- 40093*i + 1 
    index2 <- 40093*(i+1)
    prehodna.matrika.tocka <- verjetnosti[index1:index2,]
    vsota.pri.tocki.p <- prehodna.matrika.tocka %*% (stariV + vektorC)
    minimum <- pmin(minimum, vsota.pri.tocki.p)
  }
  return(minimum)
}
  
# začetne vrednosti V0 
velikost <- length(enolicna.stanja)
vektorV0.profesionalec <- Matrix(data=3,nrow=velikost,ncol=1)

prehodna.matrika.tocka <- verjetnosti.profesionalec[40094:80186,]
vsota.pri.tocki.p <- prehodna.matrika.tocka %*% (vektorV0.profesionalec + fixni.vektorC)

i = 2
minimum <- Matrix(data=100,nrow=40093,ncol=1)
index1 <- 40093*i + 1 
index2 <- 40093*(i+1)
prehodna.matrika.tocka <- verjetnosti.profesionalec[index1:index2,]
vsota.pri.tocki.p <- prehodna.matrika.tocka %*% (vektorV0.profesionalec + fixni.vektorC)
minimum <- pmin(minimum, vsota.pri.tocki.p)

V <- izracunV(fixni.vektorC, vektorV0.profesionalec, verjetnosti.profesionalec)

# ITERATIVNI ALGORITM
algoritem <- function(vektorC, V0, verjetnosti){
  napaka <- 1
  V <- V0
  i <- 1
  while (napaka > 0.001) {
    print(i)
    stariV <- V
    V <- izracunV(vektorC, stariV, verjetnosti)
    napaka <- mean(abs(V - stariV))
    i <- i+1
  }
  return(V)
}

stevilo.metov.do.konca.profesionalec <- algoritem(fixni.vektorC,vektorV0.profesionalec,verjetnosti.profesionalec)
dump(c("stevilo.metov.do.konca.profesionalec"), file = "program/strategija2_iteracija_profesionalec.r")

index.cilja.tocka <- function(vektorC, stariV, verjetnosti){
  st.tock.p <- length(verjetnosti[1,])
  for (i in 0:(st.tock.p-1)){
    index1 <- 40093*i + 1 
    index2 <- 40093*(i+1)
    prehodna.matrika.tocka <- verjetnosti[index1:index2,]
    izracun <- (stariV + vektorC) %*% prehodna.matrika.tocka
    p <- ifelse(izracun <= stariV, i, p)
    stariV <- izracun
  }
  return(p)
}

optimalna.ciljna.tocka.str2 <- function(index){
  koordinataX <- apply(index, tarce[index,1])
  koordinataY <- apply(index, tarce[index,2])
  return(rezultat.meta(koordinataX, koordinataY))
}

# TO DO: zacetne vrednosti za algoritem
# TO DO: iterativni algoritem
# TO DO: minimum = optimalna tocka

