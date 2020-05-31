####################################################################################################################
# Optimalna strategija 2: minimziranje stevila rund do konca igre
# začnemo pri 60 točkah, ko maksimizirsanje točk, ni več optimalna strategija
####################################################################################################################

# poklicemo osnovne funckije in shranjene rezultate
source("lib/libraries.r")
source("program/osnovne_funkcije.r")
# source("program/rezultati/strategija2_rezultati1_60.r")
# source("program/rezultati/strategija2_verjetnosti_profesionalec60.r")
# source("program/rezultati/strategija2_verjetnosti_zacetnik60.r")
# source("program/rezultati/strategija2_verjetnosti_rekreativec60.r")
# source("program/rezultati/strategija2_iteracija60.r")
source("program/rezultati/strategija2_koncni_rezultat.r")

###### Izberemo ciljne točke p na pikado tabli #####
vektor.razdalj <- function(){
  # funkcija vrne razdalje na katerih želimo točke
  vektor <- c(0, 11.25,70,103,130,166,5000)
  return(vektor)
}

vektor.kotov <- function(stevilo.zarkov.polje){
  # funkcija sprejme stevilo zarkov v enem polju
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

# funkcija, ki definira točke na tabli
tarce.na.tabli <- function(razdalje, koti){
  # funkcija sprejme vektor razdalj in kotov,
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


###### Definiramo možne rezultate meta puščice #####

vektor.moznih.rezultatov.meta3 <- function(){
  # funkcija vrne vse možne rezultate ciljanja v tarco
  # vrne vektor unique vredosti
  vektor <- c(0)
  for (i in 1:20){
    vektor <- c(vektor, i, i*2, i*3)
  }
  vektor <- c(vektor, 25, 50)
  return(unique(vektor))
}

mozni.rezultati <- vektor.moznih.rezultatov.meta3()


###### Definiramo prehodne verjetnosti #####

prehodne.verjetnosti <- function(tarce, mozni.rezultati, stevilo.simulacij, stdX, stdY){
  # funkcija sprejme tabelo ciljnih tocke(tarce) in vektor moznih rezultatov
  # za vsako tarco izracuna prehodne verjetnosti:
  # verjetnost, da z metom puscice dobimo rezultat r, če ciljamo v tarco p
  # za vsako tarco aproksimiramo verjetnosti z uporabo metode Monte Carlo
  # parameter stevilo.simulacij pove, kolikokrat simuliramo met v dano tarco
  st.tarc <- length(tarce[,1])
  st.rezultatov <- length(mozni.rezultati)
  tarcaX <- tarce[,1]
  tarcaY <- tarce[,2]
  stevilo.puscic.p <- stevilo.simulacij
  prehodne.verjetnosti <- as.data.frame(matrix(1,nrow = st.tarc, ncol = st.rezultatov))
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

# prehodne.verjetnosti.profesionalec <- prehodne.verjetnosti(tarce, mozni.rezultati, 10000, 5,5)
# prehodne.verjetnosti.rekreativec <- prehodne.verjetnosti(tarce, mozni.rezultati, 10000, 20,20)
# prehodne.verjetnosti.zacetnik <- prehodne.verjetnosti(tarce, mozni.rezultati, 10000, 40,40)


###### Definiramo možna stanja igralca #####

mozna.stanja <- function(igra, mozni.rezultati){
  # sprejme parameter igra (301/501 ali število točk odkar želiš to strategijo) in vektor moznih rezultatov
  # časovno zahtevna funkcija, ki vrne vektor moznih stanj (številski vektor)
  # imamo tudi podvojene vrednosti v vektorju
  vektor <- vector()
  for (S1 in igra:0){
    vektor <- c(vektor, S1* 10000 + 1 * 1000 + S1)
    rezultati <- mozni.rezultati[mozni.rezultati <= S1]
    for (r in rezultati){
      S2 <- S1 - r
      vektor <- c(vektor, S2 * 10000 + 2* 1000 + S1)
      rezultati2 = rezultati[rezultati <= S2]
      for (r2 in rezultati2){
        S3 <- S2 - r2
        vektor <- c(vektor, S3* 10000 + 3 * 1000 + S1)
      }
    }
  }
  return(vektor)
}

#stanja60 <- mozna.stanja(60, mozni.rezultati)
# enolicna.stanja60 <- unique(stanja60)
# enolicna.stanja60 <- enolicna.stanja60[1:3562]
# 
# dump(c("prehodne.verjetnosti.profesionalec",
#        "prehodne.verjetnosti.rekreativec",
#        "prehodne.verjetnosti.zacetnik",
#        "enolicna.stanja60"),file = "program/rezultati/strategija2_rezultati1_60.r")


# Na tem mestu se odločim poenostaviti igro zaradi časovne zahtevnosti algoritmov.
# Igralec konča igro, ko pride točno na 0 (ne potrebuje double out).
# V primeru negativnih točk se vrne na stanje točk na začetku runde.


###### Definiramo prehodne matrike #####

prehodna.matrika.tocka <- function(stanja, prehodne.verjetnosti.tocka, mozni.rezultati){
  # funkcija vrne prehodno matriko stanj pri ciljanju v točko p
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
    if ((S ==0) | (S1 ==0)){
      absorbirajoce.stanje <- as.character(stanje)
      matrika[i, absorbirajoce.stanje] = 1
    }
    else {
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
        matrika[i, stanje.crta] <- matrika[i, stanje.crta] + prehodne.verjetnosti.tocka[, c(as.character(rezultat))]
      }
      stanje.bust <- as.character(S1* 10000 + 1*1000 + S1)
      verjetnost.nonbust <- sum(matrika[i, ])
      matrika[i, stanje.bust] = matrika[i, stanje.bust] + (1 - verjetnost.nonbust)
    }
    print(i)
    i <- i + 1
  }
  return(matrika)
}

# 
# 
# prehodna.matrika.tocka.1.profesionalec60<- prehodna.matrika.tocka(enolicna.stanja60,
#                                                    as.data.frame(prehodne.verjetnosti.profesionalec[1,]),
#                                                    mozni.rezultati)
# prehodna.matrika.tocka.1.rekreativec60<- prehodna.matrika.tocka(enolicna.stanja60,
#                                                                   as.data.frame(prehodne.verjetnosti.rekreativec[1,]),
#                                                                   mozni.rezultati)
# prehodna.matrika.tocka.1.zacetnik60<- prehodna.matrika.tocka(enolicna.stanja60,
#                                                                   as.data.frame(prehodne.verjetnosti.zacetnik[1,]),
#                                                                   mozni.rezultati)

# vse prehodne matrike spravimo v array
tabela.verjetnosti <- function(stanja, prehodne.verjetnosti, mozni.rezultati, prva.matrika){
  # funkcija sprejme matriko prehodnih verjetnosti ciljnih točk
  # možna stanja in možne rezultate
  # vrne array prehodnih verjetnostih iz stanja S v stanje S', pri dani ciljni točki
  id <- 1
  st.tock.p <-length(prehodne.verjetnosti[,1])
  st.stanj <- length(stanja)
  tabela <-  prva.matrika
  for (i in 2:st.tock.p){
    tabela <- rbind(tabela,
                    prehodna.matrika.tocka(stanja,as.data.frame(prehodne.verjetnosti[i,]), mozni.rezultati))
    print(i)
  }
  return(tabela)
}

# verjetnosti.profesionalec60 <- tabela.verjetnosti(enolicna.stanja60, prehodne.verjetnosti.profesionalec,
#                                                 mozni.rezultati, prehodna.matrika.tocka.1.profesionalec60)
# verjetnosti.rekreativec60 <- tabela.verjetnosti(enolicna.stanja60, prehodne.verjetnosti.rekreativec,
#                                                 mozni.rezultati, prehodna.matrika.tocka.1.rekreativec60)
# verjetnosti.zacetnik60 <- tabela.verjetnosti(enolicna.stanja60, prehodne.verjetnosti.zacetnik,
#                                              mozni.rezultati,prehodna.matrika.tocka.1.zacetnik60)
# 
# dump(c("verjetnosti.profesionalec60"), file = "program/rezultati/strategija2_verjetnosti_profesionalec60.r")
# dump(c("verjetnosti.rekreativec60"), file = "program/rezultati/strategija2_verjetnosti_zacetnik60.r")
# dump(c("verjetnosti.zacetnik60"), file = "program/rezultati/strategija2_verjetnosti_rekreativec60.r")

#################################################################################################################
###### ALGORITEM ######
#################################################################################################################

###### Fiksni vektor C(S) ######

vektorC <- function(stanja){
  # funkcija sprejme vektor enolicnih stanj
  # vrne vektor v velikosti stevila stanj, ki ima vrednost 1 če smo v stanju pred 3 metom puščice, 
  # drugače pa vrednost 0
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

# fixni.vektorC60 <- vektorC(enolicna.stanja60)

###### 1 korak iterativnega algoritma ######
izracunV <- function(vektorC, stariV, verjetnosti){
  st.tock.p <- 83
  minimum <- Matrix(data=100,nrow=3562,ncol=1)
  for (i in 0:(st.tock.p-1)){
    index1 <- 3562*i + 1 
    index2 <- 3562*(i+1)
    prehodna.matrika.tocka <- verjetnosti[index1:index2,]
    vsota.pri.tocki.p <- prehodna.matrika.tocka %*% (stariV + vektorC)
    #print(vsota.pri.tocki.p)
    minimum <- pmin(minimum, vsota.pri.tocki.p)
  }
  rownames(minimum) <- rownames(verjetnosti[1:3562,])
  return(minimum)
}


###### Začetne vrednosti algoritma V0 ######
# velikost60 <- length(enolicna.stanja60)
# vektorV0.profesionalec60 <- Matrix(data=3,nrow=velikost60,ncol=1)
# vektorV0.rekreativec60 <- Matrix(data=5,nrow=velikost60,ncol=1)
# vektorV0.zacetnik60 <- Matrix(data=10,nrow=velikost60,ncol=1)

###### ITERATIVNI ALGORITM ######
algoritem <- function(vektorC, V0, verjetnosti){
  # funkcija sprejme fiksni vektor C, začetno vrednost vektorja V in veliko sparse matriko verjetnosti
  # vrne rezultat iterativnega algoritma in koncni V
  napaka <- 1
  V <- V0
  i <- 1
  while ((napaka > 0.01) & (i < 200)) {
    print(i)
    stariV <- V
    V <- izracunV(vektorC, stariV, verjetnosti)
    #print(V)
    napaka <- mean(abs(V - stariV))
    print(napaka)
    i <- i+1
  }
  return(V)
}


# koncniV.profesionalec60 <- algoritem(fixni.vektorC60,vektorV0.profesionalec60,verjetnosti.profesionalec60)
# koncniV.rekreativec60 <- algoritem(fixni.vektorC60,vektorV0.rekreativec60,verjetnosti.rekreativec60)
# koncniV.zacetnik60 <- algoritem(fixni.vektorC60,vektorV0.zacetnik60,verjetnosti.zacetnik60)
# dump(c("koncniV.profesionalec60",
#        "koncniV.rekreativec60",
#        "koncniV.zacetnik60"), file = "program/rezultati/strategija2_iteracija60.r")


###### Ciljna točka za minimizacijo števila rund ######

index.ciljna.tocka <- function(vektorC, koncniV, verjetnosti){
  # funkcija sprejme fixni vektor C,
  # vektor koncniV, ki je rezultat iterativnega algoritma in veliko sparse matriko verjetnosti
  # vrne indeks optimalne točke p za ciljanje
  st.tock.p <- 83
  minimum <- Matrix(data=100,nrow=3562,ncol=1)
  p <- Matrix(data=100,nrow=3562,ncol=1)
  for (i in 0:(st.tock.p-1)){
    index1 <- 3562*i + 1 
    index2 <- 3562*(i+1)
    prehodna.matrika.tocka <- verjetnosti[index1:index2,]
    vsota.pri.tocki.p <- prehodna.matrika.tocka  %*% (koncniV + vektorC)
    p <- ifelse(vsota.pri.tocki.p <= minimum, i+1, p)
    minimum <- pmin(minimum, vsota.pri.tocki.p)
  }
  return(p)
}

# index.ciljna.tocka.profesionalec60 <- index.ciljna.tocka(fixni.vektorC60,
#                                                         koncniV.profesionalec60, verjetnosti.profesionalec60)
# index.ciljna.tocka.rekreativec60 <- index.ciljna.tocka(fixni.vektorC60,
#                                                          koncniV.rekreativec60, verjetnosti.rekreativec60)
# index.ciljna.tocka.zacetnik60 <- index.ciljna.tocka(fixni.vektorC60,
#                                                          koncniV.zacetnik60, verjetnosti.zacetnik60)


optimalna.ciljna.tocka.str2 <- function(vektor.indeksov, tarce){
  # funkcija sprejme vektor indeksov, ki pove indeks točke v tabeli tarce
  # vrne tabelo, ki ima stolpce koordinataX, koordinataY, polje
  velikost <- length(vektor.indeksov)
  tabela <- as.data.frame(matrix(0,velikost,3))
  for (i in 1:velikost){
    indeks <- vektor.indeksov[i]
    if (indeks <= 83){
      tabela[i,1] <- tarce[indeks,1]
      tabela[i,2] <- tarce[indeks,2]
      tabela[i,3] <- rezultat.meta2(tabela[i,1],tabela[i,2])
    }
    else {
      tabela[i,1] <- 55555
      tabela[i,2] <- 55555
    }
  }
  colnames(tabela) <- c("koordinataX", "koordinataY","polje")
  return(tabela)  
}

# optimalno.polje.profesionalec60 <- optimalna.ciljna.tocka.str2(index.ciljna.tocka.profesionalec60, tarce)
# optimalno.polje.rekreativec60 <- optimalna.ciljna.tocka.str2(index.ciljna.tocka.rekreativec60, tarce)
# optimalno.polje.zacetnik60 <- optimalna.ciljna.tocka.str2(index.ciljna.tocka.zacetnik60, tarce)

stanje.in.polje <- function(enolicna.stanja, optimalno.polje){
  # funkcija, ki sprejme vektor enolicnih stanj
  # tabelo optimalno.polje, ki ima stolpce koordinataX, koordinataY, pripadajoče polje na pikado tabli
  # funkcija tabeli optimalno.polje doda stolpec "stanje" na začetek
  # dobimo tabelo, ki pove kam naj ciljamo, če smo v določenemu stanju 
  velikost <- length(enolicna.stanja)
  tabela <- as.data.frame(matrix(0,velikost,1))
  for (i in 1:velikost){
    stanje.stevilka <- enolicna.stanja[i]
    S <-  stanje.stevilka %/% 10000 # stanje pred trenutnim metom
    t <- (stanje.stevilka - 10000* S)%/%1000  # st.meta
    S1 <- stanje.stevilka %% 1000 # stanje na začetku runde
    stanje <- sprintf("%s-%s-%s", S,t,S1)
    tabela[i,1] <- stanje
  }
  tabela <- cbind(tabela, optimalno.polje)
  colnames(tabela) <-  c("stanje", "koordinataX", "koordinataY","polje") 
  return(tabela)
}

# stanje.in.polje.profesionalec <- stanje.in.polje(enolicna.stanja60,optimalno.polje.profesionalec60)
# stanje.in.polje.rekreativec <- stanje.in.polje(enolicna.stanja60,optimalno.polje.rekreativec60)
# stanje.in.polje.zacetnik <- stanje.in.polje(enolicna.stanja60,optimalno.polje.zacetnik60)

# dump(c("stanje.in.polje.profesionalec",
#        "stanje.in.polje.rekreativec",
#        "stanje.in.polje.zacetnik"), file = "program/rezultati/strategija2_koncni_rezultat.r")



narisi.tocke <- function(tocke){
  # funkcija, ki sprejme koordinato x in y in nariše točko na pikado tabli
  drawBoard(new = TRUE)
  points(tocke[,1], tocke[,2], col = "red", pch = 21, bg = "red", cex =1.6)
}
    


