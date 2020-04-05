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

mozni.rezultati <- vektor.moznih.rezultatov.meta()
mozni.rezultati2 <- vektor.moznih.rezultatov.meta2()

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
    rezultati <- ddply(meti,c("koordinataX", "koordinataY"), function(df) rezultat.meta2(df$koordinataX, df$koordinataY))
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

#prehodne.verjetnosti <- prehodne.verjetnosti(tarce, mozni.rezultati2, 5, 5,5)
dump(c("prehodne.verjetnosti"), file = "program/strategija2_rezultati.r")

mozna.stanja <- function(game, mozni.rezultati){
  vektor <- vector()
  for (S1 in game:1){
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
dump(c("stanja"), file = "program/strategija2_rezultati.r")

# # ideja sproti delaš matriko, pa gledaš če je double in zračunaš verjetnosti itd...
# prehodna.matrika.tocka <- function(game, mozni.rezultati2, prehodne.verjetnosti.tocka){
#   ################### TBD ##################################################
#   ################### TBD ##################################################
#   matrika <- data.frame()
#   for (S1 in game:1){
#     stanje <- sprintf("%s-1-%s", S1,S1)
#     rezultati = mozni.rezultati[mozni.rezultati <= S1]
#     for (r in rezultati){
#       loceno <- strsplit(r, "[-]")[[1]]
#       zadetek <-  as.integer(loceno[1])
#       pas <- as.integer(loceno[2])
#       # 1. možnost: # bust: negativno stanje ali na 0 brez double out
#       if (zadetek >= S1 & pas != 2) {
#         bust = sprintf("%s-1-%s", S1,S1)
#         # seštejemo verjetnosti od tistih ki bi an pripeljela v negativno stanje + nepravilna nula
#         nepravilna.nula1 = sprintf("%s-1", zadetek)
#         nepravilna.nula2 = sprintf("%s-3", zadetek)
#         matrika[stanje, bust] = prehodne.verjetnosti.tocka[prehodne.verjetnosti.tocka != rezultati | prehodne.verjetnosti.tocka = c(nepravilna.nula1, nepravilna.nula2) ]
#       }
#       else {
#         # 2. možnost: zadanemo v redu
#         S2 = S1 - zadetek
#         zadanemo = sprintf("%s-2-%s", S2,S1)
#         matrika[stanje, zadanemo] = prehodne.verjetnosti.tocka[r]
#         rezultati2 = rezultati[rezultati <= S2]
#       }
#       
#       for (r2 in rezultati2){
#           loceno2 <- strsplit(r2, "[-]")[[1]]
#           zadetek2 <-  as.integer(loceno2[1])
#           pas2 <- as.integer(loceno2[2])
#           # 1. možnost: # bust: negativno stanje ali na 0 brez double out
#           if (zadetek2 >= S2 & pas2 != 2) {
#             bust2 = sprintf("%s-1-%s", S1,S1)
#             # seštejemo verjetnosti od tistih ki bi an pripeljela v negativno stanje + nepravilna nula
#             nepravilna.nula1.2 = sprintf("%s-1", zadetek2)
#             nepravilna.nula2.2 = sprintf("%s-3", zadetek2)
#             matrika[stanje, bust2] = prehodne.verjetnosti.tocka[prehodne.verjetnosti.tocka != rezultati2 | prehodne.verjetnosti.tocka = c(nepravilna.nula1.2, nepravilna.nula2.2) ]
#           }
#           else {
#             # 2. možnost: zadanemo v redu
#             S3 = S2 - zadetek2
#             zadanemo2 = sprintf("%s-3-%s", S3,S1)
#             matrika[stanje, zadanemo] = prehodne.verjetnosti.tocka[r]
#             rezultati3 = rezultati[rezultati <= S3]
#           }
#         
#       }
#       
#       for (r3 in rezultati3){
#         loceno3 <- strsplit(r3, "[-]")[[1]]
#         zadetek3 <-  as.integer(loceno3[1])
#         pas3 <- as.integer(loceno3[2])
#         # 1. možnost: # bust: negativno stanje ali na 0 brez double out
#         if (zadetek3 >= S3 & pas3 != 2) {
#           bust3 = sprintf("%s-1-%s", S1,S1)
#           # seštejemo verjetnosti od tistih ki bi an pripeljela v negativno stanje + nepravilna nula
#           nepravilna.nula1.3 = sprintf("%s-1", zadetek3)
#           nepravilna.nula2.3 = sprintf("%s-3", zadetek3)
#           matrika[stanje, bust3] = prehodne.verjetnosti.tocka[prehodne.verjetnosti.tocka != rezultati2 | prehodne.verjetnosti.tocka = c(nepravilna.nula1.2, nepravilna.nula2.2) ]
#         }
#         else {
#           # 2. možnost: zadanemo v redu
#           S3 = S2 - zadetek2
#           zadanemo2 = sprintf("%s-3-%s", S3,S1)
#           matrika[stanje, zadanemo] = prehodne.verjetnosti.tocka[r]
#           rezultati2 = rezultati[rezultati <= S2]
#         }
#         ################### TBD ##################################################
#       }
#     }
#   }
#   return(vektor)
# }



# TO DO: prehodne verjetnosti stanj (mozna stanja)
# TO DO: zacetne vrednosti za algoritem
# TO DO: iterativni algoritem
# TO DO: minimum = optimalna tocka

