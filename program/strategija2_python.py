mozni_rezultati = [0, 1, 2, 3, 4,  6,  9,  8, 12,  5, 10, 15, 18,  7, 14, 21, 16, 24, 27, 20, 30, 11, 22, 33, 36, 13, 26, 39, 28, 42, 45, 32, 48, 17, 34, 51, 54, 19, 38, 57,40, 60, 25, 50]
igra = 301

def mozna_stanja(igra, mozni_rezultati):
    vektor = []
    # sprejme parameter igra (301/501) in vektor moznih rezultatov 
    # časovno zahtevna funckija, ki vrne vektor moznih stanj
    # imamo tudi podvojene vrednosti v vektorju
    for S1 in range(igra + 1):
        vektor.append("{}-1-{}".format(S1, S1))
        rezultati = [i for i in mozni_rezultati if i <= S1]
        for r in rezultati:
            S2 = S1 - r
            vektor.append('{}-2-{}'.format(S2, S1))
            rezultati2 = [i for i in rezultati if i <= S2]
            for r2 in rezultati2:
                S3 = S2 - r2
                vektor.append('{}-3-{}'.format(S3, S1))
    return(vektor)

stanja = mozna_stanja(igra, mozni_rezultati)