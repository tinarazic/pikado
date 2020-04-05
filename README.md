# Pikado 

## Program
Za zagon programa in shiny aplikacije je potrebno zagnati samo **datoteko `pikado.r`.**

S tem se zaženejo naslednje datoteke:

* osnovne funckije za igro pikada (tabla pikada, met puscice, točke):  `program/osnovne_funkcije.r`

* strategija 1 - maksimiziranje števila točk pri metu puščice:  `program/strategija1.r` 

* strategija 2 - minimziranje potrebnih rund do konca igre:  `program/strategija2.r`

* aplikacija shiny:  `shiny/server.r` in `shiny/ui.r`

Celotni opis matematičnega modela in implementacije se nahaja v datoteki **datoteki `teorija/matematicno_ozadje.pdf`** zaradi latex izrazov. Spodaj je **le začetek**.

## Namen
Cilj je analizirati optimalno strategijo pri igri pikada v odvisnosti od porazdelitve "puščice", to je od natančnostni igralca.

## Pravila igre pikada 301
Pikado tabla je krog razdeljen na 20 polj. Vrednost polja nam pove številka na obodu. Polja imajo vrednost od 1 do 20, dva majhna sredinska kroga pa imata vrednost 25 in 50 točk (veliki(outer) in mali(inner) bull). Na ozkih poljih, katera imajo dvojno ali trojno vrednost, pa se vrednost številke iz oboda podvoji oz. potroji.

Igra 301 pomeni, da imamo začetno vrednost 301 točk, katere moramo s puščicami "zapreti" na 0. Če z metom puščice pridemo na negativne točke, se vrnemo na število točk pred začetkom runde. Oznaka DO (double out) pomeni, da moramo priti na 0 tako, da zadnjo puščico vržemo v polje z dvojno vrednostjo ali v središče. Torej če nam na koncu ostane npr. 40 lahko zaključimo z metom v dvakratno vrednost polja 20. *Pri implementaciji bomo zaradi preprostosti obravnavali igro brez DO.* 

## Matematično ozadje
Za zmago v igri 301 je potrebno čim hiteje spraviti točke na nič, zato je na začetku igre optimalna strategija maksimiziranje števila točk z metom puščice. Proti zaključevanju igre pa to ni več optimalna strategija, saj lahko pridemo na negativno število točk in zapravimo našo rundo. Takrat bo optimalna strategija tista, ki bo minizirala število rund do zaključka igre. 

Nadaljevanje matematičnega ozadja je v **datoteki `teorija/matematicno_ozadje.pdf`**.