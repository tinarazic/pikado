---
output: html_document
---



Za zmago v igri 301 je potrebno čim hiteje spraviti točke na nič, zato je na začetku igre optimalna strategija maksimiziranje števila točk z metom puščice. Proti zaključevanju igre pa to ni več optimalna strategija, saj lahko pridemo na negativno število točk in zapravimo našo rundo. Takrat bo optimalna strategija tista, ki bo minizirala število rund do zaključka igre. 

Na začetku sprejmemo nekaj predpostavk:

* širino žic, ki ločujejo polja na realni pikado tabli ignoriramo

* met puščice je porazdeljen s porazdelitvijo dvorasežne normalne z neodvisnima komponentama s parametroma $(\mu_{x}, \mu_{y})$ - ciljna točka in $(\sigma_{x}, \sigma_{y})$ - natančnost igralca

* meti so neodvisni

Pri implementaciji si pomagamo z meritvami na sliki:

<img src="../../slike/koti.png" title="plot of chunk unnamed-chunk-1" alt="plot of chunk unnamed-chunk-1" width="60%" />
