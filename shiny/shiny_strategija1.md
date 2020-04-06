---
output: html_document
---



## Maksmiziranje števila točk pri metu puščice
Met puščice ima gostoto

$$
p(x,y) = \frac{1}{2\pi \sigma_{x} \sigma_{y}} exp \Big( - \frac{(x-\mu_{x})^2}{2\sigma_{x}^2} - \frac{(y-\mu_{y})^2}{2\sigma_{y}^2}\Big).
$$

Naj bo $S$ število točk, ki jih je igralec dosegel z metom puščice in $d(x,y)$ funkcija, ki pretvori koordinate meta v število točk. Potem lahko pričakovano vrednost točk pri metu puščice v ciljno točko $(\mu_{x}, \mu_{y})$ glede na natančnost igralca definiramo kot:

$$
E(S|\mu_{x}, \mu_{y}, \sigma_{x}, \sigma_{y}) = E(d(x,y)|\mu_{x}, \mu_{y}, \sigma_{x}, \sigma_{y}) = \iint_D d(x,y) p(x,y) dxdy,
$$

kjer je $D$ označuje območje polja na tabli.

Zanima nas, v katero točko moramo ciljati glede na naš nivo natančnosti, da dosežemo največ točk v enem metu. 

### Implementacija
Idejo implementiramo s pomočjo **metode Monte Carlo**. Tablo razdelimo na mrežo točk $(\mu_{x}^{n}, \mu_{y}^{m}),$ kjer je $\mu_x^n = n\Delta$, $\mu_x^m = m\Delta, \Delta = 170/N$ mm (N poljubno veliko število) in 

$$
m, n \in \{ -N, \dots, -1,0,1, \dots, N| n^2 + m^2 \leq N^2\}.
$$

V vsaki točki izračunamo zgornjo pričakovano vrednost. Točka pri kateri je pričakovana vrednost največja je **optimalna ciljna točka** pri natančnosti $(\sigma_{x}, \sigma_{y})$ in dosežena  vrednost je **optimalno številko točk**.
Integral s katerim dobimo pričakovano vrednost pa prav tako izračunamo s pomočjo metode Monte Carlo.
