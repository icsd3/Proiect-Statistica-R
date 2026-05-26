# Proiect-Statistica-R

Proiect la Statistică, anul II, UB FMI CTI, realizat de Popa Mircea Alexandru, Sîrghe Matei, Bujor Ștefan și Ungureanu Robert Anton.

## Descriere

Repository-ul conține două probleme rezolvate în R, împreună cu documentația aferentă:

- `docs/documentatie.qmd` — documentația proiectului, scrisă în Quarto
- `src/problema1.r` — simulare și analiză pentru evenimente rare
- `src/problema2.r` — aplicație Shiny pentru transformări de variabile aleatoare continue

## Problemele propuse

### Problema 1: Evenimente rare — detecție și simulare

Modelul simulează un sistem cu cereri zilnice generate aleator, unde o parte dintre ele sunt considerate suspecte. Sunt comparate mai multe strategii de verificare:

- verificare simplă, cu procent fix
- verificare adaptivă, în funcție de volumul traficului
- verificare adaptivă bazată pe scor Z și funcție logistică (sigmoidă)

Pentru evaluare sunt calculate indicatori precum rata de detecție, numărul mediu de verificări și costul mediu al strategiilor.

### Problema 2: Transformări de variabile aleatoare continue

Aplicația Shiny permite generarea de eșantioane pentru distribuții continue și aplicarea unor transformări asupra lor. Sunt acoperite două scenarii:

- componentă unidimensională: generare pentru distribuții normală, uniformă, exponențială și gamma, urmată de transformări precum $x^2$, $|x|$, $\log(x)$, $e^x$ și $\sin(x)$
- componentă bidimensională: generare de perechi independente sau bivariate normale și transformări de tip sumă, diferență, produs și normă euclidiană

Aplicația afișează histograme, statistici descriptive și grafice comparative pentru datele generate.

## Tehnologii folosite

- **R** — limbajul principal pentru simulare și analiză statistică
- **Quarto** — pentru documentația proiectului
- **Shiny** — pentru interfața web interactivă din problema 2
- **ggplot2** — pentru vizualizări statistice

## Librării R necesare
- `shiny`
- `ggplot2`
- `shinythemes`
- `MASS`

## Rulare

- Problema 1 poate fi rulată direct în R sau RStudio prin încărcarea fișierului `src/problema1.r`.
- Problema 2 se rulează ca aplicație Shiny din `src/problema2.r`.
- Documentația se poate compila din `docs/documentatie.qmd` cu Quarto.

## Structura repo-ului

- `docs/` — documentație și cerințe
- `src/` — codul sursă al proiectului

## Surse

- [Wikipedia](https://wikipedia.org) — definiții și formule
- [Shinylive](https://shinylive.io/r/examples/) — elemente UI pentru Shiny
- [Statistics How To](https://www.statisticshowto.com/probability-and-statistics/statistics-definitions/pearsons-coefficient-of-skewness/) — Coeficientul lui Pearson
- [Cool Text](https://cooltext.com/) — generare imagini pentru credite
