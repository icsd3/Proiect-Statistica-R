# Fie probabilitatea ca un acces sa fie dubios sa fie p = 0.01
# Putem considera ca un nr total de accesuri per zi urmaresc
# repartitia Poisson, deoarece modelul nostru matematic se bazeaza
# pe evenimente ce se intampla intr-un interval fix(acces la resursa)

# Sa consideram lambda = 1000, un avg. de accesuri per zi.
# Fiecare zi are un total n de accesuri
# Accese per zi ~ Poisson(365, lambda)

# 
#

set.seed(87)

lambda  <- 1000 # PARAMETRIZABIL
p       <- 0.01 # PARAMETRIZABIL
#p      <- 0.05
#p      <- 0.2
nr_zile <- 365

total_cereri <- rpois(nr_zile, lambda)

# Sa zicem ca nr de accese sus sunt ~ Binom

cereri_sus <- rbinom(
  n = nr_zile,
  size = total_cereri,
  prob = p
)

date <- data.frame(
  zi = 1:nr_zile,
  total = total_cereri,
  normale = total_cereri - cereri_sus,
  sus = cereri_sus
)


# STRATEGII VERIFICARE

# VERIFICARE SIMPLA

# cereri per zi, 10%
# ziua 1 100 (5 sus) -> 10
# ziua 2 123 -> 23

# Pt a verifica extragem 10% din acel total per zi
# sample?
proc_verificare <- 10 / 100 #PARAMETRIZABIL
date$verificate <- floor(date$total * proc_verificare)
#sample(date$total, size = as.integer(date$total * proc_verificare), 
#       replace = FALSE,
#       p = date$sus / date$total # nr caz fav / total
#      )

# VERIFICARE ADAPTIVA

# dupa lambda, daca per zi sunt mai multe cereri decat baseline-ul
# average de lambda = 1000, adaptam procentul dinamic

# Consideram ca pe langa un baseline de 20% de cereri verificate
# mai adaugam procente dupa cat de mult depaseste sau scade sub valoarea medie
# deja stiuta, adica lambda = 1000.
# proc_adapt <- 20 / 100 + (date$total - lambda)/lambda
# date$verificate <- floor(date$total * proc_adapt)
proc_adapt <- 20 / 100 + abs(date$total - lambda) * 80/100
date$verificate <- floor(date$total * proc_adapt / 100)


#alta idee, varianta in numarator
proc_adapt <- (20 +  abs(date$total - lambda) * 0.4) / 100
date$verificate <- floor(date$total * proc_adapt)
# TO:DO e ft shit strategia, tb sa o facem cumva sa fie mai agresiva pe spike-uri.


# ===========RULAT INDIFERENT DE TIPUL DE VERIFICARE=============


# hypergeom
# Repartitia hypergeometrica are
#ca parametrii N total, N1 de normale,
#N2 cele sus, K cate verificam actually

date$detectate <- rhyper(
  nn <- nr_zile,
  m = date$sus, # N2
  n = date$normale, #N1
  k = date$verificate # actual nr verificate
)

date$nedetectate <- date$sus - date$detectate






# Cerinta 5. Pt. fiecare strategie se calculeaza
# a)        - probabilitate empirica de a detecta cel putin o cerere sus/per zi
# b)        - proportie medie de cereri suspecte detectate
# c)        - proportie medie de cereri suspecte nedetectate
# d)        - nr mediu de verificari efectuate zilnic
# e)        - un indicator de eficienta, definit de NOI
#               EIGRP metric??????????

# a)
vector_logic <- date$detectate > 0
prob_empiric <- sum(vector_logic) / length(vector_logic)
# alternativ mean(date$detectate > 0)

# b)
# consideram proportia per total, maybe considerat si pt detectate/suspecte

#mean(date$detectate / date$total)
mean(date$detectate / date$sus)
# in proportie detectezi din 10 suspicioase doar una.

# c)
mean(date$nedetectate / date$sus)

# d)
mean(date$verificate)

# e)
mean(date$detectate) / mean(date$verificate) * 100
# metrica tris
# Alright sprinters we'll put a pin on that!!!!!!


# Cerinta 6. Vizualizari
# a)        - histograma nr de cereri sus per zi (frecventa cereri sus)
hist(date$sus, 
     col = "skyblue",
     border = "black",
     main = "Histograma date sus",
     xlab = "Nr. sus",
     ylab = "Frecventa",
     breaks = 10
     )

# b)        - histograma nr de cereri sus detectate per zi

hist(date$detectate, 
     col = "salmon",
     border = "black",
     main = "Histograma cereri sus detectate per zi",
     xlab = "Nr. cereri sus detectate",
     ylab = "Frecventa",
     breaks = 10
     )

# c)        - Graf comparativ intre strategii de verificare

# to do


# d)        - Evolutia zilnica a nr de cereri sus si cereri detectate

#plot zile / nr de cereri
plot(date$zi, date$sus, type = "l", col = "red", lwd = 2,
     main = "Evolutia zilnica a cererilor sus si detectate",
     xlab = "Ziua",
     ylab = "Nr. cereri",
     ylim = c(0, max(date$sus))
     )
lines(date$zi, date$detectate, col = "blue", lwd = 2)
legend("topright", legend = c("Cereri Sus", "Cereri Detectate"), col = c("red", "blue"), lwd = 2)

#plot evolutie cereri
plot(date$zi, cumsum(date$sus), type = "l", col = "red", lwd = 2,
     main = "Evolutia cumulativa a cererilor sus si detectate",
     xlab = "Ziua",
     ylab = "Nr. cereri",
     ylim = c(0, max(cumsum(date$sus)))
     )
lines(date$zi, cumsum(date$detectate), col = "blue", lwd = 2)
legend("topright", legend = c("Cereri Sus", "Cereri Detectate"), col = c("red", "blue"), lwd = 2)

# Cerinta 7. Simulare!

val_sim <- vector("list", nr_zile)

for(i in 1:nr_zile) {
  val_sim[[i]] <- numeric(date$normale[i])
  vector_curent <- val_sim[[i]]
  indici_aleatori <- sample(1:date$normale[i], date$sus[i])
  vector_curent[indici_aleatori] <- 1
  val_sim[[i]] <- vector_curent
}

Calculare_Procent <- function(procent) {
  procent <- procent/100
  prob <- vector("numeric", nr_zile)
  for(i in 1:nr_zile){
    max_range <- date$normale[i]
    num_indexes <- round(date$normale[i]*procent, 1)
    distinct_indexes <- sample(1:max_range, num_indexes)
    prob[i] <- sum(val_sim[[i]][distinct_indexes])/date$sus[i]*100
  }
  return(mean(prob))
}

Repetare_Detect_Sus <- function(numar){
  Medie_y <- c(0,0,0,0,0)
  for(i in 1:numar){
    Medie_y <- Medie_y + c(Calculare_Procent(1), Calculare_Procent(5), Calculare_Procent(10), Calculare_Procent(20), Calculare_Procent(30))
  }
  Medie_y <- Medie_y / numar
  return(Medie_y)
}

x <- c(1,5,10,20,30)
y <- c(Calculare_Procent(1), Calculare_Procent(5), Calculare_Procent(10), Calculare_Procent(20), Calculare_Procent(30))

plot(x, y, type = "b", col = "blue", pch = 16, lwd = 2,
  main = "Procent sus detectate din sus total (media la toate zilele)",
  xlab = "Procent Verificate", 
  ylab = "Procent Sus Detectate"
)
y_1000 <- Repetare_Detect_Sus(100)

# Se observa cum prin incercare aleatorie procentul de cereri suspicioase detectate creste linear cu
# procentul de date verificate dintre cele totale DECI PROBABILITATEA DE DETECTIE SE MODIFICA LINEAR


