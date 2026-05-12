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
# histograma date suspecte per an cu frecventele lor
hist(date$sus, 
     col = "skyblue",
     border = "black",
     main = "Histograma date sus",
     xlab = "Nr. sus",
     ylab = "Frecventa",
     breaks = 20
     )

# STRATEGII VERIFICARE

# VERIFICARE SIMPLA
# cereri per zi, 10%
# ziua 1 100 (5 sus) -> 10
# ziua 2 123 -> 23

# Pt a verifica extragem 10% din acel total per zi
# sample?
proc_verificare <- 10 / 100 #PARAMETRIZABIL

#sample(date$total, size = as.integer(date$total * proc_verificare), 
#       replace = FALSE,
#       p = date$sus / date$total # nr caz fav / total
#      )

# hypergeom

date$verificate <- floor(date$total * proc_verificare)
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

# VERIFICARE ADAPTIVA
# dupa lambda, daca per zi sunt mai multe cereri decat baseline-ul
# average de lambda = 1000, adaptam procentul dinamic

#TO:DO e o linie

# Cerinta 5. Pt. fiecare strategie se calculeaza
# a)        - probabilitate empirica de a detecta cel putin o cerere sus/per zi
# b)        - proportie medie de cereri suspecte detectate
# c)        - proportie medie de cereri suspecte nedetectate
# d)        - nr mediu de verificari efectuate zilnic
# e)        - un indicator de eficienta, definit de NOI
#               EIGRP metric??????????

# strategia simpla

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

