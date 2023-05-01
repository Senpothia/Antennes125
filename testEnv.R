# Etude avec l'ensemble des mesures sur 3 cartes et neutres

library(ggplot2)
library(insight)
library(rlang)

#---------------------------------------------------------------------------------------------------------
# Constantes

 env<-env(
   
   F0=100000,   # fréquence de référence
   dataSource="./data/mesures.csv",
   vdd=5,
   Vss=0,
   Rser=27,
   Rad=9,
   z=1e-2, # distance de référence 1cm
   r=0.38, # rayon de l'antenne
   Cdv1=39e-12,
   Cdv2=1.5e-09,
   #c2=Cdv1*Cdv2/(Cdv1+Cdv2)     # Influence pont capacitif
   c2=(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif
#   
 )

#env <- env(a = 1, b = "foo") 
 
 a<-function(x){ x+env$vdd}