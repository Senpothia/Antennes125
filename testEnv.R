# Etude avec l'ensemble des mesures sur 3 cartes et neutres

library(ggplot2)
library(insight)
library(rlang)

#---------------------------------------------------------------------------------------------------------
# Constantes

makeEnv<-function(){
  
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
  )
  
  return(env)
  
}


makeEnv2<-function(){
  
  
  assign("ee", new.env(), parent.env())
  
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
  )
  
  return(env)
  
}



 # env<-env(
 #   
 #   F0=100000,   # fréquence de référence
 #   dataSource="./data/mesures.csv",
 #   vdd=5,
 #   Vss=0,
 #   Rser=27,
 #   Rad=9,
 #   z=1e-2, # distance de référence 1cm
 #   r=0.38, # rayon de l'antenne
 #   Cdv1=39e-12,
 #   Cdv2=1.5e-09,
 #   #c2=Cdv1*Cdv2/(Cdv1+Cdv2)     # Influence pont capacitif
 #   c2=(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif
 #   
 #   
 # )


# Constantes

# F0<-100000   # fréquence de référence
# dataSource<-"./data/mesures.csv"
# vdd<-5
# Vss<-0
# Rser=27
# Rad=9
# z<-1e-2 # distance de référence 1cm
# r<-0.38 # rayon de l'antenne
# Cdv1<-39e-12
# Cdv2<-1.5e-09
# c2<-Cdv1*Cdv2/(Cdv1+Cdv2)     # Influence pont capacitif
# #c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif


 
 a<-function(x){ x+env$vdd}
 
 
#  If you put it in the .onLoad function (not method), you'll have to use the assign function to ensure the environment gets created in your package namespace.
# 
# .onLoad <- function(libname, pkgname)
# {
#     # ...
#     assign("myPackageEnvironment", new.env(), parent.env())
#     # ...
# }
 
 # Extrait du code de test environnement de package
 # Inclusion des constantes dans le package R
 
 
 # a<-10
 # c<-1e-09
 # 
 # 
 # my_env <- new.env(parent = emptyenv())
 # my_env$b <- 20
 # 
 # get_b <- function() {
 #   my_env$b
 # }
 # 
 # set_b <- function(value) {
 #   old <- my_env$b
 #   my_env$b <- value
 #   invisible(old)
 # }
