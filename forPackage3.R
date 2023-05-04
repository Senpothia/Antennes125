# Etude avec l'ensemble des mesures sur 3 cartes et neutres
# Fichier de préparation à la conception du package ant125

library(ggplot2)
library(insight)
library(rlang)

#---------------------------------------------------------------------------------------------------------
# Constantes

env<-new.env(parent = emptyenv())

env$F0<-100000   # fréquence de référence
env$dataSource="./data/mesures.csv"
env$vdd=5
env$Vss=0
env$Rser=27
env$Rad=9
env$z=1e-2 # distance de référence 1cm
env$r=0.38 # rayon de l'antenne
env$Cdv1=39e-12
env$Cdv2=1.5e-09
env$c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif

# ---------  GETTERS et SETTERS

# Change reference frequency

setDataSource<-function(source){
  
  env$dataSource<-source
}

setRefFrequency<-function(freq){
  
  env$F0<-freq
}

setCdv1<-function(c){
  
  env$Cdv1<-c
  updateC2()
  
}

setCdv2<-function(c){
  
  env$Cdv2<-c
  updateC2()
  
}

setVdd<-function(v){
  
  env$Vdd<-v
  
}

setVss<-function(v){
  
  env$Vss<-v
  
}

setRser<-function(r){
  
  env$Rser<-r
  
}

setRad<-function(r){
  
  env$Rad<-r
  
}

setZ<-function(z){
  
  env$z<-z
  
}

setR<-function(r){
  
  env$r<-r
  
}

updateC2<-function(){
  
  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2)    
}

getC2<-function(){
  
  env$c2=env$Cdv1*env$Cdv2/(env$Cdv1+env$Cdv2) 
  return(env$c2)
}

summaryEnv<-function(){
  
  print("--------------- CONSTANTES DE REFERENCES --------------------------------------------")
  print(paste("Fréquence de référence:", env$F0))  # fréquence de référence
  print(paste("Localisation des  données:", env$dataSource))
  print(paste("Tension d'alimentation Vdd:", env$vdd))
  print(paste("Référence de tension Vss:", env$Vss))
  print(paste("Résistance série Rser:",env$Rser))
  print(paste("Résistance de sortie driver antenne Rad:", env$Rad))
  print(paste("Distance de référence:", env$z)) # distance de référence 1cm
  print(paste("Rayon d'antenne:", env$r)) # rayon de l'antenne
  print(paste("Pont capacitif, Cdv1:",env$Cdv1))
  print(paste("Pont capacitif, Cdv2:",env$Cdv2))
  print(paste("Pont capacitif, capacité équivalente:",getC2())) # Influence pont capacitif
  print("-------------------------------------------------------------------------------------")
}

#----------------------  FONCTIONS   --------------------------------------------------------------------------------


# Enregistrement d'un graphe

saveGraphPng<-function(fileName, p){
  
  fileName<-paste("./plots/", fileName)
  fileName<-paste(fileName, ".png")
  png(fileName)
  print(p)
  dev.off()
}

# Calcul du courant d'antenne
# x: résistance d'antenne


Iant<-function(x){  # En milliampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(x+env$Rser+2*env$Rad))*1000
  return(I)
  
}


RforI<-function(I){
  rx<-function(x){abs(Iant(x) - I)}
  r<-optimize(rx, c(0, 10000))
  return (r)
}

# Calcul du courant d'antenne
# R: résistance d'antenne

IantA<-function(R){  # En Ampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(R+env$Rser+2*env$Rad))
  return(I)
  
}

# Evalue la courant d'antenne d'après l'estimateur de la résistance d'antenne
# N: nombre de spires
# Restimator: estimateur de résistance d'antenne. Expression sous forme de string
# obtenu à partir des coefficients de régression

In<-function(N, Restimator){
  
  RN<-parse(text=Restimator)
  R<-eval(RN)
  I<-Iant(R)
  return(I)
  
}

# Etimation du champ magnétique
# Evalue le champ magnétique en fonction du courant d'antenne
# I: courant estimé d'après In

Best<-function(I){
  
  b<-I*env$r^2/env$z^3
  return(b)
  
  
}

# Estimation de la capacité de résonnance

Cres<-function(N, L){
  
  L<-eval(L)
  c<-1/((2*pi*en$F0)^2*L(N))^-1
  
}

Cresonnance<-function(L, F){  # Fréquence de résonnance en kHz pour L en Henry 
  
  return(1/((2*pi*F*1000)^2*L*1e-03) - env$c2)
}

# Estimation tension d'antenne 

Vant<-function(N, R, L){
  R<-eval(R)
  L<-eval(L)
  v<-Iant(R(N))/(2*pi*env$Fo*Cres) 
  
}


# Estimation fréquence d'accord en foction des paramètres du circuit

Fres<-function(L,C){
  
  f<-1/(2*pi*sqrt(L*1e-03*C*1e-12))
  return(f)
  
}

FresN<-function(N,C){
  
  f<-1/(2*pi*sqrt(LEst(N)*1e-03*C*1e-12))
  return(f)
  
}

Fmin<-function(N, C, F){
  
  return(abs(FresN(N, C)- (F*1000)))
  
}

# Evalue l'inductance en mH pour la résonance à la fréquence F donnée en kHz et
# la capacité C donnée en pF

Lattendue<- function(C, F) {
  
  return(1e3/ (2*pi*F*1000)^2/C)
  
} 

plotFunction<-function(func, xm, xM, main, xlab, ylab){
  
  fonction<-parse(text=func)
  fonction<-eval(fonction)
  x <- seq(xm, xM, by=1)
  val <- data.frame(x = x, y = fonction(x))
  p<-(
    ggplot(data = val, aes(x = x, y = y))+#, color="red")) + 
      geom_line(color="red") + 
      scale_x_continuous(name = xlab) +
      scale_y_continuous(name = ylab) +
      ggtitle(main)
  )
  
}

#----------------------     FIN  FONCTIONS      ---------------------------------------------------------------------------

#--------------------  CyCLE DE TEST -------------------------------------------------------------------------
analyse<-function(){
  
  TAB<-getMeasures("data", ",", ".")
  CS<-regMods("data")
  
  cap<-""
  Iantenne<-""
  
  repeat{
    
    cap <- readline(prompt="Entrez la valeur de la capacité d'accord en pF: ")
    
    if(cap == "0"){ 
      
      break
      
    }else{
      
      
      # Estimation en fonction de la capacité d'accord
      
      
      # convert character into integer
      cap2 <- as.integer(cap)*1e-12
      
      print("Valeur de capacité entrée: ")
      print(cap2)
      
      # c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)  # Influence pont capacitif
      
      Cacc<-(cap2)+env$c2
      
      print("Capacité d'accord réelle: ")
      print(Lattendue(Cacc))
      
      print("Inductance attendue en mH: ")
      Latt<-Lattendue(Cacc)
      print(Latt)
      
      n<-optimisEst(CS[4], Latt, 125, c(60,120))
      
      #--------------------------------------------------------------------------------
      
      # Compte rendu 
      
      print_color("-------   RESULTAT D'OPTIMISATION    -----------\n", "red")
      print_color("\n","red")
      print_color(paste("Optimisation pour F=125kHz et C=", cap), "red")
      print_color("pF\n","red")
      print_color( paste("Jeu de données utilisé: ", env$dataSource), "red")
      print_color("\n","red")
      print_color("\n","red")
      print_color( paste("Nombre de spires estimés: ", as.character(n[1])), "red")
      print_color("\n","red")
      print_color("------------------------------------------------\n", "red")
      
      
      # Estimation en fonction du courant d'antenne
      
      Iantenne <- readline(prompt="Entrez la valeur du courant d'antenne en mA: ")
      
      if(Iantenne == "0"){ 
        
        break
        
      }
      
      # convert character into integer 
      Iantenne2 <- as.integer(Iantenne)*1e-3
      message<-cat("Valeur du courant d'antenne choisie: ", Iantenne, "mA", "\n")
      
      print_color(message, "red")
      
      r<-RforI(Iantenne2)
      
      message<-cat("Résistance estimée: ", as.character(r[1]), "\n")
      print_color(message, "red")
      print_color("------------------------------------------------\n", "red")
      
      Rattendue<-as.numeric(r[1])
      
      n<-optimisEst(CS[3], Rattendue, 125, c(60,120))
      
      print_color( paste("Nombre de spires estimés: ", as.character(n[1])), "red")
      print_color("\n","red")
      print_color("------------------------------------------------\n", "red")
      Lcal<-L125k(as.numeric(n[1]))
      print_color( paste("Inductance antenne: ", L125k(as.numeric(n[1]))), "red")
      print_color("\n","red")
      print_color( paste("Capacité d'accord: ", Cresonnance(Lcal)), "red")
      print_color("\n","red")
      print_color("-----------------  FIN DE RAPPORT  ------------\n", "red")
      
    }
    
  }
  
  print_color("FIN", "red")
  

}


