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


# Load data file for measurements and factors corrections

getMeasures<-function(file, s=";", d=","){
 
  file<-paste("./data/",file, sep="")
  file<-paste(file,".csv", sep="")
  TAB<-read.table(file,header=TRUE,sep=s,dec=d)
  return(TAB)
  
}

# Enregistrement d'un graphe

saveGraphPng<-function(fileName, p){
  
  fileName<-paste("./plots/", fileName)
  fileName<-paste(fileName, ".png")
  png(fileName)
  print(p)
  dev.off()
}

# Calcul du courant d'antenne

Iant<-function(x){  # En milliampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(x+env$Rser+2*env$Rad))*1000
  return(I)
  
}


IantA<-function(R){  # En Ampères
  
  I<-(4/pi)*((env$vdd-env$Vss)/(R+env$Rser+2*env$Rad))
  return(I)
  
}


I0n<-function(N){
  
  I<-Iant(R0Est(N))
  return(I)
  
}

I1n<-function(N){
  
  I<-Iant(R1Est(N))
  return(I)
  
}

I2n<-function(N){
  
  I<-Iant(R2Est(N))
  return(I)
  
}

I3n<-function(N){
  
  I<-Iant(R3Est(N))
  return(I)
  
}


# Etimation du champ magnétique


B0est<-function(N){
  
  b<-I0n(N) *env$r^2/env$z^3
  return(b)
  
  
}

B1est<-function(N){
  
  b<-I1n(N)*N *env$r^2/env$z^3
  return(b)
  
  
}

B2est<-function(N){
  
  b<-I2n(N)*N *env$r^2/env$z^3
  return(b)
  
  
}

B3est<-function(N){
  
  b<-I3n(N)*N *env$r^2/env$z^3
  return(b)
  
  
}

# Estimation de la capacité de résonnance

Cres<-function(N, L){
  
  L<-eval(L)
  c<-1/((2*pi*en$F0)^2*L(N))^-1
  
}

Cres1<-function(N){
  
  L<-L1Est(N)*1e-03
  c<-((2*pi*env$F0)^2*L)^-1
  
}

Cres2<-function(N){
  
  L<-L2Est(N)*1e-03
  c<-((2*pi*env$F0)^2*L)^-1
  
}

Cres3<-function(N){
  
  L<-L3Est(N)*1e-03
  c<-((2*pi*env$F0)^2*L)^-1
  
}

Cresonnance<-function(L, F=125000){  # Fréquence de résonnance à 125kHz pour L en Henry 
  
  return(1/((2*pi*F)^2*L*1e-03) - env$c2)
}

# Estimation tension d'antenne 

Vant<-function(N, R, L){
  R<-eval(R)
  L<-eval(L)
  v<-Iant(R(N))/(2*pi*env$Fo*Cres) 
  
}

V1ant<-function(N){
  
  v<-I1n(N)*1e-03/(2*pi*env$F0*Cres1(N)) 
  
  return(v)
}


V2ant<-function(N){
  
  v<-I2n(N)*1e-03/(2*pi*env$F0*Cres2(N)) 
  
  return(v)
}

V3ant<-function(N){
  
  v<-I3n(N)*1e-03/(2*pi*env$F0*Cres3(N)) 
  
  return(v)
}


# Estimation fréquence d'accord

Fres<-function(L,C){
  
  f<-1/(2*pi*sqrt(L*1e-03*C*1e-12))
  return(f)
  
}

Fres1<-function(N,C){
  
  f<-1/(2*pi*sqrt(L1Est(N)*1e-03*C*1e-12))
  return(f)
  
}

Fres1_C<-function(N){
  
  f<-1/(2*pi*sqrt(L1Est(N)*1e-03*980*1e-12))
  return(f)
  
}

Fmin<-function(N){
  
  return(abs(Fres1_C(N)-125000))
  
}

# Correction des facteur de L=f(N)

inter<-function(N){
  
  lm1$coefficients[1] + N*lm1$coefficients[2]
}

coefN<-function(N){
  
  lm2$coefficients[1] + N*lm2$coefficients[2]
}

coefN2<-function(N){
  
  lm3$coefficients[1] + N*lm3$coefficients[2]
}

Estimateur80<-function(F){
  
  
  return(inter(80)  + coefN(80) * F +   coefN2(80) * F^2)
  
}

Estimateur100<-function(F){
  
  
  return(inter(100)  + coefN(100) * F +   coefN2(100) * F^2)
  
}

Estimateur70<-function(F){
  
  
  return(inter(70)  + coefN(70) * F +   coefN2(70) * F^2)
  
}


Estimateur2<-function(N, F){
  
  return(inter(N)  + coefN(N) * F +   coefN2(N) * F^2)
  
}

# Correction des facteur de R=f(N)

interR<-function(N){
  
  lm11$coefficients[1] + N*lm11$coefficients[2]
}

coefRN<-function(N){
  
  lm21$coefficients[1] + N*lm21$coefficients[2]
}

coefRN2<-function(N){
  
  lm31$coefficients[1] + N*lm31$coefficients[2]
}

EstimR80<-function(F){
  
  
  return(interR(80)  + coefRN(80) * F +   coefRN2(80) * F^2)
  
}

EstimR100<-function(F){
  
  
  return(interR(100)  + coefRN(100) * F +   coefRN2(100) * F^2)
  
}

EstimR70<-function(F){
  
  
  return(interR(70)  + coefRN(70) * F +   coefRN2(70) * F^2)
  
}


EstimR2<-function(N, F){
  
  return(interR(N)  + coefRN(N) * F +   coefRN2(N) * F^2)
  
}

Lattendue<- function(C) {
  
  return(1e3/ (2*pi*125000)^2/C)
  
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

# Acquisition des modèles d'après le jeu de données TAB

getModels<-function(TAB){
  
  MODSLN<-list()
  MODSRN<-list()
  
  MODSLF<-list()
  MODSRF<-list()
  
  frequencies<-unique(TAB$F)     # Listes des fréquences 
  echs<-unique(TAB$ech)
  nTypes<-unique(TAB$N)
  
  
  # pour F=f0 
  # 1- L=f(N)
  # 2- R=f(N)
  
  
  #pour N=no 
  # 1- L=g(F)
  # 2- R=g(F)
  
  
  #---------------------    Courbes paramétrées par F      ------------------------------
  
  # Inductance vs N paramétrée en F
  j<-1
  
  for(i in frequencies){
    
    
    VAL=TAB[TAB$F == i,]
    modL <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
    MODSLN[[j]]<-modL
    j<-j+1
    
  }
  
  # Résistance vs N paramétrée en F
  
  j<-1
  
  for(i in frequencies){
    
    
    VAL=TAB[TAB$F == i,]
    modR <- lm(VAL$R~VAL$N+I(VAL$N^2), data=VAL)
    MODSRN[[j]]<-modR
    j<-j+1
    
  }
  
  # ---------------    Courbes paramétrées par N   -------------------------------
  # Inductance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL=TAB[TAB$N == i,]
    modL <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
    MODSLF[[j]]<-modL
    j<-j+1
    
  }
  
  
  # Résistance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL=TAB[TAB$N == i,]
    modR <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)
    MODSRF[[j]]<-modR
    j<-j+1
    
  }
  
  MODS<-list(MODSLF, MODSRF, MODSLN, MODSRN)
  return(MODS)
  
}





#----------------------     FIN  FONCTIONS      ---------------------------------------------------------------------------

# ESTIMATION DES INDUCTANCES vs TOURS

TAB<-getMeasures("mesures")

# Ajustement 1 - Neutre

print("-----  ajustement 1   ------------")

mod1 <- lm(TAB$Ln~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod1))

LnEst<-function(N){mod1$coefficients[1]  + mod1$coefficients[2] * N +   mod1$coefficients[3] * N^2}

p<-(ggplot(TAB) + geom_point(aes(x = N, y = Ln), colour = "#4271AE")  + stat_function(fun = LnEst, color = "red") +  ggtitle("Inductance / tours - neutre") 
  
)



saveGraphPng("L0", p)

# Ajustement 2 - carte 1

print("-----  ajustement 2   ------------")

mod2 <- lm(TAB$L1~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod2))

L1Est<-function(N){mod2$coefficients[1]  + mod2$coefficients[2] * N +   mod2$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = L1), colour = "#4271AE")  + stat_function(fun = L1Est, color = "red") +  ggtitle("Inductance / tours - carte 1") 
   
)


saveGraphPng("L1", p)

# Ajustement 3 - carte 2

print("-----  ajustement 3   ------------")

mod3 <- lm(TAB$L2~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod3))

L2Est<-function(N){mod3$coefficients[1]  + mod3$coefficients[2] * N +   mod3$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = L2), colour = "#4271AE")  + stat_function(fun = L2Est, color = "red") +  ggtitle("Inductance / tours - carte 2") 
    
)

saveGraphPng("L2", p)


# Ajustement 4 - carte 3

print("-----  ajustement 4   ------------")

mod4 <- lm(TAB$L3~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod4))

L3Est<-function(N){mod4$coefficients[1]  + mod4$coefficients[2] * N +   mod4$coefficients[3] * N^2}

p<-(ggplot(TAB) + geom_point(aes(x = N, y = L3), colour = "#4271AE")  + stat_function(fun = L3Est, color = "red") +  ggtitle("Inductance / tours - carte 3") 
    #  +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)



saveGraphPng("L3", p)

# Représentation globale des inductances

p<- (ggplot(TAB, color=c("Ln", "L1", "L2", "L3")) 
     + geom_point(aes(x = N, y = Ln , colour = "neutre"))  + stat_function(fun = LnEst, color = "magenta")  
     + geom_point(aes(x = N, y = L1 ,  colour = "carte1"))  + stat_function(fun = L1Est, color = "red")
     + geom_point(aes(x = N, y = L2 , colour = "carte2"))  + stat_function(fun = L2Est, color = "green")
     + geom_point(aes(x = N, y = L3 , colour = "carte3"))  + stat_function(fun = L3Est, color = "blue")
     +  ggtitle("Inductance / tours - global")
     + labs(x = "N - Tours",
            y = "(Inductance - mH)",
            color="Cartes"
     ) 
)



saveGraphPng("global", p)

#-------------------------------------------------------------------------------------------------------------------------------

# ETUDE DU COURANT D'ANTENNE

# Courant d'antenne théorique


p<-plotFunction("Iant", 0, 500, "Courant théorique antenne Vs Résistance antenne", "Résistance(Ohms)", "Iant(mA)")
print(p)
saveGraphPng("Ith", p)

#---------------------------------------------------------------------------------------------------------------------------------------

# ESTIMATION DES RESISTANCES VS TOURS


# Estimation Rant

# Ajustement 5 - Neutre
# Résistance et courant d'antenne sur neutre

print("-----  ajustement 5   ------------")

mod5 <- lm(TAB$Rn~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod5))

R0Est<-function(N){mod5$coefficients[1]  + mod5$coefficients[2] * N +   mod5$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = Rn), colour = "#4271AE")  + stat_function(fun = R0Est, color = "red") +  ggtitle("Résistance ant / tours - Neutre"))

saveGraphPng("R0", p)


p<-plotFunction("I0n", 0, 120, "Courant estimé antenne Vs tours - Neutre", "Tours", "Iant(mA)")
print(p)
saveGraphPng("I0", p)

# Ajustement 6 - Carte 1

# Résistance et courant d'antenne sur carte 1

print("-----  ajustement 6   ------------")

mod6 <- lm(TAB$R1~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod6))

R1Est<-function(N){mod6$coefficients[1]  + mod6$coefficients[2] * N +   mod6$coefficients[3] * N^2}
p<-( ggplot(TAB) + geom_point(aes(x = N, y = R1), colour = "#4271AE")  + stat_function(fun = R1Est, color = "red") +  ggtitle("Résistance ant / tours - Carte 1"))


saveGraphPng("R1", p)


p<-plotFunction("I1n", 0, 120, "Courant estimé antenne Vs tours - Carte 1", "Tours", "Iant(mA)")
print(p)
saveGraphPng("I1", p)


# Ajustement 7 - Carte 2

# Résistance et courant d'antenne sur carte 2

print("-----  ajustement 7   ------------")

mod7 <- lm(TAB$R2~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod7))

R2Est<-function(N){mod7$coefficients[1]  + mod7$coefficients[2] * N +   mod7$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = R2), colour = "#4271AE")  + stat_function(fun = R2Est, color = "red") +  ggtitle("Résistance ant / tours - carte 2"))

saveGraphPng("R2", p)


p<-plotFunction("I2n", 0, 120, "Courant estimé antenne Vs tours - carte 2", "tours", "Iant(mA)")
print(p)
saveGraphPng("I2", p)

# Ajustement 8 - Carte 3

# Résistance et courant d'antenne sur carte 3

print("-----  ajustement 8   ------------")

mod8 <- lm(TAB$R3~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod8))

R3Est<-function(N){mod8$coefficients[1]  + mod8$coefficients[2] * N +   mod8$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = R3), colour = "#4271AE")  + stat_function(fun = R3Est, color = "red") +  ggtitle("Résistance ant / tours - carte 3"))

saveGraphPng("R3", p)


p<-plotFunction("I3n", 0, 120, "Courant estimé antenne Vs tours - carte 3", "Tours", "Iant(mA)" )
print(p)
saveGraphPng("I3", p)

#--------------------------------------------------------------------------------------------------------------------------

# Représentation globale des résistances

p<- (ggplot(TAB, color=c("neutre", "carte 1", "carte 2", "carte 3")) 
     + geom_point(aes(x = N, y = Rn , colour = "neutre"))  + stat_function(fun = R0Est, color = "magenta")  
     + geom_point(aes(x = N, y = R1 , colour = "carte1"))  + stat_function(fun = R1Est, color = "red")
     + geom_point(aes(x = N, y = R2 , colour = "carte2"))  + stat_function(fun = R2Est, color = "green")
     + geom_point(aes(x = N, y = R3 , colour = "carte3"))  + stat_function(fun = R3Est, color = "blue")
     +  ggtitle("Résistance / tours - global")
     + labs(x = "N - Tours",
            y = "Résistance (Ohms)",
            color="Cartes"
     ) 
)



saveGraphPng("globalR", p)


# ---------------------------------------------------------------------------------------------------------------------------

# Représentation du champ par carte 


p<-plotFunction("B0est", 0, 120, "Champ estimé Vs tours - neutre", "tours", "Champ")
print(p)
saveGraphPng("champ0", p)

p<-plotFunction("B1est", 0, 120, "Champ estimé Vs tours - carte 1", "Tours", "Champ" )
print(p)
saveGraphPng("champ1", p)

p<-plotFunction("B2est", 0, 120, "Champ estimé Vs tours - carte 2", "Tours", "Champ")
print(p)
saveGraphPng("champ2", p)

p<-plotFunction("B3est", 0, 120, "Champ estimé Vs tours - carte 3", "Tours", "Champ" )
print(p)
saveGraphPng("champ3", p)


# ---------------------------------------------------------------------------------------------------------------------------

# Représentation conjointes des champs par carte 

N <- seq(10,120,by=1)
B0<-B0est(N)
B1<-B1est(N)
B2<-B2est(N)
B3<-B3est(N)

donnees0 <- data.frame(N,B0)
donnees1 <- data.frame(N,B1)
donnees2 <- data.frame(N,B2)
donnees3 <- data.frame(N,B3)

p<-(
  ggplot(donnees1, color=c("L1", "L2", "L3"))
  +geom_line(data=donnees1,aes(x=N,y=B1,colour="carte 1"))
  +geom_line(data=donnees2,aes(x=N,y=B2,colour="carte 2"))
  +geom_line(data=donnees3,aes(x=N,y=B3,color="carte 3"))
  +  ggtitle("Champs/ tours - global")
  + labs(x = "N - Tours",
         y = "Champ",
         color="Cartes"
  ) 
)

saveGraphPng("champs", p)

# ---------------------------------------------------------------------------------------------------------------------------

# Représentation conjointes des courants par carte 

N <- seq(10,120,by=1)
I0<-I0n(N)
I1<-I1n(N)
I2<-I2n(N)
I3<-I3n(N)

donnees0 <- data.frame(N,I0)
donnees1 <- data.frame(N,I1)
donnees2 <- data.frame(N,I2)
donnees3 <- data.frame(N,I3)
donnees<-data.frame(N, I0, I1, I2, I3)

p<-(
  ggplot(donnees, color=c("I1", "I2", "I3"))
  +geom_line(data=donnees,aes(x=N,y=I1, colour="carte 1"))
  +geom_line(data=donnees,aes(x=N,y=I2, colour="carte 2"))
  +geom_line(data=donnees,aes(x=N,y=I3, colour="carte 3"))
  +  ggtitle("Courants/ tours - global")
  + labs(x = "N - Tours",
         y = "Champ",
         color="Cartes"
  ) 
)
saveGraphPng("courants", p)


# --------------------------------------------------------------------------------------------------------------------------------

# Optimisations

# Champs magnétiques
print("----------------------------------------------------")
maxChp1<-optimize(B1est, c(0, 120), maximum = TRUE)
print("Max champ carte 1: ")
print(maxChp1)
print("----------------------------------------------------")
maxChp2<-optimize(B2est, c(0, 120), maximum = TRUE)
print("Max champ carte 2: ")
print(maxChp2)
print("----------------------------------------------------")
maxChp3<-optimize(B3est, c(0, 120), maximum = TRUE)
print("Max champ carte 3: ")
print(maxChp3)

# -------------------------------------------------------------------------------------------------------------------------- 



jpeg("./plots/tensions.jpg")
curve(V1ant, 40, 120, col="red", main="Tension antenne estimée - carte 1",  xlab="Tours",
      ylab="Tension (V)")


curve(V2ant, 40, 120, col="blue", main="Tension antenne estimée - carte 2",  xlab="Tours",
      ylab="Tension (V)", add=TRUE)


curve(V3ant, 40, 120, col="magenta", main="Tension antenne estimée - carte 3",  xlab="Tours",
      ylab="Tension (V)", add=TRUE)
legend(x = "bottomright",          # Position
       legend = c("carte 1", "carte 2", "carte 3"),  # Legend texts
       lty = c(1, 1, 1),           # Line types
       col = c("red", "blue", "magenta"),           # Line colors
       lwd = 2) 
dev.off()

# -------------------------------------------------------------------------------------------------------------------------- 

# Estimation de la capacité d'accord


jpeg("./plots/capas.jpg")
curve(Cres1, 40, 120, col="red", main="Capacité d'accord estimée - carte 1",  xlab="Tours",
      ylab="C (nF)")


curve(Cres2, 40, 120, col="blue", main="Capacité d'accord estimée - carte 2",  xlab="Tours",
      ylab="C (nF)", add=TRUE)


curve(Cres3, 40, 120, col="magenta", main="Capacité d'accord estimée - carte 3",  xlab="Tours",
      ylab="C (nF)", add=TRUE)
legend(x = "topright",          # Position
       legend = c("carte 1", "carte 2", "carte 3"),  # Legend texts
       lty = c(1, 1, 1),           # Line types
       col = c("red", "blue", "magenta"),           # Line colors
       lwd = 2) 
dev.off()



#-----------------------------------------------------------------------------------------------------------------------------
# Optimisation Freq / N



n<-optimize(Fmin, c(40, 120) )
print("Optimisation Freq / N")
print(n)


#-----------------------------------------------------------------------------------------------------------------------------
# Courbe Frequence vs tours pour C fixée
jpeg("./plots/FreqC980pf.jpeg")
curve(Fres1_C,40, 120, col="magenta", main="Fréquence d'accord estimée - carte 1 / C=980pF avant correction",  xlab="Tours",
      ylab="Fréquence (Hz)")
abline(h = 125000, col="red")
abline(v=n[1], col="red")
dev.off()

#-----------------------------------------------------------------------------------------------------------------------------
# Facteur de correction

#COR<-read.table("./data/correction2.csv",header=TRUE,sep=";",dec=",")

COR<-getMeasures("correction2")

COR10<-COR[COR$F == 10.0, ]
COR20<-COR[COR$F == 20.0, ]
COR28<-COR[COR$F == 28.5, ]
COR40<-COR[COR$F == 40.0, ]
COR50<-COR[COR$F == 50.0, ]
COR66<-COR[COR$F == 66.6, ]
COR100<-COR[COR$F == 100.0, ]

CORN68<-COR[COR$N == 68, ]
CORN73<-COR[COR$N == 73, ]
CORN90<-COR[COR$N == 90, ]
CORN120<-COR[COR$N == 120, ]


# Correction des paramètres pour l'estimation des inductances

print("-----  ajustement 9  N=68 ------------")

mod9 <- lm(CORN68$L~CORN68$F+I(CORN68$F^2), data=CORN68)

print(summary(mod9))

LN68Est<-function(F){mod9$coefficients[1]  + mod9$coefficients[2] * F +   mod9$coefficients[3] * F^2}
p<-(ggplot(CORN68) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN68Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=68") 
  
)


saveGraphPng("LN68", p)

print("-----  ajustement 10  N=73 ------------")

mod10 <- lm(CORN73$L~CORN73$F+I(CORN73$F^2), data=CORN73)

print(summary(mod10))

LN73Est<-function(F){mod10$coefficients[1]  + mod10$coefficients[2] * F +   mod10$coefficients[3] * F^2}
p<-(ggplot(CORN73) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN73Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=73") 
 
)


saveGraphPng("LN73", p)

print("-----  ajustement 11  N=90 ------------")

mod11 <- lm(CORN90$L~CORN90$F+I(CORN90$F^2), data=CORN90)

print(summary(mod11))

LN90Est<-function(F){mod11$coefficients[1]  + mod11$coefficients[2] * F +   mod11$coefficients[3] * F^2}
p<-(ggplot(CORN90) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN90Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=90") 
 
)



saveGraphPng("LN90", p)

print("-----  ajustement 12  N=120 ------------")

mod12 <- lm(CORN120$L~CORN120$F+I(CORN120$F^2), data=CORN120)

print(summary(mod11))

LN120Est<-function(F){mod12$coefficients[1]  + mod12$coefficients[2] * F +   mod12$coefficients[3] * F^2}
p<-(ggplot(CORN120) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN120Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=120") 
  
)



saveGraphPng("LN120", p)

# Représentation conjointes de toutes les courbes de dépendance de l'inductance à la fréquence


F <- seq(10,100,by=1)
L68<-LN68Est(F)
L73<-LN73Est(F)
L90<-LN90Est(F)
L120<-LN120Est(F)


donnees68 <- data.frame(F,L68)
donnees73 <- data.frame(F,L73)
donnees90 <- data.frame(F,L90)
donnees120 <- data.frame(F,L120)

donnees<-data.frame(F, L68, L73, L90, L120)


p<-(
  ggplot(donnees68, color=c("L68", "L73", "L90", "L120"))
  +geom_line(data=donnees68,aes(x=F,y=L68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=L73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=L90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=L120, colour="N=120"))
  +  ggtitle("Iductance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Inductance (mH)",
         color="Spires"
  ) 
)



saveGraphPng("dependanceF", p)
#--------------------------------------------------------------------------------------------------------

# Estimation des coefficients des modèles de L=f(N)

coef68<-mod9$coefficients
coef90<-mod11$coefficients
coef120<-mod12$coefficients

coefs1<-data.frame(N=c(68, 90, 120), coef=c(coef68[1], coef90[1], coef120[1]))
coefs2<-data.frame(N=c(68, 90, 120), coef=c(coef68[2], coef90[2], coef120[2]))
coefs3<-data.frame(N=c(68, 90, 120), coef=c(coef68[3], coef90[3], coef120[3]))

lm1 <- lm(coef~N, data=coefs1)
print("Paramètres de correction")
print(lm1$coefficients)

jpeg("./plots/degre0.jpg")
plot(x=coefs1$N, main = "Estimation coefficient regression inductance vs Fréquence", sub = "Degré 0", y=coefs1$coef, xlab = "N", ylab = "Intercept")
abline(lm1, col="blue")
dev.off()


lm2 <- lm(coef~N, data=coefs2)
print("Paramètres de correction")
print(lm2$coefficients)

jpeg("./plots/degre1.jpg")
plot(x=coefs2$N,main = "Estimation coefficient regression inductance vs Fréquence", sub = "Degré 1", y=coefs2$coef, xlab = "N", ylab = "facteur degré 1")
abline(lm2, col="red")
dev.off()


lm3 <- lm(coef~N, data=coefs3)
print("Paramètres de correction")
print(lm3$coefficients)
jpeg("./plots/degre2.jpg")
plot(x=coefs3$N, y=coefs3$coef, main = "Estimation coefficient regression inductance vs Fréquence", sub = "Degré 2", xlab = "N", ylab = "facteur degré 2")
abline(lm3, col="magenta")
dev.off()



#--------------------------------------------------------------------------------------------------------
# Test des modèles de correction pour l'inductance

# Estimation de la courbe d'inductance pour N=80

F <- seq(10,100,by=1)
L68<-LN68Est(F)
L73<-LN73Est(F)
L90<-LN90Est(F)
L120<-LN120Est(F)
L80<-Estimateur80(F)  # simulation pour N=80
L100<-Estimateur100(F)  # simulation pour N=100
L70<-Estimateur70(F)  # simulation pour N=70



donnees68 <- data.frame(F,L68)
donnees73 <- data.frame(F,L73)
donnees90 <- data.frame(F,L90)
donnees120 <- data.frame(F,L120)
donnees80 <- data.frame(F,L80)
donnees100 <- data.frame(F,L80)
donnees70 <- data.frame(F,L70)

donnees<-data.frame(F, L68, L73, L90, L120, L80, L100, L70)


p<-(
  ggplot(donnees68, color=c("L68", "L73", "L90", "L120"))
  +geom_line(data=donnees68,aes(x=F,y=L68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=L73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=L90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=L120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=L80, colour="N=80"))
  +ggtitle("Inductance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Inductance (mH)",
         color="Spires"
  ) 
)



saveGraphPng("simulN80vsFreq", p)

p<-(
  ggplot(donnees68, color=c("L68", "L73", "L90", "L120"))
  +geom_line(data=donnees68,aes(x=F,y=L68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=L73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=L90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=L120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=L80, colour="N=80"))
  +geom_line(data=donnees100,aes(x=F,y=L100, colour="N=100"))
  +ggtitle("Inductance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Inductance (mH)",
         color="Spires"
  ) 
)



saveGraphPng("simulN100vsFreq", p)

p<-(
  ggplot(donnees68, color=c("L68", "L73", "L90", "L120"))
  +geom_line(data=donnees68,aes(x=F,y=L68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=L73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=L90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=L120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=L80, colour="N=80"))
  +geom_line(data=donnees100,aes(x=F,y=L100, colour="N=100"))
  +geom_line(data=donnees70,aes(x=F,y=L70, colour="N=70"))
  +ggtitle("Inductance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Inductance (mH)",
         color="Spires"
  ) 
)


saveGraphPng("simulN70vsFreq", p)

#--------------------------------------------------------------------------------------------------------

# Estimation de la courbe d'inductance en fonction de la fréquence à 125kHz


tours<-seq(60, 120, by=1)
F<-rep(125,61 )
Ind125k<-Estimateur2(tours, F)

jpeg("./plots/Ind125kN.jpg")
plot(tours, Ind125k, type="l", main = "Inductance vs N - F=125kHz", xlab = "N", ylab = "Inductance (mH)", col="blue")
dev.off()



#---------------------------------------------------------------------------------------------------------

# ETUDE DES RESISTANCES EN FONCTION DE LA FREQUENCE - ETUDE DES CORRECTIONS


# Correction des paramètres pour l'estimation des résistances

print("-----  ajustement 21  N=68 ------------")

mod21 <- lm(CORN68$R~CORN68$F+I(CORN68$F^2), data=CORN68)

print(summary(mod21))

RN68Est<-function(F){mod21$coefficients[1]  + mod21$coefficients[2] * F +   mod21$coefficients[3] * F^2}
p<-(ggplot(CORN68) + geom_point(aes(x = F, y = R), colour = "#4271AE")  + stat_function(fun = RN68Est, color = "red") +  ggtitle("Résistance vs Fréquence - N=68") 
   
)


saveGraphPng("RN68", p)

print("-----  ajustement 22  N=73 ------------")

mod22 <- lm(CORN73$R~CORN73$F+I(CORN73$F^2), data=CORN73)

print(summary(mod22))

RN73Est<-function(F){mod22$coefficients[1]  + mod22$coefficients[2] * F +   mod22$coefficients[3] * F^2}
p<-(ggplot(CORN73) + geom_point(aes(x = F, y = R), colour = "#4271AE")  + stat_function(fun = RN73Est, color = "red") +  ggtitle("Résistance vs Fréquence - N=73") 
    
)


saveGraphPng("RN73", p)

print("-----  ajustement 23  N=90 ------------")

mod23 <- lm(CORN90$R~CORN90$F+I(CORN90$F^2), data=CORN90)

print(summary(mod23))

RN90Est<-function(F){mod23$coefficients[1]  + mod23$coefficients[2] * F +   mod23$coefficients[3] * F^2}
p<-(ggplot(CORN90) + geom_point(aes(x = F, y = R), colour = "#4271AE")  + stat_function(fun = RN90Est, color = "red") +  ggtitle("Résistance vs Fréquence - N=90") 
  
)



saveGraphPng("RN90", p)

print("-----  ajustement 24  N=120 ------------")

mod24 <- lm(CORN120$R~CORN120$F+I(CORN120$F^2), data=CORN120)

print(summary(mod24))

RN120Est<-function(F){mod24$coefficients[1]  + mod24$coefficients[2] * F +   mod24$coefficients[3] * F^2}
p<-(ggplot(CORN120) + geom_point(aes(x = F, y = R), colour = "#4271AE")  + stat_function(fun = RN120Est, color = "red") +  ggtitle("Résistance vs Fréquence - N=120") 
  
)


saveGraphPng("RN120", p)



# Représentation conjointes de toutes les courbes de dépendance de résistance à la fréquence


F <- seq(10,100,by=1)
R68<-RN68Est(F)
R73<-RN73Est(F)
R90<-RN90Est(F)
R120<-RN120Est(F)


donnees68 <- data.frame(F,R68)
donnees73 <- data.frame(F,R73)
donnees90 <- data.frame(F,R90)
donnees120 <- data.frame(F,R120)

donnees<-data.frame(F, R68, R73, R90, R120)


p<-(
  ggplot(donnees68, color=c("R68", "R73", "R90", "R120"))
  +geom_line(data=donnees68,aes(x=F,y=R68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=R73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=R90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=R120, colour="N=120"))
  +  ggtitle("Résistance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Résistance (Ohms)",
         color="Spires"
  ) 
)



saveGraphPng("dependanceRvs", p)

#--------------------------------------------------------------------------------------------------------


# Estimation des coefficients des modèles de R=f(N)

coef68<-mod21$coefficients
coef90<-mod23$coefficients
coef120<-mod24$coefficients

coefs1<-data.frame(N=c(68, 90, 120), coef=c(coef68[1], coef90[1], coef120[1]))
coefs2<-data.frame(N=c(68, 90, 120), coef=c(coef68[2], coef90[2], coef120[2]))
coefs3<-data.frame(N=c(68, 90, 120), coef=c(coef68[3], coef90[3], coef120[3]))

lm11 <- lm(coef~N, data=coefs1)
print("Paramètres de correction")
print(lm1$coefficients)

jpeg("./plots/degre0R.jpg")
plot(x=coefs1$N, main = "Estimation coefficient regression résistance vs Fréquence", sub = "Degré 0", y=coefs1$coef, xlab = "N", ylab = "Intercept")
abline(lm11, col="blue")
dev.off()


lm21 <- lm(coef~N, data=coefs2)
print("Paramètres de correction")
print(lm2$coefficients)

jpeg("./plots/degre1R.jpg")
plot(x=coefs2$N,main = "Estimation coefficient regression résistance vs Fréquence", sub = "Degré 1", y=coefs2$coef, xlab = "N", ylab = "facteur degré 1")
abline(lm21, col="red")
dev.off()


lm31 <- lm(coef~N, data=coefs3)
print("Paramètres de correction")
print(lm3$coefficients)
jpeg("./plots/degre2R.jpg")
plot(x=coefs3$N, y=coefs3$coef, main = "Estimation coefficient regression résistance vs Fréquence", sub = "Degré 2", xlab = "N", ylab = "facteur degré 2")
abline(lm31, col="magenta")
dev.off()



#--------------------------------------------------------------------------------------------------------
# Test des modèles de correction pour la résistance

# Estimation de la courbe de résistance pour N=80

F <- seq(10,100,by=1)
R68<-RN68Est(F)
R73<-RN73Est(F)
R90<-RN90Est(F)
R120<-RN120Est(F)
R80<-EstimR80(F)  # simulation pour N=80
R100<-EstimR100(F)  # simulation pour N=100
R70<-EstimR70(F)  # simulation pour N=70



donnees68 <- data.frame(F,R68)
donnees73 <- data.frame(F,R73)
donnees90 <- data.frame(F,R90)
donnees120 <- data.frame(F,R120)
donnees80 <- data.frame(F,R80)
donnees100 <- data.frame(F,R80)
donnees70 <- data.frame(F,R70)

donnees<-data.frame(F, R68, R73, R90, R120, R80, R100, R70)


p<-(
  ggplot(donnees68, color=c("R68", "R73", "R90", "R120"))
  +geom_line(data=donnees68,aes(x=F,y=R68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=R73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=R90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=R120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=R80, colour="N=80"))
  +ggtitle("Résistance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Résistance (Ohms)",
         color="Spires"
  ) 
)



saveGraphPng("simulRN80vsFreq", p)

p<-(
  ggplot(donnees68, color=c("R68", "R73", "R90", "R120"))
  +geom_line(data=donnees68,aes(x=F,y=R68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=R73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=R90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=R120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=R80, colour="N=80"))
  +geom_line(data=donnees100,aes(x=F,y=R100, colour="N=100"))
  +ggtitle("Résistance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Résistance (Ohms)",
         color="Spires"
  ) 
)



saveGraphPng("simulRN100vsFreq", p)


p<-(
  ggplot(donnees68, color=c("R68", "R73", "R90", "R120"))
  +geom_line(data=donnees68,aes(x=F,y=R68, colour="N=68"))
  +geom_line(data=donnees73,aes(x=F,y=R73, colour="N=73"))
  +geom_line(data=donnees90,aes(x=F,y=R90, colour="N=90"))
  +geom_line(data=donnees120,aes(x=F,y=R120, colour="N=120"))
  +geom_line(data=donnees80,aes(x=F,y=R80, colour="N=80"))
  +geom_line(data=donnees100,aes(x=F,y=R100, colour="N=100"))
  +geom_line(data=donnees70,aes(x=F,y=R70, colour="N=70"))
  +ggtitle("Résistance vs Fréquence - paramétrage: N")
  + labs(x = "Fréquence(kHz)",
         y = "Résistance (Ohms)",
         color="Spires"
  ) 
)


saveGraphPng("simulRN70vsFreq", p)

# Optimisation nbre de spires
print("-------   Ajustement lm4 - inductance vs N pour 125kHz -----------")

tours<-seq(60,120, by=1)
lm4 <- lm(Ind125k~tours, data=data.frame(tours, Ind125k))

L125k<-function(N){
  
  return(lm4$coefficients[1] + lm4$coefficients[2]*N)
}

print(lm4)

#--------------------------------------------------------------------------------------------------------


# Estimation de la courbe de résistance en fonction de la fréquence à 125kHz


tours<-seq(60, 120, by=1)
F<-rep(125,61 )
R125k<-EstimR2(tours, F)

jpeg("./plots/R125kN.jpg")
plot(tours, R125k, type="l", main = "Résistance vs N - F=125kHz", xlab = "N", ylab = "Résistance (Ohms)", col="blue")
dev.off()



print("-------   Ajustement lm5 - Résistance vs N pour 125kHz -----------")

lm5 <- lm(R125k~tours, data=data.frame(tours, R125k))
print(lm5)

R125k<-function(N){
  
  return(lm5$coefficients[1] + lm5$coefficients[2]*N)
}

NforR125<-function(N){
  
  return(abs(R125k(N) - Rattendue))
  
}


spires<-function(N){  # Recherche de la valeur de N pour L donnée
  
  return(abs(lm4$coefficients[1] + N*lm4$coefficients[2]- Lattendue(Cacc)))
  
}

RforI<-function(R){  # Fonction d'optimisation (R pour Iant donné)
  
  return(abs(IantA(R) - Iantenne2))
  
}


#-------------------------------------------------------------------------------------------------------------

#--------------------  CyCLE DE TEST -------------------------------------------------------------------------

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
    
    
    Cacc<-(cap2)+env$c2
    
    print("Capacité d'accord réelle: ")
    print(Lattendue(Cacc))
    
    
    print("Inductance attendue en mH: ")
    print(Lattendue(Cacc))
    
    n<-optimize(spires, c(40, 120) )
    
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
    
    r<-optimize(RforI, c(0, 10000))
    message<-cat("Résistance estimée: ", as.character(r[1]), "\n")
    print_color(message, "red")
    print_color("------------------------------------------------\n", "red")
    
    Rattendue<-as.numeric(r[1])
    
    n<-optimize(NforR125, c(40, 120))
    
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
