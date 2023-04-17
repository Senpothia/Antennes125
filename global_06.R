# Etude avec l'ensemble des mesures sur 3 cartes et neutres

library(ggplot2)

F0<-100000   # fréquence de référence

TAB<-read.table("mesures.csv",header=TRUE,sep=";",dec=",")

#-------------------------------------------------------------------------------------------------------------------------------------

# ESTIMATION DES INDUCTANCES vs TOURS


# Ajustement 1 - Neutre

print("-----  ajustement 1   ------------")

mod1 <- lm(TAB$Ln~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod1))

LnEst<-function(N){mod1$coefficients[1]  + mod1$coefficients[2] * N +   mod1$coefficients[3] * N^2}

p<-(ggplot(TAB) + geom_point(aes(x = N, y = Ln), colour = "#4271AE")  + stat_function(fun = LnEst, color = "red") +  ggtitle("Inductance / tours - neutre") 
      #+  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
      )
png("L0.png")
print(p)
dev.off()

# Ajustement 2 - carte 1

print("-----  ajustement 2   ------------")

mod2 <- lm(TAB$L1~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod2))

L1Est<-function(N){mod2$coefficients[1]  + mod2$coefficients[2] * N +   mod2$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = L1), colour = "#4271AE")  + stat_function(fun = L1Est, color = "red") +  ggtitle("Inductance / tours - carte 1") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)
png("L1.png")
print(p)
dev.off()


# Ajustement 3 - carte 2

print("-----  ajustement 3   ------------")

mod3 <- lm(TAB$L2~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod3))

L2Est<-function(N){mod3$coefficients[1]  + mod3$coefficients[2] * N +   mod3$coefficients[3] * N^2}
p<-(ggplot(TAB) + geom_point(aes(x = N, y = L2), colour = "#4271AE")  + stat_function(fun = L2Est, color = "red") +  ggtitle("Inductance / tours - carte 2") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)
png("L2.png")
print(p)
dev.off()

# Ajustement 4 - carte 3

print("-----  ajustement 4   ------------")

mod4 <- lm(TAB$L3~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod4))

L3Est<-function(N){mod4$coefficients[1]  + mod4$coefficients[2] * N +   mod4$coefficients[3] * N^2}

p<-(ggplot(TAB) + geom_point(aes(x = N, y = L3), colour = "#4271AE")  + stat_function(fun = L3Est, color = "red") +  ggtitle("Inductance / tours - carte 3") 
    #  +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)
png("L3.png")
print(p)
dev.off()

# Représentation globale

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
     
png("global.png")
print(p)
dev.off()

#-------------------------------------------------------------------------------------------------------------------------------

# ETUDE DU COURANT D'ANTENNE

# Courant d'antenne théorique

 vdd<-5
 Vss<-0
 Rser=27
 Rad=9

 Iant<-function(R){

   I<-(4/pi)*((vdd-Vss)/(R+Rser+2*Rad))*1000
   return(I)

 }

jpeg("Ith.jpg")

   curve(Iant, 0, 500, col="red", main="Courant théorique antenne Vs Résistance antenne",  xlab="Résistance(Ohms)",
         ylab="Iant(mA)")
dev.off()

#---------------------------------------------------------------------------------------------------------------------------------------

# ESTIMATION DES RESISTANCES VS TOURS


# Estimation Rant

# Ajustement 5 - Neutre
# Résistance et courant d'antenne sur neutre

 print("-----  ajustement 5   ------------")

 mod5 <- lm(TAB$Rn~TAB$N+I(TAB$N^2), data=TAB)

 print(summary(mod5))

 R0Est<-function(N){mod5$coefficients[1]  + mod5$coefficients[2] * N +   mod5$coefficients[3] * N^2}
 print(ggplot(TAB) + geom_point(aes(x = N, y = Rn), colour = "#4271AE")  + stat_function(fun = R0Est, color = "red") +  ggtitle("Résistance ant / tours - Neutre")

 )

 I0n<-function(N){

     I<-Iant(R0Est(N))
     return(I)

 }


jpeg("I0.jpg")

   curve(I0n, 0, 120, col="red", main="Courant estimé antenne Vs tours - Neutre",  xlab="Tours",
         ylab="Iant(mA)")
dev.off()

 # Ajustement 6 - Carte 1
 
 # Résistance et courant d'antenne sur carte 1

 print("-----  ajustement 6   ------------")
 
 mod6 <- lm(TAB$R1~TAB$N+I(TAB$N^2), data=TAB)

 print(summary(mod6))

 R1Est<-function(N){mod6$coefficients[1]  + mod6$coefficients[2] * N +   mod6$coefficients[3] * N^2}
 print(ggplot(TAB) + geom_point(aes(x = N, y = R1), colour = "#4271AE")  + stat_function(fun = R1Est, color = "red") +  ggtitle("Résistance ant / tours - Carte 1")

 )

 I1n<-function(N){

   I<-Iant(R1Est(N))
   return(I)

 }


 jpeg("I1.jpg")

   curve(I1n, 0, 120, col="red", main="Courant estimé antenne Vs tours - Carte 1",  xlab="Tours",
         ylab="Iant(mA)") 
 dev.off()


  # Ajustement 7 - Carte 2
  
  # Résistance et courant d'antenne sur carte 2

 print("-----  ajustement 7   ------------")
 
 mod7 <- lm(TAB$R2~TAB$N+I(TAB$N^2), data=TAB)

 print(summary(mod7))

 R2Est<-function(N){mod7$coefficients[1]  + mod7$coefficients[2] * N +   mod7$coefficients[3] * N^2}
 print(ggplot(TAB) + geom_point(aes(x = N, y = R2), colour = "#4271AE")  + stat_function(fun = R2Est, color = "red") +  ggtitle("Résistance ant / tours - carte 2")

 )

 I2n<-function(N){

   I<-Iant(R2Est(N))
   return(I)

 }


 jpeg("I2.jpg")

   curve(I2n, 0, 120, col="red", main="Courant estimé antenne Vs tours - carte 2",  xlab="Tours",
         ylab="Iant(mA)")
dev.off()

 # Ajustement 8 - Carte 3
 
 # Résistance et courant d'antenne sur carte 3
 
 print("-----  ajustement 8   ------------")
 
 mod8 <- lm(TAB$R3~TAB$N+I(TAB$N^2), data=TAB)
 
 print(summary(mod8))
 
 R3Est<-function(N){mod8$coefficients[1]  + mod8$coefficients[2] * N +   mod8$coefficients[3] * N^2}
 print(ggplot(TAB) + geom_point(aes(x = N, y = R3), colour = "#4271AE")  + stat_function(fun = R3Est, color = "red") +  ggtitle("Résistance ant / tours - carte 3")
       
 )
 
 I3n<-function(N){
   
   I<-Iant(R3Est(N))
   return(I)
   
 }
 
 
 jpeg("I3.jpg")
   
   curve(I3n, 0, 120, col="red", main="Courant estimé antenne Vs tours - carte 3",  xlab="Tours",
         ylab="Iant(mA)")
dev.off()



# -------------------------------------------------------------------------------------------------------------------------- 
 
# Etimation du champ magnétique

 z<-1e-2 # distance de référence 1cm
 r<-0.38 # rayon de l'antenne
 
 
 B0est<-function(N){
   
   b<-I0n(N) *r^2/z^3
   return(b)
   
   
 }

 B1est<-function(N){
   
   b<-I1n(N)*N *r^2/z^3
   return(b)
   
   
 }
 
 B2est<-function(N){
   
   b<-I2n(N)*N *r^2/z^3
   return(b)
   
   
 }
 
 B3est<-function(N){
   
   b<-I3n(N)*N *r^2/z^3
   return(b)
   
   
 }

 # ---------------------------------------------------------------------------------------------------------------------------
 
 # Représentation du champ par carte 

jpeg("champ0.jpg")
   curve(B0est, 0, 120, col="red", main="Champ estimé Vs tours - neutre",  xlab="Tours",
         ylab="Champ")
 
dev.off()

 
jpeg("champ1.jpg")
   
   curve(B1est, 0, 120, col="red", main="Champ estimé Vs tours - carte 1",  xlab="Tours",
         ylab="Champ")
 
dev.off()
 
 
jpeg("champ2.jpg")
   
   curve(B2est, 0, 120, col="red", main="Champ estimé Vs tours - carte 2",  xlab="Tours",
         ylab="Champ")
   
dev.off()
 
 
jpeg("champ3.jpg")
   
   curve(B3est, 0, 120, col="red", main="Champ estimé Vs tours - carte 3",  xlab="Tours",
         ylab="Champ")
   
dev.off()

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
  +geom_line(data=donnees1,aes(x=N,y=B1),color="blue")
  +geom_line(data=donnees2,aes(x=N,y=B2),color="green")
  +geom_line(data=donnees3,aes(x=N,y=B3),color="magenta")
  +  ggtitle("Champs/ tours - global")
  + labs(x = "N - Tours",
       y = "Champ",
       color="Cartes"
) 
)
png("champs.png")
print(p)
dev.off()

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

p<-(
  ggplot(donnees1, color=c("L1", "L2", "L3"))
  +geom_line(data=donnees1,aes(x=N,y=I1),color="blue")
  +geom_line(data=donnees2,aes(x=N,y=I2),color="green")
  +geom_line(data=donnees3,aes(x=N,y=I3),color="magenta")
  +  ggtitle("Courantps/ tours - global")
  + labs(x = "N - Tours",
         y = "Champ",
         color="Cartes"
  ) 
)
png("courants.png")
print(p)
dev.off()

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

# Estimation tension d'antenne 

Cres<-function(N, L){
  
  L<-eval(L)
  c<-1/((2*pi*F0)^2*L(N))^-1
  
}

Cres1<-function(N){
  
  L<-L1Est(N)*1e-03
  c<-((2*pi*F0)^2*L)^-1
  
}

Cres2<-function(N){
  
  L<-L2Est(N)*1e-03
  c<-((2*pi*F0)^2*L)^-1
  
}

Cres3<-function(N){
  
  L<-L3Est(N)*1e-03
  c<-((2*pi*F0)^2*L)^-1
  
}

Vant<-function(N, R, L){
  R<-eval(R)
  L<-eval(L)
  v<-Iant(R(N))/(2*pi*Fo*Cres) 
  
}

V1ant<-function(N){
  
  v<-I1n(N)*1e-03/(2*pi*F0*Cres1(N)) 
  
  return(v)
}


V2ant<-function(N){
  
  v<-I2n(N)*1e-03/(2*pi*F0*Cres2(N)) 
  
  return(v)
}

V3ant<-function(N){
  
  v<-I3n(N)*1e-03/(2*pi*F0*Cres3(N)) 
  
  return(v)
}


 curve(V1ant, 40, 120, col="red", main="Tension antenne estimée - carte 1",  xlab="Tours",
       ylab="Tension")
 
 
 curve(V2ant, 40, 120, col="blue", main="Tension antenne estimée - carte 2",  xlab="Tours",
       ylab="Tension", add=TRUE)

 
 curve(V3ant, 40, 120, col="magenta", main="Tension antenne estimée - carte 3",  xlab="Tours",
       ylab="Tension", add=TRUE)
 
 # -------------------------------------------------------------------------------------------------------------------------- 
 
 # Estimation de la capacité d'accord
 
 curve(Cres1, 40, 120, col="red", main="Capacité d'accord estimée - carte 1",  xlab="Tours",
       ylab="C (nF)")
 
 
 curve(Cres2, 40, 120, col="blue", main="Capacité d'accord estimée - carte 2",  xlab="Tours",
       ylab="C (nF)", add=TRUE)
 
 
 curve(Cres3, 40, 120, col="magenta", main="Capacité d'accord estimée - carte 3",  xlab="Tours",
       ylab="C (nF)", add=TRUE)
 
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
 
 #-----------------------------------------------------------------------------------------------------------------------------
 # Optimisation Freq / N
 
 Fmin<-function(N){
   
   return(abs(Fres1_C(N)-125000))
   
 }
 
 n<-optimize(Fmin, c(40, 120) )
 print("Optimisation Freq / N")
 print(n)
 
 
 #-----------------------------------------------------------------------------------------------------------------------------
 # Courbe Frequence vs tours
 
 curve(Fres1_C,40, 120, col="magenta", main="Fréquence d'accord estimée - carte 1 / C=980pF",  xlab="Tours",
       ylab="Fréquence (Hz)")
 abline(h = 125000, col="red")
 abline(v=n[1], col="red")
 
 
 
 
 
 
 