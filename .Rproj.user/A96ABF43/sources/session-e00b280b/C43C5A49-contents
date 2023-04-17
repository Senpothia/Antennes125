# Etude avec l'ensemble des mesures sur 3 cartes et neutres

library(ggplot2)
library(insight)

F0<-100000   # fréquence de référence

dataSource<-"mesures.csv"

TAB<-read.table(dataSource,header=TRUE,sep=";",dec=",")

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
  +geom_line(data=donnees1,aes(x=N,y=B1,colour="L1"))
  +geom_line(data=donnees2,aes(x=N,y=B2,colour="L2"))
  +geom_line(data=donnees3,aes(x=N,y=B3,color="L3"))
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
donnees<-data.frame(N, I0, I1, I2, I3)

p<-(
  ggplot(donnees, color=c("I1", "I2", "I3"))
  +geom_line(data=donnees,aes(x=N,y=I1, colour="I1"))
  +geom_line(data=donnees,aes(x=N,y=I2,colour="I2"))
  +geom_line(data=donnees,aes(x=N,y=I3, ,color="I3"))
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

jpeg("tensions.jpg")
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
 
 
 jpeg("capas.jpg")
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
jpeg("FreqC980pf.jpeg")
 curve(Fres1_C,40, 120, col="magenta", main="Fréquence d'accord estimée - carte 1 / C=980pF avant correction",  xlab="Tours",
       ylab="Fréquence (Hz)")
 abline(h = 125000, col="red")
 abline(v=n[1], col="red")
 dev.off()
 
 #-----------------------------------------------------------------------------------------------------------------------------
 # Facteur de correction
 
 COR<-read.table("correction2.csv",header=TRUE,sep=";",dec=",")
 
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
 
 
 # Correction des paramètres
 
 print("-----  ajustement 9  N=68 ------------")
 
 mod9 <- lm(CORN68$L~CORN68$F+I(CORN68$F^2), data=CORN68)
 
 print(summary(mod9))
 
 LN68Est<-function(F){mod9$coefficients[1]  + mod9$coefficients[2] * F +   mod9$coefficients[3] * F^2}
 p<-(ggplot(CORN68) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN68Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=68") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
 )
 png("LN68.png")
 print(p)
 dev.off()
 
 print("-----  ajustement 10  N=73 ------------")
 
 mod10 <- lm(CORN73$L~CORN73$F+I(CORN73$F^2), data=CORN73)
 
 print(summary(mod10))
 
 LN73Est<-function(F){mod10$coefficients[1]  + mod10$coefficients[2] * F +   mod10$coefficients[3] * F^2}
 p<-(ggplot(CORN73) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN73Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=73") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
 )
 png("LN73.png")
 print(p)
 dev.off()
 
 print("-----  ajustement 11  N=90 ------------")
 
 mod11 <- lm(CORN90$L~CORN90$F+I(CORN90$F^2), data=CORN90)
 
 print(summary(mod11))
 
 LN90Est<-function(F){mod11$coefficients[1]  + mod11$coefficients[2] * F +   mod11$coefficients[3] * F^2}
 p<-(ggplot(CORN90) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN90Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=90") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
 )
 png("LN90.png")
 print(p)
 dev.off()
 
 print("-----  ajustement 12  N=120 ------------")
 
 mod12 <- lm(CORN120$L~CORN120$F+I(CORN120$F^2), data=CORN120)
 
 print(summary(mod11))
 
 LN120Est<-function(F){mod12$coefficients[1]  + mod12$coefficients[2] * F +   mod12$coefficients[3] * F^2}
 p<-(ggplot(CORN120) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = LN120Est, color = "red") +  ggtitle("Inductance vs Fréquence - N=120") 
     # +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
 )
 png("LN120.png")
 print(p)
 dev.off()
 
 # Représentation conjointes de toutes les courbes de dépendance à la fréquence
 
 
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
 
 png("dependanceF.png")
 print(p)
 dev.off()
 
 #--------------------------------------------------------------------------------------------------------
 
 # Estimation des coefficients de 
 coef68<-mod9$coefficients
 coef90<-mod11$coefficients
 coef120<-mod12$coefficients
 
 coefs1<-data.frame(N=c(68, 90, 120), coef=c(coef68[1], coef90[1], coef120[1]))
 coefs2<-data.frame(N=c(68, 90, 120), coef=c(coef68[2], coef90[2], coef120[2]))
 coefs3<-data.frame(N=c(68, 90, 120), coef=c(coef68[3], coef90[3], coef120[3]))
 
 lm1 <- lm(coef~N, data=coefs1)
 print("Paramètres de correction")
 print(lm1$coefficients)
 plot(x=coefs1$N, y=coefs1$coef)
 abline(lm1, col="blue")
 
 
 lm2 <- lm(coef~N, data=coefs2)
 print("Paramètres de correction")
 print(lm2$coefficients)
 plot(x=coefs2$N, y=coefs2$coef)
 abline(lm2, col="red")
 
 
 lm3 <- lm(coef~N, data=coefs3)
 print("Paramètres de correction")
 print(lm3$coefficients)
 plot(x=coefs3$N, y=coefs3$coef)
 abline(lm3, col="magenta")
 
 inter<-function(N){
   
   lm1$coefficients[1] + N*lm1$coefficients[2]
 }
 
 coefN<-function(N){
   
   lm2$coefficients[1] + N*lm2$coefficients[2]
 }
 
 coefN2<-function(N){
   
   lm3$coefficients[1] + N*lm3$coefficients[2]
 }
 
 Estimateur<-function(F){
   
    
   return(inter(80)  + coefN(80) * F +   coefN2(80) * F^2)
   
 }
 
 Estimateur2<-function(N, F){
   
   return(inter(N)  + coefN(N) * F +   coefN2(N) * F^2)
   
 }
 
 
 
 
 #--------------------------------------------------------------------------------------------------------
 
 # Estimation de la courbe d'inductance pour N=80
 
 F <- seq(10,100,by=1)
 L68<-LN68Est(F)
 L73<-LN73Est(F)
 L90<-LN90Est(F)
 L120<-LN120Est(F)
 L80<-Estimateur(F)  # simulation pour N=80
 
 
 
 donnees68 <- data.frame(F,L68)
 donnees73 <- data.frame(F,L73)
 donnees90 <- data.frame(F,L90)
 donnees120 <- data.frame(F,L120)
 donnees80 <- data.frame(F,L80)
 
 donnees<-data.frame(F, L68, L73, L90, L120, L80)
 
 
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
 
 png("simulN80vsFreq.png")
 print(p)
 dev.off()
 
 
 #--------------------------------------------------------------------------------------------------------
 
 # Estimation de la courbe d'inductance en fonction de la fréquence à 125kHz
 
 
 tours<-seq(60, 120, by=1)
 F<-rep(125,61 )
 Ind125k<-Estimateur2(tours, F)
 plot(tours, Ind125k, type="l", col="blue")
 
 
 
 
 #---------------------------------------------------------------------------------------------------------
 
 # Estimation en fonction de la capacité d'accord
 
 cap <- readline(prompt="Entrée la valeur de la capacité d'accord en pF: ")
 # convert character into integer
 cap2 <- as.integer(cap)*1e-12
 
 print("Valeur de capacité entrée: ")
 print(cap2)
 
 c2<-(39e-12*1.5e-9)/(39e-12+1.5e-9)
 
 Cacc<-(cap2)+c2
 
 Lattendue<- function(C) {
   
   return(1e3/ (2*pi*125000)^2/C)
   
 } 
 
 print("Capacité d'accord réelle: ")
 print(Lattendue(Cacc))
 

 print("Inductance attendue en mH: ")
 print(Lattendue(Cacc))
 
 # Optimisation nbre de spires
 tours<-seq(60,120, by=1)
 lm4 <- lm(Ind125k~tours, data=data.frame(tours, Ind125k))
 
 print("-------   Ajustement lm4   -----------")
 print(lm4)
 
 spires<-function(N){
   
   return(abs(lm4$coefficients[1] + N*lm4$coefficients[2]- Lattendue(Cacc)))
   
 }
 
 n<-optimize(spires, c(40, 120) )
 
 #--------------------------------------------------------------------------------
 
 # Compte rendu 
 
 print_color("-------   RESULTAT D'OPTIMISATION    -----------\n", "red")
 print_color("\n","red")
 print_color(paste("Optimisation pour F=125kHz et C=", cap), "red")
 print_color("\n","red")
 print_color( paste("Jeu de données utilisé: ", dataSource), "red")
 print_color("\n","red")
 print_color("\n","red")
 print_color( paste("Nombre de spires estimés: ", as.character(n[1])), "red")
 print_color("\n","red")
 print_color("------------------------------------------------\n", "red")
 
 

  
  
 
 
 

 
 