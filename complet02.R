# Etude sur l'ensemble des mesures

library(ggplot2)

GLOB<-read.table("global2.csv",header=TRUE,sep=";",dec=",")

# Ajustement 1 - Inductance

print("-----  ajustement 1   ------------")
mod1 <- lm(GLOB$L~GLOB$N+I(GLOB$N^2), data=GLOB)

print(summary(mod1))

LEst<-function(N){mod1$coefficients[1]  + mod1$coefficients[2] * N +   mod1$coefficients[3] * N^2}

p<-(ggplot(GLOB) + geom_point(aes(x = N, y = L), colour = "#4271AE")  + stat_function(fun = LEst, color = "red") +  ggtitle("Inductance / tours - global")

)
print(p)
print("-----  ajustement 2   ------------")
mod5 <- lm(GLOB$R~GLOB$N+I(GLOB$N^2), data=GLOB)

print(summary(mod5))

REst<-function(N){mod5$coefficients[1]  + mod5$coefficients[2] * N +   mod5$coefficients[3] * N^2}


p<-ggplot(GLOB) + geom_point(aes(x = N, y = R), colour = "#4271AE")  + stat_function(fun = REst, color = "red") +  ggtitle("Résistance ant / tours - global")
print(p)

# Courant d'antenne théorique
vdd<-5
Vss<-0
Rser=27
Rad=9


Iant<-function(R){
  
  I<-(4/pi)*((vdd-Vss)/(R+Rser+2*Rad))*1000
  return(I)
  
}

#jpeg("Ith.jpg")

curve(Iant, 0, 500, col="red", main="Courant théorique antenne Vs Résistance antenne",  xlab="Résistance(Ohms)",
      ylab="Iant(mA)")
#dev.off()


In<-function(N){
  
  I<-Iant(REst(N))
  return(I)
  
}


#jpeg("I0.jpg")

curve(In, 0, 120, col="red", main="Courant estimé antenne Vs tours - Neutre",  xlab="Tours",
      ylab="Iant(mA)")
#dev.off()


# Etimation du champ magnétique

z<-1e-2 # distance de référence 1cm
r<-0.38 # rayon de l'antenne


BEst<-function(N){
  
  b<-In(N) *r^2/z^3
  return(b)
  
  
}


# Représentation du champ  

#jpeg("champGlobal.jpg")
curve(BEst, 0, 120, col="red", main="Champ estimé Vs tours - global",  xlab="Tours",
      ylab="Champ")

#dev.off()

# Optimisations

# Champs magnétiques
print("----------------------------------------------------")
maxChp<-optimize(BEst, c(0, 120), maximum = TRUE)
print("Max champ global: ")
print(maxChp)
