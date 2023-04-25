#--------- AUTRE MODELE   -------------------------------------------------------------------------------


# Définition des modèles


modR2<-function(F, N) {
  y<-(0.04*N^2+1) * F^2 + (0.001*N^2-10) *F  + (0.02*N^2 + 1)
  return(y/facteurR)
}


modL2<-function(F, N) {
  y<-(0.08*N^2+1) * F^2 + (0.001*N^2+1) *F  + (0.02*N^2 + 1)
  return(y/facteurL)
}

# Déclarations de variables


inductances2<-c()
resistances2<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)

colr<-c("blue", "red", "green", "orange")

facteurR<-1500
facteurL<-1500
echs<-c(rep(rep(1,7), 4),rep(rep(2, 7),4), rep(rep(3, 7),4))
F<-c(rep(frequencies,12))
N1<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7), rep(nTypes[4],7))
N<-rep(N1, 3)

n<-seq(60, 120, by=1)

# Génération des résistances et inductances


L<-round(modL2(F,N) + (rnorm(84)+1)*2 , 2)
R<-round(modR2(F,N) + (rnorm(84)+1)*2 , 2)

data<-data.frame(echs, N, F, R, L)


VAL=data[data$F == 100, ]
mod <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
print(summary(mod))
#titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
Lest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}
plot(VAL$N, VAL$L, main="1", ylim=c(0, max(VAL$L)))
lines(n, Lest2N(n), col="red")


for(f in frequencies){
VAL=data[data$F == f, ]
mod <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
print(summary(mod))
#titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
Lest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}
plot(VAL$N, VAL$L, main=as.character(f),ylim=c(0, max(VAL$L)))
lines(n, Lest2N(n), col="red")

}



