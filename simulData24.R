#--------- AUTRE MODELE   -------------------------------------------------------------------------------


# Définition des modèles


modR2<-function(F, N, facteur) {
  y<-(0.1*N+0.01) * F^2 + (0.1*N^2+0.2*N-10) *F  + (10*N^2+0.000005*N + 1)
  return(y/facteurR)
}


modL2<-function(F, N, facteur) {
  y<-(0.5*N^2-10) * F^2 + (22*N^2+0.2*N-1) *F  + (-10*N^2 +52*N  - 10)
  return(y/facteurL)
}

# Déclarations de variables


inductances2<-c()
resistances2<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)

colr<-c("blue", "red", "green", "orange")

facteurR<-1500
facteurL<-30000
echs<-c(rep(rep(1,7), 4),rep(rep(2, 7),4), rep(rep(3, 7),4))
F<-c(rep(frequencies,12))
N1<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7), rep(nTypes[4],7))
N<-rep(N1, 3)

n<-seq(60, 120, by=1)
f<-seq(10, 100, by=1)

# Génération des résistances et inductances


L<-round(modL2(F,N, 1500) + (rnorm(84)+1)*2 , 2)
R<-round(modR2(F,N, 1500) + (rnorm(84)+1)*2 , 2)

data<-data.frame(echs, N, F, R, L)

#plot.new(xlim=c(60, 120), ylim=c(0,max(data$L)))
plot.new( )
plot.window( xlim=c(60,120), ylim=c(0,max(data$L)))
axis( side=1)
axis( side=2, seq(0, 4000, by=80))


for(fq in frequencies){
VAL=data[data$F == fq, ]
mod <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
print(summary(mod))
#titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
Lest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}
#plot(VAL$N, VAL$L, main=as.character(f),ylim=c(0, max(VAL$L)))
lines(n, Lest2N(n), col="red")

}

plot.new( )
plot.window( xlim=c(10,100), ylim=c(0,max(data$L)))
axis( side=1)
axis( side=2, seq(0, 4000, by=200))

for(n in nTypes){
  VAL=data[data$N == n, ]
  mod <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
  print(summary(mod))
  #titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
  Lest2F<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}
  #plot(VAL$N, VAL$L, main=as.character(f),ylim=c(0, max(VAL$L)))
  lines(f, Lest2F(f), col="blue")
  
}



