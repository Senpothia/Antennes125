#--------- AUTRE MODELE   -------------------------------------------------------------------------------


# Définition des modèles


modR2<-function(F, N, facteur) {
  y<-(0.1*N^2-10) * F^2 + (0.002*N^2+0.2*N-1) *F  + (-8.5*N^2 +52*N + 48)
  return(y/facteurR)
}


modL2<-function(F, N, facteur) {
  y<-(0.5*N^2-10) * F^2 + (22*N^2+0.2*N-1) *F  + (-10*N^2 +52*N + 10)
  return(y/facteurL)
}

# Déclarations de variables


inductances2<-c()
resistances2<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)
colrF=c("red", "blue", "green", "magenta", "orange", "cyan", "gray")
colrN<-c("blue", "red", "green", "orange")

facteurR<-30000
facteurL<-50000
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

#---------------------------------------------------------------------------------------------------------
# GRAPHE 1: Inductance vs N


plot.new()
plot.window( xlim=c(60,120), ylim=c(0,max(data$L)+100))
axis( side=1)
axis( side=2, seq(0, 4000, by=80))
title(main="1: Inductance vs N")
legend(60, 1840, legend=c("F=10", "F=20", "F=28.5", "F=40","F=50", "F=66.6", "F=100"),
       col=c("red", "blue", "green", "magenta", "orange", "cyan", "gray"), lty=rep(1,7))


i<-0
for(fq in frequencies){
VAL=data[data$F == fq, ]
mod <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
print(summary(mod))

Lest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}

lines(n, Lest2N(n), col=colrF[i])
i<-i+1
}

#---------------------------------------------------------------------------------------------------------
# GRAPHE 2: Inductance vs F

n<-seq(60, 120, by=1)
f<-seq(10, 100, by=1)
i<-0
plot.new( )
plot.window( xlim=c(10,100), ylim=c(0,max(data$L)))
axis( side=1)
axis( side=2, seq(0, 4000, by=200))
title(main="2: Inductance vs F")
legend(10, 1600, legend=c("N=60", "N=80", "N=100", "N=120"),
       col=c("red", "blue", "green", "magenta"), lty=rep(1,4))

for(n in nTypes){
  VAL=data[data$N == n, ]
  mod <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
  print(summary(mod))
 
  Lest2F<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}
  
  lines(f, Lest2F(f), col=colrN[i])
  i<-i+1
  
}

#---------------------------------------------------------------------------------------------------------
# GRAPHE 3: Résistance vs F

n<-seq(60, 120, by=1)
f<-seq(10, 100, by=1)
plot.new( )
plot.window( xlim=c(10,100), ylim=c(0,max(data$R)))
axis( side=1)
axis( side=2, seq(0, 4000, by=200))
title(main="3: Résistance vs F")
legend(20, 400, legend=c("F=10", "F=20", "F=28.5", "F=40","F=50", "F=66.6", "F=100"),
       col=c("red", "blue", "green", "magenta", "orange", "cyan", "gray"), lty=rep(1,7))


i<-0

for(n in nTypes){
  VAL=data[data$N == n, ]
  mod <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)
  print(summary(mod))
  
  Rest2F<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}
  
  lines(f, Rest2F(f), col=colrF[i])
  i<-i+1
}

#---------------------------------------------------------------------------------------------------------
# GRAPHE 4: Résistance vs N

n<-seq(60, 120, by=1)
f<-seq(10, 100, by=1)
plot.new( )
plot.window( xlim=c(60,120), ylim=c(0,max(data$R)))
axis( side=1)
axis( side=2, seq(0, 500, by=50))
title(main="4: Résistance vs N")
legend(60, 400, legend=c("N=60", "N=80", "N=100", "N=120"),
       col=c("red", "blue", "green", "magenta"), lty=rep(1,4))


i<-0
for(fq in frequencies){
  VAL=data[data$F == fq, ]
  mod <- lm(VAL$R~VAL$N+I(VAL$N^2), data=VAL)
  print(summary(mod))
  
  Rest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}
  
  lines(n, Rest2N(n), col=colrN[i])
  i<-i+1
  
}

# Enregistrement des données

write.csv(data,"./data/data.csv", row.names = FALSE)



