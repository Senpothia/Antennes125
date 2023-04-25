#--------- AUTRE MODELE   -------------------------------------------------------------------------------

# Génération des résistances


inductances2<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)
echs<-c()
colr<-c("blue", "red", "green", "orange")

F<-rep(frequencies, 4)
N<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7),rep(nTypes[4],7))


facteurR<-1500
resistances2<-c()

modR2<-function(F, N) {
  y<-(0.04*N^2+1) * F^2 + (0.001*N^2-10) *F  + (0.02*N^2 + 1)
  return(y/facteurR)
}

for(k in 1:3){
  
  for(i in nTypes){
    
    R2<-modR2(c(10,20,28.5,40,50,66.6,100),i)
    resistances2<-append(resistances2, R2)
    echs<-append(echs, rep(k, 7))  
    
  } 

}
resistances2<-round(resistances2 + (rnorm(7)+1)*2,2)
data<-data.frame(echs, N, F, resistances2)

colnames(data)[4] = "R2"


# # Réprésentation des résistances - modèle 2
#
# # Résistance vs Fréquence

c<-0
compteur<-1
f<-seq(10, 100, by=1)
VAL=data[data$N == 60 & data$echs==1,]
plot(VAL$F, VAL$R2, ylim=c(0, max(data$R2)+10), main="R2 vs F")

for(e in unique(echs)){
  for (n in nTypes){


    VAL=data[data$N == n & data$echs==e,]
    mod <- lm(VAL$R2~VAL$F+I(VAL$F^2), data=VAL)


    print(summary(mod))
    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Rest2<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$F, VAL$R2)
    lines(f, Rest2(f), col=colr[c])

  }

}

# # Résistance vs spires




c<-0
compteur<-1
n<-seq(60, 120, by=1)
VAL=data[data$F == 100 & data$echs==1,]
plot(VAL$N, VAL$R2, ylim=c(0, max(data$R2)+10), main="R2 vs N")


for(e in unique(echs)){
  for (f in frequencies){


    VAL=data[data$F == f & data$echs==e,]
    mod <- lm(VAL$R2~VAL$N+I(VAL$N^2), data=VAL)


    print(summary(mod))
    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Rest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$N, VAL$R2)
    lines(n, Rest2N(n), col=colr[c])

    c<-c + 1

  }

}




# #---------------------------------   Autre modèle - Inductance  -------------------------------------------------------------------------


inductances2<-c()
facteurI<-1500
modL2<-function(F, N) {
  y<-(0.08*N^2+1) * F^2 + (0.001*N^2+1) *F  + (0.02*N^2 + 1)
  return(y/facteurI)
}

for(k in 1:3){

  for(i in nTypes){

    L2<-modL2(c(10,20,28.5,40,50,66.6,100),i)
    inductances2<-append(inductances2, L2)

  }

}
inductances2<-round(inductances2 + (rnorm(7)+1)*2,2)
data<-data.frame(echs, N, F, resistances2, inductances2)

colnames(data)[4] = "R2"
colnames(data)[5] = "L2"


# # Réprésentation des inductances - modèle 2
#
# # Inductance vs spires

c<-0
compteur<-1
n<-seq(60, 120, by=1)
VAL=data[data$F == 100 & data$echs==1,]
plot(VAL$N, VAL$L2, ylim=c(0, max(data$L2)+10),  main="L2 vs N")


for(e in unique(echs)){
  for (f in frequencies){


    VAL=data[data$F == f & data$echs==e,]
    mod <- lm(VAL$L2~VAL$N+I(VAL$N^2), data=VAL)


    print(summary(mod))
    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Iest2N<-function(N){mod$coefficients[1]  + mod$coefficients[2] * N +   mod$coefficients[3] * N^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$N, VAL$L2)
    lines(n, Iest2N(n), col=colr[c])

    c<-c + 1

  }

}

# # Inductance vs Fréquence

c<-0
compteur<-1
f<-seq(10, 100, by=1)
# VAL=data[data$N == 60 & data$echs==1,]
# plot(VAL$F, VAL$L2, ylim=c(0, max(data$L2)+10),  main="L2 vs F")

for(e in unique(echs)){
  for (n in nTypes){


    VAL=data[data$N == n & data$echs==e,]
    mod <- lm(VAL$L2~VAL$F+I(VAL$F^2), data=VAL)


    print(summary(mod))
    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Iest2<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$F, VAL$L2)
    lines(f, Iest2(f), col=colr[c])
    c<-c + 1

  }

}




# Enregistrement des données

write.csv(data,"./data/data.csv", row.names = FALSE)

