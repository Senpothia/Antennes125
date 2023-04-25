
library(ggplot2)
library(insight)

# f<-function(x) 2*x^2 + 10*x + 1
# g<-function(x) 2.8*x^2 + 5*x + 1
# h<-function(x) 3.6*x^2 + 1*x + 1
# curve(f,0,100,col="blue")
# curve(g,0,100,col="red", add=TRUE)
# curve(h,0,100,col="green", add=TRUE)
# 
# f1<-function(x) 2*x^2 + 50*x + 1
# f2<-function(x) 2*x^2 + 1*x + 1
# curve(f1,0,100,col="blue")
# curve(f2,0,100,col="red", add=TRUE)

#VAL=data[data$N == 60 & data$echs==1,]
#plot(VAL$F, VAL$L, type="l")
#VAL=data[data$N == 80 & data$echs==1,]
#lines(VAL$F, VAL$L, col="red")

# p<-(ggplot(VAL) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle(titre) 
#   
# )
# #png("L1.png")
# print(p)
# #dev.off()

#---------------------------------------------------------------------------
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

# Modèle d'inductance

facteurL<-700
modL<-function(F, N) { 
  y<-(0.001*N+1) * F^2 + (0.2*N-10) *F  + (0.8*N + 1)
  return(y/facteurL)  
}

f<-function(F){ modL(F, 60)}  # N=60
g<-function(F){ modL(F, 80)}  # N=80
h<-function(F){ modL(F, 100)}  # N=100
k<-function(F){ modL(F, 120)}  # N=120

curve(f,10,100,col="blue", xlab = "F")
curve(g,10,100,col="red", add=TRUE)
curve(h,10,100,col="green", add=TRUE)
curve(k,10,100,col="orange", add=TRUE)

resistances<-c()
inductances<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)
echs<-c()


# Génération des inductances

for(k in 1:3){
  
  for(i in nTypes){
    
      L<-modL(c(10,20,28.5,40,50,66.6,100),i)
      inductances<-append(inductances, L)
      echs<-append(echs, k)
    }
}

inductancesBruit<-round(inductances + (rnorm(7)+1)*2,2)
F<-rep(frequencies, 4)
N<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7),rep(nTypes[4],7))

data<-data.frame(echs, N, F, inductancesBruit)
colnames(data)[4] = "L"


# Réprésentation des inductances
colr<-c("blue", "red", "green", "orange")
c<-0
compteur<-1
f<-seq(10, 100, by=1)
VAL=data[data$N == 60 & data$echs==1,]
plot(VAL$F, VAL$L)

for(e in unique(echs)){
  for (n in nTypes){


    VAL=data[data$N == n & data$echs==e,]
    mod <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)


    print(summary(mod))

    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Lest<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$F, VAL$L)
    lines(f, Lest(f), col=colr[c])
    c<-c + 1

  }

# }


# Modèle de résistance

facteurR<-15
modR<-function(F, N) {
  y<-(0.08*N+1) * F^2 + (0.001*N-10) *F  + (0.02*N + 1)
  return(y/facteurR)
}

fr<-function(F){ modR(F, 60)}  # N=60
gr<-function(F){ modR(F, 80)}  # N=80
hr<-function(F){ modR(F, 100)}  # N=100
kr<-function(F){ modR(F, 120)}  # N=120

curve(fr,10,100,col="blue", xlab = "F")
curve(gr,10,100,col="red", add=TRUE)
curve(hr,10,100,col="green", add=TRUE)
curve(kr,10,100,col="orange", add=TRUE)


# Génération des résistances

for(k in 1:3){

  for(i in nTypes){

    R<-modR(c(10,20,28.5,40,50,66.6,100),i)
    resistances<-append(resistances, R)
   
  }

}

resistancesBruit<-round(resistances + (rnorm(7)+1)*2,2)
F<-rep(frequencies, 4)
N<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7),rep(nTypes[4],7))

data<-data.frame(echs, N, F, inductancesBruit, resistancesBruit)
colnames(data)[4] = "L"
colnames(data)[5] = "R"

# Réprésentation des résistances


c<-0
compteur<-1
f<-seq(10, 100, by=1)
VAL=data[data$N == 60 & data$echs==1,]
plot(VAL$F, VAL$R)

for(e in unique(echs)){
  for (n in nTypes){


    VAL=data[data$N == n & data$echs==e,]
    mod <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)


    print(summary(mod))

    titre<-paste("Inductance / Fréquence - N=", as.character(n), "pour éch:", as.character(e))
    Rest<-function(F){mod$coefficients[1]  + mod$coefficients[2] * F +   mod$coefficients[3] * F^2}

    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1


    points(VAL$F, VAL$R)
    lines(f, Rest(f), col=colr[c])
    c<-c + 1

  }

}


#--------------------------------------------------------------------------------------------------------

#--------- AUTRE MODELE   -------------------------------------------------------------------------------

# Génération des résistances

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
    
  }
  
}
resistances2<-round(resistances2 + (rnorm(7)+1)*2,2)
data<-data.frame(echs, N, F, inductancesBruit, resistancesBruit, resistances2)
colnames(data)[4] = "L"
colnames(data)[5] = "R"
colnames(data)[6] = "R2"


# Réprésentation des résistances - modèle 2

# Résistance vs Fréquence

c<-0
compteur<-1
f<-seq(10, 100, by=1)
VAL=data[data$N == 60 & data$echs==1,]
plot(VAL$F, VAL$R2)

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

# Résistance vs spires




#---------------------------------   Autre modèle - Inductance  -------------------------------------------------------------------------


inductances2<-c()
facteurI<-1500
modL2<-function(F, N) {
  y<-(0.08*N^2+1) * F^2 + (0.001*N^2-10) *F  + (0.02*N^2 + 1)
  return(y/facteurI)
}

for(k in 1:3){
  
  for(i in nTypes){
    
    L2<-modL2(c(10,20,28.5,40,50,66.6,100),i)
    inductances2<-append(inductances2, L2)
    
  }
  
}
inductances2<-round(inductances2 + (rnorm(7)+1)*2,2)
data<-data.frame(echs, N, F, inductancesBruit, resistancesBruit, resistances2, inductances2)
colnames(data)[4] = "L"
colnames(data)[5] = "R"
colnames(data)[6] = "R2"
colnames(data)[7] = "L2"


# Réprésentation des inductances - modèle 2

# Inductance vs Fréquence

c<-0
compteur<-1
f<-seq(10, 100, by=1)
VAL=data[data$N == 60 & data$echs==1,]
plot(VAL$F, VAL$L2)

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

# Inductance vs spires

c<-0
compteur<-1
n<-seq(60, 120, by=1)
VAL=data[data$F == 10 & data$echs==1,]
plot(VAL$N, VAL$L2)


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


# Enregistrement des données

 write.csv(data,"./data/data.csv", row.names = FALSE)

data2<-data[,-c(4,5)]
colnames(data2)[4] = "R"
colnames(data2)[5] = "L"
write.csv(data2,"./data/data2.csv", row.names = FALSE)
