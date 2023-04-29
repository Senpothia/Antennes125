

# Modèle pour R vs N - N est la variable du modèle, F est le paramètre des courbes

crn<-function(F){
  
  c<-(-10*F^2-F+48)
  return(c)
  
}


brn<-function(F){
  
  b<-0.2*F+52
  return(b)
}

arn<-function(F){
  
  a<-(0.1*F^2+0.002*F-8.5)
  return(a)
  
}  

N<-c(60, 80, 100,120) # Variable N
param<-50
F=rep(param,4)  # paramètre: F
FacteurR<-30000

modR2<-function(F, N) {
  y<-(0.1*N^2-10) * F^2 + (0.002*N^2+0.2*N-1) *F  + (-8.5*N^2 +52*N + 48)
  return(y/facteurR)
}

R2<-round(modR2(F,N),2)

VAL<-data.frame(N,R2)
m2 <- lm(VAL$R2~VAL$N+I(VAL$N^2), data=VAL)

print("-------------------  MODELE 2  -------------------------------------------------")
print("R vs N, F=100 ")
print(m2)

courbeR2<-function(N){
  
  m2$coefficients[1] +  m2$coefficients[2]*N +  m2$coefficients[3]*N^2
  
}

plot(N, R2, main="1")
curve(courbeR, 60, 120, col="red", add=TRUE)


modR3<-function(N){ (crn(param) + brn(param)*N + arn(param)*N^2)/facteurR }
R3<-round(modR3(N),2)

VAL<-data.frame(N,R2, R3)
m3 <- lm(VAL$R3~VAL$N+I(VAL$N^2), data=VAL)

print("-------------------  MODELE 3  -------------------------------------------------")
print(m3)
print("Coefficients théorique du polynôme générateur de données:")
print(paste("RN: ", "Paramètre: ", as.character(param)))
print(paste("Do:", crn(param)/FacteurR))
print(paste("D1:", brn(param)/FacteurR))
print(paste("D2:", arn(param)/FacteurR))
print("Données générées R2, R3:")
print(R2)
print(R3)
plot(N, R2, main="2", col="red")
points(N, R3, col="blue")
curve(courbeR2, 60, 120, col="red", add=TRUE)
curve(modR3, 60, 120, col="green", add=TRUE)


