

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
F=rep(100,4)  # paramètre: F
FacteurR<-30000
modR2<-function(F, N) {
  y<-(0.1*N^2-10) * F^2 + (0.002*N^2+0.2*N-1) *F  + (-8.5*N^2 +52*N + 48)
  return(y/facteurR)
}
R2<-modR2(F,N)

VAL<-data.frame(N,R2)
m2 <- lm(VAL$R2~VAL$N+I(VAL$N^2), data=VAL)
print(m2)

courbeR2<-function(N){
  
  m2$coefficients[1] +  m2$coefficients[2]*N +  m2$coefficients[3]*N^2
  
}

plot(N, R2, main="1")
curve(courbeR, 60, 120, col="red", add=TRUE)


modR3<-function(N){ (crn(100) + brn(100)*N + arn(100)*N^2)/facteurR}
R3<-modR3(N)

VAL<-data.frame(N,R2, R3)
m3 <- lm(VAL$R3~VAL$N+I(VAL$N^2), data=VAL)
print("----------------------------------------------------------------------")
print(m3)

plot(N, R2, main="2", col="red")
points(N, R3, col="blue")
curve(courbeR2, 60, 120, col="red", add=TRUE)
curve(modR3, 60, 120, col="green", add=TRUE)


