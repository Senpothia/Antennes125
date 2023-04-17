library(ggplot2)

# NEUTRE
# Mesures de références: 07/02/2023

N<-c(60, 70, 80, 90, 100)
Lm<-c(0.972, 1.33, 1.78,2.28, 2.89)

enrL<-data.frame(Lm, N)
#print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

print("-----  ajustement 1   ------------")
mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){4.750e-01  -1.545e-02 * N +   3.957e-04 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours - neutre") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red"))

print("-----  ajustement 2   ------------")
mod31 <- lm(N~Lm+I(Lm^2), data=enrL)
print(summary(mod31))
Nest_neutre<-function(L){ 30.0865  + 34.4416  * L -3.5552  * L^2}
print(ggplot(enrL) + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest_neutre, color = "red") +  ggtitle("tours / inductance - Neutre")
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red"))

# CARTE 1


N<-c(60, 70, 80, 90, 100)
Lm<-c(0.80, 1.25, 1.64,2.14, 2.69)       # inductances mesurées
Rm<-c(83.9, 117, 165, 202, 267)      # résistances mesurées
Qm<-c(6.86, 6.72, 6.24, 6.66, 6.32) # facteurs de qualité mesurés

enrL<-data.frame(Lm, N)
enrLR<-data.frame(Lm, Rm)
#print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

print("-----  ajustement 3   ------------")
mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){5.877e-01  -1.781e-02 * N +  3.886e-04 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours - carte 1")
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red"))

print("-----  ajustement 4   ------------")
mod31 <- lm(N~Lm+I(Lm^2), data=enrL)
print(summary(mod31))
Nest_c1<-function(L){28.8519 + 38.2387 * L  -4.4076 * L^2}
print(ggplot(enrL) + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest_c1, color = "red") +  ggtitle("tours / inductance - carte 1 / neutre")
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red")
      + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest_neutre, color = "blue") 
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red")
        )
#curve(Nest, 1, 4, xname = "t"


# ESTIMATIONs
 

Lth<- function(C, F){(4*pi^2*F^2*C)^-1*1e3}

Fth<-function(C, L){(2*pi*sqrt(L*1e-3*C))^-1}


Vant<-function(L,R){
  
    10/(pi^2+125000*L*1e-3*(R+36))
}

Rcal<-function(Lm, Qm){2*pi*125000*Lm/Qm}

#print(ggplot(enrLR) + geom_point(aes(x = N, y = Lm)))
plot(c(1,2,3,4,5), Rcal(Lm, Rm))

