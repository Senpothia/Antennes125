library(ggplot2)

# NEUTRE

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

# CARTE 2


N<-c(60, 70, 80, 90, 100)
Lm<-c(0.770, 1.05, 1.41,1.78, 2.22)
enrL<-data.frame(Lm, N)
#print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

print("-----  ajustement 3   ------------")
mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){3.429e-03  -1.414e-03 * N +   2.357e-04 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours - carte 2")
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red"))

print("-----  ajustement 4   ------------")
mod31 <- lm(N~Lm+I(Lm^2), data=enrL)
print(summary(mod31))
Nest_c2<-function(L){30.8610 + 41.8880 * L  -4.8518 * L^2}
print(ggplot(enrL) + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest_c2, color = "red") +  ggtitle("tours / inductance - carte 2 / neutre")
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red")
      + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest_neutre, color = "blue") 
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red")
      
      )
#curve(Nest, 1, 4, xname = "t")


# ESTIMATIONs


Lth<- function(C, F){(4*pi^2*F^2*C)^-1*1e3}

