library(ggplot2)

# NEUTRE

N<-c(60, 70, 80, 90, 100, 120)
Lm<-c(0.972, 1.33, 1.78,2.28, 2.89, 4.24)
enrL<-data.frame(Lm, N)
#print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){1.983e-1 + -8.137e-03 * N +  3.488e-04 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours - neutre") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red"))


# CARTE 1


N<-c(60, 70, 80, 90, 100, 120)
Lm<-c(0.917, 1.25, 1.34,2.14, 2.22, 3.95)
enrL<-data.frame(Lm, N)
#print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){2.3989452  -0.0602443 * N + 0.0006065 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours - carte 1")
    +  geom_hline(yintercept=1.62, linetype="dashed", color = "red"))

mod31 <- lm(N~Lm+I(Lm^2), data=enrL)
print(summary(mod31))
Nest<-function(L){27.538 + 41.251 * L -4.527 * L^2}
print(ggplot(enrL) + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Nest, color = "red") +  ggtitle("tours / inductance")
      + geom_vline(xintercept=1.62, linetype="dashed", color = "red"))
#curve(Nest, 1, 4, xname = "t")


# ESTIMATIONs


Lth<- function(C, F){(4*pi^2*F^2*C)^-1*1e3}

