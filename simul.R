library(ggplot2)

res120 <- c(35.1, 35.5, 36.6,39.2,42.5,50.6,83.2)
freq <- c(10, 20, 28.5, 40, 50,66.6, 100)

enr <- data.frame(res120, freq)


mod1 <- lm(res120~freq+I(freq^2), data=enr)

print(summary(mod1))
extrap<-function(x) 37.729 - 0.2651862 * x +  0.0071622 * x^2

print(ggplot(enr) + geom_point(aes(x = freq, y = res120),  colour = "#4271AE") + stat_function(fun = extrap, color = "red") +  ggtitle("Resistance L120 vs Frequency") )

L<-3.5e-3
f<- 125e3
Q <- function(r){L*2*3.14*f/r}
Q120<-Q(res120)
enrQ <- data.frame(Q120, freq)

print(ggplot(enrQ) + geom_point(aes(x = freq, y = Q120)))

mod2 <- lm(Q120~freq+I(freq^2), data=enrQ)

print(summary(mod2))


#----------------------------------------------------------------------------------------------------------------------------

N<-c(68, 73, 90, 120)
Lm<-c(1.25, 1.39, 2.28, 4.05)
enrL<-data.frame(Lm, N)
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm)))

mod3 <- lm(Lm~N+I(N^2), data=enrL)

print(summary(mod3))

Lest<-function(N){-0.5906188 + 0.0107794 * N + 0.0002326 * N^2}
print(ggplot(enrL) + geom_point(aes(x = N, y = Lm), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("Inductance / tours") )


mod31 <- lm(N~Lm+I(Lm^2), data=enrL)

print(summary(mod31))

Nest<-function(L){38.9747 + 25.6616 * L - 1.3980 * L^2}
print(ggplot(enrL) + geom_point(aes(x = Lm, y = N), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle("tours / inductance") )
curve(Nest, 1, 4, xname = "t")

Rm<-c(34.1, 45.5, 66.6, 83.2)
enrR<-data.frame(Rm, N)
print(ggplot(enrR) + geom_point(aes(x = N, y = Rm)))

mod4 <- lm(Rm~N+I(N^2), data=enrR)
print(summary(mod4))

Rest<-function(N){-1.720e+02 + 4.252e+00 * N + -1.771e-02 *N^2}
print(ggplot(enrR) + geom_point(aes(x = N, y = Rm), colour = "#4271AE")  + stat_function(fun = Rest, color = "red") +  ggtitle("Résistance / tours")   )



Qest <- function(N){Lest(N)*1e-3*2*3.14*125000/ Rest(N)}
Qn <- Qest(N)
enrQ <- data.frame(Qn, N)
print(ggplot(enrQ) + geom_point(aes(x = N, y = Qn)))

mod5 <- lm(Qn~N+I(N^2), data=enrQ)
print(summary(mod5))

Qest <- function(N){78.4569590 + -1.3136339 * N +  0.0081548 * N^2}
print(ggplot(enrQ) + geom_point(aes(x = N, y = Qn) , colour = "#4271AE")  + stat_function(fun = Qest, color = "red") +  ggtitle("Q Factor / tours")  )

Iest<- function(N){4/3.14*(5/(Rest(N) + 29))}
In <- Iest(N)
enrI <- data.frame(In, N)

print(ggplot(enrI) + geom_point(aes(x = N, y = In) , colour = "#4271AE")  + stat_function(fun = Iest, color = "red") +  ggtitle("Iant / tours")  )


Best<- function(N){4*3.14*1e-7*Iest(N)*0.08^2/(2*0.03)}
Bn <-Best(N)
enrB <- data.frame(Bn, N)
print(ggplot(enrB)+ geom_point(aes(x = N, y = Bn) , colour = "#4271AE")  + stat_function(fun = Best, color = "red") +  ggtitle("B / tours")  )

Lth<- function(C, F){(4*pi^2*F^2*C)^-1*1e3}

      