# Etude avec l'ensemble des mesures sur 3 carte et neutres

TAB<-read.table("global.csv",header=TRUE,sep=";",dec=",")

# Ajustement 1 - Neutre

print("-----  ajustement 1   ------------")
mod1 <- lm(TAB$Ln~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod1))

LnEst<-function(N){1.372e-01  -6.362e-03 * N +   3.366e-04 * N^2}
print(ggplot(TAB) + geom_point(aes(x = N, y = Ln), colour = "#4271AE")  + stat_function(fun = LnEst, color = "red") +  ggtitle("Inductance / tours - neutre") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
      )

# Ajustement 2 - carte 1

print("-----  ajustement 2   ------------")
mod2 <- lm(TAB$L1~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod2))

L1Est<-function(N){1.857e-01  -7.370e-03 * N +   3.230e-04 * N^2}
print(ggplot(TAB) + geom_point(aes(x = N, y = L1), colour = "#4271AE")  + stat_function(fun = L1Est, color = "red") +  ggtitle("Inductance / tours - carte 1") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)


# Ajustement 3 - carte 2

print("-----  ajustement 3   ------------")
mod3 <- lm(TAB$L2~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod3))

L2Est<-function(N){5.368e-02  -2.733e-03 * N +   2.440e-04 * N^2}
print(ggplot(TAB) + geom_point(aes(x = N, y = L2), colour = "#4271AE")  + stat_function(fun = L2Est, color = "red") +  ggtitle("Inductance / tours - carte 2") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)


# Ajustement 4 - carte 3

print("-----  ajustement 4   ------------")
mod4 <- lm(TAB$L3~TAB$N+I(TAB$N^2), data=TAB)

print(summary(mod4))

L3Est<-function(N){8.075e-02  -4.079e-03 * N +   2.957e-04 * N^2}
print(ggplot(TAB) + geom_point(aes(x = N, y = L3), colour = "#4271AE")  + stat_function(fun = L3Est, color = "red") +  ggtitle("Inductance / tours - carte 3") 
      +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)

# Représentation globale
colors <- c("Ln" = "red", "L1" = "blue", "L2" = "green", "L3" = "magenta")
print(ggplot(TAB) 
      + geom_point(aes(x = N, y = Ln), colour = "red")  + stat_function(fun = LnEst, color = "red") +  ggtitle("Inductance / tours - global") 
      + geom_point(aes(x = N, y = L1), colour = "blue")  + stat_function(fun = L1Est, color = "blue")
      + geom_point(aes(x = N, y = L2), colour = "green")  + stat_function(fun = L2Est, color = "green")
      + geom_point(aes(x = N, y = L3), colour = "magenta")  + stat_function(fun = L3Est, color = "magenta")
      + labs(x = "N - Tours",
           y = "(Inductance - mH)",
           color = "Legend") +
          scale_color_manual(values = colors)
   #   +  geom_hline(yintercept=1.62, linetype="dashed", color = "red")
)




