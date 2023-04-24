
library(ggplot2)

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

facteur<-3.5
modL<-function(F, N) { 
  y<-(0.008*N+1) * F^2 + (0.001*N-10) *F  + (0.02*N + 1)
  return(y/facteur)  
}

f<-function(F){ modL(F, 60)}  # N=60
g<-function(F){ modL(F, 80)}  # N=80
h<-function(F){ modL(F, 100)}  # N=100
k<-function(F){ modL(F, 120)}  # N=120

curve(f,10,100,col="blue", xlab = "F")
curve(g,10,100,col="red", add=TRUE)
curve(h,10,100,col="green", add=TRUE)
curve(k,10,100,col="orange", add=TRUE)

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

data<-data.frame(echs, N, F, inductancesBruit, inductancesBruit)
colnames(data)[4] = "L"
colnames(data)[5] = "R"
write.csv(data,"./data/data.csv", row.names = FALSE)


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
    # p<-(ggplot(VAL) + geom_point(aes(x = F, y = L), colour = "#4271AE")  + stat_function(fun = Lest, color = "red") +  ggtitle(titre) 
    #   
    # )
    # #png("L1.png")
    # print(p)
    # #dev.off()
    print(paste("COMPTEUR:", compteur))
    compteur<-compteur + 1
    
    
    points(VAL$F, VAL$L)
    lines(f, Lest(f), col=colr[c], add=TRUE)
    c<-c + 1
    
  }

}


