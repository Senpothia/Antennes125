f1<-function(x) 1*x^2
f2<-function(x) 2*x^2
f3<-function(x) 3*x^2

curve(f1, 0, 100)
curve(f2, 0, 100, col="red", add=TRUE)
curve(f3, 0, 100, col="blue", add=TRUE)