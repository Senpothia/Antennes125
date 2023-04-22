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

#---------------------------------------------------------------------------

model<-function(F, N) {(0.01*N+1) * F^2 + (0.001*N-10) *F  + (0.02*N + 1)}
f<-function(F){ model(F, 10)}  # N=60
g<-function(F){ model(F, 80)}  # N=80
h<-function(F){ model(F, 100)}  # N=100
k<-function(F){ model(F, 120)}  # N=120

curve(f,10,100,col="blue")
curve(g,10,100,col="red", add=TRUE)
curve(h,10,100,col="green", add=TRUE)
curve(k,10,100,col="orange", add=TRUE)

inductances<-c()
frequencies<-c(10,20,28.5,40,50,66.6,100)
nTypes<-c(60, 80, 100, 120)
echs<-c()

for(k in 1:3){
  
  for(i in frequencies){
    
    for(j in nTypes){
      
      L<-model(i,j)
      inductances<-append(inductances, L)
      echs<-append(echs, k)
    }
    
  }
  
}

inductancesBruit<-round(inductances + (rnorm(7)+1)*2,2)
F<-rep(frequencies, 4)
N<-c(rep(nTypes[1],7), rep(nTypes[2],7), rep(nTypes[3],7),rep(nTypes[4],7))

data<-data.frame(echs, N, F, inductancesBruit)
colnames(data)[4] = "L"
write.csv(data,"./data/data.csv", row.names = FALSE)



