
library(ggplot2)

x<-1:10
df<-data.frame(x)

y0<-function(x){1 + 2*x + 3*x^2}

p<-ggplot(df,aes(x))+ stat_function(fun=y0)
print(p)