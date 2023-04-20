#Test tableau de modèles

TAB<-read.table("./data/mesures.csv",header=TRUE,sep=";",dec=",")

mod1 <- lm(TAB$Ln~TAB$N+I(TAB$N^2), data=TAB)
mod2 <- lm(TAB$L1~TAB$N+I(TAB$N^2), data=TAB)
mod3 <- lm(TAB$L2~TAB$N+I(TAB$N^2), data=TAB)
mod4 <- lm(TAB$L3~TAB$N+I(TAB$N^2), data=TAB)

MODS<-list(mod1, mod2, mod3, mod4)
n<-c(1, 2, 3)
i<-1
f1s<-paste("MODS[[1]]$coefficients[", as.character(n[i]), "] + MODS[[1]]$coefficients[", as.character(n[i+1]), "]*N+ MODS[[1]]$coefficients[", as.character(n[i+2]),"]*N^2")

f<-function(N){
  
  fonction<-parse(text="N+2")
  func=eval(fonction)
  return(func(N))
  
}


f2<-function(N){
  
  fonction<-parse(text=f1s)
  func=eval(fonction)
  arg<-as.numeric(N)
  return(func(arg))
  
}

op <- function(N, func) {  # passer arguments en string:  op("2", "ad")
  
  #arg<-N
  fonction<-parse(text=func)
  y<-eval(fonction)
 # arg<-as.numeric(N)
  #y = fonction(arg)
  return (y)
  
}

# ----------------    Fonctionnel    -------------------------------


# > N<-3
#
# > eval(parse(text="N+5"))
# [1] 7
# > eval(parse(text=f1s))
# (Intercept) 
# 0.1588809 

# ex 2--------------
# N<-3
#> func<-f1s
# > func
# [1] "MODS[[1]]$coefficients[ 1 ] + MODS[[1]]$coefficients[ 2 ]*N+ MODS[[1]]$coefficients[ 3 ]*N^2"
# > fonction<-parse(text=func)
# > fonction
# expression(MODS[[1]]$coefficients[ 1 ] + MODS[[1]]$coefficients[ 2 ]*N+ MODS[[1]]$coefficients[ 3 ]*N^2)
# > eval(fonction)
# (Intercept) 
# 0.1530908 

#--------------------------------------------------------------------
