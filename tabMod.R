#Test tableau de modèles

TAB<-read.table("./data/mesures.csv",header=TRUE,sep=";",dec=",")

mod1 <- lm(TAB$Ln~TAB$N+I(TAB$N^2), data=TAB)
mod2 <- lm(TAB$L1~TAB$N+I(TAB$N^2), data=TAB)
mod3 <- lm(TAB$L2~TAB$N+I(TAB$N^2), data=TAB)
mod4 <- lm(TAB$L3~TAB$N+I(TAB$N^2), data=TAB)

MODSL<-list(mod1, mod2, mod3, mod4)


mod5 <- lm(TAB$Rn~TAB$N+I(TAB$N^2), data=TAB)
mod6 <- lm(TAB$R1~TAB$N+I(TAB$N^2), data=TAB)
mod7 <- lm(TAB$R2~TAB$N+I(TAB$N^2), data=TAB)
mod8 <- lm(TAB$R3~TAB$N+I(TAB$N^2), data=TAB)

MODSR<-list(mod5, mod6, mod7, mod8)

n<-c(1, 2, 3)
i<-1
f1s<-paste("MODS[[1]]$coefficients[", as.character(n[i]), "] + MODS[[1]]$coefficients[", as.character(n[i+1]), "]*N+ MODS[[1]]$coefficients[", as.character(n[i+2]),"]*N^2")

f<-function(N){
  
  fonction<-parse(text="N+2")
  func=eval(fonction)
  return(func(N))
  
}


f2<-function(N){        # Fonctionnel. Appel:  f2(10). f1s est une expression en string
                        # f1s <- "MODS[[1]]$coefficients[ 1 ] + MODS[[1]]$coefficients[ 2 ]*N+ MODS[[1]]$coefficients[ 3 ]*N^2"
  
  fonction<-parse(text=f1s)
  y<-eval(fonction)
  return (y)


}

op <- function(N, func) {  # Fonctionnel. Appel: op(60, f1s) f1s est une expression en string
                           # f1s <- "MODS[[1]]$coefficients[ 1 ] + MODS[[1]]$coefficients[ 2 ]*N+ MODS[[1]]$coefficients[ 3 ]*N^2"
  

  fonction<-parse(text=func)
  y<-eval(fonction)
  return (y)
  
}

estimateur<-function(i, N){  # Exemple d'appel: estimateur(1, 80)
    
    #f1s<-paste("MODS[[1]]$coefficients[", as.character(n[i]), "] + MODS[[1]]$coefficients[", as.character(n[i+1]), "]*N+ MODS[[1]]$coefficients[", as.character(n[i+2]),"]*N^2")
    #print(f1s)
    return(MODS[[i]]$coefficients[1] + MODS[[i]]$coefficients[2]*N + MODS[[i]]$coefficients[3]*N^2)
    
}

estimateur2<-function(i, N, groupe){ # Exemple d'appel: estimateur2(1, 80, "MODSR")
  
  MDS<-parse(text=groupe)
  MDLS<-eval(MDS)
  return(MDLS[[i]]$coefficients[1] + MDLS[[i]]$coefficients[2]*N + MDLS[[i]]$coefficients[3]*N^2)
  
}



# ----------------    Exemples Fonctionnels    -------------------------------


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
