# --------------  TESTS DIVERS  ------------------------------------------------------

n<-c(1, 2, 3)
i<-1
f1s<-paste("MODS[[1]]$coefficients[", as.character(n[i]), "] + MODS[[1]]$coefficients[", as.character(n[i+1]), "]*N+ MODS[[1]]$coefficients[", as.character(n[i+2]),"]*N^2")

f<-function(N){  # Fonctonnel. Appel: f(2)
  
  fonction<-parse(text="N+2")
  func=eval(fonction)
  return(func)
  
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

# estimateur<-function(i, N){  # Exemple d'appel: estimateur(1, 80)
#     
#     #f1s<-paste("MODS[[1]]$coefficients[", as.character(n[i]), "] + MODS[[1]]$coefficients[", as.character(n[i+1]), "]*N+ MODS[[1]]$coefficients[", as.character(n[i+2]),"]*N^2")
#     #print(f1s)
#     return(MODS[[i]]$coefficients[1] + MODS[[i]]$coefficients[2]*N + MODS[[i]]$coefficients[3]*N^2)
#     
# }

estimByN<-function(i, N, groupe){ # Exemple d'appel: estimateur2(1, 80, "MODSR")
  
  MDS<-parse(text=groupe)
  MDLS<-eval(MDS)
  return(MDLS[[i]]$coefficients[1] + MDLS[[i]]$coefficients[2]*N + MDLS[[i]]$coefficients[3]*N^2)
  
}

estimByF<-function(i, F, groupe){ # Exemple d'appel: estimateur2(1, 80, "MODSR")
  
  MDS<-parse(text=groupe)
  MDLS<-eval(MDS)
  return(MDLS[[i]]$coefficients[1] + MDLS[[i]]$coefficients[2]*F + MDLS[[i]]$coefficients[3]*F^2)
  
}


#---------------------------------------------------------------------------------------------------

#  FONCTIONS DE GENERATION DE DONNEES

genRF<-function(a, b, c, r){
  
  x<-c(10,20,28.5,40,50,66.6,100)
  e<-rnorm(7)*4
  print(e)
  y = (a + b*x + c*x^2 + e)/r
  return(y)
  
}

genLF<-function(a, b, c, r){
  
  x<-c(10,20,28.5,40,50,66.6,100)
  e<-rnorm(7)*4
  print(e)
  y = (a + b*x + c*x^2 + e)/r
  return(y)
  
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


#frqs<-nrow(table(TAB$F)) # Nbre de fréquence de mesures
#echs<-nrow(table(TAB$ech)) # Nbre d'échantillons
#attributes(table(TAB$F)) # voir les attibuts d'un objet  
#v<-attributes(table(TAB$F))[2] #liste des fréquences - ne pas utiliser

# liste du nom des éléments contenus dans la liste
# names(MODS)

# Elément nommé MODSLF de la liste MODS
# MODS[["MODSLF"]]

# Conversion list->matrice
#Matrix_x <- matrix(unlist(coef), ncol = 3, byrow = FALSE)


#--------------------------------------------------------------------
