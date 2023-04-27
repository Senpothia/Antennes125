#Test tableau de modèles

#TAB<-read.table("./data/mesures.csv",header=TRUE,sep=";",dec=",")
#TAB<-read.table("./data/data.csv",header=TRUE,sep=",",dec=".")

# Load data file for measurements and factors corrections

getMeasures<-function(file, s=";", d=","){
  
  file<-paste("./data/",file, sep="")
  file<-paste(file,".csv", sep="")
  TAB<-read.table(file,header=TRUE,sep=s,dec=d)
  return(TAB)
  
}

# Fourni la liste de toutes les listes de modèles RN, LN, RF, LF
# liste de liste
# RN, LN... sont des listes
# Accès à une liste: MODS["MODSLF"] avec MODS la liste des modèles issue de getModels()

getModels<-function(TAB){
  
  MODSLN<-list()
  MODSRN<-list()
  
  MODSLF<-list()
  MODSRF<-list()
  
  nameList<-c("MODSLN", "MODSRN", "MODSLF", "MODSRF")
  
  frequencies<-sort(unique(TAB$F))     # Listes des fréquences 
  echs<-sort(unique(TAB$ech))
  nTypes<-sort(unique(TAB$N))
  
  
  # pour F=f0 
  # 1- L=f(N)
  # 2- R=f(N)
  
  
  #pour N=no 
  # 1- L=g(F)
  # 2- R=g(F)
  
  
  #---------------------    Courbes paramétrées par F      ------------------------------
  
  # Inductance vs N paramétrée en F
  j<-1
  
  for(i in frequencies){
    
    
    VAL=TAB[TAB$F == i,]
    modL <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
    MODSLN[[j]]<-modL
    j<-j+1
    
  }
  
  # Résistance vs N paramétrée en F
  
  j<-1
  
  for(i in frequencies){
    
    
    VAL=TAB[TAB$F == i,]
    modR <- lm(VAL$R~VAL$N+I(VAL$N^2), data=VAL)
    MODSRN[[j]]<-modR
    j<-j+1
    
  }
  
  # ---------------    Courbes paramétrées par N   -------------------------------
  # Inductance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL=TAB[TAB$N == i,]
    modL <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
    MODSLF[[j]]<-modL
    j<-j+1
    
  }
  
  
  # Résistance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL=TAB[TAB$N == i,]
    modR <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)
    MODSRF[[j]]<-modR
    j<-j+1
    
  }
  
  MODS<-list(MODSLF, MODSRF, MODSLN, MODSRN)
  names(MODS)<-c("MODSLF", "MODSRF", "MODSLN", "MODSRN")
  return(MODS)
  
}

# Fourni la liste des fonctions de régression d'un groupe pour un ensemble de modèles
# models possède 4 groupes:LN, RN, LF, RF
# l'appel se fait sur un groupe: LN, RN, LF, RF

estimateurs<-function(models, groupe){  # ex appel: 
                                        #           M<-getModels(TAB)
                                        #           estimateurs(M, "LN")
  
  EST<-list()
  
  # groupes: LN, RN, LF, RF
  # models: liste de tous les models d'après le jeu de données traité
  
  if(groupe == "LN"){
    
    gr<-"MODSLN"
  }
  
  if(groupe == "RN"){
    
    gr<-"MODSRN"
  }
  
  if(groupe == "LF"){
    
    gr<-"MODSLF"
  }
  
  
  if(groupe == "RF"){
    
    gr<-"MODSRF"
    
  }
  
  # MDS<-parse(text=gr)
  # MDS<-eval(MDS)
  
  i<-1
  for(m in models[[gr]]){
    
    est<-paste(as.character(m$coefficients[1]), "+", as.character(m$coefficients[2]), "*x +", as.character(m$coefficients[3]),  "*x^2")
    EST[[i]]<-est
    i<-i+1
    
  }
  
  return(EST)
  
  #return(models[[gr]])
  
  
}

# Obtention listes des paramètres de modèles pour un groupe: RN, LN, LF, RF 
# contenu dans un ensemble désigné par models

getModparams<-function(models, groupe){  # ex appel:
                                      #           M<-getModels(TAB)
                                      #           getModparams(M, "LN")
  
  PAR<-list()
  
  # groupes: LN, RN, LF, RF
  # models: liste de tous les models d'après le jeu de données traité
  
  if(groupe == "LN"){
    
    gr<-"MODSLN"
  }
  
  if(groupe == "RN"){
    
    gr<-"MODSRN"
  }
  
  if(groupe == "LF"){
    
    gr<-"MODSLF"
  }
  
  
  if(groupe == "RF"){
    
    gr<-"MODSRF"
    
  }
  
  #MODS[["MODSLF"]][1]
  # |       |       |
  # liste   liste   |
  #         interne |
  #                 indice dans la liste interne        
  
  i<-1
  for(m in models[[gr]]){
    mods<-list()
    for(j in 1:3){
      
      mods[j]<-m$coefficients[j]
      
      
    }
  
    PAR[[i]]<-mods
    i<-i+1
   
  }
  
 
  return(PAR)
  
  
}

# Tri des paramètres

# étapes pour l'appel:
# > TAB<-getMeasures("data", ",", ".")
# > MODS<-getModels(TAB)
# > models<-getModparams(MODS, "RF")

# models: liste de modèles: RN, LN, LF, RF
# fourni pour l'ensemble d'une liste de modèles les coeffients triés par dégré
# dégré 0, 1, 2 du polynôme de dégré 2

getParams<-function(models){  #mod
  
  PARAMS<-list()
  D0<-list()
  D1<-list()
  D2<-list()
  
  i<-1
  for(m in models){ 
  
      D0[[i]]<-m$coefficients[1]
      D1[[i]]<-m$coefficients[2]
      D2[[i]]<-m$coefficients[3]
      i<-i+1

    
  }
  PARAMS[[1]]<-D0
  PARAMS[[2]]<-D1
  PARAMS[[3]]<-D2
  
  names(PARAMS)<-c("D0", "D1", "D2")
  return(PARAMS)
  
  
}

# Conversion de la liste de pramètres en une matrice de paramètres listés en colonnes
# coef: liste issue de getParams()

getMatParams<-function(coef){
  
  MAT <- matrix(unlist(coef), ncol = 3, byrow = FALSE)
  return(MAT)
  
}

#---------------  SCRIPT    ----------------------------------------------------------

recherche<-function(){
  
  TAB<-getMeasures("data", ",", ".")
  MODS<-getModels(TAB)
  paramsLF<-getModparams(MODS, "LF")
  matLF<-getMatParams(paramsLF)
  
  paramsRF<-getModparams(MODS, "RF")
  matRF<-getMatParams(paramsRF)
  
  paramsLN<-getModparams(MODS, "LN")
  matLN<-getMatParams(paramsLN)
  
  paramsRN<-getModparams(MODS, "RN")
  matRN<-getMatParams(paramsRN)
  
  COEFS<-list(matLF, matLN, matRF, matRN)
  names(COEFS)<-c("LF", "LN", "RF", "RN")
  return(COEFS)
  
}



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
