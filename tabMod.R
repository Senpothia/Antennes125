#Test tableau de modèles

#Chargement des données
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
  
  # LN: Inductance vs N paramétrée en F
  j<-1
  
  for(i in frequencies){
    
    
    VAL<-TAB[TAB$F == i,]
    modL <- lm(VAL$L~VAL$N+I(VAL$N^2), data=VAL)
    MODSLN[[j]]<-modL
    j<-j+1
    
  }
  names(MODSLN)<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  
  # RN: Résistance vs N paramétrée en F
  
  j<-1
  
  for(i in frequencies){
    
    
    VAL<-TAB[TAB$F == i,]
    modR <- lm(VAL$R~VAL$N+I(VAL$N^2), data=VAL)
    MODSRN[[j]]<-modR
    j<-j+1
    
  }
  
  names(MODSRN)<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  # ---------------    Courbes paramétrées par N   -------------------------------
  # LF: Inductance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL<-TAB[TAB$N == i,]
    modL <- lm(VAL$L~VAL$F+I(VAL$F^2), data=VAL)
    MODSLF[[j]]<-modL
    j<-j+1
    
  }
  
  names(MODSLF)<-c("N=60", "N=80", "N=100", "N=120")
  
  # RF: Résistance vs F paramétrée en N
  
  j<-1
  
  for(i in nTypes){
    
    
    VAL<-TAB[TAB$N == i,]
    modR <- lm(VAL$R~VAL$F+I(VAL$F^2), data=VAL)
    MODSRF[[j]]<-modR
    j<-j+1
    
  }
  names(MODSRF)<-c("N=60", "N=80", "N=100", "N=120")
  
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
  noms<-c()
  noms1<-c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100")
  noms2<-c("N=60", "N=80", "N=100", "N=120")
  
  if(groupe == "LN"){
    
    gr<-"MODSLN"
    noms<-noms1
  }
  
  if(groupe == "RN"){
    
    gr<-"MODSRN"
    noms<-noms1
  }
  
  if(groupe == "LF"){
    
    gr<-"MODSLF"
    noms<-noms2
  }
  
  
  if(groupe == "RF"){
    
    gr<-"MODSRF"
    noms<-noms2
    
  }
  
  # MDS<-parse(text=gr)
  # MDS<-eval(MDS)
  
  i<-1
  for(m in models[[gr]]){
    
    est<-paste(as.character(m$coefficients[1]), "+", as.character(m$coefficients[2]), "*x +", as.character(m$coefficients[3]),  "*x^2")
    EST[[i]]<-est
    i<-i+1
    
  }
  names(EST)<-noms
  return(EST)
  
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

# models: liste de modèles: RN, LN, LF ou RF
# fourni pour l'ensemble d'une liste de modèles les coefficients triés par dégré
# dégré: 0, 1, 2 du polynôme de dégré 2

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

# Conversion de la liste de paramètres d'un groupe en une matrice de paramètres listés en colonnes
# coef: liste issue de getParams()pour un des groupes: RN, LN, RF, LF

getMatParams<-function(coef){
  
  
  MAT <- matrix(unlist(coef), ncol = 3, byrow = TRUE)
  
  if(dim(MAT)[1] == 4){
    
    dimnames(MAT) <- list(c("N=60", "N=80", "N=100", "N=120"), c("D0", "D1", "D2"))
  }
  
  if(dim(MAT)[1] == 7){
    
    dimnames(MAT) <- list(c("F=10", "F=20", "F=28.5", "F=40", "F=50", "F=66.6", "F=100"), c("D0", "D1", "D2"))
  }
  
  return(MAT)
  
}

# Déduit les modèles sur les paramètres à partir d'une matrice de paramètres
# matrice: matrice issue d'un groupe RN, RL, LN, ou LF via getMatParams()
# abscisse: vecteur des valeurs de fréquences F ou de nbre de spires N qui ont servis aux mesures
# abscisse est le paramètre du réseau de courbes étudié: F pour RN, LN; N pour RF, LF

paraModsRegs<-function(matrice, abscisse){
  
  l<-list()
  
  for(i in 1:3){
    
    v<-vector()
    coefs<-matrice[,i]
    m <- lm(coefs~abscisse+I(abscisse^2), data=data.frame(coefs,abscisse))
    v[1]<-m$coefficients[1]
    v[2]<-m$coefficients[2]
    v[3]<-m$coefficients[3]
    l[[i]]<-v
  }
  MAT <- matrix(unlist(l), ncol = 3, byrow = TRUE)
  dimnames(MAT) <- list(c("Dgr0", "Dgr1", "Dgr2"), c("D0", "D1", "D2"))
  return(MAT)
  

}
#---------------   SCRIPT    ----------------------------------------------------------
# Retourne une liste de toutes les matrices de coefficients de groupes: RN, RF, LN, LF
# après regression sur l'ensemble des matrices de coefficients
# ex appel: regMods("data")
# data: fichier de données à traiter

regMods<-function(data){
  
  nomCoefs<-c("D0" , "D1", "D3")
  TAB<-getMeasures(data, ",", ".")
  N<-sort(unique(TAB$N))
  F<-sort(unique(TAB$F))
  
  MODS<-getModels(TAB)
  
  paramsLF<-getModparams(MODS, "LF")
  matLF<-getMatParams(paramsLF)
  dimnames(matLF) <- list(nTypes,nomCoefs)
  
  paramsRF<-getModparams(MODS, "RF")
  matRF<-getMatParams(paramsRF)
  dimnames(matRF) <- list(nTypes, nomCoefs)
  
  paramsLN<-getModparams(MODS, "LN")
  matLN<-getMatParams(paramsLN)
  dimnames(matLN) <- list(frequencies, nomCoefs)
  
  paramsRN<-getModparams(MODS, "RN")
  matRN<-getMatParams(paramsRN)
  dimnames(matRN) <- list(frequencies, nomCoefs)
  
  # liste de matrices contenant les coefficients des modèles de régression

   # print("----  LISTE DES COEFFICIENTS REELS   ------")
   # COEFS<-list(matLF, matLN, matRF, matRN)
   # names(COEFS)<-c("LF", "LN", "RF", "RN")
   # return(COEFS)
  

  print("----  REGRESSION SUR LES COEFFICIENTS REELS   ------")

  MLF<-paraModsRegs(matLF, nTypes)
  MRF<-paraModsRegs(matRF, nTypes)
  MRN<-paraModsRegs(matRN, frequencies)
  MLN<-paraModsRegs(matLN, frequencies)

  COEFS<-list(MLF, MRF, MRN, MLN)
  names(COEFS)<-c("LF", "RF", "RN", "LN")
  return(COEFS)
  
}



