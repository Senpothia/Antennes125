#Ensemble de fonctions destinées à traitées les données bruts issues des mesures
#Etablit les modèles de régression, les matrices de coefficients...
#Etablit les modèles de régressions pour les corrections à 125kHz
# Execute les graphes des modèles sur les coefficients de régression


library(ggplot2)
library(ggpubr)
library(rlang)

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

# Fournit la liste de toutes les listes de modèles RN, LN, RF, LF
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
  # N<-sort(unique(TAB$N))
  # F<-sort(unique(TAB$F))
  nTypes<-sort(unique(TAB$N))
  frequencies<-sort(unique(TAB$F))
  
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
 # MATRICE <- matrix(unlist(COEFS), ncol = 3, byrow = FALSE)
  print(COEFS)
  return(COEFS)
  
}

# Trace les courbes de régression pour les coefficients du modèle: RN, LN, RF, ou LF
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# intervalel: vecteurs de l'intervalle de mesures. En rapport avec points de mesures: frequencies ou nTypes
# ex:intervalle=c(60,120) ou intervalle=c(10, 100)

plotMODSParams<-function(matrice, intervalle){
  
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  m0<-MATRICE[,1]
  m1<-MATRICE[,2]
  m2<-MATRICE[,3]
  
  x<-seq(intervalle[1], intervalle[2], by=1)
  y0<-m0[1] + m0[2]*x + m0[2]*x^2
  y1<-m1[1] + m1[2]*x + m1[2]*x^2
  y2<-m2[1] + m2[2]*x + m2[2]*x^2
  plot(x,y0, type="l", col="red")
  lines(x,y1, col="blue")
  lines(x,y2, col="green")
  
}

# Trace les courbes de régression pour les coefficients du modèle: RN, LN, RF, ou LF
# Version ggplot. Représentations conjointes de toutes les courbes.
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# intervalel: vecteurs de l'intervalle de mesures. En rapport avec points de mesures: frequencies ou nTypes
# ex:intervalle=c(60,120) pour groupes xN ou intervalle=c(10, 100) pour les groupes xF
# Supprimer les graphes existants si les nouveaux ne sont pas enregsitrés dans le repertoire de sauvegarde.
# Sinon fermer Rstudio et relancer

plotMODSParams2<-function(matrice, intervalle){
  
  lab<-names(matrice)
  
  if(lab[1] == "LF"){labX<-"N tours"
                     titre<-"Inductance vs Fréquence"}
  if(lab[1] == "RF"){labX<-"N tours"
                     titre<-"Résistance vs Fréquence"}
  if(lab[1] == "LN"){labX<-"Fréquence"
                     titre<-"Inductance vs N tours"}
  if(lab[1] == "RN"){labX<-"Fréquence"
                     titre<-"Résistance vs N tours"}
  
  # saveFile<-paste("./plots/", lab[1],"_coefs_params.png",sep="")
  # print(saveFile)
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  m0<-MATRICE[,1]
  m1<-MATRICE[,2]
  m2<-MATRICE[,3]
  
  x<-intervalle[1]:intervalle[2]
  y0<-function(x){m0[1] + m0[2]*x + m0[2]*x^2}
  y1<-function(x){m1[1] + m1[2]*x + m1[2]*x^2}
  y2<-function(x){m2[1] + m2[2]*x + m2[2]*x^2}
  df<-data.frame(x, y0(x), y1(x), y2(x))
  p0<-ggplot(df,aes(x))+ stat_function(fun=y0, col="red") + ggtitle("Coefficient degré 0") + labs(x = labX, y = "d0") 
  #print(p0)
  p1<-ggplot(df,aes(x))+ stat_function(fun=y1, col="blue")+ ggtitle("Coefficient degré 1") + labs(x = labX, y = "d1") 
  #print(p1)
  p2<-ggplot(df,aes(x))+ stat_function(fun=y2, col="green")+ ggtitle("Coefficient degré 2") + labs(x = labX, y = "d2") 
  #print(p2)
 
  figure<-ggarrange(p0,p1,p2, heights = c(2, 2, 2),
            ncol = 1, nrow = 3, align = "v")
  
  figure<-annotate_figure(figure,
                  top = text_grob(titre, color = "black", face = "bold", size = 14)
  )
  
  print(figure)
 

  png(paste("./plots/", lab[1] ,"_coefs_params.png",sep=""))
  print(figure)
  dev.off()
  
}

# Génère l'estimateur d'après les coefficient de regressions des paramètres du modèle
# matrice: liste en matrice des coefficients de regression sur les coefficients extraits des données de mesures
# ex: 
# CS<-regMods("data")
# matrice= CS[1]
# Retourne l'équation Y(x) représentant la grandeur d'intérêt L ou N fonction de x (N ou F)
# Y(y) <-C(x) + B(x) * y + A(x) * y^2
# A, B, C facteur sous forme de polymônes de dégrés 2, résultat des régression sur les paramètres
# des modèles
# y: variable d'intérêt: N ou F
# x: paramètre: N pour le groupe xF; F pour le groupe xN
# les coefficients de Y dépendent de x. Chaque coefficient est un polynôme de degré 2

getEstimator<-function(matrice){
  
  
  
  MATRICE <- matrix(unlist(matrice), ncol = 3, byrow = FALSE)
  
  c<-MATRICE[,1]
  b<-MATRICE[,2]
  a<-MATRICE[,3]
  
  C<-paste(as.character(c[1]), "+", as.character(c[2]), "* x +", as.character(c[3]), "* x^2") 
  B<-paste(as.character(b[1]), "+", as.character(b[2]), "* x +", as.character(b[3]), "* x^2") 
  A<-paste(as.character(a[1]), "+", as.character(a[2]), "* x +", as.character(a[3]), "* x^2") 
  
  Y<-paste(C, "+", B, "* y", A, "* y^2") 
  print(Y)
  
}





