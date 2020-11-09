---
title: "Le rebond de la pandémie de la Covid-19 était-il prévisible?"
author: "P. Cuchot, P. Fournier, M. Faucher _ M2 MODE "
date: "07/11/2020"
output:
  html_document:
    self_contained: no
---

![](~/OneDrive/M2 BEE - MODE/MEPI/projet/logo.png){width=100px}


```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r, echo=FALSE}
setwd("~/OneDrive/M2 BEE - MODE/MEPI/projet")
```

# Introduction

Lors de l'annonce du premier confinement général en France le 17 mars 2020, les connaissances scientifiques ainsi que les relevés d'hospitalisation permettaient déjà la publication d’articles estimant le taux d’incidence de la Covid-19 (Roques et al., 2020), l’évolution de la pandémie selon les stratégies de déconfinement (Di Domenicali et al., 2020) ou encore des estimations du nombre de décès évités grâce au confinement (Roux et al., 2020). A l'occasion du deuxième confinement débuté le 30 octobre, il est intéressant de chercher à savoir si la situation actuelle pouvait être envisagée dès la levée du premier confinement. Nous avons cherché à savoir si un modèle épidémiologique réalisé à partir des données d’hospitalisation recueillies pendant la période du confinement permettait de prévoir l’accélération du nombre de cas débutée en juillet.

Pour répondre à cette question, un modèle de type SIHR (figure 1) a été ajusté aux données d’hospitalisation mises à disposition sur le site de Santé Publique France concernant l'épidémie de Covid-19 en France pendant la période de confinement. Les résultats obtenus avec ce modèle ont ensuite été comparés avec les données réelles, de la sortie du confinement à nos jours.

Le tri des données ainsi que le choix du modèle sont réalisés à la manière de F. Hamelin (<http://f.m.hamelin.free.fr/covid-19.html>)

![figure 1 : Schéma représentatif du modèle SIHR](~/OneDrive/M2 BEE - MODE/MEPI/projet/modele.png)

# Application

## Chargement des packages 

```{r packages}
library(rmarkdown)
library(deSolve)
library(ggplot2)
```

## Chargement des données


```{r}
setwd("~/OneDrive/M2 BEE - MODE/MEPI/projet")
dataH <- read.csv("./donnees-hospitalieres-covid19-2020-11-03-19h00.csv", sep = ";")
dataA <- read.csv("donnees-hospitalieres-nouveaux-covid19-2020-11-03-19h00.csv", sep = ";")

# on ne distingue pas les homme des femmes 
Z <- which(dataH[,2]==0)
dataH <- dataH[Z,]

# sélection de variables 
dataHH <- dataH[,c(1,4)]  # Département et nombre de personnes hospitalisées
dataAA <- dataA[,c(1,3)]  # Département et nombre d'admissions par jour
```

Comme le modèle est spatialement homogène, nous agrégerons les données à l'échelle de la France :

```{r}
n <- 95               # Nombre de département en France métropolitaine 
v <- c(1:19,21:95)    # Vecteur des départements Corse exclue
```

Pour récupérer la longueur des séries temporelles dans ces fichiers, on peut faire la gymnastique suivante :
```{r, include =FALSE}
ZH <- which(as.integer(as.character(dataH[,1]))==1);
LH <- length(dataH[ZH,2]);#longueur de la série H 

ZA <- which(as.integer(as.character(dataA[,1]))==1);
LA <- length(dataA[ZA,2]);#longueur de la série A
```

Ce qui nous permet de définir les matrices suivantes, que nous remplissons de 0 dans un premier temps :

```{r, include =FALSE}
HH <- matrix(0,n,LH)    # Matrice des hospitalisés (par département)
AA <- matrix(0,n,LA)    # Matrice des admissions
```

Nous pouvons ensuite construire ces matrices de la façon suivante :

```{r, include =FALSE}
for (i in v){ # v = vecteur avec numéros de département 
  ZH <- which(as.integer(as.character(dataHH[,1]))==i) ;
  HH[i,] <- dataHH[ZH,2] ;
  ZA <- which(as.integer(as.character(dataAA[,1]))==i) ;  
  AA[i,] <- dataAA[ZA,2] ;
}
```

Ce qui permet finalement de construire deux vecteurs qui contiennent uniquement les données qui nous intéressent :

```{r}
H <- matrix(0,1,LH)
for (i in 1:LH) H[i] <- sum(HH[,i])  # Hospitalisés
Htot <- H                            # on garde de coté l'ensemble des données 

A <- matrix(0,1,LA); 
for (i in 1:LA) A[i] <- sum(AA[,i])  # Admissions
Atot <- A                            # on garde de coté l'ensemble des données d'admission 
```

## Visualisation des données à partir du 19 mars :
```{r}
plot(1:LH,H,xlab="Temps écoulé depuis le 19 mars (en jours)",
     ylab="Nombre de personnes hospitalisées",col="blue")


plot(1:LA,A,xlab="Temps écoulé depuis le 19 mars (en jours)",
     ylab="Nombre d'admissions à l'hôpital (par jour)",col="red", xlim=c(0,50))

```

Comme le confinement a débuté le 17 mars, la tendance croissante des deux premières  semaines suit probablement une dynamique de transition suite à la mise en place du confinement. 

Comme le modèle épidémiologique suppose que les paramètres épidémiologiques sont constants, en particulier le taux de transmission du virus, nous commencerons les analyses 15 jours après le début du confinement, soit à partir du 1er avril :

```{r,include =FALSE}
T0 <- 13                # Décalage du point de départ 13 jours après le 19 mars (le 1er avril) 
Htot <- H[T0:length(H)] # On conserve l'ensemble des données d'hospotalisation
H <- H[T0:53]           # Troncations des données (du 1 avril au 11 mai = confinement)

Atot <- A[T0:length(A)] # On conserve l'ensemble des données d'admission
A <- A[T0:53] 
LH <- length(H)         # Mise à jour des longeurs des séries temporelles
LA <- length(A)
```

## Visualisation des données tronquées à partir du 1er avril :
```{r}
plot(1:LH,H,xlab="Temps écoulé depuis le 1er avril (en jours)",
     main="Nombre de personnes hospitalisées \nen bleu, admises en noir",
     col="blue", ylim = c(1,35000), type='l')
points(1:LA,A, type='l')
```

# Simulation et ajustement du modèle aux données 

```{r}
N <- 64e6                     # Population hexagonale approximative
rho <- 1/10.91                # Temps moyen avant guérison ou décès (durée contagiosite) : 10.91 jours
gamma <- 1/14                 # Durée moyenne de l'hospitalisation : 14 jours
R_0 <- 1                      # Nombre de reproduction de base en confinement
beta <- R_0*rho               # Taux de transmission (calcul très approximatif)
p <- 0.1                      # Probabilité d'être hospitalisé suite à l'infection
alpha <- p*rho/(1-p)          # Taux d'hospitalisation
P0 <- c(beta,alpha,gamma)     # Vecteur des paramètres
```

Définissons ensuite les variables du modèle et les conditions initiales (au 1er avril) :

```{r}
H0 <- H[1]        # Le nombre de personnes hospitalisées au 1er avril
I0 <- A[1]/alpha  # Les admissions correspondent à A(t) = alpha*I(t)
R0 <- N*0.01      # C'est la grande inconnue : mettons 1% d'immunisés 
S0 <- N-I0-H0-R0  # La taille de la population sensible au 1er avril
X0 <- c(S0,I0,H0) # Vecteur d'état. Pas besoin de simuler R=N-(S+I+H)

#Nous aurons besoin d'un vecteur contenant les dates sur lesquelles comparer modèle et données :
t <- 0:(LH-1)
```

Définissons la fonction SIHR qui prend en arguments 3 vecteurs : temps, variables d'états, et paramètres :
```{r}
SIHR = function(t, X, P){
  beta <- P[1];            # Le taux de transmission
  alpha <- P[2];           # Le taux d'hospitalisation
  gamma <- P[3];           # Le taux de sortie d'hôpital
  
  S <- X[1]
  I <- X[2]
  H <- X[3] # Le vecteur d'état X contient: S, I, et H
  
  y = beta*S*I/N;       # Le nombre de nouvelles infections par jour
  
  dS = -y;              # On exprime dS/dt = - beta*S*I
  dI = y-(alpha+rho)*I; # On exprime dI/dt = beta*S*I - (alpha+rho)*I
  dH = alpha*I -gamma*H;# On exprime dH/dt = alpha*I - gamma*H
  
  dX=c(dS,dI,dH);       # Renvoie dX/dt tel que demandé par la fonction ode
  
  return(list(dX));
}
```

## Calcul du log de la vraisemblance

```{r}
logLike=function(theta){
  P <- theta[1:3];           # Les paramètres beta (tx trnasmission), alpha (tx hospit.), et gamma (taux sortie hopital)
  X0 <- theta[4:6];          # Mise à jour des conditions initiales 
  
  X <- ode(X0,t,SIHR,P)      # Résolution du système d'EDO (modèle SIHR)
  
  h <- X[,4];                # Hospitalisation théoriques : H(t)
  a <- P[2]*tail(X[,3],-1);  # Admissions théoriques : alpha*I(t)
  
  LLH <- dpois(H,h,log=T)    # Probabilité d'observer H (loi de Poisson)
  LLA <- dpois(A,a,log=T)    # Probabilité d'observer A (Poisson)
  LL <- sum(c(LLH,LLA))      # Log transforme produit des probas en somme
  return(LL);                # Renvoie la log-vraisemblance (likelihood)
}
```

Avant d'appeler les fonctions ci-dessus, on définit le vecteurs des paramètres et conditions initiales :
Ici, P0 = c(beta,alpha,gamma) et X0 = conditions initiales 

```{r}
theta0 <- c(P0,X0)
```

Utilisons la fonction optim pour trouver les paramètres et conditions initiales qui maximisent la vraisemblance du modèle.

```{r,include =FALSE}
opt <- optim(theta0,logLike,control=list(fnscale=-1)) # Maximise logLike 
```

On récupère les paramètres et conditions initiales dont la vraisemblance est maximale :

```{r}
beta <- opt$par[1]   # beta (tx transmission)
alpha <- opt$par[2]  # alpha (tx hospit.)
gamma <- opt$par[3]  # gamma (taux sortie hopital)
```

Les paramètres des paramètres optimaux correspondent à :  
**`r opt$par[1]`** pour le taux de transmission.  
**`r opt$par[2]`** pour le taux de d'hospitalisation.  
**`r opt$par[3]`** pour le taux de sortie de l'hopital.  

Selon le modèle, les conditions initiales optimales sont : 
```{r}
S0 <- opt$par[4]
I0 <- opt$par[5]
H0 <- opt$par[6]
```

On met à jour les vecteurs des paramètres et conditions initiales :

```{r}
X0 <- c(S0,I0,H0);                 # Vecteur des conditions initiales
P0 <- c(beta,alpha,gamma);         # Vecteur des paramètres mis à jour
```


## On calcule la solution du modèle pour les paramètres et conditions initiales estimés :

```{r}
T=219 
t=0:T      # Mise à jour du vecteur temps
```


Calcul de la solution optimale

```{r, include=FALSE}
X=ode(X0,t,SIHR,P0)  
```


##On compare visuellement la solution du modèle et les observations :
```{r}

plot(0:(LH-1),H,xlab="Temps écoulé depuis le 1er avril (en jours)",
     main ="Nombre de personnes hospitalisées \n En bleu la période de confinement",
     col="blue", ylim=c(0,50000), xlim = c(0,230));

lines(X[,1],X[,4]); # affichage de la simulation 

points(42:length(Htot),Htot[42:length(Htot)], col=2)# affichage du reste des données 

plot(0:(LA-1),A,xlab="Temps écoulé depuis le 1er avril (en jours)",
     ylab="Nombre de nouvelles hospitalisations (par jour)",col="red");
lines(X[,1],alpha*X[,3])
```

# Utilisation du même modèle en sortie de confinement induisant une hausse du taux de transmission

Dans cette partie, nous utliserons le même modèle aux paramètres optimisés, en ne modifiant uniquement la valeur du taux de transmission. Pour cela nous réalisons plusieurs simulations pour des valeurs de BETA différentes. 

On commence par créer une liste contenant les simulations pour chaque valeur de beta

```{r}
X_list <- list()
```

Les conditions initiales correspondent aux valeurs données en sortie du modèle précédentn au jour du déconfinement

```{r}
S0 <- X[43,2]
I0 <- X[43,3]
H0 <- X[43,4]

X0 <- c(S0,I0,H0)
```

On récupère les paramètres optimaux, en modifiant le taux de transmission

```{r}
i <- 1 # Compteur nécessaire pour remplir X_list
for(beta in seq(0.064,0.15,by=0.01)){
  
  alpha <- opt$par[2]         # alpha (tx hospit.)
  gamma <- opt$par[3]         # gamma (taux sortie hopital)
  
  P0 <- c(beta,alpha,gamma);         # Vecteur des paramètres mis à jour
  
  # On calcule la solution du modèle pour les paramètres et conditions initiales estimés :
  
  T=219
  t=0:T            # Mise à jour du vecteur temps
  
  X_list[[i]]=ode(X0,t,SIHR,P0)    # Calcul de la solution 
  i <- i+1
}
```

Affiche le nombre d'individus hospitalisés (données et modèle)

```{r}
plot(43:length(Htot),Htot[43:length(Htot)],xlab="Temps écoulé depuis le 1 Avril (en jours)",
     main ="Nombre de personnes hospitalisées \n En bleu la période de confinement",
     col="blue", ylim=c(0,50000), xlim = c(0,230))

# On decale les données

points(1:43,Htot[1:43], col=1)

for (i in 1:length(X_list)){
  X_list[[i]][,1] <- X_list[[i]][,1]+44
  lines(X_list[[i]][,1],X_list[[i]][,4], col=i) # affichage de la simulation
}
```






