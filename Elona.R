
# IMPORTATION DES LIBRARIES ----
install.packages("readxl")
install.packages("lme4")
install.packages("mgcv")
library(lme4)
library(readxl)
library(nlme)
library(lmerTest)
library(mgcv)
library(knitr)
opts_chunk$set(echo = FALSE, comment = "", cache = TRUE, fig.align = "center")
library(ggplot2) # graph package
library(tinytex) # Pour la sortie pdf
library(corrplot)# Correlation matrix calculus
library(plot3D)# For 3D plot
library(MASS)# Perform NB GLM
library(DHARMa)# Model diagnosis
library(rcompanion)# Model pseudo R²
library(lattice)# multipanel graphics

# IMPORTATION DES DONNEES ----

elona_recapture <- read.csv2("recapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)
elona_firstcapture <- read.csv2("firstcapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)

# Importer la feuille 2 pour rajouter les sites

elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),as.character(elona_recapture$Month),as.character(elona_recapture$Day), sep = "-"))
elona_firstcapture$Date <- as.Date(paste(as.character(elona_firstcapture$Year),as.character(elona_firstcapture$Month),as.character(elona_firstcapture$Day), sep = "-"))

# EXPLORATION DES DONNEES ----

## 1. Valeurs extrêmes ----

# valeur abérante dans Y
boxplot(elona_recapture$Diameter)
summary(elona_recapture$Diameter)
# On a des valeurs très faibles mais ça me parait pas improbable

boxplot(elona_recapture$Peristome) # Une valeur très très haute => on va l'enlever ça parait improbable
summary(elona_recapture$Peristome)
elona_recapture$Peristome[which.max(elona_recapture$Peristome)] <- NA

boxplot(elona_recapture$Peristome) # On en a encore 3 grandes, je sais pas trop si c'est probable ou pas
elona_recapture$Peristome[elona_recapture$Peristome>15]<-NA
summary(elona_recapture$Peristome)


hist(elona_recapture$Diameter)
hist(elona_recapture$Peristome,breaks=1000) # pas normal
summary(elona_recapture$Peristome)

## 2. Nettoyage des données ----

table(elona_recapture$Site)
table(elona_recapture$Stage)# Nombre de modalités dans "Stage" va pas, pareil dans activity 
# On a des catégories (?, JUV? JUV/SUB, SUB/AD) où on a très peu d'individus, passer en NA ou dans une autre classe
elona_recapture$Stage[which(elona_recapture$Stage=="?")] <- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV?")]<- "JUV"
# Pour les deux autres on peut les passer en NA ou alors trancher mais on a aucunes données pour trancher
elona_recapture$Stage[which(elona_recapture$Stage=="JUV/SUB")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="SUB/AD")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV ")]<- "JUV"
table(elona_recapture$Stage)


table(elona_recapture$Activity)
elona_recapture$Activity[which(elona_recapture$Activity=="ACT ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT  ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT   ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="IMM")]<- "IMMO"
elona_recapture$Activity[which(elona_recapture$Activity=="IMMO ")]<- "IMMO"
elona_recapture$Activity[which(elona_recapture$Activity=="MORT ")]<- "MORT"
table(elona_recapture$Activity)

table(elona_recapture$Type_Event)
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R ")]<- "R"
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R  ")]<- "R"
table(elona_recapture$Type_Event)

# Ajouter données de sites à elona_recapture
elona_recapture<-merge(elona_recapture,elona_firstcapture[,c(10,11)], by="ID_1erC")

# Relation potentielle entre X et Y
boxplot(elona_recapture$Diameter~elona_recapture$Site)
boxplot(elona_recapture$Peristome~elona_recapture$Site)
# Recent a l'air plus haut

## 3. Analyse des interactions ----
# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte

## 4. Analyse des colinéarités ----
# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup




m1<-lmer(Diameter ~ Site + Stage +(1|Date)+(1|ID_1erC)+(1|IDZone/Site),data=elona_recapture)

# CONDITIONS DE VALIDITE ----

## 1. Normalité des résidus ----
plot(m1) # résidus bof

## 2. Homoscédasticité ----
plot(x = elona_recapture$Date, y = elona_recapture$Diameter )
plot(x = elona_recapture$Date, y = elona_recapture$Peristome )

## 3. Résidus indépendants ----

# Significativité de la variance associé au facteur aléatoire
intervals(m1) # Fonctionne pas doit être adapté à lme pas lmer

# ANALYSE ----
summary(m1)
anova(m1) # effet significatif du site

# Choix de la méthode pour répondre à la question :
# We aim to determine whether the two populations share the same demographic characteristics and traits.

## 1. Diameter ----
# Modèle linéaire général mixte
# Diameter = Site + Stage + (ID + date)
m_diam<-lmer(Diameter ~ Site + Stage + (1|ID_1erC) + (1|Date), data=elona_recapture)
plot(m_diam)


## 2. Peristome ----
# Transformation de Peristome pour lui faire suivre une loi normale
m_peris <- lmer(log(Peristome) ~ Site + Stage + (1|ID_1erC) + (1|Date), data=elona_recapture)

# Vérification des conditions de validité
plot(m_peris) # Résidus répartis de manière non normale

# Autres conditions de validités !!!!


# GAM
# On transforme Date en temps depuis le début de l'expérience parce que les GAM n'acceptent pas les données Dates
temps <- elona_recapture[,12] - min(elona_recapture$Date)
elona_recapture$temps<-temps
elona_recapture$temps<-as.numeric(elona_recapture$temps)
class(elona_recapture$temps)
hist(elona_recapture_sansNA$temps)

# On enlève les NA, ça permet d'éviter les messages d'erreurs lors de la vérification de l'homogénéité des variances
elona_recapture_sansNA<-na.omit(elona_recapture)

# Le modèle GAM
gam_peris <- mgcv::gam(Peristome ~ Site + Stage + s(ID_1erC,bs="re")+ s(temps,bs="re"),
          family = gaussian,
          data = elona_recapture_sansNA)
# regarder pourquoi on doit indiquer gaussian 
# à voir aussi il est possible de mettre bs = "re" ou bs = "cr"

# Analyse des résultats
anova(gam_peris)

summary(gam_peris)

# Vérification des conditions de validité
par(mar = c(2, 2, 2, 2))
vis.gam(gam_peris, theta = -20, color = "heat") # fonctionne pas

par(mfrow=c(1,2))
# Histogram
hist(gam_peris$residuals,col='dodgerblue3',xlab="residuals",main="Check Normality")
# Quantile-Quantile plot
qqnorm(gam_peris$residuals,pch=16,col='dodgerblue3',xlab='')
qqline(gam_peris$residuals,col='red')
# Pas terrible pour le deuxième graph

# Homogénéité de la variance
plot(residuals(gam_peris)~fitted(gam_peris)
     , col='dodgerblue3'
     , pch=16)
abline(h = 0)
# Plus de variance au centre qu'aux extrémités

plot(residuals(gam_peris)~ elona_recapture_sansNA$Site, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Sites",
     main = "")
abline(h = 0)

plot(residuals(gam_peris)~ elona_recapture_sansNA$temps, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Temps",
     main = "")
abline(h = 0)

plot(residuals(gam_peris)~ elona_recapture_sansNA$Stage, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Stage",
     main = "")
abline(h = 0)

plot(residuals(gam_peris)~ elona_recapture_sansNA$ID_1erC, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Individus",
     main = "")
abline(h = 0)


## 3. Démographie ----


# Limite de l'étude

# Fait qu'on ne sait pas si les placettes ont des ressources très différentes ou pas
# Le jeu de données ne nous permet pas de prendre en compte si les escargots bougent ou pas

# Que des psuedo-réplicats ce serait bien d'avoir d'autres sites.
# De plus, rien n'indique que ces sites sont équivalents en termes de ressources alimentaires, qualité d'habitats
# Pour réellement étudier l'effet de la densité de population sur les caractéristiques morphologiques il faudrait réaliser une étude démographique et utiliser la densité de population en varaible explicative
