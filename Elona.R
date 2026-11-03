# IMPORTATION DES LIBRARIES ----------------------------------------------------
#install.packages("readxl")
#install.packages("lme4")
#install.packages("mgcv")
library(lme4)
library(readxl)
library(GGally)
library(ggcorrplot) #graphe de correlation
library(dplyr)
library(ggplot2)
library(tidyverse)
library(predictmeans)#diagramme de cook
library(nlme)
library(lmerTest)
library(mgcv)
library(corrplot) # Correlation matrix calculus
library(lattice) # multipanel graphics
library(ade4) # PCA


# IMPORTATION DES DONNEES ------------------------------------------------------

# Importer la feuille 2 pour rajouter les sites
elona_recapture <- read.csv2("recapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)
elona_firstcapture <- read.csv2("firstcapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)


#Modification des dates
elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),as.character(elona_recapture$Month),as.character(elona_recapture$Day), sep = "-"))

# EXPLORATION DES DONNEES ------------------------------------------------------

## 1. Valeurs extrêmes ----

#Diametre
boxplot(elona_recapture$Diameter)
summary(elona_recapture$Diameter)
# On a des valeurs très faibles mais ça ne parait pas improbable


#Peristome
boxplot(elona_recapture$Peristome) # Une valeur très très haute (901mm) => on va l'enlever cela parait improbable
summary(elona_recapture$Peristome)
elona_recapture$Peristome[which.max(elona_recapture$Peristome)] <- NA

boxplot(elona_recapture$Peristome) # On en a encore 3 grandes valeurs, on ne sait pas si c'est probable ou pas
summary(elona_recapture$Peristome)
#Les 3 points correspondent à des valeurs de péristome plus larges que la taille de l'escargot (diamètre), donnant lieu à un paradoxe biologique
#De plus, ces valeurs potentiellement aberrantes ne sont pas en raccord avec d'autres mesures de diamètre et peristome sur Elona quimperiana, 
#comme l'article DAGUZAN J. & GLOAGUEN J.-C., 1986.- Contribution à l’écologie d’Elona quimperiana (de Férussac) (Gastéropode Pulmoné Stylommatophore) en Bretagne occidentale. Haliotis, 15 : 17-30.
#Nous choississons donc de remplacer ces trois valeurs par NA.
elona_recapture$Peristome[elona_recapture$Peristome>15] <- NA

## 2. Normalité de Y----

hist(elona_recapture$Peristome,breaks=1000) 
hist(elona_recapture$Peristome,breaks=1000) 
summary(elona_recapture$Peristome)
hist(elona_recapture$Diameter,col='chartreuse',xlab="Diameter",main="Check Normality
 (Diameter) ")
hist(elona_recapture$Peristome,col='purple',xlab="Peristome",main="Check Normality
(Peristome)")
# Ni le Diamètre, Ni le Péristome ne semblent suivrent une loi normale.


## 3. Nettoyage des données X ----

table(elona_recapture$Site)#avec summary, on a la meme chose avec le nb de NA en +
table(elona_recapture$Stage)# Nombre de modalités dans "Stage" va pas, pareil dans activity 
# On a des catégories (?, JUV? JUV/SUB, SUB/AD) où on a très peu d'individus, passer en NA ou dans une autre classe
elona_recapture$Stage[which(elona_recapture$Stage=="?")] <- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV?")]<- "JUV"

# Pour les deux autres on peut les passer en NA ou alors trancher mais on a aucunes données pour trancher, on les passe donc en NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV/SUB")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="SUB/AD")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV ")]<- "JUV"
table(elona_recapture$Stage)

#On supprime également les catégories non conformes 
elona_recapture$Stage = droplevels(elona_recapture$Stage)
table(elona_recapture$Stage)



#Même chose pour les modalités de "Activity"
table(elona_recapture$Activity)
elona_recapture$Activity[which(elona_recapture$Activity=="ACT ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT  ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT   ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="IMM")]<- "IMMO"
elona_recapture$Activity[which(elona_recapture$Activity=="IMMO ")]<- "IMMO"

elona_recapture$Activity[which(elona_recapture$Activity=="MORT ")]<- "MORT"
table(elona_recapture$Activity)
#On supprime également les catégories non conformes 
elona_recapture$Activity = droplevels(elona_recapture$Activity)
table(elona_recapture$Activity)




#Même chose pour les modalités de "Type_Event"
table(elona_recapture$Type_Event)
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R ")]<- "R"
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R  ")]<- "R"
table(elona_recapture$Type_Event)
#On supprime également les catégories non conformes
elona_recapture$Type_Event = droplevels(elona_recapture$Type_Event)
table(elona_recapture$Type_Event)

# Ajouter données de sites à elona_recapture
elona_recapture<-merge(elona_recapture,elona_firstcapture[,c(10,11)], by="ID_1erC")



#Même chose pour les modalités de "Stage" dans first_capture
table(elona_firstcapture$Stage)
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="?")] <- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV?")]<- "JUV"
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV/SUB")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="SUB/AD")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV ")]<- "JUV"
#On supprime également les catégories non conformes
elona_firstcapture$Stage = droplevels(elona_firstcapture$Stage)
table(elona_firstcapture$Stage)


## 4.  Relation entre les Ys (Diameter & Peristome) ----
plot(elona_recapture$Diameter~elona_recapture$Peristome, col=elona_recapture$Stage,
    ylab = "Diameter",xlab="Peristome")

elona_recapture$Peristome[which(elona_recapture$Peristome>19)] <- NA

ggplot(data = elona_recapture,  
       aes(geom_point, x = Peristome, y = Diameter,
           shape = Site, col = Stage)) +
  geom_point() +
  scale_shape_manual(values = c(0,1,2))


elona_firstcapture$X[431] <- NA
elona_firstcapture$Y[431] <- NA

## 5. Relation potentielle entre X et Y ----
boxplot(elona_recapture$Diameter~elona_recapture$Site)
boxplot(elona_recapture$Peristome~elona_recapture$Site)
# Recent a l'air plus haut

## 6. Analyse des interactions ----
# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte

ggpairs(elona_recapture, columns = c("Site", "Stage", "Activity"),
        aes(color = Diameter, alpha = 0.5))

# Interactions entre Stage et Activity
plot(elona_recapture$Stage~elona_recapture$Activity, ylab = "Demographic stages",xlab="Activity")

# Interactions entre Stage & Type_Event
plot(elona_recapture$Stage~elona_recapture$Type_Event, ylab = "Demographic stages",xlab="Type Event")

# Interactions entre Activity & Type_Event
plot(elona_recapture$Activity~elona_recapture$Type_Event, ylab = "Activity",xlab="Type_Event")


## 5. Analyse des colinéarités ----
# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup

# Seules les variables numériques peuvent être prises en compte dans la matrice de corrélation
# --> pas d'utilité je pense
cor_mat <- cor(elona_recapture[c(elona_recapture$Diameter, elona_recapture$Peristome,
                                   elona_recapture$Date)])
ggcorrplot(cor_mat, method = "square", lab = TRUE)

##creation du modele mixte du diametre avec le site en modele fixe, et les etapes,annees,  n° d'identification en numero aleatoire ------

m1<-lmer(Diameter ~ Site + (1|Year) + (1|Stage) + (1|ID_1erC), data=elona_recapture)
summary(m1)
# CONDITIONS DE VALIDITE ----

## 1. Normalité des résidus ----
plot(m1) # residuals vs fitted ok
summary(m1)
par(mfrow=c(1,2))
# Histogram
hist(residuals(m1),col='dodgerblue3',xlab="residuals",main="Check Normality")
# Quantile-Quantile plot
qqnorm(residuals(m1),pch=16,col='dodgerblue3',xlab='')#residus ne semblent pas suivre distribution normale
qqline(residuals(m1),col='red')

m1<-lmer(Diameter ~ Site + Stage +(1|Date)+(1|ID_1erC),data=elona_recapture)

## 2. Homoscédasticité ----
#on recherche l'homogeneite de la variance
#selon la date
plot(x = elona_recapture$Date, y = elona_recapture$Diameter )
plot(x = elona_recapture$Date, y = elona_recapture$Peristome )
#selon l'annee
plot(x = elona_recapture$Year, y = elona_recapture$Diameter )
plot(x = elona_recapture$Year, y = elona_recapture$Peristome )
#selon le n° d'identification
plot(x = elona_recapture$ID_1erC, y = elona_recapture$Diameter )
plot(x = elona_recapture$ID_1erC, y = elona_recapture$Peristome )
#selon le stade de developpement (classe d'Age)
plot(x = elona_recapture$Stage, y = elona_recapture$Diameter )
plot(x = elona_recapture$Stage, y = elona_recapture$Peristome )

######vif{car} ----
#regles lecture fiv coeff 
# VIF = 1 : il n’existe aucune corrélation entre une variable prédictive donnée et toute autre variable prédictive du modèle.
# VIF entre 1 et 5 : il existe une corrélation modérée entre une variable prédictive donnée et d’autres variables prédictives du modèle.
# VIF > 5 : il existe une forte corrélation entre une variable prédictive donnée et d’autres variables prédictives dans le modèle.
vif(mod_test_tot) #erreur :   there are aliased coefficients in the model
# GVIF = generalized Variation Inflation Factor | GVIFj =(1/ (1−Rj^2 )^d)
# GVIF plutot que VIF utilisé quand le modèle comprend des variables catégorielles à plusieurs niveaux.
#Stade juvenile est le facteur avec un effet le plus fort
#les correlationsd'effet fixes sont toutes <0.2



#selection modele en retirant des var : methode descendante stepAIC {MASS}
M0 <- lmer (Diameter ~1 + (1|Date), data = elona_recapture)
stepAIC (M0, mod_tot, direction = "backward")

mod_test <-     lm (Diameter ~ Site + Peristome+ Stage+Type_Event, data = elona_recapture_cor)
mod_test_tot <- lm (Diameter ~ Site + Peristome+ Stage+Type_Event + Event + Day + ID_1erC , data = elona_recapture_cor)#modele lineaire simple avec toutes les variables qui ne sont pas des "aliased coeff" pour faire fonctionner la fonction vif{car}

# #                  GVIF  Df GVIF^(1/(2*Df))
# Site       1.594304e+02   1       12.626574
# Peristome  5.746555e+00   1        2.397197
# Stage      1.100995e+01   2        1.821572
# Type_Event 5.381555e+00   2        1.523095 
# Event      2.108339e+06  19        1.466964
# Day        3.388570e+02   1       18.408068
# ID_1erC    3.271475e+07 508        1.017177
#multicolinearite moderee pour  Peristome et Type Event ?? 
# forte pour Site, Day, Peristome (erreur type gonflee ?)
#grave pour Event, ID_1erC (estimateur instable) --> on ne les utilise pas trop dans les analyses

sort(vif(mod_test_tot), decreasing = TRUE)
vif(mod_tot) #GVIF = 1 --> colinearite moderee
vif.cca(mod_tot)
# confint (mod_tot, level = 0.95, method = "boot", nsim = 500)
#500 bootstrap
#                           2.5 %     97.5 %
# .sig01                 2.05098373  2.3840300
# .sig02                 0.04657717  0.4782746
# .sig03                 0.00000000  0.4951948
# .sig04                 0.00000000  0.5966943
# .sig05                 0.00000000  1.3120622
# .sigma                 1.35065496  1.4783467
# (Intercept)           19.58192678 21.5849593
# SiteRecent_lowdensity  0.38647602  1.3611775
# StageJUV              -4.61585735 -4.0701877
# StageSUB              -1.29917847 -0.5985583
# ActivityIMMO          -0.30856977  0.4759665
# ActivityMORT          -1.31770650  0.4159386

##matrice de correlation -----



#QUESTION : What is the impact of population density on biometry and individual growth in Elona quimperianal ? ------
#GLMM 
# un glm, avec en réponse diamètre et peristome, en variables classiques le site (old ou new), stage et act (?) et en variables aléatoires l'ID, l'event et la date ?


mod_tot<-lmer(Diameter ~ Site + Stage + Activity + (1|Year)+ (1|ID_1erC) + (1|Type_Event) + (1|Event) + (1| Date),data=elona_recapture)
anova(mod_tot)
summary(mod_tot)



## 3. Résidus indépendants ----
## Checking collinearity between categorical and continuous independent variables
par(mfrow=c(1,3))
###Boxplot Dimension selon var explicatives -----
####Year ----
##Diameter and year (recapture)
boxplot(elona_recapture$Diameter~elona_recapture$Year, varwidth = TRUE, ylab = "Diameter", xlab = "Year",col=c('#000099','#339900'), ylim = c(0,25), main = "Recapture ")
##Diameter and year (First capture)
boxplot(elona_firstcapture$Diameter~elona_firstcapture$Year, varwidth = TRUE, ylab = "Diameter", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,25), main = "First Capture")

##peristome and year (recapture)
boxplot(elona_recapture$Peristome~elona_recapture$Year, varwidth = TRUE, ylab = "Peristome", xlab = "Year",col=c('#000099','#339900'), ylim = c(0,12), main = "Recapture ")
##peristome and year (First capture)
boxplot(elona_firstcapture$Peristome~elona_firstcapture$Year, varwidth = TRUE, ylab = "Peristome", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,12), main = "First Capture")
#### Event----
##Diameter and Event (recapture)
boxplot(elona_recapture$Diameter~elona_recapture$Event, varwidth = TRUE, ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
##Diameter and Event (First capture)
boxplot(elona_firstcapture$Diameter~elona_firstcapture$Event, varwidth = TRUE, ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "First Capture")

#### ID_1erC -----
boxplot(elona_recapture$Diameter~elona_recapture$ID_1erC, varwidth = TRUE, ylab = "Diameter", xlab = "ID_1erC",col=c(blues9), ylim = c(0,25), main = "Recapture ")
####TypeEvent -----
boxplot(elona_recapture$Diameter~elona_recapture$Type_Event, varwidth = TRUE, ylab = "Diameter", xlab = "Type_Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")

sum(is.na(elona_recapture_cor)) #6 NA 
which(is.na(elona_recapture_cor)) #6 NA 

##Colinearite entre les variables X num ou facteur -----
vif(m1) #erreur : model contains fewer than 2 terms
vif(elona_recapture_cor [,2:7])

# Checking collinearity between continuous independent variables

# We represent plot for each X continuous covariate pairs
#We calculate correlation between each pair of X covariate
corrplot.mixed(M1C,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8, main = "First capture")
corrplot.mixed(MRC,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8, main = " REcapture")

# Significativité de la variance associé au facteur aléatoire
intervals(m1) # Fonctionne pas doit être adapté à lme pas lmer


# ANALYSE ----
summary(m1)
anova(m1) # effet significatif du site

# Choix de la méthode pour répondre à la question :
# We aim to determine whether the two populations share the same demographic characteristics and traits.

## 1. Diameter ----
# Modèle linéaire général mixte
# Diameter = Site + Stage + (ID_1erC + Date)
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

###a. Préparation des données ----

#f(proportion de chaque classe d'âge)= Site + Date
# On considère que les individus retrouvés morts comme appartenant à leur classe d'âge respective. 

elona_dem <- data.frame(matrix(ncol = 6, nrow = 0))
L_Site <- c()
L_Event <- c()
L_Event_Site <- c()
for (site in unique(elona_recapture$Site)){
  for (event in unique(elona_recapture$Event)){
    AD <- nrow(elona_recapture %>% filter(Site == site & Event == event & Stage == "AD"))
    SUB <- nrow(elona_recapture %>% filter(Site == site & Event == event & Stage == "SUB"))
    JUV <- nrow(elona_recapture %>% filter(Site == site & Event == event & Stage == "JUV"))
    Event_Site <- paste(event, str_sub(site, 1,3), sep = "_")
    L_Site <- c(L_Site, site)
    L_Event <- c(L_Event, event)
    L_Event_Site <- c(L_Event_Site, Event_Site)
    elona_dem <- rbind(elona_dem, cbind(AD, SUB, JUV))
  }}
elona_dem <- cbind(elona_dem, L_Site, L_Event, L_Event_Site)
colnames(elona_dem) <- c('AD', 'SUB', 'JUV', 'Site', 'Event', "Event_Site")

# Ajout des proportions dans le dataframe
elona_dem <- elona_dem %>% mutate(TOT_Site = AD + SUB + JUV)
elona_dem <- elona_dem %>% mutate(Prop_AD = AD / TOT_Site)
elona_dem <- elona_dem %>% mutate(Prop_SUB = SUB / TOT_Site)
elona_dem <- elona_dem %>% mutate(Prop_JUV = JUV / TOT_Site)

# Visualisation des quantités par site
ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, AD, fill = Site))

ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, JUV, fill = Site))

ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, SUB, fill = Site))

# Visualisation du nombre total d'individus par sites
ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, TOT_Site, fill = Site))

# Visualisation des proportions par site
ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, Prop_AD, fill = Site))

ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, Prop_JUV, fill = Site))

ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, Prop_SUB, fill = Site))


# On extrait ensuite les proportions dans un nouveau tableau
prop_AD <- elona_dem$Prop_AD
prop_SUB <- elona_dem$Prop_SUB
prop_JUV <- elona_dem$Prop_JUV
proportions <- data.frame(prop_ad, prop_sub, prop_juv)
# Site <- elona_dem$Site
# Event <- elona_dem$Event
# proportions <- data.frame(prop_ad, prop_sub, prop_juv, Site, Event)

###b. Sélection du modèle ----

# On veut analyser les proportions des classes d'âges par rapport aux sites et aux évènements
# Il y a 4 possibilités

#### 1. Régression logistique ----
# En effet on veut une analyse de proportions donc on pourrait réaliser 3 modèles
# de régression logistique.

# Cependant ici nous voulons analyser 3 variables quantitatives différentes,
# ce qui n'est pas compatible avec une régression logistique multivariée.

#### 2. RDA ----
# En effet on veut analyser les relations entre un tableau avec les proportions 
# et un tableau avec les caractéristiques des captures.

# Cependant la RDA se concentre sur la prédiction de données,
# ce qui n'est pas ce que nous recherchons.

# ↓ code de la tentative
# On écrit le modèle
# RDA <- rda(proportions ~ Site + Event, data = elona_dem)
# summary(RDA)
# RsquareAdj(RDA)
# Résultats du modèle
# anova(RDA, by = "margin")

# plot(RDA, display=c("species","cn"))

#### 3. ACP ----
# Il s'agit d'un équivalent de la RDA mais concentré sur l'explication des données.

# Cependant la structure des données ne correspond pas à la méthode.

# ↓ code de la tentative
# demo_PCA <- dudi.pca(proportions[,1:5], scannf=FALSE)
# demo_PCA$cw
# demo_PCA$lw

# library(FactoMineR)
# demo_PCA <- PCA(proportions, quali.sup = c(Site, Event))
# same as dudi.pca(palmer[,3:6], scannf=FALSE, scale = TRUE)

# library(Factoshiny)
# res <- Factoshiny(proportions)

#### 4. Régression linéaire multiple ----
# La structure du modèle n'est pas analysable par les outils communs.

# ↓ code de la tentative
# demo_model <- lm(cbind(elona_dem$Prop_AD, elona_dem$Prop_SUB, elona_dem$Prop_JUV)
#                  ~ elona_dem$Site + elona_dem$Event)
# summary(demo_model)
# step(demo_model)
# AIC(demo_model)
# drop1(demo_model)
# add1(demo_model)


### Spatial description ##
## elona_sf contains the portion of the data relevant for analysis
elona_sf <- elona_firstcapture %>%
  filter(!is.na(elona_firstcapture$X)) %>%
  select(Site, 
         X, 
         Y, 
         IDZone)
## The coordinates are turned into an sf object in the geometry column
elona_sf$X <- as.numeric(as.character(elona_sf$X))
elona_sf$Y <- as.numeric(as.character(elona_sf$Y))
elona_sf <- elona_sf %>%
  st_as_sf(coords = c("X", 
                      "Y"))


## We separate the old and new sites because some zone avec the same ID (but different coodinates)
elona_sf_old <- elona_sf %>%
  filter(Site == 'Old_highdensity')
elona_sf_old <- distinct(elona_sf_old, 
                         IDZone, 
                         .keep_all = TRUE)
elona_sf_new <- elona_sf %>%
  filter(Site == 'Recent_lowdensity')
elona_sf_new <- distinct(elona_sf_new, 
                         IDZone, 
                         .keep_all = TRUE)

## After filtering to keep only one line per zone we bind the 2 sites back together
elona_sf <- rbind(elona_sf_old, 
                  elona_sf_new)


## Distances between the centroids of the two sites
st_distance(
  st_combine(elona_sf_new$geometry) %>%
    st_centroid(),
  st_combine(elona_sf_old$geometry) %>%
    st_centroid()
)

### PLOTS 

ggplot() +
  geom_sf(data = elona_sf$geometry, 
          aes(col = elona_sf$Site))

mapview(elona_sf, 
        zcol = 'IDZone')


ggplot() +
  geom_sf(data = elona_sf_new$geometry)+
  theme_void()
# ligne 1 en bas, ligne 10 en haut, colonne A à droite, G à gauche

ggplot() +
  geom_sf(data = elona_sf_old$geometry)+
  theme_void()
# ligne 1 en haut, ligne 13 en bas, colonne A à gauche, I à droite


mapview(elona_sf_new, 
        zcol = 'IDZone')


# Homoscédasticité : ncvTest {car} (cad homogeneite des variances)
ncvTest (m1$Residuals)#ne fonctionne pas sur les modeles mixtes ;(
confint(m1, level = 0.95, method = "boot", nsim = 50)#! attention peut être long
# 50 Bootstrap : 
#                             2.5 %    97.5 %
# .sig01                 1.928837139  2.255510
# .sig02                 0.004078734  5.162814
# .sig03                 0.000000000  1.133452
# .sig04                 0.119252867  3.317632
# .sigma                 1.445200328  1.576007
# (Intercept)           15.216941199 21.592898
# SiteRecent_lowdensity  0.376591648  1.380474

# 200 Bootstrap : 
#                             2.5 %    97.5 %
# .sig01                1.950901e+00  2.280667
# .sig02                3.350949e-04  4.080984
# .sig03                0.000000e+00  1.347881
# .sig04                6.294728e-02  2.951125
# .sigma                1.448151e+00  1.581611
# (Intercept)           1.555703e+01 22.053213
# SiteRecent_lowdensity 4.291910e-01  1.261994

#500 Bootstrap : 
#                             2.5 %    97.5 %
# .sig01                1.969093e+00  2.281308
# .sig02                9.295588e-04  3.928594
# .sig03                0.000000e+00  1.142263
# .sig04                6.463473e-02  2.947597
# .sigma                1.454283e+00  1.581192
# (Intercept)           1.601762e+01 21.652034
# SiteRecent_lowdensity 4.479101e-01  1.345800


# Checking collinearity between categorical and continuous independent variables
par(mfrow=c(2,2))
#Length and Sex
boxplot(elona_recapture$Diameter~elona_recapture$Stage, varwidth = TRUE, ylab = "Diameter", xlab = "Age Stage", col='grey', main = "")
#Length and Habitat
boxplot(dataFly$Length~dataFly$Habitat, varwidth = TRUE, ylab = "Length", xlab = "Habitat", col='grey', main = "")
#Width and Sex
boxplot(dataFly$Width~dataFly$Sex, varwidth = TRUE, ylab = "Width", xlab = "Sex", col='grey', main = "")
#Width and Habitat
boxplot(dataFly$Width~dataFly$Habitat, varwidth = TRUE, ylab = "Width", xlab = "Habitat", col='grey', main = "")

par(mfrow=c(2,2))
#Water Content and Sex
boxplot(dataFly$WaterContent~dataFly$Sex, varwidth = TRUE, ylab = "Water Content", xlab = "Sex", col='grey', main = "")
#Water Content and Habitat
boxplot(dataFly$WaterContent~dataFly$Habitat, varwidth = TRUE, ylab = "Water Content", xlab = "Habitat", col='grey', main = "")
#Fat Content and Sex
boxplot(dataFly$FatContent~dataFly$Sex, varwidth = TRUE, ylab = "Fat Content", xlab = "Sex", col='grey', main = "")
#Fat Content and Habitat
boxplot(dataFly$FatContent~dataFly$Habitat, varwidth = TRUE, ylab = "Fat Content", xlab = "Habitat", col='grey', main = "")



#Autres BONUS test -----
### selection Lignes où peristome n'est pas NA select{dplyr} -----
#####Recapture ----
summary(elona_recapture$Peristome)#initiallement, 125 NA
summary(elona_recapture$Diameter)#initiallement, 80 NA
elona_recapture_cor <- 
  subset (elona_recapture, 
          elona_recapture$Peristome != "NA" &  elona_recapture$Diameter!="NA" )
dim(elona_recapture_cor) ; dim(elona_recapture)
MRC <- cor(elona_recapture_cor[, c(2,3, 7,8, 9)])#correlation forte entre Diametre et Peristome 88%
#####First capture -----
summary(elona_firstcapture$Diameter)#initiallement, 3 NA
summary(elona_firstcapture$Peristome)#initiallement, 27 NA

elona_firstcapture_cor <- 
  subset (elona_firstcapture, 
          elona_firstcapture$Peristome != "NA" &  elona_firstcapture$Diameter!="NA" )
dim(elona_firstcapture_cor) ; dim(elona_recapture) #verifier le nb de lignes supprimees
M1C <- cor(elona_firstcapture_cor[, c(2,3, 6, 7,8)])#correlation forte entre Diametre et Peristome 94%



summary(elona_recapture_cor)



###PLOT OBS -----
summary(elona_recapture)
head(elona_recapture)
table(elona_recapture$Diameter)
table(elona_recapture$Peristome)
subset(elona_recapture, elona_recapture$Peristome>19)

nrow(table(elona_recapture$Date))
table (elona_recapture$Site)
table(elona_recapture$Stage)
#diagramme de cook (valeurs extremes du modele ?)
par(mfrow = c(1, 1))
plots(m1)
A<-CookD(m1,newwd=TRUE)#le calcul semble trop long, perturbe le logiciel
plot(A)
plot.lm(m1)

# Limite de l'étude

# Fait qu'on ne sait pas si les placettes ont des ressources très différentes ou pas
# Le jeu de données ne nous permet pas de prendre en compte si les escargots bougent ou pas

# Que des psuedo-réplicats ce serait bien d'avoir d'autres sites.
# De plus, rien n'indique que ces sites sont équivalents en termes de ressources alimentaires, qualité d'habitats
# Pour réellement étudier l'effet de la densité de population sur les caractéristiques morphologiques il faudrait réaliser une étude démographique et utiliser la densité de population en varaible explicative
