# IMPORTATION DES LIBRARIES ----------------------------------------------------
#install.packages("readxl")
#install.packages("lme4")
#install.packages("mgcv")
library(lme4)
library(readxl)
library(GGally)
library(ggcorrplot) #graphe de correlation
library(ggplot2)
library(tidyverse)
library(predictmeans)#diagramme de cook
library(nlme)
library(lmerTest)
library(mgcv)
library(corrplot) # Correlation matrix calculus
library(lattice) # multipanel graphics
library(ade4) # PCA
library(performance)#r2 LMM
library(car)
library(MASS)
library(dplyr)
library(sf)
library(mapview)
library(lmerTest)





# IMPORTATION DES DONNEES ------------------------------------------------------

## 1. Importation et gestion des classes ----
#Le tableau de données est organisé en deux feuilles : firstcapture donnant l'inventaire de tous les individus, et recapture informant sur les données prise lors de la recapture de ces mêmes individus.

#Importation de recapture
elona_recapture <- read_xlsx("data_elona.xlsx", sheet = 3, col_types = c('text', 'numeric', 'numeric', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'text', 'text'))
elona_recapture$Site <- as.factor(elona_recapture$Site)
elona_recapture$Stage <- as.factor(elona_recapture$Stage)
elona_recapture$Activity <- as.factor(elona_recapture$Activity)
elona_recapture$Type_Event <- as.factor(elona_recapture$Type_Event)
elona_recapture$Event <- as.factor(elona_recapture$Event)
elona_recapture$ID_1erC <- as.factor(elona_recapture$ID_1erC)
elona_recapture <- data.frame(elona_recapture$Site, elona_recapture$Diameter, elona_recapture$Peristome, elona_recapture$Stage, elona_recapture$Activity, elona_recapture$Type_Event, elona_recapture$Day, elona_recapture$Month, elona_recapture$Year, elona_recapture$Event, elona_recapture$ID_1erC)
colnames(elona_recapture) <- c('Site', 'Diameter', 'Peristome', 'Stage', 'Activity', 'Type_Event', "Day", "Month", "Year", "Event", "ID_1erC")

#Importation de firstcapture
elona_firstcapture <- read_xlsx("data_elona.xlsx", sheet = 2, col_types = c('text', 'numeric', 'numeric', 'text', 'text', 'numeric', 'numeric', 'numeric', 'text', 'text', 'text', 'text', 'text'))
elona_firstcapture$Site <- as.factor(elona_firstcapture$Site)
elona_firstcapture$Stage <- as.factor(elona_firstcapture$Stage)
elona_firstcapture$Type_Event <- as.factor(elona_firstcapture$Type_Event)
elona_firstcapture$Event <- as.factor(elona_firstcapture$Event)
elona_firstcapture$ID_1erC <- as.factor(elona_firstcapture$ID_1erC)
elona_firstcapture$IDZone <- as.factor(elona_firstcapture$IDZone)
elona_firstcapture$X <- as.factor(elona_firstcapture$X)
elona_firstcapture$Y <- as.factor(elona_firstcapture$Y)
elona_firstcapture <- data.frame(elona_firstcapture$Site, elona_firstcapture$Diameter, elona_firstcapture$Peristome, elona_firstcapture$Stage, elona_firstcapture$Type_Event, elona_firstcapture$Day, elona_firstcapture$Month, elona_firstcapture$Year, elona_firstcapture$Event, elona_firstcapture$ID_1erC,elona_firstcapture$IDZone, elona_firstcapture$X, elona_firstcapture$Y)
colnames(elona_firstcapture) <- c('Site', 'Diameter', 'Peristome', 'Stage', 'Type_Event', "Day", "Month", "Year", "Event", "ID_1erC", "IDZone", "X", "Y")




## 2. Nettoyage des données X ----

table(elona_recapture$Site)
table(elona_recapture$Stage)# Le nombre de modalités dans "Stage" ne correspd pas avec les indications
# Il a des catégories (?, JUV? JUV/SUB, SUB/AD) très peu représentées. 
# Nous faisons le choix de les recatégoriser comme suit. 
elona_recapture$Stage[which(elona_recapture$Stage=="?")] <- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV?")]<- "JUV"
elona_recapture$Stage[which(elona_recapture$Stage=="JUV/SUB")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="SUB/AD")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="NA")] <- NA
table(elona_recapture$Stage)
#On supprime ensuite les catégories qui n'existent plus
elona_recapture$Stage = droplevels(elona_recapture$Stage)
table(elona_recapture$Stage)



#Même chose pour les modalités de "Activity"
table(elona_recapture$Activity)
elona_recapture$Activity[which(elona_recapture$Activity=="IMM")]<- "IMMO"
elona_recapture$Activity = droplevels(elona_recapture$Activity)
table(elona_recapture$Activity)

# Pour une meilleure lisibilité des graphiques nous avons modifié le nom des classes pour "Event" (E1 -> E01)
elona_recapture$Event <- as.character(elona_recapture$Event)
for (i in 1:nrow(elona_recapture)){
  if (nchar(elona_recapture$Event[i]) == 2){
    elona_recapture$Event[i] <- paste(substr(elona_recapture$Event[i], 1, 1), substr(elona_recapture$Event[i], 2, 2), sep = "0")
  }
}
elona_recapture$Event <- as.factor(elona_recapture$Event)

elona_firstcapture$Event <- as.character(elona_firstcapture$Event)
for (i in 1:nrow(elona_firstcapture)){
  if (nchar(elona_firstcapture$Event[i]) == 2){
    elona_firstcapture$Event[i] <- paste(substr(elona_firstcapture$Event[i], 1, 1), substr(elona_firstcapture$Event[i], 2, 2), sep = "0")
  }
}
elona_firstcapture$Event <- as.factor(elona_firstcapture$Event)

# Ajouter données de sites à elona_recapture
elona_recapture<-merge(elona_recapture,elona_firstcapture[,c(10,11)], by="ID_1erC")



#Même chose pour les modalités de "Stage" dans first_capture
table(elona_firstcapture$Stage)
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="?")] <- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV?")]<- "JUV"
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV/SUB")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="SUB/AD")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="NA")]<- NA
elona_firstcapture$Stage = droplevels(elona_firstcapture$Stage)
table(elona_firstcapture$Stage)

## 3. Rajout des données de firstcapture dans recapture ----

# Certains escargots ne sont pas recapturés, ils ne figurent donc pas dans le dossier recapture, mais il faut tout de même les comptabiliser dans le modèle.
# Il est donc nécessaire de rajouter ces données-là dans recapture, sans toutefois ajouter ceux qui ont été recapturé (leur première capture existant déjà dans recapture)

elona_recapture<-elona_firstcapture  %>%
  dplyr::select(!c(IDZone,X,Y))%>%
  anti_join(elona_recapture,by="ID_1erC") %>%
  bind_rows(elona_recapture)


## 4. Modification des dates ----
elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),as.character(elona_recapture$Month),as.character(elona_recapture$Day), sep = "-"))


#QUESTION : What is the impact of population density on biometry and individual growth in Elona quimperianal ? ------
# We aim to determine whether the two populations share the same demographic characteristics and traits.


# EXPLORATION DES DONNEES ------------------------------------------------------

## 1. Valeurs extrêmes ----

#Diametre
boxplot(elona_recapture$Diameter)
summary(elona_recapture$Diameter)
# On a des valeurs très faibles mais ça ne parait pas improbable


#Peristome
par(mfrow=c(1,2))
boxplot(elona_recapture$Peristome) # Une valeur très très haute (901mm) => Cela paraît très improbable, nous avons fait le choix de la retirer 
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


summary(elona_recapture$Peristome)
hist(elona_recapture$Diameter,col='chartreuse',xlab="Diameter",main="Check Normality
 (Diameter) ",breaks=100)
hist(elona_recapture$Peristome,col='purple',xlab="Peristome",main="Check Normality
(Peristome)",breaks=100)
# Ni le Diamètre, Ni le Péristome ne semblent suivrent une loi normale. 
# Les queues de distribution ne sont pas symétriques.



## 4.  Relation entre les Ys (Diameter & Peristome) ----

ggplot(data = elona_recapture,  
       aes(geom_point, x = Peristome, y = Diameter,
           shape = Site, col = Stage)) +
  geom_point() +
  scale_shape_manual(values = c(0,1,2))
# Il semble y avoir une colinéarité assez forte entre le diamètre et le péristome 
# De plus, si l'on considère la classe d'âge,on observe que les individus avec un petit diamètre et péristome sont les juvéniles, ce qui fait sens biologiquement.
# Le diamètre et le péristome des individus des deux sites sont confondus pour les juvéniles.
# Mais il semble qu'il y ait une légère différence de tailles pour les individus adultes selon le site. 



## 5. Relation potentielle entre X et Y ----
par(mfrow=c(1,2))
boxplot(elona_recapture$Diameter~elona_recapture$Site, main = "Diameter")
boxplot(elona_recapture$Peristome~elona_recapture$Site, main = "Peristome")
# Il semble que les escargots du site Recent ont un diamètre et un péristome plus important, sans que cela soit significatif. 

## 6. Analyse des interactions entre les X ----

ggpairs(elona_recapture, columns = c("Site", "Stage", "Activity"),
        aes(color = Diameter, alpha = 0.5))

"A COMMENTER"

# Interactions entre Stage et Activity
plot(elona_recapture$Stage~elona_recapture$Activity, ylab = "Demographic stages",xlab="Activity")
#Il semble il y avoir une plus grande proportion d'individus adultes à l'état immobile ou mort que de junéviles ou subadultes.

# Interactions entre Stage & Type_Event
plot(elona_recapture$Stage~elona_recapture$Type_Event, ylab = "Demographic stages",xlab="Type Event")
# Les proportions de juvéniles et subadultes sont plus élevée lors des premières captures (type event = C),
# c'est logique car plus les individus sont agés plus ils ont de chance d'avoir déjà été capturés

# Interactions entre Activity & Type_Event
plot(elona_recapture$Activity~elona_recapture$Type_Event, ylab = "Activity",xlab="Type_Event")
# Graphique peu lisible car la majorité (86%) des escargots sont actifs.



## 7. Analyse des colinéarités ----
# Il faut aussi vérifier une éventuelle colinéarité entre les X
## Checking collinearity between categorical and continuous independent variables
par(mfrow=c(1,2))
####Boxplot Dimension selon var explicatives -----
####Year ----

## Diameter and year 
boxplot(elona_recapture$Diameter~elona_recapture$Year, varwidth = TRUE, ylab = "Diameter", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,25), main = "Recapture Diameter ")
# Les diamètres des coquilles ne semblent pas augmenter au cours du temps.


##peristome and year
boxplot(elona_recapture$Peristome~elona_recapture$Year, varwidth = TRUE, ylab = "Peristome", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,12), main = "Recapture Peristome")
# Tendance à légère augmentation au cours du temps de la taille du peristome


#### Event----
##Diameter and Event
boxplot(elona_recapture$Diameter~elona_recapture$Event, varwidth = TRUE, ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
##Diameter and Event 
# Le diamètre moyen varie peu, en revanche les variances sont très différentes selon l'évènement.

####TypeEvent -----
boxplot(elona_recapture$Diameter~elona_recapture$Type_Event, varwidth = TRUE, ylab = "Diameter", xlab = "Type_Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
#Difference de diametre en fonction de l'etat (logique car les individus recapturés ont plus de chance d'être plus vieux)


## 8. selection Lignes où peristome n'est pas NA select{dplyr} -----
summary(elona_recapture$Peristome)#initiallement, 139 NA
summary(elona_recapture$Diameter)#initiallement, 81 NA
elona_recapture_cor <- 
  subset (elona_recapture, 
          elona_recapture$Peristome != "NA" &  elona_recapture$Diameter!="NA" )
dim(elona_recapture_cor) ; dim(elona_recapture) #verifier le nb de lignes supprimees


## 9. Matrice de corrélation ----
MRC <- cor(elona_recapture_cor[, c(2,3,6,7,8)])
MRC
# Correlation forte entre Diamètre et Peristome (93%), comme nous l'avions remarqué graphiquement.

# We represent plot for each X continuous covariate pairs
#We calculate correlation between each pair of X covariate
corrplot.mixed(MRC,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8, main = "Recapture")


sum(is.na(elona_recapture_cor)) #6 NA 
which(is.na(elona_recapture_cor)) #6 NA 


#Tentative de matrice de correlation pour les variables catégorielles
# Seules les variables numériques peuvent être prises en compte dans la matrice de corrélation
# cor_mat <- cor(elona_recapture[c(elona_recapture$Diameter, elona_recapture$Peristome,
#                                  elona_recapture$Date)])
# ggcorrplot(cor_mat, method = "square", lab = TRUE)

## 10. Description spatiale ----
# Cette partie ne sert qu'à visualiser les sites et les quadrats et à vérifier les distances
## elona_sf contains the portion of the data relevant for analysis
elona_sf <- elona_firstcapture[,c("Site", 
                                  "X", 
                                  "Y", 
                                  "IDZone")] %>%
  filter(!is.na(elona_firstcapture$X))
## The coordinates are turned into an sf object in the geometry column
elona_sf$X <- as.numeric(as.character(elona_sf$X))
elona_sf$Y <- as.numeric(as.character(elona_sf$Y))
elona_sf <- elona_sf %>%
  st_as_sf(coords = c("X", 
                      "Y"))
## We separate the old and new sites because some zone with the same ID (but different coordinates)
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
# The 2 sites are, as expected, about 500m apart. 

### PLOT
# Both sites, zoom in to see the the quadrant's IDs
# Old site is in bottom left corner, new site is top right
mapview(elona_sf, 
        zcol = 'IDZone')





#I -  DIAMETRE -----------------------------------------------------------------
## 1. Creation du modèle ----
# Nous avons choisi de faire un modèle linéaire général mixte pour le diamètre, car nous ne voulons pas prendre en compte l'effet des individus ni de la Date des mesures.
# Toutes les variables explicatives sont catégorielles

# Modele mixte du diametre avec toutes* les variables explicatives, dont la date et n° d'identification en variables aleatoires :
# *Event et Date sont colinéaire, nous n'avons conservé que Date
# *IDZone n'est pas pertient à ajouter car les noms sont répétés entre les deux sites
m1_comp <- lmer(Diameter ~ Site + Stage + Activity + Type_Event +(1|Date) + (1|ID_1erC), data = elona_recapture)
anova(m1_comp)
# Toutes les variables sauf Activity ont un effet significatif donc on la retire
m1 <- lmer(Diameter ~ Site + Stage  + Type_Event +(1|Date) + (1|ID_1erC), data = elona_recapture)
anova(m1)

# Nous avions vu précédement que le site semblait avoir un effet différent sur le diamètre des Adultes
# Il y aurait donc une interraction entre le site et l'age, ajoutons la au modèle
# Modele mixte du diametre avec le site et l'age et leur interaction en variables fixes, et la date et n° d'identification en variables aleatoires :
m1_inter <-lmer(Diameter ~ Site + Stage + Site:Stage + Type_Event +(1|Date)+(1|ID_1erC), data = elona_recapture)
anova(m1_inter)
# Le site est moins significatif que dans le modèle précédent, 
# mais l'interaction avec l'age est aussi significative (elle a surement "absorbé" une partie de l'effet du Site).

# Nous conserverons le modèle m1_inter

## 2. Conditions de validité ----

### a. Normalité des résidus ----
par(mfrow=c(1,2))
# Histogram
hist(residuals(m1_inter),col='dodgerblue3',xlab="residuals",main="Check Normality") 
# Quantile-Quantile plot
qqnorm(residuals(m1_inter),pch=16,col='dodgerblue3',xlab='')
qqline(residuals(m1_inter),col='red')
# Les résidus ne semblent pas suivre distribution normale.
# La normalité des résidus n'est pas tout à fait respectée. Les valeurs extrèmes s'éloingnent un peu trop. 


### b. Homoscédasticité des résidus ----
plot(m1_inter)
# Il semble y avoir moins de variance pour les valeurs extrèmes
# Heteroscedasticite que l'on prendra en compte lors de l'interpretation



### c. Résidus indépendants ----

#L'independance des residus n'est pas un critere pour les modeles lineaires general mixte, donc n'est pas necessaire d'etre valide. 

"EST-CE QUE l'ON GARDE SI CA MARCHE PAS"
#Plusieurs graphes ont ete realises a titre indicatif (pour observer la colinearite des variables)
#on recherche l'independence des residus
# #selon la date
# nrow(na.omit(elona_recapture))
#plot(x = elona_recapture$Date, y = residuals(m1) ) #problème de taille des matrices
# #selon l'annee
# plot(x = elona_recapture$Year, y = residuals(m1) )
# #selon le n° d'identification
# plot(x = elona_recapture$ID_1erC, y = residuals(m1) )
# #selon le stade de developpement (classe d'Age)
# plot(x = elona_recapture$Stage, y = residuals(m1) )


### d. Colinearite entre les variables X num ou facteur -----
#vif{car}
vif(m1_inter) #GVIF ~1 donc colinearite moderee#GVIF = 1.001 a 1.003 = plutot faible donc pas trop de colinearite
# vif(elona_recapture_cor [,2:7])

#regles lecture fiv coeff 
# VIF = 1 : il n’existe aucune corrélation entre une variable prédictive donnée et toute autre variable prédictive du modèle.
# VIF entre 1 et 5 : il existe une corrélation modérée entre une variable prédictive donnée et d’autres variables prédictives du modèle.
# VIF > 5 : il existe une forte corrélation entre une variable prédictive donnée et d’autres variables prédictives dans le modèle.
# GVIF = generalized Variation Inflation Factor | GVIFj =(1/ (1−Rj^2 )^d)
# GVIF plutot que VIF utilisé quand le modèle comprend des variables catégorielles à plusieurs niveaux.
#Stade juvenile est le facteur avec un effet le plus fort

# Significativité de la variance associé au facteur aléatoire
# intervals(m1) # Fonctionne pas doit être adapté à lme pas lmer


mod_test <-     lm (Diameter ~ Site + Peristome+ Stage+Type_Event, data = elona_recapture_cor)
mod_test_tot <- lm (Diameter ~ Site + Peristome+ Stage+Type_Event + Event + Day + ID_1erC , data = elona_recapture_cor)#modele lineaire simple avec toutes les variables qui ne sont pas des "aliased coeff" pour faire fonctionner la fonction vif{car}
vif(mod_test_tot)
#erreur :   there are aliased coefficients in the model

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
vif(mod_test) #GVIF = 1 --> colinearite moderee

# confint (mod_test_tot, level = 0.95, method = "boot", nsim = 500)
#500 bootstrap
#                                            2.5 %        97.5 %
# (Intercept)                           3.56422468  7.932955e+00
# SiteRecent_lowdensity                -1.11733205  3.775009e-01
# Peristome                             1.45544302  1.634215e+00
# StageJUV                             -1.91943213 -1.455367e+00
# StageSUB                             -0.65383084 -1.707252e-01
# Type_EventR                          -0.17528457  1.605032e-01
# Type_EventR_MORT                     -1.08266541 -9.938132e-03
# EventE02                             -2.05426208  1.273395e+00
# EventE03                              0.03046729  1.027276e+00
# EventE04                             -1.17696363  1.687324e+00
# EventE09                              0.43151888  1.177013e+00
# EventE10                             -1.21769069  2.102742e+00

## 3. Interprétation ----
r2_nakagawa(m1_inter)
# 42% de la variance est expliquée par les effets fixes
# et 84% de la variance est expliquée par les effets fixes et aléatoires. 
# L'effet aléatoire explique donc une grande part de la variabilité. 
# Les variables fixes ont toutefois un effet significatif. 
anova(m1_inter)
# Le stade est la variable qui a le plus d'effet (p value tres faible).
# Toutefois le site et son interaction avec le stade ont un effet signifcatif. 
# Mais leur effet est bien plus faible. 
summary(m1_inter)
# Dans le summary on voit que l'estimate associé au changement de Site est faible (devant l'intercept et devant l'effet du changement de stade (JUV)).
# Cela confirme l'effet significatif mais faible du site.

# Le modèle est donc validé mais montre que le site a un effet significatif mais faible sur le diamètre comparativement aux autres variables. 



#II - PERISTOME ----------------------------------------------------------------
## A. Transformation logarithmique ----
### 1. Création du modèle - transformation au logarithme   ----
"EXPLIQUER LE CHOIX DU MODELE ET DU PASSAGE AU LOGARITHME"
m_peris <- lmer(log(Peristome) ~ Site + Stage + (1|ID_1erC) + (1|Date), data=elona_recapture)

### 2. Conditions de validité lors du passage au log ----

#### a. Normalité des résidus ----
par(mfrow=c(1,2))
# Histogram
hist(residuals(m_peris),col='dodgerblue3',xlab="residuals",main="Check Normality") 
# Quantile-Quantile plot
qqnorm(residuals(m_peris),pch=16,col='dodgerblue3',xlab='')#residus ne semblent pas suivre distribution normale
qqline(residuals(m_peris),col='red')
# La normalité des résidus n'est pas tout à fait respectée. Les valeurs extrèmes s'éloignent un peu trop. 


#### b. Homoscédasticité ----
plot(m_peris) # heteroscedasticite encore plus forte que l'on prendra en compte lors de l'interpretations
#La variance est plus faible pour les hautes valeurs de péristome


#### c. Résidus indépendants ----

#L'independance des residus n'est pas un critere pour les modeles lineaires general mixte, donc n'est pas necessaire d'etre valide. 


#### d. Colinearite entre les variables X num ou facteur -----
vif(m_peris) #GVIF ~1 donc colinearite moderee
#GVIF = 1.001 a 1.003 = plutot faible donc pas de colinearite très marquée.





## B. GAM ----
# Les GAM permettent d'analyser des données dont la distribution de la variable réponse n'est pas connue.
# Dans notre cas, elles permettent d'analyser nos données de peristome qui ne suivent pas une loi normale
# (même après transformation logarithmique) et dont la distribution est dificilement identifiable.


# On transforme Date en temps depuis le début de l'expérience parce que les GAM n'acceptent pas les données Dates
temps <- elona_recapture[,12] - min(elona_recapture$Date)
elona_recapture$temps<-temps
elona_recapture$temps<-as.numeric(elona_recapture$temps)
class(elona_recapture$temps)

# On enlève les NA, ça permet d'éviter les messages d'erreurs lors de la vérification de l'homogénéité des variances
elona_recapture_sansNA<-na.omit(elona_recapture)
hist(elona_recapture_sansNA$temps) 

### 1. Création du modèle GAM ----
gam_peris <- mgcv::gam(Peristome ~ Site + Stage + s(ID_1erC,bs="re")+ s(temps,bs="re"),
          family = gaussian,
          data = elona_recapture_sansNA)
"regarder pourquoi on doit indiquer gaussian 
à voir aussi il est possible de mettre bs = re ou bs = cr "


### 2. Conditions de validité----
par(mar = c(2, 2, 2, 2))
vis.gam(gam_peris, theta = -20, color = "heat") # fonctionne pas

#### a. Normalité des résidus ----
par(mfrow=c(1,2))
# Histogram
hist(gam_peris$residuals,col='dodgerblue3',xlab="residuals",main="Check Normality")
# Quantile-Quantile plot
qqnorm(gam_peris$residuals,pch=16,col='dodgerblue3',xlab='')
qqline(gam_peris$residuals,col='red')
# Mieux que le précédent modèle pour le qqplot, mais malgré tout pas une distribution normale

#### b.Homoscédasticité ----
# Homogénéité de la variance
plot(residuals(gam_peris)~fitted(gam_peris)
     , col='dodgerblue3'
     , pch=16)
abline(h = 0)
# On observe une meilleure homogénéité de la variance (il y a toujours une hétérogénéité, mais plus faible)

plot(residuals(gam_peris)~ elona_recapture_sansNA$Site, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Sites",
     main = "")
abline(h = 0)
#Faible différence de répartition des résidus entre les deux sites

plot(residuals(gam_peris)~ elona_recapture_sansNA$temps, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Temps",
     main = "")
abline(h = 0) #repartition avec en abcisse la duree depuis le debut des echantillonages mais ce sont des residus.  

plot(residuals(gam_peris)~ elona_recapture_sansNA$Stage, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Stage",
     main = "")
abline(h = 0)
#heterogenéité plus importante avec une plus grande variance pour les juvéniles, sûrement liée à la croissance des individus.


### 3. Analyse des résultats----
anova(gam_peris)
#Effet significativite du Stade de developpement et du site (p<2e-16 et p=1.63e-06)
summary(gam_peris)
#significativite des effets de la variable aleatoire de l'individu (p<2e-16), mais pas du temps. 
#Significativite de l'effet de du Site : Peristome ont un diametre legerement plus gros sur le site recent a faible densite de population (augmentation legere mais tres significative)
#Même interprétation que pour le diamètre pour le stade juvénile et subadulte : un effet significativement négatif par rapport au stade adulte, ce qui est lié à la croissance

#Significativite du modele GAM : R²
r2(gam_peris)
# R2: 0.825
# R-sq.(adj) =  0.758   Deviance explained = 82.5%
#Le modele GAM est tres significatif : il explique tres bien la variable reponse (Peristome)




# III - DEMOGRAPHIE ------------------------------------------------------------

###1. Préparation des données ----
#On cherche à expliquer les proportions dans chaque classe d'age en fonction des sites et du lieu:
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
proportions <- data.frame(prop_AD, prop_SUB, prop_JUV)
# Site <- elona_dem$Site
# Event <- elona_dem$Event
# proportions <- data.frame(prop_ad, prop_sub, prop_juv, Site, Event)

###2. Sélection du modèle ----

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

# Malgré nos tentatives, nous n'avons donc pas réussis à analyser la démographie. 


" A partir d'ici je ne comprend pas ce qui est fait ni dans quelle cadre cela s'inscrit "

# Homoscédasticité : ncvTest {car} (cad homogeneite des variances)
# (m1$Residuals)#ne fonctionne pas sur les modeles mixtes ;(
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

boxplot(elona_recapture$Diameter~elona_recapture$Stage, varwidth = TRUE, ylab = "Diameter", xlab = "Age Stage", col='grey', main = "")




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
# A<-CookD(m1,newwd=TRUE)#le calcul semble trop long, perturbe le logiciel
# plot(A)
# plot.lm(m1)

# LIMITE DE L'ETUDE ------------------------------------------------------------

# Certaines imprécision nous limitent dans notre compréhension des données par exemple
# Nous ne pouvons pas déterminer si les quadrats (au sein d'un même site) ont des ressources différentes ou pas
# Le jeu de données ne nous permet pas de prendre en compte si les escargots bougent ou pas car nous n'avons leur quadrat que pour leur première capture
# Nous ne savons pas non plus s'il est possible pour les escargots de parcourir les 500m qui séparent les sites. 

# Nous n'avons que des psuedo-réplicats il serait intéressant d'avoir d'autres sites.
# De plus, rien n'indique que ces sites sont équivalents en termes de ressources alimentaires, qualité d'habitats
# Pour réellement étudier l'effet de la densité de population sur les caractéristiques morphologiques il faudrait réaliser une étude démographique et utiliser la densité de population en varaible explicative, 
# mais aussi prendre en compte les possibles différences dans les caractéristiques (biotiques et abiotiques) des milieux

# Pour approfondir les analyses, il faudrait plus de détails sur la méthode d'échantillonnage et le milieu (type de substrat, conditions de vie, vitesse de déplacement, mode de mesure...),
# et un partitionnement plus fin de la classe junévile aurait permis une variance plus faible dans cette catégorie.  
# Nous avons fait l'hypothèse que les escargots n'ont pas migré d'un site à l'autre durant toute la durée de l'échantillonnage.

# L'estimation de la démographie aurait pu nous aiguiller sur l'hypothèse que les escargots du nouveau sites n'ont pas encore atteint la capacité limite du milieu. 

# CONCLUSION -------------------------------------------------------------------

#Dans les modèles testés, le diamètre et le péristome sont en premier lieu affecté par le stade de développement
#Ce qui est cohérent avec l'interprétation biologique: la croissance impacte la taille. 

#Le site a également un effet important : les escargots adultes sont plus gros sur le nouveau site, avec une faible densité de population.
#Cela peut être lié à différents facteurs : la plus grande quantité de ressources disponibles (liées à la densité de population), le type de milieu (substrat, exposition aux prédateurs)...
