# Noms Auteur.ices : BARON Anaelle, BRES-MONVOISIN Chloe, CORDIER Emilie, DESNYDER Eli, DAVID Meline
# M2 MODE 2026-2027
# Projet ASA (Advanced Statistical Analysis): sujet ELONA

# IMPORTATION DES LIBRARIES ----------------------------------------------------
# install.packages("readxl")
# install.packages("lme4")
# install.packages("mgcv")
library(lme4)
library(readxl)
library(GGally)
library(ggcorrplot) # Graphe de correlation
library(ggplot2)
library(tidyverse)
library(predictmeans)# Diagramme de cook
library(nlme)
library(lmerTest)
library(mgcv)
library(corrplot) # Calcul de la matrice de correlation
library(lattice) # Graphiques multipanels
library(ade4) # PCA
library(performance) # r2 LMM
library(car)
library(MASS)
library(dplyr)
library(sf)
library(mapview)
library(lmerTest)





# IMPORTATION DES DONNEES ------------------------------------------------------

## 1. Importation et gestion des classes ----
# Le tableau de donnees est organise en deux feuilles :
# firstcapture donnant l inventaire de tous les individus, et recapture
# informant sur les donnees prises lors de la recapture de ces memes individus.

# Importation de recapture
elona_recapture <- read_xlsx("data_elona.xlsx", sheet = 3,
                             col_types = c('text', 'numeric', 'numeric', 'text',
                                           'text', 'text', 'numeric', 'numeric',
                                           'numeric', 'text', 'text'))
elona_recapture$Site <- as.factor(elona_recapture$Site)
elona_recapture$Stage <- as.factor(elona_recapture$Stage)
elona_recapture$Activity <- as.factor(elona_recapture$Activity)
elona_recapture$Type_Event <- as.factor(elona_recapture$Type_Event)
elona_recapture$Event <- as.factor(elona_recapture$Event)
elona_recapture$ID_1erC <- as.factor(elona_recapture$ID_1erC)
elona_recapture <- data.frame(elona_recapture$Site, elona_recapture$Diameter,
                              elona_recapture$Peristome, elona_recapture$Stage,
                              elona_recapture$Activity, elona_recapture$Type_Event,
                              elona_recapture$Day, elona_recapture$Month,
                              elona_recapture$Year, elona_recapture$Event,
                              elona_recapture$ID_1erC)
colnames(elona_recapture) <- c('Site', 'Diameter', 'Peristome', 'Stage',
                               'Activity', 'Type_Event', "Day", "Month", "Year",
                               "Event", "ID_1erC")

# Importation de firstcapture
elona_firstcapture <- read_xlsx("data_elona.xlsx", sheet = 2,
                                col_types = c('text', 'numeric', 'numeric',
                                              'text', 'text', 'numeric',
                                              'numeric','numeric', 'text', 'text',
                                              'text', 'text', 'text'))
elona_firstcapture$Site <- as.factor(elona_firstcapture$Site)
elona_firstcapture$Stage <- as.factor(elona_firstcapture$Stage)
elona_firstcapture$Type_Event <- as.factor(elona_firstcapture$Type_Event)
elona_firstcapture$Event <- as.factor(elona_firstcapture$Event)
elona_firstcapture$ID_1erC <- as.factor(elona_firstcapture$ID_1erC)
elona_firstcapture$IDZone <- as.factor(elona_firstcapture$IDZone)
elona_firstcapture$X <- as.factor(elona_firstcapture$X)
elona_firstcapture$Y <- as.factor(elona_firstcapture$Y)
elona_firstcapture <- data.frame(elona_firstcapture$Site, elona_firstcapture$Diameter,
                                 elona_firstcapture$Peristome, elona_firstcapture$Stage,
                                 elona_firstcapture$Type_Event, elona_firstcapture$Day,
                                 elona_firstcapture$Month, elona_firstcapture$Year,
                                 elona_firstcapture$Event, elona_firstcapture$ID_1erC,
                                 elona_firstcapture$IDZone, elona_firstcapture$X,
                                 elona_firstcapture$Y)
colnames(elona_firstcapture) <- c('Site', 'Diameter', 'Peristome', 'Stage',
                                  'Type_Event', "Day", "Month", "Year", "Event",
                                  "ID_1erC", "IDZone", "X", "Y")




## 2. Nettoyage des donnees X ----

table(elona_recapture$Site)
table(elona_recapture$Stage)
# Le nombre de modalites dans "Stage" ne correspond pas avec les indications
# Il a des categories (?, JUV? JUV/SUB, SUB/AD) tres peu representees. 
# Nous faisons le choix de les recategoriser comme suit. 
elona_recapture$Stage[which(elona_recapture$Stage=="?")] <- NA
elona_recapture$Stage[which(elona_recapture$Stage=="JUV?")]<- "JUV"
elona_recapture$Stage[which(elona_recapture$Stage=="JUV/SUB")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="SUB/AD")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="")]<- NA
elona_recapture$Stage[which(elona_recapture$Stage=="NA")] <- NA
table(elona_recapture$Stage)
# On supprime ensuite les categories qui n'existent plus
elona_recapture$Stage = droplevels(elona_recapture$Stage)
table(elona_recapture$Stage)



# Meme chose pour les modalites de "Activity"
table(elona_recapture$Activity)
elona_recapture$Activity[which(elona_recapture$Activity=="IMM")]<- "IMMO"
elona_recapture$Activity = droplevels(elona_recapture$Activity)
table(elona_recapture$Activity)

# Pour une meilleure lisibilite des graphiques nous avons modifie le nom des
# classes pour "Event" (E1 -> E01)
elona_recapture$Event <- as.character(elona_recapture$Event)
for (i in 1:nrow(elona_recapture)){
  if (nchar(elona_recapture$Event[i]) == 2){
    elona_recapture$Event[i] <- paste(substr(elona_recapture$Event[i], 1, 1),
                                      substr(elona_recapture$Event[i], 2, 2),
                                      sep = "0")
  }
}
elona_recapture$Event <- as.factor(elona_recapture$Event)

elona_firstcapture$Event <- as.character(elona_firstcapture$Event)
for (i in 1:nrow(elona_firstcapture)){
  if (nchar(elona_firstcapture$Event[i]) == 2){
    elona_firstcapture$Event[i] <- paste(substr(elona_firstcapture$Event[i], 1, 1),
                                         substr(elona_firstcapture$Event[i], 2, 2),
                                         sep = "0")
  }
}
elona_firstcapture$Event <- as.factor(elona_firstcapture$Event)

# Ajouter donnees de sites a elona_recapture
elona_recapture<-merge(elona_recapture,elona_firstcapture[,c(10,11)], by="ID_1erC")



# Meme chose pour les modalites de "Stage" dans first_capture
table(elona_firstcapture$Stage)
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="?")] <- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV?")]<- "JUV"
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV/SUB")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="SUB/AD")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="NA")]<- NA
elona_firstcapture$Stage = droplevels(elona_firstcapture$Stage)
table(elona_firstcapture$Stage)

## 3. Rajout des donnees de firstcapture dans recapture ----

# Certains escargots ne sont pas recaptures, ils ne figurent donc pas dans le
# dossier recapture, mais il faut tout de meme les comptabiliser dans le modele.
# Il est donc necessaire de rajouter ces donnees-la dans recapture,
# sans toutefois ajouter ceux qui ont ete recapture
# (leur premiere capture existant deja dans recapture)

elona_recapture<-elona_firstcapture  %>%
  dplyr::select(!c(IDZone,X,Y))%>%
  anti_join(elona_recapture,by="ID_1erC") %>%
  bind_rows(elona_recapture)


## 4. Modification des dates ----
elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),
                                      as.character(elona_recapture$Month),
                                      as.character(elona_recapture$Day),
                                      sep = "-"))


# QUESTION : -------------------------------------------------------------------
# What is the impact of population density on biometry and individual growth
# in Elona quimperianal ? We aim to determine whether the two populations share
# the same demographic characteristics and traits.


# EXPLORATION DES DONNEES ------------------------------------------------------

## 1. Valeurs extremes ----

# Diametre
boxplot(elona_recapture$Diameter)
summary(elona_recapture$Diameter)
# On a des valeurs tres faibles mais ça ne parait pas improbable


# Peristome
par(mfrow=c(1,2))
boxplot(elona_recapture$Peristome)
# Une valeur tres tres haute (901mm) => Cela parait tres improbable, nous avons
# fait le choix de la retirer 
summary(elona_recapture$Peristome)
elona_recapture$Peristome[which.max(elona_recapture$Peristome)] <- NA

boxplot(elona_recapture$Peristome)
# On en a encore 3 grandes valeurs, on ne sait pas si c'est probable ou pas
summary(elona_recapture$Peristome)
elona_recapture[which(elona_recapture$Peristome>15),]
# Les 3 points correspondent a des valeurs de peristome plus larges que la taille
# de l'escargot (diametre), donnant lieu a un paradoxe biologique.
# De plus, ces valeurs potentiellement aberrantes ne sont pas en raccord avec
# d'autres mesures de diametre et peristome sur Elona quimperiana, comme l'article
# DAGUZAN J. & GLOAGUEN J.-C., 1986.- Contribution a l’ecologie d’Elona quimperiana
# (de Ferussac) (Gasteropode Pulmone Stylommatophore) en Bretagne occidentale. Haliotis, 15 : 17-30.

# Nous choississons donc de remplacer ces trois valeurs par NA.
elona_recapture$Peristome[elona_recapture$Peristome>15] <- NA

# On regarde egalement s'il y a d'autres valeurs de peristomes qui sont superieures au diametres.
which(elona_recapture$Peristome > elona_recapture$Diameter)
# Il n'y a aucun autre individu dont le peristome est superieur au diametre.


## 2. Normalite de Y----

summary(elona_recapture$Peristome)
hist(elona_recapture$Diameter,col='chartreuse',xlab="Diameter",main="Check Normality
 (Diameter) ",breaks=100)
hist(elona_recapture$Peristome,col='purple',xlab="Peristome",main="Check Normality
(Peristome)",breaks=100)
# Ni le Diametre, ni le Peristome ne semblent suivre une loi normale. 
# Les queues de distribution ne sont pas symetriques.


## 3.  Relation entre les Ys (Diameter & Peristome) ----

ggplot(data = elona_recapture,  
       aes(geom_point, x = Peristome, y = Diameter,
           shape = Site, col = Stage)) +
  geom_point() +
  scale_shape_manual(values = c(0,1,2))


# Il semble y avoir une colinéarité assez forte entre le diamètre et le péristome.
# De plus, si l'on considère la classe d'age,on observe que les individus avec un 
# petit diamètre et péristome sont les juvéniles, ce qui fait sens biologiquement.
# Le diamètre et le péristome des individus des deux sites sont confondus pour les juvéniles.

ggplot(data = elona_recapture %>% filter(Stage == "AD" & Diameter >= 15),  
       aes(geom_point, x = Peristome, y = Diameter,
           col = Site)) +
  geom_point() +
  scale_shape_manual(values = c(0,1,2))

# En ne regardant que les adultes, il semble qu'il y ait une différence de taille selon le site. 


## 4. Relation potentielle entre X et Y ----

par(mfrow=c(1,2))

boxplot(elona_recapture$Diameter ~ elona_recapture$Site, main = "Diameter")
boxplot(elona_recapture$Peristome~elona_recapture$Site, main = "Peristome")
# Il semble que les escargots du site Recent ont un diametre et un peristome plus
# important, sans que cela soit significatif. 


## 5. Analyse des interactions entre les X ----

# Interactions entre Stage et Activity
plot(elona_recapture$Stage ~ elona_recapture$Activity,
     ylab = "Demographic stages", xlab="Activity")
# Dans la categorie ACT, il y a autant de juveniles que d adultes.
# Dans les categories IMMO et MORT, la majorite des observations sont des individus adultes

# Interactions entre Stage & Type_Event
plot(elona_recapture$Stage ~ elona_recapture$Type_Event,
     ylab = "Demographic stages", xlab="Type Event")
# Les proportions de juveniles et subadultes sont plus elevee lors des premieres
# captures (type event = C), c est logique car plus les individus sont ages plus
# ils ont de chance d'avoir deja ete captures.

# Interactions entre Activity & Type_Event
plot(elona_recapture$Activity ~ elona_recapture$Type_Event,
     ylab = "Activity", xlab="Type_Event")
# Graphique peu lisible car la majorite (86%) des escargots sont actifs.


## 6. Analyse des colinearites ----

# Il faut aussi verifier une eventuelle colinearite entre les X.

par(mfrow=c(1,2))

#### a. Year ----

## Diameter and year 
boxplot(elona_recapture$Diameter~elona_recapture$Year, varwidth = TRUE, ylab = "Diameter", 
        xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,25), main = "Recapture Diameter ")
# Les diamètres des coquilles ne semblent pas augmenter au cours du temps.


##peristome and year
boxplot(elona_recapture$Peristome~elona_recapture$Year, varwidth = TRUE, ylab = "Peristome",
        xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,12), main = "Recapture Peristome")
# Tendance à légère augmentation au cours du temps de la taille du peristome


#### Event----
##Diameter and Event
boxplot(elona_recapture$Diameter~elona_recapture$Event, varwidth = TRUE, 
        ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
# Le diamètre moyen varie peu, en revanche les variances sont très différentes selon l'évènement.


####TypeEvent -----
boxplot(elona_recapture$Diameter~elona_recapture$Type_Event, varwidth = TRUE, 
        ylab = "Diameter", xlab = "Type_Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
#Difference de diametre en fonction de l'etat (logique car les individus recapturés ont plus de chance d'être plus vieux)


## 7. Selection lignes sans NA pour le Peristome -----
summary(elona_recapture$Peristome) # Initiallement, 139 NA
summary(elona_recapture$Diameter) # Initiallement, 81 NA
elona_recapture_cor <- 
  subset (elona_recapture, 
          elona_recapture$Peristome != "NA" &  elona_recapture$Diameter!="NA" )
dim(elona_recapture_cor) ; dim(elona_recapture) # Verifier le nb de lignes supprimees


## 8. Matrice de correlation ----
MRC <- cor(elona_recapture_cor[, c(2,3,6,7,8)])
MRC
# Correlation forte entre Diametre et Peristome (93%), comme nous l avions remarque graphiquement.

# We represent plot for each X continuous covariate pairs
# We calculate correlation between each pair of X covariate
corrplot.mixed(MRC,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,
               tl.cex = 0.7,number.cex =0.8, main = "Recapture")


sum(is.na(elona_recapture_cor)) # 6 NA 
which(is.na(elona_recapture_cor)) # 6 NA 

# Seules les variables numeriques peuvent etre prises en compte dans la matrice de correlation


## 9. Description spatiale ----

# Cette partie ne sert qu a visualiser les sites et les quadrats et a verifier les distances
# elona_sf contains the portion of the data relevant for analysis
elona_sf <- elona_firstcapture[,c("Site", 
                                  "X", 
                                  "Y", 
                                  "IDZone")] %>%
  filter(!is.na(elona_firstcapture$X))
# The coordinates are turned into an sf object in the geometry column
elona_sf$X <- as.numeric(as.character(elona_sf$X))
elona_sf$Y <- as.numeric(as.character(elona_sf$Y))
elona_sf <- elona_sf %>%
  st_as_sf(coords = c("X", 
                      "Y"))
# We separate the old and new sites because some zone with the same ID (but different coordinates)
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
# After filtering to keep only one line per zone we bind the 2 sites back together
elona_sf <- rbind(elona_sf_old, 
                  elona_sf_new)

# Distances between the centroids of the two sites
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
# Toutes les variables explicatives sont catégorielles et la variable réponse est quantitative continue 

# Modele mixte du diametre avec toutes* les variables explicatives, dont la date
# et n° d identification en variables aleatoires :
# *Event et Date sont colineaires, nous n'avons conserve que Date
# *IDZone n'est pas pertinent a ajouter car les noms sont repetes entre les deux sites

m1_comp <- lmer(Diameter ~ Site + Stage + Activity + Type_Event +(1|Date) + (1|ID_1erC),
                data = elona_recapture)
anova(m1_comp)

# Toutes les variables sauf Activity ont un effet significatif donc on la retire
m1 <- lmer(Diameter ~ Site + Stage  + Type_Event +(1|Date) + (1|ID_1erC),
           data = elona_recapture)
anova(m1)

# Nous avions vu précédement que le site semblait avoir un effet différent sur le diamètre des Adultes (ligne 187)
# Il y aurait donc une interaction entre le site et l'age, ajoutons la au modèle
# Modele mixte du diametre avec le site et l'age et leur interaction en variables fixes, et la date et n° d'identification en variables aleatoires :
m1_inter <-lmer(Diameter ~ Site + Stage + Site:Stage + Type_Event +(1|Date)+(1|ID_1erC), data = elona_recapture)

anova(m1_inter)
# Le site est moins significatif que dans le modele precedent, mais l interaction
# avec l age est aussi significative (elle a surement "absorbe" une partie de l'effet du Site).

# Nous conserverons le modele m1_inter


## 2. Conditions de validite ----

### a. Normalite des residus ----

par(mfrow=c(1,2))
# Histogramme
hist(residuals(m1_inter),col='dodgerblue3',xlab="residuals",main="Check Normality") 
# Quantile-Quantile plot
qqnorm(residuals(m1_inter),pch=16,col='dodgerblue3',xlab='')
qqline(residuals(m1_inter),col='red')

# Les residus ne semblent pas suivre distribution normale.
# La normalite des residus n'est pas tout a fait respectee.
# Les valeurs extremes s'eloignent un peu trop. 


### b. Homoscedasticite des residus ----

plot(m1_inter)
# Il semble y avoir moins de variance pour les valeurs extremes
# Heteroscedasticite que l'on prendra en compte lors de l'interpretation


### c. Residus independants ----

# L independance des residus n'est pas un critere pour les modeles lineaires
# general mixte, donc n'est pas necessaire d etre valide. 


### d. Colinearite entre les variables X num ou facteur -----

#vif{car}
vif(m1_inter) # GVIF ~1 donc colinearite moderee
# GVIF = 1.04 a 2.48 = plutot faible donc pas trop de colinearite

# Regles lecture fiv coeff 
# VIF = 1 : il n’existe aucune correlation entre une variable predictive
# donnee et toute autre variable predictive du modele.
# VIF entre 1 et 5 : il existe une correlation moderee entre une variable
# predictive donnee et d’autres variables predictives du modele.
# VIF > 5 : il existe une forte correlation entre une variable predictive donnee
# et d’autres variables predictives dans le modele.

# GVIF = generalized Variation Inflation Factor | GVIFj =(1/ (1−Rj^2 )^d)
# GVIF plutot que VIF utilise quand le modele comprend des variables categorielles a plusieurs niveaux.
# Stade juvenile est le facteur avec un effet le plus fort.

# Significativite de la variance associe au facteur aleatoire
# intervals(m1) # Ne Fonctionne pas, doit etre adapte a lme et pas a lmer, qu on utilise ici


mod_test <- lm (Diameter ~ Site + Peristome + Stage + Type_Event, data = elona_recapture_cor)
mod_test_tot <- lm (Diameter ~ Site + Peristome + Stage + Type_Event + Event + Day + ID_1erC,
                    data = elona_recapture_cor)
# Modele lineaire simple avec toutes les variables qui ne sont pas des
# "aliased coeff" pour faire fonctionner la fonction vif{car}
vif(mod_test_tot) # Erreur : donne des resultats NA ou infini
# Erreur :   there are aliased coefficients in the model

# #                  GVIF  Df GVIF^(1/(2*Df))
# Site       1.594304e+02   1       12.626574
# Peristome  5.746555e+00   1        2.397197
# Stage      1.100995e+01   2        1.821572
# Type_Event 5.381555e+00   2        1.523095 
# Event      2.108339e+06  19        1.466964
# Day        3.388570e+02   1       18.408068
# ID_1erC    3.271475e+07 508        1.017177

# Multicolinearite moderee pour  Peristome et Type Event ?
# Forte pour Site, Day, Peristome (erreur type gonflee ?)
# Grave pour Event, ID_1erC (estimateur instable) --> on ne les utilise pas trop dans les analyses

sort(vif(mod_test_tot), decreasing = TRUE)
vif(mod_test) # GVIF = 1 --> colinearite moderee

# confint (mod_test_tot, level = 0.95, method = "boot", nsim = 500)# /!\ ne pas run car calcul long!!
# 500 bootstrap
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

## 3. Interpretation ----
r2_nakagawa(m1_inter)
# 42% de la variance est expliquee par les effets fixes
# et 84% de la variance est expliquee par les effets fixes et aleatoires. 
# L effet aleatoire explique donc une grande part de la variabilite. 
# Les variables fixes ont toutefois un effet significatif. 
anova(m1_inter)
# Le stade est la variable qui a le plus d'effet (p value tres faible).
# Toutefois le site et son interaction avec le stade ont un effet signifcatif. 
# Mais leur effet est bien plus faible. 
summary(m1_inter)
# Dans le summary on voit que l'estimate associe au changement de Site est
# faible (devant l'intercept et devant l'effet du changement de stade (JUV)).
# Cela confirme l'effet significatif mais faible du site.

# Le modele est donc valide mais montre que le site a un effet significatif
# mais faible sur le diametre comparativement aux autres variables. 



#II - PERISTOME


## A. LMM ----


### 1. Creation du modele ----


# Le site et le stade de developpement sont des variables factorielles dont on observe l'effet, 
# tandis que la date et le n° d'identifications sont des variables avec de
# nombreuses valeurs differentes (d'ou les variables a effet fixe), qui impactent
# egalement la variable reponse mais ne constituent pas l'effet principal observe
# en priorite (d'ou les variables a effets aleatoires)

# *Event et Date sont colineaire, nous n'avons conserve que Date
# *IDZone n'est pas pertient a ajouter car les noms sont repetes entre les deux sites

m1_peris <- lmer(Peristome ~ Site + Stage + Activity + Type_Event +(1|Date) + (1|ID_1erC),
                 data = elona_recapture)
anova(m1_comp)
# Toutes la variables sauf Activity sont significatives, on la retire donc.

m_peris <- lmer(Peristome ~ Site + Stage + Type_Event + (1|ID_1erC) + (1|Date),
                data=elona_recapture)
anova(m_peris)

# Comme pour le diamètre le dite semble avoir un effet différent sur le péristome des adultes (ligne 187)
m2_inter <-lmer(Peristome ~ Site + Stage + Site:Stage + Type_Event +(1|Date)+(1|ID_1erC), data = elona_recapture)
anova(m2_inter)
# L'intéraction porte une part significative de la variance dans le modèle

### 2. Conditions de validité

#### a. Normalite des residus ----

par(mfrow=c(1,2))
# Histogram
hist(residuals(m2_inter),col='dodgerblue3',xlab="residuals",main="Check Normality") 
# Quantile-Quantile plot
qqnorm(residuals(m2_inter),pch=16,col='dodgerblue3',xlab='') #les residus ne semblent pas suivre distribution normale
qqline(residuals(m2_inter),col='red')
# La normalité des résidus n'est pas tout à fait respectée. Les valeurs extrèmes s'éloignent un peu trop. 


#### b. Homoscédasticité ----
plot(m2_inter) # heteroscedasticite encore plus forte que l'on prendra en compte lors de l'interpretations
#La variance est plus faible pour les hautes valeurs de péristome



#### c. Residus independants ----

# L independance des residus n est pas un critere pour les modeles lineaires
# general mixte, donc n'est pas necessaire d'etre valide. 


#### d. Colinearite entre les variables X num ou facteur -----

vif(m2_inter) #GVIF ~1 donc colinearite moderee
#GVIF = 1.001 a 1.003 = plutot faible donc pas de colinearite très marquée.




## B. GAM ----

# Les GAM permettent d analyser des donnees dont la distribution de la variable reponse n est pas connue.
# Dans notre cas, elles permettent d analyser nos donnees de peristome qui ne suivent pas une loi normale
# (meme apres transformation logarithmique) et dont la distribution est dificilement identifiable.


# On transforme Date en temps depuis le debut de l experience
# parce que les GAM n acceptent pas les donnees au format Dates.
temps <- elona_recapture[,12] - min(elona_recapture$Date)
elona_recapture$temps <- temps
elona_recapture$temps <- as.numeric(elona_recapture$temps)
class(elona_recapture$temps)

# On enleve les NA, ça permet d eviter les messages d erreurs lors de la verification de l homogeneite des variances
elona_recapture_sansNA<-na.omit(elona_recapture)
hist(elona_recapture_sansNA$temps) 

### 1. Creation du modele GAM ----

gam_peris <- mgcv::gam(Peristome ~ Site + Stage + Activity + Type_Event
                       + s(ID_1erC,bs="re") + s(temps,bs="re"),
          family = gaussian,
          data = elona_recapture_sansNA)
anova(gam_peris)
# Seule Activity n est pas significative, on le supprime donc du modele.

gam_peris <- mgcv::gam(Peristome ~ Site + Stage + Type_Event + s(ID_1erC,bs="re")+ s(temps,bs="re"),
                       family = gaussian,
                       data = elona_recapture_sansNA)

# La famille gaussienne du modele est declaree par defaut, et elle se rapproche
# le plus des donnees (distribution proche de normale) par rapport aux autres familles possibles
# bs = re correspond a des termes parametriques penalises par une penalite de Ridge;
# bs = cr correspond a une base a cannelure cubique (basis cubic spline)
# definie par une taille de jeu de noeuds modeste (avec une expansion equivalete entre les covariables)
# On change l'argument bs pour temps;
# Si on change bs pour ID_1erC : Erreur dans quantile.default(xu, seq(0, 1, length = nk)) :
# les facteurs (non ordonnes) ne sont pas autorises.

anova(gam_peris)

# On rajoute une intéraction entre le site et le stade developpement pour les mêmes raisons que précédemment
gam_peris_inter <- mgcv::gam(Peristome ~ Site * Stage + Type_Event + s(ID_1erC,bs="re")+ s(temps,bs="re"),
                       family = gaussian,
                       data = elona_recapture_sansNA)


### 2. Conditions de validite----

par(mar = c(2, 2, 2, 2))
vis.gam(gam_peris_inter, theta = -20, color = "heat")

### 2. Conditions de validité----
par(mar = c(2, 2, 2, 2))
vis.gam(gam_peris_inter, theta = -20, color = "heat") # pas de changement avec bs = re ou cr


#### a. Normalite des residus ----

par(mfrow=c(1,2))
# Histogram
hist(gam_peris_inter$residuals,col='dodgerblue3',xlab="residuals",main="Check Normality")
# Quantile-Quantile plot
qqnorm(gam_peris_inter$residuals,pch=16,col='dodgerblue3',xlab='')
qqline(gam_peris_inter$residuals,col='red') #pas de difference avec bs = re ou bs = cr
# Mieux que le précédent modèle pour le qqplot, mais malgré tout pas une distribution normale

#### b.Homoscédasticité ---- 
# Homogénéité de la variance
plot(residuals(gam_peris_inter)~fitted(gam_peris_inter)


#### b.Homoscedasticite ---- 

# Homogeneite de la variance
plot(residuals(gam_peris)~fitted(gam_peris)
     , col='dodgerblue3'
     , pch=16)
abline(h = 0)
# On observe une meilleure homogeneite de la variance (il y a toujours une heterogeneite, mais plus faible)

plot(residuals(gam_peris_inter)~ elona_recapture_sansNA$Site, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Sites",
     main = "")
abline(h = 0) #
# Faible difference de repartition des residus entre les deux sites

plot(residuals(gam_peris_inter)~ elona_recapture_sansNA$temps, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Temps",
     main = "")
abline(h = 0)
# Repartition avec en abcisse la duree depuis le debut des echantillonages mais ce sont des residus.  

plot(residuals(gam_peris_inter)~ elona_recapture_sansNA$Stage, 
     pch=16,
     col="dodgerblue3",
     ylab = "Residuals",
     xlab = "Stage",
     main = "")
abline(h = 0)
# Heterogeneite plus importante avec une plus grande variance pour les juveniles,
# surement liee a la croissance des individus.
# Pour les 4 plot : pas de difference avec bs=cr ou bs = res



### 3. Analyse des resultats----


### 3. Analyse des résultats----
anova(gam_peris_inter)
# Effet significatif du site, du stade de développement, du site et de l'intéraction entre le site et le stade de développement
# Parametric Terms:
#   df       F  p-value
# Site        1  27.622 1.78e-07
# Stage       2 101.518  < 2e-16
# Type_Event  2  46.589  < 2e-16
# Site:Stage  2   8.623 0.000193

summary(gam_peris_inter)  

#Tous les coefficients des variables fixe ont une valeur significative sauf StageSUB
#significativite des effets de la variable aleatoire de l'individu (p<2e-16), mais pas du temps. 
#Significativite de l'effet de du Site : Peristome ont un diametre legerement plus gros sur le site recent a faible densite de population (augmentation legere mais tres significative)
#Meme interpretation que pour le diametre pour le stade juvenile et subadulte : un effet significativement negatif par rapport au stade adulte, ce qui est lie a la croissance


#Significativite du modele GAM : R²
r2(gam_peris_inter)#pas de changement selon l'argument bs
# R2: 0.848
# R-sq.(adj) =  0.785   Deviance explained = 84.8%
#Le modele GAM est tres significatif : il explique tres bien la variable reponse (Peristome)


# Significativite des effets de la variable aleatoire de l'individu (p<2e-16), mais pas du temps. 
# Significativite de l'effet du Site : Peristome ont un diametre legerement plus
# gros sur le site recent a faible densite de population (augmentation legere mais tres significative)
# Meme interpretation que pour le diametre pour le stade juvenile et subadulte :
# un effet significativement negatif par rapport au stade adulte, ce qui est lie a la croissance



# III - DEMOGRAPHIE ------------------------------------------------------------


### 1. Preparation des donnees ----

# On cherche a expliquer les proportions dans chaque classe d'age en fonction des sites et du lieu:
# f(proportion de chaque classe d'age) = Site + Date
# On considere que les individus retrouves morts comme appartenant a leur classe d age respective. 

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

# Visualisation des quantites par site
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
# Plus forte proportion de juveniles dans l'ancien site (forte densite);
# et d'adultes dans le nouveau site (faible densite)


# On extrait ensuite les proportions dans un nouveau tableau
prop_AD <- elona_dem$Prop_AD
prop_SUB <- elona_dem$Prop_SUB
prop_JUV <- elona_dem$Prop_JUV
proportions <- data.frame(prop_AD, prop_SUB, prop_JUV)
# Site <- elona_dem$Site
# Event <- elona_dem$Event
# proportions <- data.frame(prop_ad, prop_sub, prop_juv, Site, Event)


### 2. Selection du modele ----

# On veut analyser les proportions des classes d ages par rapport aux sites et aux evenements
# Il y a 4 possibilites :


#### a. Regression logistique ----
# En effet on veut une analyse de proportions donc on pourrait realiser 3 modeles
# de regression logistique.

# Cependant ici nous voulons analyser 3 variables quantitatives differentes,
# ce qui n'est pas compatible avec une regression logistique multivariee.


#### b. RDA ----
# En effet on veut analyser les relations entre un tableau avec les proportions 
# et un tableau avec les caracteristiques des captures.

# Cependant la RDA se concentre sur la prediction de donnees,
# ce qui n'est pas ce que nous recherchons.

# ↓ code de la tentative
# On ecrit le modele
# RDA <- rda(proportions ~ Site + Event, data = elona_dem)
# summary(RDA)
# RsquareAdj(RDA)
# Resultats du modele
# anova(RDA, by = "margin")

# plot(RDA, display=c("species","cn"))


#### c. ACP ----
# Il s'agit d'un equivalent de la RDA mais concentre sur l'explication des donnees.

# Cependant la structure des donnees ne correspond pas a la methode.

# ↓ code de la tentative
# demo_PCA <- dudi.pca(proportions[,1:5], scannf=FALSE)
# demo_PCA$cw
# demo_PCA$lw

# library(FactoMineR)
# demo_PCA <- PCA(proportions, quali.sup = c(Site, Event))
# same as dudi.pca(palmer[,3:6], scannf=FALSE, scale = TRUE)

# library(Factoshiny)
# res <- Factoshiny(proportions)


#### d. Regression lineaire multiple ----
# La structure du modele n'est pas analysable par les outils communs.

# ↓ code de la tentative
# demo_model <- lm(cbind(elona_dem$Prop_AD, elona_dem$Prop_SUB, elona_dem$Prop_JUV)
#                  ~ elona_dem$Site + elona_dem$Event)
# summary(demo_model)
# step(demo_model)
# AIC(demo_model)
# drop1(demo_model)
# add1(demo_model)

# Malgre nos tentatives, nous n avons donc pas reussi a analyser la demographie. 


# Analyse du bootstrap

# Homoscedasticite : ncvTest {car} (cad homogeneite des variances)
# (m1$Residuals)#ne fonctionne pas sur les modeles mixtes ;(
confint(m1, level = 0.95, method = "boot", nsim = 50) # ! attention peut etre long
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

boxplot(elona_recapture$Diameter~elona_recapture$Stage, varwidth = TRUE,
        ylab = "Diameter", xlab = "Age Stage", col='grey', main = "")




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

# Certaines imprecision nous limitent dans notre comprehension des donnees par exemple.
# Nous ne pouvons pas determiner si les quadrats (au sein d'un meme site) ont
# des ressources differentes ou non.
# Le jeu de donnees ne nous permet pas de prendre en compte si les escargots
# bougent ou pas car nous n'avons leur quadrat que pour leur premiere capture.
# Nous ne savons pas non plus s'il est possible pour les escargots de parcourir
# les 500m qui separent les sites. 


# Il faut noter que cette etude ne porte que sur la difference de traits
# morphologique et demographique entre deux zones, du fait de l'abscence de
# replicats (il n'y a que des pseudo-repliquats), il est donc impossible de
# generaliser les conclusions de cette etude.
# De plus, rien n'indique que ces sites sont equivalents en termes de ressources
# alimentaires, qualite d'habitats.
# Pour reellement etudier l'effet de la densite de population sur les
# caracteristiques morphologiques il faudrait realiser une etude demographique
# et utiliser la densite de population en varaible explicative, mais aussi
# prendre en compte les possibles differences dans les caracteristiques
# (biotiques et abiotiques) des milieux (type de substrat, conditions de vie,
# vitesse de deplacement...).


# Pour approfondir les analyses, il faudrait plus de details sur la methode
# d'echantillonnage, et un partitionnement plus fin de la classe juvenile
# aurait permis une variance plus faible dans cette categorie.  
# Nous avons fait l'hypothese que les escargots n'ont pas migre d'un site a
# l'autre durant toute la duree de l'echantillonnage.

# L'estimation de la demographie aurait pu nous aiguiller sur l'hypothese que les
# escargots du nouveau sites n'ont pas encore atteint la capacite limite du milieu. 



# CONCLUSION -------------------------------------------------------------------

# Dans les modeles testes, le diametre et le peristome sont en premier lieu 
# affecte par le stade de developpement ce qui est coherent avec l'interpretation
# biologique: la croissance impacte la taille. 

# Le site a egalement un effet important : les escargots adultes sont plus
# gros sur le nouveau site, avec une faible densite de population.
# Cela peut etre lie a differents facteurs : la plus grande quantite de
# ressources disponibles (liees a la densite de population), le type de milieu
# (substrat, exposition aux predateurs)...
