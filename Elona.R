
# IMPORTATION DES LIBRARIES ----
install.packages("readxl")
install.packages("lme4")
library(lme4)
library(readxl)
library(GGally)
library(ggcorrplot)
library(dplyr)
library(ggplot2)
library(tidyverse)


# IMPORTATION DES DONNEES ----
# Importation et gestion des classes
elona_recapture <- read_xlsx("data_elona.xlsx", sheet = 3, col_types = c('text', 'numeric', 'numeric', 'text', 'text', 'text', 'numeric', 'numeric', 'numeric', 'text', 'text'))
elona_recapture$Site <- as.factor(elona_recapture$Site)
elona_recapture$Stage <- as.factor(elona_recapture$Stage)
elona_recapture$Activity <- as.factor(elona_recapture$Activity)
elona_recapture$Type_Event <- as.factor(elona_recapture$Type_Event)
elona_recapture$Event <- as.factor(elona_recapture$Event)
elona_recapture$ID_1erC <- as.factor(elona_recapture$ID_1erC)
elona_recapture <- data.frame(elona_recapture$Site, elona_recapture$Diameter, elona_recapture$Peristome, elona_recapture$Stage, elona_recapture$Activity, elona_recapture$Type_Event, elona_recapture$Day, elona_recapture$Month, elona_recapture$Year, elona_recapture$Event, elona_recapture$ID_1erC)
colnames(elona_recapture) <- c('Site', 'Diameter', 'Peristome', 'Stage', 'Type_Event',"Activity", "Day", "Month", "Year", "Event", "ID_1erC")


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
summary(elona_recapture$Peristome)

#Normalité de Y
hist(elona_recapture$Diameter)
hist(elona_recapture$Peristome,breaks=1000) # pas du tout normal, ressemble un peu à une loi de poisson (non)
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
elona_recapture$Stage = droplevels(elona_recapture$Stage)
table(elona_recapture$Stage)

table(elona_recapture$Activity)
elona_recapture$Activity[which(elona_recapture$Activity=="ACT ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT  ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="ACT   ")]<- "ACT"
elona_recapture$Activity[which(elona_recapture$Activity=="IMM")]<- "IMMO"
elona_recapture$Activity[which(elona_recapture$Activity=="IMMO ")]<- "IMMO"
elona_recapture$Activity[which(elona_recapture$Activity=="MORT ")]<- "MORT"
table(elona_recapture$Activity)
elona_recapture$Activity = droplevels(elona_recapture$Activity)
table(elona_recapture$Activity)

table(elona_recapture$Type_Event)
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R ")]<- "R"
elona_recapture$Type_Event[which(elona_recapture$Type_Event=="R  ")]<- "R"
table(elona_recapture$Type_Event)
elona_recapture$Type_Event = droplevels(elona_recapture$Type_Event)
table(elona_recapture$Type_Event)



table(elona_firstcapture$Stage)
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="?")] <- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV?")]<- "JUV"
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV/SUB")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="SUB/AD")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV ")]<- "JUV"
table(elona_firstcapture$Stage)


# Relation entre les Ys (Diameter & Peristome)
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

# Relation potentielle entre X et Y
boxplot(elona_recapture$Diameter~elona_recapture$Site)
boxplot(elona_recapture$Peristome~elona_recapture$Site)
# Recent a l'air plus haut

## 3. Analyse des interactions ----
# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte

ggpairs(elona_recapture, columns = c("Site", "Stage", "Activity"),
        aes(color = Diameter, alpha = 0.5))

# Interactions entre Stage et Activity
plot(elona_recapture$Stage~elona_recapture$Activity, ylab = "Demographic stages",xlab="Activity")

# Interactions entre Stage & Type_Event
plot(elona_recapture$Stage~elona_recapture$Type_Event, ylab = "Demographic stages",xlab="Type Event")

# Interactions entre Activity & Type_Event
plot(elona_recapture$Activity~elona_recapture$Type_Event, ylab = "Activity",xlab="Type_Event")


## 4. Analyse des colinéarités ----
# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup

# Seules les variables numériques peuvent être prises en compte dans la matrice de corrélation
# --> pas d'utilité je pense
cor_mat <- cor(elona_recapture[c(elona_recapture$Diameter, elona_recapture$Peristome,
                                   elona_recapture$Date)])
ggcorrplot(cor_mat, method = "square", lab = TRUE)


m1<-lmer(Diameter ~ Site + (1|Year) + (1|Stage) + (1|ID_1erC), data=elona_recapture)

# CONDITIONS DE VALIDITE ----

## 1. Normalité des résidus ----
plot(m1) # résidus ok

# Homoscédasticité







## 2. Homoscédasticité ----
plot(x = elona_recapture$Date, y = elona_recapture$Diameter )
plot(x = elona_recapture$Date, y = elona_recapture$Peristome )

## 3. Résidus indépendants ----


# ANALYSE ----
# Choix de la méthode pour répondre à la question :
# We aim to determine whether the two populations share the same demographic characteristics and traits.

## 1. Diameter ----
# Modèle linéaire général mixte
# Diameter = Site + Stage + (ID_1erC + Date)
m_diam<-lmer(Diameter ~ Site + Stage + (1|ID_1erC) + (1|Date), data=elona_recapture)

plot(m_diam)

## 2. Peristome ----


## 3. Démographie ----

###a. Select the candidate model ----
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

ggplot(data = elona_dem)+
  geom_col(aes(Event_Site, AD, fill = Site))

  
  

###b. Analyze coefficient estimates & residual deviance ----

###c. Model diagnostic ----
# Checking distribution of residuals (histogramm/Qqplot)
# Distribution of residuals depending on fitted values/ Y values
# Cook’s distance for influential observations
# Fitted values against observed ones (predictive value of model )






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

