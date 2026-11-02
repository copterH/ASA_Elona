
install.packages("lme4")
library(lme4)
library(readxl)
library(sf) # SF is a library that allows reprensentation for coordinates
library(dplyr)
library(ggplot2)
library(mapview)


elona_recapture <- read.csv2("recapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)
elona_firstcapture <- read.csv2("firstcapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)

# Importer la feuille 2 pour rajouter les sites

elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),as.character(elona_recapture$Month),as.character(elona_recapture$Day), sep = "-"))
elona_firstcapture$Date <- as.Date(paste(as.character(elona_firstcapture$Year),as.character(elona_firstcapture$Month),as.character(elona_firstcapture$Day), sep = "-"))


# valeur abérante dans Y
boxplot(elona_recapture$Diameter)
summary(elona_recapture$Diameter)
# On a des valeurs très faibles mais ça me parait pas improbable

boxplot(elona_recapture$Peristome) # Une valeur très très haute => on va l'enlever ça parait improbable
summary(elona_recapture$Peristome)
elona_recapture$Peristome[which.max(elona_recapture$Peristome)] <- NA
boxplot(elona_recapture$Peristome) # On en a encore 3 grandes, je sais pas trop si c'est probable ou pas
summary(elona_recapture$Peristome)


hist(elona_recapture$Diameter)
hist(elona_recapture$Peristome,breaks=1000) # pas du tout normal, ressemble un peu à une loi de poisson (non)
summary(elona_recapture$Peristome)


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

table(elona_firstcapture$Stage)
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="?")] <- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV?")]<- "JUV"
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV/SUB")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="SUB/AD")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="")]<- NA
elona_firstcapture$Stage[which(elona_firstcapture$Stage=="JUV ")]<- "JUV"
table(elona_firstcapture$Stage)

elona_firstcapture$X[431] <- NA
elona_firstcapture$Y[431] <- NA

# Relation potentielle entre X et Y
boxplot(elona_recapture$Diameter~elona_recapture$Site)
boxplot(elona_recapture$Peristome~elona_recapture$Site)
# Recent a l'air plus haut

# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte
# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup

m1<-lmer(Diameter ~ Site + (1|Stage)+(1|Year)+(1|Stage)+ (1|ID_1erC),data=elona_recapture)

# Conditions de validité
plot(m1) # résidus ok
# Homoscédasticité








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
