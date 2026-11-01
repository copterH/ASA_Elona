
# IMPORTATION DES LIBRARIES ----
install.packages("readxl")
install.packages("lme4")
library(lme4)
library(readxl)
library(GGally)
library(ggcorrplot)

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
summary(elona_recapture$Peristome)


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


m1<-lmer(Diameter ~ Site + (1|Stage)+(1|Year)+(1|Stage)+ (1|ID_1erC),data=elona_recapture)

# CONDITIONS DE VALIDITE ----

## 1. Normalité des résidus ----
plot(m1) # résidus ok

## 2. Homoscédasticité ----
plot(x = elona_recapture$Date, y = elona_recapture$Diameter )
plot(x = elona_recapture$Date, y = elona_recapture$Peristome )

## 3. Résidus indépendants ----


# ANALYSE ----
# Choix de la méthode pour répondre à la question :
# We aim to determine whether the two populations share the same demographic characteristics and traits.

## 1. Diameter ----
# Modèle linéaire général mixte
# Diameter = Site + Stage + (ID + date)
m_diam<-lmer(Diameter ~ Site + Stage + (1|ID_1erC) + (1|Date), data=elona_recapture)

plot(m_diam)

## 2. Peristome ----


## 3. Démographie ----

