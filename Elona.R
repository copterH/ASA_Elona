
install.packages("readxl")
install.packages("lme4")
library(lme4)
library(readxl)


elona <-read_excel("/Users/emiliecordier/Downloads/Data ASA/data_elona.xlsx",sheet=3)
elona$Diameter<-as.numeric(elona$Diameter)
elona$Peristome<-as.numeric(elona$Peristome)

# Importer la feuille 2 pour rajouter les sites
# voir pour faire fusionner la jour le mois et l'année pour mettre une date et mettre ça directement dans R
# ça sera peut-être plus informatif que juste le jour, le mois et l'année séparemment
# ou au moins le jour et le mois pour avoir la période de l'année et on met un autre effet aléatoire pour l'année

# valeur abérante dans Y
boxplot(elona$Diameter)
summary(elona$Diameter)
# On a des valeurs très faibles mais ça me parait pas improbable

boxplot(elona$Peristome) # Une valeur très très haute => on va l'enlever ça parait improbable
summary(elona$Peristome)
elona$Peristome[which.max(elona$Peristome)] <- NA
boxplot(elona$Peristome) # On en a encore 3 grandes, je sais pas trop si c'est probable ou pas
summary(elona$Peristome)


hist(elona$Diameter)
hist(elona$Peristome,breaks=1000) # pas du tout normal, ressemble un peu à une loi de poisson
summary(elona$Peristome)


table(elona$Site)
table(elona$Stage)# Nombre de modalités dans "Stage" va pas
# On a des catégories (?, JUV? JUV/SUB, SUB/AD) où on a très peu d'individus, passer en NA ou dans une autre classe
elona$Stage[which(elona$Stage=="?")]<- NA
elona$Stage[which(elona$Stage=="JUV?")]<- "JUV"
# Pour les deux autres on peut les passer en NA ou alors trancher mais on a aucunes données pour trancher
elona$Stage[which(elona$Stage=="JUV/SUB")]<- NA
elona$Stage[which(elona$Stage=="SUB/AD")]<- NA

# Relation potentielle entre X et Y
boxplot(elona$Diameter~elona$Site)
boxplot(elona$Peristome~elona$Site)
# Recent a l'air plus haut

# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte
# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup

m1<-lmer(Diameter ~ Site + (1|Stage)+(1|Year)+(1|Stage)+ (1|ID_1erC),data=elona)

# Conditions de validité
plot(m1) # résidus ok
# Homoscédasticité
