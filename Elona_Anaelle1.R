#Packages ------
# install.packages("readxl")
# install.packages("lme4")
library(lme4)
library(readxl)
library(car) #pour ncvTest (homoscedasticite) et colinearite (vif)
library(dplyr)#pour select
library(ade4)#acp
library(corrplot)
library(MASS)
#import dataset First capture & recapture -----
elona_recapture <- read.csv2("recapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)
elona_firstcapture <- read.csv2("firstcapture.csv", header = TRUE, sep = ";", dec = ",", stringsAsFactors = TRUE)

## Importer la feuille 2 pour rajouter les sites -----

elona_recapture$Date <- as.Date(paste(as.character(elona_recapture$Year),as.character(elona_recapture$Month),as.character(elona_recapture$Day), sep = "-"))
elona_firstcapture$Date <- as.Date(paste(as.character(elona_firstcapture$Year),as.character(elona_firstcapture$Month),as.character(elona_firstcapture$Day), sep = "-"))


# valeur abberante dans Y
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



# Relation potentielle entre X et Y -----
boxplot(elona_recapture$Diameter~elona_recapture$Site)
boxplot(elona_recapture$Peristome~elona_recapture$Site)
# Recent a l'air plus haut

# Dans le cours ils disent qu'il faut aussi analyser les interactions éventuelles entre les X mais là vu qu'on a des effets fixes et un effet aléaoire je sais pas si ça compte

# Il faut aussi vérifier une éventuelle colinéarité entre les X, mais pareil je sais pas si ça vaut le coup


#creation du modele mixte du diametre avec le site en modele fixe, et les etapes,annees,  n° d'identification en numero aleatoire ------
m1<-lmer(Diameter ~ Site + (1|Stage)+(1|Year)+(1|Stage)+ (1|ID_1erC),data=elona_recapture)
summary(m1)
## Conditions de validité  -----
plot(m1) # résidus ok
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

## Coefficients de correlation entre les X -----
cor(elona_firstcapture[, c(2,3,6,7, 8)])#pas de correlation forte entre les variables numeriques (NA avec peristome ??)
cor(elona_recapture[, c(2,3, 7,8, 9)])#pas de correlation forte entre les variables numeriques

###COLINEARITE ??? -----
# Checking collinearity between continuous independent variables
# We represent plot for each X continuous covariate pair.
plot(elona_recapture[,c(2,3,9)],pch=16,col='dodgerblue3')
#We calculate correlation between each pair of X covariate
Mbis<-cor(elona_recapture[,2:3])
corrplot.mixed(Mbis,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8)##erreur de coordonnees des symboles

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

##Colinearite entre les variables X num ou facteur -----
vif(m1) #erreur : model contains fewer than 2 terms
vif(elona_recapture_cor [,2:7])

summary(elona_recapture_cor)

mod_test <-     lm (Diameter ~ Site + Peristome+ Stage+Type_Event, data = elona_recapture_cor)
mod_test_tot <- lm (Diameter ~ Site + Peristome+ Stage+Type_Event + Event + Day + ID_1erC , data = elona_recapture_cor)#modele lineaire simple avec toutes les variables qui ne sont pas des "aliased coeff" pour faire fonctionner la fonction vif{car}

#####vif{car} ----
#regles lecture fiv coeff 
# VIF = 1 : il n’existe aucune corrélation entre une variable prédictive donnée et toute autre variable prédictive du modèle.
# VIF entre 1 et 5 : il existe une corrélation modérée entre une variable prédictive donnée et d’autres variables prédictives du modèle.
# VIF > 5 : il existe une forte corrélation entre une variable prédictive donnée et d’autres variables prédictives dans le modèle.
vif(mod_test_tot) #erreur :   there are aliased coefficients in the model
# GVIF = generalized Variation Inflation Factor | GVIFj =(1/ (1−Rj^2 )^d)
# GVIF plutot que VIF utilisé quand le modèle comprend des variables catégorielles à plusieurs niveaux.
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

##matrice de correlation -----

# Checking collinearity between continuous independent variables

# We represent plot for each X continuous covariate pairs
#We calculate correlation between each pair of X covariate
corrplot.mixed(M1C,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8, main = "First capture")
corrplot.mixed(MRC,upper="square",lower.col="black", tl.col="black",cl.cex = 0.8,tl.cex = 0.7,number.cex =0.8, main = " REcapture")

# Checking collinearity between categorical and continuous independent variables
par(mfrow=c(1,3))
##Boxplot Dimension selon var explicatives -----
###Year ----
#Diameter and year (recapture)
boxplot(elona_recapture$Diameter~elona_recapture$Year, varwidth = TRUE, ylab = "Diameter", xlab = "Year",col=c('#000099','#339900'), ylim = c(0,25), main = "Recapture ")
#Diameter and year (First capture)
boxplot(elona_firstcapture$Diameter~elona_firstcapture$Year, varwidth = TRUE, ylab = "Diameter", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,25), main = "First Capture")

#peristome and year (recapture)
boxplot(elona_recapture$Peristome~elona_recapture$Year, varwidth = TRUE, ylab = "Peristome", xlab = "Year",col=c('#000099','#339900'), ylim = c(0,12), main = "Recapture ")
#peristome and year (First capture)
boxplot(elona_firstcapture$Peristome~elona_firstcapture$Year, varwidth = TRUE, ylab = "Peristome", xlab = "Year",col=c('royalblue','chartreuse'), ylim = c(0,12), main = "First Capture")
### Event----
#Diameter and Event (recapture)
boxplot(elona_recapture$Diameter~elona_recapture$Event, varwidth = TRUE, ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")
#Diameter and Event (First capture)
boxplot(elona_firstcapture$Diameter~elona_firstcapture$Event, varwidth = TRUE, ylab = "Diameter", xlab = "Event",col=c(blues9), ylim = c(0,25), main = "First Capture")

## ID_1erC -----
boxplot(elona_recapture$Diameter~elona_recapture$ID_1erC, varwidth = TRUE, ylab = "Diameter", xlab = "ID_1erC",col=c(blues9), ylim = c(0,25), main = "Recapture ")
##TypeEvent -----
boxplot(elona_recapture$Diameter~elona_recapture$Type_Event, varwidth = TRUE, ylab = "Diameter", xlab = "Type_Event",col=c(blues9), ylim = c(0,25), main = "Recapture ")


sum(is.na(elona_recapture_cor)) #6 NA 
which(is.na(elona_recapture_cor)) #6 NA 
#QUESTION : What is the impact of population density on biometry and individual growth in Elona quimperianal ? ------
#GLMM 
# un glm, avec en réponse diamètre et peristome, en variables classiques le site (old ou new), stage et act (?) et en variables aléatoires l'ID, l'event et la date ?


mod_tot<-lmer(Diameter ~ Site + Stage + Activity + (1|Year)+ (1|ID_1erC) + (1|Type_Event) + (1|Event) + (1| Date),data=elona_recapture)
anova(mod_tot)
summary(mod_tot)
#Stade juvenile est le facteur avec un effet le plus fort
#les correlationsd'effet fixes sont toutes <0.2
vif(mod_tot) #GVIF = 1 --> colinearite moderee
vif.cca(mod_tot)
plot(mod_tot)#residus ok ?
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


#selection modele en retirant des var : methode descendante stepAIC {MASS}
M0 <- lmer (Diameter ~1 + (1|Date), data = elona_recapture)
stepAIC (M0, mod_tot, direction = "backward")



###PLOT OBS -----
summary(elona_recapture)
head(elona_recapture)
table(elona_recapture$Diameter)
table(elona_recapture$Peristome)
subset(elona_recapture, elona_recapture$Peristome>19)

nrow(table(elona_recapture$Date))
table (elona_recapture$Site)
table(elona_recapture$Stage)


##