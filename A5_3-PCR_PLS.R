#### Examen synthèse Capstone
### A5.3 - Modèles linéaires étendus
### Régression avec ACP (PCR) et moindres carrés partiels (PLS)
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps",
                     "glmnet", "FactoMineR", "pls")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

library(CASdatasets)

# Importation des données ----

data <- Boston

#Auto <- read.table("Auto.data", header = T, na.strings = "?",
                   #stringsAsFactors = T)

# Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors =
#                      T)

# dim(Auto)
# names(Auto)

## Aperçu jeu de données ----

head(data)
summary(data)
colnames(data)
dim(data)
str(data)

## Train-Test ----
# ici 70-30
set.seed(199000)
test.indices <- sample(1:nrow(data),0.3*nrow(data))
train <- data[-test.indices,]
test <- data[test.indices,]


## model.matrix ----
test_x <- test[, -13] # sans la variable réponse
test_y <- test[, 13] # seulement la variable réponse


# ACP classique  ----

acp <- FactoMineR::PCA(train[, c(-4, -13)],
                       scale = TRUE, ncp = 12) # GARDER JUSTE VARIABLES EXOGÈNES NUMÉRIQUES

acp$eig
ndim <- 7 # pour garder une proportion raisonnable de la variance expliquée

# PCR manuel ----
# Restructurer le jeu de données
train.pca <- data.frame(cbind(train$medv, acp$ind$coord[, 1:ndim])) # garder réponse et CP
colnames(train.pca) <- c("medv", paste0("dim", 1:ndim))

# Entraîner le modèle linéaire avec composantes principales
mod <- lm(medv~., data=train.pca)
summary(mod)

# Predict
## ATTENTION DE BIEN TRANSFORMER LES DONNÉES DE TEST SELON L'ACP
## APPRISE PLUS TÔT SUR LES DONNÉES D'ENTRAÎNEMENT

# Préparer les données de test
pca.test <- predict(acp, newdata=test_x[, -4])
pca.test <- data.frame(pca.test$coord[, 1:ndim])
colnames(pca.test) <- paste0("dim", 1:ndim)

# Effectuer les prévisions
pred_pcr1 <- predict(mod, newdata=pca.test)
(mse_pcr1 <- mean((pred_pcr1 - test_y)^2))


# PCR auto ----
# pkg pls
pcr.fit <- pls::pcr(medv~., data = train[, -4], scale = TRUE, validation='LOO')
summary(pcr.fit)

# utilisation de la validation croisée aussi possible
pcr.cv <- pls::pcr(medv ~ ., data = train[, -4], scale = TRUE, validation = "CV",
                   segments = 10) # segments == nombre de plis de la CV, 10 par défaut

summary(pcr.cv)
pls::validationplot(pcr.cv , val.type = "RMSEP")


# Predict (donnera résultats équivalents au manuel)
pred_pcr2 <- predict(pcr.fit, test_x[, -4], ncomp = 7)
(mse_pcr2 <- mean((pred_pcr2 - test_y)^2))


## Choix du nombre de composantes ----
### on peut utiliser la proportion de variance expliquée

### ou, de façon plus pertinente, utiliser les résidus PRESS avec LOO
press <- pcr.fit$validation$PRESS[1, ]
plot(1:11, press, type='b',
     xlab='Nombre de composantes principales',
     ylab='Résidus PRESS')
pls::validationplot(pcr.fit , val.type = "RMSEP")

pred_pcr3 <- predict(pcr.fit, test_x[, -4], ncomp = 4)
(mse_pcr3 <- mean((pred_pcr3 - test_y)^2)) # meilleur!

# PLS ----
pls.fit <- plsr(medv ~ ., data = data,  # jeu de données complet
                subset = which(1:nrow(data) %in% test.indices), # données d'entraînement
                scale = TRUE, validation = "CV")

summary(pls.fit)
validationplot(pls.fit , val.type = "MSEP")
pls.pred <- predict(pls.fit , test, ncomp = 1)
mean((pls.pred - test_y)^2)

# sur tout le jeu de données
pls.fit <- plsr(medv ~ . , data = data, scale = TRUE,
                ncomp = 1)
summary(pls.fit) # une composante, c'est assez fort car on essaie d'expliquer la
                # variance dans les prédicteurs et la variable réponse alors que
                # le PCR ne le cherche que pour les prédicteurs.
