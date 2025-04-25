#### Examen synthèse Capstone
### A5.8 - K plus proches voisins
###

# Importation des paquetages ----
liste.paquetage <- c("tidyverse", "FactoMineR", "ggplot2", "plotly",
                     "ISLR", "caret", "FNN", "glmnet")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Importation données ----
data <- iris
summary(iris)

# Train-test split ----
proportion_entrainement <- 0.8

set.seed(20240305)

taille_ech <- floor(proportion_entrainement * nrow(data))
idx_entrainement <- sample(1:nrow(data), size = taille_ech)

obs_entrainement <- data[idx_entrainement, -5]
obs_test <- data[-idx_entrainement, -5]

etiq_entrainement <- data[idx_entrainement, 5]
etiq_test <- data[-idx_entrainement, 5]

# K plus proches voisins (classification) ----
## Exemple Iris ----
k <-  3
processor <- preProcess(obs_entrainement, method = "scale")
train_proc <- predict(processor, obs_entrainement)
test_proc <- predict(processor, obs_test)

predictions <- FNN::knn(train_proc, test_proc, etiq_entrainement, k, prob = FALSE)
table(predictions, etiq_test)

## Exemple caravan ----
data <- Caravan

caravan_variables <- Caravan[, -86]
caravan_resp <- Caravan[, 86]

taille_ech <- floor(0.75 * nrow(caravan_variables))
idx_entrainement <- sample(seq_len(nrow(caravan_variables)), size = taille_ech)

obs_entrainement <- caravan_variables[idx_entrainement, ]
obs_test <- caravan_variables[-idx_entrainement, ]

etiq_entrainement <- factor(caravan_resp[idx_entrainement])
etiq_test <- factor(caravan_resp[-idx_entrainement])

processor <- preProcess(obs_entrainement, method = "scale")
train_proc <- predict(processor, obs_entrainement)
test_proc <- predict(processor, obs_test)

predictions <- knn(train_proc, test_proc, etiq_entrainement, k = 5, prob = FALSE)
table(predictions, etiq_test)

# K plus proches voisins (régression) ----
etiq_entrainement_reg <- ifelse(etiq_entrainement == "Yes", 1, 0)
etiq_test_reg <- ifelse(etiq_test == "Yes", 1, 0)

modele_knn_reg <- knn.reg(train_proc, test_proc, etiq_entrainement_reg, k = 5)

pred_binaire <- ifelse(modele_knn_reg$pred > 0.5, 1, 0)
table(etiq_test_reg, pred_binaire)

table_roc <- function(etiq, pred) {
    etiq <- etiq[order(pred, decreasing = TRUE)]
    data.frame(TVP = cumsum(etiq)/sum(etiq), # Taux vrais positifs
               TFP = cumsum(!etiq)/sum(!etiq)) # Taux faux positifs
}

plot(table_roc(etiq_test_reg, pred_binaire))

# Choix de K avec caret ----
## iris ----
controles <- trainControl(method  = "cv", # validation croisée
                          number  = 5) # 5 plis

fit <- caret::train(Species ~ .,
                    method     = "knn",
                    preProcess = "scale",
                    tuneGrid   = expand.grid(k = 2:8),
                    trControl  = controles,
                    metric     = "Accuracy",
                    data       = iris)
fit

## caravan ----
trControl <- trainControl(method  = "cv", # validation croisée
                          summaryFunction = twoClassSummary,
                          classProbs = TRUE,
                          number  = 4)

Caravan_train <- Caravan[idx_entrainement, ]
Caravan_test <- Caravan[-idx_entrainement, ]

fit <- caret::train(Purchase ~ .,
                    method     = "knn",
                    preProcess = "scale",
                    tuneGrid   = expand.grid(k = 1:10),
                    trControl  = trControl,
                    metric     = "ROC",
                    data       = Caravan_train)
# On devrait utiliser le jeu d'entrainement seulement ici pour trouver le k optimal

fit
preds_test <- predict(fit, Caravan_test)
table(preds_test, Caravan_test$Purchase)
