#### Examen synthèse Capstone
### A5.8 - Arbres
###

# Installation de distRforest la première fois seulement ----
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
  devtools::install_github("henckr/distRforest")
}

# Métriques utiles ----
dev_gamma = function(obs, pred){
  # Note : c'est la moyenne des déviances gamma pour chacune des observations
  # obs : vraies valeurs de la variable réponse
  # pred : prévisions du modèle

  2 * sum(-log(obs/pred) + (obs - pred)/pred) / length(obs)

}

dev_poisson <- function(obs, pred){
    # Note : c'est la moyenne des déviances Poisson pour chacune des observations
    # obs : vraies valeurs de la variable réponse
    # pred : prévisions du modèle (DOIVENT TENIR COMPTE DE L'EXPOSITION)
    dev_one <- function(pred, obs)
    {
        ifelse(obs == 0, 2 * sum(- obs + (pred)),
               2*sum(obs * log(obs / (pred)) - (obs - (pred))))
    }

    mean(sapply(1:length(pred), function(i) dev_one(pred[i], obs[i])))
}


# À utiliser dans caret via l'argument summaryFunction
gamma_loss = function(data, lev = NULL, model = NULL){

  dev = -2 * sum(-log(data$obs/data$pred) + (data$obs - data$pred)/data$pred)
  names(dev) = 'gamma_loss'
  dev

}

poisson_loss <- function(data, lev = NULL, model = NULL){
    # Note : c'est la moyenne des déviances Poisson pour chacune des observations
    # obs : vraies valeurs de la variable réponse
    # pred : prévisions du modèle (DOIVENT TENIR COMPTE DE L'EXPOSITION)
    dev_one <- function(pred, obs)
    {
        ifelse(data$obs == 0, 2 * sum(- data$obs + (data$pred)),
               2*sum(data$obs * log(data$obs / (data$pred)) - (data$obs - (data$pred))))
    }

    dev <- -2 * sum(sapply(1:nrow(data$pred),
                           function(i) dev_one(data$pred[i], data$obs[i])))
    names(dev) <- 'poisson_loss'
    dev
}

# Importation des paquetages ----
liste.paquetage <- c("caret",
                     "rpart",
                     "pROC",
                     "dplyr",
                     "distRforest",
                     "CASdatasets",
                     "rpart.plot")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Classification ----
raw_data <- read.csv("data/HomeCredit_simplifie.csv", stringsAsFactors = TRUE)
str(raw_data)

## feature engineering (si applicable) ----
raw_data$TARGET <- factor(raw_data$TARGET, levels = levels(raw_data$TARGET)[2:1])
raw_data$PERC_ANNUITY_INCOME <- raw_data$AMT_ANNUITY / raw_data$AMT_INCOME_TOTAL
raw_data$PERC_ANNUITY_GOODS_PRICE <- raw_data$AMT_ANNUITY / raw_data$AMT_GOODS_PRICE
raw_data$PERC_FINANCING <- raw_data$AMT_CREDIT / raw_data$AMT_GOODS_PRICE
raw_data$PERC_DEBTS <- raw_data$AMT_CREDIT / raw_data$AMT_INCOME_TOTAL
raw_data$PAYMENT_SPEED <- raw_data$AMT_ANNUITY / raw_data$AMT_CREDIT

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(raw_data))
train_ind <- sample(1:nrow(raw_data), size = smp_size, replace=FALSE)

train_data <- raw_data[train_ind, ]
test_data <- raw_data[-train_ind, ]

## entrainement ----
### avec avec le paramètre de complexité ----
tree.control <- rpart::rpart.control(cp=0.0,
                                     minsplit=1,
                                     minbucket=1) # overfit l'arbre au maximum

arbre <- rpart::rpart(TARGET~.,
                      data=train_data,
                      method="class",
                      control=tree.control)

plotcp(arbre)
cp_optim = arbre$cptable[which.min(arbre$cptable[,4]), 1] # meilleur cp
arbre_elag = rpart::prune(arbre, cp = cp_optim) # élagage
rpart.plot(arbre_elag, fallen.leaves = T, type = 5)

pred = predict(arbre_elag,
               newdata = test_data %>% dplyr::select(-TARGET),
               type = 'class')

summary(pred)

### avec caret----
# Validation croisée ordinaire avec : method = "cv" et number = nb_plis

#### recherche en grille ----
fit.control_holdout <- trainControl(method = "LGOCV",
                                    p = 0.8, # 20 % en validation
                                    number = 1, # holdout
                                    summaryFunction = twoClassSummary,
                                    classProbs=TRUE,
                                    search="grid")

cv.fitted.tree_holdout <- caret::train(TARGET~.,
                                       data=train_data,
                                       method="rpart",
                                       tuneLength=10,
                                       metric="ROC",
                                       trControl=fit.control_holdout)

cv.fitted.tree_holdout
cv.fitted.tree_holdout$bestTune

#### valeurs spécifiées ----
cp_grid <- expand.grid(cp = c(0.005, 0.007, 0.01))

fit.control_holdout <- trainControl(method = "LGOCV",
                                    p = 0.8,
                                    number = 1,
                                    summaryFunction = twoClassSummary,
                                    classProbs=TRUE)

cv.fitted.tree_holdout <- caret::train(TARGET~.,
                                       data=train_data,
                                       method="rpart",
                                       tuneGrid=cp_grid,
                                       metric="ROC",
                                       trControl=fit.control_holdout)

cv.fitted.tree_holdout
cv.fitted.tree_holdout$bestTune

## évalutation du modèle ----
tree_predictions_test <- predict(cv.fitted.tree_holdout, test_data, type="raw")
tree_confusion_matrix_test <- confusionMatrix(tree_predictions_test, test_data$TARGET, positive="DEFAULT")
tree_confusion_matrix_test

arbre.prev <- predict(cv.fitted.tree_holdout, newdata=test_data, type="prob")
arbre.res <- roc(test_data$TARGET, arbre.prev[,2], auc=TRUE, plot=TRUE)
arbre.res$auc

# Régression (anova) ----
data("pg15training")
data = pg15training # copie dataset
str(data)

##  feature engineering (si applicable) ----
data_sev = data %>%
  dplyr::filter(Indtppd > 0) %>% # filtre sur la condition d'avoir au moins une réclamation pour des dommages matériels
  dplyr::mutate(Severity = Indtppd/Numtppd) %>% # crée la variable de sévérité
  dplyr::select(!c(PolNum, Exppdays, Numtpbi, Numtppd, Indtpbi, Indtppd,
                   SubGroup2)) # retrait des variables sans intérêt

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(data_sev))
train_ind <- sample(1:nrow(data_sev), size = smp_size, replace=FALSE)

train_data <- data_sev[train_ind, ]
test_data <- data_sev[-train_ind, ]

## entrainement ----
### avec avec le paramètre de complexité ----
tree.control <- rpart::rpart.control(cp=0.0,
                                     minsplit=1,
                                     minbucket=1) # overfit l'arbre au maximum

arbre <- rpart::rpart(Severity~.,
                      data=train_data,
                      method = "anova",
                      control=tree.control)

plotcp(arbre)
cp_optim = arbre$cptable[which.min(arbre$cptable[,4]), 1] # meilleur cp
arbre_elag = rpart::prune(arbre, cp = cp_optim) # élagage
rpart.plot(arbre_elag, fallen.leaves = T, type = 5)

pred = predict(arbre_elag,
               newdata = test_data %>% dplyr::select(-Severity),
               type = 'vector')

summary(pred)

### avec caret----
# Validation croisée ordinaire avec : method = "cv" et number = nb_plis

#### recherche en grille ----
fit.control_holdout <- trainControl(method = "LGOCV",
                                    p = 0.8, # 20 % en validation
                                    number = 1, # holdout
                                    search="grid")

cv.fitted.tree_holdout <- caret::train(Severity~.,
                                       data=train_data,
                                       method="rpart",
                                       tuneLength=10,
                                       metric="RMSE",
                                       trControl=fit.control_holdout)

cv.fitted.tree_holdout
cv.fitted.tree_holdout$bestTune

#### valeurs spécifiées ----
cp_grid <- expand.grid(cp = c(0.005, 0.007, 0.01))

fit.control_holdout <- trainControl(method = "LGOCV",
                                    p = 0.8,
                                    number = 1)

cv.fitted.tree_holdout <- caret::train(Severity~.,
                                       data=train_data,
                                       method="rpart",
                                       tuneGrid=cp_grid,
                                       metric="RMSE",
                                       trControl=fit.control_holdout)

cv.fitted.tree_holdout
cv.fitted.tree_holdout$bestTune

## évalutation du modèle ----
tree_predictions_test <- predict(cv.fitted.tree_holdout, test_data, type="raw")
summary(tree_predictions_test)
plot(cv.fitted.tree_holdout$finalModel)

# calcul de RMSE
RMSE_arbre = sqrt(mean((tree_predictions_test - test_data$Severity)^2))

# Régression (autres distributions) ----
# Distributions : "anova", "poisson", "class", "exp", "gamma" ou "lognormal".
data("pg15training")
data = pg15training # copie dataset
str(data)

##  feature engineering (si applicable) ----
data_sev = data %>%
  dplyr::filter(Indtppd > 0) %>% # filtre sur la condition d'avoir au moins une réclamation pour des dommages matériels
  dplyr::mutate(Severity = Indtppd/Numtppd) %>% # crée la variable de sévérité
  dplyr::select(!c(PolNum, Exppdays, Numtpbi, Numtppd, Indtpbi, Indtppd,
                   SubGroup2)) # retrait des variables sans intérêt

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(data_sev))
train_ind <- sample(1:nrow(data_sev), size = smp_size, replace=FALSE)

train_data <- data_sev[train_ind, ]
test_data <- data_sev[-train_ind, ]

## entrainement ----
### avec caret ----
#### valeurs spécifiées ----
controles = distRforest::rpart.control(cp = 0,
                                       # maxsurrogate = 0, # juste pour que ça run plus vite (aucune séparation de remplacement, pcq aucun NA)
                                       xval = 0)

set.seed(21)

arbre = distRforest::rpart(Severity~.,
                           method = 'gamma',
                           data = train_data,
                           control = controles)

controles = trainControl(method = 'cv',
                         number = 4,
                         summaryFunction = gamma_loss)

set.seed(10)

fit = caret::train(Severity~.,
                   method = 'rpart',
                   tuneGrid = expand.grid(cp = seq(0, 0.03, by = 0.001)),
                   trControl = controles,
                   metric = 'gamma_loss', # cp optimal selon déviance gamma
                   data = train_data)

cp_optim_gamma = unlist(fit$bestTune)

arbre_elag_gamma = rpart::prune(arbre, cp = cp_optim_gamma)
rpart.plot(arbre_elag_gamma, fallen.leaves = T, type = 5)

## évalutation du modèle ----
pred = predict(arbre_elag_gamma, newdata = test_data %>% dplyr::select(-Severity), type = 'vector')
summary(pred)

# calcul de déviance
dev_arbre_gamma <- dev_gamma(test_data$Severity, pred) # similaire à l'arbre normal
MetricsWeighted::deviance_gamma(test_data$Severity, pred)