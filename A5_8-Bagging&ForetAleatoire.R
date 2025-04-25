#### Examen synthèse Capstone
### A5.8 - Bagging et forêt aléatoire
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

# À utiliser dans caret via l'argument summaryFunction
gamma_loss = function(data, lev = NULL, model = NULL){
  
  dev = -2 * sum(-log(data$obs/data$pred) + (data$obs - data$pred)/data$pred)
  names(dev) = 'gamma_loss'
  dev
  
}

# Importation des paquetages ----
liste.paquetage <- c("caret",
                     "rpart",
                     "pROC",
                     "randomForest",
                     "rpart.plot")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Note : Pour le bagging, simplement mettre selected_sampsize = 1 et 
#        mtry = ncol(train_data) - 1

# CLassification ----
dat.compl <- read.csv("heart_dataset.csv", stringsAsFactors = TRUE)
str(dat.compl)

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(dat.compl))
train_ind <- sample(1:nrow(dat.compl), size = smp_size, replace=FALSE)

train_data <- dat.compl[train_ind, ]
test_data <- dat.compl[-train_ind, ]

## entrainement ----
set.seed(54654)

B = 150 # initialise B

selected_sampsize = 0.5 # fixe sampsize de 50 %

foret <- randomForest(target~.,
                      data = train_data,
                      mtry = sqrt((ncol(train_data) - 1)), # règle du pouce comme initialisation en classif
                      sampsize = floor(selected_sampsize*nrow(train_data)),
                      importance = TRUE,
                      ntree = B,
                      nodesize = 2)

# choix du B avec erreur de classification (1 - accuracy) OOB
plot(seq(B), foret$err.rate[,1], type="l") # choisir B quand erreur stable

B = 100 # stable

# crée l'échantillon de validation manuellement pour tester les hyperparamètres
set.seed(131)
ind.val <- sample(1:nrow(train_data), 0.2*nrow(train_data), FALSE)
valid_data <- train_data[ind.val,]
non.valid_data <- train_data[-ind.val,]

# optimisation des hyperparamètres (mtry/ncand, nodesize/minbucket et maxdepth)
# initialisation des vecteurs
seq_mtry = seq(6)
seq_nodesize = seq(25, 200, 25)

auc_temp = mtry_mat = nodesize_mat = numeric(length(seq_mtry) *
                                               length(seq_nodesize))

i = 0
for (mtry_val in seq_mtry){
  
  for (nodesize_val in seq_nodesize){
      
      i = i + 1
      
      foret <- randomForest(target~.,
                            data = non.valid_data,
                            mtry = mtry_val,
                            sampsize = floor(selected_sampsize*nrow(non.valid_data)),
                            ntree = B,
                            nodesize = nodesize_val)
      
      # prévisions en validation
      pred = predict(foret, newdata=valid_data, type="prob")
      foret.res <- roc(valid_data$target, pred[,2], auc=TRUE, plot=TRUE)
      
      # note le résultat
      auc_temp[i] = foret.res$auc
      mtry_mat[i] = mtry_val
      nodesize_mat[i] = nodesize_val
      
      print(paste0('Progression : ', round(i/length(auc_temp), 4) * 100, ' %'))

  }
}

mat = cbind(mtry_mat,
            nodesize_mat,
            auc_temp)

# stock les valeurs optimales
mtry_optim = mat[which.max(auc_temp), 1]
nodesize_optim = mat[which.max(auc_temp), 2]

# construction de la forêt optimisée
foret_optim <- randomForest(target~.,
                            data = train_data,
                            mtry = mtry_optim,
                            sampsize = floor(selected_sampsize*nrow(train_data)),
                            ntree = B,
                            nodesize = nodesize_optim)

# revérifier que l'erreur OOB est toujours stable
plot(seq(B), foret_optim$err.rate[,1], type="l") # ok

## évaluation du modèle ----
pred_optim = predict(foret_optim, newdata=test_data, type="prob")
foret_optim.res <- roc(test_data$target, pred_optim[,2], auc=TRUE, plot=TRUE)
foret_optim.res$auc

# output
list(hyperparametres = list(mtry = mtry_optim,
                            nodesize = nodesize_optim,
                            subsample = selected_sampsize,
                            nb_arbres = B),
     modele = foret_optim,
     auc = foret_optim.res$auc)

## importance des variables ----
varImpPlot(foret_optim)

# Régression (anova) ----
data("pg15training")
data = pg15training # copie dataset
str(data)

##  feature engineering (si applicable) ----
data_sev = data %>%  
  dplyr::filter(Indtppd > 0) %>% # filtre sur la condition d'avoir au moins une réclamation pour des dommages matériels
  dplyr::mutate(Severity = Indtppd/Numtppd) %>% # crée la variable de sévérité
  dplyr::select(!c(PolNum, Exppdays, Numtpbi, Numtppd, Indtpbi, Indtppd, SubGroup2)) # retrait des variables sans intérêt

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(data_sev))
train_ind <- sample(1:nrow(data_sev), size = smp_size, replace=FALSE)

train_data <- data_sev[train_ind, ]
test_data <- data_sev[-train_ind, ]

## entrainement ----
set.seed(54654)

B = 150 # initialise B

selected_sampsize = 0.5 # fixe sampsize de 50 %

foret <- randomForest(Severity~.,
                      data = train_data,
                      mtry = floor((ncol(train_data) - 1)/3), # règle du pouce comme initialisation en regression
                      sampsize = floor(selected_sampsize*nrow(train_data)),
                      importance = TRUE,
                      ntree = B,
                      nodesize = 2)

# choix du B avec erreur de classification (1 - accuracy) OOB
plot(1:length(foret$mse), foret$mse,
     type = 'l',
     xlab = 'B',
     ylab = 'MSE',
     main = 'Random Forest') # choisir B quand erreur stable

B = 100 # stable

# crée l'échantillon de validation manuellement pour tester les hyperparamètres
set.seed(131)
ind.val <- sample(1:nrow(train_data), 0.2*nrow(train_data), FALSE)
valid_data <- train_data[ind.val,]
non.valid_data <- train_data[-ind.val,]

# optimisation des hyperparamètres (mtry/ncand, nodesize/minbucket et maxdepth)
# initialisation des vecteurs
seq_mtry = seq(6)
seq_nodesize = seq(25, 200, 25)

rmse_temp = mtry_mat = nodesize_mat = numeric(length(seq_mtry) *
                                                length(seq_nodesize))

i = 0
for (mtry_val in seq_mtry){
  
  for (nodesize_val in seq_nodesize){
    
    i = i + 1
    
    foret <- randomForest(Severity~.,
                          data = non.valid_data,
                          mtry = mtry_val,
                          sampsize = floor(selected_sampsize*nrow(non.valid_data)),
                          ntree = B,
                          nodesize = nodesize_val)
    
    # prévisions en validation
    pred = predict(foret, newdata=valid_data %>% dplyr::select(-Severity), type="response")
    foret.res <- sqrt(mean((pred - valid_data$Severity)^2)) # RMSE
    
    # note le résultat
    rmse_temp[i] = foret.res
    mtry_mat[i] = mtry_val
    nodesize_mat[i] = nodesize_val
    
    print(paste0('Progression : ', round(i/length(rmse_temp), 4) * 100, ' %'))
    
  }
}

mat = cbind(mtry_mat,
            nodesize_mat,
            rmse_temp)

# stock les valeurs optimales
mtry_optim = mat[which.min(rmse_temp), 1]
nodesize_optim = mat[which.min(rmse_temp), 2]

# construction de la forêt optimisée
foret_optim <- randomForest(Severity~.,
                            data = train_data,
                            mtry = mtry_optim,
                            sampsize = floor(selected_sampsize*nrow(train_data)),
                            ntree = B,
                            nodesize = nodesize_optim)

# revérifier que l'erreur OOB est toujours stable
plot(1:length(foret_optim$mse), foret_optim$mse,
     type = 'l',
     xlab = 'B',
     ylab = 'MSE',
     main = 'Random Forest') # ok

## évaluation du modèle ----
pred_optim = predict(foret_optim, newdata=test_data %>% dplyr::select(-Severity), type="response")
foret.optim.res <- sqrt(mean((pred_optim - test_data$Severity)^2))

# output
list(hyperparametres = list(mtry = mtry_optim,
                            nodesize = nodesize_optim,
                            subsample = selected_sampsize,
                            nb_arbres = B),
     modele = foret_optim,
     RMSE = foret.optim.res)

## importance des variables ----
varImpPlot(foret_optim)

# Régression (autres distributions) ----
# Distributions : "anova", "poisson", "class", "exp", "gamma" ou "lognormal".
data("pg15training")
data = pg15training # copie dataset
str(data)

##  feature engineering (si applicable) ----
data_sev = data %>%  
  dplyr::filter(Indtppd > 0) %>% # filtre sur la condition d'avoir au moins une réclamation pour des dommages matériels
  dplyr::mutate(Severity = Indtppd/Numtppd) %>% # crée la variable de sévérité
  dplyr::select(!c(PolNum, Exppdays, Numtpbi, Numtppd, Indtpbi, Indtppd, SubGroup2)) # retrait des variables sans intérêt

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(data_sev))
train_ind <- sample(1:nrow(data_sev), size = smp_size, replace=FALSE)

train_data <- data_sev[train_ind, ]
test_data <- data_sev[-train_ind, ]

## entrainement ----
set.seed(54654)

B = 150 # initialise B

selected_sampsize = 0.5 # fixe sampsize de 50 %

controles = rpart.control(cp = 0,
                          # split alternatif
                          maxsurrogate = 3,
                          # selon Breiman et.al (1984)
                          usesurrogate = 2,
                          # xval = 0 obligé avec rforest Gamma
                          xval = 0,
                          # limite les interactions à 5
                          maxdepth = 5,
                          # au moins 1 % des données d'entrainement par feuille
                          minbucket = floor(0.01*nrow(train_data)))

foret = rforest(Severity~.,
                data = train_data,
                method = 'gamma', # forêt aléatoire Gamma (d'où distRforest::rforest)
                ncand = floor((ncol(train_data)-1)/3), # règle du pouce comme initialisation en régression
                control = controles,
                ntrees = B,
                # proportion de sous-échantillonnage (décorréler les arbres)
                subsample = selected_sampsize,
                track_oob = T)

# choix du B avec MSE OOB
oob_df_2 <- data.frame('iteration' = seq_len(length(foret[['oob_error']])),
                       'oob_error' = foret[['oob_error']])

ggplot(oob_df_2, aes(x = iteration, y = oob_error)) + geom_point() # choisir B quand erreur stable

B = 150 # stable

# crée l'échantillon de validation manuellement pour tester les hyperparamètres
set.seed(131)
ind.val <- sample(1:nrow(train_data), 0.2*nrow(train_data), FALSE)
valid_data <- train_data[ind.val,]
non.valid_data <- train_data[-ind.val,]

# optimisation des hyperparamètres (mtry/ncand, nodesize/minbucket et maxdepth)
# initialisation des vecteurs
seq_mtry = seq(6)
seq_nodesize = seq(25, 200, 25)
seq_depth = seq(3, 5)

dev_gamma_temp = mtry_mat = nodesize_mat = depth_mat = numeric(length(seq_mtry) *
                                                                 length(seq_nodesize) *
                                                                 length(seq_depth))

i = 0
for (mtry_val in seq_mtry){
  
  for (nodesize_val in seq_nodesize){
    
    for (depth_val in seq_depth){
      
      i = i + 1
      
      controles = rpart.control(cp = 0,
                                maxsurrogate = 3,
                                usesurrogate = 2,
                                xval = 0,
                                maxdepth = depth_val,
                                minbucket = nodesize_val)
      
      foret_valid = rforest(Severity~.,
                            data = non.valid_data,
                            method = 'gamma',
                            ncand = mtry_val,
                            control = controles,
                            ntrees = B,
                            subsample = subsample)
      
      # prévisions en validation
      pred = predict.rforest(foret_valid, newdata = valid_data)
      
      # note le résultat
      dev_gamma_temp[i] = dev_gamma(valid_data$Severity, pred)
      mtry_mat[i] = mtry_val
      nodesize_mat[i] = nodesize_val
      depth_mat[i] = depth_val
      
      print(paste0('Progression : ', round(i/length(dev_gamma_temp), 4) * 100, ' %'))
      
    }
  }
}

mat = cbind(mtry_mat,
            nodesize_mat,
            depth_mat,
            dev_gamma_temp)

# stock les valeurs optimales
mtry_optim = mat[which.min(dev_gamma_temp), 1]
nodesize_optim = mat[which.min(dev_gamma_temp), 2]
depth_optim = mat[which.min(dev_gamma_temp), 3]

controles = rpart.control(cp = 0,
                          maxsurrogate = 0,
                          xval = 0,
                          maxdepth = depth_optim,
                          minbucket = nodesize_optim)

# construction de la forêt optimisée
foret_optim = rforest(Severity~.,
                      data = train_data,
                      method = 'gamma',
                      ncand = mtry_optim,
                      control = controles,
                      ntrees = B,
                      subsample = subsample,
                      track_oob = TRUE)

# revérifier que l'erreur OOB est toujours stable
oob_df_2 <- data.frame('iteration' = seq_len(length(foret_optim[['oob_error']])),
                       'oob_error' = foret_optim[['oob_error']])

ggplot(oob_df_2, aes(x = iteration, y = oob_error)) + geom_point() # ok

## évaluation du modèle ----
pred = predict.rforest(foret_optim, newdata = test_data)
dev_foret = dev_gamma(test_data$Severity, pred)

# output
list(hyperparametres = list(mtry = mtry_optim,
                            nodesize = nodesize_optim,
                            maxdepth = depth_optim,
                            subsample = subsample,
                            nb_arbres = B),
     modele = foret_optim,
     deviance = dev_foret)

## importance des variables ----
varImpPlot(foret_optim)









