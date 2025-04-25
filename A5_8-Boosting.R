#### Examen synthèse Capstone
### A5.8 - Boosting
###

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
                     "tidyr", # pivot_longer
                     "RhpcBLASctl", # parallel processing
                     "lightgbm",
                     "rpart.plot")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# CLassification ----
dat.compl <- read.csv("heart_dataset.csv", stringsAsFactors = TRUE)
str(dat.compl)

## split ----
set.seed(123)

smp_size <- floor(0.8 * nrow(dat.compl))
train_ind <- sample(1:nrow(dat.compl), size = smp_size, replace=FALSE)

data_train <- dat.compl[train_ind, ]
data_test <- dat.compl[-train_ind, ]

# optimisation des hyperparamètres par la méthode Holdout (LGOCV avec un pli)
set.seed(123)
idx_valid = sample(x = nrow(data_train),
                   size = 0.3*nrow(data_train))

data_valid = data_train[idx_valid,]
data_non_valid = data_train[-idx_valid,]

# établir les règles de conversion
conversion = lgb.convert_with_rules(data = data_train %>% dplyr::select(-target),
                                    rules = NULL)

train_conv = conversion$data

valid_conv = lgb.convert_with_rules(data = data_valid %>% dplyr::select(-target),
                                    rules = conversion$rules)$data

non_valid_conv = lgb.convert_with_rules(data = data_non_valid %>% dplyr::select(-target),
                                        rules = conversion$rules)$data

test_conv = lgb.convert_with_rules(data = data_test %>% dplyr::select(-target),
                                   rules = conversion$rules)$data

# numéro de la colonne
categ_id = names(conversion$rules) %>% sapply(function(n){which(n == names(train_conv))})

# conversion des jeux de données pour la fonction lightgbm (pas le raw data)
dtrain = lgb.Dataset(train_conv %>% as.matrix,
                     label = data_train$target,
                     categorical_feature = categ_id,
                     params = list(feature_pre_filter = F))

dvalid = lgb.Dataset(valid_conv %>% as.matrix,
                     label = data_valid$target,
                     categorical_feature = categ_id,
                     params = list(feature_pre_filter = F))

dnon_valid = lgb.Dataset(non_valid_conv %>% as.matrix,
                         label = data_non_valid$target,
                         categorical_feature = categ_id,
                         params = list(feature_pre_filter = F))

dtest = lgb.Dataset(test_conv %>% as.matrix,
                    label = data_test$target,
                    categorical_feature = categ_id,
                    params = list(feature_pre_filter = F))

# valeurs à tester
seq_min_data = seq(2, 20, 2)
seq_nrounds = seq(100, 2000, 100)
seq_max_depth = seq(3, 7)
seq_feature_frac = c(1/4, 1/2, 3/4, 1)
subsample = 0.75
learning_rate = 0.01

# initialisation des vecteurs
auc_temp = min_data_mat = nrounds_mat = max_depth_mat = feature_frac_mat = numeric(length(seq_min_data) *
                                                                                         length(seq_nrounds) *
                                                                                         length(seq_max_depth) *
                                                                                         length(seq_feature_frac))

i = 0
for (min_data_val in seq_min_data){
  
  for (nrounds_val in seq_nrounds){
    
    for (max_depth_val in seq_max_depth){
      
      for(feature_frac_val in seq_feature_frac){
        
        i = i + 1
        
        parameters = list(task = 'train', # entrainement
                          objective = 'binary', # fonction de perte
                          metric = 'ROC', # métrique d'évaluation
                          boosting = 'gbdt', # traditional Gradient Boosting Decision Tree
                          data_sample_strategy = 'bagging', # peut-être essayer 'goss'
                          learning_rate = learning_rate, # taux d'apprentissage
                          min_data_in_leaf = min_data_val, # nombre minimal d'observations par feuille
                          nrounds = nrounds_val, # nombre d'itérations du boosting
                          verbose = -1, # log seulement si c'est fatal
                          subsample = subsample, # proportion de sous-échantillonnage
                          max_depth = max_depth_val, # profondeur maximale des arbres
                          feature_fraction_bynode = feature_frac_val, # sous-ensemble de prédicteurs pour chaque noeud
                          first_metric_only = TRUE) # utiliser une seule métrique d'évaluation
        
        lgb_boost = lightgbm(dnon_valid,
                             params = parameters)
        
        # prévisions en validation
        pred = suppressWarnings(predict(lgb_boost, newdata = as.matrix(data_valid %>% dplyr::select(-target))))
        
        # note le résultat
        auc_temp[i] = roc(data_valid$target, pred, auc=TRUE, plot=TRUE)$auc
        min_data_mat[i] = min_data_val
        nrounds_mat[i] = nrounds_val
        max_depth_mat[i] = max_depth_val
        feature_frac_mat[i] = feature_frac_val
        
        print(paste0('Progression : ', round(i/length(auc_temp), 4) * 100, ' %'))
        
      }
    }
  }
}

mat = cbind(min_data_mat,
            nrounds_mat,
            max_depth_mat,
            feature_frac_mat,
            auc_temp)

# stock les valeurs optimales
min_data_optim = mat[which.max(auc_temp), 1]
nrounds_optim = mat[which.max(auc_temp), 2]
max_depth_optim = mat[which.max(auc_temp), 3]
feature_frac_optim = mat[which.max(auc_temp), 4]

parameters = list(task = 'train',
                  objective = 'binary',
                  metric = 'ROC',
                  boosting = 'gbdt',
                  data_sample_strategy = 'bagging',
                  learning_rate = learning_rate,
                  min_data_in_leaf = min_data_optim, 
                  nrounds = nrounds_optim,
                  verbose = -1,
                  subsample = subsample,
                  max_depth = max_depth_optim,
                  feature_fraction_bynode = feature_frac_optim,
                  first_metric_only = TRUE)

# construction du LightGBM optimisée
lgb_optim = lightgbm(dtrain,
                     params = parameters) # warning correct

# prévisions
pred = suppressWarnings(predict(lgb_boost, newdata = as.matrix(data_test %>% dplyr::select(-target)), type = 'response'))
pred_resume = summary(pred)

auc_optim = roc(data_test$target, pred, auc=TRUE, plot=TRUE)$auc

# output
list(hyperparametres = list(min_data = min_data_optim,
                            nrounds = nrounds_optim,
                            maxdepth = max_depth_optim,
                            feature_frac = feature_frac_optim,
                            subsample = subsample,
                            learning_rate = learning_rate),
     modele = lgb_optim,
     pred_summary = pred_resume,
     auc = auc_optim)

# Régression ----
# Distributions : 'regression' (anova), 'poisson', 'gamma', 'tweedie', 'binary', etc...
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

data_train <- data_sev[train_ind, ]
data_test <- data_sev[-train_ind, ]

## entrainement ----
# optimisation des hyperparamètres par la méthode Holdout (LGOCV avec un pli)
set.seed(123)
idx_valid = sample(x = nrow(data_train),
                   size = 0.3*nrow(data_train))

data_valid = data_train[idx_valid,]
data_non_valid = data_train[-idx_valid,]

# établir les règles de conversion
conversion = lgb.convert_with_rules(data = data_train %>% dplyr::select(-Severity),
                                    rules = NULL)

train_conv = conversion$data

valid_conv = lgb.convert_with_rules(data = data_valid %>% dplyr::select(-Severity),
                                    rules = conversion$rules)$data

non_valid_conv = lgb.convert_with_rules(data = data_non_valid %>% dplyr::select(-Severity),
                                        rules = conversion$rules)$data

test_conv = lgb.convert_with_rules(data = data_test %>% dplyr::select(-Severity),
                                   rules = conversion$rules)$data

# numéro de la colonne
categ_id = names(conversion$rules) %>% sapply(function(n){which(n == names(train_conv))})

# conversion des jeux de données pour la fonction lightgbm (pas le raw data)
dtrain = lgb.Dataset(train_conv %>% as.matrix,
                     label = data_train$Severity,
                     categorical_feature = categ_id,
                     params = list(feature_pre_filter = F))

dvalid = lgb.Dataset(valid_conv %>% as.matrix,
                     label = data_valid$Severity,
                     categorical_feature = categ_id,
                     params = list(feature_pre_filter = F))

dnon_valid = lgb.Dataset(non_valid_conv %>% as.matrix,
                         label = data_non_valid$Severity,
                         categorical_feature = categ_id,
                         params = list(feature_pre_filter = F))

dtest = lgb.Dataset(test_conv %>% as.matrix,
                    label = data_test$Severity,
                    categorical_feature = categ_id,
                    params = list(feature_pre_filter = F))

# valeurs à tester
seq_min_data = seq(220, 250, 5)
seq_nrounds = seq(200, 250, 300)
seq_max_depth = seq(3, 5)
seq_feature_frac = c(1/4, 1/2, 3/4, 1)
subsample = 0.75
learning_rate = 0.0005

# initialisation des vecteurs
dev_gamma_temp = min_data_mat = nrounds_mat = max_depth_mat = feature_frac_mat = numeric(length(seq_min_data) *
                                                                                           length(seq_nrounds) *
                                                                                           length(seq_max_depth) *
                                                                                           length(seq_feature_frac))

i = 0
for (min_data_val in seq_min_data){
  
  for (nrounds_val in seq_nrounds){
    
    for (max_depth_val in seq_max_depth){
      
      for(feature_frac_val in seq_feature_frac){
        
        i = i + 1
        
        parameters = list(task = 'train', # entrainement
                          objective = 'gamma', # fonction de perte Gamma
                          metric = 'gamma_deviance', # métrique d'évaluation
                          boosting = 'gbdt', # traditional Gradient Boosting Decision Tree
                          data_sample_strategy = 'bagging', # peut-être essayer 'goss'
                          learning_rate = learning_rate, # taux d'apprentissage
                          min_data_in_leaf = min_data_val, # nombre minimal d'observations par feuille
                          nrounds = nrounds_val, # nombre d'itérations du boosting
                          verbose = -1, # log seulement si c'est fatal
                          subsample = subsample, # proportion de sous-échantillonnage
                          max_depth = max_depth_val, # profondeur maximale des arbres
                          feature_fraction_bynode = feature_frac_val, # sous-ensemble de prédicteurs pour chaque noeud
                          first_metric_only = TRUE) # utiliser une seule métrique d'évaluation
        
        lgb_boost = lightgbm(dnon_valid,
                             # objective = 'gamma',
                             params = parameters)
        
        # prévisions en validation
        pred = suppressWarnings(predict(lgb_boost, newdata = as.matrix(data_valid %>% dplyr::select(-Severity))))
        
        # note le résultat
        dev_gamma_temp[i] = dev_gamma(data_valid$Severity, pred)
        min_data_mat[i] = min_data_val
        nrounds_mat[i] = nrounds_val
        max_depth_mat[i] = max_depth_val
        feature_frac_mat[i] = feature_frac_val
        
        print(paste0('Progression : ', round(i/length(dev_gamma_temp), 4) * 100, ' %'))
        
      }
    }
  }
}

mat = cbind(min_data_mat,
            nrounds_mat,
            max_depth_mat,
            feature_frac_mat,
            dev_gamma_temp)

# stock les valeurs optimales
min_data_optim = mat[which.min(dev_gamma_temp), 1]
nrounds_optim = mat[which.min(dev_gamma_temp), 2]
max_depth_optim = mat[which.min(dev_gamma_temp), 3]
feature_frac_optim = mat[which.min(dev_gamma_temp), 4]

parameters = list(task = 'train',
                  objective = 'gamma',
                  metric = 'gamma_deviance',
                  boosting = 'gbdt',
                  data_sample_strategy = 'bagging',
                  learning_rate = learning_rate,
                  min_data_in_leaf = min_data_optim, 
                  nrounds = nrounds_optim,
                  verbose = -1,
                  subsample = subsample,
                  max_depth = max_depth_optim,
                  feature_fraction_bynode = feature_frac_optim,
                  first_metric_only = TRUE)

# construction du LightGBM optimisée
lgb_optim = lightgbm(dtrain,
                     params = parameters) # warning correct

# prévisions
pred = suppressWarnings(predict(lgb_boost, newdata = as.matrix(data_test %>% dplyr::select(-Severity)), type = 'response'))
pred_resume = summary(pred)

# calcul déviance
dev_lgb = dev_gamma(data_test$Severity, pred)

# output
list(hyperparametres = list(min_data = min_data_optim,
                            nrounds = nrounds_optim,
                            maxdepth = max_depth_optim,
                            feature_frac = feature_frac_optim,
                            subsample = subsample,
                            learning_rate = learning_rate),
     modele = lgb_optim,
     pred_summary = pred_resume,
     deviance = dev_lgb)








