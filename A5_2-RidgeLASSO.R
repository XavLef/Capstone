#### Examen synthèse Capstone
### A5.2 - Modèles linéaires généralisés
### Régression linéaire (simple et multiple)
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps", "CASDatasets",
                     "glmnet")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Importation des données ----

data <- usexpense
usexpense$AgentWage[is.na(usexpense$AgentWage)] <- 0


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
test.indices <- sample(1:nrow(usexpense),0.3*nrow(usexpense))
train <- usexpense[-test.indices,]
test <- usexpense[test.indices,]


## model.matrix ----
x.train <- model.matrix(I(log(1+Expenses))~Group+Mutual+Stock+I(log(1+RBC))+I(log(1+StaffWage))+
                            I(log(1+AgentWage))+
                            I(log(1+LongLoss))+I(log(1+ShortLoss))+I(log(1+GWPpersonal))+
                            I(log(1+GWPcommercial))+I(log(1+Assets))+
                            I(log(1+Cash))+I(log(1+LiqRatio)), data=train)[,-1]

x.test <- model.matrix(I(log(1+Expenses))~Group+Mutual+Stock+I(log(1+RBC))+I(log(1+StaffWage))+
                            I(log(1+AgentWage))+
                            I(log(1+LongLoss))+I(log(1+ShortLoss))+I(log(1+GWPpersonal))+
                            I(log(1+GWPcommercial))+I(log(1+Assets))+
                            I(log(1+Cash))+I(log(1+LiqRatio)), data=test)[,-1]

y.train <- log(1+train$Expenses)
y.test <- log(1+test$Expenses)

# Ridge ----
## attention argument family pour utiliser autre chose qu'une loi normale
#  (utiliser un GLM)
grid.lambda <- 10^seq(3,-2,length=100)
modele.ridge <- glmnet(x.train, y.train, alpha=0,lambda=grid.lambda)
plot(modele.ridge,xvar="lambda",label=TRUE)

set.seed(4355)
cv.out <- cv.glmnet(x.train,y.train,alpha=0, nfolds=6) # validation croisée 6 plis
plot(cv.out)

(meilleur.lam.ridge <- cv.out$lambda.min)

modele.ridge_final <- glmnet(x.train, y.train, alpha = 0,
                             lambda = meilleur.lam.ridge)

predict(modele.ridge, x.test, s = meilleur.lam.ridge)
predict(modele.ridge , s = meilleur.lam.ridge, type = "coefficients")

# LASSO ----
modele.lasso <- glmnet(x.train, y.train, alpha=1,lambda=grid.lambda)
plot(modele.lasso,xvar="lambda",label=TRUE)

set.seed(4355)
cv.out <- cv.glmnet(x.train,y.train,alpha=1, nfolds=6)
plot(cv.out)
(meilleur.lam.lasso <- cv.out$lambda.min)
(msep.lasso <- mean((y.test-predict(modele.lasso,s=meilleur.lam.lasso, newx=x.test))^2))


predict(modele.lasso , s = meilleur.lam.lasso, type = "coefficients")
predict(modele.lasso , s = meilleur.lam.lasso, newx = x.test)
# Elastic Net ----

# Pour chaque alpha, on trouve le meilleur lambda par cv.

models <- list()
results <- data.frame(matrix(numeric(3 * 9), ncol = 3))
colnames(results) <- c("alpha", "lambda", "EQM_moyen")
for (i in 1:9) {
    name <- paste0("alpha", i/10)
    set.seed(934)
    models[[name]] <-
        cv.glmnet(x.train, y.train, alpha=i/10, nfolds = 5, lambda = grid.lambda)
    results[i, ] <- c(i / 10, models[[name]]$lambda.min, min(models[[name]]$cvm))
}
results
(meilleur.alpha.en <- results[which.min(results$EQM_moyen), ]$alpha);
(meilleur.lam.en <- results[which.min(results$EQM_moyen), ]$lambda)
mod.en <- glmnet(x.train, y.train, alpha = meilleur.alpha.en, lambda = grid.lambda)

pred_en <- predict(mod.en, newx = x.test, s = meilleur.lam.en)

(mse_en <- mean((pred_complet - medv.test)^2))


# Validation des résidus (maison) ----

# à adapter au jeu de données: valeur réponse - predict
#resids <- house.train$medv - predict(mod.ridge, s = meilleur.lam.ridge, type = "response", newx = x.train)

par(mfrow=c(3, 3))
for (i in 1:ncol(data))
{
    plot(x.train[, i], resids, ylab = "Résidus",
         xlab = colnames(x.train)[i])
    lines(lowess(x.train[, i], resid(mod.ridge, s = meilleur.lam.ridge)), col = "blue")
}

