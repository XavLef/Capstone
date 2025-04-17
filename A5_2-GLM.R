#### Examen synthèse Capstone
### A5.2 - Modèles linéaires généralisés
### GLM
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps", "CASDatasets",
                     "glmnet", "MASS", "pscl", "pROC")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Importation des données ----
library(CASdatasets)
data("ausprivauto0405")

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

# GLM Poisson ----
# avec terme offset pour exposition au risque
mod.offset <- glm(ClaimNb~VehAge+VehBody+Gender+DrivAge+offset(log(Exposure)),
                  family=poisson,data=ausprivauto0405)
summary(mod.offset)

# predict donne lambda chapeau, donc on peu utiliser cela
# pour calculer une probabilité en utilisant la fmp d'une loi Poisson

# statistique de Pearson
pearson <- residuals(mod0,type="pearson")
sum(pearson^2)

## Analyse de résidus ----
pearson <- residuals(mod0,type="pearson") # résidus de Pearson

dev <- residuals(mod0,type="deviance") # résidus de déviance
hatmu <- fitted(mod0)
anscombe <- 1.5*((sgautonb$Clm_Count)^(2/3)-hatmu^(2/3))/hatmu^(1/6) #  résidus Anscombe
par(mfrow=c(1,3))
hist(pearson)
hist(anscombe)
hist(dev)

## Test d'adéquation ----
(chicarre.mod.offset <- sum((ausprivauto0405$ClaimNb-fitted(mod.offset))^2/
                                fitted(mod.offset)))

1-pchisq(chicarre.mod.offset, summary(mod.offset)$df.residual) # seuil observé


# GLM Binomiale négative ----
# (pkg MASS)
mod.nb <- glm.nb(ClaimNb~VehAge+VehBody+Gender+DrivAge+offset(log(Exposure)),
                 data=ausprivauto0405)
summary(mod.nb)
AIC(mod.offset,mod.nb)

## Test de simplification du modèle NB en modèle Poisson ----
phi <- 1/summary(mod.nb)$theta

# 2*log-likelihood du modèle Binomiale négative
twoll.nb <- summary(mod.nb)$twologlik
# 2*log-likelihood du modèle Poisson
twoll.pois <- c(2*logLik(mod.offset))

stat.trv <- twoll.nb-twoll.pois
# Seuil observé du TRV
0.5*(1-pchisq(stat.trv,1)) # seuil observé. H0: modèle poisson gagne.


# GLM ZINFL ----
# (pkg pscl)
mod.gonfle <- zeroinfl(ClaimNb~VehAge+VehBody+Gender+DrivAge+
                           offset(log(Exposure))|1,data=ausprivauto0405)
summary(mod.gonfle)
AIC(mod.offset,mod.nb,mod.gonfle)

## Probabilité d'avoir 0 réclamations ----
exp(-fitted(mod.offset)[c(1,41)]) # mod Poisson
dnbinom(0,mod.nb$theta,mu=fitted(mod.nb)[c(1,41)]) # mod NB
# mod ZINFL
pi_hat <- predict(mod.gonfle,type="zero")[c(1,41)]
mu_hat <- predict(mod.gonfle,type="count")[c(1,41)]
pi_hat+(1-pi_hat)*exp(-mu_hat)

# GLM Gamma ----
data("ukaggclaim")
dat <- ukaggclaim[ukaggclaim$NClaims > 0, ]
dat$NClaims <- NULL

glm_inv <- glm(AvCost ~ OwnerAge + Model + CarAge,
               family = Gamma(link = "inverse"),
               data = dat)

glm_log <- glm(AvCost ~ OwnerAge + Model + CarAge,
               family = Gamma(link = "log"),
               data = dat)

summary(glm_inv)
summary(glm_log)

pred_inv <- predict(glm_inv, newdata = newobs, type = "response")

pred_log <- predict(glm_log, newdata = newobs, type = "response")

## Intervalles de confiance prévision ----
(pred_log <- predict(glm_log,
                     newdata = newobs,
                     type = "link",
                     se.fit = TRUE))
(boundaries_eta <- cbind("lower" = pred_log$fit - qnorm(0.975) * pred_log$se.fit,
                         "pred" = pred_log$fit,
                         "upper" = pred_log$fit + qnorm(0.975) * pred_log$se.fit))
# méthode de l'inverse (pas symétrique autour de la prévision)
exp(boundaries_eta)

# méthode Delta (symétrique) --> faudrait utiliser la dérivée du lien, etc.

# GLM binomial ----
data(credit)

mod <- glm(class~.,family=binomial,data=credit)
summary(mod)

mod.reduit <- update(mod, .~.-residence_since-age-
                         existing_credits-num_dependents-job-telephone)

anova(mod.reduit, mod, test = "Chisq") ### ATTENTION dans un GLM,
                                       ### on fait des test "Chisq" vu
                                         ### qu'on fait un test de déviance
                                        ### et non des tests F partiels.
1-pchisq(2*(c(logLik(mod)) - c(logLik(mod.reduit))),
         mod.reduit$df.residual-mod$df.residual)
qchisq(0.95, 8)


pred <- predict(mod.reduit,se.fit=TRUE)
(IC <- pred$fit[1:5] + qnorm(0.975)*cbind(rep(-1,5),rep(1,5))*pred$se.fit[1:5])

# Estimations ponctuelles de la probabilité de défaut
prob.defaut <- predict(mod.reduit, type="response")[1:5]

# I.C à 95 % pour la probabilité de défaut
IC.prob.defaut <- exp(IC)/(1+exp(IC))

data.frame(borne_inf = IC.prob.defaut[,1], prev = prob.defaut,
           borne_sup = IC.prob.defaut[,2])

## Courbe ROC ----
# pkg pROC
library(pROC)
roc.mod <- roc(credit$class,fitted(mod))
roc.mod.reduit <- roc(credit$class,fitted(mod.reduit))
plot(roc.mod)
plot(roc.mod.reduit,type="l",col=2,lty=2,add=TRUE)
legend('bottomright', c("mod", "mod.reduit"), col = c(1, 2), lty = c(1, 2))
auc(roc.mod)
auc(roc.mod.reduit)

## Matrice de confusion ----
tau4 <- c("threshold" = 0.5)
# 1 si plus grand que tau, 0 sinon
Ychap <- ifelse(fitted(mod.reduit)>tau4,1,0)

# Vrai négatif
aa <- sum((credit$class==0)*(Ychap==0))
# Faux positif
bb <- sum((credit$class==0)*(Ychap==1))
# Faux négatif
cc <- sum((credit$class==1)*(Ychap==0))
# Vrai positif
dd <- sum((credit$class==1)*(Ychap==1))

# Construction du tableau de mauvaise classification
table4 <- matrix(c(aa,cc,bb,dd),
                 nrow=2,
                 byrow = FALSE)
row.names(table4) <- c("real 0", "real 1")
colnames(table4) <- c("pred 0", "pred 1")
table4
(sens4 <- c("sens4" = dd/(cc+dd)))
(spec4 <- c("spec4" = aa/(aa+bb)))

## Choix du tau optimal ----
(coords4 <- coords(roc.mod.reduit,
                   x = 0.5, #input
                   input = "threshold", # what kind of input was x?
                   ret = c("thr", "tn", "tp", "fn", "fp", "spec", "sens"),
                   # Statistics to keep
                   transpose = TRUE))

(coords6 <- coords(roc.mod.reduit,
                   x = "best", # input
                   best.method = "closest.topleft",
                   #ok, mais best selon quoi?
                   ret = c("thr", "spec", "sens", "accuracy",
                           "tn", "tp", "fn", "fp", "precision"),
                   transpose = TRUE))

# ON POURRAIT AUSSI UTILISER CONFUSIONMATRIX DANS CARET
library(caret)
confusionMatrix(factor(credit$class), factor(Ychap))
