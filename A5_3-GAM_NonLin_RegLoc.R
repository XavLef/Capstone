#### Examen synthèse Capstone
### A5.3 - Modèles linéaires étendus
### Non-linéarité, régression locale et GAM
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps",
                     "glmnet", "FactoMineR", "pls", "gam",
                     "akima", "interp")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

library(CASdatasets)

# Importation des données ----

data <- Wage

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

# Régression polynomiale ----
fit <- lm(wage ~ poly(age , 4), data = Wage) # mieux pour les stats
coef(summary(fit))

fit2 <- lm(wage ~ poly(age , 4, raw = T), data = Wage) # mieux pour les interpr.
coef(summary(fit2))
fit2a <- lm(wage ~ age + I(age^2) + I(age^3) + I(age^4),
            data = Wage) # même affaire
coef(fit2a)

## Visualisation d'un effet ----
attach(data)
agelims <- range(age)
age.grid <- seq(from = agelims[1], to = agelims[2])
preds <- predict(fit , newdata = list(age = age.grid),
                 se = TRUE)
se.bands <- cbind(preds$fit + 2 * preds$se.fit ,
                  preds$fit - 2 * preds$se.fit)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 1, 1),
    oma = c(0, 0, 4, 0))
plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Degree -4 Polynomial", outer = T)
lines(age.grid, preds$fit, lwd = 2, col = "blue")
matlines(age.grid , se.bands, lwd = 1, col = "blue", lty = 3)
detach(data)

fit.1 <- lm(wage ~ age , data = Wage)
fit.2 <- lm(wage ~ poly(age , 2), data = Wage)
fit.3 <- lm(wage ~ poly(age , 3), data = Wage)
fit.4 <- lm(wage ~ poly(age , 4), data = Wage)
fit.5 <- lm(wage ~ poly(age , 5), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
coef(summary(fit.5))
coef(summary(fit.5))[, 3]^2 # t-values identiques aux f-statistics de anova


## Modèle binomial ----
## on peut utiliser un polynôme pour n'importe quelle famille
fit <- glm(I(wage > 250) ~ poly(age , 4), data = Wage ,
             family = binomial)
preds <- predict(fit , newdata = list(age = age.grid), se = T)
pfit <- exp(preds$fit) / (1 + exp(preds$fit))

# intervalles de confiance
se.bands.logit <- cbind(preds$fit + 2 * preds$se.fit ,
                        preds$fit - 2 * preds$se.fit)
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# plus direct
preds <- predict(fit , newdata = list(age = age.grid),
                 type = "response", se = T) # mais valeurs négatives possibles!!
attach(Wage)
plot(age , I(wage > 250), xlim = agelims , type = "n",
     ylim = c(0, .2))
points(jitter(age), I((wage > 250) / 5), cex = .5, pch = "|", col
       = "darkgrey")
lines(age.grid, pfit, lwd = 2, col = "blue")
matlines(age.grid , se.bands , lwd = 1, col = "blue", lty = 3)


# Fonctions escalier ----
table(cut(age , 4))
fit <- lm(wage ~ cut(age , 4), data = Wage)
coef(summary(fit))

# Splines ----
library(splines)
# bs = matrice des fonctions de base avec les noeuds (knots) spécifiés
## fonction de base  = cadre général pour les fonctions de régression avec noeuds
fit <- lm(wage ~ bs(age , knots = c(25, 40, 60)), data = Wage)
pred <- predict(fit , newdata = list(age = age.grid), se = T)
attach(Wage)
plot(age , wage , col = "gray")
lines(age.grid, pred$fit, lwd = 2)
lines(age.grid , pred$fit + 2 * pred$se, lty = "dashed")
lines(age.grid , pred$fit - 2 * pred$se, lty = "dashed")

# utilisation de df pour avoir des knots uniformes
dim(bs(age , knots = c(25, 40, 60)))
dim(bs(age , df = 6))
attr(bs(age , df = 6), "knots")


# ns = natural spline (spline qui doit être linéaire aux bornes du domaine,
#      donc ils ont des boundary constraints. Résultats plus stables aux extrêmes.)
# cubic spline = spline qui part d'un polynôme cubique
fit2 <- lm(wage ~ ns(age , df = 4), data = Wage) # on pourrait aussi dire knots pour
                                                # spécifier automatiquement
pred2 <- predict(fit2 , newdata = list(age = age.grid),
                 se = T)
lines(age.grid , pred2$fit , col = "red", lwd = 2)

# smoothing spline (on cherche polynôme avec contrainte de smoothness en pénalité)
## Entraîné par moindres carrés, mais avec la pénalité sur l'intégrale de
## la deuxième dérivée au carré.

plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Smoothing Spline")
# Spécifie directement le nombre de df et va chercher le lambda qui donne ça
fit <- smooth.spline(age , wage , df = 16)
# Par validation croisée, va chercher le meilleur lambda et on regarde le df associé
fit2 <- smooth.spline(age , wage , cv = TRUE) # donne un warning mais c'est ok
fit2$df
lines(fit , col = "red", lwd = 2)
lines(fit2 , col = "blue", lwd = 2)
legend("topright", legend = c("16 DF", "6.8 DF"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)

# Régression locale ----
plot(age , wage , xlim = agelims , cex = .5, col = "darkgrey")
title("Local Regression")
fit <- loess(wage ~ age , span = .2, data = Wage) # span 0.2 == 20 % des données ont un poids
fit2 <- loess(wage ~ age , span = .5, data = Wage)
lines(age.grid, predict(fit , data.frame(age = age.grid)),
      col = "red", lwd = 2)
lines(age.grid, predict(fit2 , data.frame(age = age.grid)),
      col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.2", "Span = 0.5"),
       col = c("red", "blue"), lty = 1, lwd = 2, cex = .8)


# GAM ----
library(gam)
gam1 <- lm(wage ~ ns(year , 4) + ns(age , 5) + education ,
           data = Wage) # un GAM c'est un lm avec des fonctions de base

gam.m3 <- gam(wage ~ s(year , 4) + s(age , 5) + education ,
              data = Wage)

par(mfrow = c(1, 3))
plot(gam.m3, se = TRUE, col = "blue")
plot.Gam(gam1 , se = TRUE, col = "red")


gam.m1 <- gam(wage ~ s(age , 5) + education , data = Wage)
gam.m2 <- gam(wage ~ year + s(age , 5) + education ,
                data = Wage)
anova(gam.m1, gam.m2, gam.m3, test = "F") # Le modèle avec year est meilleur
## ANOVA pour effets paramétriques --> est-ce que l'effet est statistiquement
                                    # significatif même linéairement seulement
summary(gam.m3)                     # H0 et H1 comme on le fait toujours

## ANOVA pour effets non paramétriques # est-ce que l'effet non linéaire est nécessaire?
                                        # H0: effet linéaire
                                        # H1 : effet non linéaire


preds <- predict(gam.m2, newdata = Wage)
gam.lo <- gam(
    wage ~ s(year , df = 4) + lo(age , span = 0.7) + education ,
    data = Wage) # lo pour inclure régression locale dans le GAM
plot(gam.lo, se = TRUE, col = "green")


gam.lo.i <- gam(wage ~ lo(year , age , span = 0.5) + education ,
                data = Wage) # on réduit le span du lo
plot(gam.lo.i)

## GAM binomial ----
## ou toute autre famille
gam.lr <- gam(
    I(wage > 250) ~ year + s(age , df = 5) + education ,
    family = binomial, data = Wage
)
par(mfrow = c(1, 3))
plot(gam.lr, se = T, col = "green")


gam.lr.s <- gam(
    I(wage > 250) ~ year + s(age , df = 5) + education ,
    family = binomial , data = Wage ,
    subset = (education != "1. < HS Grad")
)
plot(gam.lr.s, se = T, col = "green")
