#### Examen synthèse Capstone
### A5.2 - Modèles linéaires généralisés
### Régression linéaire (simple et multiple)
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

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

## Graphiques exploratoires ----

hist(data$medv)
ggplot(data, aes(x= medv,  y = after_stat(density))) +
    geom_histogram() + theme_minimal() +
    labs(x = "Revenu médian", y = "Densité",
         title = "Histogramme de la variable réponse")

ggplot(data, aes(x= medv)) +
    geom_density(fill = "cyan", alpha = 0.5) + theme_minimal() +
    labs(x = "Revenu médian", y = "Densité",
         title = "Densité estimée par noyau de la variable réponse")

barplot(table(iris$Species))
ggplot(iris, aes(x = Species)) + geom_bar() + theme_minimal() +
    labs(x = "Espèce", y = "Nombre", title = "Diagramme à bandes de l'espèce")
attach(iris)
boxplot(Petal.Length ~ Species)
detach(iris)


# possibilité d'attach pour ne pas avoir à écrire data$ toujours.


# Régression linéaire simple ----
## Ajustement du modèle ----
(modele <- lm(medv ~ lstat, data = data))

summary(modele) # Statistiques essentielles, test t sur les paramètres
names(modele) # Éléments de l'objet modele

anova(modele) # même résultat que dans summary pour la validité globale

## Prévision et intervalles de confiance ----

confint(modele, level = 0.95) # Intervalle de confiance paramètres
predict(modele,
        newdata = data.frame(lstat = (c(5, 10, 15))),
        interval = "confidence") # Intervalle de confiance sur la moyenne
                                 # (confidence interval)

predict(modele,
        newdata = data.frame(lstat = (c(5, 10, 15))),
        interval = "prediction") # Intervalle de confiance sur la valeur
                                 # (prediction interval)

plot(data$lstat, data$medv)
abline(modele) # permet de voir si potentiel pour non-linéarité
plot(data$lstat , data$medv , pch = "+") # changer le symbole
plot(1:20, 1:20, pch = 1:20) # options de symboles

## Analyse de résidus ----
resid.modele <- residuals(modele) # résidus
resid.student.modele <- rstudent(modele) # résidus studentisés

plot(predict(modele), resid.modele)
plot(predict(modele), resid.student.modele) # linéarité, homoscedasticité?
hist(resid.student.modele) # normalité?
qqnorm(resid.student.modele)
abline(1, 1)

residualPlots(modele) # pour postulat d'homoscedasticité
avPlots(modele) # pour postulat de linéarité

## Données influentes p. 101 notes act-2003 ----
# Éléments h_ii de la matrice chapeau sont la distance au centroïde,
# donc si grosse hvalue, loin des autres et grand levier.

# Valeur de h_ii moyenne = p  / n, donc Belsley et al. (1980), valeurs
# à grand levier sont h_ii > 2p / n.
mean(hatvalues(modele))
2 / nrow(Boston) # p  / n
plot(hatvalues(modele))
abline(h = 2 * 2 / nrow(Boston)) # observations à levier

# Diagnostic d'influence avec résidus standardisés
sum(abs(resid.student.modele) > 2) # student resid. > 2

# Mesure de l'influence sur la prévision DFFIT
# on retient les grandes valeurs absolues (> 2 sqrt(p/n) Belsley et al. (1980))
sum(abs(dffits(modele)) > 2 * sqrt(2 /  nrow(Boston)))

# Mesure de l'influence sur les paramètres DFBETA
# on retient les grandes valeurs absolues (> 2 / sqrt(n) Belsley et al. (1980))
apply(dfbeta(modele), 2, function(a) sum(abs(a) > 2 / sqrt(nrow(Boston))))
# signe est aussi indication. Si on veut une beta positif et que quelques
# dfbeta sont très négatifs, on a des coupables.

# DFBETA peu utile si beaucoup de variables, donc Cook's distance pour
# combiner les DFBETA d'une même observation.
# regarder grandes valeurs genre > 4 / (n-p) (Fox (1991))
sum(cooks.distance(modele) >  4 * (nrow(Boston) - 2))

# Influence sur la performance (variance) des estimateurs
# regarder covratio < 1 - 3p/n  ou covratio > 1 + 3p/n
covratio(modele)


# Régression linéaire multiple ----
pairs(data)
ggpairs(data) # dans GGally
(modele <- lm(medv ~ ., data = data))
summary(modele)
mod2 <- update(modele, . ~ . - age)

vif(mod2) # dans le pkg car. Validation multicolinéarité

(modele_interactions <- lm(medv ~ lstat * age, data = data)) # inclut déjà effets simples
summary(modele_interactions)


# Effets non linéaires ----

# polynomial
(modele_quad <- lm(medv ~ lstat + I(lstat^2), data = data))
summary(modele_quad)

(modele_fifth <- lm(medv ~ poly(lstat, 5), data = data))
summary(modele_fifth) # bon pour tester si effets significatifs,
                      # mais les interprétations ne sont pas bonnes vu que
                      # poly ajuste les effets orthogonaux.

(modele_fifth_int <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = data))
summary(modele_fifth_int) # effets colinéaires polynomiaux (effets supérieurs
                            # très faibles), mais nécessaire pour interprétation

fitted(modele_fifth)
fitted(modele_fifth_int) # prévisions identiques :)

# fonctions escalier
(modele_step <- lm(medv ~ cut(lstat, 5), data = data)) # breaks automatiques
summary(modele_step)

(modele_step <- lm(medv ~ cut(lstat, c(5, 10, 15, 45, 50, 55)),
                   data = data)) # breaks automatiques
summary(modele_step)

# ANOVA ----
# on veut comparer le modèle avec effet quadratique
# et le modèle simple.
(mod_simple <- lm(medv ~ lstat, data = data))
anova(mod_simple, modele_quad) # on garde le modèle complexe.
par(mfrow = c(2, 2))
plot(modele_quad)

fit.1 <- lm(medv ~ lstat, data = data)
fit.2 <- lm(medv ~ poly(lstat , 2), data = data)
fit.3 <- lm(medv ~ poly(lstat , 3), data = data)
fit.4 <- lm(medv ~ poly(lstat , 4), data = data)
fit.5 <- lm(medv ~ poly(lstat , 5), data = data)
fit.6 <- lm(medv ~ poly(lstat , 6), data = data)
anova(fit.1, fit.2, fit.3, fit.4, fit.5, fit.6)
summary(fit.6) # mêmes seuils
#anova marche toujours, qu'on utilise des poly orthogonaux ou non.

# Tests F partiels ----
drop1(modele, test = "F") # important de mettre argument test


# Sélection de variables ----
tous <- regsubsets(formula(modele), data = data, nvmax = 16, nbest = 1) # pkg leaps

tous.summary <- summary(tous)

npar <- rowSums(tous.summary$which)
Criteres <- data.frame(npar, Rcarre = tous.summary$rsq,
                       Rcarre.adj = tous.summary$adjr2, Cp = tous.summary$cp,
                       BIC = tous.summary$bic,
                       AIC = tous.summary$bic +npar*(1-log(nrow(Credit))))
round(Criteres, 3)