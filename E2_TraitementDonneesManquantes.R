#### Examen synthèse Capstone
### E2 - Données et systèmes
### Traitement des données manquantes
###

# Importation des paquetages ----
liste.paquetage <- c("MASS",
                     "Hmisc",
                     "mice",
                     "naniar")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Visualisation de la non-réponse ----
data = data.frame(x1 = c(4, 6, 10, 14, -4, 0, 10, 10, 9),
                  x2 = c(5, 7, 12, 14, -10, 3, 23, 11, 10),
                  var_cont = rnorm(9, 10, 5),
                  var_categ = c(rep('ceteg_1', 3), rep('categ_2', 6)),
                  var_NA = c(NA, 3, 2, 4, NA, 4, 6, 3, 5))

# nombre de données manquantes pour chaque variable
sapply(data, function(x) sum(is.na(x)))

# patrons de non-réponse 
md.pattern(data, rotate.names = T, plot = T)

# Identification de la non-réponse ----
# Souvent, alpha = 0.05
# H0 : MCAR (p.value > alpha)
# H1 : MAR (p.value < alpha)

## Comparer les distributions (informel) ----
# variable numérique
boxplot(data$var_cont ~ is.na(data$var_NA))

# variable cotégorielle
table_data = table(is.na(data$var_NA), data$var_categ)
mosaicplot(table_data, shade = TRUE)

## Tests statistiques (formel) ----
### T-test (variable numérique) ----
t.test(data$var_cont[is.na(data$var_NA)],
       data$var_cont[!is.na(data$var_NA)])$p.value

### Test du chi-carré (variable catégorielle) ----
chisq.test(data$var_categ,
           is.na(data$var_NA),
           correct = F)$p.value

### Test de Little ----
out = mcar_test(data) 
names(out)
out$p.value

# Traitement de la non-réponse ----
## Covariance ----
cov(data$var_NA, data$x1)
cov(data$var_NA, data$x1, use = "complete.obs") # cas complets
cov(data$var_NA, data$x1, use = "pairwise.complete.obs") # cas disponibles

## Analyse des cas complets ----
data_cas_compl = na.omit(data)

## Imputation simple par la régression ----
# Note : Les variables explicatives seraient celles qui montrent des signes MAR lors des tests formels
mod = glm(var_NA ~ x1 + x2,
          data = data,
          family = 'poisson') # choisir la bonne distribution

data$imp1 = NA
data$imp1[is.na(data$var_NA)] = predict(mod, newdata = data[is.na(data$var_NA), ])

## Imputation stochastique par la régression  ----
data$imp2 = NA
set.seed(21)
data$imp2[is.na(data$var_NA)] = rpois(sum(is.na(data$var_NA)), lambda = data$imp1[is.na(data$var_NA)])

ggplot(data, aes(x = x1)) +
  geom_point(aes(y = imp1, col = 'Simple')) + 
  geom_point(aes(y = imp2, col = 'Stochastique')) +
  geom_point(aes(y = var_NA, col = 'Originale')) +
  theme_bw()











