#### Examen synthèse Capstone
### AE2 - Analyse exploratoire et normalisation
###
###

# Paquetages ----
liste.paquetage <- c("tidyverse",
                     "FactoMineR",
                     "factoextra",
                     "plotly", "reshape2", "ISLR2")

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


# Train-test split ----
## NOTE: idéalement, on ferait ça avant l'analyse exploratoire. On explore juste
##       les données de train pour y aller vraiment à l'aveugle. On fait le même
##       prétraitement. Puis on ne fait que répéter avec test. Pour la normalisation,
##       il faut vraiment le faire comme ça pour éviter du leakage.
proportion_entrainement <- 0.8

set.seed(20240305)

taille_ech <- floor(proportion_entrainement * nrow(data))
idx_entrainement <- sample(1:nrow(data), size = taille_ech)

train <- data[idx_entrainement, ] # à voir si on garde la variable réponse ou pas
test <- data[-idx_entrainement, ]

# ACP ----
## S'ASSURER DE NE GARDER QUE LES VARIABLES NUMÉRIQUES
## NE PAS INCLURE LA VARIABLE RÉPONSE SI ON VEUT TRANSPOSER ENSUITE

train_x <- train[, colnames(train) != "medv"]
test_x <- test[, colnames(test) != "medv"]

acp <- PCA(train_x, scale = TRUE)

acp$eig

## Contributions de la première composante
sort(acp$var$contrib[,1], decreasing = TRUE)

## Magnitudes vectorielles des données
## Cela nous informe sur la qualité de la projection
## des variables dans le nouvel espace
## (une bonne projection donnera une somme de cos2
##  près de 1)
acp$var$cos2

## Autre visualisation
fviz_pca_var(acp,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)

## ggplot
train <- cbind(train, acp$ind$coord)

ggplot(data = train, aes(x=Dim.1, y=Dim.2, color = medv)) +
    geom_point() +
    xlab('Dimension 1') +
    ylab('Dimension 2')  +
    theme_minimal()

## Nombre de composantes
fviz_screeplot(acp, ncp=18)


### TRANSPOSER EN TEST
test_scores <- predict(acp, newdata = test_x)$coord
cbind(predict(acp, newdata = train_x)$coord[, 1],
      acp$ind$coord[, 1]) # OK
 # garder le nombre qu'on veut
