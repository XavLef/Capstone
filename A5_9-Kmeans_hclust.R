#### Examen synthèse Capstone
### A5.9 - Clustering (K-means et clutering hiérarchique)
###
###

# Paquetages ----
liste.paquetage <- c("tidyverse",
                     "FactoMineR",
                     "factoextra",
                     "plotly", "reshape2", "ISLR2",
                     "cluster",
                     "ggplot2",
                     "lsa", "tm", "wordcloud")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

library(CASdatasets)

# Importation des données ----

data <- flower
colnames(data) <- c("est_hivernale","aime_ombre", "a_tubercule",
                      "couleur", "sol", "preference", "hauteur_cm",
                      "dist_plants_cm")

# Train-test split ----
## NOTE: idéalement, on ferait notre apprentissage non supervisé uniquement
##       en train, ne touchant jamais au test.
# proportion_entrainement <- 0.8
#
# set.seed(20240305)
#
# taille_ech <- floor(proportion_entrainement * nrow(data))
# idx_entrainement <- sample(1:nrow(data), size = taille_ech)
#
# train <- data[idx_entrainement, ] # à voir si on garde la variable réponse ou pas
# test <- data[-idx_entrainement, ]

# Matrice de distances (numérique) ----
distances <- dist(scale(Boston))
sim <- 1 / (1 + distances)

# Matrice de dissimilaité (Gower) ----
dissim <- daisy(data)
daisy(data, type = list(asymm = "a_tubercule")) # spécifier des asymétries

# K-moyennes -----
boston_scaled <- scale(Boston)
set.seed(202)
K <- 4
kmoyennes <- kmeans(boston_scaled, K, nstart=5)

boston_clust <- data.frame(cbind(boston_scaled, "cluster" = factor(kmoyennes$cluster)))
ggplot(boston_clust, aes(x = lstat, y = medv,
                                                    col = factor(cluster))) +
    geom_point() + theme_minimal()


boston_acp <- PCA(Boston, scale = FALSE, ncp = 2, graph = FALSE)

composante1 <- boston_acp$ind$coord[,1]
composante2 <- boston_acp$ind$coord[,2]
groupe <- as.factor(kmoyennes$cluster)

ggplot() + geom_point(aes(composante1, composante2, color = groupe)) + theme_minimal()

# K-médoïdes -----
## Fournir une matrice de dissimilarité à pam
K <- 4
kmedoids <- pam(daisy(data, type = list(asymm = "a_tubercule")), K, diss = TRUE)
names(kmedoids)

kmedoids$id.med

# Déterminer k ----
## Silhouette ----
silhouettes <- rep(NA, 10)
dissim_tubercule <- daisy(data, type = list(asymm = "a_tubercule"))
for (k in 2:10) {
    sil <- silhouette(pam(dissim_tubercule, k, diss=TRUE), dissim_tubercule)
    silhouettes[k] <- mean(sil[, 3])
}

which.max(silhouettes)

silhouettes <- rep(NA, 10)
distbost <- dist(boston_scaled)
for (k in 2:10) {
    sil <- silhouette(kmeans(boston_scaled, k)$cluster, distbost)
    silhouettes[k] <- mean(sil[, 3])
}
which.max(silhouettes)


# Hclut ----
## Single linkage (distance minimale) ----
hc_min <- hclust(distbost, method = "single")
plot(hc_min)
type_min <- cutree(tree=hc_min, k=2)
table(type_min)

## Complete linkage (distance max) ----
hc_max <- hclust(distbost, method = "complete")
plot(hc_max)

type_max <- cutree(tree=hc_max, k=2)
table(type_max)

## Méthode de Ward ----
hc_ward <- hclust(distbost, method = "ward.D")
plot(hc_ward)

type_ward <- cutree(tree=hc_ward, k=2)
table(type_ward)

## cute ----
rect.hclust(hc_ward, k = 4, border = 2:5)


# Choix de k ----
## Silhouette ----
factoextra::fviz_nbclust(x = boston_scaled, FUNcluster = hcut,
                         hc_method = "ward.D", method = "silhouette",
                         k.max = 10) # JUSTE À CHANGER FUNcluter par
                                    # hcut ou kmeans.

factoextra::fviz_nbclust(x = boston_scaled, FUNcluster = kmeans,
                         method = "silhouette",
                         k.max = 10)

## Coude ----
factoextra::fviz_nbclust(x = boston_scaled, FUNcluster = kmeans,
                         method = "wss",
                         k.max = 10)

## Statistique GAP ----
clust_gap <- cluster::clusGap(boston_scaled, FUN = kmeans,
                             nstart = 25,
                              K.max = 10, B = 50)

factoextra::fviz_gap_stat(clust_gap)

