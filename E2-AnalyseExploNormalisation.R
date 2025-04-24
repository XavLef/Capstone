#### Examen synthèse Capstone
### AE2 - Analyse exploratoire et normalisation
###
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps",
                     "ggpubr", "reshape2", "pastecs",
                     "xts", "sp", "zoo", "RColorBrewer")

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

#Auto <- read.table("Auto.data", header = T, na.strings = "?",
                   #stringsAsFactors = T)

# Auto <- read.csv("Auto.csv", na.strings = "?", stringsAsFactors =
#                      T)

# dim(Auto)
# names(Auto)

# Analyse exploratoire  ----

## Aperçu jeu de données ----

head(data)
summary(data)
colnames(data)
dim(data)
str(data)

stat.desc(data)

display.brewer.all(type="all", n=4, colorblindFriendly=TRUE)

## Graphiques exploratoires ----

### Histogramme ----
hist(data$medv)
ggplot(data, aes(x= medv,  y = after_stat(density))) +
    geom_histogram() + theme_minimal() +
    labs(x = "Revenu médian", y = "Densité",
         title = "Histogramme de la variable réponse")

### Densité noyau ----
ggplot(data, aes(x= medv)) +
    geom_density(fill = "cyan", alpha = 0.5) + theme_minimal() +
    labs(x = "Revenu médian", y = "Densité",
         title = "Densité estimée par noyau de la variable réponse")

### Diagramme à bandes ----
barplot(table(iris$Species))
ggplot(iris, aes(x = Species)) + geom_bar() + theme_minimal() +
    labs(x = "Espèce", y = "Nombre",
         title = "Diagramme à bandes de l'espèce")

ggplot(iris, aes(x = Species)) + geom_bar() + theme_minimal() + coord_flip()
    labs(x = "Espèce", y = "Nombre",
         title = "Diagramme à bandes de l'espèce")

### Boxplot ----
ggplot(data, aes(x=factor(chas), y=medv))+
        geom_boxplot() + theme_minimal() +
        labs(x = "Chas", y = "Medv", title = "Medv selon le groupe de chas")

data(ausprivauto0405)
Auto <- ausprivauto0405

ggplot(Auto, aes(x=VehBody, fill=Gender)) +
    geom_bar() + coord_flip() + theme_classic()



#### Changement ordre facteurs ----
Auto$DrivAge <- factor(Auto$DrivAge,
                       levels(Auto$DrivAge)[c(6,5,4,2,1,3)])

ggplot(Auto, aes(x=DrivAge, y=VehValue, col=DrivAge)) +
    geom_boxplot() + facet_grid(~VehAge) +
    theme(axis.text.x = element_text(angle = 90))


### Nuage de points sophistiqué ----
ggplot(Auto[Auto$ClaimNb>0,],aes(x=VehValue, y=ClaimAmount/ClaimNb, col=VehAge))+
    geom_point(alpha=0.5)+
    labs(y="Montant moyen de réclamation ($)", x="Valeur du véhicule (milliers $)", title = "Montant moyen de réclamation versus valeur du véhicule", col="Âge du véhicule",
         subtitle = paste("Incluant seulement les", sum(Auto$ClaimNb>0),"observations avec au moins une réclamation"))+
    theme_classic()+theme(legend.position="bottom")

## Heatmap de corrélations -------
vars <- names(data)

df_vars <-  data[vars]

cormat<-signif(cor(df_vars),2)
melted_cormat <- melt(cormat)
head(melted_cormat)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2,
                                 fill=value)) + scale_y_discrete(limits = rev) +
    geom_tile() + ggtitle("Heatmap: Correlation Matrix") +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank(),
          axis.text.x = element_text(angle = 90)) +
    scale_fill_distiller(palette = "Green",limits=c(-1,1)) +
    geom_text(aes(Var2, Var1, label = value), color = "black",
              size = 2) +
    theme_minimal()

## Monter une carte exploratoire ----
# mapJpn <- borders("world", regions="Japan", colour="gray50", fill="white")
#
# ggplot(tremblements[which(tremblements$pays == "japan"), ], aes(x=longitude, y=latitude, size=mag, col=mag))+
#     mapJpn+
#     geom_point(alpha=0.6)+
#     theme(legend.position = "top")+
#     scale_color_distiller(palette = "BuPu", direction=1, limits=c(4,9.6))


# Prétraitement des données ----
## Visualisation rapide et identification des problèmes ----
str(data)
summary(data)

## Copie originale ----
data.init <- data

## Transformer en variable num ----
# tremblements$latitude <- as.numeric(as.character(tremblements$latitude))
# tremblements$longitude <- as.numeric(as.character(tremblements$longitude))

## Transformer en facteurs  ----
# Auto$DrivAge <- factor(Auto$DrivAge)

## Ajuster l'ordre des facteurs ----
# Auto$DrivAge <- factor(Auto$DrivAge,
#                        levels(Auto$DrivAge)[c(6,5,4,2,1,3)])

## Regrouper des facteurs ----
# tremblements$net[tremblements$net == "altas"] <- "atlas"
# tremblements$type[tremblements$type == "Earthquake"] <- "earthquake"
#
# levels(tremblements$net) <- c("atlas", "atlas", "centennial","nc",
#                               "official", "pde", "us")
#
# levels(tremblements$net) <- c("autre", "centennial", "autre", "pde", "autre")
# tremblements <- droplevels(tremblements)

## Transformation date ----
# patron <- "%Y-%m-%dT%H:%M:%S"
# tremblements$maj <- as.POSIXct(tremblements$maj, patron, tz = "UTC")
# summary(tremblements$maj)
# tremblements$temps <- as.POSIXct(tremblements$temps, patron, tz = "UTC")
# summary(tremblements$temps)

## Identifier doublons ----
#tremblements[duplicated(tremblements$id), ] #MONTRE SEULEMENT LES DEUXIÈME

# Train-test split ----
## NOTE: idéalement, on ferait ça avant l'analyse exploratoire. On explore juste
##       les données de train pour y aller vraiment à l'aveugle. On fait le même
##       prétraitement. Puis on ne fait que répéter avec test. Pour la normalisation,
##       il faut vraiment le faire comme ça pour éviter du leakage.
data <- iris
proportion_entrainement <- 0.8

set.seed(20240305)

taille_ech <- floor(proportion_entrainement * nrow(data))
idx_entrainement <- sample(1:nrow(data), size = taille_ech)

train <- data[idx_entrainement, ] # à voir si on garde la variable réponse ou pas
test <- data[-idx_entrainement, ]


# Normalisation ----
# Ne s'applique qu'on données numériques, ou faire du one-hot encoding sinon
X_oh_train <- model.matrix(Sepal.Length ~ . , data = train)[, -1]
# ENLÈVE LA VARIABLE RÉPONSE
X_oh_test <- model.matrix(Sepal.Length ~ . , data = test)[, -1]

## Échelle min-max
### Si les données sont environ uniformément distribuées et qu'il n'y a pas trop
### d'aberrations

library(caret)
processor <- preProcess(X_oh_train, method = "range")
train_proc <- predict(processor, X_oh_train)
train_std <- cbind(train_proc, train$Sepal.Length)
head(train_std)

test_proc <- predict(processor, X_oh_test)
test_std <- cbind(test_proc, test$Sepal.Length)
head(test_std)

## Standardisation

library(caret)
processor <- preProcess(X_oh_train, method = "scale")
train_proc <- predict(processor, X_oh_train)
train_std <- cbind(train_proc, train$Sepal.Length)
head(train_std)

test_proc <- predict(processor, X_oh_test)
test_std <- cbind(test_proc, test$Sepal.Length)
head(test_std)