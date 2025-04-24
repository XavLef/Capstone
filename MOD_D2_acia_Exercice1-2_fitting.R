### Module 2 AICA
## Exercice section D2
## Exercice 1 et 2 : Ajustements de distribution et de copules
library(cascsim)

## Importation des données

claiminput <- read.csv("MOD_D2_acia_short_model_data.csv")

## Dans le premier exercice, on se concentre sur la ligne "biens" ("Property")
## pour les réclamations du type "H".
lines <- c("Property")
types <- c("H")

## On veut tenir compte des réouvertures (ROPEN), des non suffisamment
## provisionnés (RBNER), des réclamations futures "UPR" et des sinistres
## SND (IBNR). Le fait d'inclure "IBNR" ici fera en sorte d'effectuer
## le traitement approprié pour la fréquence mensuelle un peu plus tard.

classes <- c("RBNER","ROPEN","IBNR","UPR")
lines <- unique(lines)
types <- unique(types)

PropertyH<-new("ClaimType", line = "Property", claimType = "H")
PropertyH@iCopula <-  TRUE

objlist <- list(PropertyH)

wd <- getwd()

## On crée l'objet pour la simulation et pour la ligne d'affaire concernée par
## l'ajustement.

## L'indice d'exposition et l'indice de gravité sont plats, donc on ne met
## pas d'indice supplémentaire. L'ajustement pour la fréquence sera fait par
## la fonction étant donné les classes de sinistres qui ont été mentionnées plus tôt.

simobj <- new("Simulation",
              lines=lines,
              types=types,
              claimobjs=objlist,
              workingFolder= wd)

## On utilise ensuite claimFitting pour effectuer l'ajustement des distributions
## Comme il est mentionné dans l'énoncé de l'exercice qu'on prend les données
## de 2017 à 2021, on utilise 2017-01-01 comme date de départ.

## Comme aucune date plus précise que "2021" n'est donnée comme dernière date, on
## fait comme si la date d'évaluation était le 31 décembre 2021 ("2021-12-31").

## Les copules seront utilisées dans l'exercice 2.

simobj <- claimFitting(simobj,
                       claiminput,
                       startDate = as.Date("2017-01-01"),
                       evaluationDate = as.Date("2021-12-31"),
                       discreteDist = c("Poisson","NegativeBinomial","Geometric"),
                       continuousDist = c("Normal","Lognormal","Weibull", "Gamma","Exponential"),
                       copulaList = c("normal","clayton","gumbel","frank","joe", "t"))
