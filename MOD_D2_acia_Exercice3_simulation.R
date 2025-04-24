### Module 2 AICA
## Exercice section D2
## Exercice 3 : Simulations des réclamations
library(cascsim)

## Les distributions postulées pour les simulations sont celles qui
## ont été sélectionnées (et estimées) dans les exercices 1 et 2.

## Importation des données

claiminput <- read.csv("MOD_D2_acia_short_model_data.csv")

## Dans cet exercice, on se concentre sur la ligne "biens" ("Property")
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

# Hypothèse pour le délai de réclamation
PropertyH@reportLag <-new("Exponential", p1 = 0.021)

# Hypothèse pour le délai de règlement
PropertyH@settlementLag <- new("Exponential", p1 = 0.026)

# Hypothèse pour la sévérité
PropertyH@severity <- new("Lognormal", p1 =7.127, p2 =0.202)

# Hypothèse pour la fréquence mensuelle
PropertyH@frequency <- new("NegativeBinomial", p1 =14.239, p2=0.413)

# Hypothèse pour la copule
PropertyH@iCopula <- TRUE
PropertyH@ssrCopula <- new("CopulaObj", type ="gumbel", dimension = 3,
                           param = 1.0033)
PropertyH@ssrCopula@marginal <- c(PropertyH@severity,
                                  PropertyH@settlementLag,
                                  PropertyH@reportLag)

simobj <- new("Simulation", lines=lines, types=types, claimobjs= objlist)


simobj@workingFolder <- getwd()

# Nombre de simulations
simobj@simNo <- 20


simobj@ncores <- 1

# Classes de réclamations à simuler
simobj@iRBNER <-TRUE # Réclamations ouvertes
simobj@iROPEN <-TRUE # Réouvertures
simobj@iIBNR <-TRUE # SSND
simobj@iUPR <-TRUE # Réclamations futures

# Établissement des dates
start_date <-  as.Date("2017-01-01")
evaluation_date <-  as.Date("2021-12-31")
future_date <-  as.Date("2022-12-31")

# Point d'ancrage pour la reproductibilité des résultats
set.seed(20240429)

# Simulation de réclamations individuelles
simdata <- claimSimulation(simobj, claiminput, startDate = start_date,
                           evaluationDate = evaluation_date,
                           futureDate = future_date)

# Statistiques descriptives
simSummary <- simSummary(simobj,simdata, startDate = start_date,
                         evaluationDate = evaluation_date,
                         futureDate = future_date)

# Triangles
simTriangle <- simTriangle(simobj,claiminput,simdata, startDate = start_date,
                           evaluationDate = evaluation_date,
                           futureDate = future_date)

# Rapport html
simReport(simobj, simSummary, simTriangle, startDate = start_date,
          evaluationDate = evaluation_date,
          futureDate = future_date)

