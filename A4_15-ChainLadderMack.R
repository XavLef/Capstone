#### Examen synthèse Capstone
### A15 - Tarification et provisionnement d'assurance à court terme
###

library(ChainLadder)

# Modèle de Chain-Ladder et Modèle de Mack ----

## Triangle incrémental des données
# s'assurer de le remplir par colonne ou par ligne selon l'argument byrow
tri_incr <- matrix(c(26312, 31467, 24672, 13055, 6158,
                     30470, 35012, 25491, 12589, NA,
                     49756, 51831, 35267, NA, NA,
                     50420, 52315, NA, NA, NA,
                     56762, NA, NA, NA, NA), 
                   ncol = 5, 
                   byrow = TRUE)

## Triangle cumulatif des données (conversion)
tri_cum <- incr2cum(tri_incr) # incrémental vers cumulatif

cum2incr(tri_cum) # cumulatif vers incrémental


## Ajustement du modèle de Mack ----
# tri_cum : triangle CUMULATIF
model1 <- MackChainLadder(tri_cum, est.sigma = "Mack")
summary(model1)

## Loss development ratio
model1$f

## Triangle avec développement
model1$FullTriangle

## Perte ultime pour chaque AY
CL_sum <- summary(model1)

CL_sum$ByOrigin
CL_sum$ByOrigin$Ultimate

## Réserve de Chain-Ladder ----
sum(CL_sum$ByOrigin$IBNR)

## Sigma^2
model1$sigma^2

## Écart-type de la réserve totale de Mack
model1$Total.Mack.S.E



## Fonction pour extraire la diagonale pour une année de calendrier spécifique
extraire_diagonale_calendrier <- function(full_triangle, calendar_year) {
    start_year <- 1997  # Année de début du triangle
    latest_year <- start_year + nrow(full_triangle) - 1
    diag <- numeric(nrow(full_triangle))

    for (i in 1:nrow(full_triangle)) {
        origin_year <- start_year + i - 1
        dev_year <- calendar_year - origin_year
        if (dev_year >= 0 && dev_year < ncol(full_triangle)) {
            diag[i] <- full_triangle[i, dev_year + 1]
        } else {
            diag[i] <- NA
        }
    }
    return(diag)
}


extraire_diagonale_calendrier(tri_cum, 1999)


## Possible de créer le tiangle avec le package ChainLadder

## Basic creation of a triangle from loss development data
triangle(c(100, 150, 175, 180, 200),
         c(110, 168, 192, 205),
         c(115, 169, 202),
         c(125, 185),
         150)

## Same, with named origin periods
triangle("2012" = c(100, 150, 175, 180, 200),
         "2013" = c(110, 168, 192, 205),
         "2014" = c(115, 169, 202),
         "2015" = c(125, 185),
         "2016" = 150)

## Again, with also named development periods
test <- triangle("2012" = c("12 months" = 100,
                    "24 months" = 150,
                    "36 months" = 175,
                    "48 months" = 180,
                    "60 months" = 200),
         "2013" = c(110, 168, 192, 205),
         "2014" = c(115, 169, 202),
         "2015" = c(125, 185),
         "2016" = 150)

modd <- MackChainLadder(test, est.sigma = "Mack")
summary(modd)

# Méthode du taux de sinistralité espéré (ELR) ----
data = data.frame(year = factor(seq(from = 2000, to = 2005)),
                  
                  primes_acq = c(200000, 205000, 210000,
                                 212500, 220000, 215000),
                  
                  mtn_payes = c(158000, 150000, 145000,
                                140000, 125000, 112000),
                  # taux de sinistralité espérés
                  ELR = c(0.85, 0.875, 0.85,
                          0.78, 0.8, 0.75))

C_LR = data$ELR * data$primes_acq
reserve_par_annee = data$ELR * data$primes_acq - data$mtn_payes
reserve_tot = sum(data$ELR * data$primes_acq) - sum(data$mtn_payes)
sum(reserve_par_annee) # verif










