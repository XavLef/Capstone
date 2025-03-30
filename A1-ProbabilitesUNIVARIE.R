#### Examen synthèse Capstone
### A1 - Probabilités
###

## Coefficients binomiaux et multinomiaux

# 10 parmi 20
choose(20, 10)

# 4, 4, 12 parmi 20
library(iterpc)
multichoose(c(4, 4, 12))


## Distributions univariées

# Mélange discret
fA <- dbinom(0:100, 100, 0.1)

fB <- dbinom(0:100, 100, 0.6)

fX <- 0.8 * fA + 0.2 * fB

# Espérance
xx <- 0:100
sum(fX * xx)

# Fmp avec nfft pour effectuer une somme de v.a.
nfft <- 2^15
lam <- 1
mm <- 0:(nfft - 1)
fM3 <- ppois(mm, lam)

sum(mm * fM3) # Espérance
sum(mm^2 * fM3) - sum(mm * fM3)^2 # Variance
cumsum(fM3)[20] # Répartition évaluée à 19

(v0.9 <- min(which(cumsum(fM3) >= 0.9)) - 1) # VaR au niveau 90
## Le -1 est important si on utilise le domaine qui commence à 0
qpois(0.9, lam)

# TVaR au niveau 90 (stop-loss  / (1 - kappa) + var0.90)
sum(pmax(mm - v0.9, 0) * fM3) / (1 - 0.9) + v0.9 #

# Simulation
N <- 10000
reaM3 <- rpois(N, lam)

mean(reaM3) # Espérance
mean(reaM3^2) - mean(reaM3)^2 # Variance
mean(reaM3 <= 19) # Répartition évaluée à 19
sort(reaM3)[0.9 * N] # VaR au niveau 90
mean(pmax(N - 2, 0)) # Stop-loss à 2
mean(sort(reaM3)[(0.9 * nn):nn]) # TVaR au niveau 90


### MÉLANGE D'ERLANG ET LOI COMPOSÉE
# Exemple de mélange d'Erlang avec loi binomiale composée en fréquence
# Aussi un exemple de modèle fréquence-sévérité (loi composée)
### ACT-2001: Introduction à l'actuariat II
## Exemple Diapos Chapitre 4 partie 1 (diapos 80-81)

r <- c(1, 2, 5, 25)
EM <- 200
q <- r / (EM + r)


b <- 1


### Repart
## Binomiale négative composée avec r et beta = 1. Le paramètre q
## est fixé tel que l'espérance de la binomiale négative est 200.
repX <- function(x, r, q, b, k0 = 10000)
{
    dnbinom(0, r, q) +
        sum(sapply(1:k0, function(a)
            dnbinom(a, r, q) * pgamma(x, a, b)))
}

repX()
VAR <- function(k, r, q, b, k0 = 10000)
{
    optimize(function(a) abs(repX(a, r, q, b, k0) - k), c(100, 1500))$minimum
}

VaR0.5 <- numeric(4)
VaR0.5[1] <- VAR(0.5, r[1], q[1], b)
VaR0.5[2] <- VAR(0.5, r[2], q[2], b)
VaR0.5[3] <- VAR(0.5, r[3], q[3], b)
VaR0.5[4] <- VAR(0.5, r[4], q[4], b)

VaR0.995 <- numeric(4)
VaR0.995[1] <- VAR(0.995, r[1], q[1], b)
VaR0.995[2] <- VAR(0.995, r[2], q[2], b)
VaR0.995[3] <- VAR(0.995, r[3], q[3], b)
VaR0.995[4] <- VAR(0.995, r[4], q[4], b)


TVaR <- function(k, r, q, b, k0 = 10000, VaR)
{


    (1 / (1 - k)) *
        sum(sapply(1:k0, function(a)
            dnbinom(a, r, q) * a / b *
                pgamma(VaR, a + 1, b, lower.tail = FALSE)))
}

TVaR0.5 <- numeric(4)
TVaR0.5[1] <- TVaR(0.5, r[1], q[1], b, VaR = VaR0.5[1])
TVaR0.5[2] <- TVaR(0.5, r[2], q[2], b, VaR = VaR0.5[2])
TVaR0.5[3] <- TVaR(0.5, r[3], q[3], b, VaR = VaR0.5[3])
TVaR0.5[4] <- TVaR(0.5, r[4], q[4], b, VaR = VaR0.5[4])

TVaR0.995 <- numeric(4)
TVaR0.995[1] <- TVaR(0.995, r[1], q[1], b, VaR = VaR0.5[1])
TVaR0.995[2] <- TVaR(0.995, r[2], q[2], b, VaR = VaR0.5[2])
TVaR0.995[3] <- TVaR(0.995, r[3], q[3], b, VaR = VaR0.5[3])
TVaR0.995[4] <- TVaR(0.995, r[4], q[4], b, VaR = VaR0.5[4])

cbind(VaR0.5, VaR0.995, TVaR0.5, TVaR0.995)


### Simulation modèle fréquence-sévérité
echantillonner_modele_2 <- function(n, qDistN, qDistX)
{
    realisations <- list()

    max_N_i <- 0

    for (i in seq(n))
    {
        U_i <- runif(1)

        realisation_i <- vector(mode = "numeric")
        realisation_i[1] <- qDistN(U_i)

        if (realisation_i[1] != 0)
        {
            U_xi <- runif(realisation_i[1])
            X_i <- sapply(U_xi, qDistX)
            realisation_i <- c(realisation_i[1], X_i)
        }

        if (realisation_i[1] > max_N_i)
        {
            max_N_i <- realisation_i[1]
        }

        realisations[[i]] <- realisation_i
    }

    list(realisations = realisations,
         max_N_i = max_N_i)
}

structurer_echantillon <- function(ech_list, max_N_i)
{
    n <- length(ech_list)

    realisations <- matrix(rep(0, n * (max_N_i + 1)), nrow = n, ncol = max_N_i + 1)

    for (i in seq(n))
    {
        realisations[i, 1:(ech_list[[i]][1] + 1)] <- ech_list[[i]]
    }

    realisations
}