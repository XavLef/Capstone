#### Examen synthèse Capstone
### A4.12 - Construction et sélection de modèle paramétrique
###
###

library(actuar)
library(MASS)
library(fitdistrplus)

#### Ajustement par maximum de vraisemblance ----

### Données individuelles et complètes ----
set.seed(123)
x <- rgamma(100, shape = 5, rate = 0.1)
fit_gamma <- fitdistr(x, "gamma")
fit_gamma$loglik
fit_gamma$vcov
fit_gamma$sd

x <- rnorm(2000, mean = 5, sd = 0.1)
fit_norm <- fitdistr(x, "normal")
fit_norm$estimate
fit_norm$loglik
fit_norm$vcov
fit_norm$sd

# (on est capables d'écrire une vraisemblance et d'utiliser
# optim...)

# Log-vraisemblance
## PAR DÉFAUT, OPTIM FAIT DE LA MINIMISATION, DONC SOIT ON CODE
## LA VRAISEMBLANCE NÉGATIVE SOIT ON FAIT -FUN DANS OPTIM
loglik <- function(par) {
    mu <- par[1]
    sigma <- par[2]
    if (sigma <= 0) return(-Inf)
    -sum(dnorm(x, mean = mu, sd = sigma, log = TRUE))
}

# Initial
start <- c(mu = mean(x), sigma = sd(x))

# Optimisation
fit <- optim(start, loglik, hessian = TRUE, method = "BFGS")

fit #  résultats cohérents avec plus haut.

## Données groupées ----
cj <- c(0, 25, 50, 100, 250, 500, 1000)
nj <- c(30, 31, 57, 42, 45, 10)
(x <- grouped.data(Group = cj, Frequency = nj))

loglik <- function(par)
{
    mu <- par[1]
    sigma <- par[2]
    if (sigma <= 0) return(-Inf)
    -sum(c(nj, 0) * log(pnorm(c(cj[-1], Inf), mu, sigma) -
                      pnorm(cj, mu, sigma)))
}

midpoints <- head(cj, -1) + diff(cj) / 2
midpoints_indiv <- unlist(sapply(1:length(nj),
                                 function(a) rep(midpoints[a],
                                                 nj[a])))
start <- c(mu = mean(midpoints_indiv),
           sigma = sd(midpoints_indiv))

optim(start, loglik, hessian = TRUE, method = "BFGS")

### Minimum distance estimation


### Distribution Splicing ----
#library(ReIns) # ATTENTION interaction entre actuar et ReIns
#SpliceFitPareto(log.inc_claims, 0.85)

# utilise des mélanges d'Erlang en bas du seuil et une Pareto en haut.
# ne pense pas qu'on en aura besoin.

### fitdist (pkg fitdistrplus)
# au besoin, utiliser fitdist, qui permet d'utiliser la méthode des moments
# ou la méthode des quantiles
# method = "mle"
# method = "mme" (moment matching estimation)
# method = "qme" (quantile matching estimation)
# method = "mge" (maximum goodness-of-fit estimation)
# method = "mse" (maximum spacing estimation)

### Données groupées
data(gdental)
quantile(gdental)
summary(gdental)

Fn(quantile(gdental))
knots(Fn) # groupes

# Ogive
curve(Fn, xlim = c(0, 4000))

## On peut aussi refaire les groupes de l'ogive
cj <- c(0, 25, 50, 100, 250, 500, 1000, 4000)
nj <- c(30, 31, 57, 107, 84, 45, 24)
Fn <- ogive(gdental)
Fn2 <- ogive(cj, nj)

curve(Fn, xlim = c(0, 4000), col = "blue")
curve(Fn2, xlim = c(0, 4000), col = "red", add = TRUE)

#### Sélection de modèles (TESTS VISUELS) ----
## Superposition des densités ----
data <- rgamma(1000, 10, 1)
fit <- fitdist(data, distr = "gamma", method = "mle")
mle_alpha <- fit$estimate[1]
mle_beta <- fit$estimate[2]

plot(density(data, bw = 1,
                    kernel = "epanechnikov"), ylim = c(0, 0.15),
     xlab = "x", ylab = "Densité",
     main = "Superposition des densités")

curve(dgamma(x, mle_alpha, mle_beta),
      add = TRUE, col = "blue", lwd = 2)


## Grahpique PP ----
library(copula)
plot(pgamma(sort(data),
            mle_alpha, mle_beta), sort(pobs(data)),
     ylab = "Répartition empirique",
     xlab = "Répartition gamma", main = "PP-Plot Gamma", col = "blue")
abline(a = 0, b = 1, lwd = 3)


## Superposition des fonctions de répartition ----
plot(ecdf(data),
     lwd = 3, main = " ",
     ylab = "F(x) ou Fn(x)",
     xlab = "Sinistres ($)")

curve(pgamma(x, mle_alpha, mle_beta), add = TRUE,
      col = "blue", lty = 1, lwd = 2)

## Différence entre la répartition estimée et la répartition
## empirique ----
plot(sort(pobs(data)) -
         pgamma(sort(data), mle_alpha,
                mle_beta),
     col = "blue", xlab = "x",
     ylab = "Différence", yaxt = "n", cex = 0.4)
abline(h = 0)

## Grahpique QQ ----
qqplot(qgamma(pobs(data), mle_alpha, mle_beta),
       data,
       main = "QQ-plot",
       xlab = "Quantiles théoriques (estimés)",
       ylab = "Quantiles empiriques")
abline(a=0, b = 1) # Le modèle a des queues moins lourdes que les données

#### Sélection de modèles (TESTS STATISTIQUES) ----
## Dans les tests, on pose toujours l'hypothèse
## H0: F = F_0. H1: F \neq F_0

## Test du chi-carré (données groupées ou grouper nos données)
n <- 16
data <- rpois(n, 134)
nk <- c(sum(data < 127),
        sum((data >= 127) & (data <= 136)),
        sum(data > 136))
y_pearson <- data.frame(Freq = c("< 127", "{127, 128, ..., 136}", "> 136"),
                        nk = nk)
lambda_chapeau <- mean(data)
E_pois <- c(0,0,0)
E_pois[1] <- n*ppois(126, lambda_chapeau)
E_pois[2] <- n*(ppois(136, lambda_chapeau) - ppois(126, lambda_chapeau))
E_pois[3] <- n*(ppois(136, lambda_chapeau, lower.tail = FALSE))
Pearson_pois <- sum(((y_pearson$nk - E_pois)^2)/E_pois)
dl <- 3-1-1
(pval <- round(pchisq(Pearson_pois, dl, lower.tail = TRUE),3))

## Test de Kolmogorov-Smirnov
x <- rgamma(100, 3, 0.1)
ks.test(x, "pgamma", 3, 0.1,
        alternative = "two.sided", exact = TRUE)

## Test de Anderson-Darling
library(goftest)
ad.test(x,
        null = "pgamma", 3, 0.1,
        estimated = FALSE) # FALSE si les paramètres sont fixés d'avance
                           # TRUE si les paramètres sont estimés avec les données


## TRV
x <- rgamma(100, 3, 0.1)

ll1 <- sum(dgamma(x, 3, 0.1, log = TRUE))
ll0 <- sum(dexp(x, 0.1, log = TRUE))

stat_trv <- 2 * (ll1 - ll0)
1 - pchisq(stat_trv, 1)

##
AIC <- function(ll, r) # r = nombre de paramètres à estimer
{                       # on veut le plus petit AIC
    2 * (-ll + r)
}

BIC <- function(ll, r, n) # n = nombre de données
{
    (-2 * ll + r * log(n))
}