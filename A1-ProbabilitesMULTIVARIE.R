#### Examen synthèse Capstone
### A1 - Probabilités - multivarié
###

#### Loi discrète bivariée à partir d'une matrice de fmp conjointe

fxy <- matrix(c(0.5, 0.05, 0.05,
                0.05, 0.3, 0.01,
                0.01, 0.02, 0.01), nrow = 3, ncol = 3, byrow = TRUE)

x <- 0:2
y <- 0:2

espxy <- sum(outer(x, y) * fxy)
sum(fxy)

# Pour faire l'espérance de X et l'espérance de Y, on a besoin des marginales!

(fx <- rowSums(fxy))
(fy <- colSums(fxy))

(espx <- sum(x * fx))
(espy <- sum(y * fy))

## Pour avoir le coefficient linéaire de Pearson, on prend les variances
(varx <- sum(x^2 * fx) - espx^2)
(vary <- sum(y^2 * fy) - espy^2)

## Covariance
(cov <- espxy - espx * espy)

(pearson <- cov / sqrt(varx * vary))

## Fonction de répartition

repart <- function(ax, ay, x, y, fxy)
{

    matx <- matrix(numeric(length(x) * length(y)), ncol = length(y))
    for (i in 1:length(x))
    {
        matx[i, ] <- rep(x[i], length(y))
    }

    maty <- matrix(numeric(length(x) * length(y)), ncol = length(y))
    for (i in 1:length(y))
    {
        maty[, i] <- rep(y[i], length(x))
    }

    sum(fxy[as.logical((matx <= ax) * (maty <= ay))])
}
repart(2, 3, x, y, fxy)
repart(1, 2, x, y, fxy)

## Densité de S

ds <- function(s, x, y, fxy)
{
    fxymod <- matrix(numeric((s + 1) * (s + 1)), ncol = s + 1)
    fxymod[1:length(x), 1:(length(y))] <- fxy
    masse <-  function(l) sum(sapply(0:l, function(a) fxymod[a + 1, l - a + 1]))
    sapply(0:s, masse)
}

(fs <- ds(4, x, y, fxy))
sum(fs)

##### Loi discrète bivariée à partir d'une matrice.
#### Simulation à partir des conditionnelles
## Simuler M1 indép et ensuite simuler de M2|M1 = m1

repconjmat <- matrix(c(0.12, 0.18, 0.18, 0.18, 0.24, 0.3,
                       0.12, 0.24, 0.24, 0.27, 0.34, 0.4,
                       0.12, 0.42, 0.52, 0.6, 0.74, 0.8,
                       0.16, 0.46, 0.56, 0.64, 0.81, 0.9,
                       0.18, 0.48, 0.58, 0.66, 0.83, 0.96,
                       0.2, 0.5, 0.6, 0.68, 0.85, 1), ncol = 6, byrow = TRUE)

FM1 <- repconjmat[, 6]
FM2 <- repconjmat[6, ]

fm1 <- c(FM1[1], diff(FM1))
fm2 <- c(FM2[1], diff(FM2))

xx <- 0:5

sum(xx * fm1)
sum(xx * fm2)

repconj <- function(m1, m2)
{
    if ((m1 < 0) | (m2 < 0))
        return(0)

    repconjmat[m1 + 1, m2 + 1]
}

massconj <- function(m1, m2)
{
    repconj(m1, m2) - repconj(m1 - 1, m2) - repconj(m1, m2 - 1) +
        repconj(m1 - 1, m2 - 1)
}

masseconj(0, 0)

fm1m2 <- repconjmat * 0

for (i in xx)
    for (j in xx)
    {
        fm1m2[i + 1, j + 1] <- massconj(i, j)
    }


sum(fm1m2)

# Covariance

fm1m2 <- 0 * repconjmat

for (i in xx)
    for (j in xx)
    {
        fm1m2[i + 1, j + 1] <- masseconj(i, j)
    }

sum(fm1m2)

sum(outer(xx, xx) * fm1m2) - sum(xx * fm1) * sum(xx * fm2)

# fM1 sachant M2

massM1.M2 <- function(m1, m2)
{
    massconj(m1, m2) / fm2[m2 + 1]
}

massM1.M2(xx, 2)

# Espérance de M1 | M2 = 2
sum(sapply(xx, function(a) a * massM1.M2(a, m2)))



MATmassM1.M2 <- matrix(numeric(length(xx)^2), ncol = length(xx))
for (m1 in xx)
    for (m2 in xx)
    {
        MATmassM1.M2[m1 + 1, m2 + 1] <- massM1.M2(m1, m2)
    }

MATmassM1.M2

set.seed(20231124)
uu <- runif(2)

reaM1 <- xx[min(which(cumsum(fM1) >= uu[1]))]
reaM2 <- xx[min(which(cumsum(MATmassM1.M2[ , reaM1 + 1]) >= uu[2]))]


### Anticomonotone et Comonotone ###
# Q1 Deux discrètes comonotones
### COMONOTONES ####
# X1 ~ Poisson(lam = 5)
# X2 ~ BN(r = 3, q = 0.4)

# Quand on connait comonotonicité ou antimonotonicité, on connait l'expression
# de la fonction de répartition
lam <- 5
r <- 3
q <- 0.4

repconj <- function(x1, x2) pmin(ppois(x1, lam), pnbinom(x2, r, q))

masseconj <- function(x1, x2) repconj(x1, x2) - repconj(x1 - 1, x2) - repconj(x1, x2 - 1) + repconj(x1 - 1, x2 - 1)

xx1 <- 0:100
xx2 <- 0:100

sum(masseconj(xx1, xx2))

sum(outer(xx1, xx2, function(x, y) masseconj(x, y)))

# E[X1 * X2]
x1x2 <- outer(xx1, xx2)
sum(outer(xx1, xx2, function(x, y) x * y * masseconj(x, y)))

# Cov
sum(outer(xx1, xx2, function(x, y) x * y * masseconj(x, y))) - lam * r * (1 - q) / q

# E[min(X1, X2)] --> BIEN REGARDER oÙ ON MET LA TRANSFORMATION
sum(outer(xx1, xx2, function(x, y) pmin(x, y) * masseconj(x, y)))

### Anticomonone ####
# X1 ~ Poisson(lam = 5)
# X2 ~ BN(r = 3, q = 0.4)

# Quand on connait comonotonicité ou antimonotonicité, on connait l'expression
# de la fonction de répartition
lam <- 5
r <- 3
q <- 0.4

repconj <- function(x1, x2) pmax(ppois(x1, lam) + pnbinom(x2, r, q) - 1, 0)

masseconj <- function(x1, x2) repconj(x1, x2) - repconj(x1 - 1, x2) - repconj(x1, x2 - 1) + repconj(x1 - 1, x2 - 1)

xx1 <- 0:100
xx2 <- 0:100

sum(masseconj(xx1, xx2))

# E[X1 * X2]
x1x2 <- outer(xx1, xx2)
sum(outer(xx1, xx2, function(x, y) x * y * masseconj(x, y)))

# Cov
sum(outer(xx1, xx2, function(x, y) x * y * masseconj(x, y))) - lam * r * (1 - q) / q

# E[min(X1, X2)] --> BIEN REGARDER oÙ ON MET LA TRANSFORMATION
sum(outer(xx1, xx2, function(x, y) pmin(x, y) * masseconj(x, y)))

