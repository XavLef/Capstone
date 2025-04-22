#### Examen synthèse Capstone
### A4.7/4.8/4.9 - Modèles de sévérité/fréquence avec actuar, modifications
###                de couverture et discrétisation
###

library(actuar)

### Calcul de FS par convolution

fx <- c(0, 0.15, 0.2, 0.25, 0.125, 0.075,
        0.05, 0.05, 0.05, 0.025, 0.025)  # fmp de sévérité discrétisée,
                                         # elle doit commence à P[X = 0]

pn <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.15, 0.06, 0.03, 0.01) # fmp de fréquence
                                                            # elle doit commencer
                                                            # à P[N = 0].

# exemple 9.5 du Loss Models
Fs <- aggregateDist("convolution", model.freq = pn,
                    model.sev = fx, x.scale = 25) # chaque bond est un bond de 25

Fs(300) # on peut l'appeler directement
plot(Fs)
mean(Fs) # espérance automatique
sum(seq(0, 250, by = 25) * fx) * sum(pn * 0:8) # espérance à la main
knots(Fs) # bonds de 25
Fs(knots(Fs)) # endroits où il y a des bonds dans Fs (sinon ce sont des plateaux)
diff(knots(Fs)) # fmp de S (on prend diff tout simplement)

### Calcul de FS avec Panjer
nfft <- 2^13
fx <- c(0, 0.63333, 0.26667, 0.1) # sévérité discrétisée
Fs <- aggregateDist("recursive", model.freq = "poisson",
                    model.sev = fx, lambda = 3, tol = 1e-9)
                # voir docu pour les possibilités, augmenter tol car juste 1e-6 par défaut
plot(Fs)
Fs(knots(Fs))		      # cdf evaluated at its knots
diff(Fs)                      # probability mass function


fx <- c(0, 0.15, 0.2, 0.25, 0.125, 0.075,
        0.05, 0.05, 0.05, 0.025, 0.025)
Fs <- aggregateDist("recursive", model.freq = "poisson",
model.sev = fx, lambda = 10)
Fs <- aggregateDist("recursive", model.freq = "poisson",
                    model.sev = fx, lambda = 250, convolve = 1, maxit = nfft)
plot(Fs)
fs <- diff(Fs)
CTE(Fs, 0.95)

# ça fait la même job que fft

lam <- 250
fxfft <- numeric(nfft)
fxfft[1:length(fx)] <- fx
ffx <- fft(fxfft)
fst <- exp(lam * (ffx - 1))
fs_prime <- Re(fft(fst^2, inverse = TRUE)) / nfft
sum(fs_prime)
cbind(fs[1600:2000], fs_prime[1600:2000])

kappa <- 0.5
v0.95 <- min(which(cumsum(fs_prime) >= kappa)) +1
sum(pmax((0:(nfft - 1)) - v0.95, 0) *fs_prime) / (1 - kappa) + v0.95
CTE(Fs, kappa) # automatiquement dans actuar, mais pas exactement le même chiffre

### Calcul de FS avec Panjer et zéro-modifiée (classe(a, b, 1))
Fs <- aggregateDist("recursive", model.freq = "zero-modified binomial",
              model.sev = c(0.3, 0.5, 0.2), x.scale = 50,
              p0 = 0.4, size = 3, prob = 0.3)
mean(Fs)
n <- 3
p <- 0.3
fn <- dbinom(0:n, n, p)
p0m <- 0.4
fn_zeromodif <- c(p0m, fn[-1] * (1 - p0m) / (1 - fn[1]))
fn_zeromodif

fx <- c(0.3, 0.5, 0.2)
nfft <- 2^6
fx_ok <- numeric(nfft)
fx_ok[1:3] <- fx
fxt <- fft(fx_ok)

fgp_n <- function(t) sum(sapply(0:(length(fn_zeromodif) - 1),
                            function(a) fn_zeromodif[a + 1] * t^a))
# pourrait aussi appliquer la formule de la fgp pour loi zero-modifiée

fgp_zeromod <- (1 - (1-p0m) / (1 - fn[1])) + (1-p0m) / (1 - fn[1]) *
                                (p * fxt + 1 - p)^n
fs <- Re(fft(sapply(fxt, fgp_n), inverse = TRUE))/nfft

fs_prime <- Re(fft(fgp_zeromod, inverse = TRUE)) / nfft

cbind(diff(Fs), fs[1:7], fs_prime[1:7]) # youpi


### actuar contient les zero-modifiées, zero-tronqées, etc.

### mweibull, levweibull donne les moments (order = k) E[X^k] et les moments
### limités (respectivement) E[min(X, d)^k].


### Calcul de la pdf d'une perte avec modification de couverture

## Default case: pdf of the per payment random variable with
## an ordinary deductible
coverage(dgamma, pgamma, deductible = 1)

## Add a limit and a deductible
f <- coverage("dgamma", "pgamma", deductible = 1, limit = 7,
              per.loss = FALSE) # same
f(0, shape = 3, rate = 1)
f(2, shape = 3, rate = 1)
f(6, shape = 3, rate = 1)
f(8, shape = 3, rate = 1)
curve(dgamma(x, 3, 1), xlim = c(0, 10), ylim = c(0, 0.3))    # original
curve(f(x, 3, 1), xlim = c(0, 10), col = 4, add = TRUE) # modified

pgamma(1, 3, 1)
dgamma(3, 3, 1)
dgamma(3, 3, 1)  / (1 - pgamma(1, 3, 1))
(1 - pgamma(7, 3, 1)) / (1 - pgamma(1, 3, 1))

points(6, f(6, 3, 1), pch = 21, bg = 4)



### Discrétisation
## Discrétisation d'une loi gamma
nfft <- 2^15
h <- 0.01
fx <- c(discretise(pgamma(x, shape = 3, rate = 0.1),
           method = "upper",
           from = 0,
           to = (nfft - 1) * h,
           step = h,
           lev = levgamma(x, shape = 3, rate = 0.1)), 0) # avec upper, ajouter
                                                        # un 0 à la fin pour avoir
                                                        # vraiment nfft.
sum(fx)
length(fx)
nfft

fx <- discretise(pgamma(x, shape = 3, rate = 0.1),
                   method = "lower",
                   from = 0,
                   to = (nfft - 1) * h,
                   step = h,
                   lev = levgamma(x, shape = 3, rate = 0.1))
fx # premier élément devrait être un 0

fx <- discretise(pgamma(x, shape = 3, rate = 0.1),
                 method = "unbiased",
                 from = 0,
                 to = (nfft - 1) * h,
                 step = h,
                 lev = levgamma(x, shape = 3, rate = 0.1))
fx # premier élément devrait être un 0
sum((0:(nfft - 1)) * h * fx) # donne la bonne espérance, car unbiased
3 / 0.1

##### SELON LE LOSS MODELS, ON APPLIQUE HABITUELLEMENT LES CHANGEMENTS
##### DE COUVERTURE AVANT DE DISCRTÉISER, DONC UTILISER COVERAGE
nfft <- 2^16

## Y^P (par paiement)
f <- coverage(cdf = "pgamma", deductible = 12, limit = 60, coinsurance = 0.5,
              inflation = 0.03,
              per.loss = FALSE) # discretize prend une cdf, donc output une cdf

(pgamma((10 / 0.5 + 12) / 1.03, 3, 0.1) - pgamma(12 / 1.03, 3, 0.1)) /
    (1 - pgamma(12 / 1.03, 3, 0.1))
f(10, 3, 0.1)

f(24, 3, 0.1) # devrait donner 1

# l'argument inflation met une multplication sur la v.a., donc si je veux
# 6 % sur deux ans, on doit mettre (1.06)^2 - 1 dans l'argument.

f_discr <- c(discretize(f(x, shape = 3, rate = 0.1),
           method = "lower",
           from = 0,
           to = (nfft - 1) * h,
           step = h))

plot((0:(nfft - 1)) * h, cumsum(f_discr)) # attention faut bien valider
                                          # les masses aux points de rupture
                                          # pour que la fmp somme à 1.
curve(f(x, shape = 3, rate = 0.1), xlim = c(0, 300), add = TRUE,
      col = "red", lwd = 3)


## Y^S (par sinistre)
f <- coverage(cdf = "pgamma", deductible = 12, limit = 60, coinsurance = 0.5,
              inflation = 0.03,
              per.loss = TRUE) # discretize prend une cdf, donc output une cdf

(pgamma((10 / 0.5 + 12) / 1.03, 3, 0.1))
f(10, 3, 0.1)
f(0, 3, 0.1)
pgamma(12 / 1.03, 3, 0.1)

f(24, 3, 0.1) # devrait donner 1
