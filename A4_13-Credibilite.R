#### Examen synthèse Capstone
### A4.13 - Crédibilité
###

library(actuar)

# rappels ----
# CV(X) = sqrt(Var(x)) / E[X] donc CV(X)^2 = Var(x) / E[X]^2

# Crédibilité complète ----
## binomiale composée ou binomiale pure  ----
# Note : varX = 0 dans le cas d'une binomiale pure
espX = 5
varX = 0
k = 0.05 # ordre
p = 0.95 # ordre
# N ~ Bin(full_cred, theta)
theta = 0.04

full_cred = ceiling((qnorm((1 + p) / 2) / k)^2 * (varX / (espX^2 * theta) +
                                                      (1 - theta) / theta))

## poisson composée ou poisson pure ----
# Note : varX = 0 dans le cas d'une poisson pure
espX = 1/2
varX = 0
k = 0.03 # ordre
p = 0.975 # ordre

full_cred = ceiling((qnorm((1 + p) / 2) / k)^2 * (1 + varX / espX^2))

## binomiale négative composée ou binomiale négative pure ----
# Note : varX = 0 dans le cas d'une binomiale négative pure
espX = 3/2
varX = (3 * 3^2) / (3 - 1)^2 * (3 - 2)
k = 0.05 # ordre
p = 0.9 # ordre
# N ~ BinNeg(full_cred, theta)
theta = 1/3

full_cred = ceiling((qnorm((1 + p) / 2) / k)^2 * (1 / (1 - theta) +
                                                      theta / (1 - theta) *
                                                        (varX / espX^2)))

# Crédibilité partielle ----
n0 = 1082 # seuil de crédibilité complète (full_cred) : E[S] >= qnorm((1 + p) / 2) / k * sqrt(Var(S))
n = 500 # ce qu'on a

## calcul de z ----
facteur_cred_classique = min(sqrt(n / n0), 1) # par défaut
facteur_cred_alternatif = min((n / n0)^(2/3), 1)

K = 10 # constante déterminée au jugement de façon à limiter les flucutations dans la prime d’une année à l’autre (swing)
formule_whitney = n / (n + K)

## prime de crédibilité ----
s_bar = 120 # expérience individuelle
m = 100 # prime collective
z = facteur_cred_classique

prime_cred = z * s_bar + (1 - z) * m

# Calcul de primes bayésiennes linéaires ----
# Note : Si ce n'est pas linéaire (voir section Linear Bayes pour les scénarios
# linéaires), on doit faire le calcul à la main (alors la prime bayésienne n'est
# pas une prime de crédibilité).

# si data = NULL : ajuste le modèle à priori (on prévoit la prime collective m = E[mu(Theta)])
# sinon ajustement du modèle à postériori (avec des données - prime bayésienne B_{n+1} = E[mu(Theta)|x1,...,xn]))

data = c(3, 1, 5, 4, 2) # sachant; ex : data = c(8, 0, 0) implique 8 sinistres dans 3 dernières années
# choix likelihood : "poisson", "exponential", "gamma", "normal", "bernoulli",
# "binomial", "geometric", "negative binomial" (mettre size = r dans les
# paramètres) ou "pareto".
likelihood = 'poisson' # chaine de caractère de la loi de S|Theta = theta

fit = cm(formula = "bayes", # prime bayésienne pure (prime de crédibilité linéaire)
         data = data, # ATTENTION (est-ce qu'on veut utiliser le data?)
         likelihood = likelihood,
         shape = 10, rate = 5) # paramètres de Theta ~ Gamma(3, 3) variables : voir dloi pour les noms)

paste0("Si on utilise data, la prime bayésienne de ", length(data) + 1,
       "ième année est ", round(predict(fit), 4),
       ". Sinon (data n'est pas dans le Call de cm), il s'agit de la prime collective")
fit$cred # z (facteur de crédibilité)
mean(data) # s_bar (expérience individuelle)
summary(fit)
# Between variance = a --> Variance of hypothetical means (VHM) Var(E[S|Theta])
# Within variance = s^2 --> Expected process variance (EPV) E[Var(S|Theta)]
# d'où Var(S) = s^2 + a
k = summary(fit)[["unbiased"]][2] / summary(fit)[["unbiased"]][1] # k = s^2 / a

# Bühlmann ----
# Note : Les primes bayésiennes linéaires et les primes de crédibilité de
# Bühlmann sont identiques dans l'approche paramétrique. Ainsi, ici, on traite
# du cas NON-PARAMÉTRIQUE.

n = 3 # nombre de contrats
data_non_param = data.frame(contract = seq(n), # id pour identifier les contrats
                            matrix(c(0, 1, 2, 1, 2, 0, # une ligne par contrat
                                     3, 4, 2, 1, 4, 4,
                                     3, 3, 2, 1, 2, 1),
                                   nrow = n,
                                   byrow = TRUE))

fit = cm(~ contract, # pour chaque contrat (ligne)
         data = data_non_param)

summary(fit)
predict(fit)

# Bühlmann-Straub ----
# Note 1 : On utilise par défaut l'estimateur sans biais de la variance inter (a hat).
# Si on veut une autre méthode, voir l'argument "method" :
# method = "Ohlsson" : estimateur sans biais (a hat);
# method = "Buhlmann-Gisler" : version tronquée à 0 --> max(a hat; 0);
# method = "iterative" : pseudo-estimateur (Bischel-Straub) de la variance (a tilde).

# Note 2 : Si les poids sont égaux, c'est simplement Bühlmann (considère pas
# l'exposition).

# Note 3 : S'il y a des valeurs manquantes dans les matrices, simplement mettre
# des NA dans ratio et weight. La fonction traite ces cas correctement.

# Attention : Si on donne la matrice S au lieu de X, il faut faire ratio = X = S / weight.

n = 3 # nombre de contrats
data_non_param = data.frame(contract = seq(n), # id pour identifier les contrats
                            # les données de sinistres
                            ratio = matrix(c(0, 1, 2, 1, 2, 0, # une ligne par contrat
                                             3, 4, 2, 1, 4, 4,
                                             3, 3, 2, 1, 2, 1),
                                           nrow = n,
                                           byrow = TRUE),
                            # les poids associés aux données de sinistres
                            weight = matrix(c(0, 1, 2, 1, 2, 0, # une ligne par contrat
                                              3, 4, 2, 1, 4, 4,
                                              3, 3, 2, 1, 2, 1),
                                              nrow = n,
                                              byrow = TRUE))

fit.B = cm(~ contract, # pour chaque contrat (ligne)
           data = data_non_param,
           ratios = ratio.1:ratio.6,
           weights = weight.1:weight.6) # tenir compte des volumes

summary(fit.B)