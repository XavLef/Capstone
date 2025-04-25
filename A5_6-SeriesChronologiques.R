#### Examen synthèse Capstone
### A5.6 - Séries chronologiques
###

# Importation des paquetages ----
liste.paquetage <- c("forecast",
                     "datasets",
                     "fpp2",
                     "ggplot2",
                     "astsa",
                     "urca",
                     "tseries",
                     "hts",
                     "seasonal",
                     "ggpubr")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# Package ‘caschrono’ was removed from the CRAN repository.
# On l'installe manuellement au besoin (à partir des archives CRAN) :
# install.packages("caschrono_2.4.tar.gz", repos = NULL, type = "source")
# library(caschrono)

# Importation de la série ----
#Monthly Airline Passenger Numbers 1949-1960
serie <- ts(data = AirPassengers, 
            frequency = 12)

autoplot(serie)

# Transformation box-cox (pas nécessaire si lambda près de 1)
lam <- BoxCox.lambda(serie)

flux <- BoxCox(serie, lambda = lam) # flux = ((sign(serie) * abs(serie)^lam) - 1) / lam

BoxCox.lambda(flux)

# Analyse exploratoire série chronologique ----
## Graphe de la série
(autoplot_serie <- autoplot(flux, col = "blue") + theme_bw() +
        xlab("Temps (mois)") +
        ylab("Série")) +
    theme(title = element_text(size = 18))

## Graphe de sous-séries
mois <- c("Janvier", "Frévier", "Mars", "Avril",
          "Mai", "Juin", "Juillet", "Août", "Septembre",
          "Octobre", "Novembre", "Décembre")

(flux_subseries <- ggsubseriesplot(flux) + xlab("Mois") + ylab("Série"))

## Graphe de saisons
(flux_seasonplot <- ggseasonplot(flux, season.labels = mois) + ggtitle("") +
        xlab("Mois") +
        scale_color_discrete(name = "Année") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)))

## Graphe de décalages
(flux_lagplot <- gglagplot(flux, set.lags = c(2, 6, 8, 10, 11, 12, 14, 16, 18)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.text.y = element_text(angle = 45, hjust = 1))  +
        labs(x = "Niveau de la série",
             y = "Niveau de la série") +
        guides(colour = guide_legend(title = "Mois")))

## Décomposition ----
## Décomposition classique
(flux_decompclassique <- autoplot(decompose(flux, type = c("additive"))) +
        xlab("Temps (mois)") + ggtitle(""))

## Décomposition STL
(flux_stl <- flux %>% stl(s.window = 1, t.window = 13, robust = TRUE) %>%
        autoplot() + xlab("Temps (mois)"))

flux %>% stl(s.window = 12, t.window = 1, robust = TRUE) %>% seasonal %>% autoplot()
flux %>% stl(s.window = 12, t.window = 1, robust = TRUE) %>% trendcycle %>% autoplot()
flux %>% stl(s.window = 12, t.window = 1, robust = TRUE) %>% remainder %>% autoplot()
flux %>% stl(s.window = 12, t.window = 1, robust = TRUE) %>% seasadj %>% autoplot()

# Méthodes de prévision ----
## Séparation train-test (jouer sur end et start pour la taille voulue)
flux.train <- window(flux, end=c(10,12)) # end = c(année, mois)
flux.test <- window(flux, start=c(11, 1)) # start = c(année, mois)

## Méthodes de base
naivess_flux <- flux.train %>% snaive(h = 24, lambda = lam)
flux %>% snaive(lambda = lam) %>% checkresiduals()

naive_flux <- flux.train %>% naive(h = 24, lambda = lam)
flux %>% naive(lambda = lam) %>% checkresiduals()

meanf_flux <- flux.train %>% meanf(h = 24, lambda = lam)
flux %>% meanf(lambda = lam) %>% checkresiduals()

derive_flux <- flux.train %>% snaive(drift = TRUE, h = 24, lambda = lam)
flux %>% snaive(drift = TRUE, lambda = lam) %>% checkresiduals

## Lissage exponentiel
ses(flux, h = 24, lambda = lam)
hw_flux <- hw(flux.train, h = 24, seasonal = "multiplicative", damped = TRUE)
autoplot(flux.test) + autolayer(hw_flux, PI = F)
autoplot(flux.test) + autolayer(hw_flux, PI = T) + autolayer(flux.test)

autoplot(flux) + autolayer(ses(flux, h = 36), PI = F) + autolayer(flux)

(gghw <- autoplot(window(flux.train)) +
        autolayer(hw_flux, PI = F, series = "Holt-Winters multiplicatif", size = 1) +
        xlab("Temps (mois)") + ylab("Flux de la rivière") +
        guides(colour = guide_legend(title = "Mois")))

(comparaison <- autoplot(window(flux.train)) +
        autolayer(hw_flux, PI = F, series = "Holt-Winters multiplicatif", size = 1) +
        autolayer(naivess_flux, PI = F, series = "Naïve saisonnière", size = 1) +
        autolayer(naive_flux, PI = F, series = "Naïve", size = 1) +
        autolayer(meanf_flux, PI = F, series = "Moyenne", size = 1) +
        autolayer(flux.test, series = "VRAI SÉRIE", size = 2) +
        autolayer(derive_flux, PI = F, series = "Naïve saisonnière avec dérive", size = 1) +
        xlab("Temps (mois)") + ylab("Flux de la rivière") +
        guides(colour = guide_legend(title = "Méthode")))

## Modèle à espace d'états (pas dans les objectifs, mais au cas où)
(mod_ets <- ets(window(flux, end = c(10, 12)),  model = "ZZZ") %>% forecast(h = 24))
mod_ets %>% checkresiduals()

# Performances en test et en entraînement
accuracy(mod_ets, flux.test)
mod_ets$model$aic
mod_ets$model$bic


# Modèles Box et Jenkins (SARIMA) ----
# Analyse initiale de la stationnarité
autoplot(flux.train)
flux.train %>% ggtsdisplay()
acf2(flux.train)

Dflux <- diff(flux.train, lag = 12)
Dflux2 <- diff(Dflux, lag = 12)
D1autoplotcorr <- Dflux %>% ggtsdisplay()

D2autoplotcorr <- Dflux2 %>% ggtsdisplay()

(D1autoplot <- autoplot(Dflux, main = "", col = "blue") +
        xlab("Temps (mois)") + ylab("Série différenciée D=1") +
        theme_bw() + theme(axis.title.x = element_text(size = 16),
                           axis.title.y = element_text(size = 16)))

(D2autoplot <- autoplot(Dflux2, main = "", col = "blue") +
        xlab("Temps (mois)") + ylab("Série différenciée D=2") +
        theme_bw() + theme(axis.title.x = element_text(size = 16),
                           axis.title.y = element_text(size = 16)))

## Validation de la stationnarité
kpss.test(Dflux) # H0 :  la série est stationnaire
adf.test(Dflux) # H0: non stationnaire

# Itération du modèle SARIMA
(fitAA <- Arima(flux.train,  # série
                order = c(1, 0, 2),  # (p, d, q)
                seasonal = c(0, 2, 0), # (P, D, Q)
                lambda = NULL, # NULL: pas de transfo box-cox, transo BoxCox.lambda si lambda = "auto"
                include.mean = FALSE, # TRUE par défaut, FAUX pour les modèles avec D ou d != 0.
                include.drift = FALSE  # Inclure tendance linéaire par régression? Habituellement FALSE
                                        # (==  régression linéaire avec un ARIMA fitté aux résidus)
                ))

t_stat(fitAA) # Significativité des paramètres
autoplot(fitAA) # Unit Root Test
residuals(fitAA) %>% ggtsdisplay()
checkresiduals(fitAA) # Test Ljung-Box, H0: indépendance des résidus
shapiro.test(residuals(fitAA)) # Shapiro-Wilk sévère, p-val < 0.1 généralement adéquat # H0: non normal
ks.test(residuals(fitAA), pnorm) # Kolmogorov-Smirnov normalité. H0: normalité
qqnorm(residuals(fitAA))
qqline(residuals(fitAA))

# continuer les itérations selon nos décisions....

## Auto
mod.auto <- auto.arima(flux.train,
           allowdrift = FALSE,
           allowmean = FALSE)

prevs <- mod.auto %>% forecast(h = 24)

confint(mod.auto)

## Visualisation finale avec train-test
accuracy(prevs, flux.test)
autoplot(flux.train) + autolayer(prevs) + autolayer(flux.test)

## Intervalles de confiance par booststrap
set.seed(20231206)
nsim <- 100

IP_auto <- forecast(mod.auto, h = 24, level = 95, bootstrap = TRUE,
                     npaths = nsim, biasadj = FALSE)


### ARCH et GARCH
## On entraîne une modèle SARIMA (ou autre) bien normal
## Ensuite, on regarde la série résiduelle.
## Si elle a l'air d'un bruit blanc, il est possible que sa variance ne soit pas constante
## Donc il faut étudier

autoplot(resid(mod.auto))
Acf(resid(mod.auto))
Acf(resid(mod.auto)^2) # variance pas un bruit blanc

## On y ajuste un garch
mod.auto_garch <- garch(resid(mod.auto), trace = TRUE)
Acf(resid(mod.auto_garch))
Acf(resid(mod.auto_garch)^2) # Maintenant variance ok

## Ne sera utile qu'en simulation pour le paramètre de vairance du rnorm (bruit blanc)
## à utiliser dans la simulation du processus sous-jacent.



