### Capstone
## A4.2 - Esimtateurs de fonction de survie

library(survival)
library(survminer)
library(KernSmooth)
library(muhaz)
library(km.ci)
library(mice)

data(lung)
lung <- na.omit(lung)
# dans le jeu de données:
# - time: survival time in days
# - status: censoring? 1 = censored, 2 = dead
# - Age in years
# - sex, male = 1



# Simuler toy dataset
set.seed(223)
n <- 100
entry <- runif(n, 0, 5)            # Entrée
event_time <- entry + rexp(n, 0.2) # Temps évéement
censored <- rbinom(n, 1, 0.2)
event <- 1 - censored              # 1 = non censuré

# Observé entry < time
data_sim <- data.frame(entry = entry, time = event_time, event = event)
data_sim <- subset(data_sim, time > entry)

# Objet "Survival" avec censure et troncation à gauche (comme une franchise)
surv_lt <- Surv(time = data_sim$entry, time2 = data_sim$time, event = data_sim$event)
# Création d'un objet survival
# Sera utilisé comme variable réponse des modèles
# time = moment d'observation pour données censurées à droite
# time = pour des intervalles, point de départ de l'intervalle
# event: 0 =  alive (censored), 1 = dead (Event at time)
# type = type de censure (le plus souvent inutile, car "right" si time2 est absent)
# ** attention de bien nommer les arguments, car le traitement est différent selon


## Objet survival avec uniquement censure à droite (limite sans franchise)
surv_obj <- Surv(time = lung$time, event = lung$status == 2)


## Estimateur de Kaplan-Meier  (Faudrait juste valider les s_i et r_i)
fit_km_tunc_cens <- survfit(surv_lt ~1, data = data_sim)
fit_km <- survfit(surv_obj ~ 1, data = lung)
summary_fit <- summary(fit_km_tunc_cens)
summary(fit_km)

ggsurvplot(fit_km_tunc_cens, title = "Kaplan-Meier", conf.int = TRUE)
ggsurvplot(fit_km, title = "Kaplan-Meier", conf.int = TRUE)

# survie à un point précis:
S_hat <- summary_fit$surv
summary(fit_km_tunc_cens, times = 15.2) # pour évaluer en un point précis
var_greenwood <- summary_fit$std.err^2

## Méthode Delta:
g_prime <- 1 / (S_hat * log(S_hat))
var_loglog <- g_prime^2 * var_greenwood

z <- qnorm(0.975)
lower <- exp(-exp(log(-log(S_hat)) - z * sqrt(var_loglog)))
upper <- exp(-exp(log(-log(S_hat)) + z * sqrt(var_loglog)))
plot(fit_km_tunc_cens, mark.time = FALSE, main = "KM")
lines(summary_fit$time, lower, col = "blue", lty = 2)
lines(summary_fit$time, upper, col = "blue", lty = 2)
legend("bottomleft", legend = "Delta", col = "blue", lty = 2)

# pour des données groupées avec Kaplan-Meier, on peut utiliser le
# point milieu des groupes et exprimer en individuel.


## Estimateur de Nelson-Aalen
fit <- survfit(surv_lt ~ 1) # comme kalpan-meier, on va utiliser les s_i et r_i

naest1 <- cumsum(fit$n.event/fit$n.risk)

plot(naest1, xlab = "Temps", ylab = "Cumulative Hazard",
     cex.lab = 1.5, cex.axis = 1.5, lwd = 2, col = "black", type = "s")

var_naa <- cumsum(fit$n.event * (fit$n.risk - fit$n.event) / fit$n.risk^3)

# méthode delta
var_survie <- exp(-naest1)^2 * var_naa

# avec des données groupées, on peut quand même calculer les ri et les si. prendre
# le point milieu (assume que c'est bon en moyenne, car postulat uniformément distribué)

## Cox pour données individuelles
# https://www.sthda.com/english/wiki/cox-proportional-hazards-model
data("lung")

res.cox <- coxph(Surv(time, status) ~ sex, data = lung)
res.cox

summary(res.cox)


res.cox <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data =  lung)
summary(res.cox)

ggsurvplot(survfit(res.cox, data = lung), color = "#2E9FDF",
           ggtheme = theme_minimal())


sex_df <- with(lung,
               data.frame(sex = c(1, 2),
                          age = rep(mean(age, na.rm = TRUE), 2),
                          ph.ecog = c(1, 1)
               )
)
sex_df

fit <- survfit(res.cox, newdata = sex_df)
ggsurvplot(fit, conf.int = TRUE, legend.labs=c("Sex=1", "Sex=2"),
           ggtheme = theme_minimal(), data = sex_df)


# vérification des postulats

test.ph <- cox.zph(res.cox)
test.ph
ggcoxzph(test.ph) # on veut voir des lignes presque horizontales (pour que ce soit bien proportionnel)




ggcoxdiagnostics(res.cox, type = "dfbeta",
                 linear.predictions = FALSE, ggtheme = theme_bw())



ggcoxdiagnostics(res.cox, type = "deviance",
                 linear.predictions = FALSE, ggtheme = theme_bw())
# Pour les résidus de déviance :
# Positive values correspond to individuals that “died too soon” compared to expected survival times.
# Negative values correspond to individual that “lived too long”.
# Very large or small values are outliers, which are poorly predicted by the model.


ggcoxfunctional(Surv(time, status) ~ age + log(age) + sqrt(age), data = lung)

## Cox pour données groupées

# même idée que les modèles précédents. On expand les groupes avec le point milieu.


## Estimateur à noyaux pour données individuelles

survival_times <- lung$time[lung$status == 1]

density_est <- bkde(survival_times, bandwidth = 50)
plot(density(survival_times))

plot(density_est$x, density_est$y, type="l", col="blue", lwd=2,
     xlab="Time", ylab="Density",
     main="Kernel Density Estimate of Survival Times")

## Juste density marche bien, donc au choix.

## Estimateur à noyau pour données groupées

# Points milieux des intervalles
intervals <- seq(0, max(lung$time) + 100, by=100)
midpoints <- (head(intervals, -1) + tail(intervals, -1)) / 2

event_counts <- hist(lung$time[lung$status == 1],
                     breaks=intervals, plot=FALSE)$counts

density_grouped <- density(midpoints,
                           weights = event_counts / sum(event_counts),
                           width = 100)


plot(density_grouped$x, density_grouped$y, type="l", col="red", lwd=2,
     xlab="Time", ylab="Density", main="Kernel Density Estimate (Grouped Data)")

