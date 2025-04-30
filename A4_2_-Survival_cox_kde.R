### Capstone
## A4.2 - Esimtateurs de fonction de survie

liste.paquetage <- c("survival", "survminer", "KernSmooth",
                     "muhaz", "km.ci", "mice")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

# data(lung)
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
plot(fit_km)
ggsurvplot(fit_km_tunc_cens, title = "Kaplan-Meier", conf.int = TRUE)
ggsurvplot(fit_km, title = "Kaplan-Meier", conf.int = TRUE)

fit_sex <- survfit(surv_obj ~ sex, data = lung) # Une courbe par sexe
plot(fit_sex, xlab = "Months",
     ylab = "Esimtated probability of survival", col = c(2, 4))
legend("bottomleft", levels(factor(lung$sex)), col = c(2, 4), lty = 1)

# survie à un point précis:
S_hat <- summary_fit$surv
summary(fit_km_tunc_cens, times = 15.2) # pour évaluer en un point précis
var_greenwood <- summary_fit$std.err^2


# Log-rank test to compare survival of males to females
attach(lung)
logrank.test <- survdiff(Surv(time, status) ~ sex)
logrank.test
# H0: il n'y a pas de différence significative entre les deux fct de survie (par sexe)

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

res.cox <- coxph(Surv(time, status) ~ factor(sex), data = lung)
res.cox

summary(res.cox)
## Le sexe 2 a exp(-0.5310) plus de risques de survie que le sexe 1.

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

plot(fit, xlab = "Months",
     ylab = "Survival Probability", col = 2:5)
legend("bottomleft", levels(factor(sex)), col = 2:5, lty =1)
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


# Nelson-Aalen ----
time <- c(500, 800, 1200, 1700) # y_j (événements : montant ou temps)
event <- c(3, 10, 11, 2) # s_j (nb. événements)
riskset <- c(52, 40, 19, 6) # r_j (nb. pers. dans l'échantillon)

## Estimation du cumulative hazard ----
h_increment <- event / riskset

H_hat <- cbind(time, H_time = cumsum(h_increment))

## Survie ----
S_hat <- cbind(time, S_time = exp(-H_hat[,2]))

# Kaplan-Maier ----
time <- c(300, 500, 700, 900, 1200, 1300, 1400) # y_j (événements : montant ou temps)
event <- c(1, 1, 2, 1, 1, 1, 1) # s_j (nb. événements)
riskset <- c(6, 5, 9, 7, 3, 2, 1) # r_j (nb. pers. dans l'échantillon)

## Survie ----
S_hat <- cbind(time, S_time = cumprod(1 - event / riskset))

## Corrections ----
y_max = 50
gamma_cor = 60

### Effron ----
S_hat_correct = rbind(S_hat,
                      c(y_max, 0))

### Klein et Moeschberger ----
S_hat_correct = rbind(S_hat,
                      c(y_max, S_hat[nrow(S_hat), 2]),
                      c(gamma_cor, 0))

### Exponentielle----
S_hat_correct = function(y) {S_hat[nrow(S_hat), 2]^(y / y_max)} # pour y >= y_max








