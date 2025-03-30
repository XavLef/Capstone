### Capstone

## Références pour le Cox proportional hazards model
# https://www.sthda.com/english/wiki/cox-proportional-hazards-model
# validation des résidus et hypothèses du modèle
# https://www.sthda.com/english/wiki/cox-model-assumptions


## Cox pour données individuelles

library(survival)
library(KernSmooth)

data(cancer)

lung <- cancer

cox_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog, data = lung)

summary(cox_model)

## Cox pour données groupées

lung_grouped <- survSplit(Surv(time, status) ~ ., data = lung,
                          cut = seq(0, max(lung$time), by=100),
                          episode="interval")

cox_grouped_model <- coxph(Surv(time, status) ~ age + sex + ph.ecog + interval,
                           data = lung_grouped)

summary(cox_grouped_model)

## Estimateur à noyaux pour données individuelles


survival_times <- lung$time[lung$status == 1]

density_est <- bkde(survival_times, bandwidth = 50)
plot(density(survival_times))

plot(density_est$x, density_est$y, type="l", col="blue", lwd=2,
     xlab="Time", ylab="Density",
     main="Kernel Density Estimate of Survival Times")

## Estimateur à noyau pour données groupées

# Points milieux des intervalles
intervals <- seq(0, max(lung$time) + 100, by=100)
midpoints <- (head(intervals, -1) + tail(intervals, -1)) / 2

event_counts <- hist(lung$time[lung$status == 1],
                     breaks=intervals, plot=FALSE)$counts

density_grouped <- density(midpoints,
                           weights = event_counts / sum(event_counts),
                           width = 50)


plot(density_grouped$x, density_grouped$y, type="l", col="red", lwd=2,
     xlab="Time", ylab="Density", main="Kernel Density Estimate (Grouped Data)")
