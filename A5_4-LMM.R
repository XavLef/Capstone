#### Examen synthèse Capstone
### A5.4 - Modèles linéaires mixtes
### Modèles linéaires mixtes
###

# Paquetages ----
liste.paquetage <- c("MASS", "ISLR2", "ggplot2",
                     "car", "GGally", "leaps",
                     "glmnet", "FactoMineR", "pls",
                     "WWGbook", "nlme")

inst <- liste.paquetage %in% installed.packages()
if(length(liste.paquetage[!inst]) > 0){
  install.packages(liste.paquetage[!inst])
}

for (i in liste.paquetage){
  suppressMessages((library(i, character.only = TRUE)))
}

library(CASdatasets)

# RatPup (2 levels) ----

data("ratpup")
str(ratpup)

summary(ratpup)
aggregate(weight ~ treatment + sex, data = ratpup, FUN = summary)
boxplot(weight ~ treatment + sex, data = ratpup)

attach(ratpup)
ratpup$sex1[sex == "Female"] <- 1
ratpup$sex1[sex == "Male"] <- 0

## Mod. 3.1 initial ----
model3.1.fit <- lme(weight ~ treatment*sex1 + litsize,
                    random = ~ 1 | litter, 
                    data = ratpup, 
                    method = "REML")

summary(model3.1.fit)
anova(model3.1.fit) # tests F de type 1. treatment not conditional on sex,
                    # sex is conditional on treatment.

random.effects(model3.1.fit) # EBLUPs

## Mod 3.1A random fx? ----
model3.1.a.fit <- gls(weight ~ treatment*sex1 + litsize, 
                      data = ratpup)

anova(model3.1.a.fit, model3.1.fit) # rejet H0, donc random fx ok.

## Mod 3.2A hetero residual ----
model3.2a.fit <- lme(weight ~ treatment*sex1 + litsize,
                     random = ~ 1  | litter,
                     data = ratpup,
                     method = "REML",
                     weights = varIdent(form = ~ 1  | treatment)) #hetero resid variance given treatment

summary(model3.2a.fit)

anova(model3.1.fit, model3.2a.fit) # rejet H0 --> garder variance hétérogène

## Model 3.2B group low/high ----
# regrouper les niveaux pour voir la comparaison
ratpup$trtgrp[treatment == "Control"] <- 1
ratpup$trtgrp[treatment == "Low" | treatment == "High"] <- 2

model3.2b.fit <- lme(weight ~ treatment + sex1 + litsize +
                     treatment*sex1, random = ~1 | litter,
                     data = ratpup,
                     method = "REML",
                     weights = varIdent(form = ~1 | trtgrp))

anova(model3.2b.fit, model3.2a.fit) # ne rejette pas, on combine!

anova(model3.1.fit, model3.2b.fit) # garder hétérogène

summary(model3.2b.fit)

## Model 3.3 del effets fixes ----
anova(model3.2b.fit, type = "sequential") # test F type I
anova(model3.2b.fit, type = "marginal") # test F type III
# on enlève treatment:sex1

model3.3.reml.fit <- lme(weight ~ sex1 + litsize + treatment,
                         random = ~1 | litter, ratpup, 
                         method = "REML",
                         weights = varIdent(form = ~1 | trtgrp))

summary(model3.3.reml.fit)

anova(model3.3.reml.fit)

## ICC ----
getVarCov(model3.3.reml.fit, individuals = "3", type = "random.effects")
getVarCov(model3.3.reml.fit, individuals = "3", type = "conditional")
getVarCov(model3.3.reml.fit, individuals = "3", type = "marginal")

## Prediction ----
predict(model3.3.reml.fit, ratpup, level = 0) # unconditional (population-average, marginal)
predict(model3.3.reml.fit, ratpup, level = 1) # conditional (litter-specific)
random.effects(model3.3.reml.fit)

## Diagnostics ----
### résidus ----
res_mod <- residuals(model3.3.reml.fit)
hist(res_mod)
aggregate(res_mod ~ trtgrp, data = cbind(res_mod, ratpup), FUN = hist)
aggregate(res_mod ~ trtgrp, data = cbind(res_mod, ratpup), FUN = function(a) shapiro.test(a)$p.value)
# rejet de la normalité

plot(model3.3.reml.fit, resid(., type="p") ~ fitted(.) |
         factor(treatment), layout=c(3,1), aspect=2, abline=0)

plot(model3.3.reml.fit, resid(., type="p") ~ fitted(.) |
         factor(treatment), id = 0.05, layout=c(3,1), aspect=2, abline=0)

plot(model3.3.reml.fit, resid(.) ~ sex1, abline=0)
# variance constante pour les valeurs de sexe

qqnorm(model3.3.reml.fit, ~resid(.) | factor(treatment),
       layout=c(3,1), aspect = 2, id = 0.05)

### effets aléatoires (on regarde leur normalité) ----
qqnorm(model3.3.reml.fit, ~ranef(.), id = 0.10)

### observed vs predicted ----
plot(model3.3.reml.fit, weight ~ fitted(.) | factor(treatment), id =
         0.05, layout=c(3,1), aspect=2) #id pour les outliers (seuil de 0.05)

# Classroom (3 levels) ----
data("classroom")
str(classroom)
summary(classroom)

attach(classroom)
boxplot(mathgain ~ classid + schoolid)

## Model 4.1 intercepts ----
model4.1.fit <- lme(mathgain ~ 1,
                    random = ~1 | schoolid/classid,
                    data = classroom,
                    method = "REML")

summary(model4.1.fit)

model4.1A.fit <- lme(mathgain ~ 1, 
                     random = ~1 | schoolid,
                     data = classroom, 
                     method = "REML")

anova(model4.1A.fit, model4.1.fit) # on rejette, donc on garde random sur class
## DIVISER CETTE PVAL PAR 2

## Model 4.2 fixes niveau 1 ----
model4.2.fit <- lme(mathgain ~ mathkind + sex + minority + ses,
                    random = ~1 | schoolid/classid,
                    data = classroom,
                    na.action = "na.omit", 
                    method = "REML")

summary(model4.2.fit)

# ANOVA --> faut prendre method = "ML", car on teste ajout effets FIXES
model4.1.ml.fit <- lme(mathgain ~ 1,
                       random = ~1 | schoolid/classid, 
                       classroom,
                       method = "ML")

model4.2.ml.fit <- lme(mathgain ~ mathkind + sex + minority + ses, 
                       random = ~1 | schoolid/classid,
                       classroom,
                       na.action = "na.omit", 
                       method = "ML")

anova(model4.1.ml.fit, model4.2.ml.fit) # on va ajouter niveaux 1

## Model 4.3 fixes niveau 2 ----
model4.3.fit <- update(model4.2.fit,
                       fixed = ~ mathkind + sex + minority + ses + 
                         yearstea + mathprep + mathknow)

summary(model4.3.fit) # on doit utiliser test t, car mathknow a des NA
# aucune des effets n'est significatif, donc on ne les met pas

## Model 4.4 fixes niveau 3 ----
model4.4.fit <- update(model4.2.fit,
                       fixed = ~ mathkind + sex + minority + ses + housepov) 
# depuis 4.2, car on n'inclut pas les effets de 4.3

summary(model4.4.fit) # effet non significatif, puis on enlève aussi sex.

## Prediction ----
predict(model4.2.fit, classroom, level = 0)
predict(model4.2.fit, classroom, level = 1)
qqnorm(unlist(ranef(model4.2.fit)))
qqline(unlist(ranef(model4.2.fit)))

# Rat brain (repeated measure) -----
data("rat.brain")
str(rat.brain)

attach(rat.brain)
region.f <- region
region.f[region == 1] <- 1
region.f[region == 2] <- 2
region.f[region == 3] <- 0
region.f <- factor(region.f)
treat <- treatment
treat[treatment == 1] <- 0
treat[treatment == 2] <- 1
rat.brain <- data.frame(rat.brain, region.f, treat)

## Model 5.1 initial ----
model5.1.fit <- lme(activate ~ region.f*treat,
                    random = ~1 | animal, 
                    method = "REML", 
                    data = rat.brain)

summary(model5.1.fit)
anova(model5.1.fit)

## Model 5.2 random treat ----
model5.2.fit <- update(model5.1.fit, random = ~treat | animal)

summary(model5.2.fit)
anova(model5.2.fit)
0.5*(1 - pchisq(26.1,1)) + 0.5*(1 - pchisq(26.1,2))

## Model 5.3 Structure cov ----
model5.3.fit <- lme(activate ~ region.f*treat,
                    random = ~treat | animal,
                    weights = varIdent(form = ~1 | treat),
                    data = rat.brain)

summary(model5.3.fit)
anova(model5.2.fit, model5.3.fit) # trop copmlexe, on garde 5.2

# Autism (Longitudinal) ----
data("autism")
attach(autism)
str(autism)
sicdegp.f <- factor(sicdegp)
age.f <- factor(age)
autism.updated <- data.frame(autism, sicdegp.f, age.f)

age.2 <- age - 2
age.2sq <- age.2*age.2

sicdegp2 <- sicdegp
sicdegp2[sicdegp == 3] <- 0
sicdegp2[sicdegp == 2] <- 2
sicdegp2[sicdegp == 1] <- 1
sicdegp2.f <- factor(sicdegp2)

autism.updated <- subset(data.frame(autism, sicdegp2.f, age.2),
                         !is.na(vsae))

## Structure hiréarchique ----
autism.grouped <- groupedData(vsae ~ age.2 | childid,
                              data = autism.updated,
                              order.groups = F)

## Mod 6.1 initial ----
model6.1.fit <- lme(vsae ~ age.2 + I(age.2^2) + sicdegp2.f +
                        age.2:sicdegp2.f + I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2), 
                    method="REML",
                    data = autism.grouped) # ne converge pas

## Mod 6.2 plus facile ----
model6.2.fit <- lme(vsae ~ age.2 + I(age.2^2) + sicdegp2.f +
                        age.2:sicdegp2.f + I(age.2^2):sicdegp2.f,
                    random = ~ age.2 + I(age.2^2) - 1, 
                    method="REML",
                    data = autism.grouped)

summary(model6.2.fit)

model6.2a.fit <- update(model6.2.fit, random = ~ age.2 - 1)
(h6.1.pvalue <- 0.5*(1-pchisq(83.9,1)) + 0.5*(1-pchisq(83.9,2)))

## Mod 6.3 réduire ----
model6.2.ml.fit <- update(model6.2.fit, method = "ML")
model6.3.ml.fit <- update(model6.2.ml.fit,
                          fixed = ~ age.2 + I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f)

anova(model6.2.ml.fit, model6.3.ml.fit) # on garde 6.2 pas d'int.

anova(model6.2.fit) # enlever interaction

model6.3.fit <- update(model6.2.fit,
                       fixed = ~ age.2 + I(age.2^2) + sicdegp2.f + age.2:sicdegp2.f)

summary(model6.3.fit)
anova(model6.3.fit)

# Veneer (clustered longit) ----
data("veneer")
attach(veneer)
str(veneer)

## Model 7.1 initial ----
model7.1.fit <- lme(gcf ~ time + base.gcf + cda + age +
                        time:base.gcf + time:cda + time:age,
                    random = list(patient = ~ time, tooth = ~1),
                    data = veneer,
                    method = "REML")

## Model 7.1 random fx ----
model7.1A.fit <- lme(gcf ~ time + base.gcf + cda + age +
                         time:base.gcf + time:cda + time:age,
                     random = list(patient = ~ time),
                     data = veneer, 
                     method = "REML")

anova(model7.1A.fit, model7.1.fit) # Faut garder tooth

## Model 7.2 structure covariance ----
model7.2A.fit <- lme(gcf ~ time + base.gcf + cda + age +
                         time:base.gcf + time:cda + time:age,
                     random = list(patient = ~ time, tooth = ~1),
                     corr = corCompSymm(0.5, form=~1 | patient/tooth),
                     weights = varIdent(form = ~1 | time),
                     data = veneer, 
                     method =  "REML")

summary(model7.2A.fit) # corCompSymm = compound symetric structure
intervals(model7.2A.fit)

model7.2B.fit <- lme(gcf ~ time + base.gcf + cda + age +
                         time:base.gcf + time:cda + time:age,
                     random = list(patient = ~ time, tooth = ~1),
                     corr=corCompSymm(0.5, form=~1 | patient/tooth),
                     data = veneer, 
                     method = "REML")

model7.2C.fit <- lme(gcf ~ time + base.gcf + cda + age +
                         time:base.gcf + time:cda + time:age,
                     random = list(patient = ~ time, tooth = ~1),
                     weights = varIdent(form = ~1 | time),
                     data = veneer, 
                     method = "REML")

anova(model7.2C.fit, model7.1.fit) # on garde 7.1

## Model 7.3 effets fixes ----
# on doit transférer au ML
model7.1.ml.fit <- lme(gcf ~ time + base.gcf + cda + age +
                           time:base.gcf + time:cda + time:age,
                       random = list(patient = ~ time, tooth = ~1),
                       data = veneer,
                       method = "ML")

model7.3.ml.fit <- lme(gcf ~ time + base.gcf + cda + age,
                       random = list(patient = ~ time, tooth = ~1),
                       data = veneer, 
                       method = "ML")

anova(model7.1.ml.fit, model7.3.ml.fit) #  on garde 7.3

## NE PAS OUBLIER DE REVENIR À REML POUR ESTIMATIONS NON BIAISÉES
model7.3.fit <- lme(gcf ~ time + base.gcf + cda + age,
                      random = list(patient = ~ time, tooth = ~1),
                      data = veneer, 
                    method = "REML")

summary(model7.3.fit)

