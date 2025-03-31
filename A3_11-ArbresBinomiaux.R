#### Examen synthèse Capstone
### A3.11 - Modèles binomiaux d’évaluation - arbres binomiaux
###

# Fonction pour dessiner l'arbre binomial ----
## Note: Comme la fonction est reprise de façon identique (ou presque) à la fonction
## binomplot, on peut se référer à la documentation de cette fonction pour comprendre
## les arguments. Ceux-ci sont inchangés.
binomplotdc <- function (s, k, v, r, tt, d, nstep, putopt = FALSE, american = TRUE,
                         plotvalues = FALSE, plotarrows = FALSE,
                         drawstrike = TRUE,
                         pointsize = 4, ylimval = c(0, 0),
                         saveplot = FALSE, saveplotfn = "binomialplot.pdf",
                         crr = FALSE, jarrowrudd = FALSE, titles = TRUE,
                         specifyupdn = FALSE,
                         up = 1.5, dn = 0.5, returnprice = FALSE, logy = FALSE)
{
  setylim <- ifelse((sum(ylimval^2) == 0), FALSE, TRUE)
  y <- binomopt(s, k, v, r, tt, d, nstep, american, putopt,
                specifyupdn, crr, jarrowrudd, up, dn, returnparams = TRUE,
                returntrees = TRUE)
  h <- tt/nstep
  for (i in c("up", "dn", "p")) assign(i, y$params[i])
  for (i in c("stree", "exertree", "oppricetree", "probtree")) assign(i,
                                                                      y[["i"]])
  nn <- 0:nstep
  payoffmult <- ifelse((putopt), -1, 1)
  stree <- y$stree
  dtree <- cbind(rbind(y$deltatree, numeric(dim(y$deltatree)[2])),
                 numeric(dim(y$stree)[1]))
  btree <- cbind(rbind(y$bondtree, numeric(dim(y$bondtree)[2])),
                 numeric(dim(y$stree)[1]))
  exertree <- y$exertree
  oppricetree <- y$oppricetree
  probtree <- y$probtree
  plotcolor <- ifelse(exertree, "green3", "red")
  if (saveplot)
    pdf(saveplotfn)
  ylim_default <- c(0, max(stree) * 1.08)
  savepar <- par(no.readonly = TRUE)
  if (logy)
    par(ylog = TRUE)
  plot(rep(nn, nn + 1) * h, stree[stree > 0], ylim = ifelse(c(setylim,
                                                              setylim), ylimval,
                                                            c(min(stree[stree > 1] - 0.95), max(stree) *
                                                                1.03)), col = plotcolor[stree > 0],
       pch = 21, cex = ifelse(stree[stree >
                                      0], sqrt(probtree[stree > 0]) * pointsize, 1), bg = plotcolor[stree >
                                                                                                      0], xlab = "Période binomiale", ylab = "Prix de l'action",
       main = paste("Arbre binomial à 4 périodes
    pour l'option", ifelse(putopt, "de vente", "d'achat"),
                    ifelse(american, "américaine", "européenne")),
       log = ifelse(logy, "y", ""),
       xlim = c(0 - h * 0.2, tt + h * 0.2),
       xaxt = "n")
  axis(1, at = c(0, 0.25, 0.5, 0.75, 1))
  
  if (titles)
    mtext(paste0("Stock = ", format(s, digits = 3), ", Strike = ",
                 format(k, digits = 3), ", Time = ", format(tt, digits = 4),
                 ifelse(tt == 1, " year,", " years,"), " Price = ",
                 format(oppricetree[1, 1], digits = 5)))
  if (drawstrike)
    abline(h = k)
  yoffset <- ifelse(setylim, 0.03 * (ylimval[2] - ylimval[1]),
                    0.03 * max(stree))
  if (plotarrows) {
    for (i in 1:nstep) {
      for (j in 1:i) {
        arrows((i - 1) * h, stree[j, i], c(i, i) * h,
               c(stree[j, i + 1], stree[j + 1, i + 1]), length = 0.06)
      }
    }
  }
  if (plotvalues) {
    for (i in 1:(nstep + 1)) {
      text((i - 1) * h, stree[1:i, i] + yoffset,
           paste0("St = ", format(stree[1:i,
                                        i], digits = 3)), cex = 0.6,
           col = "blue", font = 4)
      text((i - 1) * h, stree[1:i, i]  + yoffset - 3, paste0("Delta = ",
                                                             format(dtree[1:i,
                                                                          i], digits = 3)), cex = 0.5,
           font = 2)
      text((i - 1) * h, stree[1:i, i] + yoffset - 6, paste0("B = ",
                                                            format(btree[1:i,
                                                                         i], digits = 3)), cex = 0.5,
           font = 2)
      text((i - 1) * h, stree[1:i, i] + yoffset - 9,
           paste0(ifelse(putopt, "P =", "C ="),
                  format(oppricetree[1:i,
                                     i], digits = 3)), cex = 0.5,
           font = 2)
    }
  }
  if (saveplot)
    dev.off()
  if (returnprice)
    return(oppricetree[1, 1])
}

# Obtention des paramètres: Taux sans risque (rf) ----
rates <- read.csv("bonsTresor1an.csv", header = TRUE, skip = 10)

# rates : risk free rates (ex : si 3 % alors rates = 3, pas 0.03)
rf <- log(1 + mean(rates$V122498 / 100)) # mean(rates$V122498 / 100) est le rf 
# moyen et log(1 + x) donne la version annuelle composée continuement

# Obtention des paramètres: Volatilité (sigma) ----
values <- read.csv2("DonnéesTPGRF2.csv") # prix du s-jacent par période de longueur j

j <- 1/12 # longueur des périodes (fraction d'année)

rdm_log <- log(values$St[-1] / values$St[-length(values$St)]) # rendements du s-jacent

sdev <- sd(rdm_log) # écart-type échantillonnal du s-jacent (S_r) par période de longueur j

sigma <- sdev * sqrt(1 / j) # sigma = sqrt(S_r^2 / j) = S_r * sqrt(1/j) (on trouve l'écart-type annualisé)

c(rf, sigma)

# Option (Call/Put) ----
s0 = 100 # prix du s-jacent à t = 0
K = 95 # prix d'exercice
sigma = 0.3 # fixé selon les informations ou trouvé dans "Obtention des paramètres: Volatilité (sigma)"
rf = 0.08 # fixé selon les informations ou trouvé dans "Obtention des paramètres: Taux sans risque (rf)"
tt = 1 # maturité (T)
nn = 2 # nombre de pas (n)
# rappel : h = T / n
delt = 0 # dividend
american_bool = TRUE # européen (FALSE) ou américain (TRUE)
putopt_bool = TRUE # call (FALSE) ou put (TRUE)

up_and_down_moves = TRUE # u et d sont donnés (TRUE); FALSE sinon (ignore up et down)
up = 1.3
down = 0.8

option <- binomopt(s = s0,
                   k = K,
                   v = sigma, # (annualized standard deviation of the continuously-compounded return)
                   r = rf, # (annual continuously-compounded risk-free interest rate)
                   tt = tt,
                   nstep = nn,
                   d = delt, # (annualized, continuously-compounded)
                   american = american_bool,
                   putopt = putopt_bool,
                   specifyupdn = up_and_down_moves,
                   up = up,
                   dn = down,
                   # ne pas toucher les autres arguments
                   returntrees = TRUE, 
                   returnparams = TRUE,
                   returngreeks = TRUE)

option$price # prix de l'option
option$params["up"] # u
option$params["dn"] # d

## Graphique assopcié ----
binomplotdc(s = s0,
            k = K, 
            v = sigma, 
            r = rf, 
            tt = tt,
            nstep = nn,
            d = delt, 
            american = american_bool, 
            putopt = putopt_bool,
            specifyupdn = up_and_down_moves,
            up = up,
            dn = down,
            # ne pas toucher les autres arguments
            drawstrike = T,
            plotvalues = T, 
            plotarrows = F,
            ylimval = c(60, 160), 
            pointsize = 0, 
            titles = F)




