#### Examen synthèse Capstone
### A3.12 - Modèle de tarification d'options de Black-Scholes
###

library(derivmkts)

# Notation ----
# s0 : prix du s-jacent à t = 0
# K : prix d'exercice
# sigma : volatilité
# alpha : rendement (r = alpha --> probabilité neutre au risque)
# tt : maturité (T)
# delt : dividend

# Lois importantes ----
# R(0,t) ~ N(mu, sigma^2 * t)
# St/S0 ~ LN(mu, sigma^2 * t)
# St ~ LN(ln(S0) + mu, sigma^2 * t)
# où mu = (alpha - delta - sigma^2/2) * t

# P[St <= K] et P[St >= K] ----
s0 = 40
K = 40
sigma = 0.3
alpha = 0.08
tt = 1/2
delt = 0

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

# P[St <= K]
PleqK = pnorm(-d2)

# valid
# plnorm(q = K,
#        meanlog = log(s0) + (alpha - delt - sigma^2/2) * tt, 
#        sdlog = sqrt(sigma^2 * tt)) # fonctionne

# P[St >= K]
PgeqK = pnorm(d2)

# valid
# plnorm(q = K,
#        meanlog = log(s0) + (alpha - delt - sigma^2/2) * tt,
#        sdlog = sqrt(sigma^2 * tt),
#        lower.tail = FALSE) # fonctionne

# Fonction quantile de St ----
q_St = function(u, s0, alpha, delt, sigma, tt){
  # F_St (u)
  
  mu = (alpha - delt - sigma^2/2) * tt
  
  # output
  s0 * exp(mu + qnorm(u) * sigma * sqrt(tt))
  
}

s0 = 40
alpha = 0.08
delt = 0
sigma = 0.3
tt = 1/2

q_St(u = 0.1,
     s0 = s0,
     alpha = alpha,
     delt = delt,
     sigma = sigma, 
     tt = tt)

# valid
# qlnorm(p = 0.1,
#        meanlog = log(s0) + (alpha - delt - sigma^2/2) * tt,
#        sdlog = sqrt(sigma^2 * tt)) # fonctionne

# Espérance et variance de St ----
s0 = 40
sigma = 0.3
alpha = 0.08
tt = 1/2
delt = 0
mu = (alpha - delt - sigma^2/2) * tt

espSt = s0 * exp(mu + (sigma^2 * tt)/2)

varSt = s0^2 * exp(2 * mu + sigma^2) * (exp(sigma^2) - 1)

# Espérance tronquée et conditionnelle de St ----
s0 = 40
K = 40
sigma = 0.3
alpha = 0.08
tt = 1/2
delt = 0

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

# E[St \times 1_{St <= K}]
esp_tronq_inf = s0 * exp((alpha - delt) * tt) * pnorm(-d1)

# E[St \times 1_{St >= K}]
esp_tronq_sup = s0 * exp((alpha - delt) * tt) * pnorm(d1)

# E[St | St <= K]
esp_cond_inf = esp_tronq_inf / pnorm(-d2)

# E[St | St >= K]
esp_cond_sup = esp_tronq_sup / pnorm(d2)

# Option (Call/Put) ----
# Note : les formules sont données pour une position longue. Pour une position 
# courte (vendeur de l’option), on inverse le signe.

## avec ou sans dividendes continus ----
s0 = 100
K = 90
sigma = 0.3
alpha = r = 0.08 # environnement neutre au risque
tt = 1
delt = 0.05

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_div_cont = s0 * exp(-delt * tt) * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_div_cont = K * exp(-r * tt) * pnorm(-d2) - s0 * exp(-delt * tt) * pnorm(-d1)

## avec dividendes discrets proportionnels ----
n = 1 # nombre de dividendes versé d'ici T
s0 = 50
K = 40
sigma = 0.3
alpha = 0.08
tt = 1/2
delt = 0

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_div_disc_prop = s0 * (1 - delt)^n * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_div_disc_prop  = K * exp(-r * tt) * pnorm(-d2) - s0 * (1 - delt)^n * pnorm(-d1)

## avec dividendes discrets fixes ----
dt = c(2) # vecteur des dividendes versés à des moments connus d’avances avant T
temps = c(1/365) # moment de versement des dividendes
s0 = 50
K = 40
sigma = 0.3
alpha = r = 0.08
tt = 1/2
delt = 0

# DIFFÉRENCE
d1 = (log((s0 - sum(dt * exp(-r * temps))) / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_div_disc_fix = (s0 - sum(dt * exp(-r * temps))) * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_div_disc_fix  = K * exp(-r * tt) * pnorm(-d2) - (s0 - sum(dt * exp(-r * temps))) * pnorm(-d1)

## sur devises ----
# Note : Si l'option est libellée en W, alors W est le taux domestique (r_dom).
x0 = 1/95
s0 = x0 # DIFFÉRENCE
K = 1/90 # IMPORTANT : toujours dans la devise domestique!
r_etr = 0.015
delt = r_etr # DIFFÉRENCE
r_dom = 0.035
r = alpha = r_dom # DIFFÉRENCE
sigma = 0.1
tt = 1/2

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_devise = s0 * exp(-delt * tt) * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_devise = K * exp(-r * tt) * pnorm(-d2) - s0 * exp(-delt * tt) * pnorm(-d1)

## sur futures ----
tt_prime = 1 # l’échéance du futures (tt_prime >= tt) (DIFFÉRENCE)
ss = 100 # DIFFÉRENCE
K = 105
sigma = 0.4
alpha = r = 0.06
tt = 1
delt_forward = 0
Fot_s = ss * exp((r - delt_forward) * tt_prime) # DIFFÉRENCE
delt = r # DIFFÉRENCE (pose delt = r après le calcul de Fot_s)
tt_prime >= tt # doit être TRUE
if (tt_prime == tt){
  print("Option européenne sur action = Option européenne sur forward car les maturités sont les mêmes")
}

# DIFFÉRENCE
d1 = log(Fot_s / K) / (sigma * sqrt(tt)) + sigma * sqrt(tt) / 2
d2 = d1 - sigma * sqrt(tt)

call_future = Fot_s * exp(-r * tt) * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_div_cont = K * exp(-r * tt) * pnorm(-d2) - Fot_s * exp(-r * tt) * pnorm(-d1)

## gap ----
# Note : Si K = H, c'est une option ordinaire.
s0 = 40
H = 30 # DIFFÉRENCE
K = 40
sigma = 0.3
alpha = r = 0.08
tt = 1/2
delt = 0

d1 = (log(s0 / H) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_gap = s0 * exp(-delt * tt) * pnorm(d1) - K * exp(-r * tt) * pnorm(d2)

put_gap = K * exp(-r * tt) * pnorm(-d2) - s0 * exp(-delt * tt) * pnorm(-d1)

## échange ----
# Note : alpha (ou r) n'apparait pas ici! C'est normal.
s_1 = 40
s_2 = 30
rho = 0.3
sigma_1 = 0.3 
sigma_2 = 0.4
tt = 1/2
sigma = sqrt((sigma_1^2 + sigma_2^2 - 2 * rho * sigma_1 * sigma_2) / tt)
delt_1 = 0
delt_2 = 0

d1 = (log(s_1 / s_2) + (delt_2 - delt_1 + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

call_echange = s_1 * exp(-delt_1 * tt) * pnorm(d1) - s_2 * exp(-delt_2 * tt) * pnorm(d2)

put_echange = s_2 * exp(-delt_2 * tt) * pnorm(-d2) - s_1 * exp(-delt_1 * tt) * pnorm(-d1)

## alternative ----
s0 = 40
K = 40
sigma = 0.3
alpha = r = 0.08
tt = 1
tc = 1/2 # le moment auquel on choisit si l'option est un call ou un put (tc < tt) (DIFFÉRENCE)
delt = 0
tc <= tt # doit être TRUE

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * tt)/(sigma * sqrt(tt))
d2 = d1 - sigma * sqrt(tt)

d1_prime = (log(s0 / K) + (alpha - delt + sigma^2/2) * tc)/(sigma * sqrt(tc)) # DIFFÉRENCE
d2_prime = d1_prime - sigma * sqrt(tc)

option_alternative = exp(-r * tt) * (s0 * exp(-delt * tt) * pnorm(d1) - 
                                       K * exp(-r * tt) * pnorm(d2) +
                                       K * exp(-r * tt) * pnorm(-d2_prime) -
                                       s0 * exp(-delt * tt) * pnorm(-d1_prime))

## déclenchement différé ----
# Note :  Le facteur exp(-r * td) dans les formules de pricing permet de 
# considérer le fait que l'option devient active seulement à td.
s0 = 40
K = 40
sigma = 0.3
alpha = r = 0.08 # environnement neutre au risque
tt = 1
td = 1/2 # valuation starts at td (td < tt) (DIFFÉRENCE)
delt = 0

d1 = (log(s0 / K) + (alpha - delt + sigma^2/2) * (tt - td))/(sigma * sqrt(tt - td))
d2 = d1 - sigma * sqrt(tt - td)

call_diff = exp(-r * td) * (s0 * exp(-delt * (tt - td)) * pnorm(d1) - K * exp(-r * (tt - td)) * pnorm(d2))

put_diff = exp(-r * td) * ( K * exp(-r * (tt - td)) * pnorm(-d2) - s0 * exp(-delt * (tt - td)) * pnorm(-d1))

# Obtention des paramètres ----
values <- read.csv2("DonnéesTPGRF2.csv") # prix du s-jacent par période de longueur j

delt <- 0 # dividend

j <- 1/12 # longueur des périodes (fraction d'année)

rdm_log <- log(values$St[-1] / values$St[-length(values$St)]) # rendements du s-jacent par période de longueur j

r_bar <- mean(rdm_log)  # moyenne échantillonnale du s-jacent (\Bar{r}) par période de longueur j

sdev <- sd(rdm_log) # écart-type échantillonnal du s-jacent (S_r) par période de longueur j

sigma_hat <- sqrt(sdev^2 / j) # écart-type annualisé

alpha_hat <- r_bar / j + delt + sdev^2 / (j * 2) # moyenne annulaisée

# Greeks ----
# Note 1 : Si nous sommes dans la position courte, simplement inverser le signe.
# Note 2 : Si on a une stratégie, on somme les greeks (greeks_strat = somme des greeks).
# Note 3 : Greeks d'un portefeuille = somme pondérée des greeks.
s0 = 40
K = 45
sigma = 0.3
alpha = r = 0.08
tt = 1/2
delt = 0

bsopt(s = s0,
      k = K,
      v = sigma,
      r = alpha,
      tt = tt,
      d = delt)

# Delta hedging ----
# Note : La fonction est faite pour une option, mais si on doit faire le hedging pour 
# une stratégie, on peut simplement utiliser la fonction sur chaque composante de 
# la stratégie et agréger les résultats. Faire attention de bien spécifier si on 
# achète ou on vend l'option dans une stratégie. Ex : Si on achète un bull(call, 40, 45),
# alors, on achète le call(40), mais on vend le call(45) car 
# bull(call, 40, 45) = call(40) - call(45). Pour un exemple numérique, voir le
# code : strategie_delta_hedging.R.

delta_hedging = function(vend_bool, call_bool, n, s0, K, sigma, alpha, tt, delt){

  if (call_bool){
    
    delta = data.frame(bsopt(s = s0,
                              k = K,
                              v = sigma,
                              r = alpha,
                              tt = tt,
                              d = delt)$Call)['Delta',]
    
    transige_actions = n * (-1)^vend_bool * delta # potitif = on vend; négatif = on achète
    
    inv_initial = -transige_actions * s0 # potitif = on paye; négatif = on reçoit
    
    premium = data.frame(bsopt(s = s0,
                               k = K,
                               v = sigma,
                               r = alpha,
                               tt = tt,
                               d = delt)$Call)['Premium',]
    
    transaction_option = (-1)^(vend_bool) * n * premium # potitif = on paye; négatif = on reçoit
    
    if (inv_initial < 0 & transaction_option < 0){
      # entrée de fonds : on fait un prêt
      pret = -(inv_initial + transaction_option)
      emprunt = 0
    } else if (inv_initial > 0 & transaction_option > 0){
      # sortie de fonds : on fait un emprunt
      pret = 0
      emprunt = inv_initial + transaction_option
    } else if (inv_initial < 0 & transaction_option > 0){
      # entrée de fonds + sortie de fonds
      pret = (abs(inv_initial) > abs(transaction_option)) * (abs(inv_initial) - abs(transaction_option))
      emprunt = (abs(inv_initial) < abs(transaction_option)) * (abs(transaction_option) - abs(inv_initial))
    } else {
      # sortie de fonds + entrée de fonds
      pret =  (abs(inv_initial) < abs(transaction_option)) * (abs(transaction_option) - abs(inv_initial))
      emprunt = (abs(inv_initial) > abs(transaction_option)) * (abs(inv_initial) - abs(transaction_option))
    }
    
  } else {
    
    delta = data.frame(bsopt(s = s0,
                             k = K,
                             v = sigma,
                             r = alpha,
                             tt = tt,
                             d = delt)$Put)['Delta',]
    
    transige_actions = n * (-1)^vend_bool * delta # positif = on achète; négatif = on vend
    
    inv_initial = -transige_actions * s0 # potitif = on paye; négatif = on reçoit
    
    premium = data.frame(bsopt(s = s0,
                               k = K,
                               v = sigma,
                               r = alpha,
                               tt = tt,
                               d = delt)$Put)['Premium',]
    
    transaction_option = (-1)^(vend_bool) * n * premium # potitif = on paye; négatif = on reçoit
    
    if (inv_initial < 0 & transaction_option < 0){
      # entrée de fonds : on fait un prêt
      pret = -(inv_initial + transaction_option)
      emprunt = 0
    } else if (inv_initial > 0 & transaction_option > 0){
      # sortie de fonds : on fait un emprunt
      pret = 0
      emprunt = inv_initial + transaction_option
    } else if (inv_initial < 0 & transaction_option > 0){
      # entrée de fonds + sortie de fonds
      pret = (abs(inv_initial) > abs(transaction_option)) * (abs(inv_initial) - abs(transaction_option))
      emprunt = (abs(inv_initial) < abs(transaction_option)) * (abs(transaction_option) - abs(inv_initial))
    } else {
      # sortie de fonds + entrée de fonds
      pret =  (abs(inv_initial) < abs(transaction_option)) * (abs(transaction_option) - abs(inv_initial))
      emprunt = (abs(inv_initial) > abs(transaction_option)) * (abs(inv_initial) - abs(transaction_option))
    }
    
  }
  
  # output
  list(delta_option = round(delta, 4),
       prix_option = round(premium, 4),
       achat_ou_vente_action = paste0('On ', ifelse(transige_actions > 0, 'vend ', 'achète '), 
                                      round(abs(transige_actions), 4), ' actions pour ', round(abs(inv_initial), 4), ' $.'),
       achat_ou_vente_option = paste0('On ', ifelse(vend_bool, 'vend ', 'achète '), 
                                      n, ifelse(call_bool, ' calls', ' puts'),' pour ', round(abs(transaction_option), 4), 
                                      ' $.'),
       emprunt_ou_pret = paste0('On prête ', round(pret, 4), ' $ et on emprunte ', round(emprunt, 4), ' $.'))
  
}

s0 = 40
K = 45
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
vend_bool = TRUE # vend (TRUE); achète (FALSE)
call_bool = TRUE # call (TRUE); put (FALSE)
n = 100 # l'option est sur combien d'actions

delta_hedging(vend_bool = vend_bool,
              call_bool = call_bool,
              n = n,
              s0 = s0,
              K = K,
              sigma = sigma,
              alpha = alpha,
              tt = tt,
              delt = delt)










