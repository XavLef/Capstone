#### Examen synthèse Capstone
### A3.13 - Grecques des options et gestion du risque
###

library(derivmkts)

# Notation ----
# s0 : prix du s-jacent à t = 0
# K : prix d'exercice
# sigma : volatilité
# alpha : rendement (r = alpha --> probabilité neutre au risque)
# tt : maturité (T)
# delt : dividend


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

# Approximation Delta-Gamma-Theta ----
approx = function(st, h, st_plus_h, K, sigma, r, tt, delt, call_bool, gamma_bool, theta_bool){
  
  if (gamma_bool == FALSE & theta_bool == TRUE) {
    
    stop("Si on fait l'approximation Theta, on doit également faire 
         l'approximation Gamma; Les choix sont Delta, Delta-Gamma ou 
         Delta-Gamma-Theta")
    
  }
  
  if (call_bool){
    
    prem_delta = data.frame(bsopt(s = st,
                                  k = K,
                                  v = sigma,
                                  r = alpha,
                                  tt = tt,
                                  d = delt)$Call)[c('Premium', 'Delta', 'Gamma', 'Theta'),]
    
    true_value = data.frame(bsopt(s = st_plus_h,
                                  k = K,
                                  v = sigma,
                                  r = alpha,
                                  tt = tt - h / 365,
                                  d = delt)$Call)['Premium',]
    
  } else {
    
    prem_delta = data.frame(bsopt(s = st,
                                  k = K,
                                  v = sigma,
                                  r = alpha,
                                  tt = tt,
                                  d = delt)$Put)[c('Premium', 'Delta', 'Gamma', 'Theta'),]
    
    true_value = data.frame(bsopt(s = st_plus_h,
                                  k = K,
                                  v = sigma,
                                  r = alpha,
                                  tt = tt - h / 365,
                                  d = delt)$Put)['Premium',]
    
  }
  
  option_st_h = prem_delta[1] + 
    (st_plus_h - st) * prem_delta[2] + 
    gamma_bool * (st_plus_h - st)^2 * prem_delta[3] / 2 +
    theta_bool * h * prem_delta[4]
  
  # output : C(S_{t+h})
  list(option_st = round(prem_delta[1], 4), # prix initial de l'option
       option_st_h = round(option_st_h, 4), # réponse
       vraie_valeur = round(true_value, 4)) # valeur sans approximation
  
}

st = 40
# ATTENTION pour h
h = 1 # variation dans le temps EN JOUR puisque theta est en jour (/365) (DIFFÉRENCE)
st_plus_h = 44 # S_{t+h} (DIFFÉRENCE)
K = 45
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
call_bool = TRUE # call (TRUE); put (FALSE)
gamma_bool = FALSE # approximation Gamma (TRUE); sinon (FALSE)
theta_bool = FALSE # approximation Theta (TRUE); sinon (FALSE)

approx(st,
       h, # EN JOUR
       st_plus_h,
       K,
       sigma,
       r,
       tt,
       delt,
       call_bool,
       gamma_bool,
       theta_bool)

# Delta-Gamma hedging ----
object_to_hedge = function(option_list_vector, s0, sigma, alpha, tt, delt){
  # option_list_vector = list(
  #   list(buy = TRUE, quantity = 1, type = 'put', K = 40),
  #   list(buy = FALSE, quantity = 2, type = 'call', K = 40)
  # )
  
  gamma_object = 0 # intitialise le gamma
  
  for (sous_liste in option_list_vector){
    
    sign_var = ifelse(sous_liste$buy, 1, -1)
    
    if (sous_liste$type == 'call'){
      
      option = data.frame(bsopt(s = s0,
                                k = sous_liste$K,
                                v = sigma,
                                r = alpha,
                                tt = tt,
                                d = delt)$Call)[c('Gamma'),]
      
      gamma_tot = option * sous_liste$quantity
      
    } else if (sous_liste$type == 'put'){
      
      option = data.frame(bsopt(s = s0,
                                k = sous_liste$K,
                                v = sigma,
                                r = alpha,
                                tt = tt,
                                d = delt)$Put)[c('Gamma'),]
      
      gamma_tot = option * sous_liste$quantity
      
    } else {
      
      stop("Attention de bien spécifier 'call' ou 'put' comme type!")
      
    }
    
    gamma_object = gamma_object + sign_var * gamma_tot
    
  }
  
  # output
  list(gamma_to_hedge = gamma_object,
       conditions = list(s0 = s0, 
                         sigma = sigma, 
                         alpha = alpha,
                         tt = tt, 
                         delt = delt))
  
}

gamma_hedging = function(object_to_hedge, hedger_call_bool, K_hedger, tt_hedger){
  # hedger_call_bool : hedge l'objet avec un call (TRUE) ou un put (FASLSE)
  # K_hedger : prix d'exercice de l'option qu'on utilise pour hedger
  # tt_hedger : temps à l'échéance de l'option qu'on utilise pour hedger
  
  if (hedger_call_bool){
    
    gamma_hedger = data.frame(bsopt(s = object_to_hedge$conditions$s0,
                                    k = K_hedger,
                                    v = object_to_hedge$conditions$sigma,
                                    r = object_to_hedge$conditions$alpha,
                                    tt = tt_hedger,
                                    d = object_to_hedge$conditions$delt)$Call)[c('Gamma'),]
    
  } else {
    
    gamma_hedger = data.frame(bsopt(s = object_to_hedge$conditions$s0,
                                    k = K_hedger,
                                    v = object_to_hedge$conditions$sigma,
                                    r = object_to_hedge$conditions$alpha,
                                    tt = tt_hedger,
                                    d = object_to_hedge$conditions$delt)$Put)[c('Gamma'),]
    
  }
  
  nb_options = abs(object_to_hedge$gamma_to_hedge / gamma_hedger)
  
  buy = ifelse(sign(object_to_hedge$gamma_to_hedge) == sign(gamma_hedger),
               FALSE, # on vend l'option qu'on utilise pour hedger
               TRUE) # on achète l'option qu'on utilise pour hedger
  
  # output
  print(paste0('On ', ifelse(buy, 'achète ', 'vend '), round(nb_options, 4),
               ' ', ifelse(hedger_call_bool, 'call', 'put'), ' avec K = ', 
               K_hedger, ' et T = ', round(tt_hedger, 4), ' pour devenir Gamma neutre.'))
  
}

s0 = 40
sigma = 0.3
alpha = r = 0.08
tt = 180/365
delt = 0

# on liste les options à hedger (voir le fichier strategie_delta_gamma_hedging.R
# pour plus de détails sur l'objet "option_list_vector")
option_list_vector = list(
  list(buy = TRUE, quantity = 1, type = 'put', K = 40),
  list(buy = FALSE, quantity = 2, type = 'call', K = 40)
)

# on hedge avec :
hedger_call_bool = TRUE #  hedge l'objet avec un call (TRUE) ou un put (FASLSE)
K_hedger = 40 # prix d'exercice de l'option qu'on utilise pour hedger
tt_hedger = 180/365 # temps à l'échéance de l'option qu'on utilise pour hedger

gamma_hedging(object_to_hedge = object_to_hedge(option_list_vector, s0, sigma, alpha, tt, delt),
              hedger_call_bool,
              K_hedger,
              tt_hedger)












