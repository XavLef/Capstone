#### Examen synthèse Capstone
#### Exemple d'application de la fonction delta_hedging sur des stratégies 
#### (au lieu d'uniquement sur une option)
#### 

# stratégie 1 ----
# on achète bull(call, 40, 45) = call(40) - call(45)
s0 = 40
K = 40
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
vend_bool = FALSE # vend (TRUE); achète (FALSE)
call_bool = TRUE # call (TRUE); put (FALSE)
n = 100 # l'option est sur combien d'actions

call_40 = delta_hedging(vend_bool = vend_bool,
              call_bool = call_bool,
              n = n,
              s0 = s0,
              K = K,
              sigma = sigma,
              alpha = alpha,
              tt = tt,
              delt = delt)

s0 = 40
K = 45
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
vend_bool = TRUE # vend (TRUE); achète (FALSE)
call_bool = TRUE # call (TRUE); put (FALSE)
n = 100 # l'option est sur combien d'actions

call_45 = delta_hedging(vend_bool = vend_bool,
              call_bool = call_bool,
              n = n,
              s0 = s0,
              K = K,
              sigma = sigma,
              alpha = alpha,
              tt = tt,
              delt = delt)

call_40$delta_option - call_45$delta_option

call_40$prix_option - call_45$prix_option

call_40$achat_ou_vente_action
call_45$achat_ou_vente_action
2329.6166 - 1126.1902 # on vend des actions pour 1203.426 $

call_40$achat_ou_vente_option
call_45$achat_ou_vente_option
278.0402 - 97.1027 # on achète des options pour 180.9375 $

call_40$emprunt_ou_pret
call_45$emprunt_ou_pret
2051.5765 - 1029.0875 # on prête 1022.489 $

# stratégie 2 ----
# on  achète ratio_spread = put(45) - 2 * put(40)
s0 = 40
K = 45
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
vend_bool = FALSE # vend (TRUE); achète (FALSE)
call_bool = FALSE # call (TRUE); put (FALSE)
n = 100 # l'option est sur combien d'actions

put_45 = delta_hedging(vend_bool = vend_bool,
                        call_bool = call_bool,
                        n = n,
                        s0 = s0,
                        K = K,
                        sigma = sigma,
                        alpha = alpha,
                        tt = tt,
                        delt = delt)

s0 = 40
K = 40
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0
vend_bool = TRUE # vend (TRUE); achète (FALSE)
call_bool = FALSE # call (TRUE); put (FALSE)
n = 100 # l'option est sur combien d'actions

put_40 = delta_hedging(vend_bool = vend_bool,
                        call_bool = call_bool,
                        n = n,
                        s0 = s0,
                        K = K,
                        sigma = sigma,
                        alpha = alpha,
                        tt = tt,
                        delt = delt)

put_45$delta_option - 2*put_40$delta_option

put_45$prix_option - 2*put_40$prix_option

put_45$achat_ou_vente_action
put_40$achat_ou_vente_action # fois 2
2873.8098 - 2 * 1670.3834 # on vend des actions pour 466.957 $

put_45$achat_ou_vente_option
put_40$achat_ou_vente_option # fois 2
508.2384 - 2 * 199.0497 # on achète des options pour 110.139 $

put_45$emprunt_ou_pret
put_40$emprunt_ou_pret # fois 2
3382.0482 - 2 * 1869.4331 # on prête 356.818 $

