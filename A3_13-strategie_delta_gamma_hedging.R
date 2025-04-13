#### Examen synthèse Capstone
#### Exemple d'application de la fonction gamma_hedging sur des stratégies 
#### 

# détails sur l'objet "option_list_vector" ----
# Il s'agit d'une liste composée de sous-listes, ou chaque sous-liste représente
# une option de la stratégie qu'on souhaite Gamma hedger. Par exemple, si on
# veut Gamma hedger l'achat d'un ratio_spread = put(45, 91/365) - 2 * put(40, 91/365), 
# on aura une sous-liste list(buy = TRUE, quantity = 1, type = 'put', K = 45) 
# pour représenter l'achat d'un put(45, 91/365), et une sous-liste
# list(buy = FALSE, quantity = 2, type = 'put', K = 40) pour représenter la vente
# de 2 put(40, 91/365). Si on vendait plutôt cette stratégie, on inverserait
# simplement l'argument buy des 2 sous-listes pour obtenir 
# -put(45, 91/365) + 2 * put(40, 91/365).

# stratégie 1 ----
# on vend butterfly(call, 35, 40, 45, T = 91/365) = call(35, 91/365) - 2 * call(40, 91/365) + call(45, 91/365)
s0 = 40
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0

option_list_vector = list(
  list(buy = FALSE, quantity = 1, type = 'call', K = 35),
  list(buy = TRUE, quantity = 2, type = 'call', K = 40),
  list(buy = FALSE, quantity = 1, type = 'call', K = 45)
)

# on hedge avec call(40, T = 180/365)
hedger_call_bool = TRUE
K_hedger = 40
tt_hedger = 180/365

gamma_hedging(object_to_hedge = object_to_hedge(option_list_vector, s0, sigma, alpha, tt, delt),
              hedger_call_bool,
              K_hedger,
              tt_hedger)

# on a donc stratégie = -call(35, 91/365) + 2 * call(40, 91/365) - call(45, 91/365) - 0.829 * call(40, 180/365)
# qui est Gamma neutre. Ensuite, on peut appliquer la fonction de Delta hedging
# sur chacune des composantes et agréger les résultats pour être Delta-Gamma
# neutre. ATTENTION : On considère alors qu'on achète la stratégie (peut importe 
# si on vendait ou non initialement le butterfly; on achète la stratégie résultante).

# stratégie 2 ----
# on achète ratio_spread = put(45, 91/365) - 2 * put(40, 91/365)
s0 = 40
sigma = 0.3
alpha = r = 0.08
tt = 91/365
delt = 0

option_list_vector = list(
  list(buy = TRUE, quantity = 1, type = 'put', K = 45),
  list(buy = FALSE, quantity = 2, type = 'put', K = 40)
)

# on hedge avec call(40, T = 180/365)
hedger_call_bool = TRUE
K_hedger = 40
tt_hedger = 180/365

gamma_hedging(object_to_hedge = object_to_hedge(option_list_vector, s0, sigma, alpha, tt, delt),
              hedger_call_bool,
              K_hedger,
              tt_hedger)

# on a donc stratégie = put(45, 91/365) - 2 * put(40, 91/365) + 1.6311 * call(40, 180/365)
# qui est Gamma neutre. On peut ensuite hedger Delta comme mentionné pour la
# stratégie 1 (rappel : on achète la stratégie obtenue).





