#### Examen synthèse Capstone
#### Exemple de calcul de réserve selon les trois méthodes lorsqu'aucun
#### triangle n'est donné
#### 

# Fonctions ----
compute_betas = function(dev_factor){
  
  inv_beta = numeric(length(dev_factor))
  
  j = 0
  for (i in seq(length(dev_factor))){
    
    j = j + 1
    
    inv_beta[j] = prod(dev_factor)
    
    dev_factor = dev_factor[-1]
    
  }
  
  beta = 1 / inv_beta
  
  # output : c(beta_1, ..., beta_ult)
  beta
  
}

compute_C_BF = function(C_LR, C_ij, dev_factor, j){
  
  betas = compute_betas(dev_factor)
  
  C_BF_stock = numeric(length(dev_factor))
  
  C_BF = C_ij
  
  for (i in seq(length(C_BF_stock))){
    
    if (i != length(C_BF_stock)){
      
      C_BF = C_BF + C_LR * ((1 - betas[j]) - (1 - betas[j + 1]))
      
      j = j + 1
      
      C_BF_stock[i] = C_BF
      
    } else {
      
      C_BF = C_BF + C_LR * (1 - betas[j])
      
      C_BF_stock[i] = C_BF
      
    }
    
  }
  
  # output
  C_BF_stock
  
}

# Inputs généraux ----
dev_factor = c(1.75, 1.6, 1.4, 1.1, 1.05) # c(lambda_1, ..., lambda_ultime)

i = 1998 # année i

primes_acquises_i = 1000000 # primes acquises dans l'année i

ELR = 0.65

j = 2 # j-ième évaluation ({année à laquelle on estime C_ij} - i); 31/12/1999 = 2000

# Note : C_ij est inutile si on cherche seulement la réserve selon BF
C_ij = 320000 # paiements cumulatifs pour l'année i à la j-ième évaluation 

## ELR ----
C_LR = ELR * primes_acquises_i
reserve_LR = C_LR - C_ij

## BF ----
C_LR = ELR * primes_acquises_i
reserve_BF = C_LR * (1 - 1 / prod(dev_factor[seq(from = j, to = length(dev_factor))]))

# # verification
max(compute_C_BF(C_LR, C_ij, dev_factor, j)) - C_ij

## CL ----
C_CL = C_ij * prod(dev_factor[seq(from = j, to = length(dev_factor))])
reserve_CL = C_CL - C_ij

# Ventiler ----
# Calculer la réserve sur un horizon de t années pour C_ij
C_it = 200000 # C_ij est maintenant C_it à l'année k
k = 2003
t = k - i

## ELR ----
# Ne se calcule pas

## BF ----
C_LR = ELR * primes_acquises_i
diff_BF = compute_C_BF(C_LR, C_ij, dev_factor, j)[t - 1] - C_it

## CL ----
C_CL = C_ij * prod(dev_factor[seq(from = j, to = t - 1)])
diff_CL = C_CL - C_it

