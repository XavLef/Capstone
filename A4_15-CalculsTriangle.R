#### Examen synthèse Capstone
#### Exemple de calculs sur des triangles cumulatifs
#### 

# Fonctions ----
mtn_tot_payeX_pour_sin_survenusY = function(tri_cum, X, Y){
  # X et Y : années (numeric)
  
  diff = X - Y
  
  if (diff > 0){
    
  mtn = unname(abs(diff(tri_cum[as.character(Y), c(diff + 1, diff)])))
    
  } else {
    
    mtn = min(tri_cum[as.character(X),], na.rm = T)
    
  }
  
  # output
  mtn
  
}

mtn_tot_paye_sin_survenus = function(tri_cum, year){
  # tri_cum : triangle CUMULATIF
  # year : année (numeric)
  
  max(tri_cum[as.character(year),], na.rm = T)

}

mtn_tot_paye_annee = function(tri_cum, year){
  
  mtn_tot = 0
  
  for (annee in as.numeric(names(tri_cum[, 1]))[as.numeric(names(tri_cum[, 1])) <= year]){
    
    mtn_tot = mtn_tot +
      mtn_tot_payeX_pour_sin_survenusY(tri_cum,
                                       X = year,
                                       Y = annee)
    
  }
  
  # output
  mtn_tot
  
}

# Calculs ----
tri_cum = triangle("1993" = c(1780, 2673, 2874, 3094, 3157, 3166, 3166),
                   "1994" = c(3226, 4219, 4532, 4881, 5144, 5199),
                   "1995" = c(3652, 4989, 5762, 6436, 6720),
                   "1996" = c(2723, 4301, 5526, 6231),
                   "1997" = c(2923, 4666, 5349),
                   "1998" = c(2990, 5417),
                   "1999" = 3917)

# montant total payé pour les sinistres survenus en 1993 (année de survenance) (year)
mtn_tot_paye_sin_survenus(tri_cum, 
                          year = 1993)

# montant total payé en 1997 (X) pour les sinistres survenus en 1995 (Y)
mtn_tot_payeX_pour_sin_survenusY(tri_cum,
                                 X = 1997,
                                 Y = 1995)

# montant total payé pendant l'année 1999 (year)
mtn_tot_paye_annee(tri_cum, 1999)




