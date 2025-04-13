#### Examen synthèse Capstone
#### Exemple de conversion d'un jeu de données raw (comme au Tableau 1.7 à la 
#### page 14 du livre Provisionnement en assurance IARD de Vigou) vers un 
#### triangle cumulatif
#### 

convert_to_tri_cum = function(data){
  
  n = nrow(data)
  annees = length(levels(data$date_sin))
  list_data = list()
  dev = list()
  
  for (niveaux in levels(data$date_sin)){
    
    list_data[[niveaux]] = 0
    
  }
  
  list_init = list_data
  
  for (years in seq(annees) - 1){
    
    list_data = list_init
  
    for (rows in seq(n)){
      
      if (as.numeric(data$date_sin[rows]) == as.numeric(data$date_pmt[rows]) - years){
        
        list_data[[data$date_sin[rows]]] = list_data[[data$date_sin[rows]]] + data$mtn[rows]
        
      }
      
    }
    
    dev[[years + 1]] = unlist(list_data)
    
  }
  
  by_year = matrix(nrow = annees,
                   ncol = annees)
  
  for (years in seq(annees)){
    
    by_year[years,] = unname(sapply(dev, `[`, years))
    
  }
  
  tri_incr = matrix(by_year,
                    ncol = annees,
                    byrow = FALSE)
  
  tri_incr[tri_incr == 0] = NA
  
  tri_cum = incr2cum(tri_incr) # incrémental vers cumulatif

  # output
  tri_cum
  
}

data = data.frame(date_sin = factor(c("2000", "2000", "2000",
                                      "2000", "2000", "2000",
                                      "2000", "2001", "2001",
                                      "2001", "2001", "2002",
                                      "2002", "2002", "2003",
                                      "2003", "2004", "2004")),
                  
                  date_pmt = factor(c("2000", "2000", "2001",
                                      "2001", "2002", "2003",
                                      "2004", "2001", "2002",
                                      "2003", "2004", "2002",
                                      "2003", "2004", "2003",
                                      "2004", "2004", "2004")),
                  
                  mtn = c(200, 225, 40, 57, 90, 102, 16, 532, 125, 57, 18, 717,
                          13, 72, 440, 120, 400, 220))

tri_cum = convert_to_tri_cum(data)



