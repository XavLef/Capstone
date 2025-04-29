#### Examen synthèse Capstone
### A4.10 - Modifications de couverture
###

library(actuar)

# Données individuelles ----
n_i <- c(3, 5, 5, 6, 8, 8, 8, 8, 9, 10, 10, 11, 11, 11, 16, 21, 23, 26, 29, 36)

## F_n ----
Fn <- ecdf(n_i)
Fn_x <- environment(Fn)$x
Fn_y <- environment(Fn)$y

data.frame(x = Fn_x, y = Fn_y) # bornes : a <= x < b (ex : pour x = 5, l'intervalle est 3 <= x < 5)

## f_n ----
tab <- table(n_i)
(fn <- tab / sum(tab))

# Données groupées ----
cj <- c(2, 7, 12, 22, 38) # (2, 7], (7, 12], (12, 22], (22, 38]
nj <- c(4, 10, 2, 4)
# verif
n <- 20 # nb. obs. total
sum(nj) == n # devrait être TRUE

## ogive ----
(x <- grouped.data(Group = cj, Frequency = nj))
Fn_ogive = ogive(x)
Fn_ogive(12)

## histogramme ----
c_lower <- cj[-length(cj)]
c_upper <- cj[-1]
widths <- c_upper - c_lower

densities <- nj / (n * widths)

hist_df <- data.frame(interval_lower = c_lower,
                      interval_upper = c_upper,
                      width = widths,
                      frequency = nj,
                      density = densities)

hist_df # bornes : a < x <= b






