# custom_redistr_winners, losers, degree
# TODO: custom redistr: compute transfer for each; tax rates; dummy whether decrease own income; sociodemos determinants

# ../interactive_graph/reforme_perso_qualtrics.js converted into .R using ChatGPT on 18/05/2025

e$income_qantile <- # TODO
e$custom_redistr_winning <- e$income_qantile < e$custom_redistr_winners
e$custom_redistr_losing <- (100 - e$income_quantile) > e$custom_redistr_losers
e$custom_redistr_transfer <- compute_custom_redistr(e, return = "transfer")

compute_custom_redistr <- function(df = e, return = "transfer") { 
  
  if (return == "transfer") return(transfer)
  else if (return == "mean_redistr") return(mean_redistr)
}

# --- Parametros iniciais ---
# couple <- "Yes"  # ou "No"
# income <- 40000   # entrada exemplo
# if (couple %in% c("Yes", "はい", "Oui", "Ja", "Si", "Да", "Sì", "Sí", "Tak", "نعم", "そうだ")) {
#   income <- income / 2
# }
# 
# variant_sliders <- 0  # ou 1
# 
# if (variant_sliders == 1) {
#   winners <- 40 * 10 + 1
#   non_losers <- 1000 - 10 * 10 - 1
#   degree <- 7 + 0.1
# } else {
#   winners <- 60 * 10 + 1
#   non_losers <- 1000 - 20 * 10 - 1
#   degree <- 2 + 0.1
# }
# 
winners <- 40 * 10 + 1
non_losers <- 1000 - 10 * 10 - 1
degree <- 7 + 0.1

current <- as.numeric(read.csv2("../data_ext/world_disposable_inc.csv", header = FALSE)[2:1001, 2])
current <- c(current[1], current) 

income_threshold_winners <- income_from_quantile(current, winners)
income_threshold_losers <- income_from_quantile(current, non_losers)

new <- reference_distr(current)
future <- adjust(current)

interpole <- function(rev, current, new) {
  e <- 1
  while (e <= 1001 && current[e] < rev) {
    e <- e + 1
  }
  if (e == 1002) {
    return(new[1001])
  } else if (e == 1) {
    return(new[1])
  } else {
    return((new[e] - new[e-1]) * (rev - current[e-1]) / (current[e] - current[e-1]) + new[e-1])
  }
}

income_from_quantile <- function(donnees, q) {
  if (floor(q) == q) {
    return(donnees[q + 1])
  } else {
    return(donnees[floor(q) + 1] * (ceiling(q) - q) + donnees[ceiling(q) + 1] * (q - floor(q)))
  }
}

integral <- function(f, a, b) {
  if (b < a) return(-integral(f, b, a))
  else {
    sum <- 0
    for (i in (ceiling(a)):(floor(b) - 1)) sum <- sum + f[i + 1] 
    # sum <- sum + (ceiling(a) - a) * f[floor(a) + 1] + (b - floor(b)) * f[floor(b) + 1]
    return(sum)
  }
}

reference_distr <- function(current) {
  new <- current
  for (i in 1:winners) new[i] <- max(current[i], income_threshold_winners)
  for (i in (winners + 1):non_losers) new[i] <- current[i]
  for (i in (non_losers + 1):1000) new[i] <- min(current[i], income_threshold_losers)
  return(new)
}


# ajuste <- function(x, degree, current, new) {
#   return(x * (1 - degree) + degree * interpole(x, current, new))
# }

adjust <- function(current) {
  L <- economizable(current, "left")
  R <- economizable(current, "right")
  min_1 <- ifelse(L <= R, 0, non_losers)
  max_1 <- ifelse(L <= R, winners-1, 1000)
  min_2 <- ifelse(L <= R, non_losers, 0)
  max_2 <- ifelse(L <= R, 1000, winners-1)
  demogrant <- 2*(min(L, R) * (degree/10) + integral(current, 0, winners))
  new <- c()
  future <- current
  affine <- TRUE
  for (i in 0:winners) {
    new[i+1] <- demogrant + (i/winners) * (income_threshold_winners - demogrant)
    if (new[i+1] < current[i+1]) affine <- FALSE
    if (i > 0 && new[i] - current[i] < new[i+1] - current[i+1]) affine <- FALSE
  }
  for (i in min_1:max_1) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
  for (i in min_2:max_2) future[i+1] <- future[i+1] - (1 - min(L/R, R/L)) * (future[i+1] - current[i+1])
  for (i in min_2:max_2) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
  if (affine) for (i in 0:winners) future[i+1] <- new[i+1]
  diff <- future - current
  econ <- integral(diff, 0, winners - 1) + economizable(future, "right")
  min_income <- interpole(0, current, future)
  transfer <- (integral(future, 0, winners) - integral(current, 0, winners))/integral(current, 0, 1000)
  return(future)
}

# economisable <- function(income, degree, current, new) {
#   return(income - ajuste(income, degree, current, new))
# }

economizable <- function(current, side) {
  min_q <- ifelse(side == "left", 0, non_losers)
  max_q <- ifelse(side == "left", winners, 1000)
  black <- c()
  for (i in min_q:max_q) black <- c(black, ifelse(side == "left", income_threshold_winners - current[i+1], current[i+1] - income_threshold_losers))
  return(max(.000000001, integral(black, 0, max_q - min_q)))
}

# maj <- function(x, degree, current, new) {
#   return(degree * (interpole(x, current, new) - x))
# }

# # --- Calculos para faixas de renda ---
# valeurs <- c(10000, 40000, 100000, 500000)
# results <- sapply(valeurs, function(val) list(
#   now = val,
#   new = ajuste(val, degree, current, new),
#   econ = economisable(val, degree, current, new),
#   maj = maj(val, degree, current, new)
# ))

ggplot(curve_data, aes(x = index, y = valor, color = tipo)) +
  geom_line(size = 1.2) +
  labs(x = "Percentil da populacao", y = "Rendimento (PPP 2024 $)",
       title = "Curva de distribuicao de renda: antes e depois") +
  theme_minimal()
