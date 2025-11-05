# **This file can be removed**
# custom_redistr_winners, losers, degree
# TODO: custom redistr: tax rates; dummy whether decrease own income; sociodemos determinants

# ../interactive_graph/reforme_perso_qualtrics.js converted into .R using ChatGPT on 18/05/2025

e$income_qantile <- # TODO
e$custom_redistr_winning <- e$income_qantile < e$custom_redistr_winners
e$custom_redistr_losing <- (100 - e$income_quantile) > e$custom_redistr_losers
e$custom_redistr_transfer <- compute_custom_redistr(e, return = "transfer")

winners <- 40 * 10 + 1
non_losers <- 1000 - 10 * 10 - 1
degree <- 7 + 0.1

# current <- c(0, as.numeric(read.csv2("../data_ext/world_disposable_inc.csv", header = FALSE)[2:1001, 2]))

compute_custom_redistr <- function(df = e, name = NULL, return = "df") { 
  # TODO: check we get same results as with .js (e.g. check values of L/G and R, own_after...) - I have checked on one example
  current <- c(0, round(thousandile_world_disposable_inc)) # corresponds to c(0, thousandile_world_disposable_inc) created in questionnaire.R
  e$custom_redistr_transfer <- e$custom_redistr_future_income <- e$custom_redistr_income_min <- NA #rep(NA, nrow(df))
  futures <- matrix(NA, ncol = 1001, nrow = nrow(df))
  
  interpole_world_income <- function(rev, current, new) {
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
  
  for (k in 1:nrow(df)) {
    winners <- df$custom_redistr_winners[k]
    non_losers <- 1000 - df$custom_redistr_losers[k]
    degree <- df$custom_redistr_degree[k]
    custom_redistr_current_income <- df$income_exact[k] # TODO correct income_exact (income_exact/unit/2 if couple, etc.)
    if (!is.na(winners)) {
    income_threshold_winners <- current[winners + 1] # income_from_quantile(current, winners)
    income_threshold_losers <- current[non_losers + 1] # income_from_quantile(current, non_losers)
    
    # Define new as current bounded by the income thresholds of winners and losers
    new <- current
    for (i in 1:winners) new[i] <- pmax(current[i], income_threshold_winners)
    for (i in (winners + 1):non_losers) new[i] <- current[i]
    for (i in (non_losers + 1):1001) new[i] <- pmin(current[i], income_threshold_losers)
    future <- new
    # Computes what is "economizable", i.e. what can be redistributed on either side: what can be given on the left (among winners) or taken on the right
    L <- sum(pmax(0, income_threshold_winners - current)) # economizable(current, "left")
    R <- sum(pmax(0, current[1:1000] - income_threshold_losers)) # economizable(current, "right")
    # Iff what can be given is lower than what can be taken, the left side is binding, and we start focusing on the left side
    min_1 <- ifelse(L <= R, 0, non_losers)
    max_1 <- ifelse(L <= R, winners-1, 1000)
    min_2 <- ifelse(L <= R, non_losers, 0)
    max_2 <- ifelse(L <= R, 1000, winners-1)
    # Define the demogrant given what is economizable and the desired degree of redistribution
    demogrant <- if (winners > 0) 2*(min(L, R) * (degree/10) + sum(current[1:winners]))/winners - income_threshold_winners else 0
    # Draw a straight line between the demogrant and the threshold of winners
    # Turn to non-affine line if the affine line crosses the current line or if lower incomes raise less than higher ones
    affine <- TRUE
    for (i in 0:(winners-1)) {
      if (winners > 0) new[i+1] <- demogrant + (i/winners) * (income_threshold_winners - demogrant) # In practice, not used as affine is generally FALSE
      if (i > 0 && new[i+1] < current[i+1]) affine <- FALSE
      if (i > 0 && new[i] - current[i] < new[i+1] - current[i+1]) affine <- FALSE
    }
    # We make the future line closer to the current one compared to the horizontal "new" line, to the extent degree is small, on the binding side
    for (i in min_1:max_1) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
    # Then we adjust the non-binding side twice: first by having the maximal redistribution (given the other, binding side potential),
    for (i in min_2:max_2) future[i+1] <- future[i+1] - (1 - min(L/max(1e-9, R), R/max(1e-9, L))) * (future[i+1] - current[i+1])
    # then by accounting for the desired degree of redistr (as above)
    for (i in min_2:max_2) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
    if (affine) for (i in 0:winners) future[i+1] <- new[i+1]
    # diff <- future - current
    # econ <- integral(diff, 0, winners - 1) + economizable(future, "right")
    
    df$custom_redistr_income_min[k] <- future[1]
    df$custom_redistr_future_income[k] <- interpole_world_income(custom_redistr_current_income, current, future) 
    df$custom_redistr_transfer[k] <- 100*sum(future[1:winners] - current[1:winners])/sum(current[1:1000]) 
    # transfer <- (integral(future, 0, winners) - integral(current, 0, winners))/integral(current, 0, 1000)
    futures[k, ] <- future 
    if (return == "verbose") print(paste("L:", round(L), "   R:", round(R), "   demogrant:", round(demogrant), "   transfer:", df$custom_redistr_transfer[k], 
          "   income_threshold_winners:", income_threshold_winners, "   sum(current[1:winners]): ", sum(current[1:winners])))
    }
  }
  mean_redistr <- colSums(futures * df$weight, na.rm = T)/sum(df$weight)
  if (!is.null(name) && exists("mean_custom_redistr")) mean_custom_redistr[[name]] <<- mean_redistr
  
  if (return == "df") return(df)
  else if (return == "mean_redistr") return(mean_redistr)
  else return(futures)
}
temp <- compute_custom_redistr(e, name = "test", return = "df")
all <- compute_custom_redistr(all, name = "all")
# temp <- compute_custom_redistr(data.frame(custom_redistr_degree = 7, custom_redistr_winners = 780, custom_redistr_losers = 20, income_exact = 0), return = "verbose")

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

income_threshold_winners <- current[winners + 1] # income_from_quantile(current, winners)
income_threshold_losers <- current[non_losers + 1] # income_from_quantile(current, non_losers)

# new <- reference_distr(current)
future <- adjust(current)


# income_from_quantile <- function(donnees, q) {
#   if (floor(q) == q) {
#     return(donnees[q + 1])
#   } else {
#     return(donnees[floor(q) + 1] * (ceiling(q) - q) + donnees[ceiling(q) + 1] * (q - floor(q)))
#   }
# }

# integral <- function(f, a, b) { # TODO: improve to account for last element? (for the moment, doesn't include the highest income in the sum)
#   sum(f[(a+1):b])
#   # if (b < a) return(-integral(f, b, a))
#   # else {
#   #   sum <- 0
#   #   for (i in (ceiling(a)):(floor(b) - 1)) sum <- sum + f[i + 1] 
#   #   # sum <- sum + (ceiling(a) - a) * f[floor(a) + 1] + (b - floor(b)) * f[floor(b) + 1]
#   #   return(sum)
#   # }
# }

# reference_distr <- function(current) {
#   new <- current
#   for (i in 1:winners) new[i] <- max(current[i], income_threshold_winners)
#   for (i in (winners + 1):non_losers) new[i] <- current[i]
#   for (i in (non_losers + 1):1000) new[i] <- min(current[i], income_threshold_losers)
#   return(new)
# }

# economizable <- function(current, side) {
#   min_q <- ifelse(side == "left", 0, non_losers)
#   max_q <- ifelse(side == "left", winners, 1000)
#   black <- c()
#   for (i in min_q:max_q) black <- c(black, ifelse(side == "left", income_threshold_winners - current[i+1], current[i+1] - income_threshold_losers))
#   return(max(.000000001, integral(black, 0, max_q - min_q))) # sum(black[1:(max_q - min_q)]
# }

# adjust <- function(current) {
#   # Define new as current bounded by the income thresholds of winners and losers
#   new <- current
#   for (i in 1:winners) new[i] <- max(current[i], income_threshold_winners)
#   for (i in (winners + 1):non_losers) new[i] <- current[i]
#   for (i in (non_losers + 1):1000) new[i] <- min(current[i], income_threshold_losers)
#   # Computes what is "economizable", i.e. what can be redistributed on either side: what can be given on the left (among winners) or taken on the right
#   L <- sum(pmax(1e-9, income_threshold_winners - current)) # economizable(current, "left")
#   R <- sum(pmax(1e-9, current[1:1000] - income_threshold_losers)) # economizable(current, "right")
#   # Iff what can be given is lower than what can be taken, the left side is binding, and we start focusing on the left side
#   min_1 <- ifelse(L <= R, 0, non_losers)
#   max_1 <- ifelse(L <= R, winners-1, 1000)
#   min_2 <- ifelse(L <= R, non_losers, 0)
#   max_2 <- ifelse(L <= R, 1000, winners-1)
#   # Define the demogrant given what is economizable and the desired degree of redistribution
#   demogrant <- 2*(min(L, R) * (degree/10) + sum(current[1:winners])) # integral(current, 0, winners)
#   # Draw a straight line between the demogrant and the threshold of winners
#   # Turn to non-affine line if the affine line crosses the current line or if lower incomes raise less than higher ones
#   future <- current
#   affine <- TRUE
#   for (i in 0:winners) {
#     new[i+1] <- demogrant + (i/winners) * (income_threshold_winners - demogrant) # In practice, not used as affine is generally FALSE
#     if (new[i+1] < current[i+1]) affine <- FALSE
#     if (i > 0 && new[i] - current[i] < new[i+1] - current[i+1]) affine <- FALSE
#   }
#   # We make the future line closer to the current one compared to the horizontal "new" line, to the extent degree is small, on the binding side
#   for (i in min_1:max_1) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
#   # Then we adjust the non-binding side twice: first by having the maximal redistribution (given the other, binding side potential),
#   for (i in min_2:max_2) future[i+1] <- future[i+1] - (1 - min(L/R, R/L)) * (future[i+1] - current[i+1])
#   # then by accounting for the desired degree of redistr (as above)
#   for (i in min_2:max_2) future[i+1] <- future[i+1] - (10-degree)/10 * (future[i+1] - current[i+1])
#   if (affine) for (i in 0:winners) future[i+1] <- new[i+1]
#   # diff <- future - current
#   # econ <- integral(diff, 0, winners - 1) + economizable(future, "right")
#   custom_redistr_income_min <- future[1]
#   custom_redistr_new_income <- interpole(income_exact, current, future) # TODO correct income_exact
#   transfer <- sum(future[1:winners] - current[1:winners])/sum(current[1:1000]) 
#   # transfer <- (integral(future, 0, winners) - integral(current, 0, winners))/integral(current, 0, 1000)
#   return(future)
# }

# ggplot(curve_data, aes(x = index, y = valor, color = tipo)) +
#   geom_line(size = 1.2) +
#   labs(x = "Percentil da populacao", y = "Rendimento (PPP 2024 $)",
#        title = "Curva de distribuicao de renda: antes e depois") +
#   theme_minimal()
