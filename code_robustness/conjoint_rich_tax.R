##### Conjoint: international millionaire tax vs. domestic taxes on the rich #####
# Referee request: the international tax on millionaires bundles "tax the rich" with "fund LICs". We compare its AMCE with the AMCE of
# DOMESTIC taxes on the rich present in the same countries' conjoint pools, to isolate the premium/discount attached to the global (LIC) component.
#
# Usage (from anywhere): Rscript --vanilla code_robustness/conjoint_rich_tax.R
#   --vanilla avoids code_robustness/.Rprofile (Windows lib paths, Qualtrics credentials, package auto-install).
# Self-contained and READ-ONLY w.r.t. existing files. Inputs:
#   - conjoint_analysis/ca_[country]_.csv : conjoint data exported by 3_render.R (compute_conjoint()), one row per respondent of the final sample,
#       with policy codes (F-1-[1/2]-k), choice (conjoint), weight (= weight_country) and respondent id n.
#   - data_raw/[country].csv : only to recover vote variables (replicating prepare()/convert() in 2_prepare.R: final sample, n = paste0(country, row)).
#   - questionnaire/sources.xlsx : Elections (vote leaning), policies_leaning_party (consistent programs), Quotas (adult population for pooled weights).
#   - conjoint_analysis/policies.json : English labels of policies.
# Outputs: conjoint_analysis/rich_tax_comparison/*.csv (+ log.txt)
# Conventions (as in 4_paper.R, Section 4.1): OLS LPM of program_preferred (1 if the program is chosen; "Neither" = 0 for both programs),
#   weights = weight (country weights rescaled to adult population for pooled regressions; weight_country within country),
#   SE clustered by respondent n (CR1 as sandwich::vcovCL default: G/(G-1) * (N-1)/(N-K)), t-tests with residual df (as lmtest::coeftest).

suppressMessages({ library(readr); library(openxlsx); library(jsonlite) })
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("^--file=", "", args[grepl("^--file=", args)])
root <- if (length(script_path)) normalizePath(file.path(dirname(script_path), "..")) else normalizePath(".")
out_dir <- file.path(root, "conjoint_analysis", "rich_tax_comparison")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
sink(file.path(out_dir, "log.txt"), split = TRUE)
set.seed(20261021)
B_boot <- as.integer(Sys.getenv("B_BOOT", "999")) # number of cluster-bootstrap replications

countries_conjoint <- c("FR", "DE", "IT", "PL", "ES", "GB", "CH", "JP", "US")
languages_en <- c(FR = "EN-FR", DE = "EN-DE", IT = "EN-IT", PL = "EN-PL", ES = "EN-ES", GB = "EN-GB", CH = "EN-CH", JP = "EN-JA", US = "EN")
conjoint_attributes <- c("econ_issues", "society_issues", "climate_pol", "tax_system", "foreign_policy")
sources <- file.path(root, "questionnaire", "sources.xlsx")


##### 1. Policies and classification #####
policies_json <- fromJSON(file.path(root, "conjoint_analysis", "policies.json"))
policies <- do.call(rbind, lapply(countries_conjoint, function(c) {
  l <- unlist(setNames(policies_json[[languages_en[c]]], conjoint_attributes)) # same coding as 2_prepare.R (policies_code)
  data.frame(country = c, domain = sub("[0-9]+$", "", names(l)), code = names(l), label_en = trimws(unname(l)), stringsAsFactors = FALSE)
}))
policies <- policies[policies$label_en != "-", ]

# Domestic taxes on the rich. "main": label explicitly raises taxes on the rich / richest / large fortunes / top earners.
# "narrow": main minus the two "tax shifts" that also cut taxes for low/middle incomes (ES, JP). "broad": main + borderline items.
rich_main <- c("FR_tax_system4", "DE_tax_system4", "ES_tax_system1", "CH_tax_system2", "JP_tax_system1", "US_tax_system2")
rich_narrow <- c("FR_tax_system4", "DE_tax_system4", "CH_tax_system2", "US_tax_system2")
rich_broad_extra <- c("FR_tax_system3", "GB_tax_system1", "PL_tax_system2")
rich_broad <- c(rich_main, rich_broad_extra)
rich_cuts <- c("DE_tax_system2", "ES_tax_system2", "GB_tax_system2", "IT_tax_system2", "US_tax_system3") # opposite direction (tax cuts benefiting the rich), not used in comparisons
policies$key <- paste0(policies$country, "_", policies$code)
policies$rich_tax_class <- ifelse(policies$key %in% rich_narrow, "main+narrow", ifelse(policies$key %in% rich_main, "main (not narrow)",
                           ifelse(policies$key %in% rich_broad_extra, "broad only (borderline)", ifelse(policies$key %in% rich_cuts, "excluded: tax cut for the rich",
                           ifelse(policies$code == "foreign_policy1", "international millionaire tax", "no")))))
write.csv(policies, file.path(out_dir, "classification.csv"), row.names = FALSE)
cat("\n=== Classification of tax-related / rich-tax items ===\n")
print(policies[policies$rich_tax_class != "no" & policies$code != "foreign_policy1", c("country", "code", "rich_tax_class", "label_en")], row.names = FALSE, right = FALSE)


##### 2. Data #####
votes_xlsx <- read.xlsx(sources, sheet = "Elections", cols = 1:6)
qs <- read.xlsx(sources, sheet = "Quotas", rowNames = TRUE, rows = c(1, 2:15), cols = 1:57)
adult_pop <- setNames(round(qs[countries_conjoint, "Adult.pop"]), countries_conjoint)
leaning_party <- read.xlsx(sources, sheet = "policies_leaning_party", rowNames = TRUE, cols = c(1, 2*1:9)) # as policies_leaning_party in 2_prepare.R
leaning_party[c("foreign_policy1", "foreign_policy2"), ] <- 1

load_country <- function(c) {
  ca <- read.csv(file.path(root, "conjoint_analysis", paste0("ca_", c, "_.csv")), skip = 1, check.names = FALSE, stringsAsFactors = FALSE) # first line is a duplicated header
  raw <- suppressWarnings(read_csv(file.path(root, "data_raw", paste0(c, ".csv")), guess_max = Inf, show_col_types = FALSE, progress = FALSE))
  raw <- raw[is.na(raw$excluded) & raw$finished %in% c(TRUE, "TRUE", 1, "1"), ] # scope = "final" in prepare()
  raw$n <- paste0(c, seq_len(nrow(raw)))
  stopifnot(nrow(raw) == nrow(ca), all(raw$n == ca$n), all(raw$conjoint == ca$conjoint, na.rm = TRUE))
  # vote_factor as in 2_prepare.R (convert)
  v <- votes_xlsx[votes_xlsx$country == c, ]; row.names(v) <- v$party
  voted <- raw$voted %in% "Yes"
  vote_original <- raw[[paste0("vote_", c)]]
  vote_agg <- ifelse(vote_original %in% c("Prefer not to say", "Other", NA), -1, v[as.character(vote_original), "leaning"])
  vote <- ifelse(voted, vote_agg, -1)
  ca$vote_factor <- c("-1" = "Non-voter, PNR or Other", "0" = "Left", "1" = "Center-right or Right", "2" = "Far right")[as.character(vote)]
  ca$country <- c
  ca
}
resp <- do.call(rbind, lapply(countries_conjoint, load_country))
resp$weight_country <- resp$weight
n_c <- table(resp$country)
resp$weight <- resp$weight_country * adult_pop[resp$country] / as.numeric(n_c[resp$country]) # as all$weight (up to a constant)
resp$weight <- resp$weight / mean(resp$weight)

# consistent_conjoints_party (as in 2_prepare.R)
leaning_prog <- function(i) {
  m <- sapply(paste0("F-1-", i, "-", 1:5), function(v) { x <- resp[[v]]; x[is.na(x)] <- "-"; leaning_party[cbind(match(x, row.names(leaning_party)), match(resp$country, names(leaning_party)))] })
  mx <- apply(m, 1, max); mn <- apply(m, 1, min)
  l <- rep(1, nrow(resp)); l[mx == 2] <- 2; l[mn == 0] <- 0; l[mx - mn == 2] <- -1
  l
}
resp$consistent_conjoints_party <- (leaning_prog(1) != -1) & (leaning_prog(2) != -1)

# Long format: one row per (respondent, program)
long <- do.call(rbind, lapply(1:2, function(i) {
  p <- resp[, paste0("F-1-", i, "-", 1:5)]
  data.frame(n = resp$n, country = resp$country, profile = c("A", "B")[i], weight = resp$weight, weight_country = resp$weight_country,
             vote_factor = resp$vote_factor, consistent_conjoints_party = resp$consistent_conjoints_party,
             program = apply(p, 1, paste, collapse = " "), no_program_info = apply(p, 1, function(r) all(is.na(r))),
             program_preferred = resp$conjoint == paste("Candidate", c("A", "B")[i]), stringsAsFactors = FALSE)
}))
long <- long[!is.na(long$program_preferred), ]
in_prog <- function(code) sapply(strsplit(long$program, " "), function(x) code %in% x)
for (k in unique(policies$code)) long[[k]] <- in_prog(k)
long$millionaire_tax_in_program <- long$foreign_policy1
long$cut_aid_in_program <- long$foreign_policy2
long$foreign3_in_program <- long$foreign_policy3


##### 3. Estimation tools #####
# Weighted OLS with CR1 cluster-robust vcov (identical to sandwich::vcovCL(lm(..., weights), cluster = ~n) with defaults)
wls_cl <- function(X, y, w, cl) {
  keep <- colSums(X != 0) > 0
  X <- X[, keep, drop = FALSE]
  fit <- lm.wfit(X, y, w)
  ok <- !is.na(fit$coefficients)
  X <- X[, ok, drop = FALSE]
  b <- fit$coefficients[ok]
  e <- as.vector(y - X %*% b)
  bread <- solve(crossprod(X * sqrt(w)))
  s <- rowsum(X * (w * e), cl)
  N <- nrow(X); K <- ncol(X); G <- nrow(s)
  V <- bread %*% crossprod(s) %*% bread * G/(G - 1) * (N - 1)/(N - K)
  list(b = b, V = V, N = N, G = G, df = N - K)
}
lincom <- function(fit, a, name = "") { # a: named vector of weights on coefficients
  a <- a[names(a) %in% names(fit$b)]
  est <- sum(a * fit$b[names(a)]); se <- sqrt(as.numeric(t(a) %*% fit$V[names(a), names(a)] %*% a))
  data.frame(term = name, estimate = est, se = se, p = 2 * pt(-abs(est/se), fit$df), N_obs = fit$N, N_resp = fit$G)
}
ratio_delta <- function(fit, num, den) { # num, den: named weight vectors
  a <- sum(num * fit$b[names(num)]); b <- sum(den * fit$b[names(den)])
  nm <- union(names(num), names(den)); g <- setNames(rep(0, length(nm)), nm)
  g[names(num)] <- g[names(num)] + num / b; g[names(den)] <- g[names(den)] - den * a / b^2
  se <- sqrt(as.numeric(t(g) %*% fit$V[nm, nm] %*% g))
  c(ratio = a/b, ratio_se = se, ratio_lo = a/b - 1.96*se, ratio_hi = a/b + 1.96*se)
}
# Cluster (respondent) bootstrap, stratified by country, for difference and ratio
boot_stats <- function(X, y, w, cl, strata, num, den, B = B_boot) {
  keep <- colSums(X != 0) > 0; X <- X[, keep, drop = FALSE]
  ids <- unique(cl); rows_by_id <- split(seq_along(cl), factor(cl, levels = ids)); strata_id <- strata[match(ids, cl)]
  idx_by_stratum <- split(seq_along(ids), strata_id)
  res <- t(replicate(B, {
    draw <- unlist(lapply(idx_by_stratum, function(ix) ix[sample.int(length(ix), length(ix), replace = TRUE)]))
    r <- unlist(rows_by_id[draw], use.names = FALSE)
    b <- lm.wfit(X[r, , drop = FALSE], y[r], w[r])$coefficients
    a <- sum(num * b[names(num)]); d <- sum(den * b[names(den)])
    c(diff = a - d, ratio = a / d)
  }))
  c(boot_diff_lo = unname(quantile(res[, "diff"], .025, na.rm = TRUE)), boot_diff_hi = unname(quantile(res[, "diff"], .975, na.rm = TRUE)),
    boot_ratio_lo = unname(quantile(res[, "ratio"], .025, na.rm = TRUE)), boot_ratio_hi = unname(quantile(res[, "ratio"], .975, na.rm = TRUE)),
    boot_p_diff = 2 * min(mean(res[, "diff"] <= 0, na.rm = TRUE), mean(res[, "diff"] >= 0, na.rm = TRUE)))
}

# Design matrix. Country-specific dummies for every policy (vs. "no policy" in its domain), country fixed effects;
# common_codes: policies with a common coefficient across countries (int'l tax, cut aid); rich_keys pooled into one indicator if pool_rich.
make_X <- function(d, common_codes = c("foreign_policy1", "foreign_policy2"), rich_keys = NULL, pool_rich = FALSE, interaction = FALSE) {
  cs <- unique(d$country)
  X <- sapply(cs, function(c) 1 * (d$country == c)); colnames(X) <- paste0("FE_", cs)
  if (length(cs) == 1) colnames(X) <- "(Intercept)"
  for (k in common_codes) X <- cbind(X, matrix(1 * d[[k]], ncol = 1, dimnames = list(NULL, k)))
  d$dom_rich_tax <- FALSE
  for (c in cs) for (k in policies$code[policies$country == c]) if (!k %in% common_codes) {
    key <- paste0(c, "_", k)
    if (pool_rich && key %in% rich_keys) { d$dom_rich_tax <- d$dom_rich_tax | (d$country == c & d[[k]]); next }
    X <- cbind(X, matrix(1 * (d$country == c & d[[k]]), ncol = 1, dimnames = list(NULL, key)))
  }
  if (pool_rich) X <- cbind(X, dom_rich_tax = 1 * d$dom_rich_tax)
  if (interaction) X <- cbind(X, intl_x_dom_rich = 1 * (d$foreign_policy1 & d$dom_rich_tax))
  X
}
fit_spec <- function(d, weighted = TRUE, ...) {
  X <- make_X(d, ...); w <- if (weighted) d$weight else rep(1, nrow(d))
  list(fit = wls_cl(X, 1 * d$program_preferred, w, d$n), X = X, w = w)
}
summ_compare <- function(f, dom_terms, label, extra = list(), boot = FALSE, d = NULL) { # dom_terms: named weights of domestic rich tax coefficients
  intl <- lincom(f$fit, c(foreign_policy1 = 1), "intl_tax")
  dom <- lincom(f$fit, dom_terms, "dom_rich_tax")
  dif <- lincom(f$fit, c(foreign_policy1 = 1, -dom_terms), "diff")
  rat <- ratio_delta(f$fit, c(foreign_policy1 = 1), dom_terms)
  out <- data.frame(spec = label, intl = intl$estimate, intl_se = intl$se, intl_p = intl$p, dom = dom$estimate, dom_se = dom$se, dom_p = dom$p,
                    diff = dif$estimate, diff_se = dif$se, diff_p = dif$p, t(rat), N_obs = intl$N_obs, N_resp = intl$N_resp, extra, check.names = FALSE)
  if (boot) out <- cbind(out, t(boot_stats(f$X, 1 * d$program_preferred, f$w, d$n, d$country, c(foreign_policy1 = 1), dom_terms)))
  out
}

d_all <- long[!long$no_program_info, ]
rbind_fill <- function(...) { l <- list(...); cols <- unique(unlist(lapply(l, names)))
  do.call(rbind, lapply(l, function(x) { for (v in setdiff(cols, names(x))) x[[v]] <- NA; x[cols] })) }


##### 4. Replication of the paper's estimates (sanity check) #####
cat("\n=== Replication of 4_paper.R (all 9 countries, incl. 4 respondents with no program info as in `call`) ===\n")
Xp <- cbind("(Intercept)" = 1, millionaire_tax = 1 * long$foreign_policy1, cut_aid = 1 * long$foreign_policy2, foreign3 = 1 * long$foreign_policy3)
rep_fit <- wls_cl(Xp, 1 * long$program_preferred, long$weight, long$n)
print(round(cbind(estimate = rep_fit$b, se = sqrt(diag(rep_fit$V))), 4)) # paper: +5.1 pp (1.0), -2.9 pp
cat("Share inconsistent programs (party, weighted, per program-row):", round(100 * weighted.mean(!long$consistent_conjoints_party, long$weight), 2), "% (paper: 28.54%)\n")
sub <- long$consistent_conjoints_party
rep_fit2 <- wls_cl(Xp[sub, ], 1 * long$program_preferred[sub], long$weight[sub], long$n[sub])
print(round(cbind(estimate = rep_fit2$b, se = sqrt(diag(rep_fit2$V))), 4))


##### 5. Full AMCEs by country (all policies, each vs. "no policy" in its domain) #####
amce_full <- do.call(rbind, lapply(countries_conjoint, function(c) {
  d <- d_all[d_all$country == c, ]
  f <- fit_spec(d, common_codes = character(0))$fit
  b <- f$b[names(f$b) != "(Intercept)"]
  data.frame(country = c, key = names(b), estimate = b, se = sqrt(diag(f$V))[names(b)], p = 2 * pt(-abs(b / sqrt(diag(f$V))[names(b)]), f$df), N_obs = f$N, N_resp = f$G)
}))
amce_full <- merge(amce_full, policies[, c("key", "domain", "label_en", "rich_tax_class")], by = "key", all.x = TRUE)
amce_full <- amce_full[order(amce_full$country, amce_full$key), ]
write.csv(amce_full, file.path(out_dir, "amce_full_by_country.csv"), row.names = FALSE)


##### 6. Main comparison: by country and pooled #####
by_country <- function(rich_keys, set_name, weighted = TRUE, subset = NULL, boot = TRUE) {
  do.call(rbind, lapply(countries_conjoint, function(c) {
    ks <- rich_keys[startsWith(rich_keys, paste0(c, "_"))]
    if (!length(ks)) return(NULL)
    d <- d_all[d_all$country == c & (if (is.null(subset)) TRUE else d_all[[subset]]), ]
    f <- fit_spec(d, weighted = weighted) # with one country, "common" int'l tax / cut aid dummies are just the country's dummies
    do.call(rbind, lapply(ks, function(k) summ_compare(f, setNames(1, k), set_name, extra = list(country = c, dom_item = k), boot = boot, d = d)))
  }))
}
pooled <- function(rich_keys, set_name, weighted = TRUE, subset = NULL, boot = TRUE, d0 = d_all, interaction = FALSE) {
  cs <- unique(sub("_.*", "", rich_keys))
  d <- d0[d0$country %in% cs & (if (is.null(subset)) TRUE else d0[[subset]]), ]
  # (a) single pooled indicator for domestic rich taxes, common int'l tax coefficient
  f <- fit_spec(d, weighted = weighted, rich_keys = rich_keys, pool_rich = TRUE, interaction = interaction)
  out <- summ_compare(f, c(dom_rich_tax = 1), set_name, extra = list(country = paste0("Pooled (", paste(cs, collapse = ","), ")"), dom_item = "pooled indicator"), boot = boot, d = d)
  if (interaction) out <- list(out = out, fit = f$fit)
  out
}
pooled_items <- function(rich_keys, set_name, weighted = TRUE) { # (b) item-specific dummies, common int'l tax coefficient; diff vs. pop-weighted mean of items
  cs <- unique(sub("_.*", "", rich_keys))
  d <- d_all[d_all$country %in% cs, ]
  f <- fit_spec(d, weighted = weighted)
  wc <- adult_pop[sub("_.*", "", rich_keys)] / table(sub("_.*", "", rich_keys))[sub("_.*", "", rich_keys)] # pop weight of country split equally among its items
  rbind(summ_compare(f, setNames(as.numeric(wc / sum(wc)), rich_keys), set_name, extra = list(country = paste0("Pooled (", paste(cs, collapse = ","), ")"), dom_item = "items, pop-weighted mean")),
        do.call(rbind, lapply(rich_keys, function(k) summ_compare(f, setNames(1, k), set_name, extra = list(country = paste0("Pooled (", paste(cs, collapse = ","), ")"), dom_item = k)))))
}
popavg_country <- function(bc) { # population-weighted average of country-level estimates (independent samples)
  bc <- bc[!duplicated(bc$country), ] # one item per country (first) if several
  w <- adult_pop[bc$country] / sum(adult_pop[bc$country])
  f <- function(x, s) c(sum(w * x), sqrt(sum(w^2 * s^2)))
  i <- f(bc$intl, bc$intl_se); dm <- f(bc$dom, bc$dom_se); df <- f(bc$diff, bc$diff_se)
  data.frame(spec = bc$spec[1], intl = i[1], intl_se = i[2], intl_p = 2*pnorm(-abs(i[1]/i[2])), dom = dm[1], dom_se = dm[2], dom_p = 2*pnorm(-abs(dm[1]/dm[2])),
             diff = df[1], diff_se = df[2], diff_p = 2*pnorm(-abs(df[1]/df[2])), N_obs = sum(bc$N_obs), N_resp = sum(bc$N_resp),
             country = paste0("Pop-weighted avg of country estimates (", paste(bc$country, collapse = ","), ")"), dom_item = "country-specific", check.names = FALSE)
}

cat("\n=== Main comparison ===\n")
res_country <- rbind_fill(by_country(rich_main, "main"), by_country(rich_narrow, "narrow", boot = FALSE), by_country(rich_broad, "broad", boot = FALSE))
res_pooled <- rbind_fill(pooled(rich_main, "main"), pooled(rich_narrow, "narrow"), pooled(rich_broad, "broad"))
res_pooled_items <- rbind(pooled_items(rich_main, "main"), pooled_items(rich_broad, "broad"))
res_popavg <- popavg_country(res_country[res_country$spec == "main", ])
# Reference: int'l tax AMCE in the same full-dummy specification pooled over all 9 countries
ref9 <- fit_spec(d_all)$fit; ref9 <- lincom(ref9, c(foreign_policy1 = 1), "intl_tax (all 9 countries, full-dummy spec)")
write.csv(res_country, file.path(out_dir, "comparison_by_country.csv"), row.names = FALSE)
write.csv(rbind(res_pooled[, names(res_popavg)], res_popavg), file.path(out_dir, "comparison_pooled_summary.csv"), row.names = FALSE)
write.csv(res_pooled, file.path(out_dir, "comparison_pooled.csv"), row.names = FALSE)
write.csv(res_pooled_items, file.path(out_dir, "comparison_pooled_items.csv"), row.names = FALSE)
write.csv(ref9, file.path(out_dir, "intl_tax_all9_fulldummy.csv"), row.names = FALSE)
show <- function(x) print(cbind(x[, intersect(c("spec", "country", "dom_item"), names(x))], round(x[, sapply(x, is.numeric)], 4)), row.names = FALSE)
show(res_country); show(res_pooled); show(res_pooled_items); show(res_popavg); print(ref9)


##### 7. Secondary analyses #####
## (a) Heterogeneity by vote (pooled, main classification)
cat("\n=== Heterogeneity by vote (pooled main) ===\n")
vote_groups <- c("Left", "Center-right or Right", "Far right", "Non-voter, PNR or Other")
res_vote <- do.call(rbind, lapply(vote_groups, function(g) {
  r <- pooled(rich_main, "main", boot = FALSE, d0 = d_all[d_all$vote_factor %in% g, ])
  r$vote <- g; r
}))
# Differences between groups (independent subsamples)
gdiff <- function(g1, g2, v) { a <- res_vote[res_vote$vote == g1, ]; b <- res_vote[res_vote$vote == g2, ]
  est <- a[[v]] - b[[v]]; se <- sqrt(a[[paste0(v, "_se")]]^2 + b[[paste0(v, "_se")]]^2)
  data.frame(contrast = paste(g1, "minus", g2), quantity = v, estimate = est, se = se, p = 2*pnorm(-abs(est/se))) }
res_vote_contrasts <- do.call(rbind, lapply(list(c("Left", "Center-right or Right"), c("Left", "Far right"), c("Center-right or Right", "Far right"), c("Left", "Non-voter, PNR or Other")),
                                            function(g) do.call(rbind, lapply(c("intl", "dom", "diff"), function(v) gdiff(g[1], g[2], v)))))
write.csv(res_vote, file.path(out_dir, "heterogeneity_vote.csv"), row.names = FALSE)
write.csv(res_vote_contrasts, file.path(out_dir, "heterogeneity_vote_contrasts.csv"), row.names = FALSE)
show(res_vote); print(res_vote_contrasts, digits = 3)
cat("Note: no Far right category in the US (Trump voters coded Center-right or Right, as in vote_factor); rows with vote_factor NA:", sum(is.na(d_all$vote_factor)), "\n")

## (b) Interaction: int'l tax x domestic rich tax in the same program
cat("\n=== Interaction (substitution vs complementarity) ===\n")
inter <- function(rich_keys, set_name, weighted = TRUE) {
  r <- pooled(rich_keys, set_name, weighted = weighted, boot = FALSE, interaction = TRUE); f <- r$fit
  rbind(lincom(f, c(foreign_policy1 = 1), "intl_tax | no dom rich tax in program"),
        lincom(f, c(foreign_policy1 = 1, intl_x_dom_rich = 1), "intl_tax | dom rich tax in program"),
        lincom(f, c(dom_rich_tax = 1), "dom_rich_tax | no intl tax in program"),
        lincom(f, c(dom_rich_tax = 1, intl_x_dom_rich = 1), "dom_rich_tax | intl tax in program"),
        lincom(f, c(intl_x_dom_rich = 1), "interaction"),
        lincom(f, c(foreign_policy1 = 1, dom_rich_tax = 1, intl_x_dom_rich = 1), "both vs neither")) |> cbind(spec = set_name, weighted = weighted)
}
res_inter <- rbind(inter(rich_main, "main"), inter(rich_broad, "broad"), inter(rich_main, "main", weighted = FALSE))
# Country-level interactions (main)
res_inter_c <- do.call(rbind, lapply(unique(sub("_.*", "", rich_main)), function(c) {
  ks <- rich_main[startsWith(rich_main, c)]; d <- d_all[d_all$country == c, ]
  f <- fit_spec(d, rich_keys = ks, pool_rich = TRUE, interaction = TRUE)$fit
  cbind(lincom(f, c(intl_x_dom_rich = 1), "interaction"), country = c)
}))
write.csv(rbind(res_inter, cbind(res_inter_c[, 1:6], spec = paste0("main, ", res_inter_c$country), weighted = TRUE)), file.path(out_dir, "interaction.csv"), row.names = FALSE)
print(res_inter, digits = 3); print(res_inter_c, digits = 3)

## (c) Robustness: unweighted; consistent programs (consistent_conjoints_party)
cat("\n=== Robustness ===\n")
res_rob <- rbind(cbind(robustness = "unweighted", rbind(pooled(rich_main, "main", weighted = FALSE, boot = FALSE), pooled(rich_broad, "broad", weighted = FALSE, boot = FALSE))),
                 cbind(robustness = "consistent_conjoints_party", rbind(pooled(rich_main, "main", subset = "consistent_conjoints_party", boot = FALSE), pooled(rich_broad, "broad", subset = "consistent_conjoints_party", boot = FALSE))))
res_rob_c <- rbind(cbind(robustness = "unweighted", by_country(rich_main, "main", weighted = FALSE, boot = FALSE)),
                   cbind(robustness = "consistent_conjoints_party", by_country(rich_main, "main", subset = "consistent_conjoints_party", boot = FALSE)))
write.csv(rbind(res_rob, res_rob_c), file.path(out_dir, "robustness.csv"), row.names = FALSE)
show(res_rob); show(res_rob_c)

## (d) Sensitivity to the "no policy" baseline: within-domain demeaned contrast
# All AMCEs are relative to "no policy in the domain". If merely having a tax (resp. foreign) proposal is rewarded, domains differ in a generic "content" effect.
# Contrast: [AMCE(intl) - mean AMCE of all foreign-policy items] - [AMCE(dom rich tax) - mean AMCE of all tax-system items], by country (main items),
# and population-weighted average across countries. Also reported: mean AMCE of the other tax-system items (incl. tax cuts for the rich).
cat("\n=== Within-domain demeaned contrast (sensitivity) ===\n")
res_demeaned <- do.call(rbind, lapply(unique(sub("_.*", "", rich_main)), function(c) {
  k <- rich_main[startsWith(rich_main, c)]; d <- d_all[d_all$country == c, ]
  f <- fit_spec(d)$fit
  fp <- c("foreign_policy1", "foreign_policy2", paste0(c, "_foreign_policy3")); tx <- paste0(c, "_", policies$code[policies$country == c & policies$domain == "tax_system"])
  a <- setNames(rep(0, length(union(fp, tx))), union(fp, tx))
  a[fp] <- a[fp] - 1/length(fp); a["foreign_policy1"] <- a["foreign_policy1"] + 1; a[tx] <- a[tx] + 1/length(tx); a[k] <- a[k] - 1
  oth <- setdiff(tx, k)
  rbind(cbind(lincom(f, a, "demeaned diff: (intl - mean foreign) - (dom - mean tax)"), country = c),
        cbind(lincom(f, setNames(rep(1/length(oth), length(oth)), oth), "mean AMCE of other tax-system items"), country = c),
        cbind(lincom(f, setNames(rep(1/2, 2), c("foreign_policy2", paste0(c, "_foreign_policy3"))), "mean AMCE of other foreign-policy items"), country = c))
}))
pw <- function(term) { x <- res_demeaned[res_demeaned$term == term, ]; w <- adult_pop[x$country] / sum(adult_pop[x$country])
  est <- sum(w * x$estimate); se <- sqrt(sum(w^2 * x$se^2)); data.frame(term = term, estimate = est, se = se, p = 2*pnorm(-abs(est/se)), N_obs = sum(x$N_obs), N_resp = sum(x$N_resp), country = "Pop-weighted avg (FR,DE,ES,CH,JP,US)") }
res_demeaned <- rbind(res_demeaned, do.call(rbind, lapply(unique(res_demeaned$term), pw)))
write.csv(res_demeaned, file.path(out_dir, "demeaned_contrast.csv"), row.names = FALSE)
print(res_demeaned, digits = 3)

cat("\nSample: respondents", length(unique(d_all$n)), "; program rows", nrow(d_all), "; dropped respondents with no program info:", length(unique(long$n[long$no_program_info])), "\n")
sink()
