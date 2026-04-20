# 6_mrp.R - Multilevel Regression and Poststratification (MRP)
# Must be run after 2_prepare.R (needs: all, pop_freq, adult_pop, countries, qs)
#
# US joint distribution: ACS 5-year PUMS weighted by PWGTP (via tidycensus).
# Other countries: independence approximation / IPF from pop_freq marginals
#                  (official statistics already aggregated in sources.xlsx).
# MRP model: glmer (binomial) with random intercepts per predictor.
#
# Note: the .RData loads Hmisc which masks dplyr::summarize.
#       All summarize calls below use dplyr::summarize explicitly.

##### Libraries #####
suppressPackageStartupMessages({
  library(lme4)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(Hmisc)
  library(tidycensus)
})


##### Predictor definitions #####

age_labels   <- c("18-24", "25-34", "35-49", "50-64", "65+")
educ_levels  <- c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64")
urban_levels <- c("Cities", "Towns and suburbs", "Rural")
income_levels <- c("Q1", "Q2", "Q3", "Q4")
gender_levels <- c("Woman", "Man")

preds_default     <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region")
preds_US_default  <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region", "race")
preds_US_extended <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region",
                       "race", "employment_18_64")

preds_by_country <- list(
  FR = preds_default, DE = preds_default, IT = preds_default, PL = preds_default,
  ES = preds_default, CH = preds_default, GB = preds_default, JP = preds_default,
  RU = c("gender", "age", "education_quota", "income_quartile"),
  SA = c("gender", "age", "education_quota", "income_quartile", "region"),
  US = preds_US_default
)

# Number of active regions per country
n_reg <- c(FR=5, DE=3, IT=2, PL=2, ES=5, CH=3, GB=5, JP=5, RU=4, SA=4, US=4)


##### PUMS recoding helpers (US) #####

pums_age <- function(agep) {
  cut(as.numeric(agep), breaks = c(17,24,34,49,64,Inf),
      labels = age_labels, right = TRUE)
}

pums_educ <- function(schl, agep) {
  age <- as.numeric(agep); s <- as.numeric(schl)
  dplyr::case_when(
    age < 25 | age >= 65 ~ "Not 25-64",
    s <= 15              ~ "Below upper secondary",
    s <= 17              ~ "Upper secondary",
    TRUE                 ~ "Post secondary"
  )
}

pums_race <- function(rac1p, hisp) {
  dplyr::case_when(
    hisp != "01" ~ "Hispanic",
    rac1p == "1" ~ "White only",
    rac1p == "2" ~ "Black",
    TRUE         ~ "Other"
  )
}

pums_empl <- function(esr, agep) {
  age <- as.numeric(agep)
  dplyr::case_when(
    age >= 65                   ~ "65+",
    esr %in% c("1","2","4","5") ~ "Employed",
    esr == "3"                  ~ "Unemployed",
    TRUE                        ~ "Inactive"
  )
}

pums_region_us <- function(st) {
  st <- as.integer(st)
  dplyr::case_when(
    st %in% c(9,23,25,33,44,50,34,36,42)                           ~ "1",
    st %in% c(17,18,26,39,55,19,20,27,29,31,38,46)                 ~ "2",
    st %in% c(10,11,12,13,24,37,45,51,54,1,21,28,47,5,22,40,48)    ~ "3",
    st %in% c(4,8,16,30,32,35,49,56,2,6,15,41,53)                  ~ "4",
    TRUE                                                             ~ NA_character_
  )
}


##### US PUMS: build census frame (true joint distribution) #####

build_us_pums_frame <- function(predictors, year = 2022, survey = "acs5") {
  readRenviron("~/.Renviron")
  message("  Downloading ACS ", survey, " PUMS (", year, ") — takes several minutes...")

  pums_vars <- c("PWGTP","SEX","AGEP","SCHL","PINCP","ST","RAC1P","HISP","ESR","PUMA")
  pums <- get_pums(variables = pums_vars, state = "all",
                   year = year, survey = survey, recode = FALSE) %>%
    dplyr::filter(as.numeric(AGEP) >= 18)

  pums <- pums %>% dplyr::mutate(
    gender           = dplyr::case_when(SEX=="1"~"Man", SEX=="2"~"Woman", TRUE~NA_character_),
    age              = as.character(pums_age(AGEP)),
    education_quota  = pums_educ(SCHL, AGEP),
    race             = pums_race(RAC1P, HISP),
    region           = pums_region_us(ST),
    employment_18_64 = pums_empl(ESR, AGEP)
  )

  # Income quartiles from weighted distribution
  inc <- as.numeric(pums$PINCP); wt <- as.numeric(pums$PWGTP)
  pos <- !is.na(inc) & inc > 0
  q_breaks <- wtd.quantile(inc[pos], weights = wt[pos], probs = c(0.25, 0.5, 0.75))
  pums <- pums %>% dplyr::mutate(
    income_quartile = dplyr::case_when(
      is.na(as.numeric(PINCP)) | as.numeric(PINCP) <= 0 ~ "Q1",
      as.numeric(PINCP) <= q_breaks[1] ~ "Q1",
      as.numeric(PINCP) <= q_breaks[2] ~ "Q2",
      as.numeric(PINCP) <= q_breaks[3] ~ "Q3",
      TRUE                              ~ "Q4"
    )
  )

  # Urbanity via PUMA crosswalk (Census 2020 urban areas)
  puma_urban <- tryCatch({
    message("  Downloading PUMA-to-urbanicity crosswalk...")
    url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/ua_puma_rel_2020.txt"
    xw  <- read.csv(url(url), stringsAsFactors = FALSE)
    xw %>%
      dplyr::mutate(is_ua = !is.na(UANAME) & UATYPE == "UA") %>%
      dplyr::group_by(STATEFP, PUMACE) %>%
      dplyr::summarize(
        pop_ua    = sum(ifelse(is_ua, POPPT, 0), na.rm = TRUE),
        pop_uc    = sum(ifelse(!is_ua & !is.na(UANAME), POPPT, 0), na.rm = TRUE),
        pop_total = sum(POPPT, na.rm = TRUE),
        .groups   = "drop"
      ) %>%
      dplyr::mutate(
        pct_ua   = pop_ua / pmax(pop_total, 1),
        pct_uc   = pop_uc / pmax(pop_total, 1),
        urbanity = dplyr::case_when(
          pct_ua >= 0.5              ~ "Cities",
          (pct_ua + pct_uc) >= 0.25 ~ "Towns and suburbs",
          TRUE                       ~ "Rural"
        ),
        ST_int   = as.integer(STATEFP),
        PUMA_int = as.integer(PUMACE)
      ) %>%
      dplyr::select(ST_int, PUMA_int, urbanity)
  }, error = function(e) {
    message("  Crosswalk failed (", conditionMessage(e), "). Using national marginals proxy.")
    NULL
  })

  if (!is.null(puma_urban)) {
    pums <- pums %>%
      dplyr::mutate(ST_int = as.integer(ST), PUMA_int = as.integer(PUMA)) %>%
      dplyr::left_join(puma_urban, by = c("ST_int", "PUMA_int"))
  } else {
    u <- pop_freq$US$urbanity
    if (is.null(u)) u <- c(Cities=0.596, `Towns and suburbs`=0.144, Rural=0.260)
    u <- u / sum(u, na.rm = TRUE)
    set.seed(42)
    pums$urbanity <- sample(names(u), nrow(pums), replace = TRUE, prob = u)
    warning("US urbanity randomly assigned from national marginals.")
  }

  # Build joint distribution
  preds_avail <- intersect(predictors, names(pums))
  frame <- pums %>%
    dplyr::filter(dplyr::across(dplyr::all_of(preds_avail), ~ !is.na(.x) & .x != "")) %>%
    dplyr::mutate(PWGTP = as.numeric(PWGTP)) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(preds_avail))) %>%
    dplyr::summarize(n = sum(PWGTP, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(n > 0) %>%
    dplyr::mutate(region = as.character(region))

  message("  US PUMS frame: ", nrow(frame), " cells, N = ",
          format(round(sum(frame$n)), big.mark = ","))
  frame
}


##### Non-US census frames: independence from pop_freq marginals #####
#
# Joint distribution = product of 1-D marginal proportions.
# pop_freq already contains official statistics (Eurostat, Rosstat, etc.)
# compiled in questionnaire/sources.xlsx.

build_country_census_frame <- function(ctry, predictors) {
  marg <- pop_freq[[ctry]]
  pop  <- adult_pop[[ctry]]
  nr   <- n_reg[[ctry]]

  # Active levels per predictor
  lev <- list(
    gender           = gender_levels,
    age              = age_labels,
    education_quota  = educ_levels,
    income_quartile  = income_levels,
    urbanity         = urban_levels,
    region           = as.character(seq_len(nr)),
    race             = c("White only", "Hispanic", "Black", "Other"),
    employment_18_64 = c("Inactive", "Unemployed", "Employed", "65+"),
    vote             = c("Left", "Center-right or Right", "Far right", "Non-voter, PNR or Other")
  )
  use_preds <- intersect(predictors, names(lev))
  lev <- lev[use_preds]

  # Restrict region to levels that have positive population
  if ("region" %in% use_preds) {
    rv <- marg$region
    if (!is.null(rv)) {
      active <- names(rv[as.numeric(rv) > 0])
      active <- intersect(active, as.character(seq_len(nr)))
      if (length(active) > 0) lev$region <- active
    }
  }

  # Helper: normalized 1-D marginal, with name remapping for known cases
  race_name_map <- c(
    "White.non.Hispanic" = "White only",
    "White non-Hispanic" = "White only"
  )

  get_marg1d <- function(pred) {
    v <- marg[[pred]]
    if (is.null(v)) return(NULL)
    # Remap known alternative names
    if (pred == "race") names(v) <- dplyr::coalesce(race_name_map[names(v)], names(v))
    lv  <- lev[[pred]]
    out <- setNames(rep(1e-8, length(lv)), lv)
    m   <- intersect(names(v), lv)
    out[m] <- pmax(as.numeric(v[m]), 1e-8)
    out / sum(out)
  }

  # All cells (Cartesian product)
  combs <- expand.grid(lev, stringsAsFactors = FALSE)

  # Joint probability = product of marginals (independence assumption)
  joint_prob <- rep(1.0, nrow(combs))
  for (pred in use_preds) {
    mv <- get_marg1d(pred)
    if (!is.null(mv)) joint_prob <- joint_prob * as.numeric(mv[combs[[pred]]])
  }
  joint_prob[is.na(joint_prob) | joint_prob < 0] <- 1e-12
  joint_prob <- joint_prob / sum(joint_prob)

  frame      <- combs
  frame$n    <- joint_prob * pop
  frame      <- frame[frame$n > 0, ]
  if ("region" %in% names(frame)) frame$region <- as.character(frame$region)

  message("  ", ctry, " census frame: ", nrow(frame), " cells, N = ",
          format(round(sum(frame$n)), big.mark = ","))
  frame
}


##### Build all census frames #####

message("\n=== Building census frames ===\n")
census_frames <- list()

# --- US (PUMS) ---
message("--- US ---")
readRenviron("~/.Renviron")
has_key <- Sys.getenv("CENSUS_API_KEY") != ""

if (has_key) {
  message("  Census API key found. Attempting PUMS download for US.")
  census_frames$US_default <- tryCatch(
    build_us_pums_frame(preds_US_default,  year = 2021),
    error = function(e) { message("  US default PUMS failed: ", conditionMessage(e)); NULL }
  )
  census_frames$US_extended <- tryCatch(
    build_us_pums_frame(preds_US_extended, year = 2021),
    error = function(e) { message("  US extended PUMS failed: ", conditionMessage(e)); NULL }
  )
}

# Fallback to independence approximation if PUMS frames are missing
if (is.null(census_frames$US_default)) {
  message("  Falling back to independence approximation for US.")
  census_frames$US_default  <- build_country_census_frame("US", preds_US_default)
  census_frames$US_extended <- build_country_census_frame("US", preds_US_extended)
}
census_frames$US <- census_frames$US_default

# --- Other countries (IPF from pop_freq marginals) ---
for (ctry in setdiff(countries, "US")) {
  message("\n--- ", ctry, " ---")
  census_frames[[ctry]] <- tryCatch(
    build_country_census_frame(ctry, preds_by_country[[ctry]]),
    error = function(e) { message("  Failed: ", conditionMessage(e)); NULL }
  )
}


##### MRP function #####

apply_mrp <- function(survey_df, outcome, predictors, census_frame, country = "") {
  if (is.null(census_frame) || nrow(census_frame) == 0) {
    message("  [SKIP] ", country, " (", outcome, "): no census frame"); return(NULL)
  }

  # gcs_support is a double.item (100="Yes", 0="No"); gcs_support_control is plain numeric.
  # Handle both cases: if numeric (not double.item), treat >0 as Yes; else compare to "Yes".
  v <- survey_df[[outcome]]
  if (is.numeric(v) && !inherits(v, "double.item")) {
    survey_df$outcome_bin <- as.numeric(v > 0)   # 100 -> 1, 0 -> 0, NA -> NA
  } else {
    survey_df$outcome_bin <- as.numeric(v == "Yes")
  }

  # Align character types
  for (col in c("region", "age")) {
    if (col %in% names(survey_df))    survey_df[[col]]    <- as.character(survey_df[[col]])
    if (col %in% names(census_frame)) census_frame[[col]] <- as.character(census_frame[[col]])
  }

  avail   <- intersect(c("outcome_bin", predictors), names(survey_df))
  df_cln  <- survey_df[complete.cases(survey_df[, avail]), ]

  if (nrow(df_cln) < 30) {
    message("  [SKIP] ", country, " (", outcome, "): n=", nrow(df_cln)); return(NULL)
  }

  preds_ok <- intersect(predictors, names(df_cln))
  preds_ok <- preds_ok[vapply(preds_ok, function(p)
    length(unique(na.omit(df_cln[[p]]))) >= 2, logical(1))]

  if (length(preds_ok) == 0) {
    message("  [SKIP] ", country, " (", outcome, "): no valid predictors"); return(NULL)
  }

  formula_str <- paste0("outcome_bin ~ 1 + ",
                        paste0("(1|", preds_ok, ")", collapse = " + "))
  message("  Fitting ", country, "/", outcome,
          "  n=", nrow(df_cln), "  preds=", paste(preds_ok, collapse=","))

  model <- tryCatch(
    lme4::glmer(as.formula(formula_str), data = df_cln, family = binomial,
                control = lme4::glmerControl(optimizer = "bobyqa",
                                             optCtrl   = list(maxfun = 3e5))),
    error = function(e) { message("  glmer error: ", conditionMessage(e)); NULL }
  )
  if (is.null(model)) return(NULL)

  census_frame$pred <- tryCatch(
    predict(model, newdata = census_frame, type = "response", allow.new.levels = TRUE),
    error = function(e) { message("  predict error: ", conditionMessage(e))
                          rep(NA_real_, nrow(census_frame)) }
  )

  ok  <- !is.na(census_frame$pred)
  if (!any(ok)) { message("  No valid predictions for ", country); return(NULL) }

  est <- sum(census_frame$pred[ok] * census_frame$n[ok]) / sum(census_frame$n[ok])
  raw <- mean(df_cln$outcome_bin, na.rm = TRUE)

  message("  >> ", country, " | ", outcome,
          "  MRP=", round(est, 3), "  raw=", round(raw, 3))

  list(country = country, outcome = outcome,
       estimate_mrp = est, estimate_raw = raw,
       n_survey = nrow(df_cln), model = model, frame = census_frame)
}


##### Apply MRP to all countries #####

message("\n=== Applying MRP ===\n")
mrp_results <- list()

# US
us_df <- all[all$country == "US", ]
mrp_results$US_gcs_support <- apply_mrp(
  us_df, "gcs_support", preds_US_default, census_frames$US_default, "US")
mrp_results$US_gcs_support_control <- apply_mrp(
  us_df, "gcs_support_control", preds_US_extended, census_frames$US_extended, "US")

# Other countries
for (ctry in setdiff(countries, "US")) {
  df    <- all[all$country == ctry, ]
  preds <- preds_by_country[[ctry]]
  frame <- census_frames[[ctry]]

  mrp_results[[paste0(ctry, "_gcs_support")]] <- apply_mrp(
    df, "gcs_support", preds, frame, ctry)
  mrp_results[[paste0(ctry, "_gcs_support_control")]] <- apply_mrp(
    df, "gcs_support_control", preds, frame, ctry)
}


##### Summary table #####

mrp_summary <- do.call(rbind, lapply(mrp_results, function(r) {
  if (is.null(r)) return(NULL)
  data.frame(country = r$country, outcome = r$outcome,
             estimate_mrp = round(r$estimate_mrp, 4),
             estimate_raw = round(r$estimate_raw, 4),
             n_survey = r$n_survey, stringsAsFactors = FALSE)
}))

if (!is.null(mrp_summary) && nrow(mrp_summary) > 0) {
  cat("\n=== MRP Results ===\n")
  print(mrp_summary, row.names = FALSE)

  mrp_wide <- tidyr::pivot_wider(
    mrp_summary[, c("country","outcome","estimate_mrp")],
    id_cols     = "country",
    names_from  = "outcome",
    values_from = "estimate_mrp",
    names_prefix = "mrp_"
  )
  cat("\nWide format:\n")
  print(mrp_wide, row.names = FALSE)
}

saveRDS(mrp_results,   "../data_ext/mrp_results.rds")
saveRDS(census_frames, "../data_ext/mrp_census_frames.rds")
if (!is.null(mrp_summary))
  write.csv(mrp_summary, "../data_ext/mrp_summary.csv", row.names = FALSE)

message("\nDone. Results saved to data_ext/mrp_results.rds and mrp_summary.csv")
