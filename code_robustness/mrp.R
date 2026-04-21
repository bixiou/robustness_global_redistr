# 6_mrp.R - Multilevel Regression and Poststratification (MRP)
# Must be run after 2_prepare.R (needs: all, pop_freq, adult_pop, countries, qs)
#
# US joint distribution: ACS 5-year PUMS weighted by PWGTP (via tidycensus).
# Other countries: independence approximation from pop_freq marginals
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
has_eurostat <- requireNamespace("eurostat", quietly = TRUE)
if (has_eurostat) suppressPackageStartupMessages(library(eurostat))

# Absolute cache path — resolved once at startup against the current working directory.
CACHE_DIR <- normalizePath(file.path("..", "data_ext", "pums_cache"), mustWork = FALSE)
message("Cache directory: ", CACHE_DIR)
dir.create(CACHE_DIR, showWarnings = FALSE, recursive = TRUE)

# US income quartile thresholds (2025 USD, inflated from historical ACS quartile breaks).
# Applied to ADJINC-adjusted PINCP.  To approximate deflation to ACS survey year t,
# multiply by (CPI_t / CPI_2025).  For ACS 2023: factor ≈ 0.942; for 2022 ≈ 0.920.
# These match the income-bracket thresholds shown to survey respondents.
us_income_q_breaks_2025 <- c(Q1Q2 = 36205, Q2Q3 = 72097, Q3Q4 = 129899)

# CPI scaling factors relative to 2025 for common ACS survey years
us_cpi_scale <- c("2019" = 0.874, "2020" = 0.875, "2021" = 0.899,
                  "2022" = 0.930, "2023" = 0.955, "2024" = 0.978, "2025" = 1.000)


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
    s <= 19              ~ "Upper secondary",   # includes HS, GED, some college/vocational (no degree)
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

build_us_pums_frame <- function(predictors, year = 2023, survey = "acs5",
                               cache_dir = CACHE_DIR) {
  readRenviron("~/.Renviron")
  dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
  pums_cache <- file.path(cache_dir, paste0("pums_ALL_", year, "_", survey, ".rds"))
  message("  PUMS cache file: ", pums_cache)

  # ST removed: ACS 2023 rejects it as a GET variable (state comes from the geo filter).
  # tidycensus auto-includes state FIPS as column "state"; we rename it to ST below.
  # CPI scaling on the fixed thresholds already normalises to the ACS survey year.
  pums_vars <- c("PWGTP","SEX","AGEP","SCHL","PINCP","RAC1P","HISP","ESR","PUMA")

  pums <- if (file.exists(pums_cache)) {
    message("  Loading ACS ", survey, " PUMS (", year, ") from cache...")
    readRDS(pums_cache)
  } else {
    message("  Downloading ACS ", survey, " PUMS (", year, ") — takes several minutes...")
    raw <- get_pums(variables = pums_vars, state = "all",
                    year = year, survey = survey, recode = FALSE) %>%
      dplyr::filter(as.numeric(AGEP) >= 18) %>%
      dplyr::rename(ST = state)
    tryCatch(saveRDS(raw, pums_cache),
             error = function(e) message("  PUMS cache write failed: ", conditionMessage(e)))
    raw
  }

  pums <- pums %>% dplyr::mutate(
    gender           = dplyr::case_when(SEX=="1"~"Man", SEX=="2"~"Woman", TRUE~NA_character_),
    age              = as.character(pums_age(AGEP)),
    education_quota  = pums_educ(SCHL, AGEP),
    race             = pums_race(RAC1P, HISP),
    region           = pums_region_us(ST),
    employment_18_64 = pums_empl(ESR, AGEP)
  )

  # Income quartiles using fixed 2025-dollar thresholds scaled to the ACS year.
  # This ensures the PUMS cells align with the survey brackets shown to respondents.
  scale <- as.numeric(us_cpi_scale[as.character(year)])
  if (is.na(scale)) scale <- 1.0
  q_breaks <- us_income_q_breaks_2025 * scale
  message("  Income quartile breaks (", year, " USD): ",
          paste(round(q_breaks), collapse = ", "))
  pums <- pums %>% dplyr::mutate(
    PINCP_adj = as.numeric(PINCP),   # nominal ACS-year dollars; thresholds CPI-scaled below
    income_quartile = dplyr::case_when(
      is.na(PINCP_adj) | PINCP_adj <= 0 ~ "Q1",
      PINCP_adj <= q_breaks["Q1Q2"]     ~ "Q1",
      PINCP_adj <= q_breaks["Q2Q3"]     ~ "Q2",
      PINCP_adj <= q_breaks["Q3Q4"]     ~ "Q3",
      TRUE                              ~ "Q4"
    )
  )

  # Urbanity via PUMA crosswalk (Census 2020 Urban Areas).
  # The survey assigns urbanity via zipcode_US.csv which also uses Census 2020 UA boundaries,
  # so both use the same source and the same three-category scheme
  # (Cities = UA ≥50 %, Towns & suburbs = UA+UC ≥25 %, Rural otherwise).
  # They differ only in spatial granularity (zip vs PUMA); any residual mismatch is minor.
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


##### Eurostat 3-way joint distribution: gender × age × education #####
#
# Replaces the independence assumption for these three correlated variables.
# Uses:
#   demo_pjan   — Population 1 Jan by single year of age and sex  (gender × age counts)
#   edat_lfs_9903 — Population by educational attainment, sex and age (%) giving
#                   P(education | gender, age group)
# Available for EU-27, EEA, UK, and CH.  Returns NULL (→ fall back) for other countries.

eurostat_countries <- c("AT","BE","BG","CY","CZ","DE","DK","EE","ES","FI","FR",
                        "GR","HR","HU","IE","IT","LT","LU","LV","MT","NL","PL",
                        "PT","RO","SE","SI","SK","GB","CH","NO","IS","LI","JP")

get_eurostat_gae_joint <- function(geo_code, year_target = 2022) {
  if (!has_eurostat) return(NULL)
  if (!geo_code %in% eurostat_countries) return(NULL)

  tryCatch({
    message("  Downloading Eurostat GAE joint for ", geo_code, "...")

    # ── 1. Education conditional: P(edu | gender, age_group) ──────────────────
    educ_raw <- eurostat::get_eurostat("edat_lfs_9903",
                                       filters  = list(geo = geo_code),
                                       cache    = TRUE,
                                       time_format = "num")
    yrs_e <- sort(unique(educ_raw$time[educ_raw$time <= year_target]), decreasing = TRUE)
    if (length(yrs_e) == 0) return(NULL)
    yr_e  <- yrs_e[1]

    educ <- educ_raw %>%
      dplyr::filter(time == yr_e,
                    unit    == "PC",
                    sex     %in% c("M","F"),
                    !isced11 %in% c("TOTAL","NRP"),
                    !age     %in% c("Y15-64","Y15T64","Y_GE15","TOTAL")) %>%
      dplyr::transmute(
        sex, age, isced11,
        pct = dplyr::coalesce(as.numeric(values), 0)
      )

    # Map Eurostat age codes to our 5 groups
    age_map <- c(
      "Y18-24"="18-24","Y15-24"="18-24","Y18T24"="18-24",
      "Y25-34"="25-34","Y25T34"="25-34",
      "Y35-44"="35-49","Y35T44"="35-49",
      "Y45-54"="35-49","Y45T54"="35-49",
      "Y50-64"="50-64","Y55-64"="50-64","Y50T64"="50-64","Y55T64"="50-64",
      "Y45-64"="50-64","Y45T64"="50-64",
      "Y65-74"="65+", "Y_GE65"="65+","Y65T74"="65+",
      "Y75-84"="65+", "Y_GE75"="65+"
    )

    educ <- educ %>%
      dplyr::mutate(
        age_grp          = age_map[age],
        gender           = dplyr::case_when(sex=="M"~"Man", sex=="F"~"Woman"),
        education_quota  = dplyr::case_when(
          isced11 %in% c("ED0-2","ED0_2","L_SEK1_L_SEK2") ~ "Below upper secondary",
          isced11 %in% c("ED3_4","ED3-4","SEK2_POST_SEK2") ~ "Upper secondary",
          isced11 %in% c("ED5-8","ED5_8","TER")            ~ "Post secondary",
          TRUE                                              ~ NA_character_
        )
      ) %>%
      dplyr::filter(!is.na(age_grp), !is.na(gender), !is.na(education_quota)) %>%
      dplyr::group_by(gender, age_grp, education_quota) %>%
      dplyr::summarize(pct = sum(pct, na.rm = TRUE), .groups = "drop")

    if (nrow(educ) == 0) return(NULL)

    # Normalise P(edu | gender, age_grp) so it sums to 1 per (gender, age_grp)
    educ <- educ %>%
      dplyr::group_by(gender, age_grp) %>%
      dplyr::mutate(pct_cond = pct / pmax(sum(pct, na.rm = TRUE), 1e-9)) %>%
      dplyr::ungroup()

    # ── 2. Gender × age marginal: counts from demo_pjan ────────────────────────
    demo_raw <- eurostat::get_eurostat("demo_pjan",
                                       filters  = list(geo = geo_code),
                                       cache    = TRUE,
                                       time_format = "num")
    yrs_d <- sort(unique(demo_raw$time[demo_raw$time <= year_target + 1]), decreasing = TRUE)
    if (length(yrs_d) == 0) return(NULL)
    yr_d  <- yrs_d[1]

    demo <- demo_raw %>%
      dplyr::filter(time == yr_d, sex %in% c("M","F"), age != "TOTAL", age != "UNK") %>%
      dplyr::mutate(
        age_n  = suppressWarnings(as.integer(gsub("^Y", "", age))),
        gender = dplyr::case_when(sex=="M"~"Man", sex=="F"~"Woman")
      ) %>%
      dplyr::filter(!is.na(age_n), age_n >= 18) %>%
      dplyr::mutate(
        age_grp = as.character(cut(age_n, breaks = c(17,24,34,49,64,Inf),
                                   labels = age_labels, right = TRUE))
      ) %>%
      dplyr::filter(!is.na(age_grp)) %>%
      dplyr::group_by(gender, age_grp) %>%
      dplyr::summarize(n_ga = sum(as.numeric(values), na.rm = TRUE), .groups = "drop") %>%
      dplyr::mutate(prop_ga = n_ga / sum(n_ga, na.rm = TRUE))

    if (nrow(demo) == 0) return(NULL)

    # ── 3. Combine into P(gender, age, education) ─────────────────────────────
    # For ages 18-24 and 65+: education_quota = "Not 25-64"
    all_combs <- expand.grid(
      gender          = gender_levels,
      age             = age_labels,
      education_quota = educ_levels,
      stringsAsFactors = FALSE
    )

    joint <- all_combs %>%
      dplyr::left_join(demo %>% dplyr::select(gender, age_grp, prop_ga),
                       by = c("gender", "age" = "age_grp")) %>%
      dplyr::left_join(educ %>% dplyr::select(gender, age_grp, education_quota, pct_cond),
                       by = c("gender", "age" = "age_grp", "education_quota")) %>%
      dplyr::mutate(
        # Ages 18-24 / 65+ → only "Not 25-64" has non-zero weight
        pct_cond = dplyr::case_when(
          age %in% c("18-24","65+") & education_quota == "Not 25-64" ~ 1.0,
          age %in% c("18-24","65+")                                   ~ 0.0,
          age %in% c("25-34","35-49","50-64") & education_quota == "Not 25-64" ~ 0.0,
          TRUE ~ dplyr::coalesce(pct_cond, 0.0)
        ),
        prop_ga   = dplyr::coalesce(prop_ga, 0.0),
        joint_prop = prop_ga * pct_cond
      ) %>%
      dplyr::filter(joint_prop > 0) %>%
      dplyr::mutate(joint_prop = joint_prop / sum(joint_prop, na.rm = TRUE))

    message("  Eurostat GAE joint for ", geo_code, ": ", nrow(joint),
            " cells (year educ=", yr_e, ", demo=", yr_d, ")")
    joint[, c("gender","age","education_quota","joint_prop")]

  }, error = function(e) {
    message("  Eurostat GAE joint failed for ", geo_code, ": ", conditionMessage(e))
    NULL
  })
}


##### Non-US census frames: joint or independence-based #####
#
# For countries with Eurostat data (EU+GB+CH+JP):
#   P(gender,age,education,income,urbanity,region) ≈
#       P(gender,age,education) [Eurostat 3-way]  ×
#       P(income) × P(urbanity) × P(region)       [independence]
# This removes the most problematic independence assumption (education × age correlation).
#
# For other countries (RU, SA, JP if Eurostat unavailable):
#   Full product of 1-D marginals from pop_freq (independence approximation).

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

  # Try Eurostat gender × age × education joint distribution
  gae_preds <- c("gender","age","education_quota")
  use_gae   <- all(gae_preds %in% use_preds)
  gae_joint <- if (use_gae) get_eurostat_gae_joint(ctry) else NULL

  if (!is.null(gae_joint)) {
    # ── Path A: Eurostat joint for GAE × independent marginals for rest ────────
    other_preds <- setdiff(use_preds, gae_preds)

    if (length(other_preds) > 0) {
      other_lev   <- lev[other_preds]
      other_combs <- expand.grid(other_lev, stringsAsFactors = FALSE)

      other_prob <- rep(1.0, nrow(other_combs))
      for (pred in other_preds) {
        mv <- get_marg1d(pred)
        if (!is.null(mv)) other_prob <- other_prob * as.numeric(mv[other_combs[[pred]]])
      }
      other_prob[is.na(other_prob) | other_prob < 0] <- 1e-12
      other_prob <- other_prob / sum(other_prob)

      # Full cross-product: (GAE rows) × (other rows)
      idx_g <- rep(seq_len(nrow(gae_joint)),  each = nrow(other_combs))
      idx_o <- rep(seq_len(nrow(other_combs)), times = nrow(gae_joint))
      combs      <- cbind(gae_joint [idx_g, gae_preds, drop=FALSE],
                          other_combs[idx_o, ,         drop=FALSE])
      joint_prob <- gae_joint$joint_prop[idx_g] * other_prob[idx_o]
    } else {
      combs      <- gae_joint[, gae_preds, drop = FALSE]
      joint_prob <- gae_joint$joint_prop
    }

    joint_prob[is.na(joint_prob) | joint_prob < 0] <- 1e-12
    joint_prob <- joint_prob / sum(joint_prob)
    method     <- "Eurostat GAE + independent marginals"

  } else {
    # ── Path B: Full independence from pop_freq 1-D marginals (fallback) ───────
    combs      <- expand.grid(lev, stringsAsFactors = FALSE)
    joint_prob <- rep(1.0, nrow(combs))
    for (pred in use_preds) {
      mv <- get_marg1d(pred)
      if (!is.null(mv)) joint_prob <- joint_prob * as.numeric(mv[combs[[pred]]])
    }
    joint_prob[is.na(joint_prob) | joint_prob < 0] <- 1e-12
    joint_prob <- joint_prob / sum(joint_prob)
    method     <- "independence (pop_freq marginals)"
  }

  frame      <- as.data.frame(combs, stringsAsFactors = FALSE)
  frame$n    <- joint_prob * pop
  frame      <- frame[frame$n > 0, ]
  if ("region" %in% names(frame)) frame$region <- as.character(frame$region)

  message("  ", ctry, " census frame [", method, "]: ",
          nrow(frame), " cells, N = ", format(round(sum(frame$n)), big.mark = ","))
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
    build_us_pums_frame(preds_US_default,  year = 2023),
    error = function(e) { message("  US default PUMS failed: ", conditionMessage(e)); NULL }
  )
  census_frames$US_extended <- tryCatch(
    build_us_pums_frame(preds_US_extended, year = 2023),
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

# --- Other countries (cross product from pop_freq marginals) ---
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


##### Margin comparison: pop_freq vs census frames ##############################
#
# For each country and each 1-D predictor, compare:
#   - pop_freq marginals (used for survey weighting / stored in sources.xlsx)
#   - Census-frame marginals (derived from PUMS or Eurostat + independent marginals)
# Flags deviations > 5 percentage points.

compute_margin_comparison <- function(ctry, census_frame, predictors) {
  if (is.null(census_frame) || nrow(census_frame) == 0) return(NULL)
  marg <- pop_freq[[ctry]]
  rows <- list()
  for (pred in intersect(predictors, names(marg))) {
    if (!pred %in% names(census_frame)) next
    # Census frame marginal
    cf_tab <- tapply(census_frame$n, census_frame[[pred]], sum, na.rm = TRUE)
    cf_tab <- cf_tab / sum(cf_tab, na.rm = TRUE)
    # pop_freq marginal
    pf_raw <- marg[[pred]]
    pf_tab <- setNames(as.numeric(pf_raw) / sum(as.numeric(pf_raw), na.rm = TRUE),
                       names(pf_raw))
    lvls <- union(names(cf_tab), names(pf_tab))
    for (lv in lvls) {
      cf_pct <- if (!is.na(cf_tab[lv]) && !is.null(cf_tab[lv])) cf_tab[lv] else NA_real_
      pf_pct <- if (!is.na(pf_tab[lv]) && !is.null(pf_tab[lv])) pf_tab[lv] else NA_real_
      dev    <- cf_pct - pf_pct
      rows[[length(rows)+1]] <- data.frame(
        country    = ctry,
        predictor  = pred,
        level      = as.character(lv),
        census_pct = round(100 * cf_pct, 2),
        popfreq_pct = round(100 * pf_pct, 2),
        deviation_pp = round(100 * dev, 2),
        flag = abs(dev) > 0.05 & !is.na(dev),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}


##### Representation analysis: over/under-represented cells ####################
#
# For each country, compute the ratio  (survey share) / (census-frame share)
# for every cell formed by the cross of quota predictors.
# Ratios > 1.5 or < 0.67 (i.e. more than 50 % off) are flagged.

compute_representation <- function(ctry, survey_df, census_frame, predictors) {
  if (is.null(census_frame) || nrow(census_frame) == 0) return(NULL)
  use_preds <- intersect(predictors, names(census_frame))
  use_preds <- use_preds[use_preds %in% names(survey_df)]
  if (length(use_preds) == 0) return(NULL)

  # Align character types
  for (col in use_preds) {
    survey_df[[col]]    <- as.character(survey_df[[col]])
    census_frame[[col]] <- as.character(census_frame[[col]])
  }

  # Survey shares (pairwise intersections for readability)
  rows <- list()
  for (i in seq_along(use_preds)) {
    for (j in seq_along(use_preds)) {
      if (j <= i) next
      p1 <- use_preds[i]; p2 <- use_preds[j]
      # Survey distribution
      sv <- survey_df[!is.na(survey_df[[p1]]) & !is.na(survey_df[[p2]]), ]
      if (nrow(sv) < 5) next
      sv_tab <- as.data.frame(table(sv[[p1]], sv[[p2]]), stringsAsFactors = FALSE)
      names(sv_tab) <- c("lv1","lv2","sv_n")
      sv_tab$sv_pct <- sv_tab$sv_n / sum(sv_tab$sv_n, na.rm = TRUE)
      # Census frame distribution
      cf <- census_frame[!is.na(census_frame[[p1]]) & !is.na(census_frame[[p2]]), ]
      cf_tab <- cf %>%
        dplyr::group_by(lv1 = .data[[p1]], lv2 = .data[[p2]]) %>%
        dplyr::summarize(cf_n = sum(n, na.rm = TRUE), .groups = "drop") %>%
        dplyr::mutate(cf_pct = cf_n / sum(cf_n, na.rm = TRUE))
      comb <- dplyr::left_join(sv_tab, cf_tab, by = c("lv1","lv2"))
      comb$ratio <- comb$sv_pct / pmax(comb$cf_pct, 1e-6)
      comb$country <- ctry; comb$var1 <- p1; comb$var2 <- p2
      comb$flag <- comb$ratio > 1.5 | comb$ratio < 0.67
      rows[[length(rows)+1]] <- comb[, c("country","var1","var2","lv1","lv2",
                                          "sv_pct","cf_pct","ratio","flag")]
    }
  }
  if (length(rows) == 0) return(NULL)
  res <- do.call(rbind, rows)
  res <- res[order(res$ratio, decreasing = TRUE), ]
  row.names(res) <- NULL
  res
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


##### Margin comparison & representation tables #################################

message("\n=== Computing margin comparisons ===\n")
margin_rows  <- list()
repres_rows  <- list()

for (ctry in countries) {
  preds  <- preds_by_country[[ctry]]
  cf_key <- if (ctry == "US") "US_default" else ctry
  cf     <- census_frames[[cf_key]]
  sv     <- all[all$country == ctry, ]

  mc <- compute_margin_comparison(ctry, cf, preds)
  if (!is.null(mc)) margin_rows[[ctry]] <- mc

  rp <- compute_representation(ctry, sv, cf, preds)
  if (!is.null(rp)) repres_rows[[ctry]] <- rp
}

margin_comparison <- do.call(rbind, margin_rows)
representation    <- do.call(rbind, repres_rows)

if (!is.null(margin_comparison) && nrow(margin_comparison) > 0) {
  cat("\n=== Margin comparison (flagged rows, deviation > 5 pp) ===\n")
  print(margin_comparison[margin_comparison$flag, ], row.names = FALSE)
}
if (!is.null(representation) && nrow(representation) > 0) {
  cat("\n=== Most over-represented cells (ratio > 1.5) ===\n")
  over <- representation[representation$flag & representation$ratio > 1, ]
  print(utils::head(over[order(over$ratio, decreasing = TRUE), ], 30), row.names = FALSE)
  cat("\n=== Most under-represented cells (ratio < 0.67) ===\n")
  under <- representation[representation$flag & representation$ratio < 1, ]
  print(utils::head(under[order(under$ratio), ], 30), row.names = FALSE)
}


##### Save ######################################################################

saveRDS(mrp_results,       "../data_ext/mrp_results.rds")
saveRDS(census_frames,     "../data_ext/mrp_census_frames.rds")
if (!is.null(mrp_summary))
  write.csv(mrp_summary,       "../data_ext/mrp_summary.csv",         row.names = FALSE)
if (!is.null(margin_comparison))
  write.csv(margin_comparison, "../data_ext/mrp_margin_comparison.csv", row.names = FALSE)
if (!is.null(representation))
  write.csv(representation,    "../data_ext/mrp_representation.csv",   row.names = FALSE)

message("\nDone. Saved mrp_results.rds, mrp_summary.csv, mrp_margin_comparison.csv, ",
        "mrp_representation.csv")
