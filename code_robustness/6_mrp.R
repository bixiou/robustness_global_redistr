# 6_mrp.R - Multilevel Regression and Poststratification (MRP)
# Must be run after 2_prepare.R (needs: all, pop_freq, adult_pop, countries, qs)
#
# US joint distribution: ACS PUMS weighted by PWGTP (via tidycensus).
# Other countries: IPF from official marginals in pop_freq (sources: Eurostat, national stats).
# MRP model: glmer (binomial) with random intercepts per predictor cell.
#
# Outcomes:  gcs_support        (binary Yes/No)
#            gcs_support_control (gcs_support when variant_warm_glow == "None")
#
# Default predictors  (all countries): gender, age, education_quota, income_quartile, urbanity, region
# US default predictors               : same + race
# US extended (gcs_support_control)  : same + employment_18_64
# (vote excluded from census frames — not available in PUMS or comparable international censuses)

##### Libraries #####
pkgs <- c("lme4", "tidyverse", "Hmisc", "mipfp", "tidycensus", "eurostat")
for (p in pkgs) if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
library(lme4)
library(tidyverse)
library(Hmisc)     # wtd.quantile
library(mipfp)     # Ipfp


##### Predictor definitions #####

age_labels  <- c("18-24", "25-34", "35-49", "50-64", "65+")
educ_levels <- c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64")
urban_levels <- c("Cities", "Towns and suburbs", "Rural")

preds_default     <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region")
preds_US_default  <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region", "race")
preds_US_extended <- c("gender", "age", "education_quota", "income_quartile", "urbanity", "region",
                       "race", "employment_18_64")

# Per-country predictor lists (match the quotas used for weighting in 2_prepare.R)
preds_by_country <- list(
  FR = preds_default,
  DE = preds_default,
  IT = preds_default,
  PL = preds_default,
  ES = preds_default,
  CH = preds_default,
  GB = preds_default,
  JP = preds_default,
  RU = c("gender", "age", "education_quota", "income_quartile"),             # quotas$RU
  SA = c("gender", "age", "education_quota", "income_quartile", "region"),   # no urbanity for SA
  US = preds_US_default
)

# Number of active regions per country (Region.5 = 0 for IT, PL, DE, CH, RU, SA, US)
n_reg <- c(FR=5, DE=3, IT=2, PL=2, ES=5, CH=3, GB=5, JP=5, RU=4, SA=4, US=4)


##### PUMS recoding helpers (US) #####

pums_age <- function(agep) {
  cut(as.numeric(agep), breaks = c(17,24,34,49,64,Inf),
      labels = age_labels, right = TRUE)
}

pums_educ <- function(schl, agep) {
  # SCHL codes: 1-15=<HS, 16-17=HS diploma/GED, 18-24=post-secondary
  age <- as.numeric(agep)
  dplyr::case_when(
    age < 25 | age >= 65    ~ "Not 25-64",
    as.numeric(schl) <= 15  ~ "Below upper secondary",
    as.numeric(schl) <= 17  ~ "Upper secondary",
    TRUE                    ~ "Post secondary"
  )
}

pums_race <- function(rac1p, hisp) {
  # HISP: "01"=non-Hispanic; RAC1P: 1=White, 2=Black
  dplyr::case_when(
    hisp != "01" ~ "Hispanic",
    rac1p == "1" ~ "White only",
    rac1p == "2" ~ "Black",
    TRUE         ~ "Other"
  )
}

pums_empl <- function(esr, agep) {
  # ESR: 1,2=employed (civilian), 3=unemployed, 4,5=armed forces, 6=not in LF
  age <- as.numeric(agep)
  dplyr::case_when(
    age >= 65               ~ "65+",
    esr %in% c("1","2","4","5") ~ "Employed",
    esr == "3"              ~ "Unemployed",
    TRUE                    ~ "Inactive"
  )
}

pums_region_us <- function(st) {
  # 4 US Census regions mapped to survey regions 1-4 (Region.5 absent for US)
  st <- as.integer(st)
  dplyr::case_when(
    st %in% c(9,23,25,33,44,50,34,36,42)                                 ~ "1",
    st %in% c(17,18,26,39,55,19,20,27,29,31,38,46)                       ~ "2",
    st %in% c(10,11,12,13,24,37,45,51,54,1,21,28,47,5,22,40,48)          ~ "3",
    st %in% c(4,8,16,30,32,35,49,56,2,6,15,41,53)                        ~ "4",
    TRUE                                                                   ~ NA_character_
  )
}


##### Download US ACS PUMS and compute joint distribution #####

build_us_pums_frame <- function(predictors, year = 2022, survey = "acs5") {
  library(tidycensus)

  message("  Downloading ACS ", survey, " PUMS (", year, ") — may take several minutes...")
  pums_vars <- c("PWGTP","SEX","AGEP","SCHL","PINCP","ST","RAC1P","HISP","ESR","PUMA")

  pums <- get_pums(
    variables = pums_vars, state = "all",
    year = year, survey = survey, recode = FALSE
  ) %>% filter(as.numeric(AGEP) >= 18)

  # Recode demographics
  pums <- pums %>% mutate(
    gender          = dplyr::case_when(SEX=="1"~"Man", SEX=="2"~"Woman", TRUE~NA_character_),
    age             = as.character(pums_age(AGEP)),
    education_quota = pums_educ(SCHL, AGEP),
    race            = pums_race(RAC1P, HISP),
    region          = pums_region_us(ST),
    employment_18_64 = pums_empl(ESR, AGEP)
  )

  # Income quartiles from weighted distribution (person income > 0)
  inc <- as.numeric(pums$PINCP)
  wt  <- as.numeric(pums$PWGTP)
  pos <- !is.na(inc) & inc > 0
  qs_income <- wtd.quantile(inc[pos], weights = wt[pos], probs = c(0.25, 0.5, 0.75))
  pums <- pums %>% mutate(
    income_quartile = dplyr::case_when(
      is.na(as.numeric(PINCP)) | as.numeric(PINCP) <= 0 ~ "Q1",
      as.numeric(PINCP) <= qs_income[1] ~ "Q1",
      as.numeric(PINCP) <= qs_income[2] ~ "Q2",
      as.numeric(PINCP) <= qs_income[3] ~ "Q3",
      TRUE                               ~ "Q4"
    )
  )

  # Urbanity: PUMA-to-urbanicity crosswalk (2020 Census urban areas)
  puma_urban <- tryCatch({
    message("  Downloading PUMA urbanicity crosswalk (Census 2020)...")
    url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/ua_puma_rel_2020.txt"
    xw  <- read.csv(url(url), stringsAsFactors = FALSE)
    xw %>%
      mutate(is_ua = !is.na(UANAME) & UATYPE == "UA") %>%  # Urbanized Area (50k+)
      group_by(STATEFP, PUMACE) %>%
      summarize(
        pop_ua    = sum(ifelse(is_ua,  POPPT, 0), na.rm=TRUE),
        pop_uc    = sum(ifelse(!is_ua & !is.na(UANAME), POPPT, 0), na.rm=TRUE),
        pop_total = sum(POPPT, na.rm=TRUE),
        .groups   = "drop"
      ) %>%
      mutate(
        pct_ua   = pop_ua / pmax(pop_total, 1),
        pct_uc   = pop_uc / pmax(pop_total, 1),
        urbanity = dplyr::case_when(
          pct_ua >= 0.5                        ~ "Cities",
          (pct_ua + pct_uc) >= 0.25           ~ "Towns and suburbs",
          TRUE                                 ~ "Rural"
        ),
        ST_int   = as.integer(STATEFP),
        PUMA_int = as.integer(PUMACE)
      ) %>%
      select(ST_int, PUMA_int, urbanity)
  }, error = function(e) {
    message("  Crosswalk download failed (", conditionMessage(e), "). Using state-based proxy.")
    NULL
  })

  if (!is.null(puma_urban)) {
    pums <- pums %>%
      mutate(ST_int=as.integer(ST), PUMA_int=as.integer(PUMA)) %>%
      left_join(puma_urban, by=c("ST_int","PUMA_int"))
  } else {
    # Fallback: assign Cities/Rural from national marginals, Towns and suburbs = residual
    u_shares <- pop_freq$US$urbanity
    if (is.null(u_shares))
      u_shares <- c(Cities=0.596, `Towns and suburbs`=0.144, Rural=0.260)
    u_shares <- u_shares / sum(u_shares, na.rm=TRUE)
    set.seed(42)
    pums$urbanity <- sample(names(u_shares), nrow(pums), replace=TRUE, prob=u_shares)
    warning("US urbanity randomly assigned from national marginals (PUMA crosswalk unavailable).")
  }

  # Build joint distribution: group_by predictors, sum PWGTP
  preds_avail <- intersect(predictors, names(pums))
  frame <- pums %>%
    filter(across(all_of(preds_avail), ~ !is.na(.x) & .x != "")) %>%
    mutate(PWGTP = as.numeric(PWGTP)) %>%
    group_by(across(all_of(preds_avail))) %>%
    summarize(n = sum(PWGTP, na.rm=TRUE), .groups="drop") %>%
    filter(n > 0) %>%
    mutate(region = as.character(region))

  message("  US PUMS frame: ", nrow(frame), " cells, N = ",
          format(round(sum(frame$n)), big.mark=","))
  frame
}


##### Build census frame for non-US country via IPF from official marginals #####
#
# Uses marginals in pop_freq (sourced from Eurostat, national statistics offices).
# IPF (Iterative Proportional Fitting) preserves all 1-D marginals while minimising
# deviation from independence. Falls back to independence assumption on failure.
#
# For EU countries, also attempts to download gender × age cross-tab from Eurostat
# (demo_pjangroup) to use as a 2-D IPF constraint.

eurostat_geo_map <- c(
  FR="FR", DE="DE", IT="IT", PL="PL", ES="ES", CH="CH", GB="UK", JP=NA, RU=NA, SA=NA
)

download_eurostat_gender_age <- function(geo_code, year=2023) {
  library(eurostat)
  tryCatch({
    df <- get_eurostat("demo_pjangroup",
                       filters = list(geo=geo_code, time=as.character(year), sex=c("M","F")),
                       type="code", cache=TRUE)
    df
  }, error = function(e) { message("  Eurostat gender×age download failed: ", conditionMessage(e)); NULL })
}

parse_eurostat_gender_age <- function(df) {
  if (is.null(df) || nrow(df)==0) return(NULL)
  df %>%
    filter(sex %in% c("M","F")) %>%
    mutate(
      gender = ifelse(sex=="M","Man","Woman"),
      age_s  = dplyr::case_when(
        age %in% c("Y18","Y19","Y20","Y21","Y22","Y23","Y24","Y15-19","Y20-24","Y18-24") ~ "18-24",
        age %in% c("Y25","Y26","Y27","Y28","Y29","Y30","Y31","Y32","Y33","Y34","Y25-34") ~ "25-34",
        age %in% c("Y35","Y36","Y37","Y38","Y39","Y40","Y41","Y42","Y43","Y44",
                   "Y45","Y46","Y47","Y48","Y49","Y35-49","Y35-44","Y45-49")            ~ "35-49",
        age %in% c("Y50","Y51","Y52","Y53","Y54","Y55","Y56","Y57","Y58","Y59",
                   "Y60","Y61","Y62","Y63","Y64","Y50-64","Y50-54","Y55-59","Y60-64")   ~ "50-64",
        grepl("^Y6[5-9]|^Y7|^Y8|^Y9|GE65|GE70|GE75|GE80", age)                        ~ "65+",
        TRUE ~ NA_character_
      )
    ) %>%
    filter(!is.na(age_s), !is.na(values), values >= 0) %>%
    group_by(gender, age_s) %>%
    summarize(n=sum(values, na.rm=TRUE), .groups="drop")
}


build_country_census_frame <- function(ctry, predictors) {
  marg   <- pop_freq[[ctry]]
  pop    <- adult_pop[[ctry]]
  n_r    <- n_reg[[ctry]]

  # ── Determine active levels per predictor ────────────────────────────────
  lev <- list(
    gender          = c("Woman","Man"),
    age             = age_labels,
    education_quota = educ_levels,
    income_quartile = c("Q1","Q2","Q3","Q4"),
    urbanity        = urban_levels,
    region          = as.character(seq_len(n_r))
  )
  # Subset to requested predictors and drop empty levels
  use_preds <- intersect(predictors, names(lev))
  lev       <- lev[use_preds]

  # For region: only keep levels present in pop_freq (non-zero)
  if ("region" %in% use_preds) {
    reg_marg <- marg$region
    if (!is.null(reg_marg)) {
      active_reg <- names(reg_marg[as.numeric(reg_marg) > 0])
      active_reg <- intersect(active_reg, as.character(seq_len(n_r)))
      if (length(active_reg) > 0) lev$region <- active_reg
    }
  }

  dims <- sapply(lev, length)

  # ── Build 1-D marginal targets from pop_freq ─────────────────────────────
  get_marg <- function(pred) {
    v <- marg[[pred]]
    if (is.null(v)) return(NULL)
    lv <- lev[[pred]]
    # Map by name; fill missing with tiny value
    out <- setNames(rep(1e-8, length(lv)), lv)
    matched <- intersect(names(v), lv)
    out[matched] <- pmax(as.numeric(v[matched]), 1e-8)
    out / sum(out)
  }

  target_list <- list()
  target_data <- list()
  dim_idx     <- setNames(seq_along(use_preds), use_preds)

  # Try 2D gender × age from Eurostat
  geo <- eurostat_geo_map[[ctry]]
  used_2d_ga <- FALSE
  if (!is.na(geo) && "gender" %in% use_preds && "age" %in% use_preds) {
    raw <- download_eurostat_gender_age(geo)
    parsed <- parse_eurostat_gender_age(raw)
    if (!is.null(parsed) && nrow(parsed) >= 4) {
      mat <- matrix(1e-8, nrow=length(lev$gender), ncol=length(lev$age),
                    dimnames=list(gender=lev$gender, age=lev$age))
      for (i in seq_len(nrow(parsed))) {
        g <- parsed$gender[i]; a <- parsed$age_s[i]
        if (g %in% lev$gender && a %in% lev$age)
          mat[g, a] <- mat[g, a] + parsed$n[i]
      }
      if (sum(mat) > 0) {
        mat <- mat / sum(mat)
        target_list[[length(target_list)+1]] <- c(dim_idx["gender"], dim_idx["age"])
        target_data[[length(target_data)+1]] <- mat
        used_2d_ga <- TRUE
        message("  ", ctry, ": using Eurostat 2D gender×age IPF constraint")
      }
    }
  }

  # 1D marginals for all remaining predictors
  for (pred in use_preds) {
    if (used_2d_ga && pred %in% c("gender","age")) next   # already covered by 2D
    mv <- get_marg(pred)
    if (!is.null(mv)) {
      target_list[[length(target_list)+1]] <- dim_idx[[pred]]
      target_data[[length(target_data)+1]] <- mv
    }
  }

  # ── Run IPF ──────────────────────────────────────────────────────────────
  seed <- array(1, dim=dims, dimnames=lev)

  joint <- if (length(target_list) > 0) {
    tryCatch(
      Ipfp(seed, target_list, target_data, iter=2000, tol=1e-7)$p.hat,
      error = function(e) {
        message("  IPF failed for ", ctry, ": ", conditionMessage(e),
                " — using independence approximation.")
        NULL
      }
    )
  } else NULL

  if (is.null(joint)) {
    # Independence fallback: joint prob = product of 1D marginals
    combs      <- expand.grid(lev, stringsAsFactors=FALSE)
    joint_prob <- rep(1.0, nrow(combs))
    for (pred in use_preds) {
      mv <- get_marg(pred)
      if (!is.null(mv)) joint_prob <- joint_prob * as.numeric(mv[combs[[pred]]])
    }
    joint_prob[is.na(joint_prob) | joint_prob < 0] <- 1e-12
    joint_prob <- joint_prob / sum(joint_prob)

    frame      <- combs
    frame$n    <- joint_prob * pop
    frame      <- frame[frame$n > 0, ]
    if ("region" %in% names(frame)) frame$region <- as.character(frame$region)

    message("  ", ctry, " census frame (independence): ", nrow(frame), " cells, N = ",
            format(round(sum(frame$n)), big.mark=","))
    return(frame)
  }

  # ── Expand IPF result to data frame ─────────────────────────────────────
  frame      <- as.data.frame.table(joint, stringsAsFactors=FALSE)
  names(frame)[names(frame)=="Freq"] <- "prop"
  frame$n    <- frame$prop * pop
  frame      <- frame[frame$n > 0, names(frame) != "prop"]
  frame$region <- as.character(frame$region)

  message("  ", ctry, " census frame: ", nrow(frame), " cells, N = ",
          format(round(sum(frame$n)), big.mark=","))
  frame
}


##### Build all census frames #####

message("\n=== Building census frames ===\n")

census_frames <- list()

# Non-US countries
for (ctry in setdiff(countries, "US")) {
  message("\n--- ", ctry, " ---")
  census_frames[[ctry]] <- tryCatch(
    build_country_census_frame(ctry, preds_by_country[[ctry]]),
    error = function(e) { message("Failed: ", conditionMessage(e)); NULL }
  )
}

# US: PUMS (requires Census API key)
# Set key once with: census_api_key("YOUR_KEY", install=TRUE)
# Get key at: https://api.census.gov/data/key_signup.html
message("\n--- US ---")
has_key <- tryCatch(
  { library(tidycensus); Sys.getenv("CENSUS_API_KEY") != "" },
  error = function(e) FALSE
)

if (has_key) {
  census_frames$US_default  <- tryCatch(
    build_us_pums_frame(preds_US_default,  year=2022),
    error = function(e) { message("US default PUMS failed: ", conditionMessage(e)); NULL }
  )
  census_frames$US_extended <- tryCatch(
    build_us_pums_frame(preds_US_extended, year=2022),
    error = function(e) { message("US extended PUMS failed: ", conditionMessage(e)); NULL }
  )
} else {
  message("No Census API key — falling back to IPF for US.")
  census_frames$US_default  <- tryCatch(
    build_country_census_frame("US", preds_US_default),
    error = function(e) { message("US default IPF failed: ", conditionMessage(e)); NULL }
  )
  census_frames$US_extended <- tryCatch(
    build_country_census_frame("US", preds_US_extended),
    error = function(e) { message("US extended IPF failed: ", conditionMessage(e)); NULL }
  )
}
census_frames$US <- census_frames$US_default


##### MRP function #####
#
# Fits glmer(outcome_binary ~ 1 + (1|pred1) + (1|pred2) + ..., family=binomial)
# on the survey subset for `country`, then poststratifies on `census_frame`.
#
# Returns: list with estimate_mrp, estimate_raw, n_survey, model, frame.

apply_mrp <- function(survey_df, outcome, predictors, census_frame, country="") {
  if (is.null(census_frame) || nrow(census_frame)==0) {
    message("  [SKIP] ", country, " (", outcome, "): no census frame"); return(NULL)
  }

  survey_df$outcome_bin <- as.numeric(survey_df[[outcome]] == "Yes")

  # Align types between survey and census frame
  if ("region" %in% names(survey_df))    survey_df$region    <- as.character(survey_df$region)
  if ("age"    %in% names(survey_df))    survey_df$age       <- as.character(survey_df$age)
  if ("region" %in% names(census_frame)) census_frame$region <- as.character(census_frame$region)
  if ("age"    %in% names(census_frame)) census_frame$age    <- as.character(census_frame$age)

  vars   <- c("outcome_bin", predictors)
  avail  <- intersect(vars, names(survey_df))
  df_cln <- survey_df[complete.cases(survey_df[, avail]), ]

  if (nrow(df_cln) < 30) {
    message("  [SKIP] ", country, " (", outcome, "): n=", nrow(df_cln)); return(NULL)
  }

  # Only include predictors with ≥2 distinct levels
  preds_ok <- intersect(predictors, names(df_cln))
  preds_ok <- preds_ok[vapply(preds_ok, function(p)
    length(unique(na.omit(df_cln[[p]]))) >= 2, logical(1))]

  if (length(preds_ok)==0) {
    message("  [SKIP] ", country, " (", outcome, "): no valid predictors"); return(NULL)
  }

  formula_str <- paste0("outcome_bin ~ 1 + ", paste0("(1|", preds_ok, ")", collapse=" + "))
  message("  Fitting ", country, " / ", outcome,
          "  (n=", nrow(df_cln), ", preds: ", paste(preds_ok, collapse=", "), ")")

  model <- tryCatch(
    glmer(as.formula(formula_str), data=df_cln, family=binomial,
          control=glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=3e5))),
    error = function(e) { message("  glmer error: ", conditionMessage(e)); NULL }
  )
  if (is.null(model)) return(NULL)

  # Predict on census cells
  preds_shared    <- intersect(preds_ok, names(census_frame))
  census_frame$pred <- tryCatch(
    predict(model, newdata=census_frame, type="response", allow.new.levels=TRUE),
    error = function(e) { message("  predict error: ", conditionMessage(e)); rep(NA_real_, nrow(census_frame)) }
  )

  ok  <- !is.na(census_frame$pred)
  est <- sum(census_frame$pred[ok] * census_frame$n[ok]) / sum(census_frame$n[ok])
  raw <- mean(df_cln$outcome_bin, na.rm=TRUE)

  message("  >> ", country, " | ", outcome,
          "  MRP=", round(est,3), "  raw=", round(raw,3))

  list(country=country, outcome=outcome,
       estimate_mrp=est, estimate_raw=raw,
       n_survey=nrow(df_cln), model=model, frame=census_frame)
}


##### Apply MRP to all countries #####

message("\n=== Applying MRP ===\n")

mrp_results <- list()
non_us <- setdiff(countries, "US")

for (ctry in non_us) {
  df     <- all[all$country == ctry, ]
  preds  <- preds_by_country[[ctry]]
  frame  <- census_frames[[ctry]]

  mrp_results[[paste0(ctry,"_gcs_support")]] <- apply_mrp(
    df, "gcs_support", preds, frame, ctry)

  mrp_results[[paste0(ctry,"_gcs_support_control")]] <- apply_mrp(
    df, "gcs_support_control", preds, frame, ctry)
}

# US: separate census frames for default vs extended
us_df <- all[all$country == "US", ]

mrp_results$US_gcs_support <- apply_mrp(
  us_df, "gcs_support",         preds_US_default,  census_frames$US_default,  "US")

mrp_results$US_gcs_support_control <- apply_mrp(
  us_df, "gcs_support_control", preds_US_extended, census_frames$US_extended, "US")


##### Summary table #####

mrp_summary <- do.call(rbind, lapply(mrp_results, function(r) {
  if (is.null(r)) return(NULL)
  data.frame(country=r$country, outcome=r$outcome,
             estimate_mrp=round(r$estimate_mrp, 4),
             estimate_raw=round(r$estimate_raw, 4),
             n_survey=r$n_survey, stringsAsFactors=FALSE)
}))

if (!is.null(mrp_summary) && nrow(mrp_summary) > 0) {
  cat("\n=== MRP Results ===\n")
  print(mrp_summary, row.names=FALSE)

  mrp_wide <- mrp_summary %>%
    pivot_wider(id_cols=country, names_from=outcome,
                values_from=estimate_mrp,
                names_prefix="mrp_")
  cat("\nWide:\n")
  print(mrp_wide, row.names=FALSE)
}

saveRDS(mrp_results,   "../data_ext/mrp_results.rds")
saveRDS(census_frames, "../data_ext/mrp_census_frames.rds")
if (!is.null(mrp_summary)) write.csv(mrp_summary, "../data_ext/mrp_summary.csv", row.names=FALSE)

message("\nDone. Results in data_ext/mrp_results.rds and mrp_summary.csv")
