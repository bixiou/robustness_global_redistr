setwd("C:/Users/fabre/Documents/www/robustness_global_redistr/code_robustness")
readRenviron("~/.Renviron")   # loads CENSUS_API_KEY (stored in Documents/.Renviron)
if (Sys.getenv("CENSUS_API_KEY") == "") stop("CENSUS_API_KEY not found in ~/.Renviron")
load(".RData")

# Compute absolute cache path once, so it works regardless of how R is invoked.
# normalizePath resolves relative paths against getwd() at script start time.
CACHE_DIR <- normalizePath(file.path("..", "data_ext", "pums_cache"), mustWork=FALSE)
message("Cache directory: ", CACHE_DIR)
dir.create(CACHE_DIR, showWarnings=TRUE, recursive=TRUE)

suppressPackageStartupMessages({
  library(lme4); library(dplyr); library(tidyr); library(Hmisc); library(tidycensus)
})

# ── Predictor definitions ──────────────────────────────────────────────────────
age_labels   <- c("18-24","25-34","35-49","50-64","65+")
educ_levels  <- c("Below upper secondary","Upper secondary","Post secondary","Not 25-64")
urban_levels <- c("Cities","Towns and suburbs","Rural")
income_levels <- c("Q1","Q2","Q3","Q4")
gender_levels <- c("Woman","Man")
preds_US_default  <- c("gender","age","education_quota","income_quartile","urbanity","region","race")
preds_US_extended <- c("gender","age","education_quota","income_quartile","urbanity","region",
                       "race","employment_18_64")

# ── PUMS helper functions (numeric inputs) ─────────────────────────────────────
pums_age_n <- function(agep_n) {
  cut(agep_n, breaks=c(17,24,34,49,64,Inf), labels=age_labels, right=TRUE)
}

pums_educ_n <- function(schl_n, agep_n) {
  dplyr::case_when(
    agep_n < 25 | agep_n >= 65 ~ "Not 25-64",
    schl_n <= 15               ~ "Below upper secondary",
    schl_n <= 19               ~ "Upper secondary",   # includes HS, GED, some college/vocational (no degree)
    TRUE                       ~ "Post secondary"
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

pums_empl_n <- function(esr, agep_n) {
  dplyr::case_when(
    agep_n >= 65                   ~ "65+",
    esr %in% c("1","2","4","5")    ~ "Employed",
    esr == "3"                     ~ "Unemployed",
    TRUE                           ~ "Inactive"
  )
}

pums_region_us_int <- function(st_int) {
  dplyr::case_when(
    st_int %in% c(9,23,25,33,44,50,34,36,42)                         ~ "1",
    st_int %in% c(17,18,26,39,55,19,20,27,29,31,38,46)               ~ "2",
    st_int %in% c(10,11,12,13,24,37,45,51,54,1,21,28,47,5,22,40,48)  ~ "3",
    st_int %in% c(4,8,16,30,32,35,49,56,2,6,15,41,53)                ~ "4",
    TRUE                                                               ~ NA_character_
  )
}

# US income quartile thresholds (2025 USD, inflated).
# Scaled to the ACS survey year using approximate CPI factors.
us_income_q_breaks_2025 <- c(Q1Q2 = 36205, Q2Q3 = 72097, Q3Q4 = 129899)
us_cpi_scale <- c("2019"=0.874,"2020"=0.875,"2021"=0.899,"2022"=0.930,
                   "2023"=0.955,"2024"=0.978,"2025"=1.000)

# ── Memory-efficient PUMS frame builder ────────────────────────────────────────
# Processes each state individually to avoid OOM from binding 50 states at once.
# Strategy:
#   1. Download PUMA urbanicity crosswalk (small, once).
#   2. For each state: download PUMS, select needed columns, filter adults,
#      convert to numeric — then release raw download.
#   3. Compute year-adjusted income quartile thresholds (fixed 2025 values × CPI scale).
#   4. For each state: recode predictors, aggregate to cell counts, release state data.
#   5. Combine cell count tables (tiny) and return.
#
# Urbanity note: PUMA crosswalk and survey zipcode_US.csv both use Census 2020 Urban Area
# boundaries and the same three-category scheme (Cities/Towns&suburbs/Rural), so they
# are consistent.  Minor residual differences arise from zip vs PUMA granularity only.
build_us_pums_frame <- function(predictors, year=2023, survey="acs5",
                               cache_dir=CACHE_DIR) {
  message("  Building US PUMS frame (memory-efficient), cache: ", cache_dir)
  dir.create(cache_dir, showWarnings=FALSE, recursive=TRUE)

  # ST removed: ACS 2023 rejects it as a GET variable on per-state endpoints.
  # Region is derived from the loop variable instead (state abbr → FIPS → region).
  pums_vars <- c("PWGTP","SEX","AGEP","SCHL","PINCP","RAC1P","HISP","ESR","PUMA")

  # State abbreviation → FIPS integer (used to assign region without ST in PUMS data)
  state_fips <- c(AL=1,AK=2,AZ=4,AR=5,CA=6,CO=8,CT=9,DE=10,DC=11,FL=12,GA=13,
                  HI=15,ID=16,IL=17,IN=18,IA=19,KS=20,KY=21,LA=22,ME=23,MD=24,
                  MA=25,MI=26,MN=27,MS=28,MO=29,MT=30,NE=31,NV=32,NH=33,NJ=34,
                  NM=35,NY=36,NC=37,ND=38,OH=39,OK=40,OR=41,PA=42,RI=44,SC=45,
                  SD=46,TN=47,TX=48,UT=49,VT=50,VA=51,WA=53,WV=54,WI=55,WY=56)
  all_states <- c("AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
                  "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
                  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
                  "VA","WA","WV","WI","WY","DC")

  # Step 1: PUMA-to-urbanicity crosswalk — cached across runs
  xwalk_cache <- file.path(cache_dir, "puma_urban_2020.rds")
  puma_urban <- if (file.exists(xwalk_cache)) {
    message("  Loading PUMA urbanicity crosswalk from cache...")
    readRDS(xwalk_cache)
  } else {
    tryCatch({
      message("  Downloading PUMA urbanicity crosswalk...")
      url <- "https://www2.census.gov/geo/docs/maps-data/data/rel2020/ua_puma_rel_2020.txt"
      xw  <- read.csv(url(url), stringsAsFactors=FALSE)
      result <- xw %>%
        dplyr::mutate(is_ua = !is.na(UANAME) & UATYPE == "UA") %>%
        dplyr::group_by(STATEFP, PUMACE) %>%
        dplyr::summarize(
          pop_ua    = sum(ifelse(is_ua, POPPT, 0), na.rm=TRUE),
          pop_uc    = sum(ifelse(!is_ua & !is.na(UANAME), POPPT, 0), na.rm=TRUE),
          pop_total = sum(POPPT, na.rm=TRUE), .groups="drop") %>%
        dplyr::mutate(
          pct_ua   = pop_ua / pmax(pop_total, 1),
          pct_uc   = pop_uc / pmax(pop_total, 1),
          urbanity = dplyr::case_when(
            pct_ua >= 0.5              ~ "Cities",
            (pct_ua + pct_uc) >= 0.25 ~ "Towns and suburbs",
            TRUE                       ~ "Rural"),
          ST_int   = as.integer(STATEFP),
          PUMA_int = as.integer(PUMACE)) %>%
        dplyr::select(ST_int, PUMA_int, urbanity)
      tryCatch(saveRDS(result, xwalk_cache),
               error=function(e) message("  Crosswalk cache write failed: ", conditionMessage(e)))
      result
    }, error=function(e){ message("  Crosswalk failed: ", conditionMessage(e)); NULL })
  }

  # Step 2: Download each state, keep only minimal columns — cached per state × year
  message("  Downloading state PUMS (", length(all_states), " states)...")
  state_mini <- vector("list", length(all_states))
  names(state_mini) <- all_states
  for (st in all_states) {
    st_cache <- file.path(cache_dir, paste0("pums_", st, "_", year, "_", survey, ".rds"))
    if (file.exists(st_cache)) {
      state_mini[[st]] <- readRDS(st_cache)
      next
    }
    for (attempt in 1:3) {
      result <- tryCatch({
        raw <- get_pums(variables=pums_vars, state=st, year=year, survey=survey, recode=FALSE)
        raw %>%
          dplyr::filter(as.numeric(AGEP) >= 18) %>%
          dplyr::transmute(
            PWGTP_n   = as.numeric(PWGTP),
            PINCP_adj = as.numeric(PINCP),
            AGEP_n    = as.numeric(AGEP),
            SCHL_n    = as.numeric(SCHL),
            SEX       = SEX,
            RAC1P     = RAC1P,
            HISP      = HISP,
            ESR       = ESR,
            ST_int    = as.integer(state_fips[st]),
            PUMA_int  = as.integer(PUMA)
          )
      }, error=function(e){ message("  ",st," attempt ",attempt,": ",conditionMessage(e)); NULL })
      if (!is.null(result)) {
        tryCatch(saveRDS(result, st_cache),
                 error=function(e) message("  Cache write failed for ", st, ": ", conditionMessage(e)))
        state_mini[[st]] <- result
        break
      }
      Sys.sleep(5)
    }
    if (is.null(state_mini[[st]])) message("  Skipping ", st, " after 3 failed attempts.")
  }
  gc()

  # Step 3: Income quartile thresholds — fixed 2025 values scaled to ACS survey year.
  # Using fixed thresholds ensures PUMS cells align with the income brackets
  # shown to survey respondents (which were calibrated to the same 2025 thresholds).
  scale <- as.numeric(us_cpi_scale[as.character(year)])
  if (is.na(scale)) scale <- 1.0
  qb <- us_income_q_breaks_2025 * scale
  message("  Income quartile thresholds (", year, " USD, CPI scale=", round(scale,3), "): ",
          paste(round(qb), collapse=", "))

  preds_avail <- intersect(predictors,
    c("gender","age","education_quota","income_quartile",
      "urbanity","region","race","employment_18_64"))

  # Step 4: Per-state recode + aggregate to cell counts
  message("  Recoding and aggregating per state...")
  cell_data <- lapply(all_states, function(st) {
    d <- state_mini[[st]]
    if (is.null(d)) return(NULL)
    d2 <- d %>%
      dplyr::mutate(
        gender           = dplyr::case_when(SEX=="1"~"Man", SEX=="2"~"Woman", TRUE~NA_character_),
        age              = as.character(pums_age_n(AGEP_n)),
        education_quota  = pums_educ_n(SCHL_n, AGEP_n),
        race             = pums_race(RAC1P, HISP),
        region           = pums_region_us_int(ST_int),
        employment_18_64 = pums_empl_n(ESR, AGEP_n),
        income_quartile  = dplyr::case_when(
          is.na(PINCP_adj) | PINCP_adj <= 0 ~ "Q1",
          PINCP_adj <= qb["Q1Q2"]           ~ "Q1",
          PINCP_adj <= qb["Q2Q3"]           ~ "Q2",
          PINCP_adj <= qb["Q3Q4"]           ~ "Q3",
          TRUE                              ~ "Q4"
        )
      )
    if (!is.null(puma_urban)) {
      d2 <- dplyr::left_join(d2, puma_urban, by=c("ST_int","PUMA_int"))
    } else {
      u <- if (!is.null(pop_freq$US$urbanity)) pop_freq$US$urbanity
           else c(Cities=0.596, `Towns and suburbs`=0.144, Rural=0.260)
      u <- u / sum(u, na.rm=TRUE)
      set.seed(42 + match(st, all_states))
      d2$urbanity <- sample(names(u), nrow(d2), replace=TRUE, prob=u)
    }
    d2 %>%
      dplyr::filter(dplyr::across(dplyr::all_of(preds_avail), ~ !is.na(.x) & .x != "")) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(preds_avail))) %>%
      dplyr::summarize(n=sum(PWGTP_n, na.rm=TRUE), .groups="drop")
  })
  rm(state_mini); gc()

  # Step 5: Combine tiny cell count tables
  frame <- dplyr::bind_rows(cell_data) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(preds_avail))) %>%
    dplyr::summarize(n=sum(n, na.rm=TRUE), .groups="drop") %>%
    dplyr::filter(n > 0) %>%
    dplyr::mutate(region=as.character(region))
  message("  US PUMS frame: ", nrow(frame), " cells, N=",
          format(round(sum(frame$n)), big.mark=","))
  frame
}

# ── MRP function ───────────────────────────────────────────────────────────────
apply_mrp <- function(survey_df, outcome, predictors, census_frame, country="") {
  if (is.null(census_frame)||nrow(census_frame)==0) {
    message("  [SKIP] ",country," (",outcome,"): no census frame"); return(NULL)
  }
  v <- survey_df[[outcome]]
  if (is.numeric(v) && !inherits(v,"double.item")) survey_df$outcome_bin <- as.numeric(v > 0)
  else survey_df$outcome_bin <- as.numeric(v == "Yes")
  for (col in c("region","age")) {
    if (col %in% names(survey_df))    survey_df[[col]]    <- as.character(survey_df[[col]])
    if (col %in% names(census_frame)) census_frame[[col]] <- as.character(census_frame[[col]])
  }
  avail  <- intersect(c("outcome_bin", predictors), names(survey_df))
  df_cln <- survey_df[complete.cases(survey_df[, avail]), ]
  if (nrow(df_cln) < 30) {
    message("  [SKIP] ",country," (",outcome,"): n=",nrow(df_cln)); return(NULL)
  }
  preds_ok <- intersect(predictors, names(df_cln))
  preds_ok <- preds_ok[vapply(preds_ok, function(p)
    length(unique(na.omit(df_cln[[p]]))) >= 2, logical(1))]
  if (length(preds_ok)==0) {
    message("  [SKIP] ",country," (",outcome,"): no valid predictors"); return(NULL)
  }
  formula_str <- paste0("outcome_bin ~ 1 + ", paste0("(1|",preds_ok,")", collapse=" + "))
  message("  Fitting ",country,"/",outcome,
          "  n=",nrow(df_cln),"  preds=",paste(preds_ok,collapse=","))
  model <- tryCatch(
    lme4::glmer(as.formula(formula_str), data=df_cln, family=binomial,
                control=lme4::glmerControl(optimizer="bobyqa", optCtrl=list(maxfun=3e5))),
    error=function(e){ message("  glmer error: ",conditionMessage(e)); NULL })
  if (is.null(model)) return(NULL)
  census_frame$pred <- tryCatch(
    predict(model, newdata=census_frame, type="response", allow.new.levels=TRUE),
    error=function(e){ message("  predict error: ",conditionMessage(e))
                       rep(NA_real_, nrow(census_frame)) })
  ok  <- !is.na(census_frame$pred)
  if (!any(ok)) { message("  No valid predictions for ",country); return(NULL) }
  est <- sum(census_frame$pred[ok]*census_frame$n[ok]) / sum(census_frame$n[ok])
  raw <- mean(df_cln$outcome_bin, na.rm=TRUE)
  message("  >> ",country," | ",outcome,
          "  MRP=",round(est,3),"  raw=",round(raw,3))
  list(country=country, outcome=outcome, estimate_mrp=est, estimate_raw=raw,
       n_survey=nrow(df_cln), model=model, frame=census_frame)
}

# ── Build US census frames ─────────────────────────────────────────────────────
message("\n=== US: PUMS census frames ===\n")
us_default_frame <- tryCatch(
  build_us_pums_frame(preds_US_default,  year=2023),
  error=function(e){ message("US default PUMS failed: ",conditionMessage(e)); NULL })
us_extended_frame <- tryCatch(
  build_us_pums_frame(preds_US_extended, year=2023),
  error=function(e){ message("US extended PUMS failed: ",conditionMessage(e)); NULL })

# ── Fit US MRP models (4 combinations per TODO) ────────────────────────────────
message("\n=== US: MRP (4 outcome × predictor combinations) ===\n")
us_df <- all[all$country == "US", ]

res_gcs_default  <- apply_mrp(us_df, "gcs_support",         preds_US_default,  us_default_frame,  "US")
res_gcs_extended <- apply_mrp(us_df, "gcs_support",         preds_US_extended, us_extended_frame, "US")
res_ctl_default  <- apply_mrp(us_df, "gcs_support_control", preds_US_default,  us_default_frame,  "US")
res_ctl_extended <- apply_mrp(us_df, "gcs_support_control", preds_US_extended, us_extended_frame, "US")

# ── Merge with existing non-US results ────────────────────────────────────────
existing <- readRDS("../data_ext/mrp_results.rds")
existing$US_gcs_support           <- res_gcs_default
existing$US_gcs_support_extended  <- res_gcs_extended
existing$US_gcs_support_control   <- res_ctl_default
existing$US_gcs_support_control_extended <- res_ctl_extended

saveRDS(existing, "../data_ext/mrp_results.rds")

# Update census frames
census_frames_all <- readRDS("../data_ext/mrp_census_frames.rds")
census_frames_all$US_default  <- us_default_frame
census_frames_all$US_extended <- us_extended_frame
census_frames_all$US          <- us_default_frame
saveRDS(census_frames_all, "../data_ext/mrp_census_frames.rds")

# ── Summary ────────────────────────────────────────────────────────────────────
mrp_summary <- do.call(rbind, lapply(existing, function(r) {
  if (is.null(r)) return(NULL)
  data.frame(country=r$country, outcome=r$outcome,
             estimate_mrp=round(r$estimate_mrp, 4),
             estimate_raw=round(r$estimate_raw, 4),
             n_survey=r$n_survey, stringsAsFactors=FALSE)
}))

mrp_wide <- tidyr::pivot_wider(
  mrp_summary[, c("country","outcome","estimate_mrp")],
  id_cols="country", names_from="outcome", values_from="estimate_mrp", names_prefix="mrp_")

cat("\n=== Final MRP Results (all countries) ===\n")
print(mrp_summary[order(mrp_summary$country, mrp_summary$outcome), ], row.names=FALSE)
cat("\nWide format:\n")
print(as.data.frame(mrp_wide), row.names=FALSE)

write.csv(mrp_summary, "../data_ext/mrp_summary.csv", row.names=FALSE)
message("\nDone. mrp_results.rds, mrp_census_frames.rds and mrp_summary.csv updated.")
