##### Functions #####
remove_id <- function(file, folder = "../data_raw/") {
  filename <- paste(folder, file, ".csv", sep = "")
  
  filename_copy <- paste("./deprecated/", file, sample.int(10^5, 1), ".csv", sep = "") # in case the three last lines don't work
  file.copy(filename, filename_copy)
  data <- read_csv(filename_copy)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic")))]
  write_csv(data, filename, na = "")
  file.remove(filename_copy)
} 


##### Load data #####
survey_list <- all_surveys()
pilots <- paste0(c("PL", "GB", "US"), "p") 
pilot_names <- setNames(paste0(c("PL", "GB", "US"), "_pilot"), pilots) 
# cut=0 at March 4 2025, 00:05 Paris time (after 46 PL; 118 GB; 46 US)
for (p in pilots) { # pilots
  print(p)
  data <- fetch_survey(survey_list$id[survey_list$name == pilot_names[p]], include_display_order = T, verbose = T, convert = F)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
  for (v in names(data)) label(data[[v]]) <- c(v = paste0(v, ": ", label(data[[v]])))
  write_csv(data, paste0("../data_raw/", p, ".csv"), na = "")
  eval(str2expression(paste0(p, " <- read_csv('../data_raw/", p, ".csv')")))
  saveRDS(label(data), paste0("../data_raw/labels/", p, ".rds"))
  labels <- readRDS(paste0("../data_raw/labels/", p, ".rds"))
  for (v in names(d(p))) eval(str2expression(paste0("label(", p, "[[v]]) <- labels[[v]]")))
}
GBp <- fetch_survey(survey_list$id[survey_list$name == "GB_pilot"], include_display_order = T, verbose = T, convert = F) # labels using sjlabelled package
# write.csv(d("GBp"), paste0("../data_raw/GBp.csv"), quote = F, na = "", row.names = F)
# Slightly different from manual export .csv: no second row with question text; timezone is different (in e.g. startDate); True => TRUE; income bug; some additional "" are removed
View(GBp)
  
for (v in names(GBp)[1:80]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)[81:160]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)[161:240]) { print(decrit(v, GBp)); print("____________");}
for (v in names(GBp)) if (grepl("Q_TotalDuration", v)) { print(decrit(as.numeric(GBp[[v]]))); print("____________");}

table(GBp$ncqg)
table(GBp$group_defended)
PLp$comment_field
decrit(GBp$Q_TerminateFlag)

stats_exclude <- function(data_name, all = F, old_names = F) {
  cat("\n")
  cat(paste("\nSURVEY:", data_name))
  cat("\n\n")
  e <- read_csv(paste0("../data_raw/", data_name, ".csv"))
  # e <- d(data_name)
  # if (descr) {
  #   for (v in names(e)[1:80]) { print(decrit(v, e)); print("____________");}
  #   for (v in names(e)[81:160]) { print(decrit(v, e)); print("____________");}
  #   for (v in names(e)[161:211]) { print(decrit(v, e)); print("____________");}
  # }
  if (!old_names) {
    e$Q_TerminateFlag <- e$excluded
    e$Finished <- e$finished
    e$Progress <- e$progress
  }
  if ("Finished...7" %in% names(e)) e$Finished <- e$Finished...7
  cat(paste0(nrow(e), " total obs.\n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "QuotaMet")/nrow(e), 1), "% quota met (incl. socio-demo screened)\n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "QuotaMet" & (e$income=="Prefer not to say" | e$urbanity %in% 0 | e$region %in% 0 | e$age_exact %in% "Below 18"))/nrow(e), 1), "% socio-demo screened\n"))
  if (all) cat(paste0(sum((!e$Q_TerminateFlag %in% "QuotaMet") & (e$income=="Prefer not to say" | e$urbanity %in% 0 | e$region %in% 0 | e$age_exact %in% "Below 18")), " socio-demo screened not flagged as Quota Met\n"))
  cat(paste0(round(100*sum(e$income %in% "Prefer not to say")/nrow(e), 1), "% socio-demo screened due to PNR income\n"))
  cat(paste0(round(100*sum(e$urbanity %in% 0 | e$region %in% 0)/nrow(e), 1), "% socio-demo screened due to unrecognized zipcode\n"))
  if (all) cat(paste0(round(100*sum(e$age_exact %in% "Below 18")/nrow(e), 1), "% socio-demo screened due to age < 18\n"))
  cat(paste0(sum(!e$Q_TerminateFlag %in% "QuotaMet"), " valid: not quota met or socio-demo screened \n"))
  cat(paste0(sum(e$Q_TerminateFlag %in% "Screened"), " screened after socio-demo\n"))
  if (all) cat(paste0(sum(e$Finished %in% c(TRUE, "TRUE", 1)), " finished\n"))
  if (all) cat(paste0(sum((!is.na(e$Q_TerminateFlag)) & !e$Q_TerminateFlag %in% c("Screened", "QuotaMet")), " unknown Q_TerminateFlag\n"))
  if (all) cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "Screened")/nrow(e), 1), "% screened in total \n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "Screened")/sum(!e$Q_TerminateFlag %in% "QuotaMet"), 1), "% screened among valid \n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little"))/sum(!e$Q_TerminateFlag %in% "QuotaMet"), 1), "% screened among valid due to failed attention_test\n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "Screened" & e$Q_TotalDuration < 360)/sum(!e$Q_TerminateFlag %in% "QuotaMet"), 1), "% screened among valid due to duration < 360\n"))
  cat(paste0(round(100*sum(e$Q_TerminateFlag %in% "Screened" & !(e$attention_test %in% "A little") & e$Q_TotalDuration < 360)/sum(!e$Q_TerminateFlag %in% "QuotaMet"), 1), "% screened among valid due to both reasons\n"))
  if (all) cat(paste0(sum((e$Q_TerminateFlag %in% "Screened" & (e$attention_test %in% "A little") & e$Q_TotalDuration >= 420) | # TODO for non-pilot: replace 420 by 360
                            (is.na(e$Q_TerminateFlag) & ((!e$attention_test %in% "A little") | e$Q_TotalDuration < 360))), " unexplained screened or unexplained non-screened\n"))
  # if (all) cat(paste0(sum((e$Q_TerminateFlag %in% "Screened" & (e$attention_test %in% "A little") & e$Q_TotalDuration >= 420)), " unexplained screened\n"))
  # if (all) cat(paste0(sum((is.na(e$Q_TerminateFlag) & ((!e$attention_test %in% "A little") | e$Q_TotalDuration < 360))), " unexplained non-screened\n"))
  if (all) cat(paste0(sum(is.na(e$Q_TerminateFlag)), " legit: not quota met nor screened out\n"))
  cat(paste0(round(100*sum(!e$Finished %in% c(TRUE, "TRUE", 1))/sum(is.na(e$Q_TerminateFlag)), 1), "% dropout among legit\n"))
  if (all) cat(paste0(round(100*sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag))/sum(is.na(e$Q_TerminateFlag)), 1), "% finished among legit\n"))
  cat(paste0(sum(e$Finished %in% c(TRUE, "TRUE", 1) & is.na(e$Q_TerminateFlag)), " final: finished, not quota met nor screened out\n"))
  if (all) cat("Progress among legit\n")
  if (all) cat(decrit(e$Progress[(is.na(e$Q_TerminateFlag))], weight = F))
  if (all) cat(paste0(sum((is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)), " in final sample: finished and legit\n"))
  cat("Duration in final sample")
  print(decrit(e$Q_TotalDuration[(is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)]/60, weight = F))
  if (all) {
    cat("Duration in final sample for cut == 1\n")
    print(decrit(e$Q_TotalDuration[e$cut == 1 & (is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)]/60, weight = F))
    cat("Duration in final sample for cut == 0\n")
    print(decrit(e$Q_TotalDuration[e$cut == 0 & (is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)]/60, weight = F))
    cat("Duration in final sample for long\n")
    print(decrit(e$Q_TotalDuration[e$long > .42 & (is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)]/60, weight = F))
    cat("Duration in final sample for short\n")
    print(decrit(e$Q_TotalDuration[e$long < .42 & (is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)]/60, weight = F))
  }
  print(summary(lm(Q_TotalDuration/60 ~ (long > .42) , data = e, subset = ((is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)))))
  if (all) print(summary(lm(Q_TotalDuration/60 ~ (long > .42) * cut, data = e, subset = ((is.na(e$Q_TerminateFlag)) & e$Finished %in% c(TRUE, "TRUE", 1)))))
  cat("___________________________________________")
}
# for (p in pilots) stats_exclude(p)
stats_exclude("USp")
# e <- read_csv(paste0("../data_raw/USp.csv"))
# write.csv(e$zipcode[e$urbanity %in% 0], "../data_ext/unrecognized_zipcodes_US.csv", quote = F, row.names = F)

weighting <- function(e, country = e$country[1], printWeights = T, variant = NULL, min_weight_for_missing_level = F, trim = T) {
  if (!missing(variant)) print(variant)
  vars <- quotas[[paste0(c(country, variant), collapse = "_")]]
  freqs <- list()
  for (v in vars) {
    if (!(v %in% names(e))) warning(paste(v, "not in data"))
    e[[v]] <- as.character(e[[v]])
    e[[v]][is.na(e[[v]])] <- "NA"
    var <- ifelse(v %in% names(levels_quotas), v, paste(country, v, sep="_"))
    if (!(var %in% names(levels_quotas))) warning(paste(var, "not in levels_quotas"))
    levels_v <- as.character(levels_quotas[[var]])
    missing_levels <- setdiff(levels(as.factor(e[[v]])), levels_v)
    present_levels <- which(levels_v %in% levels(as.factor(e[[v]])))
    if (length(present_levels) != length(levels_v)) warning(paste0("Following levels are missing from data: ", var, ": ", 
        paste(levels_v[!1:length(levels_v) %in% present_levels], collapse = ', '), " (for ", country, "). Weights are still computed, neglecting this category."))
    prop_v <- pop_freq[[country]][[var]][present_levels]
    if (min_weight_for_missing_level) freq_missing <- rep(0.000001, length(missing_levels)) # imputes 0 weight for levels present in data but not in the weight's definitio
    else freq_missing <- vapply(missing_levels, function(x) sum(e[[v]]==x), FUN.VALUE = c(0))
    freq_v <- c(prop_v*(nrow(e)-sum(freq_missing)), freq_missing)
    df <- data.frame(c(levels_v[present_levels], missing_levels), freq_v)
    # df <- data.frame(c(levels_v, missing_levels), nrow(e)*c(pop_freq[[country]][[var]], rep(0.0001, length(missing_levels))))
    names(df) <- c(v, "Freq")
    freqs <- c(freqs, list(df))
  }
  # print(freqs)
  unweigthed <- svydesign(ids=~1, data=e)
  raked <- rake(design= unweigthed, sample.margins = lapply(vars, function(x) return(as.formula(paste("~", x)))), population.margins = freqs)
  
  if (printWeights) {    print(summary(weights(raked))  )
    print(paste("(mean w)^2 / (n * mean w^2): ", representativity_index(weights(raked)), " (pb if < 0.5)")) # <0.5 : problématique
    print(paste("proportion not in [0.25; 4]: ", round(length(which(weights(raked)<0.25 | weights(raked)>4))/ length(weights(raked)), 3), "Nb obs. in sample: ", nrow(e)))
  }
  if (trim) return(weights(trimWeights(raked, lower=0.25, upper=4, strict=TRUE)))
  else return(weights(raked, lower=0.25, upper=4, strict=TRUE))
}

prepare <- function(country = "US", scope = "final", fetch = T, convert = T, rename = T, duration_min = 360, pilot = FALSE, weighting = TRUE, remove_id = NULL) { # scope: all, stayed, final
  sample_name <- paste0(country, if (pilot) "p" else NULL)
  if (is.null(remove_id)) remove_id <- sample_name != "USp"
  if (fetch) {
    print(country)
    survey_list <- all_surveys()
    e <- fetch_survey(survey_list$id[survey_list$name == paste0(country, if (pilot) "_pilot" else NULL)], include_display_order = T, verbose = T, convert = F, col_types = cols("m" = col_character()))
    if (!remove_id) e$ExternalReference <- e$m
    if (!remove_id) e$DistributionChannel <- e$IPAddress
    e <- e[,which(!(names(e) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
    if (rename) e <- rename_survey(e, pilot = pilot)
    for (v in names(e)) label(e[[v]]) <- c(v = paste0(v, ": ", label(e[[v]])))
    write_csv(e, paste0("../data_raw/", sample_name, ".csv"), na = "")
    saveRDS(label(e), paste0("../data_raw/labels/", sample_name, ".rds"))
    e <- read_csv(paste0("../data_raw/", sample_name, ".csv"))
  }
  e <- read_csv(paste0("../data_raw/", sample_name, ".csv"))
  labels <- readRDS(paste0("../data_raw/labels/", sample_name, ".rds"))
  for (v in names(e)) label(e[[v]]) <- labels[[v]]
  
  if (convert) {
    all_na <- c()
    for (v in 1:ncol(e)) if (all(is.na(e[[v]])) & is.na(names(e)[[v]])) all_na <- c(all_na, v)
    e <- e[, setdiff(1:ncol(e), all_na)]
    
    e$fast <- e$duration < duration_min
    label(e$fast) <- paste0("fast: T/F duration < ", duration_min/60, " min")
    # e$excluded <- e$Q_TerminateFlag
    # e$finished <- e$Finished
    e$valid <- !e$excluded %in% "QuotaMet" 
    label(e$valid) <- "valid: T/F Not quota met or socio-demo screened (excluded != QuotaMet)"
    e$legit <- is.na(e$excluded) 
    label(e$legit) <- "legit: T/F Not excluded (not quota met nor screened) (is.na(excluded))"
    e$dropout <- is.na(e$excluded) & !e$finished %in% c(TRUE, "TRUE", 1, "1") 
    label(e$dropout) <- "dropout: T/F Chose to leave the survey (is.na(excluded) & !finished)"
    e$stayed <- e$finished %in% c(TRUE, "TRUE", 1, "1") & !e$excluded %in% "QuotaMet" # TODO: check includes failed attention_test
    label(e$stayed) <- "stayed: T/F Did not drop out (excluded != QuotaMet & finished)" # Includes Screened (fast or quality) but excludes dropout
    e$final <- is.na(e$excluded) & e$finished %in% c(TRUE, "TRUE", 1, "1") 
    label(e$final) <- "final: T/F In Final sample (!excluded & finished)"
    # progress_socio <- if (country == "US1") 19 else { if (country == "US2") 14 else 15 }
    # e$dropout_late <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$excluded) & e$finished != "1" & n(e$progress) >= progress_socio
    # label(e$dropout_late) <- "dropout: Respondent who did not complete the survey though was not excluded, and who dropped out after the socio-demographic questions." }
    if (scope %in% names(e)) e <- e[e[[scope]] == T,]
    
    e <- convert(e, country = country, pilot = pilot, weighting = weighting)
    e <- e[,!duplicated(names(e))]
  }
  
  if (weighting) {
    e$weight <- weighting(e, country)
    e$weight_all <- weighting(e, country, variant = "all")
    if (("vote_us" %in% names(e) & (sum(e$vote_us=="PNR/no right")!=0)) | ("vote" %in% names(e))) e$weight_vote <- weighting(e, country, variant = "vote")
    if (country == "EU") { for (c in countries_EU) e$weight_country[e$country == c] <- weighting(e[e$country == c,], c) } else e$weight_country <- e$weight
  } else {
    e$weight <- e$weight_country <- 1
  }
  
  if (sample_name == "USp") {
    invalid_IDs <- read.csv("../data_ext/invalid_IDs_USp.csv", colClasses = "character")[[1]]
    # print(decrit(e$gender[e$id %in% invalid_IDs], weight = F))
    # print(decrit(e$age[e$id %in% invalid_IDs], weight = F))
    # print(decrit(e$education[e$id %in% invalid_IDs & e$age > 25 & e$age < 65], weight = F))
    # e$urban_rural <- e$urbanity
    # e <- create_item("urban_rural", labels = c("Cities" = 1, "Rural" = 2), values = list(1, c(2:4)), df = e)
    # print(decrit(e$urban_rural[e$id %in% invalid_IDs], weight = F))
    # print(decrit(e$income_quartile[e$id %in% invalid_IDs], weight = F))
    # print(decrit(e$region[e$id %in% invalid_IDs], weight = F))
    # print(decrit(e$race[e$id %in% invalid_IDs], weight = F))
    e <- e[!e$id %in% invalid_IDs, ]
    e$id <- NA
  }
  
  return(e)
}

define_var_lists <- function() {
  text_support <<- c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")
  text_pnr <<- c("Prefer not to say")
  variables_solidarity_support <<- c("solidarity_support_billionaire_tax", "solidarity_support_corporate_tax", "solidarity_support_expanding_security_council", "solidarity_support_foreign_aid", 
    "solidarity_support_debt_relief", "solidarity_support_bridgetown", "solidarity_support_loss_damage", "solidarity_support_ncqg_300bn", "solidarity_support_shipping_levy", "solidarity_support_aviation_levy")
  variables_solidarity_support_short <<- paste0(c("solidarity_support_billionaire_tax", "solidarity_support_corporate_tax", "solidarity_support_expanding_security_council", "solidarity_support_foreign_aid", 
                                     "solidarity_support_bridgetown"), "_short")
  # variables_support <<- names(e)[grepl('support', names(e))]
  variables_wealth_tax_support <<- c("global_tax_support", "hic_tax_support", "intl_tax_support")
  variables_top_tax_support <<- c("top1_tax_support", "top3_tax_support", "top1_tax_support_cut", "top3_tax_support_cut")
  variables_likert <<- c(variables_solidarity_support, top_tax_support, "reparations_support", variables_solidarity_support_short)
  variables_yes_no <<- c("ncs_support", "gcs_support", "ics_support", wealth_tax_support, "couple")
  variables_race <<- c("race_white", "race_black", "race_hispanic", "race_asian", "race_native", "race_hawaii", "race_other", "race_pnr")
  variables_home <<- c("home_tenant", "home_owner", "home_landlord", "home_hosted")
  variables_global_movement <<- c("global_movement_no", "global_movement_spread", "global_movement_demonstrate", "global_movement_strike", "global_movement_donate")
  variables_why_hic_help_lic <<- c("why_hic_help_lic_responsibility", "why_hic_help_lic_interest", "why_hic_help_lic_duty", "why_hic_help_lic_none")
  variables_custom_redistr <<- c("custom_redistr_satisfied", "custom_redistr_skip")
  variables_variant <<- c("variant_split", "variant_warm_glow", "variant_realism", "variant_ncqg_maritime", "variant_radical_redistr", "variant_gcs", "variant_sliders", "variant_radical_transfer", 
                          "variant_synthetic", "variant_comprehension", "variant_belief")
  # variables_variant_binary <<- c("variant_split", "variant_realism", "variant_ncqg_maritime", "variant_radical_redistr", "variant_sliders", "variant_radical_transfer", 
  #                                "variant_synthetic", "variant_comprehension", "variant_belief")
  variables_binary <<- c(variables_race, variables_home, variables_global_movement, variables_why_hic_help_lic, variables_custom_redistr)
  variables_duration <<- c("duration", "duration_consent", "duration_socios_demos", "duration_field", "duration_conjoint", "duration_global_tax", "duration_warm_glow_substitute", "duration_gcs", 
                           "duration_ics", "duration_warm_glow_realism", "duration_ncqg_maritime", "duration_wealth_tax", "duration_preferred_transfer_mean", "duration_radical_redistr", 
                           "duration_custom_redistr", "duration_well_being", "duration_extra", "duration_main_questions")
  variables_split_few <<- c("revenue_split_few_domestic_education_healthcare", "revenue_split_few_domestic_welfare", "revenue_split_few_domestic_tax_reduction", 
                            "revenue_split_few_domestic_deficit_reduction", "revenue_split_few_global")
  variables_split_many_domestic <<- c("revenue_split_many_domestic_education", "revenue_split_many_domestic_healthcare", "revenue_split_many_domestic_defense", "revenue_split_many_domestic_deficit_reduction", 
                             "revenue_split_many_domestic_justice_police", "revenue_split_many_domestic_pensions", "revenue_split_many_domestic_welfre", "revenue_split_many_domestic_infrastructure", 
                             "revenue_split_many_domestic_tax_reduction")
  variables_split_many_global <<- c("revenue_split_many_global_education_healthcare", "revenue_split_many_global_renewables_adaptation", 
  "revenue_split_many_global_loss_damage", "revenue_split_many_global_forestation")
  variables_split_many <<- c(variables_split_many_domestic, variables_split_many_global)
  variables_split_maritime <<- c("maritime_split_ldc", "maritime_split_companies", "maritime_split_decarbonization")
  variables_split <<- c(variables_split_few, variables_split_many, variables_split_maritime)
  variables_numeric <<- c(variables_duration, "hh_size", "Nb_children__14", "donation", "gcs_belief", variables_split)
  variables_well_being <<- c("well_being_gallup_0", "well_being_gallup_1", "well_being_wvs_0", "well_being_wvs_1")
  variables_transfer_how <<- c("transfer_how_agencies", "transfer_how_govt_conditional", "transfer_how_govt_unconditional", "transfer_how_local_authorities", 
                              "transfer_how_ngo", "transfer_how_social_protection", "transfer_how_cash_unconditional")
  variables_sustainable_future <<- c("sustainable_future_a", "sustainable_future_s", "sustainable_future_b")
}
define_var_lists()
# for (v in names(e)) if (length(unique(e[[v]])) == 2) print(v)
# for (v in names(e)) if ("No" %in% unique(e[[v]])) print(v)
# for (v in names(e)) if (is.logical(e[[v]])) print(v)
# names(e)[grepl('race', names(e))]
# cat(names(e)[grepl('sustainable', names(e)) & !grepl('order', names(e))], sep = '", "')

create_item <- function(var, labels, df, grep = FALSE, keep_original = FALSE, missing.values = NA, values = names(labels), annotation = NULL) {
  # Creates a memisc item s.t. var %in% values[[i]] will yield value/label labels[i]/names(labels)[i]
  # var: a character or vector of characters
  # labels: a named numeric vector
  # missing.values: a numeric or character vector
  # values: a vector of characters or a list of such vectors
  if (length(var) > 1) { 
    for (v in var) df <- create_item(v, df = df, labels = labels, grep = grep, missing.values = missing.values, values = values, annotation = NULL)
  } else { if (var %in% names(df)) {
    # print(var)
    if (keep_original) df[[paste0(var, "_original")]] <- df[[var]]
    temp <- NA
    for (i in seq(labels)) temp[if (grep) grepl(values[[i]], df[[var]]) else df[[var]] %in% values[[i]]] <- labels[i] 
    temp[is.na(df[[var]])] <- NA
    df[[var]] <- as.item(temp, labels = labels, grep = grep, missing.values = missing.values, annotation = if (is.null(annotation)) Label(df[[var]]) else annotation) 
  }  }
  return(df)
}

convert <- function(e, country = e$country[1], pilot = FALSE, weighting = TRUE) {
  define_var_lists()
  e$country_name <- countries_names[country]
  for (i in intersect(variables_numeric, names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector(gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in intersect(variables_duration, names(e))) e[[v]] <- e[[v]]/60
  label(e$duration) <- "duration: Duration (in min)"
  
  for (j in intersect(variables_binary, names(e))) { # TODO: variant NAs
    temp <- label(e[[j]])
    e[[j]] <- !is.na(e[[j]])
    # e[[j]] <- e[[j]] %in% "" # e[[j]][e[[j]]!=""] <- TRUE
    # e[[j]][is.na(e[[j]])] <- FALSE
    label(e[[j]]) <- temp
  }
  
  if ("race_black" %in% names(e)) {
    e$race <- "Other"
    e$race[e$race_white==T & e$race_asian == FALSE & e$race_native == FALSE] <- "White only"
    e$race[e$race_hispanic==T] <- "Hispanic"
    e$race[e$race_black==T] <- "Black"
    if (any(e$race == "White only")) e$race <- relevel(as.factor(e$race), "White only")
    label(e$race) <- "race: White only/Hispanic/Black/Other. True proportions: .601/.185/.134/.08"
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% "A little"
  
  # for (j in intersect(variables_yes_no, names(e))) {
  #   temp <- 1*(e[j][[1]] %in% "Yes") - 0.1*(e[j][[1]] %in% text_pnr) # - (e[j][[1]] %in% text_no)
  #   temp[is.na(e[j][[1]])] <- NA
  #   e[j][[1]] <- as.item(temp, labels = c("No" = 0, "PNR" = -0.1, "Yes" = 1), missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
  # }
  # 
  # for (v in intersect(variables_likert, names(e))) {
  #   if (v %in% names(e)) {
  #     temp <- 2 * (e[[v]] %in% "Strongly support") + (e[[v]] %in% "Somewhat support") - (e[[v]] %in% "Somewhat oppose") - 2 * (e[[v]] %in% "Strongly oppose")
  #     temp[is.na(e[[v]])] <- NA
  #     # e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")), missing.values=c(NA), annotation=Label(e[[v]])) 
  #     e[[v]] <- as.item(temp, labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), missing.values = NA, annotation=Label(e[[v]])) 
  # } }
  e$variant_long <- e$long > .42 # info_solidarity; nb_solidarity; top_tax_support
  # cut: fields; transfer_how OR radical redistr; custom redistr; comprehension
  e$age <- e$age_exact
  e <- create_item("age", labels = c("18-24" = 21.5, "25-34" = 30, "35-49" = 42.5, "50-64" = 57.5, "65+" = 71), 
                   values = list(c("18 to 20", "21 to 24"), c("25 to 29", "30 to 34"), c("35 to 39", "40 to 44", "45 to 49"), c("50 to 54", "55 to 59", "60 to 64"), 
                                 c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 99", "100 or above")), df = e)
  e <- create_item("education", labels = c("Below upper secondary" = 1, "Upper secondary" = 2, "Above upper secondary" = 3), grep = T, keep_original = T, values = c("1|2", "3", "4|5|6|7"), df = e)
  e$income_quartile <- e$income
  e <- create_item("income_quartile", labels = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "PNR" = 0), values = c("100|200|250", "300|400|500", "600|700|750", "800|900", "not"), grep = T, missing.values = c("PNR"), df = e)  
  # e$urban_rural <- e$urbanity
  # e <- create_item("urban_rural", labels = c("Cities" = 1, "Rural" = 2), values = list(1, c(2:4)), df = e)
  e <- create_item(variables_yes_no, labels = c("No" = 0, "PNR" = -0.1, "Yes" = 1), values = c("No", list(text_pnr), "Yes"), missing.values = c("", NA, "PNR"), df = e)
  e <- create_item(variables_likert, c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), df = e)
  e <- create_item("millionaire", c("Very unlikely" = -3, "Unlikely" = -1, "Likely" = 1, "Very likely" = 3, "I am already a millionaire" = 5), df = e)
  e <- create_item("likely_solidarity", c("Very unlikely" = -3, "Unlikely" = -1, "Likely" = 1, "Very likely" = 3), df = e)
  e <- create_item("ncqg", c("Stop" = 0, "Reduce" = 1, "Maintain ($26 bn)" = 2, "More loans" = 3, "Intermediate ($200 bn)" = 4, "Developing ($600 bn)" = 5, "NGOs ($1,000 bn)" = 6),
                   grep = T, keep_original = T, values = c("Stop", "Reduce", "\\$26", "Increase loans", "\\$200", "\\$600", "\\$1,000"), df = e)
  e <- create_item("ncqg_full", c("$0" = 0, "$26 bn" = 26, "$100 bn" = 100, "300 bn" = 300, "$600 bn" = 600, "$1,000 bn" = 1000, "$5,000 bn" = 5000),
                   grep = T, keep_original = T, values = c("\\$0", "\\$26", "\\$100", "\\$300", "\\$600", "\\$1,000", "\\$5,000"), df = e)
  if (all(c("ncqg", "ncqg_full") %in% names(e))) {
    e$variant_ncqg <- ifelse(e$variant_ncqg_maritime %in% 2, "full", "new")
    label(e$variant_ncqg) <- "variant_ncqg: full/new. full: A lot of explanations, answers in numerical grant-equivalent; new: Shorter, answers in terms of who defends them or what they mean."
    # e$ncqg_fusion <- as.character(e$ncqg_original)
    # e$ncqg_fusion[e$variant_ncqg %in% "full"] <- as.character(e$ncqg_full_original)[e$variant_ncqg %in% "full"]
    e$ncqg_fusion <- ifelse(e$variant_ncqg %in% "full", as.character(e$ncqg_full_original), as.character(e$ncqg_original))
    e <- create_item("ncqg_fusion", c("$0" = 0, "Less" = 10, "Stable" = 26, "More loans" = 30, "$100 bn" = 100, "$200 bn" = 200, "300 bn" = 300, "$600 bn" = 600, "$1,000 bn" = 1e3, "$5,000 bn" = 5e3),
                   grep = T, values = c("Stop|\\$0", "Reduce", "\\$26", "Increase loans", "\\$100", "\\$200", "\\$300", "\\$600", "\\$1,000", "\\$5,000"), df = e)
  }
  e <- create_item(variables_transfer_how, c("Wrong" = -1, "Acceptable" = 0, "Right" = 1, "Best" = 2), grep = T, values = c("wrong", "acceptable", "right", "best"), df = e)
  e$variant_sustainable_future <- ifelse(!is.na(e$sustainable_future_a), "a", ifelse(!is.na(e$sustainable_future_b), "b", "s")) # variant_radical_redistr
  label(e$variant_sustainable_future) <- "variant_sustainable_future: a/b/s a: A == sustainable / b: B == sustainable / s: B == sustainable and shorter (bullet points)."
  e$sustainable_future <- ifelse(grepl("B", e$sustainable_future_b) | grepl("B", e$sustainable_future_s) | grepl("A", e$sustainable_future_a), T, F)
  e <- create_item("vote_intl_coalition", c("Less likely" = -1, "Equally likely" = 0, "More likely" = 1), grep = T, values = c("less likely", "not depend", "more likely"), df = e)
  e <- create_item("gcs_comprehension", c("decrease" = -1, "not be affected" = 0, "increase" = 1), df = e)
  e$gcs_understood <- e$gcs_comprehension == 1
  e <- create_item("my_tax_global_nation", c("Strongly disagree" = -2, "Disagree" = -1, "Neither agree nor disagree" = 0, "Agree" = 1, "Strongly agree" = 2), df = e)
  e <- create_item("group_defended", c("Family and self" = -2, "Region, continent or religion" = -1, "Fellow citizens" = 0, "Humans" = 1, "Sentient beings" = 2),
                   grep = T, values = c("family", "religion", "Americans", "Humans", "Sentient"), df = e) # In NHB 0-7, Relatives 1; Culture/religion 3; Europeans 5
  e <- create_item("survey_biased", c("Yes, left" = -1, "Yes, right" = 0, "No" = 1), grep = T, values = c("left", "right", "No"), df = e)

  # TODO: variant, sociodemos, field, conjoint
  
  for (v in variables_well_being) e[[paste0(v, "_original")]] <- e[[v]]
  for (v in variables_well_being) e[[v]] <- as.numeric(gsub("[^0-9]", "", e[[v]])) # TODO: label
  
  e$info_solidarity <- e$variant_realism %in% 1
  e$variant_info_solidarity <- ifelse(e$info_solidarity, ifelse(e$variant_long, "Long info", "Short info"), "No info")
  
  for (v in variables_solidarity_support_short) e[[sub("_short", "", v, "_long")]] <- e[[sub("_short", "", v)]]
  for (v in variables_solidarity_support_short) e[[sub("_short", "", v)]] <- ifelse(e$variant_long, e[[sub("_short", "", v)]], e[[v]])
  e$share_solidarity_short_supported <- rowMeans((e[, sub("_short", "", variables_solidarity_support_short)]) > 0)  
  e$share_solidarity_short_opposed <- rowMeans((e[, sub("_short", "", variables_solidarity_support_short)]) < 0)  
  e$share_solidarity_supported <- rowMeans((e[, variables_solidarity_support]) > 0)  
  e$share_solidarity_opposed <- rowMeans((e[, variables_solidarity_support]) < 0)  
  
  e$top1_tax_support <- ifelse(e$cut, e$top1_tax_support_cut, e$top1_tax_support)
  e$top3_tax_support <- ifelse(e$cut, e$top3_tax_support_cut, e$top3_tax_support)
  e$top_tax_support <- ifelse(e$variant_radical_redistr == 0, e$top1_tax_support, e$top3_tax_support) # TODO: label
  e$variant_top_tax <- ifelse(e$variant_radical_redistr == 0, "top1", "top3")
  e$variant_top_tax_full <- paste0(e$variant_top_tax, ifelse(e$variant_long, "_long", "_short"))
  
  e$well_being <- e$variant_well_being <- NA
  for (v in variables_well_being) {
    e$variant_well_being[!is.na(e[[v]])] <- sub("well_being_", "", v)
    e$well_being[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])] }
  e$variant_well_being_scale <- ifelse(grepl("0", e$variant_well_being), "10", "9")
  e$variant_well_being_wording <- ifelse(grepl("gallup", e$variant_well_being), "Gallup", "WVS")
  
  e$variant_wealth_tax <- e$wealth_tax_support <- NA
  for (v in variables_wealth_tax_support) e$variant_wealth_tax[!is.na(e[[v]])] <- sub("_tax_support", "", v)
  for (v in variables_wealth_tax_support) e$wealth_tax_support[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])]
  e <- create_item("wealth_tax_support", c("No" = 0, "Yes" = 1), values = c(0, 1), missing.values = c("", NA), df = e)
  
  e$split_nb_global <- rowSums(!is.na(e[, variables_split_many_global]))
  e$split_nb_global[e$variant_split == 1] <- NA
  e$split_many_global <- rowSums(e[, variables_split_many_global], na.rm = T)
  e$split_many_global[!e$split_nb_global %in% 1:4] <- NA
  
  e$split_both_global <- ifelse(e$variant_split == 1, e$revenue_split_few_global, e$split_many_global)
  e$split_both_global[e$split_nb_global %in% 0] <- NA
  e$split_both_nb_global <- ifelse(e$variant_split == 1, 1, e$split_nb_global)
  
  # TODO update for non-pilot
  e$race_asked <- e$country %in% "US"
  e$custom_redistr_asked <- e$cut %in% 0
  e$radical_redistr_asked <- e$why_hic_help_lic_asked <- e$global_movement_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 1
  # e$global_movement_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 1
  # e$transfer_how_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 0
  for (l in c("race", "global_movement", "why_hic_help_lic", "custom_redistr")) {
    for (v in eval(str2expression(paste0("variables_", l)))) e[[v]][!e[[paste0(l, "_asked")]]] <- NA
  }

  # e$global_movement_any <- as.logical(rowSums(e[, variables_global_movement[2:5]]))
  # e$global_movement_any[!e$global_movement_asked] <- NA 
  # e$why_hic_help_lic_any <- as.logical(rowSums(e[, variables_why_hic_help_lic[1:3]]))
  # e$why_hic_help_lic_duty[!e$why_hic_help_lic_asked] <- NA
  
  return(e)
}

# Pilots
pilot_countries <- c("PL", "GB", "US")
pilot_data <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = F, convert = T, rename = T, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")) # remove_id = F
p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data)
list2env(pilot_data, envir = .GlobalEnv)

e <- USp

countries_names <- countries_names_fr <- c("Poland", "United Kingdom", "United States") # TODO
names(countries_names) <- pilot_countries
countries_EU <- c("Poland")
pilot_countries_all <- c(pilot_countries, "")
# sum(duplicated(p$distr))

for (v in names(p)[1:80]) { print(decrit(v, p)); print("____________");}
for (v in names(p)[81:160]) { print(decrit(v, p)); print("____________");}
for (v in names(p)[161:211]) { print(decrit(v, p)); print("____________");}
for (c in paste0(pilot_countries, "p")) print(paste(c, mean(d(c)$gcs_support %in% "Yes")))
summary(lm(gcs_support %in% "Yes" ~ country, data = p))
summary(lm(ics_support %in% "Yes" ~ variant_gcs, data = p))

for (i in 1:length(e)) {
  # label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
  print(paste(i, label(e[[i]])))
  # print(names(e)[i])
}
# list2env(setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = FALSE, convert = TRUE, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")), envir = .GlobalEnv)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, lapply(paste0(pilot_countries, "p"), function(c) d(c)))
# USp <- prepare(country = "US", scope = "final", fetch = F, convert = T, pilot = TRUE, weighting = FALSE)
# PLp <- prepare(country = "PL", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# GBp <- prepare(country = "GB", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, list(USp, PLp, GBp))













