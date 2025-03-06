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
# GBp <- fetch_survey(survey_list$id[survey_list$name == "GB_pilot"], include_display_order = T, verbose = T, convert = F) # labels using sjlabelled package
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

stats_exclude <- function(data_name, all = F) {
  cat("\n")
  cat(paste("\nSURVEY:", data_name))
  cat("\n\n")
  e <- d(data_name)
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
for (p in pilots) stats_exclude(p)

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

prepare <- function(country = "US", scope = "final", fetch = T, convert = T, rename = T, duration_min = 360, pilot = FALSE, weighting = TRUE) { # scope: all, stayed, final
  sample_name <- paste0(country, if (pilot) "p" else NULL)
  if (fetch) {
    print(country)
    survey_list <- all_surveys()
    e <- fetch_survey(survey_list$id[survey_list$name == paste0(country, if (pilot) "_pilot" else NULL)], include_display_order = T, verbose = T, convert = F)
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
  }
  
  return(e)
}

define_var_lists <- function() {
  text_support <<- c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")
  text_pnr <<- c("Prefer not to say")
  variables_solidarity_support <<- c("solidarity_support_billionaire_tax", "solidarity_support_corporate_tax", "solidarity_support_expanding_security_council", "solidarity_support_foreign_aid", 
    "solidarity_support_debt_relief", "solidarity_support_bridgetown", "solidarity_support_loss_damage", "solidarity_support_ncqg_300bn", "solidarity_support_shipping_levy", "solidarity_support_aviation_levy")
  # variables_support <<- names(e)[grepl('support', names(e))]
  wealth_tax_support <<- c("global_tax_support", "hic_tax_support", "intl_tax_support")
  top_tax_support <<- c("top1_tax_support", "top3_tax_support", "top1_tax_support_cut", "top3_tax_support_cut")
  variables_likert <<- c(variables_solidarity_support, top_tax_support, "reparations_support")
  variables_yes_no <<- c("ncs_support", "gcs_support", "ics_support", wealth_tax_support, "couple")
  variables_race <<- c("race_white", "race_black", "race_hispanic", "race_asian", "race_native", "race_hawaii", "race_other", "race_pnr")
  variables_home <<- c("home_tenant", "home_owner", "home_landlord", "home_hosted")
  variables_global_movement <<- c("global_movement_no", "global_movement_spread", "global_movement_demonstrate", "global_movement_strike", "global_movement_donate")
  variables_why_hic_help_lic <<- c("why_hic_help_lic_responsibility", "why_hic_help_lic_interest", "why_hic_help_lic_duty", "why_hic_help_lic_none", "why_hic_help_lic_order_responsibility", 
                                   "why_hic_help_lic_order_interest", "why_hic_help_lic_order_duty")
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
  variables_split_many <<- c("revenue_split_many_domestic_education", "revenue_split_many_domestic_healthcare", "revenue_split_many_domestic_defense", "revenue_split_many_domestic_deficit_reduction", 
                             "revenue_split_many_domestic_justice_police", "revenue_split_many_domestic_pensions", "revenue_split_many_domestic_welfre", "revenue_split_many_domestic_infrastructure", 
                             "revenue_split_many_domestic_tax_reduction", "revenue_split_many_global_education_healthcare", "revenue_split_many_global_renewables_adaptation", 
                             "revenue_split_many_global_loss_damage", "revenue_split_many_global_forestation")
  variables_split_maritime <<- c("maritime_split_ldc", "maritime_split_companies", "maritime_split_decarbonization")
  variables_split <<- c(variables_split_few, variables_split_many, variables_split_maritime)
  variables_numeric <<- c(variables_duration, "hh_size", "Nb_children__14", "donation", "gcs_belief", variables_split)
}
# for (v in names(e)) if (length(unique(e[[v]])) == 2) print(v)
# for (v in names(e)) if ("No" %in% unique(e[[v]])) print(v)
# for (v in names(e)) if (is.logical(e[[v]])) print(v)
# names(e)[grepl('race', names(e))]
# cat(names(e)[grepl('split', names(e)) & !grepl('order', names(e))], sep = '", "')

convert <- function(e, country = e$country[1], pilot = FALSE, weighting = TRUE) {
  define_var_lists()
  # sociodemos, millionaire, field, conjoint, split, likely_solidarity, gcs_belief, donation, ncqg, sustainable_future, attention, transfer_how, vote_intl_coalition, 
  # well_being, gcs_comprehension, my_tax_global_nation, group_defended, survey_biased, long
  
  for (i in intersect(variables_numeric, names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector(gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in intersect(variables_duration, names(e))) e[[v]] <- e[[v]]/60
  
  for (j in intersect(variables_yes_no, names(e))) {
    temp <- 1*(e[j][[1]] %in% "Yes") - 0.1*(e[j][[1]] %in% text_pnr) # - (e[j][[1]] %in% text_no)
    temp[is.na(e[j][[1]])] <- NA
    e[j][[1]] <- as.item(temp, labels = c("No" = 0, "PNR" = -0.1, "Yes" = 1), missing.values = c("",NA,"PNR"), annotation=attr(e[j][[1]], "label"))
  }
  
  for (j in intersect(variables_binary, names(e))) { # TODO: variant NAs
    temp <- label(e[[j]])
    e[[j]] <- !is.na(e[[j]])
    # e[[j]] <- e[[j]] %in% "" # e[[j]][e[[j]]!=""] <- TRUE
    # e[[j]][is.na(e[[j]])] <- FALSE
    label(e[[j]]) <- temp
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% "A little"
  
  for (v in intersect(variables_likert, names(e))) {
    if (v %in% names(e)) {
      temp <-  temp <- 2 * (e[[v]] %in% "Strongly support") + (e[[v]] %in% "Somewhat support") - (e[[v]] %in% "Somewhat oppose") - 2 * (e[[v]] %in% "Strongly oppose")
      temp[is.na(e[[v]])] <- NA
      # e[[v]] <- as.item(temp, labels = structure(c(-2:2), names = c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")), missing.values=c(NA), annotation=Label(e[[v]])) 
      e[[v]] <- as.item(temp, labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), missing.values = NA, annotation=Label(e[[v]])) 
  } }
  
  return(e)
}

# Pilots
pilot_countries <- c("PL", "GB", "US")
pilot_data <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = T, convert = T, rename = T, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p"))
p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data)
list2env(pilot_data, envir = .GlobalEnv)

e <- USp

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













