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

prepare <- function(country = "US", scope = "final", fetch = T, convert = T, duration_min = 360, pilot = FALSE, weighting = TRUE) { # scope: all, stayed, final
  sample_name <- paste0(country, if (pilot) "p" else NULL)
  if (fetch) {
    print(country)
    survey_list <- all_surveys()
    e <- fetch_survey(survey_list$id[survey_list$name == paste0(country, if (pilot) "_pilot" else NULL)], include_display_order = T, verbose = T, convert = F)
    e <- e[,which(!(names(e) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
    for (v in names(e)) label(e[[v]]) <- c(v = paste0(v, ": ", label(e[[v]])))
    write_csv(e, paste0("../data_raw/", sample_name, ".csv"), na = "")
    saveRDS(label(e), paste0("../data_raw/labels/", sample_name, ".rds"))
    e <- read_csv(paste0("../data_raw/", sample_name, ".csv"))
  }
  e <- read_csv(paste0("../data_raw/", sample_name, ".csv"))
  labels <- readRDS(paste0("../data_raw/labels/", sample_name, ".rds"))
  for (v in names(e)) label(e[[v]]) <- labels[[v]]
  
  if (convert) {
    # e <- rename(e, country = country, pilot = pilot)
    all_na <- c()
    for (v in 1:ncol(e)) if (all(is.na(e[[v]])) & is.na(names(e)[[v]])) all_na <- c(all_na, v)
    e <- e[, setdiff(1:ncol(e), all_na)]
    
    e$speeder <- e$Q_TotalDuration < duration_min
    label(e$speeder) <- paste0("speeder: T/F duration < ", duration_min/60, " min")
    # s$excluded <- e$Q_TerminateFlag
    e$valid <- !e$Q_TerminateFlag %in% "QuotaMet" 
    label(e$valid) <- "valid: T/F Not quota met or socio-demo screened (excluded != QuotaMet)"
    e$legit <- is.na(e$Q_TerminateFlag) 
    label(e$legit) <- "legit: T/F Not excluded (not quota met nor screened) (is.na(excluded))"
    e$dropout <- is.na(e$Q_TerminateFlag) & !e$Finished %in% c(TRUE, "TRUE", 1, "1") 
    label(e$dropout) <- "dropout: T/F Chose to leave the survey (is.na(excluded) & !finished)"
    e$stayed <- e$Finished %in% c(TRUE, "TRUE", 1, "1") & !e$Q_TerminateFlag %in% "QuotaMet" # TODO: check includes failed attention_test
    label(e$stayed) <- "stayed: T/F Did not drop out (excluded != QuotaMet & finished)" # Includes Screened (speeder or quality) but excludes dropout
    e$final <- is.na(e$Q_TerminateFlag) & e$Finished %in% c(TRUE, "TRUE", 1, "1") 
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

convert <- function(e, country = e$country[1], pilot = FALSE, weighting = TRUE) {
  return(e)
}

# Pilots
pilot_countries <- c("PL", "GB", "US")
pilot_data <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = FALSE, convert = TRUE, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p"))
p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data)
list2env(pilot_data, envir = .GlobalEnv)

# list2env(setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = FALSE, convert = TRUE, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")), envir = .GlobalEnv)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, lapply(paste0(pilot_countries, "p"), function(c) d(c)))
# USp <- prepare(country = "US", scope = "final", fetch = F, convert = T, pilot = TRUE, weighting = FALSE)
# PLp <- prepare(country = "PL", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# GBp <- prepare(country = "GB", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, list(USp, PLp, GBp))













