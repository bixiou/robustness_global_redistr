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

prepare <- function(incl_quality_fail = FALSE, exclude_speeder=TRUE, exclude_screened=TRUE, only_Finished=TRUE, only_known_agglo=T, Q_TotalDuration_min=360, 
                    country = "US", wave = NULL, weighting = TRUE, zscores = T, zscores_dummies = FALSE, remove_id = FALSE, 
                    efa = FALSE, combine_age_50 = T, define_var_lists = T) { 
  filename <- paste0(c(country, wave), collapse="")
  file <- paste0("../data_raw/", filename, ".csv")
  # if (remove_id) remove_id(filename)
  e <- read_csv(file)
  
  if (missing(wave)) wave <- "full"
  # e <- relabel_and_rename(e, country = country, wave = wave)
  all_na <- c()
  for (v in 1:ncol(e)) if (all(is.na(e[[v]])) & is.na(names(e)[[v]])) all_na <- c(all_na, v)
  e <- e[, setdiff(1:ncol(e), all_na)]
  
  print(paste(length(which(e$Q_TerminateFlag=="QuotaMet")), "QuotaMet"))
  e$Finished[e$Q_TerminateFlag=="QuotaMet"] <- "False" # To check the number of QuotaMet that shouldn't have incremented the quota, comment this line and: decrit(e$each_strate[e$exclu=="QuotaMet" & e$csp=="Employé" & !grepl("2019-03-04 07", e$date)])
  if (incl_quality_fail & "attention_test" %in% names(e)) e <- e[replace_na(e$Q_TerminateFlag, "na") != "QuotaMet" & !is.na(e$attention_test) & !(replace_na(e$Q_TerminateFlag, "na") == "Screened" & replace_na(e$attention_test) == "A little"),] # allqa: e[e$Finished == 1
  if (exclude_screened & !incl_quality_fail) { e <- e[is.na(e$Q_TerminateFlag),] }
  if (exclude_speeder) e <- e[as.numeric(as.vector(e$Q_TotalDuration)) > Q_TotalDuration_min,] # & !incl_quality_fail
  if (only_Finished & !incl_quality_fail) e <- e[e$Finished==1,] 
  # if (only_Finished | incl_quality_fail) { # TODO: le faire marcher même pour les autres
  e <- convert(e, country = country, wave = wave, weighting = weighting, zscores = zscores, zscores_dummies = zscores_dummies, efa = efa, combine_age_50 = combine_age_50, only_Finished = only_Finished, define_var_lists = define_var_lists)
  e <- e[,!duplicated(names(e))]
  # if (!incl_quality_fail) e <- e[e$attention_test == T, ] # TODO!
  if (weighting) {
    e$weight <- weighting(e, sub("[0-9p]+", "", country))
    e$weight_all <- weighting(e, sub("[0-9p]+", "", country), variant = "all")
    if (("vote_us" %in% names(e) & (sum(e$vote_us=="PNR/no right")!=0)) | ("vote" %in% names(e))) e$weight_vote <- weighting(e, sub("[0-9]+[a-z]*", "", country), variant = "vote")
    if (country == "EU") { for (c in countries_EU) e$weight_country[e$country == c] <- weighting(e[e$country == c,], c) } else e$weight_country <- e$weight
  }

  if ("attention_test" %in% names(e)) {
    e$failed_test <- no.na(e$attention_test) != "A little"
    label(e$failed_test) <- "failed_test: Failed the attention_test"
    e$valid <- (as.numeric(e$progress) > 1) & (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$Q_TerminateFlag)
    label(e$valid) <- "valid: Respondents that has not been screened out due to speed or failure to the attention test."
    e$dropout <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$Q_TerminateFlag) & e$Finished != "1"
    label(e$dropout) <- "dropout: Respondent who did not complete the survey though was not Q_TerminateFlag."
    if (country %in% c("US1", "US2", "EU")) {
      progress_socio <- if (country == "US1") 19 else { if (country == "US2") 14 else 15 }
      # max(as.numeric(e$progress[is.na(e$gcs_win_lose) & (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$Q_TerminateFlag) & e$Finished != "1"]))
      e$dropout_late <- (e$attention_test == "A little" | is.na(e$attention_test)) & is.na(e$Q_TerminateFlag) & e$Finished != "1" & n(e$progress) >= progress_socio
      label(e$dropout_late) <- "dropout: Respondent who did not complete the survey though was not Q_TerminateFlag, and who dropped out after the socio-demographic questions." }
    e$Finished_attentive <- (e$valid | (e$Q_TotalDuration <= Q_TotalDuration_min & e$attention_test=="A little")) & e$Finished==1
    label(e$Finished_attentive) <- "Finished_attentive: Respondent completed the survey and did not fail the attention test."
    e$stayed <- !e$dropout & no.na(e$Q_TerminateFlag) != "QuotaMet"
    label(e$stayed) <- "stayed: T/F quotas are allowed and do not drop out"
  }
  # e$sample <- "a"
  # e$sample[e$Finished=="True"] <- "e"
  # e$sample[e$Finished=="True" & e$Q_TotalDuration > Q_TotalDuration_min] <- "p"
  # e$sample[e$Finished=="True" & e$Q_TotalDuration > Q_TotalDuration_min & e$Q_TerminateFlag==""] <- "r"
  
  return(e)
}

