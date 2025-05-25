# TODO: Quota education JP
# TODO: labels
# TODO: fields
# TODO: custom redistr: tax rates; dummy whether decrease own income; sociodemos determinants
# TODO: conjoint (need Paris)

# check:
# no NA in well_being, group_defended, also in pilots
# mean(e$convergence_support > 0)
# sapply(countries[-9], function(c) round(mean(d(c)$convergence_support > 0, na.rm = T), 3))
# results of global_tax_attitudes with new def of swing_state / dem
# missing urbanity, region: decrit(e$country[is.na(e$region)]) GB$zipcode[is.na(GB$region)]


##### Parameters #####
countries <- c("FR", "DE", "IT", "PL", "ES", "GB", "CH", "JP", "RU", "SA", "US")
countries_names <- c("FR" = "France", "DE" = "Germany", "IT" = "Italy", "PL" = "Poland", "ES" = "Spain", "GB" = "United Kingdom", "CH" = "Switzerland", "JP" = "Japan", "RU" = "Russia", "SA" = "Saudi Arabia", "US" = "USA")
countries_names_fr <- c("FR" = "France", "DE" = "Allemagne", "IT" = "Italie", "PL" = "Pologne", "ES" = "Espagne", "GB" = "Royaume-Uni", "CH" = "Suisse", "JP" = "Japon", "RU" = "Russie", "SA" = "Arabie Saoudite", "US" = "États-Unis")
# names(countries_names) <- pilot_countries
countries_EU <- countries_names[1:5]
countries_Eu <- countries_names[1:7]
pilot_countries <- c("PL", "GB", "US")
pilot_countries_all <- c(pilot_countries, "")
special_levels <- list("All" = list("var" = "country_name", "value" = countries_names), "$ bold('All')" = list("var" = "country_name", "value" = countries_names),
                       "Europe" = list("var" = "country_name", "value" = countries_Eu), "$ bold('Europe')" = list("var" = "country_name", "value" = countries_Eu), 
                       "European Union" = list("var" = "country_name", "value" = countries_EU), "$ bold('European Union')" = list("var" = "country_name", "value" = countries_EU),
                       "Saudi citizens" = list("var" = "saudi", "value" = T),
                       "U.S. Democrats" = list("var" = "vote_voters", "value" = "Harris"),
                       "U.S. Republicans" = list("var" = "vote_voters", "value" = "Trump"),
                       "U.S. Non-voters" = list("var" = "vote_voters", "value" = "Non-voter or PNR"))
levels_default <- c("$ bold('All')", "$ bold('Europe')", countries_names)
levels_EU <- c("$ bold('All')", "$ bold('European Union')", countries_names)
levels_saudi <- c("$ bold('All')", "$ bold('Europe')", countries_names[1:10], "Saudi citizens", countries_names[11])
levels_merge_EU <- c("$ bold('All')", "$ bold('European Union')", countries_names[!countries_names %in% countries_EU])      
                  
languages_country <- list(FR = "FR", DE = "DE", IT = "IT", PL = "PL", ES = "ES-ES", GB = "EN-GB", CH = c("EN-CH", "DE-CH", "FR-CH", "IT-CH"), JP = "JA", RU = "RU", SA = c("AR", "EN-SA"), US = c("EN", "ES-US")) 
# list(FR = c("EN-FR", "FR"), DE = c("EN-DE", "DE"), IT = c("EN-IT", "IT"), PL = c("EN-PL", "PL"), ES = c("EN-ES", "ES-ES"), GB = "EN-GB", CH = c("EN-CH", "DE-CH", "FR-CH", "IT-CH"), JP = c("EN-JA", "JA"), RU = c("EN-RU", "RU"), SA = c("AR", "EN-SA"), US = c("EN", "ES-US"))
languages <- unname(unlist(languages_country)) # c("FR", "DE", "IT", "PL", "ES-ES", "EN-GB", "CH", "JA", "RU", "AR", "EN", "FR-CH", "DE-CH", "IT-CH", "ES-US")
policies_conjoint <- fromJSON("../conjoint_analysis/policies.json")
conjoint_attributes <- c("econ_issues", "society_issues", "climate_pol", "tax_system", "foreign_policy")
conjoint.attributes <- c("Economic issues", "Societal issues", "Climate policy", "Tax system", "Foreign policy")
policies_domains <- c()
for (l in languages[!languages %in% c("RU", "EN-SA", "AR")]) policies_domains <- c(policies_domains, setNames(conjoint_attributes, names(policies_conjoint[[l]])))
policies_code <- c()
for (l in languages[!languages %in% c("RU", "EN-SA", "AR")]) {
  policies_l <- unlist(setNames(policies_conjoint[[l]], conjoint_attributes))
  policies_code <- c(policies_code, setNames(names(policies_l), policies_l))
}
policies_code <- c(policies_code[!names(policies_code) %in% "-"], "-" = "-")
# policies_main_language <- policies_english <- c()
# for (l in countries) policies_main_language <- c(policies_main_language, setNames(policies_conjoint[[l]], ))
thousandile_world_disposable_inc <- as.numeric(read.csv2("../data_ext/world_disposable_inc.csv", header = FALSE)[2:1001, 2]) # created in questionnaire.R
current <- c(0, thousandile_world_disposable_inc)
mean_custom_redistr <- list()

{
  levels_quotas <- list(
    "gender" = c("Woman", "Other", "Man"), # we could add: urbanity, education, wealth, occupation, employment_agg, marital_status, Nb_children, HH_size, home (ownership)
    "income_quartile" = c("Q1", "Q2", "Q3", "Q4"), # 1:4, 
    "age" = c("18-24", "25-34", "35-49", "50-64", "65+"),
    "urbanity" = c("Cities", "Towns and suburbs", "Rural"),
    "education_quota" = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64"), # "Not 25-64"
    "employment_18_64" = c("Inactive", "Unemployed", "Employed", "65+"),
    "vote" = c("Left", "Center-right or Right", 'Far right', "Non-voter, PNR or Other"),
    "region" = 1:5, # It's OK if some values are missing in one population. (2 regions: IT, PL; 3 regions: DE, CH; 4 regions: RU, SA, US)
    "Eu_country" = c("FR", "DE", "ES", "IT", "PL", "GB", "CH"),
    "EU_country" = c("FR", "DE", "ES", "IT", "PL"),
    "gender_nationality" = c("Woman, Saudi", "Woman, non-Saudi", "Man, Saudi", "Man, non-Saudi"),
    # "US_region" = c("Northeast", "Midwest", "South", "West"),
    "US_race" = c("White only", "Hispanic", "Black", "Other"),
    "US_vote_US" = c("Harris", "Trump", "Other/Non-voter", "PNR/no right"), # TODO? vote_autres
    "US_urban" = c(TRUE, FALSE)
  )
  
  # TODO? automatic _vote in quotas, nb_regions automatic
  quotas <- list("default" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "region"),
                 # "EU" = c("gender", "income_quartile", "age", "education_quota", "country", "urbanity"),
                 # "EU_vote" = c("gender", "income_quartile", "age", "education_quota", "country", "urbanity", "vote"),
                 # "EU_all" = c("gender", "income_quartile", "age", "education_quota", "country", "urbanity", "employment_18_64", "vote"), 
                 # "US_vote" = c("gender", "income_quartile", "age", "education_quota", "race", "region", "urban", "vote_US"),
                 "US_all" = c("gender", "income_quartile", "age", "education_quota", "race", "region", "urban", "employment_18_64", "vote"),
                 "EU" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "country"), # TODO
                 "Eu" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "country"),
                 # "FR" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "region"), #, "urban_category") From oecd_climate: Pb sur cette variable car il y a des codes postaux à cheval sur plusieurs types d'aires urbaines. Ça doit fausser le type d'aire urbaine sur un peu moins de 10% des répondants. Plus souvent que l'inverse, ça les alloue au rural alors qu'ils sont urbains.
                 # # Au final ça rajoute plus du bruit qu'autre chose, et ça gène pas tant que ça la représentativité de l'échantillon (surtout par rapport à d'autres variables type age ou diplôme). Mais ça justifie de pas repondérer par rapport à cette variable je pense. cf. FR_communes.R pour les détails.
                 "SA" = c("gender_nationality", "income_quartile", "age", "education_quota", "region"),
                 "US" = c("gender", "income_quartile", "age", "education_quota", "urbanity", "region", "race")
  )
  for (q in names(quotas)) quotas[[paste0(q, "_vote")]] <- c(quotas[[q]], "vote")
  # for (c in countries_EU) quotas[[paste0(c, "_all")]] <- c(quotas[[c]], "employment_18_64", "vote")
  
  qs <- read.xlsx("../questionnaire/sources.xlsx", sheet = "Quotas", rowNames = T, rows = c(1, 2:14), cols = 1:57)
  adult_pop <- setNames(qs[countries, "Adult.pop"], countries)
  
  pop_freq <- list(
    "Eu" = list( 
      "Eu_country" = unlist(qs["Eu", c("FR", "DE", "ES", "IT", "PL", "GB", "CH")]/1000) 
    ),
    "EU" = list( 
      "EU_country" = unlist(qs["Eu", c("FR", "DE", "ES", "IT", "PL")]/sum(qs["Eu", c("FR", "DE", "ES", "IT", "PL")])) 
    ),
    "US" = list(
      # "urbanity" = c(qs["US", "Cities"], 0.001, qs["US","Rural"])/1000,
      "US_urban" = c(qs["US", "Cities"], qs["US","Rural"])/1000,
      # "US_region" = unlist(qs["US", c("Region.1", "Region.2", "Region.3", "Region.4")]/1000),
      "US_race" = unlist(qs["US", c("White.non.Hispanic", "Hispanic", "Black", "Other")]/1000)
      # "US_vote_US" = c(0.308637, 0.31822, 0.373142, 0.000001) # https://en.wikipedia.org/wiki/2024_United_States_presidential_election
    ))
  for (c in c("EU", countries)) {
    pop_freq[[c]]$gender <- c("Woman" = qs[c,"women"], 0.001, "Man" = qs[c,"men"])/1000
    pop_freq[[c]]$income_quartile <- rep(.25, 4)
    pop_freq[[c]]$age <- unlist(qs[c, c("18-24", "25-34", "35-49", "50-64", ">65")]/1000)
    pop_freq[[c]]$education_quota <- unlist(c(qs[c, c("Below.upper.secondary.25-64.0-2", "Upper.secondary.25-64.3", "Above.Upper.secondary.25-64.4-8")]/1000, "Not 25-64" = sum(unlist(qs[c, c("18-24", ">65")]/1000)))) # It's called 3 and 4-8 though in reality it's 3-4 and 5-8.
    pop_freq[[c]]$urbanity <- unlist(qs[c, c("Cities", "Towns.and.suburbs", "Rural")]/1000)
    pop_freq[[c]]$region <- unlist(qs[c, paste0("Region.", 1:5)]/1000)
    pop_freq[[c]]$employment_18_64 <- unlist(c(c("Inactive" = qs[c, "Inactivity"], "Unemployed" = qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000, "Employed" =  1000-qs[c, "Inactivity"]-qs[c, "Unemployment"]*(1000-qs[c, "Inactivity"])/1000)*(1000-qs[c, c(">65")])/1000, "65+" = qs[c, c(">65")])/1000)
    pop_freq[[c]]$gender_nationality <- unlist(setNames(qs[c, c("White.non.Hispanic", "Hispanic", "Black", "Other")],  c("WoSaudi", "WoNonSaudi", "ManSaudi", "ManNonSaudi"))/1000)
    # pop_freq[[c]]$vote <- unlist(c(c("Left" = qs[c, "Left"], "Center-right or Right" = qs[c, "Center-right.or.Right"], "Far right" = qs[c, "Far.right"])*(1000-qs[c, "Abstention"])/sum(qs[c, c("Left", "Center-right.or.Right", "Far.right")], na.rm = T), "Abstention" = qs[c, "Abstention"])/1000) # We exclude Other in this variant
    pop_freq[[c]]$vote <- unlist(c("Left" = qs[c, "Left"], "Center-right or Right" = qs[c, "Center-right.or.Right"], "Far right" = qs[c, "Far.right"], "Non-voter, PNR or Other" = sum(qs[c, "Abstention"], qs[c, "Vote_other"]))/1000) # We exclude Other in this variant
  }
}

votes_xlsx <- read.xlsx("../questionnaire/sources.xlsx", sheet = "elections", cols = 1:6)
votes <- list()
for (c in unique(votes_xlsx$country)) votes[[c]] <- votes_xlsx[votes_xlsx$country == c, ]  
for (c in unique(votes_xlsx$country)) row.names(votes[[c]]) <- votes[[c]]$party


##### Functions #####
remove_id <- function(file, folder = "../data_raw/") {
  filename <- paste(folder, file, ".csv", sep = "")
  
  filename_copy <- paste("./deprecated/", file, sample.int(10^5, 1), ".csv", sep = "") # in case the three last lines don't work
  file.copy(filename, filename_copy)
  data <- read_csv(filename_copy)
  data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic", "interview")))]
  write_csv(data, filename, na = "")
  file.remove(filename_copy)
} 


##### Load data #####
# survey_list <- all_surveys()
# pilots <- paste0(c("PL", "GB", "US"), "p")
# pilot_names <- setNames(paste0(c("PL", "GB", "US"), "_pilot"), pilots)
# # cut=0 at March 4 2025, 00:05 Paris time (after 46 PL; 118 GB; 46 US)
# for (p in pilots) { # pilots
#   print(p)
#   data <- fetch_survey(survey_list$id[survey_list$name == pilot_names[p]], include_display_order = T, verbose = T, convert = F)
#   data <- data[,which(!(names(data) %in% c("PSID", "ResponseId", "PID", "tic", "IPAddress", "m")))]
#   for (v in names(data)) label(data[[v]]) <- c(v = paste0(v, ": ", label(data[[v]])))
#   write_csv(data, paste0("../data_raw/", p, ".csv"), na = "")
#   eval(str2expression(paste0(p, " <- read_csv('../data_raw/", p, ".csv')")))
#   saveRDS(label(data), paste0("../data_raw/labels/", p, ".rds"))
#   labels <- readRDS(paste0("../data_raw/labels/", p, ".rds"))
#   for (v in names(d(p))) eval(str2expression(paste0("label(", p, "[[v]]) <- labels[[v]]")))
# }
# USp <- fetch_survey(survey_list$id[survey_list$name == "US_pilot"], include_display_order = T, verbose = T, convert = F) # labels using sjlabelled package
# # write.csv(d("GBp"), paste0("../data_raw/GBp.csv"), quote = F, na = "", row.names = F)
# # Slightly different from manual export .csv: no second row with question text; timezone is different (in e.g. startDate); True => TRUE; income bug; some additional "" are removed
# View(GBp)
# 
# for (v in names(GBp)[1:80]) { print(decrit(v, GBp)); print("____________");}
# for (v in names(GBp)[81:160]) { print(decrit(v, GBp)); print("____________");}
# for (v in names(GBp)[161:240]) { print(decrit(v, GBp)); print("____________");}
# for (v in names(GBp)) if (grepl("Q_TotalDuration", v)) { print(decrit(as.numeric(GBp[[v]]))); print("____________");}
# 
# table(GBp$ncqg)
# table(GBp$group_defended)
# PLp$comment_field
# decrit(GBp$Q_TerminateFlag)

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
  if (all) cat(paste0(sum((e$Q_TerminateFlag %in% "Screened" & (e$attention_test %in% "A little") & e$Q_TotalDuration >= 360) | # /!\ for pilot: replace 360 by 420
                            (is.na(e$Q_TerminateFlag) & ((!e$attention_test %in% "A little") | e$Q_TotalDuration < 360))), " unexplained screened or unexplained non-screened\n"))
  # if (all) cat(paste0(sum((e$Q_TerminateFlag %in% "Screened" & (e$attention_test %in% "A little") & e$Q_TotalDuration >= 420)), " unexplained screened\n"))
  # if (all) cat(paste0(sum((is.na(e$Q_TerminateFlag) & ((!e$attention_test %in% "A little") | e$Q_TotalDuration < 360))), " unexplained non-screened\n"))
  if (all) cat(paste0(sum(is.na(e$Q_TerminateFlag)), " legit: not quota met nor screened out\n"))
  cat(paste0(round(100*sum(!e$Finished %in% c(TRUE, "TRUE", 1))/sum(is.na(e$Q_TerminateFlag)), 1), "% dropout among legit\n")) # 13-23% dropout
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
# stats_exclude("USp")
# e <- read_csv(paste0("../data_raw/USp.csv"))
# write.csv(e$zipcode[e$urbanity %in% 0], "../data_ext/unrecognized_zipcodes_US.csv", quote = F, row.names = F)

weighting <- function(e, country = e$country[1], printWeights = T, variant = NULL, min_weight_for_missing_level = F, trim = T) {
  if (!missing(variant)) print(variant)
  country_variant <- paste0(c(country, variant), collapse = "_") 
  if (!country_variant %in% names(quotas)) {
    warning("No country-variant quotas found, using default variables.")
    country_variant <- ifelse(is.null(variant), "default", paste0("default_", variant)) }
  vars <- quotas[[country_variant]]
  freqs <- list()
  for (v in vars) {
    if (!(v %in% names(e))) warning(paste(v, "not in data"))
    e[[v]] <- as.character(e[[v]], include.missings = T)
    e[[v]][is.na(e[[v]])] <- "NA"
    var <- ifelse(v %in% names(levels_quotas), v, paste(country, v, sep="_"))
    if (!(var %in% names(levels_quotas))) warning(paste(var, "not in levels_quotas"))
    levels_v <- as.character(levels_quotas[[var]])
    levels_v <- levels_v[levels_v != 0]
    missing_levels <- setdiff(levels(as.factor(e[[v]])), levels_v) 
    present_levels <- which(levels_v %in% levels(as.factor(e[[v]]))) 
    if (length(present_levels) != length(levels_v)) warning(paste0("Following levels are missing from data: ", var, ": ", 
        paste(levels_v[!1:length(levels_v) %in% present_levels], collapse = ', '), " (for ", country, "). Weights are still computed, neglecting this category."))
    prop_v <- pop_freq[[country]][[var]][present_levels]
    if (min_weight_for_missing_level) freq_missing <- rep(0.000001, length(missing_levels)) # imputes 0 weight for levels present in data but not in the weight's definition
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
    e <- fetch_survey(survey_list$id[survey_list$name == paste0(country, if (pilot) "_pilot" else "_survey")], include_display_order = T, verbose = T, convert = F, col_types = cols("m" = col_character()))
    if (!remove_id) e$ExternalReference <- e$m
    if (!remove_id) e$DistributionChannel <- e$IPAddress
    if (remove_id) e$interview <- grepl("@", e$interview)
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
    e$stayed <- e$finished %in% c(TRUE, "TRUE", 1, "1") & !e$excluded %in% "QuotaMet" # includes failed attention_test
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
    e$weight_country <- e$weight <- weighting(e, country)
    label(e$weight_country) <- "weight_country: [0.25; 4] Weight to adjust to country demographics. Sums to nrow([country]). Quota variables used: quotas$[country], with frequencies pop_freq$[country]."
    label(e$weight) <- "weight: Weight for the international sample: weight_country is rescaled so that each country is weighted according to its adult population. Sums to nrow(all). (Created outside 'prepare')"
    # e$weight_all <- weighting(e, country, variant = "all") # TODO
    if ("vote" %in% names(e)) e$weight_vote <- weighting(e, country, variant = "vote") # ("vote_us" %in% names(e) & (sum(e$vote_us=="PNR/no right")!=0)) | ("vote" %in% names(e))
    # if (country == "EU") { for (c in countries_EU) e$weight_country[e$country == c] <- weighting(e[e$country == c,], c) } else e$weight_country <- e$weight
    if (any(e$custom_redistr_asked) & !pilot) e <- compute_custom_redistr(e, name = paste0(country, if (pilot) "p")) # TODO make it work for pilot
  } else {
    # e$weight <- e$weight_country <- 1
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
    if (remove_id) e$id <- NA
  }
  
  return(e)
}

define_var_lists <- function() {
  text_support <<- c("Strongly oppose","Somewhat oppose","Indifferent","Somewhat support","Strongly support")
  text_pnr <<- c("Prefer not to say")
  variables_solidarity_support <<- c("solidarity_support_billionaire_tax", "solidarity_support_corporate_tax", "solidarity_support_expanding_security_council", "solidarity_support_foreign_aid", 
    "solidarity_support_debt_relief", "solidarity_support_bridgetown", "solidarity_support_loss_damage", "solidarity_support_ncqg_300bn", "solidarity_support_shipping_levy", "solidarity_support_aviation_levy")
  variables_solidarity_support_control <<- paste0(variables_solidarity_support, "_control")
  variables_solidarity_support_short <<- paste0(c("solidarity_support_billionaire_tax", "solidarity_support_corporate_tax", "solidarity_support_expanding_security_council", "solidarity_support_foreign_aid", 
                                     "solidarity_support_bridgetown"), "_short")
  # variables_support <<- names(e)[grepl('support', names(e))]
  variables_wealth_tax_support <<- c("global_tax_support", "hic_tax_support", "intl_tax_support")
  variables_top_tax_support <<- c("top1_tax_support", "top3_tax_support")
  variables_likert <<- c(variables_solidarity_support, variables_top_tax_support, paste0(variables_top_tax_support, "_cut"), "reparations_support", variables_solidarity_support_short)
  variables_yes_no <<- c("ncs_support", "gcs_support", "ics_support", wealth_tax_support, "couple")
  variables_race <<- c("race", "race_white", "race_black", "race_hispanic", "race_asian", "race_native", "race_hawaii", "race_other", "race_pnr")
  variables_home <<- c("home_tenant", "home_owner", "home_landlord", "home_hosted")
  variables_global_movement <<- c("global_movement_no", "global_movement_spread", "global_movement_demonstrate", "global_movement_strike", "global_movement_donate")
  variables_why_hic_help_lic <<- c("why_hic_help_lic_responsibility", "why_hic_help_lic_interest", "why_hic_help_lic_duty", "why_hic_help_lic_none")
  variables_custom_redistr <<- c("custom_redistr_satisfied", "custom_redistr_skip")
  variables_custom_redistr_param <<- c("custom_redistr_winners", "custom_redistr_losers", "custom_redistr_degree")
  variables_custom_redistr_all <<- c(variables_custom_redistr_param, "custom_redistr_income_min", "custom_redistr_transfer", variables_custom_redistr)
  variables_variant <<- c("variant_split", "variant_warm_glow", "variant_realism", "variant_ncqg_maritime", "variant_radical_redistr", "variant_ics", "variant_sliders", "variant_radical_transfer", 
                          "variant_synthetic", "variant_comprehension", "variant_belief", "variant_field", "variant_sliders")
  # variables_variant_binary <<- c("variant_split", "variant_realism", "variant_ncqg_maritime", "variant_radical_redistr", "variant_sliders", "variant_radical_transfer", 
  #                                "variant_synthetic", "variant_comprehension", "variant_belief")
  variables_binary <<- c(variables_race[-1], variables_home, variables_global_movement, variables_why_hic_help_lic, variables_custom_redistr)
  variables_duration <<- c("duration", "duration_consent", "duration_socios_demos", "duration_field", "duration_conjoint", "duration_global_tax", "duration_warm_glow_substitute", "duration_gcs", 
                           "duration_ics", "duration_warm_glow_realism", "duration_ncqg_maritime", "duration_wealth_tax", "duration_preferred_transfer_mean", "duration_radical_redistr", 
                           "duration_custom_redistr", "duration_well_being", "duration_scenarios_tax", "duration_end", "duration_extra", "duration_main_questions", "duration_feedback")
  variables_split_few <<- c("revenue_split_few_domestic_education_healthcare", "revenue_split_few_domestic_welfare", "revenue_split_few_domestic_tax_reduction", 
                            "revenue_split_few_domestic_deficit_reduction", "revenue_split_few_global")
  variables_split_many_domestic <<- c("revenue_split_many_domestic_education", "revenue_split_many_domestic_healthcare", "revenue_split_many_domestic_defense", "revenue_split_many_domestic_deficit_reduction", 
                             "revenue_split_many_domestic_justice_police", "revenue_split_many_domestic_pensions", "revenue_split_many_domestic_welfare", "revenue_split_many_domestic_infrastructure", 
                             "revenue_split_many_domestic_tax_reduction")
  variables_split_many_global <<- c("revenue_split_many_global_education_healthcare", "revenue_split_many_global_renewables_adaptation", 
  "revenue_split_many_global_loss_damage", "revenue_split_many_global_forestation")
  variables_split_many <<- c(variables_split_many_domestic, variables_split_many_global)
  variables_split_few_agg <<- paste0(variables_split_few, "_agg")
  variables_split_many_domestic_agg <<- paste0(variables_split_many_domestic, "_agg")
  variables_split_many_global_agg <<- paste0(variables_split_many_global, "_agg")
  variables_split_many_agg <<- paste0(variables_split_many, "_agg")
  variables_maritime_split <<- rev(c("maritime_split_ldc", "maritime_split_companies", "maritime_split_decarbonization"))
  variables_split <<- c(variables_split_few, variables_split_many, variables_maritime_split)
  variables_split_agg <<- c(variables_split_few_agg, variables_split_many_agg)
  variables_numeric <<- c(variables_duration, "hh_size", "Nb_children__14", "donation", "gcs_belief_us", "gcs_belief_own", variables_split)
  variables_gcs_belief <<- c("gcs_belief_us", "gcs_belief_own")
  variables_ics <<- c("ics_high_support", "ics_high_color_support", "ics_mid_support", "ics_low_support")
  variables_gcs_all <<- c("gcs_support_control", variables_gcs_belief)
  variables_gcs_ics <<- c("gcs_support_control", variables_ics)
  variables_gcs_ics_all <<- c("gcs_support_control", variables_gcs_belief, variables_ics)
  variables_ncs_gcs_ics <<- c("ncs_support", "gcs_support_control", variables_ics)
  variables_ncs_gcs_ics_all <<- c("ncs_support", "gcs_support_control", variables_gcs_belief, variables_ics)
  variables_well_being <<- c("well_being_gallup_0", "well_being_wvs_0", "well_being_gallup_1", "well_being_wvs_1")
  variables_transfer_how <<- c("transfer_how_agencies", "transfer_how_govt_conditional", "transfer_how_govt_unconditional", "transfer_how_local_authorities", 
                              "transfer_how_ngo", "transfer_how_social_protection", "transfer_how_cash_unconditional")
  variables_sustainable_future <<- c("sustainable_future_a", "sustainable_future_s", "sustainable_future_b")
  variables_radical_redistr <<- c(variables_top_tax_support, "sustainable_future", "convergence_support", "global_movement_spread", "vote_intl_coalition", "reparations_support", "my_tax_global_nation")
  variables_group_defended_5 <<- c("universalist", "antispecist", "humanist", "nationalist", "individualist")
  variables_group_defended_4 <<- c("antispecist", "humanist", "nationalist", "individualist")
  variables_group_defended_3 <<- c("universalist", "nationalist", "individualist")
  variables_field <<- paste0(c("wish", "issue", "concerns", "injustice"), "_field")
  variables_field_all <<- c(variables_field, "comment_field")
  variables_conjoint_domains <<- c("F-1-1", "F-1-2", "F-1-3", "F-1-4", "F-1-5") # , "F-1-6"
  variables_conjoint_policies <<- c("F-1-1-1", "F-1-1-2", "F-1-1-3", "F-1-1-4", "F-1-1-5", "F-1-2-1", "F-1-2-2", "F-1-2-3", "F-1-2-4", "F-1-2-5") #, "F-1-1-6", "F-1-2-6"
  variables_conjoint_all <<- c(variables_conjoint_domains, variables_conjoint_policies)
  variables_sociodemos_all <<- c("gender", "age_exact", "foreign", "foreign_born_family", "foreign_born", "foreign_origin", "couple", "hh_size", "Nb_children__14", "race", "income", "income_quartile", "income_exact", "education_original", "education", "education_quota", 
                                 "employment_status", "employment_agg", "working", "retired_or_not_working", "employment_18_64", "urbanity", "region", "owner", "home", "millionaire", "nationality_SA", "voted", "vote")
  variables_quotas_base <<- c("man", "age_factor", "income_quartile", "education", "urbanity", "region") 
  variables_socio_demos <<- c(variables_quotas_base, "millionaire_agg", "couple", "employment_agg", "vote_factor") # add "hh_size", "owner", "wealth_factor", "donation_charities"?
  variables_politics <<- c("voted", "vote", "vote_agg", "group_defended")
  variables_vote <<- c("voted", "voted_original", "vote_original", "vote", "vote_agg", "vote_agg_factor", "vote_factor", "vote_voters", "vote_group", "vote_major", "vote_major_voters", "vote_major_candidate", "vote_leaning")
  covariates <<- c("country_name", "man", "age_factor", "income_quartile", "millionaire_agg", "education", "urban", "couple", "employment_agg", "voted", "vote_agg") # "race_white", "region"
}
define_var_lists()
# for (v in names(e)) if (length(unique(e[[v]])) == 2) print(v)
# for (v in names(e)) if ("No" %in% unique(e[[v]])) print(v)
# for (v in names(e)) if (is.logical(e[[v]])) print(v)
# names(e)[grepl('race', names(e))]
# cat(names(e)[grepl('sustainable', names(e)) & !grepl('order', names(e))], sep = '", "')

create_item <- function(var, new_var = var, labels, df, grep = FALSE, keep_original = FALSE, missing.values = NA, values = names(labels), annotation = NULL) {
  # Creates a memisc item s.t. var %in% values[[i]] will yield value/label labels[i]/names(labels)[i]
  # var: a character or vector of characters
  # labels: a named numeric vector
  # missing.values: a numeric or character vector
  # values: a vector of characters or a list of such vectors
  if (length(var) > 1) { 
    for (v in 1:length(var)) df <- create_item(var[v], new_var = new_var[v], df = df, labels = labels, grep = grep, missing.values = missing.values, values = values, annotation = NULL)
  } else { if (var %in% names(df)) {
    # print(var)
    if (keep_original) df[[paste0(var, "_original")]] <- df[[var]]
    temp <- NA
    for (i in seq(labels)) temp[if (grep) grepl(values[[i]], df[[var]]) else df[[var]] %in% values[[i]]] <- labels[i] 
    temp[is.na(df[[var]])] <- NA
    df[[new_var]] <- as.item(temp, labels = labels, grep = grep, missing.values = missing.values, annotation = if (is.null(annotation)) Label(df[[var]]) else annotation) 
    # Turns it into a factor if it is not numeric
    if (is.character(labels)) df[[new_var]] <- as.factor(df[[new_var]])
    if (is.character(labels) & !is.null(annotation)) label(df[[new_var]]) <- annotation
  }  }
  return(df)
}

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
    winners <- 10*df$custom_redistr_winners[k]
    non_losers <- 1000 - 10*df$custom_redistr_losers[k]
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

convert <- function(e, country = e$country[1], pilot = FALSE, weighting = TRUE) {
  # time_scenarios_tax? dunno whether there is TODO here
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

  define_var_lists()
  e$country_name <- countries_names[country]
  for (i in intersect(variables_numeric, names(e))) {
    lab <- label(e[[i]])
    e[[i]] <- as.numeric(as.vector(gsub("[^0-9\\.]", "", e[[i]]))) # /!\ this may create an issue with UK zipcodes as it removes letters
    label(e[[i]]) <- lab
  }
  for (v in intersect(variables_duration, names(e))) e[[v]] <- e[[v]]/60
  label(e$duration) <- "duration: Duration (in min)"
  e$duration_feedback <- e$duration - rowSums(e[, intersect(variables_duration[-1], names(e))], na.rm = T)
  
  for (j in intersect(variables_binary, names(e))) { 
    temp <- label(e[[j]])
    e[[j]] <- !is.na(e[[j]])
    # e[[j]] <- e[[j]] %in% "" # e[[j]][e[[j]]!=""] <- TRUE
    # e[[j]][is.na(e[[j]])] <- FALSE
    label(e[[j]]) <- temp
  }
  
  if ("attention_test" %in% names(e)) e$attentive <- e$attention_test %in% "A little"
  
  # Socio-demos
  e$man <- e$gender %in% "Man"
  
  if ("race_black" %in% names(e)) {
    e$race <- "Other"
    e$race[e$race_white==T & e$race_asian == FALSE & e$race_native == FALSE] <- "White only"
    e$race[e$race_hispanic==T] <- "Hispanic"
    e$race[e$race_black==T] <- "Black"
    if (any(e$race == "White only")) e$race <- relevel(as.factor(e$race), "White only")
    label(e$race) <- "race: White only/Hispanic/Black/Other. True proportions: .601/.185/.134/.08"
  }
  
  e <- create_item("age_exact", new_var = "age", labels = c("18-24" = 21.5, "25-34" = 30, "35-49" = 42.5, "50-64" = 57.5, "65+" = 71), 
                   values = list(c("18 to 20", "21 to 24"), c("25 to 29", "30 to 34"), c("35 to 39", "40 to 44", "45 to 49"), c("50 to 54", "55 to 59", "60 to 64"), 
                                 c("65 to 69", "70 to 74", "75 to 79", "80 to 84", "85 to 89", "90 to 99", "100 or above")), df = e)
  e$age_factor <- relevel(as.factor(e$age), "35-49")
  e <- create_item("education", labels = c("Below upper secondary" = 1, "Upper secondary" = 2, "Above upper secondary" = 3), grep = T, keep_original = T, values = c("1|2", "3", "4|5|6|7"), df = e)
  e$post_secondary <- e$education %in% 2
  e$education_quota <- ifelse(e$age > 25 & e$age < 65, e$education, 0)
  if (country == "JP") e$education_quota[e$education_quota %in% 1] <- 2 # In JP official stats, there is no one Below upper secondary. They'd have weights of 0 if we didn't relabeled them.
  # e$diploma_25_64 <- e$diploma
  # e$diploma_25_64[e$age < 25 | e$age > 65] <- 0 # "Not 25-64"
  # e$diploma_25_64 <- as.item(as.numeric(as.vector(e$diploma_25_64)), labels = structure(c(1:3, 0), names = c("Below upper secondary", "Upper secondary", "Post secondary", "Not 25-64")), missing.values=c(NA, 0), 
  #                            annotation="diploma_25_64: 0: Not 25-64 if age is not within 25-64 (missing value) / 1: Below upper secondary (ISCED 0-2) / 2: Upper secondary (ISCED 3) / 3: Post secondary (ISCED 4-8), recoded from education.")
  e <- create_item("education_quota", labels = c("Not 25-64" = 0, "Below upper secondary" = 1, "Upper secondary" = 2, "Post secondary" = 3), values = 0:3, missing.values = c(NA, 0), df = e)
  # e <- create_item("education_quota", labels = c("Below upper secondary" = 1, "Upper secondary" = 2, "Post secondary" = 3), values = c(1, 2, 3), df = e)
  
  e <- create_item("income", new_var = "income_quartile", labels = c("Q1" = 1, "Q2" = 2, "Q3" = 3, "Q4" = 4, "PNR" = 0), values = c("100|200|250", "300|400|500", "600|700|750", "800|900", "not"), grep = T, missing.values = c("PNR"), df = e)  
  e$urban <- e$urbanity == 1
  e <- create_item("urbanity", labels = c("Cities" = 1, "Towns and suburbs" = 2, "Rural" = 3), grep = T, values = c("1", "2", "3|4"), keep_original = T, missing.values = 0, df = e)
  if (country == "US") e$urbanity[e$urbanity %in% c(2, 4)] <- 3 
  # e$urban_rural <- e$urbanity
  # e <- create_item("urban_rural", labels = c("Cities" = 1, "Rural" = 2), values = list(1, c(2:4)), df = e)
  if ("foreign" %in% names(e)) {
    e <- create_item("foreign", new_var = "foreign_born_family", labels = c("No" = 0, "One parent" = 1, "Two parents" = 2, "Self" = 3), grep = T, values = c("too", "one of", "both", "Yes"), df = e)
    e$foreign_born <- e$foreign_born_family %in% 3
    e$foreign_origin <- e$foreign_born_family > 0 
  }
  
  e <- create_item("employment_status", new_var = "employment_agg", labels = c("Not working", "Student", "Working", "Retired"), grep = T, values = c("Inactive|Unemployed", "Student", "Retired", "employed$"), df = e, 
                   annotation = "employment_agg: Not working (Inactive or Unemployed) / Student / Retired / Employed (full-time, part-time, or self-employed). Built from employment_status.")
  e$retired_or_not_working <- e$employment_agg %in% c("Retired", "Not working")
  e$working <- e$employment_agg == "Working"
  
  e$employment_18_64 <- "Employed"
  e$employment_18_64[grepl("Unemployed", e$employment_status)] <- "Unemployed"
  e$employment_18_64[grepl("Inactive|Student|Retired", e$employment_status)] <- "Inactive"
  e$employment_18_64[e$age > 64] <- "65+"
  label(e$employment_18_64) <- "employment_18_64: 65+ / Inactive (Inactive, Student or Retired) / Unemployed / Employed (full-time, part-time, or self-employed). Built from employment_status."
  
  e$owner <- e$home_owner == T | e$home_landlord == T
  label(e$owner) <- "owner: Owner or Landlord renting out property to: Are you a homeowner or a tenant?"
  
  e <- create_item("voted", new_var = "voted_original", c("No right" = -1, "PNR" = -0.1, "No" = 0, "Yes" = 1), grep = T, values = c("right", "Prefer not", "No", "Yes"), df = e)
  e$voted <- e$voted %in% "Yes"
  
  if (country %in% names(votes)) {
    e$vote_original <- e[[paste0("vote_", country)]]
    e$vote_agg <- ifelse(e$vote_original %in% c("Prefer not to say", "Other"), -1, votes[[country]][e$vote_original, "leaning"]) # PNR, Other as -1
    e$vote_leaning <- ifelse(e$vote_original == "Other", NA, e$vote_agg) # PNR as -1, Other as NA
    e$vote_major_candidate <- votes[[country]][sub("Prefer not to say", "Other", e$vote_original), "major"] %in% 1
    e$vote_major <- ifelse(e$vote_major_candidate, e$vote_original, "PNR or Other")
    e$vote_voters <- ifelse(e$voted, e$vote_original, "Non-voter or PNR")
    e$vote_major_voters <- ifelse(e$voted, ifelse(e$vote_major %in% "PNR or Other", "Non-voter, PNR or Other", e$vote_major), "Non-voter, PNR or Other")
    if (country %in% countries_EU) e$vote_group <- votes[[country]][sub("Prefer not to say", "Other", e$vote_original), "group"]
    e$vote <- ifelse(e$voted, e$vote_agg, -1) # Only on voters
    
    e <- create_item("vote_agg", labels = c("PNR or Other"  = -1, "Left" = 0, "Center-right or Right" = 1, "Far right" = 2), values = -1:2, missing.values = -1, df = e)
    e <- create_item("vote", labels = c("Non-voter, PNR or Other"  = -1, "Left" = 0, "Center-right or Right" = 1, "Far right" = 2), values = -1:2, missing.values = -1, df = e)
    e$vote_agg_factor <- relevel(as.factor(as.character(e$vote_agg, include.missings = T)), "PNR or Other")
    e$vote_factor <- relevel(as.factor(as.character(e$vote, include.missings = T)), "Non-voter, PNR or Other") # Left
  }

  if (country == "US") {
    # label(e$flipped_2024) <- "flipped_2024: T/F Lives in one of the 6 States that flipped from Democrat to Republican in the 2024 presidential election (AZ, GA, MI, NV, PA, WI)."
    e$swing_state <- floor(n(e$zipcode)/100) %[]% c(30, 38) | floor(n(e$zipcode)/1000) %[]% c(48, 49) | floor(n(e$zipcode)/100) %[]% c(889, 900) | floor(n(e$zipcode)/100) %[]% c(150, 196) | floor(n(e$zipcode)/1000) %[]% c(53, 54) | floor(n(e$zipcode)/1000) %[]% c(85, 86) | floor(n(e$zipcode)/1000) %[]% c(30, 31) | floor(n(e$zipcode)/100) %[]% c(398, 399) | floor(n(e$zipcode)/1000) %[]% c(27, 28) 
    label(e$swing_state) <- "swing_state_5pp: T/F Lives in one of the 8 States around the tipping-point one: with less than 5 p.p. difference in margin with the national one at the 2024 Presidential election (NH, WI, MI, PA, GA, NV, NC, AZ)." 
    e$democratic_state <- floor(n(e$zipcode)/1000) %[]% c(80, 81) | floor(n(e$zipcode)/100) %[]% c(900, 994) | floor(n(e$zipcode)/1000) %[]% c(5, 6) | floor(n(e$zipcode)/1000) %[]% c(60, 62) | floor(n(e$zipcode)/100) %[]% c(10, 29) | floor(n(e$zipcode)/1000) %[]% c(10, 14) | floor(n(e$zipcode)/100) == 5 | floor(n(e$zipcode)/100) %[]% c(197, 205) 
    label(e$democratic_state) <- "democratic_state: T/F Lives in one of the 14 States with Democratic margin >10pp in 2024 Presidential election: California + Illinois + New York + Washington + Massachusetts + Oregon + Connecticut + Delaware + Hawaii + Rhose Island + DC + Vermont + Maryland + Colorado" # Almost same states as >15pp in 2020: just CO instead of NJ
    # 8 States with less than 5pp difference in margin from national margin in 2024:  NH (030-038), WI (53-54), MI (48-49), PA (150-196), GA (30-31/398-399), NV (889-899), NC (27-28), AZ (85-86)
    # Michigan MI (zipcodes: 48-49, 2.8%D margin, 15 electoral votes), Nevada NV (889-899, 2.4%D, 6), Pennsylvania PA (150-196, 1.2%D, 19), Wisconsin WI (53-54, .6%D, 10, tipping-point state), Arizona AZ (85-86, .3%D, 11), Georgia GA (30-31/398-399, .2%D, 16), North Carolina NC (27-28, 1.4%R, 16). [If instead we use 5pp, it adds Florida FL (32-34, 3.4%R, 30)] Apart from these, Dems should secure 228 electoral votes, i.e. need 42 more to win (out of 538 electoral votes or 83 swing ones).
    # If instead we had chosen < 5 p.p., we'd also have Minnesota (Dem) and Nebraska-2 (Dem) and wouldn't have Arizona. 
    # Other notable states: FL (32-34), MN (550-567), CO (80-81), NJ (07-08), NE-2 (68007, 68010, 68022, 68028, 68046, 68059, 68064, 68069, 68102, 68104, 68105, 68106, 68107, 68108, 68110, 68111, 68112, 68114, 68116, 68117, 68118, 68122, 68123, 68124, 68127, 68128, 68130, 68131, 68132, 68133, 68134, 68135, 68136, 68137, 68138, 68142, 68144, 68147, 68152, 68154, 68157, 68164, 68178) # https://statisticalatlas.com/congressional-district/Nebraska/Congressional-District-2/Overview
    # sources: https://en.wikipedia.org/wiki/Swing_state#Swing_states_by_results, https://en.wikipedia.org/wiki/2024_United_States_presidential_election#Results_by_state, zipcodes: https://www.mapsofworld.com/usa/zipcodes/
  }

  # Other variables
  if (country == "SA") e$saudi <- e$nationality_SA == "Saudi"
  if (country == "SA") e$gender_nationality <- paste0(ifelse(e$man, "Man", "Woman"), ", ", ifelse(e$saudi, "Saudi", "non-Saudi"))
  e <- create_item("millionaire", labels = c("Very unlikely" = -3, "Unlikely" = -1, "Likely" = 1, "Very likely" = 3, "I am already a millionaire" = 5), df = e)
  e <- create_item("millionaire", new_var = "millionaire_agg", c("Unlikely" = -1, "Likely" = 0, "Already" = 1), grep = T, values = c("nlikely", "Very l|Likely", "already"), df = e)
  e <- create_item(variables_yes_no, labels = c("No" = 0, "PNR" = -0.1, "Yes" = 100), values = c("No", list(text_pnr), "Yes"), missing.values = c("", NA, "PNR"), df = e)
  e <- create_item(variables_likert, labels = c("Strongly oppose" = -2, "Somewhat oppose" = -1, "Indifferent" = 0, "Somewhat support" = 1, "Strongly support" = 2), df = e)
  e <- create_item("likely_solidarity", labels = c("Very unlikely" = -3, "Unlikely" = -1, "Likely" = 1, "Very likely" = 3), df = e)
  e <- create_item("ncqg", labels = c("Stop" = 0, "Reduce" = 1, "Maintain ($26 bn)" = 2, "Meet goal ($100 bn)" = 3, "Intermediate ($200 bn)" = 4, "Developing ($600 bn)" = 5, "NGOs ($1,000 bn)" = 6),
                   grep = T, keep_original = T, values = c("Stop", "Reduce", "\\$26", "meet|Meet", "level between", "\\$600", "\\$1,000"), df = e)
  e <- create_item("ncqg_full", labels = c("$0" = 0, "$26 bn" = 26, "$100 bn" = 100, "$300 bn" = 300, "$600 bn" = 600, "$1,000 bn" = 1000, "$5,000 bn" = 5000),
                   grep = T, keep_original = T, values = c("\\$0", "\\$26", "\\$100", "\\$300", "\\$600", "\\$1,000", "\\$5,000"), df = e)
  if (all(c("ncqg", "ncqg_full") %in% names(e))) {
    e$variant_ncqg <- ifelse(e$variant_ncqg_maritime %in% 2, "Full", "Short")
    label(e$variant_ncqg) <- "variant_ncqg: Full/Short. Full: A lot of explanations, answers in numerical grant-equivalent; Short: Shorter, answers in terms of who defends them or what they mean."
    # e$ncqg_fusion <- as.character(e$ncqg_original)
    # e$ncqg_fusion[e$variant_ncqg %in% "full"] <- as.character(e$ncqg_full_original)[e$variant_ncqg %in% "full"]
    e$ncqg_fusion <- ifelse(e$variant_ncqg %in% "full", as.character(e$ncqg_full_original), as.character(e$ncqg_original))
    e <- create_item("ncqg_fusion", labels = c("$0" = 0, "Less" = 10, "Stable" = 26, "More loans" = 30, "$100 bn" = 100, "$200 bn" = 200, "300 bn" = 300, "$600 bn" = 600, "$1,000 bn" = 1e3, "$5,000 bn" = 5e3),
                   grep = T, values = c("Stop|\\$0", "Reduce", "\\$26", "Increase loans", "\\$100", "\\$200", "\\$300", "\\$600", "\\$1,000", "\\$5,000"), df = e)
  }
  e <- create_item(variables_transfer_how, labels = c("Wrong" = -1, "Acceptable" = 0, "Right" = 1, "Best" = 2), grep = T, values = c("wrong", "acceptable", "right", "best"), df = e)
  e$variant_sustainable_future <- ifelse(!is.na(e$sustainable_future_a), "a", ifelse(!is.na(e$sustainable_future_b), "b", "s")) # variant_radical_redistr
  label(e$variant_sustainable_future) <- "variant_sustainable_future: a/b/s a: A == sustainable / b: B == sustainable / s: B == sustainable and shorter (bullet points)."
  if (pilot) e$sustainable_future <- ifelse(grepl("B", e$sustainable_future_b) | grepl("B", e$sustainable_future_s) | grepl("A", e$sustainable_future_a), T, F)
  else e$sustainable_future <- ifelse(grepl("B", e$sustainable_future_b) | grepl("A", e$sustainable_future_a), T, F)
  e <- create_item("vote_intl_coalition", labels = c("Less likely" = -1, "Equally likely" = 0, "More likely" = 1), grep = T, values = c("less likely", "not depend", "more likely"), df = e)
  e$vote_intl_coalition_less_likely <- e$vote_intl_coalition == -1
  if ("convergence_support" %in% names(e)) e$convergence_support[is.na(e$convergence_support)] <- "prefer not" # only 11, when I realized that there was not yet "force response" for this question
  if ("convergence_support" %in% names(e)) e <- create_item("convergence_support", labels = c("No" = -1, "PNR" = 0, "Yes" = 1), grep = T, values = c("No", "prefer not", "Yes"), missing.values = c(0, NA), df = e)
  e <- create_item("gcs_comprehension", labels = c("decrease" = -1, "not be affected" = 0, "increase" = 1), df = e)
  e$gcs_understood <- e$gcs_comprehension == 1
  e <- create_item("my_tax_global_nation", labels = c("Strongly disagree" = -2, "Disagree" = -1, "Neither agree nor disagree" = 0, "Agree" = 1, "Strongly agree" = 2), df = e)
  e <- create_item("group_defended", labels = c("Family and self" = -2, "Region, continent or religion" = -1, "Fellow citizens" = 0, "Humans" = 1, "Sentient beings" = 2),
                   grep = T, values = c("family", "religion", "Americans", "Humans", "Sentient"), df = e) # In NHB 0-7, Relatives 1; Culture/religion 3; Europeans 5
  e <- create_item("survey_biased", labels = c("Yes, left" = -1, "Yes, right" = 0, "No" = 1), grep = T, values = c("left", "right", "No"), df = e)

  for (v in variables_well_being) e[[paste0(v, "_original")]] <- e[[v]]
  for (v in variables_well_being) e[[v]] <- as.numeric(gsub("[^0-9]", "", e[[v]])) # TODO: label
  
  e$variant_field <- e$field <- NA
  for (v in intersect(variables_field, names(e))) e$field[!is.na(e[[v]])] <- e[[v]][!is.na(e[[v]])]
  for (v in intersect(variables_field, names(e))) e$variant_field[!is.na(e[[paste0("Open-endedfield_order_", v)]])] <- sub("_field", "", v)
  
  if ("gcs_belief_own" %in% names(e)) {
    e$variant_belief_eu <- ifelse(e$variant_belief %in% 1, "US", "Own")
    if (country == "US") e$variant_belief_eu[e$variant_belief_eu %in% "US"] <- "EU"
    e$gcs_belief <- ifelse(e$variant_belief %in% 1, e$gcs_belief_us, e$gcs_belief_own)
    e$variant_belief <- ifelse(e$variant_belief %in% 1, "US", "Own") # "In the U.S. [except in US: EU]", "In own country"
  }
  
  e$info_solidarity <- e$variant_realism %in% 1
  if (pilot) e$variant_info_solidarity <- relevel(as.factor(ifelse(e$info_solidarity, ifelse(e$variant_long, "Long info", "Short info"), "No info")), "No info")
  
  e <- create_item("variant_warm_glow", labels = c("None" = 0, "NCS" = 1, "donation" = 2), values = 0:2, df = e)
  e$variant_warm_glow <- as.factor(e$variant_warm_glow)
  e$gcs_support_control <- ifelse(e$variant_warm_glow == "None", e$gcs_support, NA)
  
  for (v in unique(e$variant_ics)) e[[paste0("ics_", v, "_support")]] <- ifelse(e$variant_ics == v, e$ics_support, NA)
  
  for (v in intersect(variables_solidarity_support_short, names(e))) e[[sub("_short", "", v, "_long")]] <- e[[sub("_short", "", v)]]
  for (v in intersect(variables_solidarity_support_short, names(e))) e[[sub("_short", "", v)]] <- ifelse(e$variant_long, e[[sub("_short", "", v)]], e[[v]])
  e$share_solidarity_short_supported <- rowMeans((e[, sub("_short", "", variables_solidarity_support_short)]) > 0)  
  e$share_solidarity_short_opposed <- rowMeans((e[, sub("_short", "", variables_solidarity_support_short)]) < 0)  
  e$share_solidarity_supported <- rowMeans((e[, variables_solidarity_support]) > 0)  
  e$share_solidarity_opposed <- rowMeans((e[, variables_solidarity_support]) < 0)  
  for (v in variables_solidarity_support) e[[paste0(v, "_control")]] <- ifelse(e$info_solidarity, NA, e[[v]])
  
  if (pilot) {
    e$top1_tax_support <- ifelse(e$cut, e$top1_tax_support_cut, e$top1_tax_support)
    e$top3_tax_support <- ifelse(e$cut, e$top3_tax_support_cut, e$top3_tax_support)
  }
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
  e <- create_item("wealth_tax_support", labels = c("No" = 0, "Yes" = 1), values = c(0, 1), missing.values = c("", NA), df = e)
  
  e$humanist <- grepl("Humans", e$group_defended)
  e$universalist <- grepl("Sentient|Humans", e$group_defended)
  e$antispecist <- grepl("Sentient", e$group_defended)
  e$nationalist <- grepl("Fellow", e$group_defended)
  e$individualist <- grepl("self", e$group_defended)
  
  e$split_nb_global <- rowSums(!is.na(e[, variables_split_many_global]))
  e$split_nb_global[e$variant_split == 1] <- NA
  e$split_many_global <- rowSums(e[, variables_split_many_global], na.rm = T)
  e$split_many_global[!e$split_nb_global %in% 1:4] <- NA
  
  e$split_both_global <- ifelse(e$variant_split == 1, e$revenue_split_few_global, e$split_many_global)
  e$split_both_global[e$split_nb_global %in% 0] <- NA
  e$split_both_nb_global <- ifelse(e$variant_split == 1, 1, e$split_nb_global)
  
  for (v in variables_split_agg) {
    e[[v]] <- pmin(e[[sub("_agg", "", v)]], 35)
    e <- create_item(v, labels = c("0" = 0, "5" = 5, "10" = 10, "15" = 15, "20" = 20, "25" = 25, "30" = 30, "35-100" = 35), values = c(0, 5, 10, 15, 20, 25, 30, 35), missing.values = c("", NA), df = e)
  }
  
  e$mean_order_many_global <- rowMeans(e[, sub("many_", "many_order_", variables_split_many_global)], na.rm = T)
  
  # Orders with full randomization: revenue_split_few, revenue_split_many, solidarity_support, why_hic_help_lic, maritime_split
  # Orders with random flip: ncqg, transfer_how, vote_intl_coalition, gcs_comprehension
  e$ncqg_order <- ifelse(!is.na(e$ncqg_order), ifelse(e$ncqg_order %in% 1, "increasing", "decreasing"), NA)
  e$transfer_how_order <- ifelse(e$transfer_how_order_agencies == 1, "global_first", "local_first")
  e$vote_intl_coalition_order <- ifelse(e$vote_intl_coalition_order_more_likely == 1, "more_first", "less_first")
  e$gcs_comprehension_order <- ifelse(e$gcs_comprehension_order %in% 1, "increase_first", "decrease_first")
  
  # NAs for questions not asked
  # TODO update for RU
  e$race_asked <- e$country %in% "US"
  e$custom_redistr_asked <- e$cut %in% 0 & country != "RU" # Asked in all non-pilot except RU
  e$radical_redistr_asked <- e$why_hic_help_lic_asked <- e$global_movement_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 1 # Asked in all non-pilot
  for (v in c("winners", "losers")) e[[paste0("custom_redistr_", v)]] <- e[[paste0("custom_redistr_", v)]]/10
  # e$global_movement_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 1
  # e$transfer_how_asked <- e$cut %in% 0 | e$variant_radical_transfer %in% 0
  for (l in c("race", "global_movement", "why_hic_help_lic", "custom_redistr")) {
    for (v in eval(str2expression(paste0("variables_", l)))) e[[v]][!e[[paste0(l, "_asked")]]] <- NA
  }
  
  e$custom_redistr_unsatisfied_unskip <- ifelse(e$custom_redistr_asked, !e$custom_redistr_satisfied & !e$custom_redistr_skip, NA)
  e$custom_redistr_both_satisfied_skip <- ifelse(e$custom_redistr_asked, e$custom_redistr_satisfied & e$custom_redistr_skip, NA) # flag bad quality
  e$variant_sliders <- ifelse(e$variant_sliders %in% 1, "concentrated", "diffuse")
  label(e$variant_sliders) <- "variant_sliders: Concentrated/Diffuse. Values of the initial position of sliders in custom_redistr. Concentrated/Diffuse: Winners: 40/60; Losers: 10/20; Degree: 7/2."
  # e$income_qantile <- # TODO
  # e$custom_redistr_winning <- e$income_qantile < e$custom_redistr_winners
  # e$custom_redistr_losing <- (100 - e$income_quantile) > e$custom_redistr_losers
  
  # unused: variant_radical_transfer, variant_comprehension, variant_synthetic
  e$variant_split <- ifelse(e$variant_split == 2, "Many", "Few") 
  # e$variant_realism <- ifelse(e$variant_realism == 1, "Info", "No info") # => info_solidarity
  e$variant_ncqg_maritime <- ifelse(e$variant_ncqg_maritime %in% 0, "maritime", e$variant_ncqg)
  
  e$n <- paste0(country, 1:nrow(e))
  if (!country %in% c("RU", "SA")) {
    e$conjoint_number <- ifelse(e$conjoint == "Candidate A", 1, ifelse(e$conjoint == "Candidate B", 2, NA))
    e$conjoint_misleading <- ifelse(is.na(e$conjoint_number), 1, e$conjoint_number)
    for (v in intersect(variables_conjoint_domains, names(e))) {
      e[[paste0(v, "_original")]] <- e[[v]]
      e[[v]] <- policies_domains[e[[v]]] # common policy name for all countries
    }
    policies_l <- unlist(setNames(policies_conjoint[[languages_country[[country]][1]]], conjoint_attributes))
    for (v in intersect(variables_conjoint_policies, names(e))) {
      e[[paste0(v, "_original")]] <- e[[v]]
      e[[v]] <- policies_code[e[[v]]]
      # e[[v]] <- policies_l[policies_code[e[[v]]]] # policy name in country's main language (replace 1 by paste0("EN-", country)) for english
    }  
  }
  # e$global_movement_any <- as.logical(rowSums(e[, variables_global_movement[2:5]]))
  # e$global_movement_any[!e$global_movement_asked] <- NA 
  # e$why_hic_help_lic_any <- as.logical(rowSums(e[, variables_why_hic_help_lic[1:3]]))
  # e$why_hic_help_lic_duty[!e$why_hic_help_lic_asked] <- NA
  
  return(e)
}

##### Load data #####
start_time <- Sys.time()
survey_data <- setNames(lapply(countries[-9], function(c) { prepare(country = c, scope = "final", 
                        fetch = T, convert = T, remove_id = T, rename = T, pilot = FALSE, weighting = T) }), countries[-9]) # remove_id = F
all <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, survey_data)
list2env(survey_data, envir = .GlobalEnv)
all$weight <- all$weight_country * (adult_pop[all$country]/sum(adult_pop[unique(all$country)])) / (sapply(all$country, function(c) { sum(all$country == c)})/(nrow(all)))

e <- all
beep()
Sys.time() - start_time # 6 min

all <- compute_custom_redistr(all, name = "all") # 4 min TODO: Replace it by it being computed as the average of countries'
beep()
Sys.time() - start_time # 10 min

# Pilots
# pilot_data <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = T, remove_id = T, convert = T, rename = T, pilot = TRUE, weighting = T) }), paste0(pilot_countries, "p")) # remove_id = F
# pilot <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data)
# list2env(pilot_data, envir = .GlobalEnv) # 35 in both pilot and all: 16 in PL, 14 in GB, 5 in US
# beep()
# 
# data_all <- setNames(lapply(countries[-9], function(c) { prepare(country = c, scope = "all", fetch = T, convert = T, rename = T, pilot = FALSE, weighting = FALSE) }), countries[-9]) # remove_id = F
# a <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, data_all)

# write.csv(all[, c("n", "country", "distr", "id", "interview")], "../Adrien's/all_id.csv", quote = F, row.names = F)
# save.image(".RData")

# Oldies

# PL <- prepare(country = "PL", scope = "final", fetch = T, convert = T, rename = T, pilot = FALSE, weighting = T, remove_id = T)

# pilot_data_id <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", remove_id = F, fetch = T, convert = T, rename = T, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")) # remove_id = F
# i <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data_id)
# list2env(pilot_data, envir = .GlobalEnv)
# write.csv(i[, c("id", "country")], "../deprecated/IDs_pilot.csv", row.names = F, quote = F)
# sum(is.na(i$id))
# sum(duplicated(i$distr))


# PLa <- prepare(country = "PL", scope = "all", fetch = T, convert = T, rename = T, pilot = TRUE, weighting = FALSE)
# pilot_data_all <- setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "all", fetch = T, convert = T, rename = T, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")) # remove_id = F
# a <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, pilot_data_all)

# sum(duplicated(p$distr))

# for (v in names(p)[1:80]) { print(decrit(v, p)); print("____________");}
# for (v in names(p)[81:160]) { print(decrit(v, p)); print("____________");}
# for (v in names(p)[161:211]) { print(decrit(v, p)); print("____________");}
# for (c in paste0(pilot_countries, "p")) print(paste(c, mean(d(c)$gcs_support %in% "Yes")))
# summary(lm(gcs_support %in% "Yes" ~ country, data = p))
# summary(lm(ics_support %in% "Yes" ~ variant_ics, data = p))
# 
# for (i in 1:length(e)) {
#   # label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
#   print(paste(i, label(e[[i]])))
#   # print(names(e)[i])
# }
# list2env(setNames(lapply(pilot_countries, function(c) { prepare(country = c, scope = "final", fetch = FALSE, convert = TRUE, pilot = TRUE, weighting = FALSE) }), paste0(pilot_countries, "p")), envir = .GlobalEnv)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, lapply(paste0(pilot_countries, "p"), function(c) d(c)))
# USp <- prepare(country = "US", scope = "final", fetch = F, convert = T, pilot = TRUE, weighting = FALSE)
# PLp <- prepare(country = "PL", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# GBp <- prepare(country = "GB", scope = "final", fetch = T, convert = T, pilot = TRUE, weighting = FALSE)
# p <- Reduce(function(df1, df2) { merge(df1, df2, all = T) }, list(USp, PLp, GBp))


##### Codebook #####
# export_codebook(p, "../questionnaire/codebook_p.csv", stata = FALSE, omit = c(1, 2, 7, 9:13, 197)) 











