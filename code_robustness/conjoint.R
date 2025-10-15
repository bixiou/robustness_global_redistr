policies.names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies", rowNames = T)) #, rows = c(1, 16:41), cols = 1:6))
policies.names <- policies.names[is.na(as.numeric(row.names(policies.names))), ] # NAs by coercion normal

formula_cjoint <- as.formula("selected ~ econ_issues + society_issues + climate_pol + tax_system + foreign_policy")

amce <- ca <- list() # We should have "Old qualtrics format detected." (otherwise it would assume new format and delete the first observation).

# Requires: amce, variables_conjoint_all, variables_conjoint_policies, policies_conjoint, conjoint_attributes, policies.names, formula_cjoint, languages_country, files ../conjoint_analysis/[lang].dat
compute_conjoint <- function(df_names = countries[!countries %in% c("SA", "RU")], export = T, include_indifferent = T, subset = "", weights = "weight", width = 1000, height = 700, max_length_string = 75, font = 16) {
  # subset should be the name of a T/F variable
  coefs <- list() # We should have "Old qualtrics format detected." (otherwise it would assume new format and delete the first observation).
  for (df in df_names) { # c([!countries %in% c("SA", "RU")], paste0(pilot_countries, "p"))
    print(df)
    data <- d(df)[if (subset != "") d(df)[[subset]] else T, c(variables_conjoint_all, 'conjoint_misleading', 'conjoint', 'country', 'weight', 'n')]
    main_language <- languages_country[[sub("p", "", df)]][1]
    csv.path <- paste0("../conjoint_analysis/ca_", df, "_", subset, ".csv")
    write.csv(data, csv.path, row.names = FALSE) # !is.na(d(df)$conjoint_number) 'conjoint_number', 
    temp <- readLines(csv.path)
    writeLines(c(temp[1], temp), csv.path)
    
    # /!\ If bugs, it's because a patched version of cjoint should be installed (cf. .Rprofile)
    
    ca <- read.qualtrics(csv.path, responses = 'conjoint_misleading', covariates = c(variables_conjoint_policies), respondentID = "n") # names(d(n))[cols_conjoint] , letter = "F"
    names(ca)[1] <- "n"
    ca <- merge(data[, intersect(names(data), c("country", "conjoint", "weight", "n"))], ca) # vote "conjoint_number", 
    design_cjoint <- makeDesign(filename = paste0("../conjoint_analysis/", main_language, ".dat")) 

    # To allow for ties, replace selected==NA by 0.5 and previously make conjoint_r_number == "" for respondents who chose "neither" (read.qualtrics removes NA from ca but keeps "" which it converts into selected==NA).
    if (nrow(ca) < 2*nrow(data)) warning(paste("/!\\ The question was not asked to", nrow(data) - nrow(ca)/2, "respondents in ", df))
    if (include_indifferent) ca$selected[ca$conjoint %in% "Neither of them"] <- .5
    else ca <- ca[!ca$conjoint %in% "Neither of them",]
    if (any(is.na(ca))) warning(paste("/!\\", sum(sapply(1:nrow(ca), function(i) any(is.na(ca[i,]))))), "rows with some NAs have been removed in ", df)
    for (lang in c(paste0("EN-", ifelse(df == "JP", "JA", df)), main_language)) if (lang %in% names(policies_conjoint)) {
      key <- paste0(lang, if (subset != "") "_", subset, if (!include_indifferent) "_wo_neither", if (is.null(weights)) "_unweighted" else if (weights != "weight") paste0("_", weights))
      coefs[[key]] <- amce(formula_cjoint, ca[sapply(1:nrow(ca), function(i) !any(is.na(ca[i,]))),], design = design_cjoint, cluster = FALSE, weights = weights)
      policies_l <- c(unlist(setNames(policies_conjoint[[lang]], conjoint_attributes)), "-" = "-")
      for (i in 1:length(coefs[[key]]$attributes)) coefs[[key]]$user.names[[i+1]] <- names(policies_conjoint[[lang]])[i]
      for (i in 1:length(coefs[[key]]$user.levels)) coefs[[key]]$user.levels[[i]] <- break_strings(policies_l[coefs[[key]]$user.levels[[i]]], max_length = max_length_string, sep = "\n")
    }
  }
  
  if (export) for (j in names(coefs)) amce[[j]] <<- coefs[[j]]
  if (export) for (key in names(coefs)) { # for (df in df_names) { for (key in paste0(c(paste0("EN-", ifelse(df == "JP", "JA", df)), languages_country[[sub("p", "", df)]][1]), "_", subset)) if (lang %in% names(policies_conjoint)) {
    pdf(paste0("../figures/all/conjoint_", key, ".pdf"), width = width/72, height = height/72) 
    plot(coefs[[key]], xlab = "Average Marginal Component Effect", text.size = font)
    invisible(dev.off())
    # save_plot(filename = paste0("conjoint_", lang), folder = paste0('../figures/', df, '/'), width = 1100, height = 700, method='dev', trim = F, format = 'pdf') 
  } 
}

compute_conjoint(subset = "consistent_conjoints")
compute_conjoint(subset = "consistent_conjoints_strict")
compute_conjoint(include_indifferent = FALSE)
compute_conjoint(weights = NULL)
compute_conjoint()


# Share of partisan policies in the country pool:
round(colMeans(policies_leaning != 1, na.rm = T), 2)
sapply(c("all", countries[-c(9,10)]), function(c) round(mean(d(c)$consistent_conjoints, na.rm = T), 3))
sapply(c("all", countries[-c(9,10)]), function(c) round(mean(d(c)$consistent_conjoints_strict, na.rm = T), 3))

summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight))
# Effects are preserved when inconsistent programs are removed (considering the two policies as consistent with any program). Cf. Cuesta et al. (22)
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints))
# The effect of cutting aid disappears when removing left-leaning programs where it is present; while wealth tax is preserved when removing right-leaning programs where it is present. => Cutting aid is harmful only for left-leaning programs; wealth tax is helpful for any program.
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_strict))

summary(reg_conjoint <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$leaning_conjoint == "Left")) # +4*** / -4***

# plot(amce$FR) 
# plot(amce$DE) 
# plot(amce$IT)
# plot(amce$PL)
# plot(amce$ES) 
# plot(amce$GB)
# plot(amce$CH) 
# plot(amce$`EN-JA`) 
# plot(amce$EN) 
# plot(amce$GBp)
# plot(amce$PLp)
# plot(amce$USp)
# plot(amce$`EN-FR`) 
# plot(amce$`EN-DE`) 
# plot(amce$`EN-IT`)
# plot(amce$`EN-PL`)
# plot(amce$`EN-ES`) 
# 
# # Before it has been functionalized:
# 
# d <- function(str, alt_data = eu, alt_var = "country") {
#   if (exists(str) && is.data.frame(eval(str2expression(str)))) return(eval(str2expression(str))) # data from name
#   else return(alt_data[alt_data[[alt_var]] == toupper(str),])
# }
#
# for (df in countries[!countries %in% c("SA", "RU")]) { # c([!countries %in% c("SA", "RU")], paste0(pilot_countries, "p"))
#   print(df)
#   main_language <- languages_country[[sub("p", "", df)]][1]
#   csv.path <- paste0("../conjoint_analysis/ca_", df, ".csv")
#   write.csv(d(df)[, c(variables_conjoint_all, 'conjoint_misleading', 'conjoint', 'n')], csv.path, row.names = FALSE) # !is.na(d(df)$conjoint_number) 'conjoint_number',
#   temp <- readLines(csv.path)
#   writeLines(c(temp[1], temp), csv.path)
# 
#   # /!\ If bugs, it's because a patched version of cjoint should be installed (cf. .Rprofile)
# 
#   ca[[df]] <- read.qualtrics(csv.path, responses = 'conjoint_misleading', covariates = c(variables_conjoint_policies), respondentID = "n") # names(d(n))[cols_conjoint] , letter = "F"
#   names(ca[[df]])[1] <- "n"
#   ca[[df]] <- merge(d(df)[, intersect(names(d(df)), c("country", "conjoint", "n"))], ca[[df]]) # vote "conjoint_number",
#   # names(policies_conjoint[["EN-GB"]])
#   # domain_names <- names(policies_conjoint[[main_language]])
#   # for (i in 1:5) {
#   #   # ca[[df]][[domain_names[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]])
#   #   # ca[[df]][[conjoint.attributes[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]])
#   #   # for (c in paste0(pilot_countries, "p")) {
#   #   #   temp <- which(ca[[df]]$country == c) # & !(ca[[df]][[conjoint_attributes[i]]] %in% c("soc3", "tax3", "-")))
#   #   #   ca[[df]][[conjoint.attributes[i]]][temp] <- as.character(policies.names[as.character(ca[[df]][[conjoint_attributes[i]]][temp]), c])
#   #   # }
#   #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "-"] <- "-"
#   #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "soc3"] <- "Making abortion a right at the federal level"
#   #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "tax3"] <- "Increase corporate income tax rate from 21% to 28%"
#   #   ca[[df]][[domain_names[i]]] <- as.factor(gsub(",", ";;", gsub(":", "##", ca[[df]][[conjoint_attributes[i]]])))
#   #   ca[[df]][[paste0(domain_names[i], ".rowpos")]] <- ca[[df]][[paste0(conjoint_attributes[i], ".rowpos")]]
#   # }
#   design_cjoint <- makeDesign(filename = paste0("../conjoint_analysis/", main_language, ".dat"))
#   # formula_cjoint <- as.formula(paste("selected ~ `", paste0(domain_names, collapse = "` + `"), "`"))
# 
#   # To allow for ties, replace selected==NA by 0.5 and previously make conjoint_r_number == "" for respondents who chose "neither" (read.qualtrics removes NA from ca[[df]] but keeps "" which it converts into selected==NA).
#   if (nrow(ca[[df]]) < 2*nrow(d(df))) warning(paste("/!\\ The question was not asked to", nrow(d(df)) - nrow(ca[[df]])/2, "respondents in ", df))
#   if (include_indifferent) ca[[df]]$selected[ca[[df]]$conjoint %in% "Neither of them"] <- .5
#   else ca[[df]] <- ca[[df]][!ca[[df]]$conjoint %in% "Neither of them",]
#   if (any(is.na(ca[[df]]))) warning(paste("/!\\", sum(sapply(1:nrow(ca[[df]]), function(i) any(is.na(ca[[df]][i,]))))), "rows with some NAs have been removed in ", df)
#   for (lang in c(paste0("EN-", ifelse(df == "JP", "JA", df)), main_language)) if (lang %in% names(policies_conjoint)) {
#     amce[[lang]] <- amce(formula_cjoint, ca[[df]][sapply(1:nrow(ca[[df]]), function(i) !any(is.na(ca[[df]][i,]))),], design = design_cjoint, cluster = FALSE, weights = NULL)
#     policies_l <- c(unlist(setNames(policies_conjoint[[lang]], conjoint_attributes)), "-" = "-")
#     for (i in 1:length(amce[[lang]]$attributes)) amce[[lang]]$user.names[[i+1]] <- names(policies_conjoint[[lang]])[i]
#     for (i in 1:length(amce[[lang]]$user.levels)) amce[[lang]]$user.levels[[i]] <- break_strings(policies_l[amce[[lang]]$user.levels[[i]]], max_length = 75, sep = "\n")
#   }
# }
# 
# for (df in countries[!countries %in% c("SA", "RU")]) { for (lang in c(paste0("EN-", ifelse(df == "JP", "JA", df)), languages_country[[sub("p", "", df)]][1])) if (lang %in% names(policies_conjoint)) {
#   pdf(paste0('../figures/', df, '/', "conjoint_", lang, ".pdf"), width = 1000/72, height = 700/72)
#   plot(amce[[lang]], xlab = "Average Marginal Component Effect", text.size = 16)
#   invisible(dev.off())
#   # save_plot(filename = paste0("conjoint_", lang), folder = paste0('../figures/', df, '/'), width = 1100, height = 700, method='dev', trim = F, format = 'pdf')
# } }


# ##### Attempt to make it work at CIRED #####
# decrit(e$conjoint)
# decrit(e$`F-1-1-1`) # foreign1: millionaire tax / foreign2: cut aid
# e$program_a <- sapply(1:nrow(e), function(n) {paste(e[n, paste0("F-1-1-", 1:5)], collapse = ' ')})
# e$program_b <- sapply(1:nrow(e), function(n) {paste(e[n, paste0("F-1-2-", 1:5)], collapse = ' ')})
# e$millionaire_tax_in_a <- grepl("foreign_policy1", e$program_a)
# e$millionaire_tax_in_b <- grepl("foreign_policy1", e$program_b)
# e$cut_aid_in_a <- grepl("foreign_policy2", e$program_a)
# e$cut_aid_in_b <- grepl("foreign_policy2", e$program_b)
# e$millionaire_tax_in_program <- e$millionaire_tax_in_a
# e$cut_aid_in_program <- e$cut_aid_in_a
# e$program <- e$program_a
# e$program_preferred <- e$conjoint == "candidate A"
# temp <- e
# temp$millionaire_tax_in_program <- temp$millionaire_tax_in_b
# temp$cut_aid_in_program <- temp$cut_aid_in_b
# temp$program <- temp$program_b
# temp$program_preferred <- temp$conjoint == "candidate B"
# ce <- cbind(e, temp)
# rm(temp)
# summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program, data = call))
# summary(lm(program_preferred ~ millionaire_tax_in_program, data = call))
# summary(lm(program_preferred ~ cut_aid_in_program, data = call))
# decrit("conjoint", all) # 27% Neither
# summary(lm(program_preferred ~ cut_aid_in_program, data = ce, subset = country == "CH"))
# summary(lm(program_preferred ~ cut_aid_in_program, data = ce, subset = country != "CH"))
