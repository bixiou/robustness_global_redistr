d <- function(str, alt_data = eu, alt_var = "country") {
  if (exists(str) && is.data.frame(eval(str2expression(str)))) return(eval(str2expression(str))) # data from name
  else return(alt_data[alt_data[[alt_var]] == toupper(str),])
}
# policies_names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies")) # , rowNames = T, rows = c(1, 16:41), cols = 1:6
# languages <- colnames(policies_names)[!grepl("_|[a-z]", colnames(policies_names))]
# languages_country <- list(FR = "FR", DE = "DE", IT = "IT", PL = "PL", ES = "ES-ES", GB = "EN-GB", 
#                           CH = c("CH", "DE-CH", "FR-CH", "IT-CH"), JP = "JA", RU = "RU", SA = "AR", US = c("EN", "ES-US"))
# conjoint_attributes <- c("econ_issues", "society_issues", "climate_pol", "tax_system", "foreign_policy")

include_indifferent <- T # TODO functionalize
policies.names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies", rowNames = T)) #, rows = c(1, 16:41), cols = 1:6))
policies.names <- policies.names[is.na(as.numeric(row.names(policies.names))), ] # NAs by coercion normal

formula_cjoint <- as.formula("selected ~ econ_issues + society_issues + climate_pol + tax_system + foreign_policy")
# formula_cjoint_specific <- as.formula("selected ~ `Economic issues` + `Societal issues` + `Climate policy` + `Tax system` + `Foreign policy`")
# /!\ If Error in if (any(as.vector(r_1) - as.vector(cross_tab_std[m,... add a (second) blank line at the end of the .dat
# design_cjoint_US <- makeDesign(filename = "../conjoint_analysis/9d_F.dat") # gives the probability that a profile appears: sum(design_cjoint$J)=1, dim(design_cjoint$J) = 5 4 4 4 5. 
# design_cjoint_EU <- makeDesign(filename = "../conjoint_analysis/9d_F_EU.dat") 
# design_cjoint_both <- makeDesign(filename = "../conjoint_analysis/9d_F_both.dat") # The weighted are the average of the US and EU weights
amce <- ca <- list() # We should have "Old qualtrics format detected." (otherwise it would assume new format and delete the first observation).
for (df in countries[!countries %in% c("SA", "RU")]) { # c([!countries %in% c("SA", "RU")], paste0(pilot_countries, "p"))
  print(df)
  main_language <- languages_country[[sub("p", "", df)]][1]
  policies_l <- c(unlist(setNames(policies_conjoint[[main_language]], conjoint_attributes)), "-" = "-")
  csv.path <- paste0("../conjoint_analysis/ca_", df, ".csv")
  write.csv(d(df)[, c(variables_conjoint_all, 'conjoint_misleading', 'conjoint', 'n')], csv.path, row.names = FALSE) # !is.na(d(df)$conjoint_number) 'conjoint_number', 
  temp <- readLines(csv.path)
  writeLines(c(temp[1], temp), csv.path)
  
  # /!\ If bugs, it's because a patched version of cjoint should be installed (cf. .Rprofile)
  
  ca[[df]] <- read.qualtrics(csv.path, responses = 'conjoint_misleading', covariates = c(variables_conjoint_policies), respondentID = "n") # names(d(n))[cols_conjoint] , letter = "F"
  names(ca[[df]])[1] <- "n"
  ca[[df]] <- merge(d(df)[, intersect(names(d(df)), c("country", "conjoint", "n"))], ca[[df]]) # vote "conjoint_number", 
  # names(policies_conjoint[["EN-GB"]])
  # domain_names <- names(policies_conjoint[[main_language]])
  # for (i in 1:5) {
  #   # ca[[df]][[domain_names[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]])
  #   # ca[[df]][[conjoint.attributes[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]])
  #   # for (c in paste0(pilot_countries, "p")) {
  #   #   temp <- which(ca[[df]]$country == c) # & !(ca[[df]][[conjoint_attributes[i]]] %in% c("soc3", "tax3", "-")))
  #   #   ca[[df]][[conjoint.attributes[i]]][temp] <- as.character(policies.names[as.character(ca[[df]][[conjoint_attributes[i]]][temp]), c])
  #   # }
  #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "-"] <- "-"
  #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "soc3"] <- "Making abortion a right at the federal level"
  #   # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "tax3"] <- "Increase corporate income tax rate from 21% to 28%"
  #   ca[[df]][[domain_names[i]]] <- as.factor(gsub(",", ";;", gsub(":", "##", ca[[df]][[conjoint_attributes[i]]])))
  #   ca[[df]][[paste0(domain_names[i], ".rowpos")]] <- ca[[df]][[paste0(conjoint_attributes[i], ".rowpos")]]
  # }
  design_cjoint <- makeDesign(filename = paste0("../conjoint_analysis/", main_language, ".dat")) 
  # formula_cjoint <- as.formula(paste("selected ~ `", paste0(domain_names, collapse = "` + `"), "`"))
  
  # To allow for ties, replace selected==NA by 0.5 and previously make conjoint_r_number == "" for respondents who chose "neither" (read.qualtrics removes NA from ca[[df]] but keeps "" which it converts into selected==NA).
  if (nrow(ca[[df]]) < 2*nrow(d(df))) warning(paste("/!\\ The question was not asked to", nrow(d(df)) - nrow(ca[[df]])/2, "respondents in ", df))
  if (include_indifferent) ca[[df]]$selected[ca[[df]]$conjoint %in% "Neither of them"] <- .5
  else ca[[df]] <- ca[[df]][!ca[[df]]$conjoint %in% "Neither of them",]
  if (any(is.na(ca[[df]]))) warning(paste("/!\\", sum(sapply(1:nrow(ca[[df]]), function(i) any(is.na(ca[[df]][i,]))))), "rows with some NAs have been removed in ", df)
  amce[[df]] <- amce(formula_cjoint, ca[[df]][sapply(1:nrow(ca[[df]]), function(i) !any(is.na(ca[[df]][i,]))),], design = design_cjoint, cluster = FALSE, weights = NULL)
  # if (lang == "main") 
  for (i in 1:length(amce[[df]]$attributes)) amce[[df]]$user.names[[i+1]] <- names(policies_conjoint[[main_language]])[i]
  for (i in 1:length(amce[[df]]$user.levels)) amce[[df]]$user.levels[[i]] <- policies_l[amce[[df]]$user.levels[[i]]]
  # else if (lang == "english") # TODO
}

for (df in countries[!countries %in% c("SA", "RU")]) {
  plot(amce[[df]], xlab = "Average Marginal Component Effect", text.size = 16)
  save_plot (filename = paste0("conjoint_", df), folder = paste0('../figures/', df, '/'), width = 1100, height = 500, method='dev', trim = T, format = 'pdf') 
}

plot(amce$FR) 
plot(amce$DE) 
plot(amce$IT)
plot(amce$PL)
plot(amce$ES) 
plot(amce$GB)
plot(amce$CH) 
plot(amce$JP) # TODO: display
plot(amce$US) 
plot(amce$GBp)
plot(amce$PLp)
plot(amce$USp)

View(ca[[df]])


##### Attempt to make it work at CIRED #####
decrit(e$conjoint)
decrit(e$`F-1-1-1`) # foreign1: millionaire tax / foreign2: cut aid
e$program_a <- sapply(1:nrow(e), function(n) {paste(e[n, paste0("F-1-1-", 1:5)], collapse = ' ')})
e$program_b <- sapply(1:nrow(e), function(n) {paste(e[n, paste0("F-1-2-", 1:5)], collapse = ' ')})
e$millionaire_tax_in_a <- grepl("foreign_policy1", e$program_a)
e$millionaire_tax_in_b <- grepl("foreign_policy1", e$program_b)
e$cut_aid_in_a <- grepl("foreign_policy2", e$program_a)
e$cut_aid_in_b <- grepl("foreign_policy2", e$program_b)
e$millionaire_tax_in_program <- e$millionaire_tax_in_a
e$cut_aid_in_program <- e$cut_aid_in_a
e$program <- e$program_a
e$program_preferred <- e$conjoint == "Candidate A"
temp <- e
temp$millionaire_tax_in_program <- temp$millionaire_tax_in_b
temp$cut_aid_in_program <- temp$cut_aid_in_b
temp$program <- temp$program_b
temp$program_preferred <- temp$conjoint == "Candidate B"
ce <- cbind(e, temp)
rm(temp)
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program, data = call))
summary(lm(program_preferred ~ millionaire_tax_in_program, data = call))
summary(lm(program_preferred ~ cut_aid_in_program, data = call))
decrit("conjoint", all) # 27% Neither
summary(lm(program_preferred ~ cut_aid_in_program, data = ce, subset = country == "CH"))
summary(lm(program_preferred ~ cut_aid_in_program, data = ce, subset = country != "CH"))
