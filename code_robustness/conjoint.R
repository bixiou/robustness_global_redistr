policies_names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies")) # , rowNames = T, rows = c(1, 16:41), cols = 1:6
languages <- colnames(policies_names)[!grepl("_|[a-z]", colnames(policies_names))]

policies.names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies", rowNames = T)) #, rows = c(1, 16:41), cols = 1:6))
policies.names <- policies.names[is.na(as.numeric(row.names(policies.names))), ] # NAs by coercion normal

conjoint_attributes <- c("econ_issues", "society_issues", "climate_pol", "tax_system", "foreign_policy")
formula_cjoint <- as.formula("selected ~ econ_issues + society_issues + climate_pol + tax_system + foreign_policy")
# formula_cjoint_specific <- as.formula("selected ~ `Economic issues` + `Societal issues` + `Climate policy` + `Tax system` + `Foreign policy`")
# /!\ If Error in if (any(as.vector(r_1) - as.vector(cross_tab_std[m,... add a (second) blank line at the end of the .dat
# design_cjoint_US <- makeDesign(filename = "../conjoint_analysis/9d_F.dat") # gives the probability that a profile appears: sum(design_cjoint$J)=1, dim(design_cjoint$J) = 5 4 4 4 5. 
# design_cjoint_EU <- makeDesign(filename = "../conjoint_analysis/9d_F_EU.dat") 
# design_cjoint_both <- makeDesign(filename = "../conjoint_analysis/9d_F_both.dat") # The weighted are the average of the US and EU weights
amce <- ca <- list() # We should have "Old qualtrics format detected." (otherwise it would assume new format and delete the first observation).
for (df in paste0(pilot_countries, "p")) { # "usp", "eup", "ep"
  print(df)
  csv.path <- paste0("../conjoint_analysis/ca_", df, ".csv")
  write.csv(d(df)[!is.na(d(df)$conjoint_number), c(variables_conjoint_all, 'conjoint_number', 'conjoint', 'n')], csv.path, row.names = FALSE)
  temp <- readLines(csv.path)
  writeLines(c(temp[1], temp), csv.path)
  # /!\ Bugs at CIRED but works well at home
  ca[[df]] <- read.qualtrics(csv.path, responses = 'conjoint_number', covariates = c(variables_conjoint_policies), respondentID = "n") # names(d(n))[cols_conjoint]
  names(ca[[df]])[1] <- "n"
  ca[[df]] <- merge(d(df)[, intersect(names(d(df)), c("country", "n"))], ca[[df]]) # vote
  for (i in 1:5) {
    ca[[df]][[conjoint.attributes[i]]] <- as.character(ca[[df]][[conjoint_attributes[i]]]) 
    for (c in paste0(pilot_countries, "p")) {
      temp <- which(ca[[df]]$country == c) # & !(ca[[df]][[conjoint_attributes[i]]] %in% c("soc3", "tax3", "-")))
      ca[[df]][[conjoint.attributes[i]]][temp] <- as.character(policies.names[as.character(ca[[df]][[conjoint_attributes[i]]][temp]), c])
    }
    # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "-"] <- "-"
    # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "soc3"] <- "Making abortion a right at the federal level"
    # ca[[df]][[conjoint.attributes[i]]][ca[[df]][[conjoint_attributes[i]]] == "tax3"] <- "Increase corporate income tax rate from 21% to 28%"
    ca[[df]][[conjoint.attributes[i]]] <- as.factor(ca[[df]][[conjoint.attributes[i]]])
    ca[[df]][[paste0(conjoint.attributes[i], ".rowpos")]] <- ca[[df]][[paste0(conjoint_attributes[i], ".rowpos")]]
  }
  design_cjoint <- makeDesign(filename = paste0("../conjoint_analysis/9d_", sub("p", "", c), ".dat")) 
  amce[[df]] <- amce(formula_cjoint, ca[[df]], design = design_cjoint, cluster = FALSE, weights= NULL)
}