##### labels_vars #####
labels_vars <- c(
  "(Intercept)" = "Constant",
  "finished" = "Finished",
  "excluded" = "Excluded",
  "country" = "Country",
  "region" = "Region",
  "gender" = "Gender",
  "age_exact" = "Age",
  "country_name" = "Country",
  "couple" = "Lives with partner",
  "hh_size" = "Household size",
  "zipcode" = "Postal code",
  "urbanity" = "Degree of urbanization",
  "urban" = "Urban",
  "age" = "Age",
  "age_factor" = "Age",
  "Nb_children__14" = "Number of children below 14",
  "income" = "Income",
  "income_factor" = "Income quartile",
  "income_quartile" = "Income quartile",
  "income_decile" = "Income decile",
  "income_quartile" = "Income quartile",
  "education" = "Highest diploma",
  # "diploma" = "Highest diploma",
  "post_secondary" = "Education: Post secondary",
  # "diploma_25_64" = "Highest diploma among 25-64",
  # "education" = "Highest diploma (ISCED class)",
  "education_original" = "Highest diploma",
  "employment_agg" = "Employment status",
  "employment_status" = "Employment status",
  "employment_18_64" = "Employment among 18-64",
  "race" = "Race",
  "race_white" = "Race: White",
  "race_black" = "Race: Black",
  "race_hispanic" = "Race: Hispanic",
  "race_asian" = "Race: Asian",
  "home_tenant" = "Home: tenant",
  "home_owner" = "Home: owner",
  "home_landlord" = "Home: landlord",
  "home_hosted" = "Home: hosted",
  "owner" = "Owner",
  "swing_state" = "Swing State",
  "vote" = "Vote",
  "vote_factor" = "Vote",
  "vote3" = "Vote",
  "vote_all" = "Vote (actual and hypothetical)",
  "vote_us" = "Vote",
  "vote_fr_voters" = "Vote (voters)",
  "vote_fr_non_voters" = "Vote (non voters)",
  "vote_agg" = "Vote (actual and hypothetical)",
  "vote_participation" = "Voted at last election",
  "voted" = "Voted at last election",
  # "vote_us_voters" = "Vote (voters)",
  # "vote_us_non_voters" = "Vote intention (non voters)",
  "gcs_support" = "Global climate scheme (GCS)", # "Supports the Global Climate Plan", # "Soutient le Plan mondial pour le climat", #"Global climate scheme (GCS)", # 
  # "gcs_support_100" = "Support for the GCS",
  # "gcs_support_90" = "Support for a Global Climate Scheme at $90/tCO2",
  # "gcs_support_neg" = "Support for the GCS",
  "gcs_belief" = "Belief about GCS support",
  "gcs_belief_own" = "Belief about GCS support in own country",
  "gcs_belief_us" = "Belief about GCS support in the U.S.",
  "attention_test" = "Attention test",
  # "global_tax_global_share" = "Preferred share of global tax for LICs (in %)",
  "group_defended" = "Group defended when voting",
  "survey_biased" = "Survey biased", 
  "survey_biased_yes" = "Survey is biased", 
  "survey_biased_left" = "Survey is left-wing biased", 
  "survey_biased_right" = "Survey is right-wing biased", 
  "survey_biased_no" = "Survey is not biased", 
  "interview" = "Agrees for interview",
  "duration" = "Duration",
  "duration_feedback" = "Duration: feedback",
  "gcs_understood" = "With GCS, gasoline price will increase", 
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "nationalist" = "Nationalist",
  "universalist" = "Universalist",
  "individualist" = "Individualist",
  "woman" = "Gender: Woman",
  "man" = "Gender: Man",
  "global_tax_more_30p" = "Preferred share of global wealth tax for low-income countries: ≥ 30%",
  "global_tax_more_10p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 10%",
  "branch_gcs" = "Treatment",
  # "maritime_split_ldc" = "Maritime levy revenue preferred allocation to:\nSustainable transition in LDCs",
  # "maritime_split_companies" = "Maritime levy revenue preferred allocation to:\nShipping companies to mitigate price increases",
  # "maritime_split_decarbonization" = "Maritime levy revenue preferred allocation to:\nRD&D of zero-emission fuels and ships",
  "maritime_split_ldc" =       "Sustainable transition in LDCs",
  "maritime_split_companies" = "Shipping companies to mitigate price increases",
  "maritime_split_decarbonization" = "RD&D of zero-emission fuels and ships",
  "solidarity_support_shipping_levy" = "International levy on carbon emissions from shipping,\nfinancing countries' budgets in proportion to their population", # "Global maritime fuel levy with equal pc revenue sharing", # 
  "why_hic_help_lic_responsibility" = "Responsibility: Historical responsibility of HICs for situation in LICs", # High-income countries have a historical responsibility for the current situation in low-income countries
  "why_hic_help_lic_interest" = "Interest: Long-term interest of HICs to help LICs", # In the long run, it is in the interest of high-income countries to help low-income countries
  "why_hic_help_lic_duty" = "Duty: Helping countries in need is the right thing to do.", # Helping those in need is the right thing to do. This is also true at the international level
  "why_hic_help_lic_none" = "None of the above",
  "global_movement_no" = "Would not support such a movement",
  "global_movement_spread" = "Could sign a petition and spread ideas",
  "global_movement_demonstrate" = "Could attend a demonstration",
  "global_movement_strike" = "Could go on strike",
  "global_movement_donate" = "Could donate [$100] to a strike fund",
  "solidarity_support_billionaire_tax" = "Minimum tax of 2% on billionaires' wealth, in voluntary countries", # A minimum tax of 2% on the wealth of billionaires, in voluntary countries
  "solidarity_support_corporate_tax" = "Raise global minimum tax on profit from 15% to 35%, allocating revenues to countries based on sales", 
  # Raising the globally agreed minimum tax rate on profits of multinational firms from 15% to 35%, closing loopholes and allocating revenues to countries where sales are made
  "solidarity_support_expanding_security_council" = "Expand Security Council to new permanent members (e.g. India, Brazil, AU), restrict veto use", 
  # Expanding the UN Security Council (in charge of peacekeeping) to new permanent members such as India, Brazil, and the African Union, and restricting the use of the veto
  "solidarity_support_foreign_aid" = "At least 0.7% of developed countries' GDP in foreign aid", # Developed countries contributing at least 0.7% of their GDP in foreign aid and development assistance
  "solidarity_support_debt_relief" = "Debt relief for vulnerable countries, suspending payments until they are more able to repay", # Debt relief for vulnerable countries by suspending repayments until they are better able to repay, promoting their development
  "solidarity_support_bridgetown" = "Bridgetown initiative: MDBs expanding sustainable investments in LICs, and at lower interest rates", # Institutions like the World Bank investing in many more sustainable projects in lower-income countries, and offering lower interest rates (the Bridgetown initiative)
  "solidarity_support_loss_damage" = "L&D: Developed countries financing a fund to help vulnerable countries cope with cliamte Loss and damage", # Developed countries financing a fund to help vulnerable countries cope with loss and damage from climate change
  "solidarity_support_ncqg_300bn" = "NCQG: Developing countries providing $300 bn a year in climate finance for developing countries", # Developed countries providing $300 billion a year (0.4% of their GDP) to finance climate action in developing countries
  "solidarity_support_shipping_levy" = "Levy on shipping carbon emissions, returned to countries based on population", # An international levy on carbon emissions from shipping, funding national budgets in proportion to population
  "solidarity_support_aviation_levy" = "Levy on aviation carbon emissions, returned to countries based on population", # An international levy on carbon emissions from aviation, raising ticket prices by 30% and funding national budgets in proportion to population
  "transfer_how_agencies" = "Development aid agencies", # Transfers to public development aid agencies which then finance suitable projects
  "transfer_how_govt_conditional" = "Government, conditional on financing poverty reduction", # Transfers to the national government conditioned on the use of funds for poverty reduction programs
  "transfer_how_govt_unconditional" = "Government, unconditional", # Unconditional transfers to the national government
  "transfer_how_local_authorities" = "Local authorities", # Unconditional transfers to local authorities (municipality, village chief...)
  "transfer_how_ngo" = "Local NGOs with democratic processes", # Transfers to local NGOs with democratic decision-making processes
  "transfer_how_social_protection" = "Target cash transfers (child allowances, disability & elderly pensions)", # Cash transfers to parents (child allowances), to the disabled and to the elderly
  "transfer_how_cash_unconditional" = "Unconditional cash transfers to each household", # Unconditional cash transfers to each household
  setNames(names(p), names(p))
)
for (v in intersect(names(all), names(labels_vars))) { # intersect(c(socio_demos, socio_demos_us), names(all)), 
  if (grepl("TRUE / FALSE", Levels(all[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(all[[v]]), NA)) {
    if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
  }
}


##### labels_vars_short_html #####
labels_vars_short_html <- c(
  "gcs_understood" = "With GCS,<br>gasoline price will increase",
  "global_tax_more_30p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 30%",
  "democratise_un_imf_support" = "Democratise international institutions (UN, IMF) by making<br>a country's voting right proportional to its population"
)

##### labels_vars_country #####
labels_vars_country <- list() #"US" = c(), "DE" = c(), "FR" = c(), "ES" = c(), "UK" = c())
labels_vars_country$FR["gcs_support"] <- "Plan mondial pour le climat"
labels_vars_country$FR["global_tax_global_share"] <- "Part préférée de l'ISF mondial pour les pays pauvres (en %)"
# for (c in countries) labels_vars_country[[c]] <- labels_vars


##### heatmaps_defs #####
heatmaps_defs <- list()
heatmaps_defs <- list(
  "gcs_support" = list(vars = "gcs_support", conditions = ">= 1"), 
  "duration" = list(vars = variables_duration, conditions = ""),
  "share_solidarity_supported" = list(vars = c("share_solidarity_supported"), conditions = c("")),
  "transfer_how" = list(vars = variables_transfer_how, conditions = ">= 1"), 
  "solidarity_support" = list(vars = variables_solidarity_support),
  "global_movement" = list(vars = variables_global_movement, conditions = ">= 1"), 
  "why_hic_help_lic" = list(vars = variables_why_hic_help_lic, conditions = ">= 1")
)
# TODO heatmaps: transfer_how, solidarity_support, global_movement, why_help_lic

##### vars_heatmaps #####
vars_heatmaps <- c("variables_transfer_how", "solidarity_support", "global_movement", "why_hic_help_lic") 
# TODO: special = all; automatize conditions = ">= 1" for binary vars; automatize folder creation; remove dependencies on objects such as countries_names_fr; remove NULL

heatmaps_defs <- fill_heatmaps(vars_heatmaps, heatmaps_defs)

# heatmap_multiple(heatmaps_defs)


##### barres_defs #####
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  "maritime_split_decarbonization" = list(height = 250),
  "maritime_split_companies" = list(height = 250),
  "maritime_split_ldc" = list(height = 250)#,
  # "solidarity_support_shipping_levy" = list(height = 250, width = 870)
  # "understood_each" = list(vars = variables_understood, width = 850), 
  # "negotiation" = list(width = 940), 
  # "points_mean" = list(vars = variables_points_us_agg, width = 850, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)), # 1080 points_us
)

vars_barres <- c("ncqg", "ncqg_full", "maritime_split", "solidarity_support_shipping_levy") 

barres_defs <- fill_barres(vars_barres, barres_defs) # , df = us1

vars_barresN <- vars_barres
barresN_defs <- fill_barres(vars_barresN, along = "country_name")


##### Plot #####
# barres_multiple(barresN_defs[c("foreign_aid_raise_support")])
barres_multiple(barres_defs["solidarity_support_shipping_levy"])
barres_multiple(barresN_defs["solidarity_support_shipping_levy"], folder = "../figures/country_comparison/")
barres_multiple(barres_defs)

# heatmap_multiple(heatmaps_defs["var"])
heatmap_multiple(heatmaps_defs)
heatmap_multiple(heatmaps_defs["global_movement"])


##### Maritime #####
mean_maritime_split <- array(NA, dim = c(3, 4), dimnames = list(variables_maritime_split, paste0(pilot_countries_all, "p")))
for (c in paste0(pilot_countries_all, "p")) for (v in variables_maritime_split) mean_maritime_split[v, c] <- wtd.mean(d(c)[[v]], d(c)$weight)
barres(mean_maritime_split/100, save = T, file = "../figures/country_comparison/mean_maritime_split", export_xls = T, 
       miss = F, rev_color = T, sort = F, legend = c("Decarbonized fuels & ships", "Shipping companies", "LDCs"), labels = rev(c(countries_names, "All")))

median_maritime_split <- array(NA, dim = c(3, 4), dimnames = list(variables_maritime_split, paste0(pilot_countries_all, "p")))
for (c in paste0(pilot_countries_all, "p")) for (v in variables_maritime_split) median_maritime_split[v, c] <- wtd.median(d(c)[[v]], d(c)$weight, na.rm = T)
barres(median_maritime_split/100, save = T, file = "../figures/country_comparison/median_maritime_split", export_xls = T, 
       miss = F, rev_color = T, sort = F, legend = c("Decarbonized fuels & ships", "Shipping companies", "LDCs"), labels = rev(c(countries_names, "All")))
barres(as.matrix(median_maritime_split[,4])/100, save = T, file = "../figures/all/median_maritime_split", export_xls = T, 
       miss = F, rev_color = T, sort = F, legend = c("Decarbonized fuels & ships", "Shipping companies to reduce prices", "Sust. transition in LDCs"), 
       labels = c("Median preferred allocation for\nglobal maritime levy revenue\n(3,018 respondents from Poland, UK & U.S.)"))

barres(as.matrix(mean_maritime_split[,4])/100, save = T, file = "../figures/all/mean_maritime_split", export_xls = T, 
       miss = F, rev_color = T, sort = F, legend = c("Decarbonized fuels & ships", "Shipping companies to reduce prices", "Sust. transition in LDCs"), 
       labels = c("Mean preferred allocation for\nglobal maritime levy revenue\n(3,018 respondents from Poland, UK & U.S.)"))
