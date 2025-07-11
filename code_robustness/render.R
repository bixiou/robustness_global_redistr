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
  "revenue_split_few_domestic_tax_reduction" = "Domestic: Reduction in the income tax",
  "revenue_split_few_domestic_tax_reduction_agg" = "Domestic: Reduction in the income tax",
  "split_many_global_when_appear" = "Share allocated to Global spending options\nwhen such options are part of the 5 (out of 13) randomly selected ones",
  "split_many_global" = "Share allocated to Global spending options\nwhen 5 out of 13 options are randomly selected",
  "gcs_support" = "Global climate scheme (GCS)", # "Supports the Global Climate Plan", # "Soutient le Plan mondial pour le climat", #"Global climate scheme (GCS)", # 
  # "gcs_support_100" = "Support for the GCS",
  # "gcs_support_90" = "Support for a Global Climate Scheme at $90/tCO2",
  "gcs_support_control" = "Supports the Global Climate Scheme (GCS)",
  "gcs_belief" = "Belief about GCS support",
  "gcs_belief_own" = "Belief about GCS support in own country",
  "gcs_belief_us" = "Belief about GCS support in the U.S.\n(except for the U.S.: support in the EU)",
  "ncs_support" = "Supports the National Climate Scheme", 
  "ics_high_color_support" = "$ atop('     Supports the GCS if its other members* cover 64-72% of world emissions',          
                                     '*' * bold('High color') * ': High + Distributive effects displayed using colors on world map')", 
  "ics_high_support" = "$ atop('                    Supports the GCS if its other members* cover 64-72% of world emissions', 
                               '*' * bold(High) * ': Global South + China + EU + various HICs (UK, Japan, South Korea, Canada...)')", 
  "ics_mid_support" = "$ atop('Supports the GCS if its other members* cover 56% of world emissions', 
                              '                                                                   *' * bold('Mid') * ': Global South + China')", 
  "ics_low_support" = "$ atop('Supports the GCS if its other members* cover 25-33% of world emissions', 
                              '                                                                            *' * bold('Low') * ': Global South + EU')", # The syntax $ paste uses plotmath, cf. https://www.rdocumentation.org/packages/grDevices/versions/3.6.2/topics/plotmath
  # "ics_high_support" = "Supports the GCS if its other members* cover 64-72% of world emissions\n*High: Global South + China + EU + various HICs (UK, Japan, South Korea, Canada...)", 
  # "ics_high_color_support" = "Supports the GCS if its other members* cover 64-72% of world emissions\n*High color: High + Distributive effects displayed using colors on world map", 
  # "ics_mid_support" = "Supports the GCS if its other members* cover 56% of world emissions\n*Mid: Global South + China", 
  # "ics_low_support" = "Supports the GCS if its other members* cover 25-33% of world emissions\n*Low: Global South + EU", 
  "attention_test" = "Attention test",
  # "global_tax_support" = "Supports tax on millionaires' wealth with 30% funding low-income countries if\nGlobal: implemented by all other countries",
  # "hic_tax_support" = "Supports tax on millionaires' wealth with 30% funding low-income countries if\nHigh-income: implemented by all other HICs and not by some MICs (such as China)",
  # "intl_tax_support" = "Supports tax on millionaires' wealth with 30% funding low-income countries if\nInternational: implemented by some (e.g. EU, UK, Brazil) and not by others (e.g. U.S., China)",
  "global_tax_support" = "Global: implemented by all other countries",
  "hic_tax_support" = "High-income: implemented by all other HICs and not by some MICs (such as China)",
  "intl_tax_support" = "International: implemented by some (e.g. EU, UK, Brazil) and not by others (e.g. U.S., China)",
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
  "gcs_comprehension" = "As a result of the GCS, gasoline price would...", 
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "my_tax_global_nation" = '"My taxes should go towards solving global problems"',
  "convergence_support" = '"Governments should actively cooperate to have\nall countries converge in terms of GDP per capita by the end of the century"',
  "nationalist" = "Fellow citizens", # "Nationalist",
  "universalist" = "Humans or Sentient beings", # "Universalist",
  "individualist" = "Family and self", # "Individualist",
  "humanist" = "Humans", # "Humanist",
  "antispecist" = "Sentient beings (humans and animals)", # "Antispeciesist",
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
  "why_hic_help_lic_responsibility" = "Responsibility: Historical responsibility of HICs for situation in LICs", # High-income countries have a historical responsibility for the current situation in low-income countries
  "why_hic_help_lic_interest" = "Interest: Long-term interest of HICs to help LICs", # In the long run, it is in the interest of high-income countries to help low-income countries
  "why_hic_help_lic_duty" = "Duty: Helping countries in need is the right thing to do", # Helping those in need is the right thing to do. This is also true at the international level
  "why_hic_help_lic_none" = "None of the above",
  "global_movement_no" = "Would not support such a movement",
  "global_movement_spread" = "Could sign a petition and spread ideas",
  "global_movement_demonstrate" = "Could attend a demonstration",
  "global_movement_strike" = "Could go on strike",
  "global_movement_donate" = "Could donate [$100] to a strike fund",
  "solidarity_support_billionaire_tax" = "Minimum tax of 2% on billionaires' wealth, in voluntary countries", # A minimum tax of 2% on the wealth of billionaires, in voluntary countries
  "solidarity_support_corporate_tax" = "Raise global minimum tax on profit from 15% to 35%, allocating revenues to countries based on sales", 
  # Raising the globally agreed minimum tax rate on profits of multinational firms from 15% to 35%, closing loopholes and allocating revenues to countries where sales are made
  "solidarity_support_expanding_security_council" = "Expand Security Council to new permanent members (e.g. India, Brazil, African Union), restrict veto use", 
  # Expanding the UN Security Council (in charge of peacekeeping) to new permanent members such as India, Brazil, and the African Union, and restricting the use of the veto
  "solidarity_support_foreign_aid" = "At least 0.7% of developed countries' GDP in foreign aid", # Developed countries contributing at least 0.7% of their GDP in foreign aid and development assistance
  "solidarity_support_debt_relief" = "Debt relief for vulnerable countries, suspending payments until they are more able to repay", # Debt relief for vulnerable countries by suspending repayments until they are better able to repay, promoting their development
  "solidarity_support_bridgetown" = "Bridgetown initiative: MDBs expanding sustainable investments in LICs, and at lower interest rates", # Institutions like the World Bank investing in many more sustainable projects in lower-income countries, and offering lower interest rates (the Bridgetown initiative)
  "solidarity_support_loss_damage" = "L&D: Developed countries financing a fund to help vulnerable countries cope with climate Loss and damage", # Developed countries financing a fund to help vulnerable countries cope with loss and damage from climate change
  "solidarity_support_ncqg_300bn" = "NCQG: Developing countries providing $300 bn a year in climate finance for developing countries", # Developed countries providing $300 billion a year (0.4% of their GDP) to finance climate action in developing countries
  "solidarity_support_shipping_levy" = "International levy on shipping carbon emissions, returned to countries based on population", # An international levy on carbon emissions from shipping, funding national budgets in proportion to population
  # "solidarity_support_shipping_levy" = "International levy on carbon emissions from shipping,\nfinancing countries' budgets in proportion to their population", # "Global maritime fuel levy with equal pc revenue sharing", # 
  "solidarity_support_aviation_levy" = "International levy on aviation carbon emissions, raising prices by 30%, returned to countries based on population", # An international levy on carbon emissions from aviation, raising ticket prices by 30% and funding national budgets in proportion to population
  "ncqg" = "Preferred North-to-South climate grant funding in 2035", # Preferred North-to-South climate funding
  "ncqg_full" = "Preferred North-to-South climate grant funding in 2035",
  "transfer_how_agencies" = "Development aid agencies", # Transfers to public development aid agencies which then finance suitable projects
  "transfer_how_govt_conditional" = "Government, conditional on financing poverty reduction", # Transfers to the national government conditioned on the use of funds for poverty reduction programs
  "transfer_how_govt_unconditional" = "Government, unconditional", # Unconditional transfers to the national government
  "transfer_how_local_authorities" = "Local authorities", # Unconditional transfers to local authorities (municipality, village chief...)
  "transfer_how_ngo" = "Local NGOs with democratic processes", # Transfers to local NGOs with democratic decision-making processes
  "transfer_how_social_protection" = "Targeted cash transfers (child allowances, disability & elderly pensions)", # Cash transfers to parents (child allowances), to the disabled and to the elderly
  "transfer_how_cash_unconditional" = "Unconditional cash transfers to each household", # Unconditional cash transfers to each household
  "sustainable_future" = "Prefers sustainable future", 
  "top1_tax_support" = "Supports tax on world top 1% to finance global poverty reduction\n(Additional 15% tax on income over [$120k/year in PPP])",
  "top3_tax_support" = "Supports tax on world top 3% to finance global poverty reduction\n(Additional 15% tax over [$80k], 30% over [$120k], 45% over [$1M])",
  "vote_intl_coalition" = "More likely to vote for party if part of worldwide coalition for climate action and global redistribution",
  "reparations_support" = "Supports reparations for colonization and slavery in the form of funding education and technology transfers",
  "custom_redistr_winners" = "Preferred share of winners",
  "custom_redistr_losers" = "Preferred share of losers",
  "custom_redistr_degree" = "Preferred degree of redistribution",
  "custom_redistr_income_min" = "Implied minimum income",
  "custom_redistr_transfer" = "Implied transfer (in % of world income)",
  "well_being_gallup_0" = "Well-being: Gallup, 0-10 scale",
  "well_being_gallup_1" = "Well-being: Gallup, 1-10 scale",
  "well_being_wvs_0" = "Well-being: World Values Survey, 0-10 scale",
  "well_being_wvs_1" = "Well-being: World Values Survey, 1-10 scale",
  setNames(names(all), names(all))
)
for (v in names(all)) { # intersect(c(socio_demos, socio_demos_us), names(all)), 
  if (grepl("-", Label(all[[v]])) & labels_vars[v] == v) labels_vars[v] <- sub("(.*)- ", "", Label(all[[v]]))
  if (grepl("_control", v) & labels_vars[v] == v) labels_vars[v] <- labels_vars[sub("_control", "", v)]
  if (grepl("TRUE / FALSE", Levels(all[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(all[[v]]), NA)) {
    if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
  }
}


##### labels_vars_short_html #####
# labels_vars_short_html <- c(
#   "gcs_understood" = "With GCS,<br>gasoline price will increase",
#   "global_tax_more_30p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 30%",
#   "democratise_un_imf_support" = "Democratise international institutions (UN, IMF) by making<br>a country's voting right proportional to its population"
# )
# 
# ##### labels_vars_country #####
# labels_vars_country <- list() #"US" = c(), "DE" = c(), "FR" = c(), "ES" = c(), "UK" = c())
# labels_vars_country$FR["gcs_support"] <- "Plan mondial pour le climat"
# labels_vars_country$FR["global_tax_global_share"] <- "Part préférée de l'ISF mondial pour les pays pauvres (en %)"
# # for (c in countries) labels_vars_country[[c]] <- labels_vars


##### heatmaps_defs #####
heatmaps_defs <- list()
heatmaps_defs <- list(
  "gcs_support" = list(vars = "gcs_support", conditions = ">= 1"),
  "gcs_support_control" = list(vars = "gcs_support", conditions = ">= 1"),  
  "belief" = list(vars = variables_gcs_belief, conditions = "", nb_digits = 0), 
  "gcs_belief" = list(vars = c("gcs_support", variables_gcs_belief), conditions = "", nb_digits = 0), 
  "variables_ics" = list(vars = variables_ics, conditions = ">= 1"), 
  "gcs_all" = list(vars = variables_gcs_all, conditions = "", nb_digits = 0), 
  "gcs_ics" = list(vars = variables_gcs_ics, conditions = ">= 1"), 
  "gcs_ics_all" = list(vars = variables_gcs_ics_all, conditions = "", nb_digits = 0), 
  "ncs_gcs_ics" = list(vars = variables_ncs_gcs_ics, conditions = ">= 1"), 
  "ncs_gcs_ics_all" = list(vars = variables_ncs_gcs_ics_all, conditions = "", nb_digits = 0), 
  "duration" = list(vars = variables_duration, conditions = ""),
  "share_solidarity_supported" = list(vars = c("share_solidarity_supported"), conditions = c("")),
  "transfer_how" = list(vars = variables_transfer_how, conditions = c(">= 1", "< 0", "> 1"), sort = T), 
  "solidarity_support" = list(vars = variables_solidarity_support_control, sort = T),
  "solidarity_support_incl_info" = list(vars = variables_solidarity_support, sort = T),
  "global_movement" = list(vars = variables_global_movement, conditions = ">= 1"), 
  "why_hic_help_lic" = list(vars = variables_why_hic_help_lic, conditions = ">= 1"), 
  "sustainable_future" = list(vars = "sustainable_future", conditions = ">= 1"), 
  "top_tax" = list(vars = c("top1_tax_support", "top3_tax_support"), conditions = ">= 1"),
  "wealth_tax_support" = list(vars = variables_wealth_tax_support, conditions = ">= 1"),
  "custom_redistr_all" = list(vars = variables_custom_redistr_all, conditions = ""),
  "radical_redistr" = list(vars = variables_radical_redistr, conditions = c(">= 1", "/")),
  "radical_redistr_few" = list(vars = c("top1_tax_support", "top3_tax_support", "convergence_support", "reparations_support", "my_tax_global_nation"), conditions = c(">= 1", "/")),
  "well_being" = list(vars = variables_well_being, conditions = ""),
  "group_defended_3" = list(vars = variables_group_defended_3, conditions = ">= 1"),
  "group_defended_4" = list(vars = variables_group_defended_4, conditions = ">= 1"),
  "group_defended_5" = list(vars = variables_group_defended_5, conditions = ">= 1"),
  "split_few" = list(vars = variables_split_few, conditions = c("", ">= 1")), # white color at 20
  "split_many" = list(vars = variables_split_many, conditions = c("", ">= 1")),
  "split_many_global" = list(vars = variables_split_many_global, conditions = c("", ">= 1"))
)
# TODO! vote, fields

##### vars_heatmaps #####
vars_heatmaps <- c("transfer_how", "solidarity_support", "global_movement", "why_hic_help_lic", "convergence_support", "my_tax_global_nation", "reparations_support") 
# TODO: automatize conditions = ">= 1" for binary vars; automatize folder creation; remove dependencies on objects such as countries_names_fr; remove NULL

heatmaps_defs <- fill_heatmaps(vars_heatmaps, heatmaps_defs)

heatmap_multiple(heatmaps_defs)
# heatmap_multiple(heatmaps_defs)


##### barres_defs #####
barres_defs <- list( # It cannot contained unnamed strings (e.g. it can contain "var" = "var" but not simply "var")
  "split_few" = list(vars = variables_split_few_agg, width = 850, rev_color = T), #, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)) TODO add var name in transform_mean to use non _agg var and compute true mean
  "maritime_split_decarbonization" = list(height = 250),
  "maritime_split_companies" = list(height = 250),
  "maritime_split_ldc" = list(height = 250),
  "split_many" = list(vars = variables_split_many_agg, width = 850, rev_color = T),
  "split_many_global" = list(vars = variables_split_many_global_agg, width = 850, rev_color = T),
  "ncqg" = list(vars = "ncqg", width = 1070, height = 640),
  "ncqg_full" = list(vars = "ncqg_full", width = 1070, height = 640)
  # ncqg: rev = F, rev_color = T
  # "split_many"
  # "split_many_global
  # "solidarity_support_shipping_levy" = list(height = 250, width = 870)
  # "understood_each" = list(vars = variables_understood, width = 850), 
  # "negotiation" = list(width = 940), 
  # "points_mean" = list(vars = variables_points_us_agg, width = 850, sort = FALSE, add_means = T, show_legend_means = T, transform_mean = function(x) return(x/100)), # 1080 points_us
)

vars_barres <- c("maritime_split", "solidarity_support_aviation_levy", "solidarity_support_billionaire_tax", "sustainable_future", "vote_intl_coalition", 
                 "group_defended", "reparations_support", "gcs_support_control", "gcs_comprehension", "survey_biased") # 

barres_defs <- fill_barres(vars_barres, barres_defs) # , df = us1
barresN_defs <- fill_barres(vars_barres, along = "country_name")

vars_barres1 <- c("split_few", "split_many", "split_many_global") # , "maritime_split" TODO: no error when variable not found
vars_barresN <- setdiff(names(barres_defs), vars_barres1)

##### Plot #####
# barres_multiple(barresN_defs[c("foreign_aid_raise_support")])
barres_multiple(barres_defs[vars_barres1]) # TODO: pb maritime_split => avoid error when variables not present
barres_multiple(barresN_defs[names(barresN_defs)[!names(barresN_defs) %in% vars_barres1]], nolabel = T)
# barres_multiple(barres_defs)
barres_multiple(barresN_defs["gcs_support_control"])
barres_multiple(barres_defs["split_few"])

# heatmap_multiple(heatmaps_defs["var"])
heatmap_multiple(heatmaps_defs["solidarity_support"])
heatmap_multiple(heatmaps_defs["gcs_ics_all"])
heatmap_multiple(heatmaps_defs["gcs_belief"]) # TODO! fix colors
heatmap_multiple(heatmaps_defs[c("gcs_ics_all", "ncs_gcs_ics_all")])
heatmap_multiple(heatmaps_defs[c("ncs_gcs_ics_all")], levels = levels_merge_EU)
heatmap_multiple(heatmaps_defs[c("gcs_ics_all", "solidarity_support")])


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


##### Split #####
# data_countries <- countries[-9]
# data_split_few <- matrix(NA, dimnames = list(variables_split_few, data_countries), nrow = 5, ncol = length(data_countries))
# for (v in variables_split_few) for (c in data_countries) data_split_few[v, c] <- wtd.mean(d(c)[[v]], d(c)$weight, na.rm = T)
# barres(data_split_few/100, save = T, export_xls = T, miss = F, rev_color = T, sort = F, file = "../figures/country_comparison/split_few_bars",
#        legend = labels_vars[variables_split_few], labels = countries_names[data_countries])


##### Custom redistr #####
current_inc <- c(0, round(thousandile_world_disposable_inc))
plot(0:1000, current_inc, type = 'l', lwd = 2, col = "red", ylim = c(0, 1e5))
lines(0:1000, mean_custom_redistr[["all"]], type = 'l', lwd = 2, col = "green")
grid()

plot(0:1000, log10(current_inc), type = 'l', lwd = 2, col = "red", ylim = c(3, 6))
lines(0:1000, log10(mean_custom_redistr[["all"]]), type = 'l', lwd = 2, col = "green")
grid()
lines(0:1000, log10(pmax(250*12, world_income_after_tax("top1"))), type = 'l', lwd = 2, col = "blue")
lines(0:1000, log10(pmax(400*12, world_income_after_tax("top3"))), type = 'l', lwd = 2, col = "cyan")

plot(seq(0, 1e5, 1e2), tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = seq(0, 1e5, 1e2)), type = 'l', lwd = 2, ylim = c(0, .2), ylab = "Tax rate", xlab = "Individualized yearly income (in PPP 2024 $)")
grid()

plot(seq(0, 1e6, 1e3), tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = seq(0, 1e6, 1e3)), type = 'l', lwd = 2, ylim = c(0, .3), ylab = "Tax rate", xlab = "Individualized yearly income (in PPP 2024 $)")
lines(seq(0, 1e6, 1e3), tax_rates_custom_redistr(world_income_after_tax("top1"), at = seq(0, 1e6, 1e3)), type = 'l', lwd = 2, col = "blue")
lines(seq(0, 1e6, 1e3), tax_rates_custom_redistr(world_income_after_tax("top3"), at = seq(0, 1e6, 1e3)), type = 'l', lwd = 2, col = "purple")
grid()

plot(seq(0, 1e5, 1e2), tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = seq(0, 1e5, 1e2), marginal = T), type = 'l', lwd = 2, ylim = c(0, .4), ylab = "Tax rate", xlab = "Individualized yearly income (in PPP 2024 $)")
grid() # ~ 7-8% above 25k & 15% above 40k

plot(seq(0, 99.9, .1), tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = 1:1000, marginal = T, fct_income = F), type = 'l', lwd = 2, ylim = c(0, .4), ylab = "Tax rate", xlab = "Individualized income centile")
grid() 

plot(seq(0, 99.9, .1), tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = 1:1000, marginal = F, fct_income = F), type = 'l', lwd = 2, ylim = c(-2, .5), ylab = "Tax rate", xlab = "Individualized income centile")
grid() 



##### Presentation #####
# Revenue split
barres_multiple(barres_defs["split_few"]) # 670 x 330

# Warm glow -- moral substitute

# International Climate Scheme
heatmap_multiple(heatmaps_defs[c("ncs_gcs_ics_all")]) # 1700 x 650

# Realistic global policies
heatmap_multiple(heatmaps_defs[c("solidarity_support")]) # 1480 x 790

# Warm glow -- realism

# International wealth tax
heatmap_multiple(heatmaps_defs[c("wealth_tax_support")]) # 1370 x 320

# NCQG
barres_multiple(barresN_defs[c("ncqg", "ncqg_full")]) # 850 x 610

# Preferred means of transfers
heatmap_multiple(heatmaps_defs[c("transfer_how")]) # 1240 x 520

# Radical redistribution
heatmap_multiple(heatmaps_defs[c("radical_redistr")]) 
heatmap_multiple(heatmaps_defs[c("radical_redistr_few")], weights = F) # 1550 x 450 TODO! fix weights

barres_multiple(barresN_defs[c("group_defended")]) # 1300 x 700
heatmap_multiple(heatmaps_defs[c("global_movement")]) # 1080 x 410
heatmap_multiple(heatmaps_defs[c("why_hic_help_lic")]) # 1200 x 380
heatmap_multiple(heatmaps_defs[c("sustainable_future")]) # 1100 x 300
barres_multiple(barresN_defs[c("vote_intl_coalition")], df = all[!all$country %in% c("SA", "RU"),]) # 800 x 500

# Custom redistribution
heatmap_multiple(heatmaps_defs[c("custom_redistr_all")])

# Conjoint analysis

# Open-ended fields

## Other
barres_defs[["solidarity_support_billionaire_tax"]]$labels <- "Un impôt minimum de 2% sur la fortune des milliardaires,<br>dans les pays volontaires"
barres_defs[["solidarity_support_billionaire_tax"]]$legend <- c("Très opposé⋅e", "Plutôt opposé⋅e", "Indifférent⋅e ou ne sais pas", "Plutôt favorable", "Très favorable")
barres_multiple(barres_defs[c("solidarity_support_billionaire_tax")], df = FR, levels = "France")


##### Paper #####
# 1? coverage map
# 2. survey_flow
# 3. keywords in fields (taken jointly)
# 4. revenue_split_global+revenue_split_few_global+revenue_split_few_domestic_education_healthcare: one point + error bar for mean per country; one global, one point for # 0% (per country + global)
# 4bis. Average split by country (+ dot for share of 0 for global) Pb: no error bars
# 5a. ICS: mean of variant (incl. NCS, GCS) (per country + global) 
# 5b. wealth tax by coverage: mean of variant (country + global)
# 6. conjoint: foreign aid + global tax (per country + global)
# 7. warm_glow: effect of info + display donation vs. control (per country + global)
# 7bis: 2SLS
# 8. solidarity_support (on control): heatmap
# 9. radical_redistr: heatmap sustainability, top_tax, reparations, NCQG? TODO!, vote_intl_coalition, group_defended?, my_tax_global_nation, TODO my_tax_global_nation other source?, convergence_support
# 10. group_defended: barresN or barres?
# 11. transfer_how: heatmap (maybe just one row grouping all countries and options in columns)
# 12. average custom_redistr

# 4. Revenue split
plot_along("country_name", vars = c("revenue_split_few_global", "revenue_split_few_domestic_education_healthcare", "split_many_global", variables_split_many_global), 
           name = "split_main_means", levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = 1300, height = 650) 
plot_along("country_name", vars = c("revenue_split_few_global", "revenue_split_few_domestic_education_healthcare", "split_many_global", variables_split_many_global), 
           name = "split_main_means_nolegend", no_legend = T, levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = 900, height = 650) 

plot_along("country_name", vars = c("revenue_split_few_global", "revenue_split_few_domestic_education_healthcare", "split_many_global", variables_split_many_global), 
           name = "split_main_nb0", conditions = "== 0", to_percent = T, levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = 1300, height = 650) 
plot_along("country_name", vars = c("revenue_split_few_global", "revenue_split_few_domestic_education_healthcare", "split_many_global", variables_split_many_global), 
           name = "split_main_nb0_nolabel", no_label = T, conditions = "== 0", to_percent = T, levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = 500, height = 650) 

# 4bis. 
split_few <- array(NA, dim = c(5, 12), dimnames = list(variables_split_few, rev(names(levels_default_list))))
for (c in names(levels_default_list)) for (v in variables_split_few) split_few[v, c] <- with(all[all$country_name %in% levels_default_list[[c]],], wtd.mean(eval(str2expression(v)), weight))/100
split_many <- array(NA, dim = c(13, 12), dimnames = list(variables_split_many, rev(names(levels_default_list))))
for (c in names(levels_default_list)) for (v in variables_split_many) split_many[v, c] <- with(all[all$country_name %in% levels_default_list[[c]],], wtd.mean(eval(str2expression(v)), weight))
split_many <- sweep(split_many, 2, colSums(split_many), "/")
split_few_global_nb0 <- sapply(rev(names(levels_default_list)), function(c) with(all[all$country_name %in% levels_default_list[[c]],], wtd.mean(revenue_split_few_global > 0, weight)))
# dimnames(split_few) <- list(labels_vars[variables_split_few], names(levels_default_list))
# dimnames(split_many) <- list(labels_vars[variables_split_many], names(levels_default_list))

# TODO: bold for special_levels
# TODO: width & height in barres
barres(data = split_few, file = "../figures/country_comparison/split_few_bars", save = T, export_xls = T, color = color(9)[c(1,6:9)], sort = F, miss = F, legend = labels_vars[variables_split_few], labels = names(levels_default_list))
barres(data = split_few, file = "../figures/country_comparison/split_few_bars_nb0", add_means = split_few_global_nb0, name_mean = "Share allocating at least 5% to Global", save = T, export_xls = T, color = color(9)[c(1,6:9)], sort = F, miss = F, legend = labels_vars[variables_split_few], labels = names(levels_default_list))
barres(data = split_many, file = "../figures/country_comparison/split_many_bars", save = T, export_xls = T, color = color(19)[c(1:4,11:19)], sort = F, miss = F, legend = labels_vars[variables_split_many], labels = names(levels_default_list))

# 5a. ICS: mean of variant 
# TODO: fix labels
legend_ncs_gcs_ics <- c("Supports the National Climate Scheme", "Supports the Global Climate Scheme (GCS)", 
                        "Supports the GCS if its other members* cover 25-33% of world emissions<br>**Low**: Global South + EU",
                        "Supports the GCS if its other members* cover 56% of world emissions<br>**Mid**: Global South + China",
                        "Supports the GCS if its other members* cover 64-72% of world emissions<br>**High**: Global South + China + EU + various HICs (UK, Japan, South Korea, Canada...)",
                        "Supports the GCS if its other members* cover 64-72% of world emissions<br>**High color**: High + Distributive effects displayed using colors on world map")
legend_ncs_gcs_ics <- c("Supports the National Climate Scheme", "Supports the Global Climate Scheme (GCS)", 
                        "Supports the GCS if coverage is **Low**<br>Other members: Global South + EU<br>(25-33% of world emissions)",
                        "Supports the GCS if coverage is **Mid**<br>Global South + China<br>(56% of world emissions)",
                        "Supports the GCS if coverage is **High**<br>Global South + China + EU + various HICs<br>(UK, Japan, South Korea, Canada...; 64-72% of world emissions)",
                        "Supports the GCS if coverage is **High**, **color** variant<br>Global South + China + EU + various HICs<br>+ Distributive effects displayed using colors on world map")
plot_along("country_name", vars = variables_ncs_gcs_ics, levels_along = levels_default_list, labels = legend_ncs_gcs_ics, save = T, return_mean_ci = F, df = all, width = 1240, height = 480, origin = 50, plot_origin_line = T) 
# Up: 1240 x 480 / Down: 750 x 790
# plot_along("country_name", vars = variables_ncs_gcs_ics, levels_along = levels_default_list, labels = legend_ncs_gcs_ics, save = T, return_mean_ci = F, invert_y_along = T, legend_top = T, df = all, width = 780, height = 650, legend_vertical = T, origin = 50, plot_origin_line = T) 


# 5b. Wealth tax by coverage
# TODO weight (_control) for mean_ci
# TODO save mean_ci .xlsx
# TODO aesthetics: print axes
# TODO handle missing values in subsamples with levels_along as list
legend_wealth_tax <- c("**Global**:<br>Implemented by<br>All other countries", 
                       "**High-income**:<br>All other HICs and<br>not some MICs (such as China)",
                        "**International**:<br>Some countries (e.g. EU, UK, Brazil)<br>and not others (e.g. U.S., China)")
plot_along("country_name", vars = variables_wealth_tax_support, labels = legend_wealth_tax, levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = dev.size('px')[1], height = dev.size('px')[2], origin = 50, plot_origin_line = T) 
                       #  mean_ci = NULL, covariates = NULL, subsamples = NULL, conditions = c(" > 0"), invert_y_along = FALSE, factor_along = FALSE, outcomes = paste0(vars, conditions), 
                       # origin = 'others_at_mean', logit = c(FALSE), atmean = T, logit_margin = T, labels_along = levels_along, names_levels = paste0(along, levels_along), levels_along = Levels(df[[along]]),  # condition = "> 0", #country_heterogeneity = FALSE, along_labels,
                       # confidence = 0.95, weight = "weight", heterogeneity_condition = "", return_mean_ci = FALSE, print_name = FALSE, legend_top = FALSE, to_percent = FALSE, colors = NULL, color_RdBu = FALSE,
                       # legend_x = '', legend_y = '', plot_origin_line = FALSE, name = NULL, folder = '../figures/country_comparison/', order_y = NULL, order_along = NULL)
# Up: 870 x 380 / Down: 1300 x 650
# plot_along("country_name", vars = variables_wealth_tax_support, labels = legend_wealth_tax, levels_along = levels_default_list, save = T, return_mean_ci = F, invert_y_along = T, legend_top = T, df = all, width = dev.size('px')[1], height = dev.size('px')[2], origin = 50, plot_origin_line = T) 

# 6. conjoint: foreign aid + global tax
# TODO allow several colors
# TODO bold all/Europe
# TODO remove labels for one of them
plot_along(along = "millionaire_tax_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370, 
           covariates = "millionaire_tax_in_program", levels_subsamples = levels_default_list[-c(11)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

plot_along(along = "cut_aid_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370, 
           covariates = "cut_aid_in_program", levels_subsamples = levels_default_list[-c(11)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

# 7. Warm glow
plot_along(along = "variant_warm_glow", vars = "gcs_support", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all[all$variant_warm_glow != "NCS" & all$country != "SA",], width = 400, height = 370, 
           covariates = "variant_warm_glow", levels_subsamples = levels_default_list[-11], colors = "black", origin = 0, plot_origin_line = T, no_legend = T, condition = " > 0") 

plot_along(along = "info_solidarity", vars = "share_solidarity_supported", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all, width = 400, height = 370, 
           covariates = "info_solidarity", levels_subsamples = levels_default_list, colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

# 7bis: 2SLS
plot_along(along = "info_solidarity", vars = "likely_solidarity", condition = "> 0", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all, width = 400, height = 370, 
           covariates = "info_solidarity", levels_subsamples = levels_default_list, colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

# 8. Realistic policies 1336 x 737
heatmap_multiple(heatmaps_defs[c("solidarity_support")])


## Tables
# 2SLS
first_stage <- lm((likely_solidarity > 0) ~ info_solidarity, data = e, weights = weight)
iv_model <- ivreg(share_solidarity_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = weight)
# first_stage_f <- summary(iv_model, diagnostics = TRUE)$diagnostics["Weak instruments", "statistic"]
ols_model <- lm(share_solidarity_supported ~ (likely_solidarity > 0), data = e, weights = weight)
direct_effect <- lm(share_solidarity_supported ~ info_solidarity, data = e, weights = weight)
stargazer(first_stage, iv_model, ols_model, direct_effect,
          column.labels = c("IV 1st Stage", "IV 2nd Stage", "OLS", "Direct Effect"), model.names = FALSE, no.space = TRUE,
          keep.stat = c("n", "rsq", "f"), label = "tab:iv", dep.var.caption = "", #, "adj.rsq"), dep.var.caption = "Dependent variable:" ,
          dep.var.labels = c("\\makecell{Believes global\\\\redistr. likely}", "Share of plausible global policies supported"),
          covariate.labels = c("Information treatment", "Believes global redistribution likely", "(Intercept)"),
          type = "latex", style = "default", out = "../tables/iv.tex",
          title = "Effect on support for global redistribution of believing that it is likely.")  # add.lines = list(c("1st Stage F-statistic", round(first_stage_f, 2), "", "", ""))
