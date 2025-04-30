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
  "vote_agg" = "Vote (actual and hypothetical)",
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
  "donation_nation" = "Donation to own country",
  "donation_africa" = "Donation to Africa",
  "donation" = "Donation (any)",
  "global_tax_support" = "Global tax on millionaires funding low-income countries",
  "national_tax_support" = "National tax on millionaires funding public services",
  "global_tax_global_share" = "Preferred share of global tax for LICs (in %)",
  "global_tax_sharing" = "Sharing half of global tax with low-income countries",
  "foreign_aid_belief" = "Belief about foreign aid", # / public spending",
  "foreign_aid_actual" = "Actual foreign aid (in % of public spending)",
  "foreign_aid_preferred_no_info" = "Preferred foreign aid (no info)",
  "foreign_aid_preferred_info" = "Preferred foreign aid (with info)",
  "foreign_aid_preferred" = "Preferred foreign aid (any branch)",
  "foreign_aid_preferred_info_agg" = "Preferred foreign aid (with info)",
  "foreign_aid_preferred_no_info_agg" = "Preferred foreign aid (no info)",
  "foreign_aid_raise_how_defense" = "Lower spending on defense",
  "foreign_aid_raise_how_pensions" = "Lower spending on retirement pensions",
  "foreign_aid_raise_how_healthcare" = "Lower spending on healthcare",
  "foreign_aid_raise_how_welfare" = "Lower spending on welfare benefits",
  "foreign_aid_raise_how_education" = "Lower spending on education",
  "foreign_aid_raise_how_other" = "Lower spending on other programs",
  "foreign_aid_raise_how_wealthy" = "Higher taxes on the wealthiest",
  "foreign_aid_raise_how_corporations" = "Higher corporate income tax rate",
  "foreign_aid_raise_how_income_tax" = "Higher personal income tax rates",
  "foreign_aid_raise_how_deficit" = "Higher public deficit",
  "foreign_aid_reduce_how_defense" = "Higher spending on defense",
  "foreign_aid_reduce_how_pensions" = "Higher spending on retirement pensions",
  "foreign_aid_reduce_how_healthcare" = "Higher spending on healthcare",
  "foreign_aid_reduce_how_welfare" = "Higher spending on welfare benefits",
  "foreign_aid_reduce_how_education" = "Higher spending on education",
  "foreign_aid_reduce_how_other" = "Higher spending on other programs",
  "foreign_aid_reduce_how_wealthy" = "Lower taxes on the wealthiest",
  "foreign_aid_reduce_how_corporations" = "Lower corporate income tax rate",
  "foreign_aid_reduce_how_income_tax" = "Lower personal income tax rates",
  "foreign_aid_reduce_how_deficit" = "Lower public deficit",
  "foreign_aid_raise_support" = "[Country]'s foreign aid should be increased",
  "foreign_aid_raise_support_no_null" = "[Country]'s foreign aid should be increased",
  "foreign_aid_condition_human_rights" = "That recipient countries comply with climate targets and human rights",
  "foreign_aid_condition_fight_migration" = "That recipient countries cooperate to fight illegal migrations",
  "foreign_aid_condition_all_high_income" = "That other high-income countries also increase their foreign aid",
  "foreign_aid_condition_tax_rich" = "That this is financed by increased taxes on millionaires",
  "foreign_aid_condition_no_diversion" = "That we can be sure the aid reaches people in need and money is not diverted",
  "foreign_aid_condition_other_choice" = "Other",
  "foreign_aid_no_ineffective" = "Aid perpetuates poverty as it makes people feel less responsible for themselves",
  "foreign_aid_no_diversion" = "Aid is not effective as most of it is diverted",
  "foreign_aid_no_pressure" = "Aid is a pressure tactic for high-income countries that prevents low-income countries from developing freely",
  "foreign_aid_no_not_our_role" = "[Country] is not responsible for what happens in other countries",
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already a lot to do to support the [country] people in need",
  "foreign_aid_no_other_choice" = "Other",
  "petition_gcs" = "Petition for the GCS",
  "petition_nr" = "Petition for NR",
  "petition" = "Petition (any)",
  "negotiation_country_respecting" = "Favors [Country]'s interests restrained by global justice",
  "negotiation_global_before" = "Favors global justice, restrained or not by [Country]'s interests",
  "negotiation_only_country" = "Favors [Country]'s interests even against global justice",
  "donation_charities" = "Donation to charities",
  "interested_politics" = "Interested in politics",
  "group_defended" = "Group defended when voting",
  "group_defended_agg" = "Group defended when voting",
  "group_defended_agg2" = "Group defended when voting",
  "group_defended_agg5" = "Group defended when voting",
  "group_defended_agg6" = "Group defended when voting",
  "involvement_govt" = "Desired level of involvement of government",
  "political_affiliation" = "Political affiliation",
  "left_right" = "Left - right on economics",
  "vote_participation" = "Voted at last election",
  "voted" = "Voted at last election",
  "vote_fr_voters" = "Vote (voters)",
  "vote_fr_non_voters" = "Vote (non voters)",
  "vote_de_voters" = "Vote (voters)",
  "vote_de_non_voters" = "Vote (non voters)",
  "vote_es_voters" = "Vote (voters)",
  "vote_es_non_voters" = "Vote (non voters)",
  "vote_uk_voters" = "Vote (voters)",
  "vote_uk_non_voters" = "Vote (non voters)",
  "problem_inequality" = "Income inequality in [Country] is a problem",
  "problem_climate" = "Climate change is a problem",
  "problem_poverty" = "Global poverty is a problem",
  "points_econ1" = "econ1",
  "points_econ2" = "econ2: [Higher minimum wage] (DE: Bürgerversicherung)",
  "points_econ3" = "econ3",
  "points_econ4" = "econ4",
  "points_soc1" = "soc1",
  "points_soc2" = "soc2",
  "points_soc3" = "Making abortion a right at the federal level",
  "points_climate1" = "climate1",
  "points_climate2" = "climate2: Thermal insulation plan (US: also transport)",
  "points_climate3" = "climate3: Ban the sale of new combustion-engine cars by 2030",
  "points_tax1_nr" = "tax1: National redistribution scheme",
  "points_tax2_wealth_tax" = "tax2: Wealth tax (ES: raise tax on top incomes)",
  "points_tax3" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_tax3_corporate_tax" = "Increase corporate income tax<br>rate from 21% to 28%",
  "points_foreign1_gcs" = "foreign1: Global climate scheme",
  "points_foreign2_tax_rich" = "foreign2: Global tax on millionaires",
  "points_foreign3_assembly" = "foreign3: Global democratic assembly on climate change",
  "points_foreign4_aid" = "foreign4: Doubling foreign aid",
  "survey_biased" = "Survey biased", 
  "survey_biased_yes" = "Survey is biased", 
  "survey_biased_left" = "Survey is left-wing biased", 
  "survey_biased_right" = "Survey is right-wing biased", 
  "survey_biased_no" = "Survey is not biased", 
  "interview" = "Agrees for interview",
  "duration" = "Duration",
  "duration_agg" = "Duration",
  "duration_gcs" = "Duration: GCS comprehension",
  "duration_nr" = "Duration: NR comprehension",
  "duration_both" = "Duration: GCS+NR comprehension",
  # "duration_gcs" = "Duration: GCS questions",
  "duration_conjoint_a" = "Duration: conjoint (a)",
  "duration_conjoint_b" = "Duration: conjoint (b)",
  "duration_conjoint_c" = "Duration: conjoint (c)",
  "duration_conjoint_d" = "Duration: conjoint (d)",
  "duration_gcs_perception" = "Duration: G perceptions",
  "duration_other_policies" = "Duration: other policies",
  "duration_feedback" = "Duration: feedback",
  "duration_points" = "Duration: 100 points",
  "score_understood" = "Number of correct answers to understanding questions",
  "gcs_understood" = "With GCS, typical [country] people lose and poorest humans win",
  "nr_understood" = "With NR, typical [country] people win and richest lose",
  "both_understood" = "With GCS+NR, typical [country] people neither win nor lose",
  "share_policies_supported" = "Share of policies supported",
  "dropout" = "Dropped out",
  "petition_matches_support" = "Petition and support answers match",
  "conjoint_a_matches_support" = "Conjoint (a) and support answers match",
  "nationalist" = "Nationalist",
  "universalist" = "Universalist",
  "individualist" = "Individualist",
  "egoistic" = "Individualist",
  "woman" = "Gender: Woman",
  "man" = "Gender: Man",
  "ets2_equal_cash_support" = "ETS2 with equal cash transfer (105€/year for each European)",
  "ets2_country_cash_support" = "ETS2 with cash transfer in proportion to country's emissions",
  "ets2_investments_support" = "ETS2 with low-carbon investments",
  "ets2_vulnerable_investments_support" = "ETS2 with transfers to vulnerable and low-carbon investments",
  "ets2_no_european" = "Policies should be at national level", # Does not support ETS2 because<br>
  "ets2_no_revenue_use" = "Would prefer other revenue use",
  "ets2_no_pricing" = "Opposes carbon pricing",
  "ets2_no_climate_action" = "Opposes more climate action",
  "ets2_no_understanding" = "Does not understand",
  "ets2_no_dont_know" = "Does not know",
  "ets2_oppose" = "Does not support any of the ETS2 variants",
  "global_tax_more_half" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 50%",
  "global_tax_more_30p" = "Preferred share of global wealth tax for low-income countries: ≥ 30%",
  "global_tax_more_10p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 10%",
  "branch_list_exp_g" = "List contains: GCS",
  "branch_list_exp_r" = "List contains: NR",
  "branch_list_exp_g:branch_list_exp_r" = "List contains: GCS $\\times$ NR",
  "branch_list_exp_gTRUE:branch_list_exp_rTRUE" = "List contains: GCS $\\times$ NR",
  "branch_list_exp" = "Branch of list experiment",
  "branch_c_gcs" = "GCS in Progressive platform",
  "branch_petition" = "Branch of petition",
  "branch_donationOwn nation" = "Poor is in own country",
  "branch_donationOwn nation:vote3_factorAbstention/PNR/Other" = "Poor is in own country $\\times$ Vote: Abstention/PNR/Other",
  "branch_donationOwn nation:vote3_factorTrump" = "Poor is in own country $\\times$ Vote: Trump",
  "branch_donationOwn nation:vote_BidenTRUE" = "Poor is in own country $\\times$ Vote: Biden",
  "branch_donationOwn nation:vote_not_BidenTRUE" = "Poor is in own country $\\times$ Vote: \\textit{not} Biden",
  "vote_factorPNR/Non-voter" = "Vote: PNR/Non-voter",
  "branch_donationOwn nation:vote_factorCenter-right or Right" = "Poor in own country $\\times$ Vote: Center-right or Right",
  "branch_donationOwn nation:vote_factorLeft" = "Poor in own country $\\times$ Vote: Left",
  "branch_donationOwn nation:vote_factorPNR/Non-voter" = "Poor in own country $\\times$ Vote: PNR/Non-voter",
  "branch_donationOwn nation:vote_factorFar right" = "Poor in own country $\\times$ Vote: Far right",
  "branch_gcs" = "Treatment",
  "branch_gcsfield" = "Treatment: Open-ended field on GCS pros & cons",
  "branch_gcsimportant" = "Treatment: Closed questions on GCS pros & cons", 
  "branch_gcsinfo" = "Treatment: Info on actual support for GCS and NR",
  "waveUS1" = "Wave: \\textit{US1}",
  "waveUS2" = "Wave: \\textit{US2}",
  "(Intercept)" = "Constant"
)
for (v in c(variables_gcs_field_names, variables_poverty_field_names, "gcs_field_empty", "poverty_field_empty")) labels_vars[v] <-sub("dont", "don't", gsub("_", " ", gsub(".*_field_", "", v)))
for (v in c(variables_gcs_field_contains, variables_poverty_field_contains)) labels_vars[v] <-  paste0(gsub(".*_", "", v), ": ", gsub(".*contains: ", "", Label(all[[v]])))
for (v in c(variables_donation, variables_points, variables_belief, variables_foreign_aid_amount, "share_policies_supported")) labels_vars[paste0(v, "_agg")] <- labels_vars[v]
for (v in intersect(names(all), names(labels_vars))) { # intersect(c(socio_demos, socio_demos_us), names(all)), 
  if (grepl("TRUE / FALSE", Levels(all[[v]])[1])) labels_vars[paste0(v, "TRUE")] <- labels_vars[v]
  else for (l in setdiff(Levels(all[[v]]), NA)) {
    if (!paste0(v, l) %in% names(labels_vars)) labels_vars[paste0(v, l)] <- paste0(labels_vars[v], ": ", l)
  }
}


##### labels_vars_short_html #####
labels_vars_short_html <- c(
  "score_understood" = "Number of correct answers<br>to understanding questions",
  "gcs_understood" = "With GCS,<br>typical [country] people lose and poorest humans win",
  "nr_understood" = "With NR,<br>typical [country] people win and richest lose",
  "both_understood" = "With GCS+NR,<br>typical [country] people neither win nor lose",
  "list_exp_gl" = "GCS/C/O",
  "list_exp_rgl" = "NR/GCS/C/O", 
  "list_exp_l" = "C/O",
  "list_exp_rl" = "NR/C/O",   
  "global_tax_more_30p" = "Preferred share of global wealth tax<br>for low-income countries: ≥ 30%",
  "conjoint_crg_cr_binary" = "<b>C+NR+GCS</b> vs. C+NR",
  "conjoint_cr_gr_binary" = "C+NR vs. <b>GCS+NR</b>",
  "conjoint_r_rcg_binary" = "NR vs. <b>NR+C+GCS</b>",
  "conjoint_rg_r_binary" = "<b>NR+GCS</b> vs. NR",
  "conjoint_rc_r_binary" = "<b>NR+C</b> vs. NR",
  "conjoint_left_right_binary" = "<b>Left</b> vs. Right",
  "conjoint_leftg_right_binary" = "<b>Left+GCS</b> vs. Right",
  "conjoint_left_a_b_binary" = "Random program <b>A</b> vs. B",
  "conjoint_left_ag_b_binary" = "Random program <b>A+GCS</b> vs. B",
  "conjoint_r" = "Random program <b>with GCS</b> vs. without",
  "foreign_aid_raise_support" = "Should [Country]'s foreign aid be increased?",
  "foreign_aid_raise_support_no_null" = "Should [Country]'s foreign aid be increased?",
  "problem_inequality" = "Income inequality in [Country]",
  "problem_climate" = "Climate change",
  "problem_poverty" = "Global poverty",
  "gcs_important_limit_CC" = "It would succeed in limiting climate change",
  "gcs_important_hurt_economy" = "It would hurt the [Country] economy",
  "gcs_important_hurt_me" = "It would penalize my household",
  "gcs_important_change_lifestyles" = "It would make people change their lifestyle",
  "gcs_important_reduce_poverty" = "It would reduce poverty in low-income countries",
  "gcs_important_hurt_poor" = "It might be detrimental to some poor countries",
  "gcs_important_foster_cooperation" = "It could foster global cooperation",
  "gcs_important_fuel_corruption" = "It could fuel corruption<br>in low-income countries",
  "gcs_important_fuel_fraud" = "It could be subject to fraud",
  "gcs_important_difficult_enact" = "It would be technically difficult<br>to put in place",
  "gcs_important_having_info" = "Having enough information<br>on this scheme and its consequences",
  "negotiation" = "What [Country] should defend in climate negotiations",
  "climate_compensation_support" = "Payments from high-income countries to compensate<br>low-income countries for climate damages",
  "climate_mitigation_support" = "High-income countries funding renewable<br>energy in low-income countries",
  "climate_adaptation_support" = "High-income countries contributing $100 billion per year<br>to help low-income countries adapt to climate change",
  "debt_cancellation_support" = "Cancellation of low-income countries' public debt",
  "democratise_un_imf_support" = "Democratise international institutions (UN, IMF) by making<br>a country's voting right proportional to its population",
  "remove_tariffs_support" = "Removing tariffs on imports from low-income countries",
  "global_min_wage_support" = "A minimum wage in all countries<br>at 50% of local median wage",
  "global_register_support" = "Fight tax evasion by creating a global financial register<br>to record ownership of all assets",
  "cap_wealth_support" = "A maximum wealth limit of<br>$10 billion (US) / €100 million (EU)",
  "foreign_aid_condition_human_rights" = "That recipient countries comply<br>with climate targets and human rights",
  "foreign_aid_condition_fight_migration" = "That recipient countries cooperate<br>to fight illegal migrations",
  "foreign_aid_condition_all_high_income" = "That other high-income countries<br>also increase their foreign aid",
  "foreign_aid_condition_tax_rich" = "That this is financed by<br>increased taxes on millionaires",
  "foreign_aid_condition_no_diversion" = "That we can be sure the aid reaches<br>people in need and money is not diverted",
  "foreign_aid_condition_other_choice" = "Other",
  "foreign_aid_no_ineffective" = "Aid perpetuates poverty as it makes<br>people feel less responsible for themselves",
  "foreign_aid_no_diversion" = "Aid is not effective<br>as most of it is diverted",
  "foreign_aid_no_pressure" = "Aid is a pressure tactic for high-income countries that<br>prevents low-income countries from developing freely",
  "foreign_aid_no_not_our_role" = "[Country] is not responsible<br>for what happens in other countries",
  "foreign_aid_no_nation_first" = "Charity begins at home: there is already<br>a lot to do to support the [country] people in need",
  "ets2_equal_cash_support" = "ETS2 with equal cash transfer<br>(105€/year for each European)",
  "ets2_country_cash_support" = "ETS2 with cash transfer<br>in proportion to country's emissions",
  "ets2_investments_support" = "ETS2 with low-carbon investments",
  "ets2_vulnerable_investments_support" = "ETS2 with transfers to vulnerable<br>and low-carbon investments",
  "ets2_no_european" = "Policies should be at national level",
  "ets2_no_revenue_use" = "Would prefer other revenue use",
  "ets2_no_pricing" = "Opposes carbon pricing",
  "ets2_no_climate_action" = "Opposes more climate action",
  "ets2_no_understanding" = "Does not understand",
  "ets2_no_dont_know" = "Does not know"
)

##### labels_vars_country #####
labels_vars_country <- list() #"US" = c(), "DE" = c(), "FR" = c(), "ES" = c(), "UK" = c())
for (c in countries) {
  names_policies_names <- paste0("points_", row.names(policies.names))
  for (v in intersect(c(variables_points), names_policies_names)) {
    labels_vars_country[[c]][v] <- policies.names[sub("points_", "", v), c]
    labels_vars_country[[c]][paste0(v, "_agg")] <- policies.names[sub("points_", "", v), c] # common ones in English
  }
  for (v in setdiff(names_policies_names[grepl("[0-9]", names_policies_names)], variables_points)) { # common ones in local language
    var <- variables_points[grepl(v, variables_points)]
    if (length(var) == 1) labels_vars_country[[c]][paste0(var, "_agg")] <- policies.names[sub("points_", "", v), c]
  }
}
labels_vars_country$US["foreign_aid_no_nation_first"] <- "Charity begins at home: there is already a lot to do to support the American people in need"
labels_vars_country$FR["gcs_support"] <- "Plan mondial pour le climat"
labels_vars_country$FR["conjoint_left_ag_b_binary"] <- "Programme aléatoire : A+Plan préféré à B"
labels_vars_country$FR["global_tax_global_share"] <- "Part préférée de l'ISF mondial pour les pays pauvres (en %)"
labels_vars_country$FR["climate_compensation_support"] <- "Paiements des pays du Nord indemnisant les pays du Sud pour les dégâts causés par le changement climatique"
labels_vars_country$FR["climate_mitigation_support"] <- "Paiements des pays du Nord des énergies renouvelables dans les pays du Sud"
labels_vars_country$FR["climate_adaptation_support"] <- "Paiements des pays du Nord de 100 milliards $/an pour aider\nles pays du Sud à s'adapter au changement climatique"
labels_vars_country$FR["debt_cancellation_support"] <- "Annulation de la dette publique des pays à faibles revenus"
labels_vars_country$FR["remove_tariffs_support"] <- "Suppression des droits de douane sur les importations en provenance des pays pauvres"
labels_vars_country$FR["democratise_un_imf_support"] <- "Démocratisation des institutions internationales (ONU, FMI):\ndroit de vote d'un pays proportionnel à sa population"
labels_vars_country$FR["global_min_wage_support"] <- "Instauration d'un salaire minimum dans tous les pays égal à 50% du salaire médian local"
labels_vars_country$FR["global_register_support"] <- "Lutte contre l'évasion fiscale avec un registre mondial répertoriant qui possède quoi"
labels_vars_country$FR["cap_wealth_support"] <- "Limite maximale de patrimoine à 100 millions € (Eu) /\n 10 milliards $ (U.S.)  pour chaque humain"
labels_vars_country$FR["national_tax_support"] <- "Taxe nationale sur les millionnaires finançant les services publics"
labels_vars_country$FR["global_tax_support"] <- "Taxe mondiale sur les millionnaires finançant les pays à faible revenu"


##### Labels vars #####
labels_vars <- c(
  # "maritime_split_ldc" = "Maritime levy revenue preferred allocation to:\nSustainable transition in LDCs",
  # "maritime_split_companies" = "Maritime levy revenue preferred allocation to:\nShipping companies to mitigate price increases",
  # "maritime_split_decarbonization" = "Maritime levy revenue preferred allocation to:\nRD&D of zero-emission fuels and ships",
  "maritime_split_ldc" =       "Sustainable transition in LDCs",
  "maritime_split_companies" = "Shipping companies to mitigate price increases",
  "maritime_split_decarbonization" = "RD&D of zero-emission fuels and ships",
  "solidarity_support_shipping_levy" = "International levy on carbon emissions from shipping,\nfinancing countries' budgets in proportion to their population", # "Global maritime fuel levy with equal pc revenue sharing", # 
  setNames(names(p), names(p))) # TODO!
labels_vars_country <- list()
for (c in pilot_countries) labels_vars_country[[c]] <- labels_vars


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
