# e <- read_csv('../data_raw/IT.csv')
# e <- fetch_survey(survey_list$id[survey_list$name == "FR_survey"], include_display_order = T, verbose = T, convert = F, col_types = cols("m" = col_character()))
# for (i in 1:length(e)) {
#   # label(e[[i]]) <- paste(names(e)[i], ": ", label(e[[i]]), e[[i]][1], sep="") #
#   # print(paste(i, label(e[[i]])))
#   print(names(e)[i])
# }

rename_survey <- function(e, pilot = FALSE) {
  if (pilot) {
    names(e) <- c(
      "date",
      "date_end",
      "status_response", # "ip",
      "progress",
      "duration",
      "finished",
      "date_recorded", #"ID_qualtrics",
      "name",
      "firstname",
      "mmail",
      "id",
      "latitude",
      "longitude",
      "distr",
      "lang", #"consent",
      "Q75_First Click",
      "Q75_Last Click",
      "Q75_Page Submit", 
      "Q75_Click Count",
      "gender",
      "hidden_country",
      "age_exact",
      "foreign",
      "couple",
      "hh_size",
      "Nb_children__14",
      "race_white",
      "race_black",
      "race_hispanic",
      "race_asian",
      "race_native",
      "race_hawaii",
      "race_other",
      "race_pnr",
      "income",
      "education",
      "employment_status",
      "zipcode",
      "home_tenant",
      "home_owner",
      "home_landlord",
      "home_hosted",
      "millionaire",
      "voted",
      "vote_US",
      "vote_GB",
      "vote_FR",
      "vote_CH",
      "vote_PL",
      "concerns_field",
      "wish_field",
      "issue_field",
      "injustice_field",
      "conjoint",
      "revenue_split_few_domestic_education_healthcare",
      "revenue_split_few_domestic_welfare",
      "revenue_split_few_domestic_tax_reduction",
      "revenue_split_few_domestic_deficit_reduction",
      "revenue_split_few_global",
      "revenue_split_few_order_domestic_education_healthcare",
      "revenue_split_few_order_domestic_welfare",
      "revenue_split_few_order_domestic_tax_reduction",
      "revenue_split_few_order_domestic_deficit_reduction",
      "revenue_split_few_order_global",
      "revenue_split_many_domestic_education",
      "revenue_split_many_domestic_healthcare",
      "revenue_split_many_domestic_defense",
      "revenue_split_many_domestic_deficit_reduction",
      "revenue_split_many_domestic_justice_police",
      "revenue_split_many_domestic_pensions",
      "revenue_split_many_domestic_welfare",
      "revenue_split_many_domestic_infrastructure",
      "revenue_split_many_domestic_tax_reduction",
      "revenue_split_many_global_education_healthcare",
      "revenue_split_many_global_renewables_adaptation",
      "revenue_split_many_global_loss_damage",
      "revenue_split_many_global_forestation",
      "revenue_split_many_order_domestic_education",
      "revenue_split_many_order_domestic_healthcare",
      "revenue_split_many_order_domestic_defense",
      "revenue_split_many_order_domestic_deficit_reduction",
      "revenue_split_many_order_domestic_justice_police",
      "revenue_split_many_order_domestic_pensions",
      "revenue_split_many_order_domestic_welfre",
      "revenue_split_many_order_domestic_infrastructure",
      "revenue_split_many_order_domestic_tax_reduction",
      "revenue_split_many_order_global_education_healthcare",
      "revenue_split_many_order_global_renewables_adaptation",
      "revenue_split_many_order_global_loss_damage",
      "revenue_split_many_order_global_forestation",
      "hidden_sum",
      "ncs_support",
      "donation",
      "gcs_support",
      "gcs_belief_us",
      "ics_support",
      "likely_solidarity",
      "solidarity_support_billionaire_tax",
      "solidarity_support_corporate_tax",
      "solidarity_support_expanding_security_council",
      "solidarity_support_foreign_aid",
      "solidarity_support_debt_relief",
      "solidarity_support_bridgetown",
      "solidarity_support_loss_damage",
      "solidarity_support_ncqg_300bn",
      "solidarity_support_shipping_levy",
      "solidarity_support_aviation_levy",
      "solidarity_support_billionaire_tax_short",
      "solidarity_support_corporate_tax_short",
      "solidarity_support_expanding_security_council_short",
      "solidarity_support_foreign_aid_short",
      "solidarity_support_bridgetown_short",
      "global_tax_support",
      "hic_tax_support",
      "intl_tax_support",
      "maritime_split_ldc",
      "maritime_split_companies",
      "maritime_split_decarbonization",
      "maritime_split_order_ldc",
      "maritime_split_order_companies",
      "maritime_split_order_decarbonization",
      "ncqg_full",
      "ncqg",
      "ncqg_order", 
      "ncqg_order_4",
      "ncqg_order_5",
      "ncqg_order_2",
      "ncqg_order_3",
      "ncqg_order_6",
      "ncqg_order_7",
      "sustainable_future_a",
      "sustainable_future_s",
      "sustainable_future_b",
      "top1_tax_support",
      "top1_tax_support_cut",
      "top3_tax_support",
      "top3_tax_support_cut",
      "attention_test",
      "transfer_how_agencies",
      "transfer_how_govt_conditional",
      "transfer_how_govt_unconditional",
      "transfer_how_local_authorities",
      "transfer_how_ngo",
      "transfer_how_social_protection",
      "transfer_how_cash_unconditional",
      "transfer_how_order_agencies",
      "transfer_how_order_govt_conditional",
      "transfer_how_order_govt_unconditional",
      "transfer_how_order_local_authorities",
      "transfer_how_order_ngo",
      "transfer_how_order_social_protection",
      "transfer_how_order_cash_unconditional",
      "global_movement_no",
      "global_movement_spread",
      "global_movement_demonstrate",
      "global_movement_strike",
      "global_movement_donate",
      "vote_intl_coalition",
      "vote_intl_coalition_order_more_likely",
      "vote_intl_coalition_order_not_depend",
      "vote_intl_coalition_order_less_likely",
      "why_hic_help_lic_responsibility",
      "why_hic_help_lic_interest",
      "why_hic_help_lic_duty",
      "why_hic_help_lic_none",
      "why_hic_help_lic_order_responsibility",
      "why_hic_help_lic_order_interest",
      "why_hic_help_lic_order_duty",
      "why_hic_help_lic_order_none",
      "reparations_support",
      "income_exact",
      "Q94_First Click",
      "Q94_Last Click",
      "Q94_Page Submit", # TODO
      "Q94_Click Count",
      "custom_redistr_satisfied",
      "custom_redistr_skip",
      "well_being_gallup_0",
      "well_being_gallup_1",
      "well_being_wvs_0",
      "well_being_wvs_1",
      "gcs_comprehension",
      "gcs_comprehension_order", 
      "gcs_comprehension_order_3",
      "gcs_comprehension_order_5",
      "my_tax_global_nation",
      "group_defended",
      "donation_charities",
      "interested_politics",
      "involvement_govt",
      "Q93_First Click",
      "Q93_Last Click",
      "Q93_Page Submit", # TODO
      "Q93_Click Count",
      "survey_biased",
      "comment_field",
      "interview",
      "custom_redistr_winners",
      "custom_redistr_losers",
      "custom_redistr_degree",
      "complete",
      "quotasfull",
      "screenout",
      "speeder",
      "Q_TotalDuration", 
      "excluded", 
      "country",
      "region",
      "urbanity",
      "cut",
      "long",
      "variant_split",
      "variant_warm_glow",
      "variant_realism",
      "variant_ncqg_maritime",
      "variant_radical_redistr",
      "variant_ics",
      "variant_sliders",
      "variant_radical_transfer",
      "variant_synthetic",
      "variant_comprehension",
      "variant_belief",
      "F-1-1",
      "F-1-2",
      "F-1-3",
      "F-1-4",
      "F-1-5",
      "F-1-6",
      "F-1-1-1",
      "F-1-1-2",
      "F-1-1-3",
      "F-1-1-4",
      "F-1-1-5",
      "F-1-1-6",
      "F-1-2-1",
      "F-1-2-2",
      "F-1-2-3",
      "F-1-2-4",
      "F-1-2-5",
      "F-1-2-6",
      "language", # TODO?
      "time_start",
      "duration_consent",
      "time_consent",
      "duration_socios_demos",
      "time_socios_demos",
      "duration_field",
      "time_field",
      "duration_conjoint",
      "time_conjoint",
      "duration_global_tax",
      "time_global_tax",
      "duration_warm_glow_substitute",
      "time_warm_glow_substitute",
      "duration_gcs",
      "time_gcs",
      "duration_ics",
      "time_ics",
      "duration_warm_glow_realism",
      "time_warm_glow_realism",
      "duration_ncqg_maritime",
      "time_ncqg_maritime",
      "duration_wealth_tax",
      "time_wealth_tax",
      "duration_preferred_transfer_mean",
      "time_preferred_transfer_mean",
      "duration_radical_redistr",
      "time_radical_redistr",
      "duration_custom_redistr",
      "time_custom_redistr",
      "duration_well_being",
      "time_well_being",
      "duration_extra",
      "time_extra",
      "duration_main_questions",
      "Open-endedfield_order_issue_field",
      "Open-endedfield_order_injustice_field",
      "Open-endedfield_order_wish_field",
      "Open-endedfield_order_concerns_field",
      "CapSharenon-universal_order_gcs_mid",
      "CapSharenon-universal_order_gcs_high",
      "CapSharenon-universal_order_ics_support",
      "CapSharenon-universal_order_gcs_low",
      "CapSharenon-universal_order_gcs_high_color",
      "Wealthtaxdependingonsetsofcountries_order_hic_tax_support",
      "Wealthtaxdependingonsetsofcountries_order_global_tax_support",
      "Wealthtaxdependingonsetsofcountries_order_intl_tax_support",
      "Well-being_order_well_being_gallup_1",
      "Well-being_order_well_being_wvs_1",
      "Well-being_order_well_being_wvs_0",
      "Well-being_order_well_being_gallup_0"
      # "fast",
      # "valid",
      # "legit",
      # "dropout",
      # "stayed",
      # "final"
    ) 
  } else {
    names(e) <- c(  
      "date", 
      "date_end",
      "status_response", # "ip",
      "progress",
      "duration",
      "finished",
      "date_recorded", #"ID_qualtrics",
      "name",
      "firstname",
      "mmail",
      "id",
      "latitude",
      "longitude",
      "distr",
      "lang", #"consent",
      "Q75_First Click",
      "Q75_Last Click",
      "Q75_Page Submit", 
      "Q75_Click Count",
      "gender",
      "hidden_country",
      "age_exact",
      "foreign",
      "couple",
      "hh_size",
      "Nb_children__14",
      "race_white",
      "race_black",
      "race_hispanic",
      "race_asian",
      "race_native",
      "race_hawaii",
      "race_other",
      "race_pnr",
      "income",
      "education",
      "employment_status",
      "zipcode",
      "home_tenant",
      "home_owner",
      "home_landlord",
      "home_hosted",
      "millionaire",
      "voted",
      "nationality_SA",
      "vote_US",
      "vote_GB",
      "vote_FR",
      "vote_CH",
      "vote_PL",
      "vote_IT",
      "vote_ES",
      "vote_DE",
      "vote_JP",
      "concerns_field",
      "wish_field",
      "issue_field",
      "injustice_field",
      "conjoint",
      "revenue_split_few_domestic_education_healthcare",
      "revenue_split_few_domestic_welfare",
      "revenue_split_few_domestic_tax_reduction",
      "revenue_split_few_domestic_deficit_reduction",
      "revenue_split_few_global",
      "revenue_split_few_order_domestic_education_healthcare",
      "revenue_split_few_order_domestic_welfare",
      "revenue_split_few_order_domestic_tax_reduction",
      "revenue_split_few_order_domestic_deficit_reduction",
      "revenue_split_few_order_global",
      "revenue_split_many_domestic_education",
      "revenue_split_many_domestic_healthcare",
      "revenue_split_many_domestic_defense",
      "revenue_split_many_domestic_deficit_reduction",
      "revenue_split_many_domestic_justice_police",
      "revenue_split_many_domestic_pensions",
      "revenue_split_many_domestic_welfare",
      "revenue_split_many_domestic_infrastructure",
      "revenue_split_many_domestic_tax_reduction",
      "revenue_split_many_global_education_healthcare",
      "revenue_split_many_global_renewables_adaptation",
      "revenue_split_many_global_loss_damage",
      "revenue_split_many_global_forestation",
      "revenue_split_many_order_domestic_education",
      "revenue_split_many_order_domestic_healthcare",
      "revenue_split_many_order_domestic_defense",
      "revenue_split_many_order_domestic_deficit_reduction",
      "revenue_split_many_order_domestic_justice_police",
      "revenue_split_many_order_domestic_pensions",
      "revenue_split_many_order_domestic_welfre",
      "revenue_split_many_order_domestic_infrastructure",
      "revenue_split_many_order_domestic_tax_reduction",
      "revenue_split_many_order_global_education_healthcare",
      "revenue_split_many_order_global_renewables_adaptation",
      "revenue_split_many_order_global_loss_damage",
      "revenue_split_many_order_global_forestation",
      "hidden_sum",
      "ncs_support",
      "donation",
      "gcs_support",
      "gcs_belief_us",
      "gcs_belief_own", # TODO
      "ics_support",
      "likely_solidarity",
      "solidarity_support_billionaire_tax",
      "solidarity_support_corporate_tax",
      "solidarity_support_expanding_security_council",
      "solidarity_support_foreign_aid",
      "solidarity_support_debt_relief",
      "solidarity_support_bridgetown",
      "solidarity_support_loss_damage",
      "solidarity_support_ncqg_300bn",
      "solidarity_support_shipping_levy",
      "solidarity_support_aviation_levy",
      "solidarity_support_order_billionaire_tax",
      "solidarity_support_order_corporate_tax",
      "solidarity_support_order_expanding_security_council",
      "solidarity_support_order_foreign_aid",
      "solidarity_support_order_debt_relief",
      "solidarity_support_order_bridgetown",
      "solidarity_support_order_loss_damage",
      "solidarity_support_order_ncqg_300bn",
      "solidarity_support_order_shipping_levy",
      "solidarity_support_order_aviation_levy",
      "maritime_split_ldc",
      "maritime_split_companies",
      "maritime_split_decarbonization",
      "maritime_split_order_ldc",
      "maritime_split_order_companies",
      "maritime_split_order_decarbonization",
      "ncqg_full",
      "ncqg",
      "ncqg_order", 
      "ncqg_order_4",
      "ncqg_order_5",
      "ncqg_order_2",
      "ncqg_order_3",
      "ncqg_order_6",
      "ncqg_order_7",
      "global_tax_support",
      "hic_tax_support",
      "intl_tax_support",
      "sustainable_future_a",
      "sustainable_future_b",
      "top1_tax_support",
      "top3_tax_support",
      "attention_test",
      "transfer_how_agencies",
      "transfer_how_govt_conditional",
      "transfer_how_govt_unconditional",
      "transfer_how_local_authorities",
      "transfer_how_ngo",
      "transfer_how_social_protection",
      "transfer_how_cash_unconditional",
      "transfer_how_order_agencies",
      "transfer_how_order_govt_conditional",
      "transfer_how_order_govt_unconditional",
      "transfer_how_order_local_authorities",
      "transfer_how_order_ngo",
      "transfer_how_order_social_protection",
      "transfer_how_order_cash_unconditional",
      "convergence_support",
      "global_movement_no",
      "global_movement_spread",
      "global_movement_demonstrate",
      "global_movement_strike",
      "global_movement_donate",
      "vote_intl_coalition",
      "vote_intl_coalition_order_more_likely",
      "vote_intl_coalition_order_not_depend",
      "vote_intl_coalition_order_less_likely",
      "why_hic_help_lic_responsibility",
      "why_hic_help_lic_interest",
      "why_hic_help_lic_duty",
      "why_hic_help_lic_none",
      "why_hic_help_lic_order_responsibility",
      "why_hic_help_lic_order_interest",
      "why_hic_help_lic_order_duty",
      "why_hic_help_lic_order_none",
      "reparations_support",
      "income_exact",
      "Q94_First Click",
      "Q94_Last Click",
      "Q94_Page Submit", # TODO
      "Q94_Click Count",
      "custom_redistr_satisfied",
      "custom_redistr_skip",
      "well_being_gallup_0",
      "well_being_gallup_1",
      "well_being_wvs_0",
      "well_being_wvs_1",
      "gcs_comprehension",
      "gcs_comprehension_order", 
      "gcs_comprehension_order_3",
      "gcs_comprehension_order_5",
      "my_tax_global_nation",
      "group_defended",
      "donation_charities",
      "interested_politics",
      "involvement_govt",
      "Q93_First Click",
      "Q93_Last Click",
      "Q93_Page Submit", # TODO
      "Q93_Click Count",
      "survey_biased",
      "comment_field",
      "interview",
      "custom_redistr_winners",
      "custom_redistr_losers",
      "custom_redistr_degree",
      "complete",
      "quotasfull",
      "screenout",
      "speeder",
      "Q_TotalDuration", 
      "excluded", 
      "country",
      "region",
      "urbanity",
      "cut",
      "long",
      "variant_split",
      "variant_warm_glow",
      "variant_realism",
      "variant_ncqg_maritime",
      "variant_radical_redistr",
      "variant_ics",
      "variant_sliders",
      "variant_radical_transfer",
      "variant_synthetic",
      "variant_comprehension",
      "variant_belief",
      "F-1-1",
      "F-1-2",
      "F-1-3",
      "F-1-4",
      "F-1-5",
      "F-1-6",
      "F-1-1-1",
      "F-1-1-2",
      "F-1-1-3",
      "F-1-1-4",
      "F-1-1-5",
      "F-1-1-6",
      "F-1-2-1",
      "F-1-2-2",
      "F-1-2-3",
      "F-1-2-4",
      "F-1-2-5",
      "F-1-2-6",
      "language", # TODO?
      "time_start",
      "duration_consent",
      "time_consent",
      "duration_socios_demos",
      "time_socios_demos",
      "duration_field",
      "time_field",
      "duration_conjoint",
      "time_conjoint",
      "duration_global_tax",
      "time_global_tax",
      "duration_warm_glow_substitute",
      "time_warm_glow_substitute",
      "duration_gcs",
      "time_gcs",
      "duration_ics",
      "time_ics",
      "duration_warm_glow_realism",
      "time_warm_glow_realism",
      "duration_ncqg_maritime",
      "time_ncqg_maritime",
      "duration_wealth_tax",
      "time_wealth_tax",
      "duration_preferred_transfer_mean",
      "time_preferred_transfer_mean",
      "duration_radical_redistr",
      "time_radical_redistr",
      "duration_custom_redistr",
      "time_custom_redistr",
      "duration_scenarios_tax",
      "time_scenarios_tax", # TODO
      "duration_end",
      "time_end",
      "duration_main_questions",
      "Open-endedfield_order_issue_field",
      "Open-endedfield_order_injustice_field",
      "Open-endedfield_order_wish_field",
      "Open-endedfield_order_concerns_field",
      "CapSharenon-universal_order_gcs_mid",
      "CapSharenon-universal_order_gcs_high",
      "CapSharenon-universal_order_ics_support",
      "CapSharenon-universal_order_gcs_low",
      "CapSharenon-universal_order_gcs_high_color",
      "Wealthtaxdependingonsetsofcountries_order_hic_tax_support",
      "Wealthtaxdependingonsetsofcountries_order_global_tax_support",
      "Wealthtaxdependingonsetsofcountries_order_intl_tax_support",
      "Well-being_order_well_being_gallup_1",
      "Well-being_order_well_being_wvs_1",
      "Well-being_order_well_being_wvs_0",
      "Well-being_order_well_being_gallup_0"
      # "fast",
      # "valid",
      # "legit",
      # "dropout",
      # "stayed",
      # "final"
    ) 
  }
  return(e)  
}

