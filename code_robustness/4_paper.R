# 2-3h to run everything

##### Introduction #####
# At least 70%: country_comparison/solidarity_support_share
# 64% support: country_comparison/radical_redistr_all_share
# Custom redistribution:  
decrit("custom_redistr_self_lose") # 46%
decrit("custom_redistr_self_gain") # 9%
decrit(all$revenue_split_few_global > 0) # 87%
decrit(all$revenue_split_few_global) # 17.5%
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight))
decrit("vote_intl_coalition") # 36.47% vs. 17%
decrit("global_movement_support") # 68%
decrit("global_movement_support", which = all$millionaire == 5) # 52%
decrit("global_movement_support", which = all$millionaire == 5, weight = F) # 561 millionaires
decrit("global_tax_support") # 74%
decrit("intl_tax_support") # 68%
decrit("ics_high_support") # 68.49%
decrit("ics_low_support") # 65%
# Pluralistic ignorance: country_comparison/ncs_gcs_ics_all_control_features_median_belief_various
round(sapply(c("all", countries), function(c) wtd.median(d(c)$gcs_belief_own, weight = d(c)$weight, na.rm = T) - wtd.mean(d(c)$gcs_support, d(c)$weight))) # -16 pp
round(wtd.median(all$gcs_belief_us, weight = all$weight * (all$country != "US"), na.rm = T) - wtd.mean(US$gcs_support, US$weight)) # -22 pp
# Warm glow:
summary(lm((likely_solidarity > 0) ~ info_solidarity, data = e, weights = weight)) # +8pp*** + 33%
summary(lm(share_solidarity_supported ~ info_solidarity, data = e, weights = weight)) # +1pp. + 33% (p: .09)
summary(ivreg(share_solidarity_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = weight)) # +14pp. (p: .08)
iv_model <- ivreg(share_solidarity_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = weight)
# Why help countries in need: country_comparison/why_hic_help_lic_positive
# Reparations: country_comparison/radical_redistr_all_share
decrit("group_defended") # 45%, 32%
# Variance decomposition: all/lmg_share_solidarity_supported_few_group, all/lmg_gcs_support_few_group
for (l in names(lmgs)) print(paste0("R² ", l, ": ", round(lmgs[[l]]@R2, 4)))
# Universalists by country: country_comparison/group_defended_nolabel
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight), 2)) # lower in JP, US
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$country_name %in% countries_Eu)), 5)) # 50.003% Majority in Europe
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg == "Left")), 2)) # 59% among left voters


##### 1. Data and design #####
## Samples
decrit(all$country, weight = F)
decrit(all$date[all$country != "RU"]) # Apr 15 - Jul 3
decrit(all$date[all$country == "RU"]) # Sep 19 - Oct 9
binconf(469/2, 469) - .5 # 4.5%
binconf(1000/2, 1000) - .5 # 3.1%
binconf(3000/2, 3000) - .5 # 1.8%

## Representativeness
# tables/All_Eu_EU_bold, tables/FR_DE_IT_bold, tables/PL_ES_GB_CH_bold, tables/JP_SA_US_bold
# Variance decomposition: tables/all/lmg_gcs_support_few 11%, tables/all/lmg_share_solidarity_supported_few 17%
# tables/all/lmg_gcs_support_few_wo_vote_country 5%, tables/all/lmg_share_solidarity_supported_few_wo_vote_country 6%
for (l in c("lmg_gcs_support_few", "lmg_share_solidarity_supported_few", "lmg_gcs_support_few_wo_vote_country", "lmg_share_solidarity_supported_few_wo_vote_country")) print(paste0("R² ", l, ": ", round(lmgs[[l]]@R2, 2)))
# Determinants: tables/determinants_paper, tables/determinants_custom_redistr
# Vote representativeness: country_comparison/vote_representativeness, country_comparison/vote_pnr_out
# Main results by vote: country_comparison/main_radical_redistr_pol_positive, country_comparison/main_radical_redistr_pol_share

## Data quality
sapply(c("all", countries), function(c) round(median(d(c)$duration, na.rm = T), 3))
decrit("survey_biased") # 69.53%
# Feedback: all/comment_field_en, country_comparison/comment_manual_positive
# Attrition
mean(a$valid) # 23% allowed
mean(a$dropout[a$valid]) # 17% drop out among allowed => stayed == valid & !dropout
mean(a$dropout_late[a$valid])
mean(!a$legit[a$stayed]) # 16% excluded => final == legit & stayed
mean(!a$attentive[a$stayed]) # 9% inattentive
mean(a$duration[a$stayed] < 6) # 13% too fast
mean((a$duration < 6 & !a$attentive)[a$stayed]) # 5% too fast & inattentive
# Influence of item order: tables/order
# Comparison other surveys: figures/all/my_tax_global_nation_comparison, figures/all/billionaire_stostad

## Survey structure
# Placebo: tables/placebo


##### 2.1 Top-of-mind Considerations #####
# Most figures come from country_comparison/concerns_field, country_comparison/wish_field, country_comparison/issue_field, country_comparison/injustice_field, 
decrit(all$field_keyword_global_inequality, all, which = all$variant_field == "injustice") # .013
decrit(all$field_gpt_global_inequality, all, which = all$variant_field == "injustice") # .085
decrit(all$field_manual_global_inequality, all, which = all$variant_field == "injustice") # .037
# decrit(all$field_gpt_global_inequality[all$field_manual_global_inequality]) # 86%
# decrit(all$field_manual_global_inequality[all$field_keyword_global_inequality]) # 87%
# decrit(all$field_gpt_global_inequality[all$field_keyword_global_inequality]) # 94%
# Examples of what ChatGPT classifies as global_inequality but I don't:
all$field_en[all$variant_field == "injustice"][sample(setdiff(which(all$field_gpt_global_inequality[all$variant_field == "injustice"]), which(all$field_manual_global_inequality[all$variant_field == "injustice"])), 10)]
# Examples of what I classify as global_inequality but the keyword search doesn't:
all$field_en[all$variant_field == "injustice"][sample(setdiff(which(all$field_manual_global_inequality[all$variant_field == "injustice"]), which(all$field_keyword_global_inequality[all$variant_field == "injustice"])), 10)]
decrit(all$field_gpt_global_inequality[all$field_en %in% c("poverty", "Poverty")]) # 42 out of 47
all$field_keyword_global_inequality[all$field_en %in% "inequality among humans"]
decrit(all$field_manual_money, all, which = all$variant_field %in% c("concerns", "wish")) # .3048
summary(lm(field_manual_money ~ factor(income_decile), data = all, subset = all$variant_field %in% c("concerns", "wish"), weights = weight))
decrit("field_manual_money", which = all$variant_field %in% c("concerns", "wish") & all$income_decile == 10, data = all) # .22
decrit("field_manual_money", which = all$variant_field %in% c("concerns", "wish") & all$income_decile == 1, data = all) # .35
# Footnote:
summary(lm(e$field_manual_money ~ gini_2019[e$country])) # 8pp
summary(lm(e$field_manual_money ~ gdp_pc_nominal_2024[e$country]))
summary(lm(e$field_manual_money ~ gini_2019[e$country] + gdp_pc_nominal_2024[e$country]))

decrit(all$field_manual_own_country | all$field_manual_global_inequality, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .11
decrit(all$field_manual_own_country, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .0151
decrit(all$field_manual_global_inequality, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .10
sort(round(sapply(c("all", countries), function(c) wtd.mean(d(c)$field_manual_global_inequality, d(c)$weight * (d(c)$variant_field %in% "injustice") )), 3)) # IT > PL > ES > ... > JP > RU
decrit(grepl("clean water", all$field_en), which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all)
decrit(grepl("starv", all$field_en), which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all)


# Correlations
# summary(lm(field_manual_immigration ~ vote == "Far right", data = all, weights = weight)) # 10 pp more likely from a baseline of 3pp; R²: .03
summary(lm(vote == "Far right" ~ field_manual_immigration, data = all, weights = weight)) # 22.1 pp more likely from a baseline of 8.77pp; R²: .03
(22.1+8.78)/8.78 # 3.5
summary(lm(age > 65 ~ field_manual_old_age, data = all, weights = weight)) # 25 pp more likely from a baseline of 24 pp; R²: .01
with(e, cor(field_manual_immigration, vote == "Far right", use = "complete.obs")) # .17
with(e, cor(field_manual_far_right_criticism, vote == "Left", use = "complete.obs")) # .16
with(e, cor(field_manual_old_age, age, use = "complete.obs")) # .13
with(e, cor(field_manual_old_age, age > 50, use = "complete.obs")) # .12
with(e, cor(field_manual_health, age, use = "complete.obs")) # .11
with(e, cor(field_manual_health, age_exact > 55, use = "complete.obs")) # .10
with(e, cor(field_manual_job, employment_status == "Unemployed (searching for a job)", use = "complete.obs")) # .09
with(e, cor(field_manual_animals, group_defended == "Sentient beings", use = "complete.obs")) # .09
with(e, cor(field_manual_education, employment_status == "Student", use = "complete.obs")) # .09
with(e, cor(field_manual_environment, vote == "Left", use = "complete.obs")) # .08
with(e, cor(field_manual_money, income_quartile, use = "complete.obs")) # -.08
with(e, cor(field_manual_far_right_criticism, vote == "Non-voter, PNR or Other", use = "complete.obs")) # -.07
with(e, cor(field_manual_nothing, vote == "Non-voter, PNR or Other", use = "complete.obs")) # .07
with(US, cor(field_manual_discrimination, race_black, use = "complete.obs")) # .07
with(e, cor(field_manual_inequality, vote == "Left", use = "complete.obs")) # .06
with(e, cor(field_manual_far_right_criticism, vote == "Far right", use = "complete.obs")) # -.06
with(e, cor(field_manual_corruption, vote == "Far right" , use = "complete.obs")) # .05
with(e, cor(field_manual_security, vote == "Far right", use = "complete.obs")) # .05
with(e, cor(field_manual_family, group_defended == "Family and self", use = "complete.obs")) # .04
with(e, cor(field_manual_taxes_welfare, vote == "Left", use = "complete.obs")) # -.04
with(e, cor(field_manual_education, education, use = "complete.obs")) # .03
with(e, cor(field_manual_own_country, vote == "Far right", use = "complete.obs")) # .03
with(e, cor(field_manual_relationships, couple, use = "complete.obs")) # -.03
with(e, cor(field_manual_happiness, well_being, use = "complete.obs")) # .02
# Below, not statistically significant at 5% threshold
with(e, cor(field_manual_global_issue, group_defended == "Humans", use = "complete.obs")) # .02
with(e, cor(field_manual_global_issue, vote == "Left", use = "complete.obs")) # .01
with(e, cor(field_manual_discrimination, foreign_origin, use = "complete.obs")) # .01

# Slant is based on subjective impressions, cf. 2_prepare.R


##### 2.2 Prioritization of Public Spending Items #####
# Figures: country_comparison/split_few_bars; country_comparison/split_few_bars_nb0
wtd.mean(all$revenue_split_few_global, all$weight) # 17.5%
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$revenue_split_few_global, d(c)$weight), 3))) # 14% JP to 21% ES, SA
wtd.t.test(x = all$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = all$weight) # p < 1e-100
wtd.t.test(x = JP$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = JP$weight) # p = .07
wtd.t.test(x = CH$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = CH$weight) # p < 1e-4
wtd.mean(all$revenue_split_few_global, all$weight)/(33.4*2/5) # +31%
wtd.mean(all$revenue_split_few_global == 0, all$weight) # 13.3%
# Footnote:
wtd.mean(all$revenue_split_few_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES")) # 17.84%
wtd.mean(all$revenue_split_few_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES"))/(33.4*2/5) # +34%
wtd.mean(all$revenue_split_few_global, all$weight)/wtd.mean(all$revenue_split_few_domestic_education_healthcare, all$weight) # 68%

sort(sapply(variables_split_many, function(c) wtd.mean(all[[c]], all$weight, na.rm = T)), decreasing = T) # 27.26, 22.38, 18.6, 16.24
with(all, summary(lm((split_many_global/split_nb_global) ~ as.factor(split_nb_global), weights = weight))) 
wtd.mean(all$split_nb_global, all$weight) # 1.5
wtd.mean(all$split_many_global, all$weight) # 26.9%
wtd.mean(all$split_many_global, all$weight)/wtd.mean(all$split_nb_global, all$weight) # 17.5%
# wtd.mean(all$split_many_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES"))/wtd.mean(all$split_nb_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES")) # 17.5%
decrit("revenue_split", RU) # 12.2%, 5%
wtd.mean(RU$revenue_split == 0, RU$weight) # 12%


##### 3.1 International Climate Scheme #####
# Main figure: country_comparison/ncs_gcs_ics_all_control_features_median_belief_various
## Pluralistic ignorance
round(wtd.median(all$gcs_belief_own, weight = all$weight, na.rm = T) - sapply(c("all", countries), function(c) wtd.mean(d(c)$gcs_support, d(c)$weight))) # -16 pp
round(wtd.median(all$gcs_belief_us, weight = all$weight * (all$country != "US"), na.rm = T) - wtd.mean(US$gcs_support, US$weight)) # -22 pp
gcs_support <- sapply(c("all", countries), function(c) wtd.mean(d(c)$gcs_support, d(c)$weight))

decrit(all$gcs_belief_own >= 50) # 39%
decrit("gcs_support", which = all$gcs_belief_own >= 50) # .72 
decrit("gcs_support", which = all$gcs_belief_own < 50) # .44 
decrit("gcs_belief_own", which = all$gcs_support > 0) # mean 48%, median 50%
decrit("gcs_belief_own", which = all$gcs_support == 0) # mean 31%, median 30%
decrit(all$gcs_belief_own >= 50, which = all$gcs_support > 0) # 51%
decrit(all$gcs_belief_own >= 50, which = all$gcs_support == 0) # 24%

# wtd.mean(all$gcs_belief_own - sapply(countries, function(c) wtd.mean(d(c)$gcs_support, d(c)$weight))[all$country], all$weight) # -14 pp
# wtd.mean(all$gcs_belief_us - wtd.mean(US$gcs_support, US$weight), all$weight * (all$country != "US")) # -18 pp

## ICS
summary(lm(ics_support ~ variant_ics, data = all, weights = weight)) # 4 pp**, -6.6***
# summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight))
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight, subset = all$country %in% names(countries_Eu))) # Eu
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight, subset = all$country %in% c("JP", "US"))) # JP & US
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "mid"), data = all, weights = weight, subset = all$country == "JP")) # high
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight, subset = all$country == "US")) # high
summary(lm(ics_support ~ variant_ics + I(country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP")) + I(country %in% c("US", "SA")) + variant_ics:I(country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP")) + variant_ics:I(country %in% c("US", "SA")), data = all, weights = weight, subset = grepl("high", all$variant_ics))) 
summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("ES", "CH")))  # 1pp
# summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP"))) 
# summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("US", "SA"))) 

decrit("gcs_understood") # 74%
summary(lm(ics_support ~ gcs_understood * variant_ics, data = all, weights = weight)) # no interaction effect gcs_understood:high_color
summary(lm(reg_formula("gcs_support", c(variables_socio_demos, "!gcs_understood")), data = all, weights = weight)) # 5***
summary(lm(reg_formula("ics_support", c(variables_socio_demos, "!gcs_understood")), data = all, weights = weight)) # 4***
# summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = gcs_understood))
# summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = !gcs_understood))
# summary(lm(reg_formula("gcs_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight)) # -5***
# summary(lm(reg_formula("ics_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight, subset = !all$variant_ics %in% c("high_color"))) # -5***
# summary(lm(reg_formula("ics_high_color_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight)) # -2
# summary(lm(ics_support ~ (variant_ics == "high_color"), data = all, weights = weight, subset = all$gcs_understood)) # -4***
wtd.mean(sapply(countries, function(c) wtd.mean(d(c)$ncs_support > 0, d(c)$weight)) - c(0.440548811, .377372885, 0.715080473, 0.541907808, 0.578242322, 0.579737655, NA, 0.592091901, NA, NA, 0.529638041), adult_pop, na.rm = T) # 12pp Figure A5 of Dechezleprêtre et al. (2025), https://github.com/bixiou/intl_climate_attitudes/blob/main/xlsx/country_comparison/national_policies_new_share_countries.xlsx


##### 3.2 Wealth Tax Funding LICs #####
sapply(c("all", countries), function(c) round(mean(d(c)$global_tax_support, na.rm = T), 3)) # 74%
sapply(c("all", countries), function(c) round(mean(d(c)$hic_tax_support, na.rm = T), 3)) # 70%
sapply(c("all", countries), function(c) round(mean(d(c)$intl_tax_support, na.rm = T), 3)) # 68%
with(e, summary(lm(wealth_tax_support ~ (variant_wealth_tax == "global") + (variant_wealth_tax == "intl")))) # -4.8***, -1.4


##### 4.1 Conjoint analysis #####
decrit("conjoint") # 27%
summary(reg_conjoint <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight)) # +5*** / -3***
coeftest(reg_conjoint, vcov = vcovCL(reg_conjoint, cluster = ~n))

# Conditional logit
(summary(clog <- clogit(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program + strata(n), data = call, weights = weight)))
prob <- wtd.mean(call$program_preferred, call$weight)
coef(clog)["millionaire_tax_in_programTRUE"] * prob * (1 - prob) # .06
coef(clog)["cut_aid_in_programTRUE"] * prob * (1 - prob) # -.027

# Comparison with average effect size
conjoint_effects <- sapply(names(amce)[grepl("EN", names(amce)) & (nchar(names(amce)) < 6)], function(i) sapply(names(amce[[i]]$estimates), function(k) amce[[i]]$estimates[[k]][1,]))
for (i in 1:5) for (j in 1:9) conjoint_effects[[i,j]] <- conjoint_effects[[i,j]] * adult_pop[-c(9:10)][j]/mean(adult_pop[-c(9:10)])
conjoint_effects <- conjoint_effects_mod <- unlist(conjoint_effects)
# mean(abs(conjoint_effects)) # 6pp
conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy2"] <- -conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy2"]
conjoint_effects_mod[!names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2")] <- abs(conjoint_effects_mod[!names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2")])
mean(conjoint_effects_mod[names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1")])/mean(abs(conjoint_effects_mod)) # 96%
# mean(conjoint_effects_mod[names(conjoint_effects) %in% c("foreignpolicyforeignpolicy2")])/mean(abs(conjoint_effects_mod)) # 42%
# mean(conjoint_effects_mod[names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1")])
# mean(abs(conjoint_effects_mod)) - mean(conjoint_effects_mod[names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1")]) # .003
# summary(lm((conjoint_effects_mod) ~ I(names(conjoint_effects_mod) %in% c("foreignpolicyforeignpolicy1")))) # not significant
# summary(lm((conjoint_effects_mod) ~ I(names(conjoint_effects_mod) %in% c("foreignpolicyforeignpolicy2")))) # lower
X <- abs(conjoint_effects_mod) # I test whether mean(Y) <= mean(X), by sampling new Ys 
# Y <- conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy1"] 
permutation_means <- replicate(1e5, {mean(sample(X, 9, replace = FALSE))}) # 9 corresponds to the number of foreignpolicyforeignpolicy1 coefs
mean(mean(conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy1"]) > permutation_means) # One-sided p-value: .43
mean(mean(conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy2"]) > permutation_means) # One-sided p-value: .01

## Consistent programs:
# Effects are preserved when inconsistent programs are removed (considering the two policies as consistent with any program). Cf. Cuesta et al. (22)
decrit(!call$consistent_conjoints_party, data = call) # 28.54%
reg_conjoint_cons_party <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_party) 
coeftest(reg_conjoint_cons_party, vcov = vcovCL(reg_conjoint_cons_party, cluster = ~n)) # 5*** -3***
decrit("consistent_conjoints_party_strict", data = call) # 42.54%
reg_conjoint_cons_party_strict <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_party_strict) 
coeftest(reg_conjoint_cons_party_strict, vcov = vcovCL(reg_conjoint_cons_party_strict, cluster = ~n)) # 5*** -4***

## Footnote on policy popularity
effects_conjoint <- policies_leaning[-18,]
for (c in countries[-c(9:10)]) for (v in row.names(effects_conjoint)) if (!is.na(policies_leaning[v,c])) effects_conjoint[v,c] <- amce[[languages_country[[c]][1]]]$estimates[[gsub("[_0-9]", "", v)]][1, paste0(gsub("[_0-9]", "", v), sub("_", "", v))]
colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 0), na.rm = T)
colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 1), na.rm = T)
colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 2), na.rm = T)
#              FR DE IT PL ES GB JP US CH   Overall
# Most liked   L  F  L  L  L  L  L  L  F    L
# Lest liked   C  L  F  F  F  FC CF CF L    C
wtd.mean(colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 0), na.rm = T), adult_pop[-c(9,10)])
wtd.mean(colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 1), na.rm = T), adult_pop[-c(9,10)])
wtd.mean(colSums((effects_conjoint - colMeans(effects_conjoint, na.rm = T)) * (policies_leaning_party[-18,] == 2), na.rm = T), adult_pop[-c(9,10)])


#####  4.2 Testing Warm Glow ##### 
# Figures: country_comparison/gcs_support_by_variant_warm_glow, country_comparison/share_solidarity_supported_by_info_solidarity
summary(lm(reg_formula("gcs_support", c(control_variables[-11], "variant_warm_glow")), data = all, weights = weight, subset = variant_warm_glow != "NCS" & !country %in% c("SA", "RU"))) # 3pp**

# 2SLS
summary(first_stage_wo_control <- lm((likely_solidarity > 0) ~ info_solidarity, data = e, weights = weight)) # 33% + 8pp***
summary(first_stage <- lm(reg_formula("(likely_solidarity > 0)", c("info_solidarity", control_variables[-11])), data = e, weights = weight))
summary(iv_model <- ivreg(reg_formula("share_solidarity_supported", c("(likely_solidarity > 0)", control_variables[-11])), instruments = reg_formula("", c("info_solidarity", control_variables[-11])), data = e, weights = weight)) 
(effF_wo_control <- eff_F(data = e, Y = "share_solidarity_supported", D = "I(likely_solidarity > 0)", Z = "info_solidarity", controls = NULL, weights = "weight")) # 65
(effF <- eff_F(data = e, Y = "share_solidarity_supported", D = "I(likely_solidarity > 0)", Z = "info_solidarity", controls = control_variables[-11], weights = "weight")) # 67
summary(ols_model <- lm(reg_formula("share_solidarity_supported", c("(likely_solidarity > 0)", control_variables[-11])), data = e, weights = weight)) # 14.53***
summary(direct_effect <- lm(reg_formula("share_solidarity_supported", c("info_solidarity", control_variables[-11])), data = e, weights = weight)) 
stargazer(first_stage_wo_control, first_stage, iv_model, ols_model, direct_effect, 
          se = lapply(c("first_stage_wo_control", "first_stage", "iv_model", "ols_model", "direct_effect"), function(m) sqrt(diag(vcovHC(eval(str2expression(m)), type = "HC1")))),
          column.labels = c("IV 1st Stage", "IV 1st Stage", "IV 2nd Stage", "OLS", "Direct Effect"), model.names = FALSE, no.space = TRUE,
          keep.stat = c("n", "rsq"), label = "tab:iv", dep.var.caption = "", 
          dep.var.labels = c("\\makecell{Believes global\\\\redistribution likely}", "Share of plausible global policies supported"),
          covariate.labels = c("Information treatment", "Believes global redistribution likely", "(Intercept)"), keep = c("solidarity", "Constant"),
          type = "latex", style = "default", out = "../tables/IV_warm_glow_no_star.tex", float = FALSE, star.cutoffs = NA,
          title = "Effect on support for global redistribution of believing that it is likely.", omit.table.layout = "n", 
          # notes = "\\textit{Note:} Robust standard errors are in parentheses. \\hfill $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01",
          add.lines = list(c("Controls: sociodemos and vote", "", "\\checkmark", "\\checkmark", "\\checkmark", "\\checkmark"), 
                           c("Effective F-statistic", sprintf("%.2f", effF_wo_control), sprintf("%.2f", effF), "", "", "")))  


##### 5.1 Acceptance of Currently Debated Global Policies #####
## Plausible Global Policies
# Figure absolute support: country_comparison/solidarity_support_share
# Figure net support: country_comparison/synthetic_indicators_mean
# Figures broken down by political leaning: country_comparison/main_radical_redistr_pol_positive, country_comparison/solidarity_support_pol_share, country_comparison/synthetic_indicators_pol_mean
summary(lm(share_solidarity_diff ~ saudi, data = all, subset = all$country == "SA", weights = weight)) # Saudis vs. non-Saudis
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Left")), 2))) # IT first
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right" | (d(c)$country == "US" & d(c)$vote == 1))), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Left")), 2)))

# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right")), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == 1)), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == -1)), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right")), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote > 0)), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight), 2)))

## NCQG
# Figures country_comparison/ncqg_nolabel, country_comparison/ncqg_full_nolabel
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$ncqg_fusion >= 600, d(c)$weight * (d(c)$variant_ncqg == "Short"), na.rm = T), 2)) # 19%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$ncqg_fusion >= 600, d(c)$weight * (d(c)$variant_ncqg == "Full"), na.rm = T), 2)) # 19%


##### 5.2 Support for Radical Proposals, Political Action, and Broad Values #####
# General Figure: country_comparison/radical_redistr_all_share

## Global income redistribution
# Figure acceptance with features: country_comparison/top_tax_all_share_various
# Figure absolute support: country_comparison/top_tax_positive
# Figure opposition: country_comparison/top_tax_affected_negative_various
# Figure affected: country_comparison/top_tax_affected_share_various
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top1_tax_support > 0, d(c)$weight), 2))) # 56%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top1_tax_support < 0, d(c)$weight), 2))) # 25%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top3_tax_support > 0, d(c)$weight), 3))) # 50.3%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top3_tax_support < 0, d(c)$weight), 2))) # 28%

## Convergence 
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$convergence_support > 0, d(c)$weight), 2))) # 61%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$convergence_support < 0, d(c)$weight), 2))) # 26%
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$convergence_support > 0, d(c)$weight * (d(c)$convergence_support != 0)), 2))) # 56% US

## Willingness to Act
# Figure global movement: country_comparison/global_movement_positive
# On millionaires: country_comparison/main_radical_redistr_pol_positive
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$global_movement_spread | d(c)$global_movement_demonstrate | d(c)$global_movement_donate | d(c)$global_movement_strike, d(c)$weight), 3))) # 68%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$global_movement_demonstrate | d(c)$global_movement_donate | d(c)$global_movement_strike, d(c)$weight), 3))) # 29%
# Figure vote: country_comparison/vote_intl_coalition_nolabel
wtd.mean((all$vote == -1) * (all$vote_agg == 0), all$weight) # 5%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$vote_intl_coalition > 0, d(c)$weight * (d(c)$vote == -1) * (d(c)$vote_agg == 0)), 3))) # 46%
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$vote_intl_coalition < 0, d(c)$weight * (d(c)$vote == -1) * (d(c)$vote_agg == 0)), 3))) # 10%
# Footnote:
decrit(all$country, data = all, which = all$millionaire == 5) # 60%
decrit(all$country %in% names(countries_Eu), data = all, which = all$millionaire == 5) # 26%

## Reasons for Helping LICs
# Figure country_comparison/why_hic_help_lic_positive

## Reparations
# Figure country_comparison/reparations_support_nolabel
decrit(all$reparations_support > 0) # 35%
decrit(all$reparations_support < 0) # 42%
decrit(IT$reparations_support > 0, data = IT) # 56%

## Comparison with Global Nation
# General Figure: country_comparison/radical_redistr_all_share
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight), 3)) # 41%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$my_tax_global_nation < 0, d(c)$weight), 3)) # 28%
# (mean_gn25 <- wtd.mean(sapply(names(my_taxes_global_nation_2023)[!is.na(my_taxes_global_nation_2023)], function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), adult_pop[!is.na(my_taxes_global_nation_2023)])) # 59%
# (mean_gn23 <- wtd.mean(my_taxes_global_nation_2023, adult_pop, na.rm = T)) # 56% in Global Nation (2023)
# mean_gn25 - mean_gn23 # .03

## Moral circle
# Figure country_comparison/group_defended_nolabel
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight), 4))) # 45% all; 30% JP, 57% SA
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$country_name %in% countries_Eu)), 5)) # 50.003% Eu
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg == "Left")), 2)) # 59%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg > 0)), 2)) # 32%
# sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote == -1)), 2))
# sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg == "Far right"| (d(c)$country == "US" & d(c)$vote == 1))), 2))

cor(e$field_universalism, e$latent_support_global_redistr, use = "complete.obs") # 5%
cor(e$field_manual_inequality, e$latent_support_global_redistr, use = "complete.obs") # 9%
cor(e$field_keyword_inequality, e$latent_support_global_redistr, use = "complete.obs") # 8%
cor(e$field_gpt_inequality, e$latent_support_global_redistr, use = "complete.obs") # 6%
cor(e$universalist, e$latent_support_global_redistr, use = "complete.obs") # 37%
cor(e$field_universalism, e$universalist, use = "complete.obs") # 3%
cor(e$field_universalism2, e$latent_support_global_redistr, use = "complete.obs") # 3%
summary(lm(latent_support_global_redistr ~ field_universalism, data = all, weights = weight))
summary(lm(latent_support_global_redistr ~ field_universalism2, data = all, weights = weight))


##### 5.3 Preferred Channels for Transferring Resources to LICs #####
# Figure support (Right or Best): country_comparison/transfer_how_positive
# Figure preference (Best): country_comparison/transfer_how_above_one
# Figure opposition (Wrong): country_comparison/transfer_how_negative


##### 5.4 Custom Global Income Redistribution #####
# Footnote anchoring:
(summary(reg_anchoring_winners <- lm(custom_redistr_winners ~ variant_sliders, weights = weight, data = all, subset = custom_redistr_satisfied_touched > 0))) # 7.3***
reg_anchoring_winners$coefficients[2]/20 # 36%
(summary(reg_anchoring_losers <- lm(custom_redistr_losers ~ variant_sliders, weights = weight, data = all, subset = custom_redistr_satisfied_touched > 0))) # 5.7***
reg_anchoring_losers$coefficients[2]/10 # 57%
(summary(reg_anchoring_degree <- lm(custom_redistr_degree ~ variant_sliders, weights = weight, data = all, subset = custom_redistr_satisfied_touched > 0))) # -2.1***
reg_anchoring_degree$coefficients[2]/5 # 42%

# Heterogeneity
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_satisfied, d(c)$weight))) # 56%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_skip, d(c)$weight))) # 43%
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Left") # 61%
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Far right") # 56.51%
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Center-right or Right") # 54%
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Non-voter, PNR or Other") # 52%
decrit("custom_redistr_satisfied", all, which = all$education == 1) # 49%
decrit("custom_redistr_satisfied", all, which = all$education == 2) # 57%
decrit("custom_redistr_satisfied", all, which = all$education == 3) # 57%

# Median preferred parameters:
# Figure questionnaire/survey_custom_redistr_median_zoom
decrit("custom_redistr_winners", which = all$custom_redistr_satisfied > 0) # 49%
decrit("custom_redistr_losers", which = all$custom_redistr_satisfied > 0) # 18%
decrit("custom_redistr_degree", which = all$custom_redistr_satisfied > 0) # 5%
algo_dis_av(49*10, 1000 - 18*10, 5, verbose = T) # 287$, 5.4%

# Self-interest in custom choice:
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_self_lose, d(c)$weight * d(c)$custom_redistr_satisfied), 3)) # 48%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_self_gain, d(c)$weight * d(c)$custom_redistr_satisfied), 3)) # 9%
decrit(all$custom_redistr_winners * all$custom_redistr_degree * all$custom_redistr_losers == 0, weights = all$weight, which = all$custom_redistr_satisfied > 0) # 10%

# Mean preferred redistr:
# Figures: mean_custom_redistr/all_satisfied, country_comparison/custom_redistr_most_mean, country_comparison/custom_redistr_satisfied_mean
100*sum(mean_custom_redistr[["all_satisfied"]][1:max_winners] - current[1:max_winners])/sum(current[1:1000]) # 5.4%
(max_winners <- min(which(mean_custom_redistr[["all_satisfied"]] < current_inc)))/10 # 73%
(1000 - max(which(mean_custom_redistr[["all_satisfied"]] > current_inc)))/10 # 27%
round(mean_custom_redistr$all_satisfied[1]/12) # 247
Gini(current) # .67
Gini(mean_custom_redistr$all_satisfied) # .59
Gini(median_redistr <- algo_dis_av(49*10, 1000 - 18*10, 5)) # .58
# Top 10% share
sum(current[901:1001])/sum(current) # 52%
sum(mean_custom_redistr$all_satisfied[901:1001])/sum(mean_custom_redistr$all_satisfied) # 47%
sum(median_redistr[901:1001])/sum(median_redistr) # 47%
# Bottom 50%
sum(current[1:501])/sum(current) # 8%
sum(mean_custom_redistr$all_satisfied[1:501])/sum(mean_custom_redistr$all_satisfied) # 13%
sum(median_redistr[1:501])/sum(median_redistr) # 13%
# Interdecile ratio
current[901]/current[101] # 42
mean_custom_redistr$all_satisfied[901]/mean_custom_redistr$all_satisfied[101] # 11
median_redistr[901]/median_redistr[101] # 10

# Figure tax rates: all/tax_radical_redistr
# Figures by country: country_comparison/custom_redistr_satisfied_mean, country_comparison/custom_redistr_satisfied_median
# Figures individual heterogeneity: all/custom_redistr_winners_agg, all/custom_redistr_losers_agg, all/custom_redistr_income_min_ceiling, all/custom_redistr_transfer_ceiling


##### A Raw Results #####
# All figures are generated and exported in 3_render.R


### Survey Sources and Features ###
##### C.2 Country-specific Features #####
table_feature <- figures[c(50, 40, 37, 47, 48, 18:19, 22, 24, 26, 35:36, 29), c(3:13)] # 9:12 17
colnames(table_feature) <- countries
table_feature <- gsub("early|onthly|et|ross", "", gsub("^\\$", "\\\\$", gsub("€", "\\euro{}", gsub("&nbsp;", "~", table_feature, fixed = T), fixed = T)))
row.names(table_feature) <- c("\\ref{q:ncs_support} NCS \\verb|amount_expenses| (LCU/m.)", "\\ref{q:gcs_support} GCS net cost (\\$/month)", "\\ref{q:gcs_support} GCS \\verb|amount_lost| (LCU/month)", 
                              "\\ref{q:gcs_support} GCS \\verb|amount_bi| (LCU/month)", "\\ref{q:gcs_support} GCS \\verb|price_increase| (\\%)", "\\ref{q:income} Income type: \\textbf{n}et/\\textbf{g}ross", 
                              "\\ref{q:top3_tax_support} Income period: \\textbf{m}onth/\\textbf{y}ear", "\\ref{q:top3_tax_support} 80k \\$PPP \\verb|lcu_80k|", "\\ref{q:top1_tax_support} 120k \\$PPP \\verb|lcu_120k|", "\\ref{q:top3_tax_support} 1M \\$PPP \\verb|lcu_1M|", 
                              "\\ref{q:revenue_split_few} Wealth tax revenue (\\$ bn)", "\\ref{q:revenue_split_few} Wealth tax revenue (\\% GNI)", "\\quad LCU per dollar (on Apr. 2, 2025)")
table_feature[3:4,] <- gsub("[^0-9]*", "", table_feature[3:4,])
table_feature %>% kable("latex", booktabs = TRUE, escape = FALSE, table.envir = NULL) %>% save_kable("../tables/features.tex")
temp <- readLines("../tables/features.tex")
cat(sub(" & FR", "Question; Feature & FR", temp), file = "../tables/features.tex")


##### C.3 Exploraty Factor Analaysis #####
gsub("-", "$-$", gsub("\\verb|convergence", "\\bottomrule \\end{tabular} \\switchcolumn \\begin{tabular}[h]{lr} \\verb|convergence", 
  kable(round(setNames(loadings, paste0("\\verb|", sub("expanding_security_council", "un_reform", gsub("solidarity_support", "pl_support", gsub("^transfer_|^why_hic_", "", names(loadings)))), 
  "|"))[order(-abs(loadings))], 3), format = "latex", booktabs = TRUE, col.names = c("Variable name", "Loading"), linesep = "", escape = FALSE), fixed = T)) %>% save_kable("../tables/efa.tex")
correlations <- cor(as.data.frame(lapply(e[, c(variables_interest, "latent_support_global_redistr")], as.numeric)), use = "pairwise.complete.obs")
# corrplot(cors)
sort(rowMeans(abs(correlations), na.rm = T)) # share_solidarity_supported .42, solidarity_support_ncqg_300bn 40, my_tax_global_nation .35, vote_intl_coalition .35, ncqg .35, global_movement_no .34, 

# MIRT
cor(all$irt_global_redistr, all$latent_support_global_redistr) # .98


##### C.4 Definition of Keywords #####
export_keywods <- function(keys = keywords, strings = keywords_labels, file = "../tables/keywords.tex") {
  cat(paste("\\begin{itemize} \n \\item \\textbf{", paste(sapply(names(keys), function(k) paste0(strings[k], "}: \\texttt{", 
                                                                                                 gsub("|", "\\allowbreak|", gsub("^", "\\^{}", gsub("$", "\\$", keys[k], fixed = T), fixed = T), fixed = T))), 
                                                          collapse = "};\n \\item \\textbf{"), "}. \n \\end{itemize}"), file = file)
}
export_keywods()
export_keywods(keywords_comment, keywords_comment_labels, "../tables/keywords_comment.tex")


##### D Representativeness of the Surveys ######
# representativeness_table("All")
representativeness_table(c("All", "Eu", "EU"))
# representativeness_table(c("Eu", countries[1:3]))
representativeness_table(countries[1:3])
representativeness_table(countries[4:7])
representativeness_table(countries[c(8,10,11)], omit = c("Not 25-64")) # TODO vote; employment
representativeness_table(countries[8:11], omit = c("Not 25-64"))

# # Prez:
# representativeness_table(countries[1:6])
# representativeness_table(countries[7:11], omit = c("Not 25-64"))
  

##### E Determinants of Support #####
desc_table(c("share_solidarity_supported", "gcs_support/100", "universalist", "vote_intl_coalition > 0", "convergence_support > 0", "wealth_tax_support", "sustainable_future"),  # "\\makecell{Preferred amount\\\\of climate finance\\\\(NCQG)}"
           dep.var.labels = c("\\makecell{Share of\\\\plausible\\\\policies\\\\supported}", "\\makecell{Supports\\\\the Global\\\\Climate\\\\Scheme}", "\\makecell{Universalist\\\\(Group\\\\defended:\\\\\\textit{Humans} or\\\\\\textit{Sentient beings})}", 
                              "\\makecell{More likely\\\\to vote\\\\for party\\\\in global\\\\coalition}", "\\makecell{Endorses\\\\convergence\\\\of all countries'\\\\GDP p.c.\\\\by 2100}", "\\makecell{Supports an\\\\international\\\\wealth tax\\\\funding LICs}", "\\makecell{Prefers a\\\\sustainable\\\\future}"),
          indep_vars = c(control_variables), filename = "determinants_paper", nolabel = F, model.numbers = T, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA", "Income quartile: NA")) 

desc_table(c("custom_redistr_transfer", "custom_redistr_transfer", "custom_redistr_transfer", "custom_redistr_self_lose", "custom_redistr_self_lose", "custom_redistr_satisfied", "custom_redistr_untouched", "custom_redistr_satisfied_touched"),  # "\\makecell{Preferred amount\\\\of climate finance\\\\(NCQG)}"
           dep.var.labels = c("\\makecell{Custom transfer\\\\(in \\% of world GDP)}", "\\makecell{Loses\\\\from custom\\\\redistribution}", "\\makecell{Satisfied with\\\\own custom\\\\redistr.}", "\\makecell{Has not\\\\touched the\\\\sliders}", "\\makecell{Touched the\\\\sliders and\\\\satisfied}"),
           indep_vars = control_variables, data = list(all, all[all$custom_redistr_satisfied > 0,], all[all$custom_redistr_satisfied_touched > 0,], all, all[all$custom_redistr_satisfied_touched > 0,], all, all, all),
           add_lines = list(c(44, paste("\\hline  \\\\[-1.8ex] Subsample: \\textit{Satisfied} & & \\checkmark & & & & & &")),
                            c(45, paste("Subsample: \\textit{Touched \\& Satisfied} & & & \\checkmark & & \\checkmark & & & \\\\"))),
           filename = "determinants_custom_redistr", nolabel = F, model.numbers = F, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA", "Income quartile: NA"))


##### F The Determination of a Custom Redistribution
pdf("../figures/all/tax_radical_redistr.pdf", width = 520/100, height = 465/100)
par(mar = c(4.1, 4.1, .2, .5)) # 520 x 465
plot(log10(seq(0, 1.1e6, 1e3)), 100*tax_rates_custom_redistr(mean_custom_redistr[["all"]], at = seq(0, 1.1e6, 1e3)), xaxt = 'n', type = 'l', lwd = 2, xlim = c(4, 6), ylim = c(0, 36), ylab = "Tax rate (in %)", xlab = "Individualized yearly income (in PPP 2024 $)")
lines(log10(seq(0, 1.1e6, 1e3)), 100*tax_rates_custom_redistr(world_income_after_tax("top1"), at = seq(0, 1.1e6, 1e3)), type = 'l', lwd = 2, lty = 2, col = "blue")
lines(log10(seq(0, 1.1e6, 1e3)), 100*tax_rates_custom_redistr(world_income_after_tax("top3"), at = seq(0, 1.1e6, 1e3)), type = 'l', lwd = 2, lty = 3, col = "purple")
lines(log10(seq(0, 1.1e6, 1e3)), 100*tax_rates_custom_redistr(world_income_after_tax("approx_mean"), at = seq(0, 1.1e6, 1e3)), type = 'l', lwd = 2, lty = 4, col = "black")
axis(1, at = setdiff(log10(c(seq(0, 1e4, 1e3), seq(0, 1e5, 1e4), seq(0, 1e6, 1e5))), log10(c(1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6))), tcl= -0.3, labels=NA)
axis(1, at = log10(c(1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6)), tcl= -.5, labels = c("1k", "3k", "10k", "30k", "100k", "300k", "1M"))
abline(h = seq(0, 35, 5), v = log10(c(1e3, 3e3, 1e4, 3e4, 1e5, 3e5, 1e6)), col = "lightgray", lty = "dotted")
legend("topleft", legend = c("Top 1% tax (15% tax above $120k/year)", "Top 3% tax (15% >80k, 30% >120k, 45% >1M)", "Average custom redistribution", "Approximation of above (7% > 25k, 16% > 40k)"), col = c("blue", "purple", "black", "black"), lwd = 2, lty = c(2,3,1,4))
dev.off()
# save_plot(filename = "tax_radical_redistr", folder = '../figures/all/', width = 520, height = 465, format = "pdf")


##### G Comparison with Other Surveys #####
global_nation_all <- heatmap_table(vars = heatmaps_defs[["radical_redistr"]]$vars, labels = heatmaps_defs[["radical_redistr"]]$labels, along = "country_name", data = all, levels = levels_default, conditions = ">= 1")
global_nation_all[-c(3,5),] <- (global_nation_all/(global_nation_all+heatmap_table(vars = heatmaps_defs[["radical_redistr"]]$vars, labels = heatmaps_defs[["radical_redistr"]]$labels, along = "country_name", data = all, levels = levels_default, weights = F, conditions = "<= -1")))[-c(3,5),]
# global_nation <- rbind(global_nation, c(wtd.mean(all$my_tax_global_nation_external, all$weight, na.rm = T), wtd.mean(all$my_tax_global_nation_external, all$weight * all$country_name %in% countries_Eu, na.rm = T), my_taxes_global_nation))
# In 2024 Global Nation used a different translation. Their survey is on 18-70 yrs, they don't mention quotas, they weight ex post for gender, age, education.
global_nation_all <- rbind(global_nation_all, c(wtd.mean(all$my_tax_global_nation_2023, all$weight, na.rm = T), wtd.mean(all$my_tax_global_nation_2023, all$weight * all$country_name %in% countries_Eu, na.rm = T), my_taxes_global_nation_2023))
row.names(global_nation_all)[9] <- "\"My taxes ... global problems\" (Global Nation, 2023)" # 2024
save_plot(as.data.frame(global_nation_all), filename = "../xlsx/country_comparison/radical_redistr_all")
pdf("../figures/country_comparison/radical_redistr_all_share.pdf", width = 1550/72, height = 500/72)
heatmap_plot(global_nation_all, proportion = T, percent = T)
invisible(dev.off())
# save_plot(filename = "country_comparison/radical_redistr_all_share", width = 1550, height = 650, format = "pdf", trim = T)

pdf("../figures/all/my_tax_global_nation_comparison.pdf", width = 330/105, height = 330/105)
par(mar = c(3.1, 3.1, .1, .1), mgp = c(2, .7, 0))
plot(0:1, 0:1, type = 'l', lty = 2, xlab = "This survey", ylab = "Global Nation (2023)", xlim = c(.43, .8), ylim = c(.43, .8)) 
grid()
points(sapply(countries, function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), my_taxes_global_nation_2023, pch = 18)
text(sapply(countries, function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), my_taxes_global_nation_2023, labels = countries, pos = 4)
dev.off()
# save_plot(filename = "../figures/all/my_tax_global_nation_comparison", width = 330, height = 330, format = "pdf", trim = FALSE)
cor(sapply(countries, function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), my_taxes_global_nation_2023, use = "complete.obs") # .70

pdf("../figures/all/billionaire_stostad.pdf", width = 330/105, height = 330/105)
par(mar = c(3.1, 3.1, .1, .1), mgp = c(2, .7, 0))
plot(0:1, 0:1, type = 'l', lty = 2, xlab = "This survey", ylab = "Cappelen, Støstad & Tungodden", xlim = c(.54, .815), ylim = c(.54, .815))
grid()
points(sapply(countries, function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight)), stostad_billionaire_tax_absolute, pch = 18)
text(sapply(countries, function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight)), stostad_billionaire_tax_absolute, labels = countries, pos = 4)
dev.off()
# save_plot(filename = "../figures/all/billionaire_stostad", width = 330, height = 330, format = "pdf", trim = FALSE)
cor(sapply(countries, function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight)), stostad_billionaire_tax_absolute, use = "complete.obs") # .86
# plot(sapply(countries, function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight * (d(c)$solidarity_support_billionaire_tax_control != 0))), stostad_billionaire_tax_relative)
# lines(0:1, 0:1, type = 'l')

# Study 1 was conducted between August 9th and September 15th, 2024, and Study 2 between May 12th and June 21st, 2025
stostad2 <- read.dta13("../data_ext/stostad2.dta")
stostad2$iso[stostad2$iso == "UK"] <- "GB"
stostad0_billionaire_tax_absolute <- sapply(stostad2$iso, function(c) if (c %in% stostad2$iso) (stostad2$agree1 + stostad2$vagree1)[stostad2$iso == c & stostad2$Survey2 == 0] else NA)
stostad0_billionaire_tax_oppose <- sapply(stostad2$iso, function(c) if (c %in% stostad2$iso) (stostad2$disagree1 + stostad2$vdisagree1)[stostad2$iso == c & stostad2$Survey2 == 0] else NA) 
stostad0_billionaire_tax_relative <- stostad0_billionaire_tax_absolute / (stostad0_billionaire_tax_absolute + stostad0_billionaire_tax_oppose)
stostad1_billionaire_tax_absolute <- sapply(stostad2$iso, function(c) if (c %in% stostad2$iso) (stostad2$agree1 + stostad2$vagree1)[stostad2$iso == c & stostad2$Survey2 == 1] else NA)
stostad1_billionaire_tax_oppose <- sapply(stostad2$iso, function(c) if (c %in% stostad2$iso) (stostad2$disagree1 + stostad2$vdisagree1)[stostad2$iso == c & stostad2$Survey2 == 1] else NA) 
stostad1_billionaire_tax_relative <- stostad1_billionaire_tax_absolute / (stostad1_billionaire_tax_absolute + stostad1_billionaire_tax_oppose)
cor(stostad0_billionaire_tax_absolute, stostad1_billionaire_tax_absolute, use = "complete.obs") # .91
cor(stostad0_billionaire_tax_absolute[countries], stostad1_billionaire_tax_absolute[countries], use = "complete.obs") # .90
cor(sapply(countries[-c(7,9,10)], function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight)), stostad1_billionaire_tax_absolute[countries[-c(7,9,10)]]) # .86
cor(stostad0_billionaire_tax_absolute[countries[-c(7,9,10)]], sapply(countries[-c(7,9,10)], function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight))) # .83


##### H Attrition Analysis #####
desc_table(dep_vars = c("dropout", "dropout_late", "attentive == F", "duration", "duration < 6"), weights = NULL, #ci = T, report = 'vcsp', 
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition", save_folder = "../tables/", data = c(list(a[a$valid == T,]), list(a[a$valid == T,]), list(a[a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,])), 
           indep_vars = control_variables, omit = c("illionaire", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA", "Income quartile: NA")) 

desc_table(dep_vars = c("variant_wealth_tax == 'global'", "variant_wealth_tax == 'intl'", "variant_ics == 'low'", "variant_ics == 'high'", "variant_ics == 'high_color'", "variant_warm_glow == 'NCS'", "variant_warm_glow == 'None'", "info_solidarity"), dep.var.caption = "Random branch:", #, "variant_top_tax == 'top1'" omit = c("Constant"), # c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{Wealth tax\\\\coverage:\\\\Global}", "\\makecell{Wealth tax\\\\coverage:\\\\Int'l}", "\\makecell{Int'l CS\\\\coverage:\\\\Low}", "\\makecell{Int'l CS\\\\coverage:\\\\High}", "\\makecell{Int'l CS\\\\coverage:\\\\High color}", "\\makecell{National\\\\CS\\\\asked}", "\\makecell{Warm glow\\\\substitute:\\\\Control}", "\\makecell{Warm glow\\\\realism: Info\\\\treatment}"), # ci = T, report = 'vcsp', 
           filename = "attrition_treatment", weights = NULL, save_folder = "../tables/", data = a[a$valid == T,], indep_vars = "dropout") 

desc_table(dep_vars = c("variant_field == 'concerns'", "variant_field == 'injustice'", "variant_field == 'issue'", "variant_field == 'wish'", "variant_split == 'Few'", "variant_belief == 'Own'", "variant_ncqg == 'Full'", "variant_sustainable_future == 'a'", "variant_top_tax == 'top1'", "variant_sliders == 'diffuse'"), dep.var.caption = "Random branch:", #, "variant_top_tax == 'top1'" omit = c("Constant"), # c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{Field:\\\\Concerns}", "\\makecell{Field:\\\\Injustice}", "\\makecell{Field:\\\\Issue}", "\\makecell{Field:\\\\Wish}", "\\makecell{Budget\\\\split:\\\\Few}", "\\makecell{GCS\\\\belief:\\\\Own}", "\\makecell{NCQG:\\\\Full}", "\\makecell{Sustainable\\\\Future:\\\\A}", "\\makecell{Income\\\\tax:\\\\top 1\\%}", "\\makecell{Custom\\\\sliders:\\\\Diffuse}"), # ci = T, report = 'vcsp', 
           filename = "attrition_treatment2", weights = NULL, save_folder = "../tables/", data = list(a[a$valid == T,], a[a$valid == T,], a[a$valid == T,], a[a$valid == T,], a[a$valid == T & a$country != "RU",], a[a$valid == T & a$country != "RU",], a[a$valid == T,], a[a$valid == T & a$variant_sustainable_future != "s",], a[a$valid == T,], a[a$valid == T & a$country != "RU",]), indep_vars = "dropout") 


##### I Balance Analysis #####
desc_table(dep_vars = c("variant_wealth_tax == 'global'", "variant_wealth_tax == 'intl'", "variant_ics == 'low'", "variant_ics == 'high'", "variant_ics == 'high_color'", "variant_warm_glow == 'NCS'", "variant_warm_glow == 'None'", "info_solidarity"), dep.var.caption = "Random branch:", #, "variant_top_tax == 'top1'" omit = c("Constant"), # c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{Wealth tax\\\\coverage:\\\\Global}", "\\makecell{Wealth tax\\\\coverage:\\\\Int'l}", "\\makecell{Int'l CS\\\\coverage:\\\\Low}", "\\makecell{Int'l CS\\\\coverage:\\\\High}", "\\makecell{Int'l CS\\\\coverage:\\\\High color}", "\\makecell{National\\\\CS\\\\asked}", "\\makecell{Warm glow\\\\substitute:\\\\Control}", "\\makecell{Warm glow\\\\realism: Info\\\\treatment}"), # ci = T, report = 'vcsp', 
           filename = "balance", weights = NULL, save_folder = "../tables/", data = all, indep_vars = control_variables, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA", "Income quartile: NA")) 


##### J Placebo Tests #####
gcs_field <- lm(gcs_support > 0 ~ variant_field, data = all, weights = weight)
gcs_split <- lm(gcs_support > 0 ~ variant_split, data = all, weights = weight, subset = all$country != "RU")
ics_belief <- lm(ics_support > 0 ~ variant_belief, data = all, weights = weight, subset = all$country != "RU")
solidarity_warm_glow <- lm(share_solidarity_supported ~ variant_warm_glow, data = all, weights = weight)
solidarity_ics <- lm(share_solidarity_supported ~ variant_ics, data = all, weights = weight)
wealth_ics <- lm(wealth_tax_support > 0 ~ variant_ics, data = all, weights = weight)
# gcs_info <- lm(gcs_support > 0 ~ info_solidarity, data = all, weights = weight)
stargazer(gcs_field, gcs_split, ics_belief, solidarity_warm_glow, solidarity_ics, wealth_ics, type = "latex", style = "default", out = "../tables/placebo.tex", 
          keep.stat = c("n", "rsq"), label = "tab:placebo", dep.var.caption = "", model.names = FALSE, no.space = TRUE, float = FALSE, #, "adj.rsq"), dep.var.caption = "Dependent variable:" ,
          dep.var.labels = c("\\makecell{Supports\\\\the Global\\\\Climate Scheme}", "\\makecell{Supports\\\\the Int'l\\\\Clim. Sch.}",
                             "\\makecell{Share of\\\\policies\\\\supported}", "\\makecell{Supports\\\\the int'l\\\\wealth tax}"),
          covariate.labels = c("Open-ended field variant: Injustice", "Open-ended field variant: Issue", "Open-ended field variant: Wish", "Revenue split variant: Many", "GCS belief variant: U.S.",
                               "Warm glow variant: National CS", "Warm glow variant: Donation", "Int'l CS variant: High color", "Int'l CS variant: Low", "Int'l CS variant: Mid", "(Intercept)"),
          title = "Placebo tests of treatments on unrelated outcomes (simple OLS regressions).") 


##### K Main Results on Selected Demographics, Including by Vote ######
plot_along(along = "country_name", weight = "weight", name = "variables_ncs_gcs_ics_by_country_pol", vars = variables_ncs_gcs_ics, levels_along = c("All", levels_pol[-1]), labels = legend_ncs_gcs_ics, save = T, return_mean_ci = F, df = all, width = 1000, height = 480, origin = 50, plot_origin_line = T, weight_non_na = F)

plot_along(along = "country_name", weight = "weight", name = "variables_wealth_tax_support_by_country_pol",  vars = variables_wealth_tax_support, labels = legend_wealth_tax, levels_along = c("All", levels_pol[-1]), save = T, return_mean_ci = F, df = all, width = 820, height = 380, origin = 50, plot_origin_line = T, weight_non_na = F)

# TODO!
plot_along(along = "millionaire_tax_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370,
           weight = "weight", name = "program_preferred_by_millionaire_tax_in_program_pol", covariates = "millionaire_tax_in_program", levels_subsamples = levels.pol[-c(10,11)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T)
plot_along(along = "cut_aid_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370,
           weight = "weight", name = "program_preferred_by_cut_aid_in_program_pol", covariates = "cut_aid_in_program", levels_subsamples = levels.pol[-c(10,11)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T)

plot_along(along = "variant_warm_glow", vars = "gcs_support", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all[all$variant_warm_glow != "NCS" & !all$country %in% c("RU"),], width = 400, height = 370,
           weight = "weight", name = "gcs_support_by_variant_warm_glow_pol", covariates = "variant_warm_glow", levels_subsamples = levels.pol[-c(10,11)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T, condition = " > 0")
plot_along(along = "info_solidarity", vars = "share_solidarity_supported", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all, width = 400, height = 370,
           weight = "weight", name = "share_solidarity_supported_by_info_solidarity_pol", covariates = "info_solidarity", levels_subsamples = levels.pol, colors = "black", origin = 0, plot_origin_line = T, no_legend = T)

heatmap_multiple(heatmaps_defs["solidarity_support"], name = "solidarity_support_pol", levels = levels_pol)
heatmap_multiple(heatmaps_defs["radical_redistr"], name = "radical_redistr_pol", levels = levels_pol)
heatmap_multiple(heatmaps_defs["main_radical_redistr"], name = "main_radical_redistr_pol", levels = levels_pol)


##### L Main Results Weighted by Vote ######
# Main results: variables_ncs_gcs_ics_by_country, variables_wealth_tax_support_by_country, program_preferred_by.., gcs_support_by_variant_warm_glow, share_solidarity_supported_by_info_solidarity, solidarity_support_share, radical_redistr_all_share
plot_along("country_name", weight = "weight_vote", name = "variables_ncs_gcs_ics_by_country_weight_vote", vars = variables_ncs_gcs_ics, levels_along = levels_default_list, labels = legend_ncs_gcs_ics, save = T, return_mean_ci = F, df = all, width = 1000, height = 480, origin = 50, plot_origin_line = T) 

plot_along("country_name", weight = "weight_vote", name = "variables_wealth_tax_support_by_country_weight_vote",  vars = variables_wealth_tax_support, labels = legend_wealth_tax, levels_along = levels_default_list, save = T, return_mean_ci = F, df = all, width = 820, height = 400, origin = 50, plot_origin_line = T) 

plot_along(along = "millionaire_tax_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370, 
           weight = "weight_vote", name = "program_preferred_by_millionaire_tax_in_program_weight_vote", covariates = "millionaire_tax_in_program", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 
plot_along(along = "cut_aid_in_program", vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = call[!call$country %in% c("SA", "RU"),], width = 400, height = 370, 
           weight = "weight_vote", name = "program_preferred_by_cut_aid_in_program_weight_vote", covariates = "cut_aid_in_program", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

plot_along(along = "variant_warm_glow", vars = "gcs_support", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all[all$variant_warm_glow != "NCS" & !all$country %in% c("SA", "RU"),], width = 400, height = 370, 
           weight = "weight_vote", name = "gcs_support_by_variant_warm_glow_weight_vote", covariates = "variant_warm_glow", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T, condition = " > 0") 
plot_along(along = "info_solidarity", vars = "share_solidarity_supported", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, df = all, width = 400, height = 370, 
           weight = "weight_vote", name = "share_solidarity_supported_by_info_solidarity_weight_vote", covariates = "info_solidarity", levels_subsamples = levels_default_list, colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

heatmap_multiple(heatmaps_defs["solidarity_support"], name = "solidarity_support_weight_vote", variant_weight = "vote")
heatmap_multiple(heatmaps_defs["radical_redistr"], name = "radical_redistr_weight_vote", variant_weight = "vote")


##### M Main Results on the Extended Sample ######
# Main results: variables_ncs_gcs_ics_by_country, variables_wealth_tax_support_by_country, program_preferred_by.., gcs_support_by_variant_warm_glow, share_solidarity_supported_by_info_solidarity, solidarity_support_share, radical_redistr_all_share
plot_along("country_name", df = a[a$stayed,], name = "variables_ncs_gcs_ics_by_country_extended", vars = variables_ncs_gcs_ics, levels_along = levels_default_list, labels = legend_ncs_gcs_ics, save = T, return_mean_ci = F, width = 1000, height = 480, origin = 50, plot_origin_line = T) 

plot_along("country_name", df = a[a$stayed,], name = "variables_wealth_tax_support_by_country_extended",  vars = variables_wealth_tax_support, labels = legend_wealth_tax, levels_along = levels_default_list, save = T, return_mean_ci = F, width = 820, height = 400, origin = 50, plot_origin_line = T) 

plot_along(along = "millionaire_tax_in_program", df = calla[!calla$country %in% c("SA", "RU") & calla$stayed,], vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, width = 400, height = 370,
           name = "program_preferred_by_millionaire_tax_in_program_extended", covariates = "millionaire_tax_in_program", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T)
plot_along(along = "cut_aid_in_program", df = calla[!calla$country %in% c("SA", "RU") & calla$stayed,], vars = "program_preferred", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, width = 400, height = 370,
           name = "program_preferred_by_cut_aid_in_program_extended", covariates = "cut_aid_in_program", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T)

plot_along(along = "variant_warm_glow", df = a[a$variant_warm_glow != "NCS" & !a$country %in% c("SA", "RU") & a$stayed,], vars = "gcs_support", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, width = 400, height = 370, 
           name = "gcs_support_by_variant_warm_glow_extended", covariates = "variant_warm_glow", levels_subsamples = levels_default_list[-c(11,12)], colors = "black", origin = 0, plot_origin_line = T, no_legend = T, condition = " > 0") 
plot_along(along = "info_solidarity", df = a[a$stayed,], vars = "share_solidarity_supported", subsamples = "country_name", save = T, plotly = T, return_mean_ci = F, width = 400, height = 370, 
           name = "share_solidarity_supported_by_info_solidarity_extended", covariates = "info_solidarity", levels_subsamples = levels_default_list, colors = "black", origin = 0, plot_origin_line = T, no_legend = T) 

heatmap_multiple(heatmaps_defs["solidarity_support"], name = "solidarity_support_extended", data = a[a$stayed,])
heatmap_multiple(heatmaps_defs["radical_redistr"], name = "radical_redistr_extended", data = a[a$stayed,])


##### N Influence of the Item Order on Answers #####
# revenue_split_few_global, on NCQG; of wording on NCQG
# Orders with full randomization: revenue_split_few, revenue_split_many, solidarity_support, why_hic_help_lic
sustainable_future_variant <- lm(sustainable_future ~ (variant_sustainable_future == "a"), data = all, weights = weight) # 3pp more likely to choose sustainable if it is scenario A
transfer_how_cash_unconditional_order <- lm(transfer_how_cash_unconditional > 0 ~ transfer_how_order, data = all, weights = weight) # 9pp less likely to consider UCT right way if it's the first option
why_hic_help_lic_duty_order <- lm(why_hic_help_lic_duty ~ (why_hic_help_lic_order_duty == 3), data = all, weights = weight) # 5pp less likely to choose duty if last option
gcs_comprehension_order <- lm(gcs_comprehension > 0 ~ gcs_comprehension_order, data = all, weights = weight) # 3pp more likely to be correct if it's first option
ncqg_fusion_variant <- lm(ncqg_fusion >= 100 ~ variant_ncqg, data = all, weights = weight) # 8pp more likely to choose >= 100 bn in long version
ncqg_order <- lm(ncqg > 2 ~ ncqg_order, data = all, weights = weight) # 9pp less likely to choose >= 100 bn if increasing order (simple version)
solidarity_support_order <- lm(unlist(all[,variables_solidarity_support]) > 0 ~ unlist(all[,variables_solidarity_support_order]) == 1, weights = rep(all$weight, length(variables_solidarity_support_order)))
split_order <- lm(unlist(all[,c(variables_split_few, variables_split_many)]) > 15 ~ factor(unlist(all[,c(variables_split_few_order, variables_split_many_order)])), weights = rep(all$weight, 18))

stargazer(sustainable_future_variant, transfer_how_cash_unconditional_order, why_hic_help_lic_duty_order, gcs_comprehension_order, ncqg_fusion_variant, ncqg_order, solidarity_support_order, split_order, type = "latex", style = "default", out = "../tables/order.tex",
          keep.stat = c("n", "rsq"), label = "tab:order", dep.var.caption = "", model.names = FALSE, no.space = TRUE, float = FALSE,  #, "adj.rsq"), dep.var.caption = "Dependent variable:" ,
          dep.var.labels = c("\\makecell{Prefers\\\\Sustain.\\\\future}", "\\makecell{Finds\\\\Uncond.\\\\cash\\\\transfers\\\\Right}", "\\makecell{Agrees it\\\\is HIC's\\\\duty to\\\\help LICs}", "\\makecell{Understood\\\\Global\\\\Clim. Sch.}",
                             "\\makecell{Preferred\\\\NCQG\\\\$\\geq\\$ 100$ bn}", "\\makecell{Pref. NCQG\\\\$\\geq\\$ 100$ bn\\\\(variant\\\\ \\textit{Short})}", "\\makecell{Supports\\\\a\\\\plausible\\\\policy}", "\\makecell{Allocates\\\\$\\geq 15\\%$ to\\\\spending\\\\item}"),
          covariate.labels = c("Scenario A = Sustainable", "Cash transfers first item", "Duty last item", "Correct answer first item", "Variant: \\textit{Short}", "Items in increasing order", "That item is the first one", "Order of the item: 2", "Order of the item: 3", "Order of the item: 4", "Order of the item: 5"),
          title = "Effect on answers of the random order of response items.") 


##### O Supplementary Tables #####
# with(all, summary(lm((split_many_global/split_nb_global) ~ as.factor(split_nb_global), weights = weight))) 
# Table S14 ICS ~ country coverage
same_reg_subsamples(dep.var = "ics_support", dep.var.caption = "Supports the International Climate Scheme", covariates = c("variant_ics"), display_mean = F, along.levels = c("Europe", countries), constant_instead_mean = F, model.numbers = F,
                    data_list = lapply(levels_plain, function(c) all[all$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Variant: High Color", "Variant: Low", "Variant: Mid"), p_instead_SE = F, filename = "ics", omit.note = T, mean_above = F)
# same_reg_subsamples(dep.var = "ics_support", dep.var.caption = "Supports the International Climate Scheme", covariates = c("variant_ics", control_variables[-11]), display_mean = T, along.levels = c("Europe", countries), constant_instead_mean = T, model.numbers = F, keep = "_ics",
#                     data_list = lapply(levels_plain, function(c) all[all$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Variant: High Color", "Variant: Low", "Variant: Mid"), p_instead_SE = F, filename = "ics", omit.note = T, mean_above = F)
# summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight)) 

# Table S17 wealth tax ~ country coverage
same_reg_subsamples(dep.var = "wealth_tax_support", dep.var.caption = "Supports the International Wealth Tax", covariates = c("variant_wealth_tax"), display_mean = F, along.levels = c("Europe", countries), constant_instead_mean = F, model.numbers = F,
                    data_list = lapply(levels_plain, function(c) all[all$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Variant: Global", "Variant: Int'l"), p_instead_SE = F, filename = "wealth_tax", omit.note = T, mean_above = F)
# same_reg_subsamples(dep.var = "wealth_tax_support", dep.var.caption = "Supports the International Wealth Tax", covariates = c("variant_wealth_tax", control_variables[-11]), display_mean = T, along.levels = c("Europe", countries), constant_instead_mean = T, model.numbers = F, keep = "wealth_tax",
#                     data_list = lapply(levels_plain, function(c) all[all$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Variant: Global", "Variant: Int'l"), p_instead_SE = F, filename = "wealth_tax", omit.note = T, mean_above = F)

# Table S18 conjoint
same_reg_subsamples(dep.var = "program_preferred", dep.var.caption = "Program is preferred", covariates = c("cut_aid_in_program", "millionaire_tax_in_program", "foreign3_in_program"), display_mean = F, along.levels = c("Europe", countries[-c(9,10)]), constant_instead_mean = F, model.numbers = F,
                    data_list = lapply(levels_plain[-c(11,12)], function(c) call[call$country_name %in% c(c, special_levels[[c]]$value),]), cluster = "n", covariate.labels = c("Cut aid", "Int'l tax", "Foreign3"), p_instead_SE = F, filename = "conjoint", omit.note = T, mean_above = F)
# same_reg_subsamples(dep.var = "program_preferred", dep.var.caption = "Program is preferred", covariates = c("millionaire_tax_in_program", "cut_aid_in_program", "foreign3_in_program", control_variables[-11]), display_mean = F, along.levels = c("Europe", countries), constant_instead_mean = F, model.numbers = F,
#                     data_list = lapply(levels_plain[-c(11,12)], function(c) call[call$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Cut aid", "Int'l tax", "Foreign3"), p_instead_SE = F, filename = "conjoint_control", omit.note = T, mean_above = F, keep = "in_program")

# Table S19 warm glow substitute
same_reg_subsamples(dep.var = "gcs_support/100", dep.var.caption = "Supports the Global Climate Scheme", covariates = c("variant_warm_glow", control_variables[-11]), display_mean = T, along.levels = c("Europe", countries[-c(9,10)]), constant_instead_mean = T, model.numbers = F, keep = "warm",
                    data_list = lapply(levels_plain[-c(11,12)], function(c) all[all$country_name %in% c(c, special_levels[[c]]$value) & all$variant_warm_glow != "NCS" & !all$country %in% c("SA", "RU"),]), covariate.labels = c("Variant: Donation"), p_instead_SE = F, filename = "warm_glow_substitute", omit.note = T, mean_above = F)

# Table S20 warm glow realism
same_reg_subsamples(dep.var = "share_solidarity_supported", dep.var.caption = "Share of plausible policies supported", covariates = c("info_solidarity", control_variables[-11]), display_mean = T, along.levels = c("Europe", countries), constant_instead_mean = T, model.numbers = F,
                    data_list = lapply(levels_plain, function(c) all[all$country_name %in% c(c, special_levels[[c]]$value),]), covariate.labels = c("Info Treatment"), p_instead_SE = F, filename = "warm_glow_realism", omit.note = T, mean_above = F, keep = c("info"))

# Table S21 2SLS without control
summary(first_stage_wo_control <- lm((likely_solidarity > 0) ~ info_solidarity, data = e, weights = weight)) # 33% + 8pp***
summary(iv_model_wo_control <- ivreg(reg_formula("share_solidarity_supported", c("(likely_solidarity > 0)")), instruments = reg_formula("", c("info_solidarity")), data = e, weights = weight)) 
(effF_wo_control <- eff_F(data = e, Y = "share_solidarity_supported", D = "I(likely_solidarity > 0)", Z = "info_solidarity", controls = NULL, weights = "weight")) # 65
summary(ols_model_wo_control <- lm(reg_formula("share_solidarity_supported", c("(likely_solidarity > 0)")), data = e, weights = weight)) # 14.53***
summary(direct_effect_wo_control <- lm(reg_formula("share_solidarity_supported", c("info_solidarity")), data = e, weights = weight)) 
stargazer(first_stage_wo_control, iv_model_wo_control, ols_model_wo_control, direct_effect_wo_control, 
          se = lapply(c("first_stage_wo_control", "iv_model_wo_control", "ols_model_wo_control", "direct_effect_wo_control"), function(m) sqrt(diag(vcovHC(eval(str2expression(m)), type = "HC1")))),
          column.labels = c("IV 1st Stage", "IV 2nd Stage", "OLS", "Direct Effect"), model.names = FALSE, no.space = TRUE,
          keep.stat = c("n", "rsq"), label = "tab:iv_wo_control", dep.var.caption = "", star.cutoffs = NA,
          dep.var.labels = c("\\makecell{Believes global\\\\redistr. likely}", "Share of plausible global policies supported"),
          covariate.labels = c("Information treatment", "Believes global redistr. likely", "(Intercept)"), keep = c("solidarity", "Constant"),
          type = "latex", style = "default", out = "../tables/IV_warm_glow_wo_control_no_star.tex", float = FALSE,
          title = "Effect on support for global redistribution of believing that it is likely.", omit.table.layout = "n", 
          add.lines = list(#c("Controls: sociodemos and vote", "", "", "", ""), 
                           c("Effective F-statistic", sprintf("%.2f", effF_wo_control), "", "", "")))  
