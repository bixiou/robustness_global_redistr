##### Data and design #####
decrit(all$country, weight = F)
decrit(all$date[all$country != "RU"]) # Apr 15 - Jul 3
decrit(all$date[all$country == "RU"]) # Sep 19 - 
sapply(c("all", countries), function(c) round(median(d(c)$duration, na.rm = T), 3))

(mean_gn25 <- wtd.mean(sapply(names(my_taxes_global_nation_2023)[!is.na(my_taxes_global_nation_2023)], function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), adult_pop[!is.na(my_taxes_global_nation_2023)])) 
(mean_gn23 <- wtd.mean(my_taxes_global_nation_2023, adult_pop, na.rm = T))
mean_gn25 - mean_gn23 # .03
(mean_bi25 <- wtd.mean(sapply(names(stostad_billionaire_tax_absolute)[!is.na(stostad_billionaire_tax_absolute)], function(c) wtd.mean(d(c)$solidarity_support_billionaire_tax_control > 0, d(c)$weight)), adult_pop[!is.na(stostad_billionaire_tax_absolute)])) 
(mean_bi24 <- wtd.mean(stostad_billionaire_tax_absolute, adult_pop, na.rm = T)) 
mean_bi25 - mean_bi24 # -.044

# Attrition
mean(a$valid) # 23% allowed
mean(a$dropout[a$valid]) # 17% drop out among allowed => stayed == valid & !dropout
mean(a$dropout_late[a$valid])
mean(!a$legit[a$stayed]) # 16% excluded => final == legit & stayed
mean(!a$attentive[a$stayed]) # 9% inattentive
mean(a$duration[a$stayed] < 6) # 13% too fast
mean((a$duration < 6 & !a$attentive)[a$stayed]) # 5% too fast & inattentive


##### Open-ended fields #####
decrit(all$field_keyword_global_inequality, all, which = all$variant_field == "injustice") # .013
decrit(all$field_gpt_global_inequality, all, which = all$variant_field == "injustice") # .085
decrit(all$field_manual_global_inequality, all, which = all$variant_field == "injustice") # .037
all$field_en[all$variant_field == "injustice"][sample(setdiff(which(all$field_gpt_global_inequality[all$variant_field == "injustice"]), which(all$field_manual_global_inequality[all$variant_field == "injustice"])), 10)]
all$field_en[all$variant_field == "injustice"][sample(setdiff(which(all$field_manual_global_inequality[all$variant_field == "injustice"]), which(all$field_keyword_global_inequality[all$variant_field == "injustice"])), 10)]
decrit(all$field_gpt_global_inequality[all$field_en %in% c("poverty", "Poverty")])
decrit(all$field_gpt_global_inequality[all$field_en %in% "inequality among humans"])
decrit(all$field_manual_money, all, which = all$variant_field %in% c("concerns", "wish")) # .3048
summary(lm(field_manual_money ~ factor(income_decile), data = all, subset = all$variant_field %in% c("concerns", "wish"), weights = weight))
decrit("field_manual_money", which = all$variant_field %in% c("concerns", "wish") & all$income_decile == 10, data = all) # .21
decrit("field_manual_money", which = all$variant_field %in% c("concerns", "wish") & all$income_decile == 1, data = all) # .36
decrit(all$field_manual_own_country | all$field_manual_global_inequality, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .13
decrit(all$field_manual_global_inequality, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .11
decrit(all$field_manual_own_country, which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all) # .02
decrit(grepl("clean water", all$field_en), which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all)
decrit(grepl("starv", all$field_en), which = all$variant_field %in% c("injustice") & all$field_manual_inequality, data = all)

summary(lm(e$field_manual_money ~ gini_2019[e$country])) # 8pp
summary(lm(e$field_manual_money ~ gdp_pc_nominal_2024[e$country]))
summary(lm(e$field_manual_money ~ gini_2019[e$country] + gdp_pc_nominal_2024[e$country]))
sapply(c("all", countries), function(c) round(wtd.mean(grepl("?", d(c)$field, fixed = T), d(c)$weight), 3))


# Correlations
summary(lm(field_manual_immigration ~ vote == "Far right", data = all, weights = weight)) # 10 pp more likely from a baseline of 3pp; R²: .03
summary(lm(vote == "Far right" ~ field_manual_immigration, data = all, weights = weight)) # 22.1 pp more likely from a baseline of 8.77pp; R²: .03
22.1/8.77 # 2.5
summary(lm(age > 65 ~ field_manual_old_age, data = all, weights = weight)) # 25 pp more likely from a baseline of 24 pp; R²: .01
with(e, cor(field_manual_immigration, vote == "Far right", use = "complete.obs")) # .18
with(e, cor(field_manual_far_right_criticism, vote == "Left", use = "complete.obs")) # .13
with(e, cor(field_manual_old_age, age, use = "complete.obs")) # .12
with(e, cor(field_manual_old_age, age > 50, use = "complete.obs")) # .12
with(e, cor(field_manual_health, age, use = "complete.obs")) # .12
with(e, cor(field_manual_health, age_exact > 55, use = "complete.obs")) # .12
with(e, cor(field_manual_job, employment_status == "Unemployed (searching for a job)", use = "complete.obs")) # .11
with(e, cor(field_manual_education, employment_status == "Student", use = "complete.obs")) # .10
with(e, cor(field_manual_animals, group_defended == "Sentient beings", use = "complete.obs")) # .09
with(e, cor(field_manual_money, income_quartile, use = "complete.obs")) # -.08
with(e, cor(field_manual_environment, vote == "Left", use = "complete.obs")) # .08
with(e, cor(field_manual_inequality, vote == "Left", use = "complete.obs")) # .07
with(e, cor(field_manual_far_right_criticism, vote == "Non-voter, PNR or Other", use = "complete.obs")) # -.07
with(e, cor(field_manual_far_right_criticism, vote == "Far right", use = "complete.obs")) # -.06
with(e, cor(field_manual_nothing, vote == "Non-voter, PNR or Other", use = "complete.obs")) # .06
with(US, cor(field_manual_discrimination, race_black, use = "complete.obs")) # .05
with(e, cor(field_manual_security, vote == "Far right", use = "complete.obs")) # .05
with(e, cor(field_manual_family, group_defended == "Family and self", use = "complete.obs")) # .04
with(e, cor(field_manual_taxes_welfare, vote == "Left", use = "complete.obs")) # -.04
with(e, cor(field_manual_corruption, vote == "Far right" , use = "complete.obs")) # .04
with(e, cor(field_manual_education, education, use = "complete.obs")) # .04
with(e, cor(field_manual_own_country, vote == "Far right", use = "complete.obs")) # .03
with(e, cor(field_manual_relationships, couple, use = "complete.obs")) # -.03
# Below, not statistically significant
with(e, cor(field_manual_global_issue, vote == "Left", use = "complete.obs")) # .02
with(e, cor(field_manual_global_issue, group_defended == "Humans", use = "complete.obs")) # .02
with(e, cor(field_manual_discrimination, foreign_origin, use = "complete.obs")) # .01
with(e, cor(field_manual_happiness, well_being, use = "complete.obs")) # .01


##### Revenue split #####
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$revenue_split_few_global, d(c)$weight), 3)))
wtd.mean(all$revenue_split_few_global, all$weight) # 17.5%
wtd.t.test(x = all$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = all$weight) # p < 1e-100
wtd.t.test(x = JP$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = JP$weight) # p = .07
wtd.t.test(x = CH$revenue_split_few_global, y = 33.4*2/5, alternative = "greater", weight = CH$weight) # p < 1e-4
wtd.mean(all$revenue_split_few_global, all$weight)/(33.4*2/5) # +31%
wtd.mean(all$revenue_split_few_global == 0, all$weight) # 13.3%
wtd.mean(all$revenue_split_few_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES")) # 17.84%
wtd.mean(all$revenue_split_few_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES"))/(33.4*2/5) # +34%
wtd.mean(all$revenue_split_few_global, all$weight)/wtd.mean(all$revenue_split_few_domestic_education_healthcare, all$weight) # 68%
sort(sapply(variables_split_many, function(c) mean(e[[c]], na.rm = T)), decreasing = T) # 27.0, 22.5, 18.6, 16.5
with(all, summary(lm((split_many_global/split_nb_global) ~ as.factor(split_nb_global)))) 
wtd.mean(all$split_nb_global, all$weight) # 1.5
wtd.mean(all$split_many_global, all$weight) # 26.9%
wtd.mean(all$split_many_global, all$weight)/wtd.mean(all$split_nb_global, all$weight) # 17.5%
wtd.mean(all$split_many_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES"))/wtd.mean(all$split_nb_global, all$weight * all$country %in% c("US", "FR", "DE", "GB", "ES")) # 17.5%
decrit("revenue_split", RU)
table(RU$revenue_split)
wtd.mean(RU$revenue_split, RU$weight) # 12.3%
wtd.mean(RU$revenue_split == 0, RU$weight) # 12%
Ecdf(RU$revenue_split)


##### ICS #####
# Pluralistic ignorance
round(wtd.median(all$gcs_belief_own, weight = all$weight, na.rm = T) - sapply(c("all", countries), function(c) wtd.mean(d(c)$gcs_support, d(c)$weight))) # -14 pp
round(wtd.median(all$gcs_belief_us, weight = all$weight * (all$country != "US"), na.rm = T) - wtd.mean(US$gcs_support, US$weight)) # -18 pp

wtd.mean(all$gcs_belief_own - sapply(countries, function(c) wtd.mean(d(c)$gcs_support, d(c)$weight))[all$country], all$weight) # -14 pp
wtd.mean(all$gcs_belief_us - wtd.mean(US$gcs_support, US$weight), all$weight * (all$country != "US")) # -18 pp

# ICS
summary(lm(ics_support ~ variant_ics, data = all, weights = weight)) # 4 pp**
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight))
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "mid"), data = all, weights = weight, subset = all$country == "JP")) # high
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight, subset = all$country == "US")) # high*
summary(lm(ics_support ~ (variant_ics == "high") + (variant_ics == "high_color") + (variant_ics == "low"), data = all, weights = weight, subset = all$country %in% names(countries_Eu))) # low 4**
summary(lm(ics_support ~ variant_ics + I(country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP")) + I(country %in% c("US", "SA")) + variant_ics:I(country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP")) + variant_ics:I(country %in% c("US", "SA")), data = all, weights = weight, subset = grepl("high", all$variant_ics))) 
summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("ES", "CH"))) 
summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("FR", "RU", "IT", "GB", "DE", "PL", "JP"))) 
summary(lm(ics_support ~ variant_ics, data = all, weights = weight, subset = all$country %in% c("US", "SA"))) 

decrit("gcs_understood")
summary(lm(reg_formula("gcs_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight)) # -5***
summary(lm(reg_formula("ics_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight, subset = all$variant_ics != "high_color")) # -5***
summary(lm(reg_formula("ics_high_color_support", c(variables_socio_demos, "gcs_understood")), data = all, weights = weight)) # -2
decrit("ics_mid_support", which = all$ncs_support > 0)
decrit("ncs_support", which = all$ics_mid_support > 0)

# Wealth tax
sapply(c("all", countries), function(c) round(mean(d(c)$global_tax_support, na.rm = T), 3)) # 74%
sapply(c("all", countries), function(c) round(mean(d(c)$hic_tax_support, na.rm = T), 3)) # 70%
sapply(c("all", countries), function(c) round(mean(d(c)$intl_tax_support, na.rm = T), 3)) # 68%
with(e, summary(lm(wealth_tax_support ~ (variant_wealth_tax == "global") + (variant_wealth_tax == "intl")))) # -5***, -1.5


##### Sincerity of support #####
##### Conjoint analysis #####
decrit("conjoint") # 27%
summary(reg_conjoint <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight)) # +4*** / -4***
coeftest(reg_conjoint, vcov = vcovCL(reg_conjoint, cluster = ~n))
# Effects are preserved when inconsistent programs are removed (considering the two policies as consistent with any program). Cf. Cuesta et al. (22)
summary(reg_conjoint_cons <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints))
coeftest(reg_conjoint_cons, vcov = vcovCL(reg_conjoint_cons, cluster = ~n))
# The effect of cutting aid disappears when removing left-leaning programs where it is present; while wealth tax is preserved when removing right-leaning programs where it is present. => Cutting aid is harmful only for left-leaning programs; wealth tax is helpful for any program.
summary(reg_conjoint_cons_strict <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_strict))
coeftest(reg_conjoint_cons_strict, vcov = vcovCL(reg_conjoint_cons_strict, cluster = ~n))
summary(reg_conjoint_cons_party <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_party))
coeftest(reg_conjoint_cons_party, vcov = vcovCL(reg_conjoint_cons_party, cluster = ~n))
decrit(call$consistent_conjoints) # 59%
decrit(call$consistent_conjoints_strict) # 39%
decrit(call$consistent_conjoints_party) # 40%
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, subset = vote %in% c("Center-right or Right"), weights = weight))
# Comparison with average effect size
conjoint_effects <- sapply(names(amce)[grepl("EN", names(amce)) & (nchar(names(amce)) < 6)], function(i) sapply(names(amce[[i]]$estimates), function(k) amce[[i]]$estimates[[k]][1,]))
for (i in 1:5) for (j in 1:9) conjoint_effects[[i,j]] <- conjoint_effects[[i,j]] * adult_pop[-c(9:10)][j]/mean(adult_pop[-c(9:10)])
conjoint_effects <- conjoint_effects_mod <- unlist(conjoint_effects)
mean(abs(conjoint_effects))
# conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy2"] <- -conjoint_effects_mod[names(conjoint_effects) %in% "foreignpolicyforeignpolicy2"]
# conjoint_effects_mod[!names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2")] <- abs(conjoint_effects_mod[!names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2")])
# mean(conjoint_effects_mod[names(conjoint_effects) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2")])/mean(abs(conjoint_effects_mod)) # 69%
summary(lm((conjoint_effects_mod) ~ I(names(conjoint_effects_mod) %in% c("foreignpolicyforeignpolicy1", "foreignpolicyforeignpolicy2"))))


#####  Warm glow ##### 
summary(lm(gcs_support ~ variant_warm_glow, data = all, weights = weight, subset = variant_warm_glow != "NCS" & !country %in% c("SA", "RU")))
sapply(c("all", countries), function(c) round(mean(d(c)$likely_solidarity > 0, na.rm = T), 3)) # 38%

# 2SLS
first_stage <- lm((likely_solidarity > 0) ~ info_solidarity, data = e, weights = weight)
iv_model <- ivreg(share_solidarity_supported ~ (likely_solidarity > 0) | info_solidarity, data = e, weights = weight)
effF <- eff_F(data = e, Y = "share_solidarity_supported", D = "I(likely_solidarity > 0)", Z = "info_solidarity", controls = NULL, weights = "weight")
# (first_stage_f <- summary(iv_model, diagnostics = TRUE)$diagnostics["Weak instruments", "statistic"])
# ivmodel(Y = as.numeric(e$share_solidarity_supported), D = as.numeric(e$likely_solidarity > 0), Z = e$info_solidarity)
ols_model <- lm(share_solidarity_supported ~ (likely_solidarity > 0), data = e, weights = weight)
direct_effect <- lm(share_solidarity_supported ~ info_solidarity, data = e, weights = weight)
stargazer(first_stage, iv_model, ols_model, direct_effect,
          column.labels = c("IV 1st Stage", "IV 2nd Stage", "OLS", "Direct Effect"), model.names = FALSE, no.space = TRUE,
          keep.stat = c("n", "rsq"), label = "tab:iv", dep.var.caption = "", #, "adj.rsq"), dep.var.caption = "Dependent variable:" ,
          dep.var.labels = c("\\makecell{Believes global\\\\redistr. likely}", "Share of plausible global policies supported"),
          covariate.labels = c("Information treatment", "Believes global redistribution likely", "(Intercept)"),
          type = "latex", style = "default", out = "../tables/iv.tex", float = FALSE,
          title = "Effect on support for global redistribution of believing that it is likely.",
          add.lines = list(c("Effective F-statistic", sprintf("%.2f", effF), "", "", "")))  
summary(direct_effect)$coefficients[,4]


##### Breadth #####
# Currently debated policies
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported, d(c)$weight), 3)))
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_opposed, d(c)$weight), 2)))
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight), 2)))
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Left")), 2)))
sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right" | (d(c)$country == "US" & d(c)$vote == 1))), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Left")), 2)))

# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right")), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == 1)), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == -1)), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote == "Far right")), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight), 2)))
# sort(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight * (d(c)$vote > 0)), 2)) - sapply(c("all", countries), function(c) round(wtd.mean(d(c)$share_solidarity_supported - d(c)$share_solidarity_opposed, d(c)$weight), 2)))

# Global income redistribution
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top1_tax_support > 0, d(c)$weight), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top1_tax_support < 0, d(c)$weight), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top3_tax_support > 0, d(c)$weight), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$top3_tax_support < 0, d(c)$weight), 3)))

# Convergence 
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$convergence_support > 0, d(c)$weight), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$convergence_support < 0, d(c)$weight), 3)))

# Willingness to act
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$global_movement_spread | d(c)$global_movement_demonstrate | d(c)$global_movement_donate | d(c)$global_movement_strike, d(c)$weight), 3))) 
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$global_movement_demonstrate | d(c)$global_movement_donate | d(c)$global_movement_strike, d(c)$weight), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$vote_intl_coalition > 0, d(c)$weight * (d(c)$vote == -1) * (d(c)$vote_agg == 0)), 3)))
(sapply(c("all", countries), function(c) round(wtd.mean(d(c)$vote_intl_coalition < 0, d(c)$weight * (d(c)$vote == -1) * (d(c)$vote_agg == 0)), 3)))
wtd.mean((all$vote == -1) * (all$vote_agg == 0), all$weight)

# Comparison
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight), 3))
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$my_tax_global_nation < 0, d(c)$weight), 3))
(mean_gn25 <- wtd.mean(sapply(names(my_taxes_global_nation_2023)[!is.na(my_taxes_global_nation_2023)], function(c) wtd.mean(d(c)$my_tax_global_nation > 0, d(c)$weight * (d(c)$my_tax_global_nation != 0))), adult_pop[!is.na(my_taxes_global_nation_2023)])) 
(mean_gn23 <- wtd.mean(my_taxes_global_nation_2023, adult_pop, na.rm = T))
mean_gn25 - mean_gn23 # .03

# Moral circle
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight), 2))
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$country_name %in% countries_Eu)), 5))
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg == "Left")), 2))
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg > 0)), 2))
# sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote == -1)), 2))
# sapply(c("all", countries), function(c) round(wtd.mean(d(c)$universalist, d(c)$weight * (d(c)$vote_agg == "Far right"| (d(c)$country == "US" & d(c)$vote == 1))), 2))


##### Custom redistr #####
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_satisfied, d(c)$weight), 3))
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_skip, d(c)$weight), 3))
decrit("custom_redistr_satisfied")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Left")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Far right")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Center-right or Right")
decrit("custom_redistr_satisfied", all, which = all$vote_factor == "Non-voter, PNR or Other")
decrit("custom_redistr_satisfied", all, which = all$education == 1)
decrit("custom_redistr_satisfied", all, which = all$education == 2)
decrit("custom_redistr_satisfied", all, which = all$education == 3)
decrit("custom_redistr_winners", which = all$custom_redistr_satisfied) # 476/49
decrit("custom_redistr_losers", which = all$custom_redistr_satisfied) # 183/18
decrit("custom_redistr_degree", which = all$custom_redistr_satisfied) # 4.69/5
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_self_lose, d(c)$weight * d(c)$custom_redistr_satisfied), 3)) # 48%
sapply(c("all", countries), function(c) round(wtd.mean(d(c)$custom_redistr_self_gain, d(c)$weight * d(c)$custom_redistr_satisfied), 3)) # 9%
decrit(all$custom_redistr_winners * all$custom_redistr_degree * all$custom_redistr_losers == 0, which = all$custom_redistr_satisfied) # 10%
round(mean_custom_redistr$all_satisfied[1]/12) # 247
(max_winners <- min(which(mean_custom_redistr[["all_satisfied"]] < current_inc))) # 728
100*sum(mean_custom_redistr[["all_satisfied"]][1:max_winners] - current[1:max_winners])/sum(current[1:1000]) # 5.4

decrit("custom_redistr_winners") # 474
decrit("custom_redistr_losers") # 17.7
decrit("custom_redistr_degree") # 4.67
474/464 # 1.02
195/177 # 1.1
4.75/4.67 # 1.02
with(e, summary(lm(custom_redistr_winners ~ variant_sliders, subset = custom_redistr_satisfied_touched == T))) # 7.93***
with(e, summary(lm(custom_redistr_losers ~ variant_sliders, subset = custom_redistr_satisfied_touched == T))) # 5.51***
with(e, summary(lm(custom_redistr_degree ~ variant_sliders, subset = custom_redistr_satisfied_touched == T))) # -2.11***
7.93/20
2.11/5
sapply(c("all", countries[-9]), function(c) round(wtd.median(d(c)$custom_redistr_winners, d(c)$weight * d(c)$custom_redistr_satisfied_touched, na.rm = T), 3)) # 48
sapply(c("all", countries[-9]), function(c) round(wtd.median(d(c)$custom_redistr_losers, d(c)$weight * d(c)$custom_redistr_satisfied_touched, na.rm = T), 3)) # 18
sapply(c("all", countries[-9]), function(c) round(wtd.median(d(c)$custom_redistr_degree, d(c)$weight * d(c)$custom_redistr_satisfied_touched, na.rm = T), 3)) # 5

decrit("custom_redistr_winners", data = all, which = all$custom_redistr_satisfied > 0) # 48
decrit("custom_redistr_winners", data = all, which = all$custom_redistr_satisfied_touched > 0) # 46
decrit("custom_redistr_losers", data = all, which = all$custom_redistr_satisfied > 0) # 18.3
decrit("custom_redistr_losers", data = all, which = all$custom_redistr_satisfied_touched > 0) # 19.51
decrit("custom_redistr_transfer", data = all, which = all$custom_redistr_satisfied > 0) # 5.4
decrit("custom_redistr_transfer", data = all, which = all$custom_redistr_satisfied_touched > 0) # 5.8
decrit("custom_redistr_income_min", data = all, which = all$custom_redistr_satisfied > 0) # 247
decrit("custom_redistr_income_min", data = all, which = all$custom_redistr_satisfied_touched > 0) # 249


##### Survey features #####
# Features
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

# Keywords
export_keywods <- function(keys = keywords, strings = keywords_labels, file = "../tables/keywords.tex") {
  cat(paste("\\begin{itemize} \n \\item \\textbf{", paste(sapply(names(keys), function(k) paste0(strings[k], "}: \\texttt{", 
                                                                                                 gsub("|", "\\allowbreak|", gsub("^", "\\^{}", gsub("$", "\\$", keys[k], fixed = T), fixed = T), fixed = T))), 
                                                          collapse = "};\n \\item \\textbf{"), "}. \n \\end{itemize}"), file = file)
}
export_keywods()
export_keywods(keywords_comment, keywords_comment_labels, "../tables/keywords_comment.tex")

# EFA
gsub("-", "$-$", gsub("\\verb|convergence", "\\bottomrule \\end{tabular} \\switchcolumn \\begin{tabular}[h]{lr} \\verb|convergence", 
  kable(round(setNames(loadings, paste0("\\verb|", sub("expanding_security_council", "un_reform", gsub("solidarity_support", "pl_support", gsub("^transfer_|^why_hic_", "", names(loadings)))), 
  "|"))[order(-abs(loadings))], 3), format = "latex", booktabs = TRUE, col.names = c("Variable name", "Loading"), linesep = "", escape = FALSE), fixed = T)) %>% save_kable("../tables/efa.tex")
correlations <- cor(as.data.frame(lapply(e[, c(variables_interest, "latent_support_global_redistr")], as.numeric)), use = "pairwise.complete.obs")
# corrplot(cors)
sort(rowMeans(abs(correlations), na.rm = T)) # share_solidarity_supported .42, solidarity_support_ncqg_300bn 40, my_tax_global_nation .35, vote_intl_coalition .35, ncqg .35, global_movement_no .34, 



##### Representativeness ######
# representativeness_table("All")
representativeness_table(c("All", "Eu", "EU"))
# representativeness_table(c("Eu", countries[1:3]))
representativeness_table(countries[1:3])
representativeness_table(countries[4:7])
representativeness_table(countries[c(8,10,11)], omit = c("Not 25-64")) # TODO vote; employment
representativeness_table(countries[8:11], omit = c("Not 25-64"))

# Prez:
representativeness_table(countries[1:6])
representativeness_table(countries[7:11], omit = c("Not 25-64"))
  

##### Determinants #####
desc_table(c("share_solidarity_supported", "gcs_support/100", "universalist", "vote_intl_coalition > 0", "convergence_support > 0", "wealth_tax_support", "sustainable_future"),  # "\\makecell{Preferred amount\\\\of climate finance\\\\(NCQG)}"
           dep.var.labels = c("\\makecell{Share of\\\\plausible\\\\policies\\\\supported}", "\\makecell{Supports\\\\the Global\\\\Climate\\\\Scheme}", "\\makecell{Universalist\\\\(Group\\\\defended:\\\\Humans or\\\\Sentient beings)}", 
                              "\\makecell{More likely\\\\to vote\\\\for party\\\\in global\\\\coalition}", "\\makecell{Endorses\\\\convergence\\\\of all countries'\\\\GDP p.c.\\\\by 2100}", "\\makecell{Supports an\\\\international\\\\wealth tax\\\\funding LICs}", "\\makecell{Prefers a\\\\sustainable\\\\future}"),
           indep_vars = c(control_variables), filename = "determinants_paper", nolabel = F, model.numbers = T, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 


##### Attrition #####
desc_table(dep_vars = c("dropout", "dropout_late", "attentive == F", "duration", "duration < 6"), weights = NULL, #ci = T, report = 'vcsp', 
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition", save_folder = "../tables/", data = c(list(a[a$valid == T,]), list(a[a$valid == T,]), list(a[a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,])), 
           indep_vars = control_variables, omit = c("illionaire", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 


##### Balance analysis #####
desc_table(dep_vars = c("variant_wealth_tax == 'global'", "variant_wealth_tax == 'intl'", "variant_ics == 'low'", "variant_ics == 'high'", "variant_ics == 'high_color'", "variant_warm_glow == 'NCS'", "variant_warm_glow == 'None'", "info_solidarity"), dep.var.caption = "Random branch:", #, "variant_top_tax == 'top1'" omit = c("Constant"), # c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{Wealth tax\\\\coverage:\\\\Global}", "\\makecell{Wealth tax\\\\coverage:\\\\Int'l}", "\\makecell{Int'l CS\\\\coverage:\\\\Low}", "\\makecell{Int'l CS\\\\coverage:\\\\High}", "\\makecell{Int'l CS\\\\coverage:\\\\High color}", "\\makecell{National\\\\CS\\\\asked}", "\\makecell{Warm glow\\\\substitute:\\\\Control}", "\\makecell{Warm glow\\\\realism: Info\\\\treatment}"), # ci = T, report = 'vcsp', 
           filename = "balance", weights = NULL, save_folder = "../tables/", data = all, indep_vars = control_variables, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 
