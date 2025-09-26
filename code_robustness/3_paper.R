
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


##### Revenue split #####
wtd.mean(all$revenue_split_few_global, all$weight) # 17.5%
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


##### ICS #####
# Pluralistic ignorance
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
# Conjoint analysis
decrit("conjoint") # 27%
summary(reg_conjoint <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight)) # +4*** / -4***
coeftest(reg_conjoint, vcov = vcovCL(reg_conjoint, cluster = ~n))
# Effects are preserved when inconsistent programs are removed (considering the two policies as consistent with any program). Cf. Cuesta et al. (22)
summary(reg_conjoint_cons <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints))
coeftest(reg_conjoint_cons, vcov = vcovCL(reg_conjoint_cons, cluster = ~n))
# The effect of cutting aid disappears when removing left-leaning programs where it is present; while wealth tax is preserved when removing right-leaning programs where it is present. => Cutting aid is harmful only for left-leaning programs; wealth tax is helpful for any program.
summary(reg_conjoint_cons_strict <- lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, weights = weight, subset = call$consistent_conjoints_strict))
coeftest(reg_conjoint_cons_strict, vcov = vcovCL(reg_conjoint_cons_strict, cluster = ~n))
decrit(call$consistent_conjoints) # 59%
decrit(call$consistent_conjoints_strict) # 39%
summary(lm(program_preferred ~ millionaire_tax_in_program + cut_aid_in_program + foreign3_in_program, data = call, subset = vote %in% c("Center-right or Right"), weights = weight))

# Warm glow
summary(lm(gcs_support ~ variant_warm_glow, data = all, weights = weight, subset = variant_warm_glow != "NCS" & !country %in% c("SA", "RU")))
sapply(c("all", countries), function(c) round(mean(d(c)$likely_solidarity > 0, na.rm = T), 3)) # 38%

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
          type = "latex", style = "default", out = "../tables/IV.tex", float = FALSE,
          title = "Effect on support for global redistribution of believing that it is likely.")  # add.lines = list(c("1st Stage F-statistic", round(first_stage_f, 2), "", "", ""))
summary(direct_effect)$coefficients[,4]


##### Representativeness ######
# representativeness_table("All")
representativeness_table(c("All", "Eu", "EU"))
# representativeness_table(c("Eu", countries[1:3]))
representativeness_table(countries[1:3])
representativeness_table(countries[4:7])
representativeness_table(countries[c(8,10,11)], omit = c("Not 25-64")) # TODO vote; employment
representativeness_table(countries[8:11], omit = c("Not 25-64"))
  

##### Determinants #####
desc_table(c("share_solidarity_supported", "gcs_support/100", "universalist", "vote_intl_coalition > 0", "convergence_support > 0", "wealth_tax_support", "sustainable_future"),  # "\\makecell{Preferred amount\\\\of climate finance\\\\(NCQG)}"
           dep.var.labels = c("\\makecell{Share of\\\\plausible\\\\policies\\\\supported}", "\\makecell{Supports\\\\the Global\\\\Climate\\\\Scheme}", "\\makecell{Universalist\\\\(Group\\\\defended:\\\\Humans or\\\\Sentient beings)}", 
                              "\\makecell{More likely\\\\to vote\\\\for party\\\\in global\\\\coalition}", "\\makecell{Endorses\\\\convergence\\\\of all countries'\\\\GDP p.c.\\\\by 2100}", "\\makecell{Supports an\\\\international\\\\wealth tax\\\\funding LICs}", "\\makecell{Prefers a\\\\sustainable\\\\future}"),
           indep_vars = control_variables, filename = "determinants_paper", nolabel = F, model.numbers = T, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 


##### Attrition #####
desc_table(dep_vars = c("dropout", "dropout_late", "attentive == F", "duration", "duration < 6"), weights = NULL, #ci = T, report = 'vcsp', 
           dep.var.labels = c("\\makecell{Dropped out}", "\\makecell{Dropped out\\\\after\\\\socio-eco}", "\\makecell{Failed\\\\attention test}", "\\makecell{Duration\\\\(in min)}", "\\makecell{Duration\\\\below\\\\6 min}"),
           filename = "attrition", save_folder = "../tables/", data = c(list(a), list(a), list(a[a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,]), list(a[a$attentive == T & a$stayed == T,])), 
           indep_vars = control_variables, omit = c("illionaire", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 


##### Balance analysis #####
desc_table(dep_vars = c("variant_wealth_tax == 'global'", "variant_wealth_tax == 'intl'", "variant_ics == 'low'", "variant_ics == 'high'", "variant_ics == 'high_color'", "variant_warm_glow == 'NCS'", "variant_warm_glow == 'None'", "info_solidarity"), dep.var.caption = "Random branch:", #, "variant_top_tax == 'top1'" omit = c("Constant"), # c("Constant", "Race: Other", "factorNA", "partner"),
           dep.var.labels = c("\\makecell{Wealth tax\\\\coverage:\\\\Global}", "\\makecell{Wealth tax\\\\coverage:\\\\Int'l}", "\\makecell{Int'l CS\\\\coverage:\\\\Low}", "\\makecell{Int'l CS\\\\coverage:\\\\High}", "\\makecell{Int'l CS\\\\coverage:\\\\High color}", "\\makecell{National\\\\CS\\\\asked}", "\\makecell{Warm glow\\\\substitute:\\\\Control}", "\\makecell{Warm glow\\\\realism: Info\\\\treatment}"), # ci = T, report = 'vcsp', 
           filename = "balance", weights = NULL, save_folder = "../tables/", data = all, indep_vars = control_variables, omit = c("Country", "Employment", "partner", "Constant", "Race: Other", "region", "Region", "factorNA", "Urbanity: NA", "Urbanicity: NA")) 
