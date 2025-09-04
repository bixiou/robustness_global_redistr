



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
