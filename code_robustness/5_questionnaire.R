##### Radical redistribution: example world income tax #####
# Data fetch and preparation
gethin <- read.csv("../data_ext/fisher-gethin-redistribution-2024-06-27.csv") # Fisher-Post & Gethin (2023) https://www.dropbox.com/scl/fi/yseottljqpzom1lrqga5c?e=1

inflation <- read.xlsx("../data_ext/inflation_imf.xlsx") # IMF WEO (Oct 2024) https://www.imf.org/external/datamapper/PCPIPCH@WEO/WEOWORLD/VEN
gethin <- merge(gethin, inflation, all.x = T)
for (y in 2018:2024) gethin[[paste0("inflation_", y)]][is.na(gethin[[paste0("inflation_", y)]]) | gethin[[paste0("inflation_", y)]] == "no data"] <- 1
for (y in 2018:2024) gethin[[paste0("inflation_", y)]] <- as.numeric(gethin[[paste0("inflation_", y)]])
gethin$inflation_2023_2024 <- (1+gethin$inflation_2023/100)*(1+gethin$inflation_2024/100)

growth <- read.xlsx("../data_ext/growth_imf.xlsx") # Real GDP growth, IMF WEO (Oct 24), Accessed on 12/21/2024, https://www.imf.org/external/datamapper/NGDP_RPCH@WEO/OEMDC/ADVEC/WEOWORLD
gethin <- merge(gethin, growth, all.x = T)
gethin$growth_2020_2024 <- (1+gethin$growth_2020/100)*(1+gethin$growth_2021/100)*(1+gethin$growth_2022/100)*(1+gethin$growth_2023/100)*(1+gethin$growth_2024/100)
gethin$growth_2020_2024[is.na(gethin$growth_2020_2024)] <- 1

# Constants
countries <- c("FR", "DE", "IT", "PL", "ES", "GB", "CH", "JP", "RU", "SA", "US")
countries_names <- c("France", "Germany", "Italy", "Poland", "Spain", "UK", "Switzerland", "Japan", "Russia", "Saudia Arabia"," U.S.") # France, Germany, Italy, Poland, Spain, UK, Switzerland, Japan, Russia, Saudia Arabia, U.S.
countries_iso3 <- c("FRA", "DEU", "ITA", "POL", "ESP", "GBR", "CHE", "JPN", "RUS", "SAU", "USA")
# lcu_per_dollar <- setNames(c(0.97, 0.97, 0.97, 4.15, 0.97, 0.809, 0.912, 158, 105, 3.75, 1), countries) # 01/25
lcu_per_dollar <- setNames(c(0.926, 0.926, 0.926, 3.87, 0.926, 0.773, 0.896, 149, 84.3, 3.75, 1), countries) # 04/25
pop <- sapply(countries, function(c) mean(gethin$npop[gethin$iso == c], na.rm = T))
world_population <- 8231613070 # UN, 2025, Accessed 12/21/2024, https://population.un.org/dataportal/data/indicators/49/locations/900/start/2024/end/2025/table/pivotbylocation?df=e5e54b33-f396-4e7a-a2e6-938af4215c20
(inflation_2023_2024 <- sapply(countries, function(c) mean(gethin$inflation_2023_2024[gethin$iso == c], na.rm = T)))
(xppp_us <- sapply(countries, function(c) mean(gethin$xppp_us[gethin$iso == c], na.rm = T)))
usd_lcu <- xppp_us * inflation_2023_2024 / inflation_2023_2024["US"]

# Income is expressed in PPP $ 2024 (we inflate all 2019 LCU quantiles using country growth, add inflation up to 2022 with defl, convert to 2022 $ PPP with xppp_us, add U.S. inflation up to 2024)
gethin$lcu19_growth_ppp24 <- gethin$growth_2020_2024 * mean(gethin$inflation_2023_2024[gethin$iso == "US"], na.rm = T) / (gethin$xppp_us * gethin$defl) 
gethin$disposable_inc <- gethin$a_pdi * gethin$lcu19_growth_ppp24 # a: average, pdi: disposable (pretax - direct taxes + gov_soc: social assistance transfers)

# Nominal income
ppp <- read.xlsx("../data_ext/ppp.xlsx") # 10/21/2025 https://databank.worldbank.org/source/world-development-indicators/Series/PA.NUS.PRVT.PP#
gethin <- merge(gethin, ppp, all.x = T)
gethin$disposable_inc_mer <- gethin$disposable_inc * no.na(gethin$ppp2022, wtd.mean(gethin$ppp2022, gethin$npop))

# Aggregate country distributions into world one
compute_world_distrib_from_gethin <- function(var, year = 2019) {
  data <- gethin %>% arrange(year, !!as.symbol(var)) %>% group_by(year) %>% mutate(x = cumsum(weight), tot = sum(weight), x = x / tot) # Computes cumulative weights for each value
  breakpoints <- c(seq(0, 0.99, by = 0.01), 0.999, 0.9999, 1)
  data <- data %>% mutate(p = cut(x, breaks = breakpoints, labels = breakpoints[-length(breakpoints)], include.lowest = TRUE), # Computes world percentile for each value = mean_income
                          p = as.numeric(as.character(p)), p = ifelse(is.na(p), 0.9999, p)) %>% # Replace NA with 0.9999
    group_by(year, p) %>% dplyr::summarize(!!as.symbol(paste0(var, "_thre")) := min(!!as.symbol(var)), # Defines threshold as min mean_income of corresponding percentile
                                           !!as.symbol(paste0(var, "_mean")) := weighted.mean(!!as.symbol(var), weight)) %>% ungroup() # Defines mean as mean mean_income weighted by corresponding pop
  if (!is.null(year)) data <- data[data$year == year, names(data) != "year"]
  return(data)
}

world_disposable_inc <- compute_world_distrib_from_gethin("disposable_inc") # PPP $ 2024
thousandile_world_disposable_inc <- c(quadratic_interpolations(pmax(0, world_disposable_inc$disposable_inc_mean), pmax(0, world_disposable_inc$disposable_inc_thre), 
                                                               c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), world_disposable_inc$disposable_inc_mean[101:102] %*% c(.9, .1))

# thousandile_US_disposable_inc <- c(quadratic_interpolations(pmax(0, data$disposable_inc_mean), pmax(0, data$disposable_inc_thre), 
#                                                                c((0:99)/100, .999, 1), seq(0.000, .998, 0.001)), data$disposable_inc_mean[101:102] %*% c(.9, .1))
# tax_revenue(rate = .15, threshold = 80e3) + tax_revenue(rate = .15, threshold = 120e3) + .15*((data$disposable_inc_mean[101]-1e6)*1e-4+(data$disposable_inc_mean[100]-1e6)*1e-3)/mean(thousandile_US_disposable_inc)
# /!\ Pb: the top interpolation is linear, inflating the mean income => do piecewise linear to preserve the mean (but won't preserve the smoothness)

# Export world income distribution
write.csv2(data.frame(quantiles = c(1:1000)/1000, revenus = round(thousandile_world_disposable_inc)), file = "../data_ext/world_disposable_inc.csv", row.names = F)

# Tax revenue from a given linear tax, in proportion of world income (slightly underestimated, at the threshold percentile)
tax_revenue <- function(distr = thousandile_world_disposable_inc, weight = NULL, rate = .1, threshold = 48e3) {
  if (is.null(weight)) return(sum(pmax(0, rate*(distr - threshold)), na.rm = T)/sum(distr, na.rm = T)) 
  else return(sum(pmax(0, rate*(distr - threshold)) * weight, na.rm = T)/sum(distr * weight, na.rm = T)) } 
# Tax cost of funding a given income floor (= poverty gap), in proportion of world income
tax_cost <- function(threshold = 2555, distr = thousandile_world_disposable_inc, weight = NULL) {
  if (is.null(weight)) return(sum(pmax(0, threshold - distr), na.rm = T)/sum(distr, na.rm = T)) 
  else return(sum(pmax(0, threshold - distr) * weight, na.rm = T)/sum(distr * weight, na.rm = T)) } 


## Figures tax top 1%: 15% tax > 120k$/year funding $8/day ~ $250/month floor
tax_revenue(rate = .15, threshold = 120e3)/tax_cost(8*365) # 1.04

# Poverty rate
mean(thousandile_world_disposable_inc < 250*12)*world_population/1e9 # 2.0G people with less than $250 per month

# Share of top income affected by new tax (in %)
1-mean(thousandile_world_disposable_inc < 120e3) # 1.4% at world level
(share_affected_tax_top1 <- round(sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc >= 120e3], na.rm = T))))
# FR DE IT PL ES GB CH JP RU SA US 
# 2  4  2  2  2  2  4  4  2 11  8
wtd.mean(share_affected_tax_top1, pop) # 4.7% in survey countries

# Contribution as country share of GDP
gdp_contribution_tax_top1 <- sapply(countries, function(c) tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3))
(gdp_received_tax_top1 <- sapply(countries, function(c) round(100*tax_cost(3000, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c]), 2)))
round(100*gdp_contribution_tax_top1 - gdp_received_tax_top1, 1)
# FR DE IT PL ES GB CH JP RU SA US 
# 1  2  1  1  1  1  1  1  2  5  3 
wtd.mean(100*gdp_contribution_tax_top1 - gdp_received_tax_top1, pop) # 2.1% in survey countries


## Figures tax top 3%: 15% tax > 80k$/year + 15% tax > 120k$/year + 15% tax > 1M$/year funding $13/day ~ $400/month floor
(tax_revenue(rate = .15, threshold = 80e3) + tax_revenue(rate = .15, threshold = 120e3) + 
    .15*(world_disposable_inc$disposable_inc_mean[102]-1e6)*1e-4/mean(thousandile_world_disposable_inc))/tax_cost(13*365) # 1.01

# Poverty rate
mean(thousandile_world_disposable_inc < 400*12)*world_population/1e9 # 3.1G people with less than $400 per month

# Share of top income affected by new tax (in %)
1-mean(thousandile_world_disposable_inc < 80e3) # 3.1% at world level
(share_affected_new_tax_top3 <- round(sapply(countries, function(c) 100 - 1e-3*min(gethin$p[gethin$iso == c & gethin$disposable_inc >= 80e3], na.rm = T))))
# FR DE IT PL ES GB CH JP RU SA US 
#  5 10  5  4  5  5 18 10  4 16 18
wtd.mean(share_affected_new_tax_top3, pop) # 10.6% in survey countries

# Contribution as country share of GDP
gdp_contribution_tax_top3 <- sapply(countries, function(c) (tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 80e3)
                                                            + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3)
                                                            + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 1e6)))
(gdp_received_tax_top3 <- sapply(countries, function(c) round(100*tax_cost(4800, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c]), 2)))
round(100*gdp_contribution_tax_top3 - gdp_received_tax_top3, 1)
# FR DE IT PL ES GB CH JP RU SA US 
# 2  4  3  4  3  3  3  4  5 12  8
wtd.mean(100*gdp_contribution_tax_top3 - gdp_received_tax_top3, pop) # 5.5% in survey countries

## Tax collected and poverty gap in nominal terms
gdp_cost_tax_top1 <- sapply(unique(gethin$iso), function(c) (tax_cost(3000, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c])))
sum(sapply(unique(gethin$iso), function(c) pmax(0, (100*gdp_cost_tax_top1[c])*sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T))), na.rm=T)/
  sum(sapply(unique(gethin$iso), function(c) sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T)), na.rm=T) # 1.3%

gdp_contribution_tax_top1 <- sapply(unique(gethin$iso), function(c) (tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3)))
sum(sapply(unique(gethin$iso), function(c) pmax(0, (100*gdp_contribution_tax_top1[c])*sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T))), na.rm=T)/
  sum(sapply(unique(gethin$iso), function(c) sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T)), na.rm=T) # 1.8%

gdp_cost_tax_top3 <- sapply(unique(gethin$iso), function(c) (tax_cost(4800, gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c])))
sum(sapply(unique(gethin$iso), function(c) pmax(0, (100*gdp_cost_tax_top3[c])*sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T))), na.rm=T)/
  sum(sapply(unique(gethin$iso), function(c) sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T)), na.rm=T) # 3.2%

gdp_contribution_tax_top3 <- sapply(unique(gethin$iso), function(c) (tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 80e3)
                                                                     + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 120e3)
                                                                     + tax_revenue(gethin$disposable_inc[gethin$iso == c], gethin$weight[gethin$iso == c], .15, 1e6)))
sum(sapply(unique(gethin$iso), function(c) pmax(0, (100*gdp_contribution_tax_top3[c])*sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T))), na.rm=T)/
  sum(sapply(unique(gethin$iso), function(c) sum(gethin$disposable_inc_mer[gethin$iso == c] * gethin$weight[gethin$iso == c], na.rm = T)), na.rm=T) # 4.8%

## Thresholds in LCU 2024 (from PPP $ 2024)
80e3*usd_lcu
120e3*usd_lcu
1e6*usd_lcu
# per year 
round(80e3*usd_lcu/1e3)*1e3
round(120e3*usd_lcu/1e3)*1e3
round(1e6*usd_lcu/1e3)*1e3
# per month
round(80e3*usd_lcu/1e2/12)*1e2
round(120e3*usd_lcu/1e2/12)*1e2
round(1e6*usd_lcu/1e4/12)*1e4


##### Wealth tax revenue by country #####
## 2% tax above 5M, assuming 30% evasion/depreciation
# Data from WID, for 2022 (courtesy of bajard.felix@laposte.net), in current USD. 
wealth <- read.csv("../data_ext/wealth_tax_wid.csv") # /!\ wealth_above_threshold is total, not marginal wealth. E.g. someone with 150M will have wealth_above_threshold = 150M, not 50M, for threshold = 100M.
names(wealth) <- c("n", "code", "year", "threshold", "gdp", "national_wealth", "wealth_above_threshold", "headcount_above_threshold", "threshold_constant_2023", "headcount_at_bracket", "wealth_at_bracket")
wealth_tax_revenue <- sapply(countries, function(c) .02 * (1-.3) * (wealth$wealth_above_threshold[wealth$threshold == 5e6 & wealth$year == 2022 & wealth$code == c] - 
  5e6 * wealth$headcount_above_threshold[wealth$threshold == 5e6 & wealth$year == 2022 & wealth$code == c]))
round(wealth_tax_revenue*lcu_per_dollar/1e9)
# For revenue estimates of LICs, cf. Sub-Saharan Africa on https://wid.world/world-wealth-tax-simulator/: a 2% tax above 5M would collect 0.16% of LICs' GDP
#    LICs GDP = 664 G$ in 2023 according to https://data.worldbank.org/indicator/NY.GDP.MKTP.CD?end=2023&locations=XM&start=2023 (fetched 08/01/2025)
(lic_revenue <- round(0.0016*664*lcu_per_dollar)) # $1 billion: revenue in LICs
gni <- WDI(indicator = "NY.GNP.ATLS.CD", latest = 1) 
# wealth_tax_revenue_over_gdp <- wealth_tax_revenue/sapply(countries, function(c) median(wealth$gdp[wealth$code == c & wealth$year == 2022]))
wealth_tax_revenue_over_gdp <- wealth_tax_revenue/sapply(countries, function(c) gni$NY.GNP.ATLS.CD[gni$iso2c == c & gni$year == 2023])
round(100*wealth_tax_revenue_over_gdp, 1)


##### revenue_split_many #####
# Probas with 5 items
dhyper(0, 4, 9, 5) # 10% chances to get 0 global policy
dhyper(1, 4, 9, 5) # 39% chances to get exactly 1 global policy
dhyper(3, 4, 9, 5) + dhyper(4, 4, 9, 5) # 12% chances to get more global than national policies

# Probas with 6 items
dhyper(0, 4, 9, 6) # 5% chances to get 0 global policy
dhyper(1, 4, 9, 6) # 29% chances to get exactly 1 global policy
dhyper(4, 4, 9, 6) # 2% chances to get more global than national policies

# Probas with 4 items
dhyper(0, 4, 9, 4) # 18% chances to get 0 global policy
dhyper(1, 4, 9, 4) # 47% chances to get exactly 1 global policy
dhyper(3, 4, 9, 4) + dhyper(4, 4, 9, 4) # 5% chances to get more global than national policies
dhyper(4, 4, 9, 4) # 1% chance to get only global policies


##### Climate Scheme #####
# cf. gcs.R
# TODO integrate files from global_tax_attitudes and run


##### Russian Statistical Survey of Income and Participation in Social Programs 2023 #####
# Rosstat disposable income is ~20% lower than LIS', itself lower than GDP pc (WID matches GDP pc)
ru_hh <- read.dta13("../data_ext/RU_hh.dta") # Data and doc here: https://rosstat.gov.ru/free_doc/new_site/vndn-2023/index.html
ru_hh$hh_size <- as.numeric(substr(ru_hh$R_2_0, 1, 2))
ru_hh$uc <- 1 + 0.5 * pmax(0, ru_hh$hh_size - 1) - 0.2 * ru_hh$CH_0_15  # I count 15-16 yrs as below 15 as there is no hh data on below_15
# R_H_DOXOD_RASP: disposable cash income / R_H_DOX_RASPOL_BEZ: total disposable income (incl. non-cash except imputed rent)
# R_H_DOXOD_DEN: total cash income / R_H_DOX_SOVK_BEZ: total income (incl. non-cash except imputed rent) / KVZV: weighing coefficient 
# FED_OKR: Federal district. 30: Central; 31: Northwest; 33: Volga; 34: Ural; 35: Siberian; 36: Far Eastern; 37: Southern; 38: North Caucasus (H00_02: <100 territories but unrelated to zipcode)
# Urban: H00_04 == 1 / H00_07 === R_1_1_1: town_size: 7: <200; 8: 201-1000; 9: 1001-5k; 10: rural >5k; 1: urban <50k; 2: 50-100k; 3: 100k-250k; 4: 250k-500k; 5: 500k-1M; 6: >1M
# R_10_1 == 1: working (2: not working)
# Marital status: https://rosstat.gov.ru/free_doc/new_site/vndn-2023/Stats/IND_OSN/H01_04.html
# H01_01: gender (only indiv) / R_3_1: age group (only indiv) / CH_0_17: children below 18 (only in indiv) / 	CH_0_15: nb people below 16 / H00_06: hh number
# R_5_1 === I01_10: education level https://rosstat.gov.ru/free_doc/new_site/vndn-2023/Stats/IND_OSN/R_5_1.html
ru_hh$disp_inc <- ru_hh$R_H_DOXOD_RASP/ru_hh$uc 
ru_hh$tot_inc_pc <- ru_hh$R_H_DOXOD_DEN/ru_hh$hh_size 
round(wtd.quantile(ru_hh$disp_inc, weights = ru_hh$KVZV, c(.1, .2, .25, .3, .4, .5, .6, .7, .75, .8, .9)))
wtd.mean(ru_hh$tot_inc_pc, weights = ru_hh$KVZV)
round(wtd.quantile(ru_hh$tot_inc_pc/1200, weights = ru_hh$KVZV, c(.1, .2, .25, .3, .4, .5, .6, .7, .75, .8, .9)))
ru_ind <- read.dta13("../data_ext/RU_ind.dta") 
ru_ind$weight <- ru_ind$KVZV
ru <- merge(ru_ind, ru_hh, by = "H00_06")
round(wtd.quantile(ru$R_H_DOXOD_RASP, weights = ru$weight * !ru$CH_0_17, c(.1, .2, .25, .3, .4, .5, .6, .7, .75, .8, .9)))


##### Margins of error #####
binconf(234.5, 469, alpha = .05) # 4.5pp
binconf(500, 1000, alpha = .05) # 3.1p
binconf(1500, 3000, alpha = .05) # 1.8pp


##### Conjoint analysis: Extract policies from sources.xlsx and export to JSON #####
# /!\ Assumes "-" ends each policy domain.
policies_names <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Policies")) # , rowNames = T, rows = c(1, 16:41), cols = 1:6
languages <- colnames(policies_names)[!grepl("_|[a-z]", colnames(policies_names))]
json <- "{"
j <- 1
for (j in 1:length(languages)) {
  c <- languages[j]
  json <- paste0(json, '\n\t"', c, '": {')
  policies_c <- policies_names[, c]
  policies_c <- policies_c[!is.na(policies_c)]
  new_domain <- T
  i <- 1
  while (i <= length(policies_c)) {
    # if (no.na(policies_c[i], rep = "") != "") {
    if (new_domain) json <- paste0(json, '\n\t\t"', policies_c[i], '": [')
    else json <- paste0(json, '\n\t\t\t"', policies_c[i], '"') # Change this if we need to exclude some policies e.g. '-'
    if (policies_c[i] == "-") { # /!\ Assumes "-" ends all domains.
      json <- if (i == length(policies_c)) paste(json, '\n\t\t]') else paste(json, '\n\t\t],') 
    } else if (!new_domain) json <- paste0(json, ',')
    # } 
    new_domain <- policies_c[i] == "-"
    i <- i+1
  }
  json <- paste(json, if (j == length(languages)) '\n\t}' else '\n\t},')
} 
json <- paste0(json, '\n}')

write(json, "../conjoint_analysis/policies.json")
write(json, "../questionnaire/policies.json")
policies_conjoint <- fromJSON("../conjoint_analysis/policies.json")

for (l in languages) {
  ldat <- "Attributes\n"
  for (d in names(policies_conjoint[[l]])) ldat <- paste0(ldat, policies_domains[d], ":", paste0(policies_code[policies_conjoint[[l]][[d]]], collapse = ','), "\n")
  ldat <- paste0(ldat, "Weights\n") # /!\ Assumes equal weight for each policy in a given domain
  for (d in names(policies_conjoint[[l]])) ldat <- paste0(ldat, policies_domains[d], ":", paste0(rep(1/length(policies_conjoint[[l]][[d]]), length(policies_conjoint[[l]][[d]])), collapse = ','), "\n")
  ldat <- paste0(ldat, "Restrictions\n") # /!\ Assumes no restrictions, i.e. no incompatibility between policies
  write(ldat, paste0("../conjoint_analysis/", l, ".dat"))
}
# Export .dat required to process conjoint analysis results
# for (l in languages) {
#   ldat <- "Attributes\n"
#   for (d in names(policies_conjoint[[l]])) ldat <- paste0(ldat, d, ":", paste0(gsub(",", ";;", gsub(":", "##", policies_conjoint[[l]][[d]])), collapse = ','), "\n")
#   ldat <- paste0(ldat, "Weights\n") # /!\ Assumes equal weight for each policy in a given domain
#   for (d in names(policies_conjoint[[l]])) ldat <- paste0(ldat, d, ":", paste0(rep(1/length(policies_conjoint[[l]][[d]]), length(policies_conjoint[[l]][[d]])), collapse = ','), "\n")
#   ldat <- paste0(ldat, "Restrictions\n") # /!\ Assumes no restrictions, i.e. no incompatibility between policies
#   write(ldat, paste0("../conjoint_analysis/", l, ".dat"))
# }


##### Export in .csv #####
features <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "features", colNames = F))
write.table(features, "../questionnaire/features.csv", sep="|",  col.names=FALSE, row.names = F, quote = F, na = "")

features_inc <- as.matrix(read.xlsx("../questionnaire/sources.xlsx", sheet = "Income", colNames = F))
write.table(features_inc, "../questionnaire/income.csv", sep="|",  col.names=FALSE, row.names = F, quote = F, na = "")


##### Convert .qsf to .tex #####
questions_df <- parse_questionnaire()
View(questions_df)
writeClipboard(questionnaire_to_latex(questions_df[1:80,]))
cat(questionnaire_to_latex(questions_df[1:80,]))
